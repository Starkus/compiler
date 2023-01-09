Array<u64, ThreadAllocator> g_procedureAddresses;

enum RelocationType
{
	RELOCATIONTYPE_INVALID = 0,
	RELOCATIONTYPE_PROCEDURE,
	RELOCATIONTYPE_STATIC_DATA,
	RELOCATIONTYPE_EXTERNAL_PROCEDURE,
	RELOCATIONTYPE_LABEL,
};
struct Relocation
{
	RelocationType type;
	union {
		u32 procedureIdx;
		IRLabel *label;
	};
	u64 destOffset;
	s32 offsetShift;
};
DynamicArray<Relocation, ThreadAllocator> g_relocations;

X64InstructionStream X64InstructionStreamBegin(
		BucketArray<X64Instruction, LinearAllocator, 1024> *instructionArray)
{
	X64InstructionStream stream;
	stream.idx = -1;
	stream.instructionArray = instructionArray;
	stream.instructionArrayCount = instructionArray->count;
	stream.stack.size = 0;
	return stream;
}
X64Instruction *X64InstructionStreamAdvance(X64InstructionStream *iterator)
{
	X64Instruction *result = nullptr;

	while (true) {
		if (iterator->stack.size == 0) {
			++iterator->idx;
			if (iterator->idx >= iterator->instructionArrayCount)
				return nullptr;
			result = &(*iterator->instructionArray)[iterator->idx];
			break;
		}
		else {
			X64InstructionStream::Frame *frame = &iterator->stack[iterator->stack.size - 1];
			if (frame->instruction->type == X64_Patch) {
				if (++frame->idx == 1) {
					result = frame->instruction->patch2;
					break;
				}
				else {
					--iterator->stack.size;
					continue;
				}
			}
			else if (frame->instruction->type == X64_Patch_Many) {
				if (++frame->idx < (s64)frame->instruction->patchInstructions.size) {
					result = &frame->instruction->patchInstructions[frame->idx];
					break;
				}
				else {
					--iterator->stack.size;
					continue;
				}
			}
		}
	}

	while (true) {
		if (result->type == X64_Patch) {
			*FixedArrayAdd(&iterator->stack) = { result, 0 };
			result = result->patch1;
		}
		else if (result->type == X64_Patch_Many) {
			*FixedArrayAdd(&iterator->stack) = { result, 0 };
			result = &result->patchInstructions[0];
		}
		else
			break;
	}
	return result;
}

String X64IRValueToStr(Context *context, IRValue value,
		BucketArray<Value, LinearAllocator, 256> *localValues)
{
	String result = "???VALUE"_s;

	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_FLOAT);
	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_STRING);

	if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
		result = TPrintF("%lld", value.immediate);
		return result;
	}

	u64 size = 0;
	TypeInfo typeInfo = GetTypeInfo(context, StripAllAliases(context, value.typeTableIdx));
	bool isXMM;
	size = typeInfo.size;
	Value v;
	s64 offset = 0;

	if (value.valueType == IRVALUETYPE_PROCEDURE) {
		result = GetProcedureRead(context, value.procedureIdx).name;
		goto decoratePtr;
	}

	if (value.valueType == IRVALUETYPE_MEMORY)
		offset = value.mem.offset;

	if (value.valueIdx & VALUE_GLOBAL_BIT)
		v = GetGlobalValue(context, value.valueIdx);
	else
		v = (*localValues)[value.valueIdx];

	if (v.flags & (VALUEFLAGS_ON_STATIC_STORAGE | VALUEFLAGS_IS_EXTERNAL)) {
		if (v.flags & VALUEFLAGS_IS_EXTERNAL)
			result = StringExpand(v.externalSymbolName);
		else {
			u8 *ptrToStaticData = *(u8 **)HashMapGet(context->globalValueContents,
					value.valueIdx & VALUE_GLOBAL_MASK);
			ASSERT(ptrToStaticData >= STATIC_DATA_VIRTUAL_ADDRESS &&
					ptrToStaticData < STATIC_DATA_VIRTUAL_ADDRESS_END);
			result = TPrintF("__start_of_static_data+0%llXh", (u64)(ptrToStaticData -
						STATIC_DATA_VIRTUAL_ADDRESS));
		}

		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);

		// Array indexing
		if (value.valueType == IRVALUETYPE_MEMORY && value.mem.elementSize > 0) {
			u32 indexTypeIdx;
			if (value.mem.indexValueIdx & VALUE_GLOBAL_BIT)
				indexTypeIdx = GetGlobalValue(context, value.mem.indexValueIdx).typeTableIdx;
			else
				indexTypeIdx = (*localValues)[value.mem.indexValueIdx].typeTableIdx;
			String indexRegisterStr = X64IRValueToStr(context,
					IRValueValue(value.mem.indexValueIdx, indexTypeIdx), localValues);
			result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.mem.elementSize);
		}

		goto decoratePtr;
	}

	isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;

	if (v.flags & VALUEFLAGS_IS_ALLOCATED) {
		if (v.flags & VALUEFLAGS_IS_MEMORY) {
			ASSERT(!(v.flags & VALUEFLAGS_FORCE_REGISTER));
			offset += v.stackOffset;
			if (v.flags & VALUEFLAGS_BASE_RELATIVE)
				result = "rbp"_s;
			else
				result = "rsp"_s;

			if (offset > 0)
				result = TPrintF("%S+0%xh", result, offset);
			else if (offset < 0)
				result = TPrintF("%S-0%xh", result, -offset);
		}
		else if (value.valueType == IRVALUETYPE_MEMORY) {
			ASSERTF(v.allocatedRegister <= R15_idx, "Value \"%S\" not allocated to GP register!", v.name);
			result = x64RegisterNames64[v.allocatedRegister];

			if (offset > 0)
				result = TPrintF("%S+0%xh", result, offset);
			else if (offset < 0)
				result = TPrintF("%S-0%xh", result, -offset);
		}
		else if (!isXMM) {
			ASSERTF(v.allocatedRegister <= R15_idx, "Value \"%S\" not allocated to GP register!", v.name);
			switch (size) {
			case 8:
				result = x64RegisterNames64[v.allocatedRegister]; break;
			case 4:
				result = x64RegisterNames32[v.allocatedRegister]; break;
			case 2:
				result = x64RegisterNames16[v.allocatedRegister]; break;
			case 1:
				result = x64RegisterNames8 [v.allocatedRegister]; break;
			default:
				ASSERT(!"Invalid size for a register!");
			}

			if (offset > 0)
				result = TPrintF("%S+0%xh", result, offset);
			else if (offset < 0)
				result = TPrintF("%S-0%xh", result, -offset);
		}
		else {
			ASSERTF(v.allocatedRegister >= XMM0_idx && v.allocatedRegister <= XMM15_idx,
					"Value \"%S\" not allocated to XMM register!", v.name);
			result = x64RegisterNames64[v.allocatedRegister];
		}
	}
	// Not allocated
	else {
#if DEBUG_BUILD
		if (v.name)
			result = TPrintF("$vr%d\"%S\"", value.valueIdx, v.name);
		else
#endif
			result = TPrintF("$vr%d", value.valueIdx);

		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);

		// Array indexing
		if (value.valueType == IRVALUETYPE_MEMORY && value.mem.elementSize > 0) {
			String indexRegisterStr = X64IRValueToStr(context,
					IRValueValue(context, value.mem.indexValueIdx), localValues);
			result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.mem.elementSize);
		}
	}

	// Array indexing
	if (value.valueType == IRVALUETYPE_MEMORY && value.mem.elementSize > 0) {
		String indexRegisterStr = X64IRValueToStr(context,
				IRValueValue(value.mem.indexValueIdx, TYPETABLEIDX_S64), localValues);
		result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.mem.elementSize);
	}

	if (value.valueType != IRVALUETYPE_MEMORY && !(v.flags & VALUEFLAGS_IS_MEMORY))
		return result;

decoratePtr:
	{
#if IS_WINDOWS
		switch (size) {
		case 1:
			result = TPrintF("BYTE PTR [%S]", result);
			break;
		case 2:
			result = TPrintF("WORD PTR [%S]", result);
			break;
		case 4:
			result = TPrintF("DWORD PTR [%S]", result);
			break;
		case 8:
			result = TPrintF("QWORD PTR [%S]", result);
			break;
		case 16:
			result = TPrintF("XMMWORD PTR [%S]", result);
			break;
		default:
			ASSERT(!"Invalid register size");
		}
#else
		switch (size) {
		case 1:
			result = TPrintF("BYTE [%S]", result);
			break;
		case 2:
			result = TPrintF("WORD [%S]", result);
			break;
		case 4:
			result = TPrintF("DWORD [%S]", result);
			break;
		case 8:
			result = TPrintF("QWORD [%S]", result);
			break;
		case 16:
			result = TPrintF("OWORD [%S]", result);
			break;
		default:
			ASSERT(!"Invalid register size");
		}
#endif
	}
	return result;
}

bool IsValueInMemory(Context *context, IRValue irValue)
{
	if (irValue.valueType == IRVALUETYPE_MEMORY)
		return true;
	if (irValue.valueType != IRVALUETYPE_VALUE)
		return false;
	Value value = IRGetValue(context, irValue.valueIdx);
	if (value.flags & (VALUEFLAGS_FORCE_MEMORY | VALUEFLAGS_IS_MEMORY |
				VALUEFLAGS_ON_STATIC_STORAGE))
		return true;
	return false;
}

bool FitsInOperand(Context *context, u8 acceptableOperands, IRValue value)
{
	bool isImmediate = value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER ||
					   value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT;
	if (isImmediate)
		return acceptableOperands & OPERANDTYPE_IMMEDIATE;
	if (!IsValueInMemory(context, value))
		return acceptableOperands & OPERANDTYPE_REGISTER;
	return acceptableOperands & OPERANDTYPE_MEMORY;
}

bool CanValueBeMemory(Context *context, IRValue value)
{
	if (value.valueType == IRVALUETYPE_MEMORY)
		return true;
	bool isImmediate = value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER ||
					   value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT;
	if (isImmediate)
		return false;
	if (IRGetValue(context, value.valueIdx).flags & VALUEFLAGS_FORCE_REGISTER)
		return false;
	if ((IRGetValue(context, value.valueIdx).flags & (VALUEFLAGS_IS_ALLOCATED |
			VALUEFLAGS_IS_MEMORY)) == VALUEFLAGS_IS_ALLOCATED)
		return false;
	return true;
}

inline void X64AddInstruction0(Context *context, SourceLocation loc, X64InstructionType type)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	*BucketArrayAdd(&jobData->beInstructions) = { loc, type };
}

inline void X64AddInstruction1(Context *context, SourceLocation loc, X64InstructionType type, IRValue dst)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	*BucketArrayAdd(&jobData->beInstructions) = { loc, type, dst };
}

inline void X64AddInstruction2(Context *context, SourceLocation loc, X64InstructionType type, IRValue dst,
		IRValue src)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	*BucketArrayAdd(&jobData->beInstructions) = { loc, type, dst, src };
}

void X64Mov(Context *context, SourceLocation loc, IRValue dst, IRValue src);
void X64MovNoTmp(Context *context, SourceLocation loc, IRValue dst, IRValue src)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	X64Instruction result = { loc };
	TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
	TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));

	// MOVUPS
	if (dstType.size == 16) {
		ASSERT(srcType.size == 16);
		result.type = X64_MOVUPS;
		result.dst = dst;
		result.src = src;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	}

	ASSERT(dstType.size <= 8);
	ASSERT(srcType.size <= 8);

	if (dstType.size != srcType.size)
		LogCompilerError(context, loc, TPrintF("Different sizes on MOV instruction: %d and %d",
					dstType.size, srcType.size));
	if (dstType.typeCategory == TYPECATEGORY_FLOATING) {
		// MOVSS and MOVSD
		if (srcType.typeCategory != TYPECATEGORY_FLOATING)
			LogCompilerError(context, loc, "Conversion of integer to float requires special"
					" instruction"_s);

		if (srcType.size == 4)
			result.type = X64_MOVSS;
		else {
			ASSERT(srcType.size == 8);
			result.type = X64_MOVSD;
		}
	}
	else {
		// MOV
		if (srcType.typeCategory == TYPECATEGORY_FLOATING)
			LogCompilerError(context, loc, "Conversion of float to integer requires special"
					" instruction"_s);

		result.type = X64_MOV;
	}

	if (CanValueBeMemory(context, dst) && src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		ASSERT(GetTypeInfo(context, src.typeTableIdx).size <= 4);

	result.dst = dst;
	result.src = src;
	*BucketArrayAdd(&jobData->beInstructions) = result;
}

IRValue X64CopyToRegister(Context *context, SourceLocation loc, IRValue src)
{
	if (src.valueType == IRVALUETYPE_VALUE && !CanValueBeMemory(context, src))
		// Already in a register?
		return src;

	u32 flags = VALUEFLAGS_FORCE_REGISTER;
	if (src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER) {
		Value srcValue = IRGetValue(context, src.valueIdx);
		flags |= srcValue.flags & VALUEFLAGS_IS_USED;
		flags |= src.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
	}
	IRValue tmp = IRValueNewValue(context, "_movtmp"_s, src.typeTableIdx, flags,
			src.valueIdx);

	X64MovNoTmp(context, loc, tmp, src);
	return tmp;
}

void X64ReduceRM64BitImmediate(Context *context, SourceLocation loc, u32 leftTypeIdx, IRValue *right)
{
	// Can't directly encode a 64 bit immediate
	if (right->valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
		right->typeTableIdx = leftTypeIdx;

		TypeInfo typeInfo = GetTypeInfo(context, right->typeTableIdx);
		if (typeInfo.typeCategory == TYPECATEGORY_ENUM)
			typeInfo = GetTypeInfo(context, typeInfo.enumInfo.typeTableIdx);
		bool isSigned = false;
		if (typeInfo.typeCategory == TYPECATEGORY_INTEGER)
			isSigned = typeInfo.integerInfo.isSigned;
		// Unsigned
		if (typeInfo.typeCategory == TYPECATEGORY_POINTER ||
			(typeInfo.size > 4 && !isSigned))
		{
			if ((u32)right->immediate != right->immediate) {
				IRValue tmp = IRValueNewValue(context, "_movimmtmp"_s, leftTypeIdx,
						VALUEFLAGS_FORCE_REGISTER);
				X64MovNoTmp(context, loc, tmp, *right);
				*right = tmp;
			}
			else
				right->typeTableIdx = TYPETABLEIDX_U32;
		}
		// Signed
		else if (typeInfo.size > 4 && isSigned) {
			if ((s32)right->immediate != right->immediate) {
				IRValue tmp = IRValueNewValue(context, "_movimmtmp"_s, leftTypeIdx,
						VALUEFLAGS_FORCE_REGISTER);
				X64MovNoTmp(context, loc, tmp, *right);
				*right = tmp;
			}
			else
				right->typeTableIdx = TYPETABLEIDX_S32;
		}
	}
}

void X64Mov(Context *context, SourceLocation loc, IRValue dst, IRValue src)
{
	if (src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		src.typeTableIdx = dst.typeTableIdx;

	if (CanValueBeMemory(context, dst))
		src = X64CopyToRegister(context, loc, src);

	X64MovNoTmp(context, loc, dst, src);
}

void X64Test(Context *context, SourceLocation loc, IRValue value)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	TypeInfo typeInfo = GetTypeInfo(context, value.typeTableIdx);
	if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
		floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));

	X64Instruction cmpInst;
	cmpInst.loc = loc;
	cmpInst.dst = value;
	switch (floatingType) {
	case X64FLOATINGTYPE_NONE:
	{
		cmpInst.type = X64_CMP;
		cmpInst.src = IRValueImmediate(0, TYPETABLEIDX_U8);
	} break;
	case X64FLOATINGTYPE_F32:
	{
		cmpInst.type = X64_COMISS;
		IRValue zero = IRValueNewValue(context, "_zero"_s, TYPETABLEIDX_128, 0);
		X64AddInstruction2(context, loc, X64_XORPS, zero, zero);
		cmpInst.src = zero;
	} break;
	case X64FLOATINGTYPE_F64:
	{
		cmpInst.type = X64_COMISD;
		IRValue zero = IRValueNewValue(context, "_zero"_s, TYPETABLEIDX_128, 0);
		X64AddInstruction2(context, loc, X64_XORPD, zero, zero);
		cmpInst.src = zero;
	} break;
	default:
		ASSERT(false);
	}

	u8 accepted = x64InstructionInfos[cmpInst.type].operandTypesLeft;
	if (!FitsInOperand(context, accepted, cmpInst.dst)) {
		ASSERT(accepted & OPERANDTYPE_REGISTER);
		IRValue newValue = IRValueNewValue(context, "_test_hlp"_s, cmpInst.dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER);
		X64Mov(context, loc, newValue, cmpInst.dst);
		cmpInst.dst = newValue;
	}

	*BucketArrayAdd(&jobData->beInstructions) = cmpInst;
}

IRValue X64PushRegisterParameter(u32 typeTableIdx, s32 *numberOfGPR, s32 *numberOfXMM)
{
	bool isXMM = typeTableIdx == TYPETABLEIDX_F32 || typeTableIdx == TYPETABLEIDX_F64;

	if (!isXMM) switch((*numberOfGPR)++) {
		case 0: return RDI;
		case 1: return RSI;
		case 2: return RDX;
		case 3: return RCX;
		case 4: return R8;
		case 5: return R9;
	}
	else if (*numberOfXMM < 16) {
		return IRValueValue(XMM0.valueIdx + (*numberOfXMM)++, typeTableIdx);
	}
	return { IRVALUETYPE_INVALID };
}

void X64CopyMemory(Context *context, SourceLocation loc, IRValue dst, IRValue src, IRValue size)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	ASSERT(dst.valueType == IRVALUETYPE_VALUE ||
		   dst.valueType == IRVALUETYPE_MEMORY);
	ASSERT(src.valueType == IRVALUETYPE_VALUE ||
		   src.valueType == IRVALUETYPE_MEMORY);
	u32 dstIdx = dst.valueIdx;
	u32 srcIdx = src.valueIdx;

	// First attempt to copy manually
	if (size.valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
		TypeInfo dstTypeInfo = GetTypeInfo(context, IRGetValue(context, dstIdx).typeTableIdx);
		TypeInfo srcTypeInfo = GetTypeInfo(context, IRGetValue(context, srcIdx).typeTableIdx);
		s64 sizeImm = size.immediate;

		s64 copiedBytes = 0;
		while (sizeImm - copiedBytes >= 16) {
			X64Mov(context, loc,
					IRValueMemory(dstIdx, TYPETABLEIDX_128, copiedBytes),
					IRValueMemory(srcIdx, TYPETABLEIDX_128, copiedBytes));
			copiedBytes += 16;
		}
		while (sizeImm - copiedBytes >= 8) {
			X64Mov(context, loc,
					IRValueMemory(dstIdx, TYPETABLEIDX_U64, copiedBytes),
					IRValueMemory(srcIdx, TYPETABLEIDX_U64, copiedBytes));
			copiedBytes += 8;
		}
		while (sizeImm - copiedBytes >= 4) {
			X64Mov(context, loc,
					IRValueMemory(dstIdx, TYPETABLEIDX_U32, copiedBytes),
					IRValueMemory(srcIdx, TYPETABLEIDX_U32, copiedBytes));
			copiedBytes += 4;
		}
		while (sizeImm - copiedBytes >= 1) {
			X64Mov(context, loc,
					IRValueMemory(dstIdx, TYPETABLEIDX_U8, copiedBytes),
					IRValueMemory(srcIdx, TYPETABLEIDX_U8, copiedBytes));
			++copiedBytes;
		}
	}
	else {
		ASSERT(g_copyMemoryProcIdx != U32_MAX);
		X64Mov(context, loc, RDI, dst);
		X64Mov(context, loc, RSI, src);
		X64Mov(context, loc, RDX,  size);
		X64Instruction result = { loc, X64_CALL };
		result.procedureIdx = g_copyMemoryProcIdx;
		ArrayInit(&result.parameterValues, 3);
		*ArrayAdd(&result.parameterValues) = RDI.valueIdx;
		*ArrayAdd(&result.parameterValues) = RSI.valueIdx;
		*ArrayAdd(&result.parameterValues) = RDX.valueIdx;
		*BucketArrayAdd(&jobData->beInstructions) = result;
	}
}

bool X64WinABIShouldPassByCopy(Context *context, u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
	// @Speed
	return  typeInfo.typeCategory == TYPECATEGORY_ARRAY ||
		  ((typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			typeInfo.typeCategory == TYPECATEGORY_UNION) &&
			typeInfo.size != 1 &&
			typeInfo.size != 2 &&
			typeInfo.size != 4 &&
			typeInfo.size != 8);
}

Array<u32, ThreadAllocator> X64ReadyWin64Parameters(Context *context, SourceLocation loc,
		ArrayView<IRValue> parameters, bool isCaller)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	int parameterCount = (int)parameters.size;

	Array<u32, ThreadAllocator> parameterValues;
	ArrayInit(&parameterValues, parameterCount * 2);

	for (int i = 0; i < parameterCount; ++i) {
		IRValue param = parameters[i];
		u32 paramTypeIdx = StripAllAliases(context, param.typeTableIdx);
		TypeInfo paramType = GetTypeInfo(context, paramTypeIdx);

		if (isCaller && X64WinABIShouldPassByCopy(context, paramTypeIdx)) {
			static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

			u32 tmpValueIdx = IRNewValue(context, "paramcpy"_s, paramTypeIdx, 0);
			IRValue tmpValue = IRValueValue(tmpValueIdx, voidPtrTypeIdx);

			X64Instruction pushInst = { loc, X64_Push_Value };
			pushInst.valueIdx = tmpValueIdx;
			*BucketArrayAdd(&jobData->beInstructions) = pushInst;

			IRValue ptr = IRValueNewValue(context, "paramptr"_s, TYPETABLEIDX_S64, 0);
			X64AddInstruction2(context, loc, X64_LEA, ptr, tmpValue);

			X64CopyMemory(context, loc, ptr, param, IRValueImmediate(paramType.size));
			param = ptr;
			paramTypeIdx = TYPETABLEIDX_S64;
		}

		bool isXMM = paramType.typeCategory == TYPECATEGORY_FLOATING;

		IRValue slot;
		switch(i) {
		case 0:
			slot = isXMM ? XMM0 : RCX; break;
		case 1:
			slot = isXMM ? XMM1 : RDX; break;
		case 2:
			slot = isXMM ? XMM2 : R8;  break;
		case 3:
			slot = isXMM ? XMM3 : R9;  break;
		default:
			if (isCaller)
				slot = IRValueValue(jobData->x64SpilledParametersWrite[i], TYPETABLEIDX_S64);
			else
				slot = IRValueValue(jobData->x64SpilledParametersRead[i], TYPETABLEIDX_S64);
		}
		slot.typeTableIdx = paramTypeIdx;

		if (isCaller)
			X64Mov(context, loc, slot, param);
		else {
			if (X64WinABIShouldPassByCopy(context, paramTypeIdx)) {
				u32 ptrTypeIdx = GetTypeInfoPointerOf(context, paramTypeIdx);
				param.typeTableIdx = ptrTypeIdx;
				slot.typeTableIdx = ptrTypeIdx;
				X64CopyMemory(context, loc, param, slot,
						IRValueImmediate(paramType.size));
			}
			else
				X64Mov(context, loc, param, slot);
		}

		*ArrayAdd(&parameterValues) = slot.valueIdx;
	}

	if (isCaller) {
		if (jobData->allocatedParameterCount < parameterCount)
			jobData->allocatedParameterCount = parameterCount;
	}

	return parameterValues;
}

Array<u32, ThreadAllocator> X64ReadyLinuxParameters(Context *context, SourceLocation loc,
		ArrayView<IRValue> parameters, bool isCaller)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	int parameterCount = (int)parameters.size;

	Array<u32, ThreadAllocator> parameterValues;
	ArrayInit(&parameterValues, parameterCount * 2);

	s32 numberOfGPR = 0;
	s32 numberOfXMM = 0;
	s32 numberOfSpilled = 0;
	for (int i = 0; i < parameterCount; ++i) {
		IRValue param = parameters[i];
		u32 paramTypeIdx = param.typeTableIdx;

		TypeInfo paramTypeInfo = GetTypeInfo(context, paramTypeIdx);
		bool isStruct = paramTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			(paramTypeInfo.typeCategory == TYPECATEGORY_ARRAY && paramTypeInfo.arrayInfo.count == 0);
		if (isStruct && paramTypeInfo.size <= 16) {
			ArrayView<StructMember> members;
			if (paramTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
				members = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT).structInfo.members;
			else
				members = paramTypeInfo.structInfo.members;

			IRValue first, second = {};
			int regCount = 1;

			first = IRValueMemory(param.valueIdx, TYPETABLEIDX_S64, 0);
			if (members[0].typeTableIdx == TYPETABLEIDX_F64 ||
			   (members.size == 2 &&
				members[0].typeTableIdx == TYPETABLEIDX_F32 &&
				members[1].typeTableIdx == TYPETABLEIDX_F32))
			{
				// An F64 or two consecutive F32s into an XMM register.
				first.typeTableIdx = TYPETABLEIDX_F64;
			}
			else if (members[0].typeTableIdx == TYPETABLEIDX_F32 &&
					(members.size == 1 || members[1].offset >= 8)) {
				// An only F32 into XMM register
				first.typeTableIdx = TYPETABLEIDX_F32;
			}
			else {
				if (paramTypeInfo.size > 4)
					first.typeTableIdx = TYPETABLEIDX_S64;
				else if (paramTypeInfo.size > 2)
					first.typeTableIdx = TYPETABLEIDX_S32;
				else if (paramTypeInfo.size > 1)
					first.typeTableIdx = TYPETABLEIDX_S16;
				else
					first.typeTableIdx = TYPETABLEIDX_S8;
			}

			int firstMember = 0;
			for (; firstMember < members.size; ++firstMember) {
				if (members[firstMember].offset >= 8)
					break;
			}
			if (firstMember > 0 && firstMember < members.size) {
				second = IRValueMemory(param.valueIdx, TYPETABLEIDX_S64, 8);
				regCount = 2;

				if (members[firstMember].typeTableIdx == TYPETABLEIDX_F64 ||
				   (firstMember < members.size - 1 &&
					members[firstMember].typeTableIdx   == TYPETABLEIDX_F32 &&
					members[firstMember+1].typeTableIdx == TYPETABLEIDX_F32))
				{
					// An F64 or two consecutive F32s into an XMM register.
					second.typeTableIdx = TYPETABLEIDX_F64;
				}
				else if (members[firstMember].typeTableIdx == TYPETABLEIDX_F32 &&
						(firstMember == members.size - 1 ||
						 members[firstMember+1].offset >= 8)) {
					// An only F32 into XMM register
					second.typeTableIdx = TYPETABLEIDX_F32;
				}
				else {
					if (paramTypeInfo.size > 12)
						second.typeTableIdx = TYPETABLEIDX_S64;
					else if (paramTypeInfo.size > 10)
						second.typeTableIdx = TYPETABLEIDX_S32;
					else if (paramTypeInfo.size > 9)
						second.typeTableIdx = TYPETABLEIDX_S16;
					else
						second.typeTableIdx = TYPETABLEIDX_S8;
				}
			}

			s32 oldNumberOfGPR = numberOfGPR;
			s32 oldNumberOfXMM = numberOfXMM;
			if (regCount >= 1) {
				IRValue firstSlot = X64PushRegisterParameter(first.typeTableIdx, &numberOfGPR, &numberOfXMM);
				IRValue secondSlot = { IRVALUETYPE_INVALID };
				if (regCount >= 2)
					secondSlot = X64PushRegisterParameter(second.typeTableIdx, &numberOfGPR, &numberOfXMM);

				if (firstSlot.valueType != IRVALUETYPE_INVALID &&
				   (regCount < 2 || secondSlot.valueType != IRVALUETYPE_INVALID)) {
					if (isCaller) {
						X64Mov(context, loc, firstSlot,  first);
						if (secondSlot.valueType != IRVALUETYPE_INVALID)
							X64Mov(context, loc, secondSlot, second);
					}
					else {
						X64Mov(context, loc, first,  firstSlot);
						if (secondSlot.valueType != IRVALUETYPE_INVALID)
							X64Mov(context, loc, second, secondSlot);
					}

					*ArrayAdd(&parameterValues) = firstSlot.valueIdx;
					*ArrayAdd(&parameterValues) = secondSlot.valueIdx;

					continue;
				}
				else {
					// Restore number of used registers and keep going.
					numberOfGPR = oldNumberOfGPR;
					numberOfXMM = oldNumberOfXMM;
				}
			}
		}

		if (paramTypeInfo.size > 8) {
			int sizeLeft = (int)paramTypeInfo.size;
			while (sizeLeft > 0)
			{
				u32 typeTableIdx = TYPETABLEIDX_S8;
				if (sizeLeft > 4)
					typeTableIdx = TYPETABLEIDX_S64;
				else if (sizeLeft > 2)
					typeTableIdx = TYPETABLEIDX_S32;
				else if (sizeLeft > 1)
					typeTableIdx = TYPETABLEIDX_S16;
				param.typeTableIdx = typeTableIdx;

				if (isCaller) {
					IRValue slot = IRValueMemory(jobData->x64SpilledParametersWrite[numberOfSpilled++],
							typeTableIdx);
					X64Mov(context, loc, slot, param);
				}
				else {
					IRValue slot = IRValueMemory(jobData->x64SpilledParametersRead[numberOfSpilled++],
							typeTableIdx);
					X64Mov(context, loc, param, slot);
				}
				param.mem.offset += 8;
				sizeLeft -= 8;
			}
		}
		else {
			IRValue slot;
			if (isCaller) {
				slot = X64PushRegisterParameter(param.typeTableIdx, &numberOfGPR, &numberOfXMM);
				if (slot.valueType == IRVALUETYPE_INVALID)
					slot = IRValueMemory(jobData->x64SpilledParametersWrite[numberOfSpilled++],
							TYPETABLEIDX_S64);

				slot.typeTableIdx = param.typeTableIdx;
				X64Mov(context, loc, slot, param);
			}
			else {
				slot = X64PushRegisterParameter(param.typeTableIdx, &numberOfGPR, &numberOfXMM);
				if (slot.valueType == IRVALUETYPE_INVALID)
					slot = IRValueMemory(jobData->x64SpilledParametersRead[numberOfSpilled++],
							TYPETABLEIDX_S64);

				slot.typeTableIdx = param.typeTableIdx;
				X64Mov(context, loc, param, slot);
			}

			*ArrayAdd(&parameterValues) = slot.valueIdx;
		}
	}

	if (isCaller)
	{
		if (jobData->allocatedParameterCount < numberOfSpilled)
			jobData->allocatedParameterCount = numberOfSpilled;
	}

	return parameterValues;
}

void X64ConvertInstruction(Context *context, IRInstruction inst)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	X64Instruction result = { inst.loc };

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	bool isSigned = false;
	{
		u32 typeTableIdx = TYPETABLEIDX_S64;
		if      (inst.type >= IRINSTRUCTIONTYPE_CompareBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_CompareEnd)
			typeTableIdx = inst.binaryOperation.left.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_BinaryBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_BinaryEnd)
			typeTableIdx = inst.binaryOperation.left.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_UnaryBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_UnaryEnd)
			typeTableIdx = inst.unaryOperation.in.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_CompareJumpBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_CompareJumpEnd)
			typeTableIdx = inst.conditionalJump2.left.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_AssignmentBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_AssignmentEnd)
			typeTableIdx = inst.assignment.dst.typeTableIdx;

		typeTableIdx = StripAllAliases(context, typeTableIdx);
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);

		if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
		{
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));
			isSigned = true;
		}
		else if (typeInfo.typeCategory == TYPECATEGORY_INTEGER)
			isSigned = typeInfo.integerInfo.isSigned;
	}

	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		X64Mov(context, inst.loc, inst.assignment.dst, inst.assignment.src);
		return;
	}
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_FLOAT:
	{
		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
		TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));
		ASSERT(dstType.typeCategory == TYPECATEGORY_FLOATING);
		ASSERT(srcType.typeCategory != TYPECATEGORY_FLOATING);
		// Immediates should be converted to float in previous stages.
		ASSERT(src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER);

		// Extend to 32 bit if smaller
		if (srcType.size < 4) {
			bool isSrcSigned = srcType.typeCategory == TYPECATEGORY_INTEGER &&
				srcType.integerInfo.isSigned;
			IRValue newValue = IRValueNewValue(context, "_cvt_tmp"_s, TYPETABLEIDX_U32,
					VALUEFLAGS_FORCE_REGISTER);
			X64InstructionType extendType = isSrcSigned ? X64_MOVSX : X64_MOVZX;
			X64AddInstruction2(context, inst.loc, extendType, newValue, src);
			src = newValue;
		}

		if (dstType.size == 4)
			X64AddInstruction2(context, inst.loc, X64_CVTSI2SS, dst, src);
		else {
			ASSERT(dstType.size == 8);
			X64AddInstruction2(context, inst.loc, X64_CVTSI2SD, dst, src);
		}
		return;
	}
	case IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT:
	{
		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
		TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));
		ASSERT(dstType.typeCategory != TYPECATEGORY_FLOATING);
		ASSERT(srcType.typeCategory == TYPECATEGORY_FLOATING);
		// X64_CVTTSD2SI and CVTTSD2SI are R-RM
		ASSERT(dst.valueType == IRVALUETYPE_VALUE ||
			   dst.valueType == IRVALUETYPE_MEMORY);
		IRValue newValue = IRValueNewValue(context, "_cvttsd2si_tmp"_s, dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, dst.valueIdx);

		X64InstructionType type;
		if (srcType.size == 4)
			type = X64_CVTTSS2SI;
		else {
			ASSERT(srcType.size == 8);
			type = X64_CVTTSD2SI;
		}
		X64AddInstruction2(context, inst.loc, type, newValue, src);
		X64AddInstruction2(context, inst.loc, X64_MOV, dst, newValue);
		return;
	}
	case IRINSTRUCTIONTYPE_CONVERT_PRECISION:
	{
		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
		TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));
		ASSERT(dstType.typeCategory == TYPECATEGORY_FLOATING);
		ASSERT(srcType.typeCategory == TYPECATEGORY_FLOATING);

		// X64_CVTSD2SS and X64_CVTSS2SD are R-RM
		ASSERT(dst.valueType == IRVALUETYPE_VALUE ||
			   dst.valueType == IRVALUETYPE_MEMORY);
		IRValue newValue = IRValueNewValue(context, "_cvtsd2ss_tmp"_s, dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, dst.valueIdx);

		if (dstType.size == 4) {
			ASSERT(srcType.size == 8);
			X64AddInstruction2(context, inst.loc, X64_CVTSD2SS, newValue, src);
		}
		else {
			ASSERT(dstType.size == 8);
			ASSERT(srcType.size == 4);
			X64AddInstruction2(context, inst.loc, X64_CVTSS2SD, newValue, src);
		}
		X64AddInstruction2(context, inst.loc, X64_MOVSS, dst, newValue);
		return;
	}
	case IRINSTRUCTIONTYPE_SIGN_EXTEND:
	{
		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
		TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));
		ASSERT(dstType.typeCategory != TYPECATEGORY_FLOATING);
		ASSERT(srcType.typeCategory != TYPECATEGORY_FLOATING);
		ASSERT(dstType.size > srcType.size);

		// MOVSXD and MOVSX are R-RM
		IRValue tmp = IRValueNewValue(context, "_movsxd_tmp"_s, dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER);

		X64InstructionType type;
		if (srcType.size == 4)
			type = X64_MOVSXD;
		else {
			ASSERT(srcType.size < 4);
			type = X64_MOVSX;
		}
		X64AddInstruction2(context, inst.loc, type, tmp, src);
		X64AddInstruction2(context, inst.loc, X64_MOV, dst, tmp);
		return;
	}
	case IRINSTRUCTIONTYPE_ZERO_EXTEND:
	{
		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
		TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));
		ASSERT(dstType.typeCategory != TYPECATEGORY_FLOATING);
		ASSERT(srcType.typeCategory != TYPECATEGORY_FLOATING);
		ASSERT(dstType.size > srcType.size);

		if (srcType.size == 4) {
			// x86-64 automatically zero-extends 32 to 64 bits
			// Since either operand could be memory, we first store the value in a 32-bit register
			// then copy its 64-bit counterpart to dst.
			IRValue tmp = IRValueNewValue(context, "_zero_ext_tmp"_s, dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			tmp.typeTableIdx = src.typeTableIdx;
			X64AddInstruction2(context, inst.loc, X64_MOV, tmp, src);
			tmp.typeTableIdx = dst.typeTableIdx;
			X64AddInstruction2(context, inst.loc, X64_MOV, dst, tmp);
		}
		else {
			ASSERT(srcType.size < 4);
			// MOVZXD is R-RM
			IRValue tmp = IRValueNewValue(context, "_movzx_tmp"_s, dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64AddInstruction2(context, inst.loc, X64_MOVZX, tmp, src);
			X64AddInstruction2(context, inst.loc, X64_MOV, dst, tmp);
		}
		return;
	}
	case IRINSTRUCTIONTYPE_TRUNCATE:
	{
		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
		TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));
		ASSERT(dstType.typeCategory != TYPECATEGORY_FLOATING);
		ASSERT(srcType.typeCategory != TYPECATEGORY_FLOATING);
		ASSERT(dstType.size < srcType.size);
		ASSERT(dstType.size <= 8);
		ASSERT(srcType.size <= 8);

		src.typeTableIdx = dst.typeTableIdx;
		X64AddInstruction2(context, inst.loc, X64_MOV, dst, src);
		return;
	}
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		src.typeTableIdx = voidPtrTypeIdx;
		if (src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
			X64Mov(context, inst.loc, dst, src);
		}
		else if (IsValueInMemory(context, dst)) {
			IRValue tmp = IRValueNewValue(context, "_lea_mm_tmp"_s, dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64AddInstruction2(context, inst.loc, X64_LEA, tmp, src);
			X64Mov(context, inst.loc, dst, tmp);
			src = tmp;
		}
		else
			X64AddInstruction2(context, inst.loc, X64_LEA, dst, src);
		return;
	}
	case IRINSTRUCTIONTYPE_ADD:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_ADD;
			goto doRM_RMI;
		case X64FLOATINGTYPE_F32:
			result.type = X64_ADDSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_ADDSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_SUBTRACT:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_SUB;
			goto doRM_RMI;
		case X64FLOATINGTYPE_F32:
			result.type = X64_SUBSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_SUBSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
	{
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			X64Mov(context, inst.loc, inst.unaryOperation.out, inst.unaryOperation.in);
			result.type = X64_NEG;
			goto doRM;
		case X64FLOATINGTYPE_F32:
			result.type = X64_VXORPS;
			result.src  = X64CopyToRegister(context, inst.loc, inst.unaryOperation.in);
			result.src2 = IRValueImmediateF32(context, -0.0);
			result.src2.typeTableIdx = TYPETABLEIDX_128;
			break;
		case X64FLOATINGTYPE_F64:
			result.type = X64_VXORPD;
			result.src  = X64CopyToRegister(context, inst.loc, inst.unaryOperation.in);
			result.src2 = IRValueImmediateF64(context, -0.0);
			result.src2.typeTableIdx = TYPETABLEIDX_128;
			break;
		}
		result.dst = inst.unaryOperation.out;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_BITWISE_AND:
		result.type = X64_AND;
		goto doRM_RMI;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
		result.type = X64_OR;
		goto doRM_RMI;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
		result.type = X64_XOR;
		goto doRM_RMI;
	case IRINSTRUCTIONTYPE_BITWISE_NOT:
		result.type = X64_NOT;
		goto doRM;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
		{
			IRValue left  = inst.binaryOperation.left;
			IRValue right = inst.binaryOperation.right;
			IRValue out   = inst.binaryOperation.out;

			if (left.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && left.immediate > 0 &&
					IsPowerOf264(left.immediate))
			{
				IRValue tmp = left;
				left = right;
				right = tmp;
			}

			if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && right.immediate > 0 &&
					IsPowerOf264(right.immediate))
			{
				u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
				IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx,
						immitateFlag, left.valueIdx);

				X64Mov(context, inst.loc, tmp, left);
				X64AddInstruction2(context, inst.loc, X64_SAL, tmp,
						IRValueImmediate(63 - Nlz64(right.immediate), TYPETABLEIDX_U8));
				X64Mov(context, inst.loc, out, tmp);
				return;
			}
			else {
				if (isSigned) {
					result.type = X64_IMUL;
					goto doRM_RMI;
				}
				else {
					IRValue typedRax = RAX;
					typedRax.typeTableIdx = left.typeTableIdx;
					X64Mov(context, inst.loc, typedRax, left);

					X64AddInstruction2(context, inst.loc, X64_XOR, EDX, EDX);

					IRValue multiplier = right;
					u8 accepted = x64InstructionInfos[X64_MUL].operandTypesLeft;
					if (!FitsInOperand(context, accepted, multiplier)) {
						ASSERT(accepted & OPERANDTYPE_REGISTER);
						IRValue newValue = IRValueNewValue(context, multiplier.typeTableIdx,
								VALUEFLAGS_FORCE_REGISTER);
						X64Mov(context, inst.loc, newValue, multiplier);
						multiplier = newValue;
					}
					result.type = X64_MUL;
					result.dst = multiplier;
					*BucketArrayAdd(&jobData->beInstructions) = result;

					X64Mov(context, inst.loc, out, typedRax);
					return;
				}
			}
		}
		case X64FLOATINGTYPE_F32:
			result.type = X64_MULSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_MULSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_DIVIDE:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
		{
			IRValue left  = inst.binaryOperation.left;
			IRValue right = inst.binaryOperation.right;
			IRValue out   = inst.binaryOperation.out;

			if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && IsPowerOf264(right.immediate)) {
				u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
				IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx,
						immitateFlag, left.valueIdx);

				TypeInfo leftType = GetTypeInfo(context, left.typeTableIdx);
				X64InstructionType shiftType = isSigned ? X64_SAR : X64_SHR;

				X64Mov(context, inst.loc, tmp, left);
				X64AddInstruction2(context, inst.loc, shiftType, tmp,
						IRValueImmediate(63 - Nlz64(right.immediate), TYPETABLEIDX_U8));
				X64Mov(context, inst.loc, out, tmp);
			}
			else {
				X64Mov(context, inst.loc, RAX, left);

				if (isSigned)
					X64AddInstruction0(context, inst.loc, X64_CQO);
				else
					X64AddInstruction2(context, inst.loc, X64_XOR, EDX, EDX);

				IRValue divisor = right;
				u8 accepted = x64InstructionInfos[X64_DIV].operandTypesLeft;
				if (!FitsInOperand(context, accepted, divisor)) {
					ASSERT(accepted & OPERANDTYPE_REGISTER);
					IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
							VALUEFLAGS_FORCE_REGISTER);
					X64Mov(context, inst.loc, newValue, divisor);
					divisor = newValue;
				}
				result.type = isSigned ? X64_IDIV : X64_DIV;
				result.dst = divisor;
				*BucketArrayAdd(&jobData->beInstructions) = result;

				X64Mov(context, inst.loc, out, RAX);
			}
			return;
		}
		case X64FLOATINGTYPE_F32:
			result.type = X64_DIVSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_DIVSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_MODULO:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && IsPowerOf264(right.immediate))
		{
			u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx, immitateFlag,
					left.valueIdx);

			X64Mov(context, inst.loc, tmp, left);
			X64AddInstruction2(context, inst.loc, X64_AND, tmp,
					IRValueImmediate(right.immediate - 1, TYPETABLEIDX_U32));
			X64Mov(context, inst.loc, out, tmp);
		}
		else {
			X64Mov(context, inst.loc, RAX, left);
			if (isSigned)
				X64AddInstruction0(context, inst.loc, X64_CQO);
			else
				X64AddInstruction2(context, inst.loc, X64_XOR, EDX, EDX);

			IRValue divisor = right;
			u8 accepted = x64InstructionInfos[X64_DIV].operandTypesLeft;
			if (!FitsInOperand(context, accepted, divisor)) {
				ASSERT(accepted & OPERANDTYPE_REGISTER);
				IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
						VALUEFLAGS_FORCE_REGISTER);
				X64Mov(context, inst.loc, newValue, divisor);
				divisor = newValue;
			}
			result.type = isSigned ? X64_IDIV : X64_DIV;
			result.dst = divisor;

			*BucketArrayAdd(&jobData->beInstructions) = result;
			X64Mov(context, inst.loc, out, RDX);
		}
		return;
	}
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
	{
		TypeInfo leftType = GetTypeInfo(context, inst.binaryOperation.left.typeTableIdx);
		result.type = isSigned ? X64_SAL : X64_SHL;
		goto doShift;
	}
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
	{
		TypeInfo leftType = GetTypeInfo(context, inst.binaryOperation.left.typeTableIdx);
		result.type = isSigned ? X64_SAR : X64_SHR;
		goto doShift;
	}
	case IRINSTRUCTIONTYPE_LABEL:
		result.type = X64_Label;
		result.label = inst.label;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	case IRINSTRUCTIONTYPE_JUMP:
		result.type = X64_JMP;
		result.label = inst.jump.label;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
		result.type = X64_JE;
		goto doConditionalJump;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
		result.type = X64_JNE;
		goto doConditionalJump;
	case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
		result.type = X64_JE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
		result.type = X64_JNE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JG;
		else
			result.type = X64_JA;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JL;
		else
			result.type = X64_JB;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JGE;
		else
			result.type = X64_JAE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JLE;
		else
			result.type = X64_JBE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETG;
		else
			result.type = X64_SETA;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETL;
		else
			result.type = X64_SETB;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETGE;
		else
			result.type = X64_SETAE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETBE;
		else
			result.type = X64_SETBE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_EQUALS:
		result.type = X64_SETE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_NOT_EQUALS:
		result.type = X64_SETNE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_NOT:
	{
		X64Test(context, inst.loc, inst.unaryOperation.in);

		result.type = X64_SETE;
		result.dst = inst.unaryOperation.out;
		if (GetTypeInfo(context, result.dst.typeTableIdx).size != 1)
		{
			X64Mov(context, inst.loc, result.dst, IRValueImmediate(0, TYPETABLEIDX_U8));
			result.dst.typeTableIdx = TYPETABLEIDX_S8;
		}

		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT:
	{
		CallingConvention callingConvention;

		// At this point, we have the actual values that go into registers/stack slots. If something
		// is passed by copy, we already have the pointer to the copy as argument value, so we don't
		// care.
		if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
		{
			ASSERT(inst.procedureCall.procedureIdx != 0);
			result.type = X64_CALL;
			result.procedureIdx = inst.procedureCall.procedureIdx;

			u32 procTypeIdx = GetProcedureRead(context, result.procedureIdx).typeTableIdx;
			callingConvention = GetTypeInfo(context, procTypeIdx).procedureInfo.callingConvention;
		}
		else
		{
			result.type = X64_CALL_Indirect;
			result.procedureIRValue = inst.procedureCall.procIRValue;

			u32 procTypeIdx = inst.procedureCall.procIRValue.typeTableIdx;
			callingConvention = GetTypeInfo(context, procTypeIdx).procedureInfo.callingConvention;
		}

		// At worst each parameter should add 2 values to this array, this is why we multiply
		// capacity by 2.
		// @Improve: dynamic array.
		ArrayInit(&result.parameterValues, inst.procedureCall.parameters.size * 2);

		FixedArray<IRValue, 32> paramSources;
		paramSources.size = 0;

		bool isReturnByCopy = false;

		if (inst.procedureCall.returnValues.size)
		{
			IRValue returnValue = inst.procedureCall.returnValues[0];
			isReturnByCopy = IRShouldPassByCopy(context, returnValue.typeTableIdx);
			if (isReturnByCopy)
			{
				static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

				u32 tmpValueIdx = IRNewValue(context, "returncpy"_s, returnValue.typeTableIdx, 0);
				IRValue tmpValue = IRValueValue(tmpValueIdx, voidPtrTypeIdx);

				X64Instruction pushInst = { inst.loc, X64_Push_Value };
				pushInst.valueIdx = tmpValueIdx;
				*BucketArrayAdd(&jobData->beInstructions) = pushInst;

				IRValue ptr = IRValueNewValue(context, "returnptr"_s, TYPETABLEIDX_S64, 0);
				X64AddInstruction2(context, inst.loc, X64_LEA, ptr, tmpValue);

				*FixedArrayAdd(&paramSources) = ptr;
			}
		}

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
			*FixedArrayAdd(&paramSources) = inst.procedureCall.parameters[i];

		Array<u32, ThreadAllocator> paramValues;
		switch (callingConvention)
		{
			case CC_WIN64:
				paramValues =
					X64ReadyWin64Parameters(context, inst.loc, paramSources, true);
				break;
			case CC_DEFAULT:
			case CC_LINUX64:
			default:
				paramValues =
					X64ReadyLinuxParameters(context, inst.loc, paramSources, true);
		}

		result.parameterValues.data = paramValues.data;
		result.parameterValues.size = paramValues.size;
#if DEBUG_BUILD
		result.parameterValues._capacity = paramValues._capacity;
#endif

#if IS_LINUX
		// Check syscalls
		if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
		{
			String procName = GetProcedureRead(context, inst.procedureCall.procedureIdx).name;
			int syscallCount = ArrayCount(x64LinuxSyscallNames);
			for (int i = 0; i < syscallCount; ++i)
			{
				if (StringEquals(procName, x64LinuxSyscallNames[i]))
				{
					result.type = X64_SYSCALL;
					X64Mov(context, inst.loc, RAX, IRValueImmediate(i, TYPETABLEIDX_U32));
					break;
				}
			}
		}
#endif

		*BucketArrayAdd(&jobData->beInstructions) = result;

		u64 returnValueCount = inst.procedureCall.returnValues.size;
		if (returnValueCount) {
			if (callingConvention == CC_DEFAULT) {
				static IRValue integerReturnRegisters[]  = { RAX, RDI, RSI, RDX, RCX, R8, R9 };
				static IRValue floatingReturnRegisters[] = {
					XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8 };

				int integerIdx = 0;
				int floatingIdx = 0;
				for (int i = 0; i < returnValueCount; ++i) {
					IRValue slot;
					IRValue out = inst.procedureCall.returnValues[i];
					u32 returnTypeIdx = out.typeTableIdx;
					TypeInfo returnTypeInfo = GetTypeInfo(context, returnTypeIdx);
					if (returnTypeInfo.typeCategory == TYPECATEGORY_FLOATING) {
						slot = floatingReturnRegisters[floatingIdx++];
						slot.typeTableIdx = out.typeTableIdx;
						X64Mov(context, inst.loc, out, slot);
					}
					else {
						slot = integerReturnRegisters[integerIdx++];
						slot.typeTableIdx = out.typeTableIdx;

						if (IRShouldPassByCopy(context, returnTypeIdx) &&
								out.valueType != IRVALUETYPE_IMMEDIATE_INTEGER)
						{
							IRValue ptr = IRValueNewValue(context, "retptr"_s, TYPETABLEIDX_S64, 0);
							IRValue outButS64 = out;
							outButS64.typeTableIdx = TYPETABLEIDX_S64;
							X64AddInstruction2(context, inst.loc, X64_LEA, ptr, outButS64);

							X64CopyMemory(context, inst.loc, ptr, slot,
									IRValueImmediate(returnTypeInfo.size));
						}
						else
							X64Mov(context, inst.loc, out, slot);
					}
				}
			}
			else {
				ASSERT(returnValueCount == 1);
				IRValue out = inst.procedureCall.returnValues[0];
				u32 returnTypeIdx = out.typeTableIdx;
				if (IRShouldPassByCopy(context, returnTypeIdx) &&
						out.valueType != IRVALUETYPE_IMMEDIATE_INTEGER) {
					IRValue ptr = IRValueNewValue(context, "retptr"_s, TYPETABLEIDX_S64, 0);
					IRValue outButS64 = out;
					outButS64.typeTableIdx = TYPETABLEIDX_S64;
					X64AddInstruction2(context, inst.loc, X64_LEA, ptr, outButS64);

					TypeInfo returnTypeInfo = GetTypeInfo(context, returnTypeIdx);
					X64CopyMemory(context, inst.loc, ptr, RAX,
							IRValueImmediate(returnTypeInfo.size));
				}
				else if (GetTypeInfo(context, returnTypeIdx).typeCategory == TYPECATEGORY_FLOATING) {
					IRValue typedXmm0 = XMM0;
					typedXmm0.typeTableIdx = returnTypeIdx;
					X64Mov(context, inst.loc, out, typedXmm0);
				}
				else {
					IRValue typedRax = RAX;
					typedRax.typeTableIdx = returnTypeIdx;
					X64Mov(context, inst.loc, out, typedRax);
				}
			}
		}
		return;
	}
	case IRINSTRUCTIONTYPE_INTRINSIC:
	{
		switch (inst.intrinsic.type) {
		case INTRINSIC_BREAKPOINT:
			X64AddInstruction0(context, inst.loc, X64_INT3);
			return;
		case INTRINSIC_SQRT32:
			result.type = X64_SQRTSS;
			goto doTwoArgIntrinsic;
		case INTRINSIC_SQRT64:
			result.type = X64_SQRTSD;
			goto doTwoArgIntrinsic;
		default:
			ASSERT(!"Invalid intrinsic");
		}
		return;
	}
	case IRINSTRUCTIONTYPE_COPY_MEMORY:
	{
		X64CopyMemory(context, inst.loc, inst.copyMemory.dst, inst.copyMemory.src,
				inst.copyMemory.size);
		return;
	}
	case IRINSTRUCTIONTYPE_ZERO_MEMORY:
	{
		ASSERT(inst.zeroMemory.dst.valueType  == IRVALUETYPE_VALUE ||
			   inst.zeroMemory.dst.valueType  == IRVALUETYPE_MEMORY);
		u32 dstIdx = inst.zeroMemory.dst.valueIdx;

		// First attempt to zero manually
		if (inst.zeroMemory.size.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			TypeInfo dstTypeInfo = GetTypeInfo(context, IRGetValue(context, dstIdx).typeTableIdx);
			s64 size = inst.zeroMemory.size.immediate;

			s64 copiedBytes = 0;
			if (size - copiedBytes >= 16) {
				IRValue zeroXmmReg = IRValueNewValue(context, "_zeroxmm"_s, TYPETABLEIDX_128,
						VALUEFLAGS_FORCE_REGISTER);
				X64AddInstruction2(context, inst.loc, X64_XORPS, zeroXmmReg, zeroXmmReg);
				while (size - copiedBytes >= 16) {
					X64Mov(context, inst.loc,
							IRValueMemory(dstIdx, TYPETABLEIDX_128, copiedBytes), zeroXmmReg);
					copiedBytes += 16;
				}
			}
			if (size - copiedBytes >= 1) {
				IRValue zeroReg = IRValueNewValue(context, "_zeroreg"_s, TYPETABLEIDX_U32,
						VALUEFLAGS_FORCE_REGISTER);
				X64AddInstruction2(context, inst.loc, X64_XOR, zeroReg, zeroReg);
				zeroReg.typeTableIdx = TYPETABLEIDX_U64;
				while (size - copiedBytes >= 8) {
					X64Mov(context, inst.loc,
							IRValueMemory(dstIdx, TYPETABLEIDX_U64, copiedBytes), zeroReg);
					copiedBytes += 8;
				}
				zeroReg.typeTableIdx = TYPETABLEIDX_U32;
				while (size - copiedBytes >= 4) {
					X64Mov(context, inst.loc,
							IRValueMemory(dstIdx, TYPETABLEIDX_U32, copiedBytes), zeroReg);
					copiedBytes += 4;
				}
				zeroReg.typeTableIdx = TYPETABLEIDX_U8;
				while (size - copiedBytes >= 1) {
					X64Mov(context, inst.loc,
							IRValueMemory(dstIdx, TYPETABLEIDX_U8, copiedBytes), zeroReg);
					++copiedBytes;
				}
			}
		}
		else {
			ASSERT(g_zeroMemoryProcIdx != U32_MAX);
			X64Mov(context, inst.loc, RCX, inst.zeroMemory.dst);
			X64Mov(context, inst.loc, RDX, inst.zeroMemory.size);
			result.type = X64_CALL;
			result.procedureIdx = g_zeroMemoryProcIdx;
			ArrayInit(&result.parameterValues, 2);
			*ArrayAdd(&result.parameterValues) = RCX.valueIdx;
			*ArrayAdd(&result.parameterValues) = RDX.valueIdx;
			*BucketArrayAdd(&jobData->beInstructions) = result;
		}
	}
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
		result.type = X64_Push_Scope;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
		result.type = X64_Pop_Scope;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
		result.type = X64_Push_Value;
		result.valueIdx = inst.pushValue.valueIdx;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	case IRINSTRUCTIONTYPE_COMMENT:
		result.type = X64_Comment;
		result.comment = inst.comment;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	case IRINSTRUCTIONTYPE_RETURN:
		return;
	case IRINSTRUCTIONTYPE_COMPILER_BREAKPOINT:
		BREAK;
		return;
	default:
		ASSERT(!"Unrecognized IR instruction type");
		return;
	}

doRM:
	{
		IRValue operand = inst.unaryOperation.out;

		if (operand.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			(operand.immediate & 0xFFFFFFFF00000000))
		{
			IRValue tmp = IRValueNewValue(context, operand.typeTableIdx, 0);
			X64Mov(context, inst.loc, tmp, operand);
			operand = tmp;
		}

		X64Mov(context, inst.loc, operand, inst.unaryOperation.in);

		result.dst = operand;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		return;
	}
doRM_RMI:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, "_rmrmitmp"_s, left.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | immitateFlag, left.valueIdx);

		X64MovNoTmp(context, inst.loc, tmp, left);

		u8 accepted = x64InstructionInfos[result.type].operandTypesRight;
		if (!FitsInOperand(context, accepted, right)) {
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			right = X64CopyToRegister(context, inst.loc, right);
		}

		X64ReduceRM64BitImmediate(context, inst.loc, left.typeTableIdx, &right);

		if (result.type == X64_IMUL)
			ASSERT(GetTypeInfo(context, left.typeTableIdx).size == GetTypeInfo(context, right.typeTableIdx).size);

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		X64Mov(context, inst.loc, out, tmp);

		return;
	}
doX_XM:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out = inst.binaryOperation.out;

		u32 immitateFlagLeft = out.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, left.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | immitateFlagLeft, out.valueIdx);

		X64MovNoTmp(context, inst.loc, tmp, left);

		u8 accepted = x64InstructionInfos[result.type].operandTypesRight;
		if (!FitsInOperand(context, accepted, right) ||
			right.typeTableIdx != left.typeTableIdx)
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagRight = right.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, out.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagRight, right.valueIdx);
			X64Mov(context, inst.loc, newValue, right);
			right = newValue;
		}

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		X64Mov(context, inst.loc, out, tmp);

		return;
	}
doShift:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, 0);

		X64Mov(context, inst.loc, tmp, left);

		if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
			right.typeTableIdx = TYPETABLEIDX_U8;
		else {
			IRValue typedRcx = RCX;
			typedRcx.typeTableIdx = right.typeTableIdx;
			X64Mov(context, inst.loc, typedRcx, right);
			right = CL;
		}

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		X64Mov(context, inst.loc, out, tmp);

		return;
	}
doConditionalJump:
	{
		X64Test(context, inst.loc, inst.conditionalJump.condition);

		result.label = inst.conditionalJump.label;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	}
doConditionalJump2:
	{
		X64Instruction cmpInst;
		cmpInst.loc = inst.loc;

		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			cmpInst.type = X64_CMP;
			break;
		case X64FLOATINGTYPE_F32:
			cmpInst.type = X64_COMISS;
			break;
		case X64FLOATINGTYPE_F64:
			cmpInst.type = X64_COMISD;
			break;
		default:
			ASSERT(false);
		}

		IRValue left  = inst.conditionalJump2.left;
		u8 accepted = x64InstructionInfos[cmpInst.type].operandTypesLeft;
		if (!FitsInOperand(context, accepted, left))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagLeft = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, "_jump_hlp"_s, left.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagLeft, left.valueIdx);
			X64Mov(context, inst.loc, newValue, left);
			left = newValue;
		}

		IRValue right = inst.conditionalJump2.right;
		X64ReduceRM64BitImmediate(context, inst.loc, left.typeTableIdx, &right);
		accepted = x64InstructionInfos[cmpInst.type].operandTypesRight;
		if (!FitsInOperand(context, accepted, right) ||
			right.typeTableIdx != left.typeTableIdx ||
			(IsValueInMemory(context, left) && IsValueInMemory(context, right)))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagRight = right.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, "_jump_hlp"_s, left.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagRight, right.valueIdx);
			X64Mov(context, inst.loc, newValue, right);
			right = newValue;
		}
		cmpInst.dst = left;
		cmpInst.src = right;

		result.label = inst.conditionalJump2.label;

		*BucketArrayAdd(&jobData->beInstructions) = cmpInst;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	}
doConditionalSet:
	{
		X64Instruction cmpInst;
		cmpInst.loc = inst.loc;
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			cmpInst.type = X64_CMP;
			break;
		case X64FLOATINGTYPE_F32:
			cmpInst.type = X64_COMISS;
			break;
		case X64FLOATINGTYPE_F64:
			cmpInst.type = X64_COMISD;
			break;
		default:
			ASSERT(false);
		}

		cmpInst.dst = inst.binaryOperation.left;
		cmpInst.src = inst.binaryOperation.right;

		X64ReduceRM64BitImmediate(context, inst.loc, cmpInst.dst.typeTableIdx, &cmpInst.src);

		u8 accepted = x64InstructionInfos[cmpInst.type].operandTypesLeft;
		if (!FitsInOperand(context, accepted, cmpInst.dst) ||
			(IsValueInMemory(context, cmpInst.dst) && IsValueInMemory(context, cmpInst.src)))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			IRValue newValue = IRValueNewValue(context, "_setcc_hlp"_s, cmpInst.dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, inst.loc, newValue, cmpInst.dst);
			cmpInst.dst = newValue;
		}

		*BucketArrayAdd(&jobData->beInstructions) = cmpInst;

		result.dst = inst.binaryOperation.out;
		if (GetTypeInfo(context, result.dst.typeTableIdx).size != 1)
		{
			X64Mov(context, inst.loc, result.dst, IRValueImmediate(0, TYPETABLEIDX_U8));
			result.dst.typeTableIdx = TYPETABLEIDX_S8;
		}
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	}
doTwoArgIntrinsic:
	{
		IRValue left  = inst.intrinsic.parameters[0];
		IRValue right = inst.intrinsic.parameters[1];
		IRValue out   = inst.intrinsic.parameters[0];

		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, 0);

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		X64Mov(context, inst.loc, out, tmp);
		return;
	}
}

String X64InstructionToStr(Context *context, X64Instruction inst,
	BucketArray<Value, LinearAllocator, 256> *localValues)
{
	String mnemonic = x64InstructionInfos[inst.type].mnemonic;
	switch (inst.type)
	{
	case X64_CALL:
		return TPrintF("call %S", GetProcedureRead(context, inst.procedureIdx).name);
	case X64_CALL_Indirect:
	{
		String proc = X64IRValueToStr(context, inst.procedureIRValue, localValues);
		return TPrintF("call %S", proc);
	}
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
	case X64_JG:
	case X64_JL:
	case X64_JGE:
	case X64_JLE:
	case X64_JA:
	case X64_JB:
	case X64_JAE:
	case X64_JBE:
		goto printLabel;
	case X64_Label:
		return TPrintF("L_%S_%llx:", inst.label->name, (u64)inst.label);
	case X64_Comment:
		return TPrintF("; %S", inst.comment);
	case X64_Ignore:
	case X64_Push_Scope:
	case X64_Pop_Scope:
	case X64_Push_Value:
		return {};
	default:
	{
		X64InstructionInfo instInfo = x64InstructionInfos[inst.type];
		if (instInfo.operandTypesLeft != OPERANDTYPE_NONE) {
			if (instInfo.operandTypesRight != OPERANDTYPE_NONE) {
				if (instInfo.operandTypesDest != OPERANDTYPE_NONE)
					goto printDstSrcSrc2;
				else
					goto printDstSrc;
			}
			else
				goto printDst;
		}
		else
			return mnemonic;
	}
	}

printDst:
	{
		String dst = X64IRValueToStr(context, inst.dst, localValues);
		return TPrintF("%S %S", mnemonic, dst);
	}
printDstSrc:
	{
		String dst = X64IRValueToStr(context, inst.dst, localValues);
		String src = X64IRValueToStr(context, inst.src, localValues);
		return TPrintF("%S %S, %S", mnemonic, dst, src);
	}
printDstSrcSrc2:
	{
		String dst  = X64IRValueToStr(context, inst.dst,  localValues);
		String src  = X64IRValueToStr(context, inst.src,  localValues);
		String src2 = X64IRValueToStr(context, inst.src2, localValues);
		return TPrintF("%S %S, %S, %S", mnemonic, dst, src, src2);
	}
printLabel:
	{
		return TPrintF("%S L_%S_%llx", mnemonic, inst.label->name, (u64)inst.label);
	}
}

xed_encoder_operand_t X64IRValueToXEDOperand(Context *context, SourceLocation loc, IRValue value,
		Relocation *displacementRelocation,
		BucketArray<Value, LinearAllocator, 256> *localValues)
{
	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_FLOAT);
	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_STRING);

	xed_encoder_operand_t result = {};

	u64 size = 0;
	TypeInfo typeInfo = GetTypeInfo(context, StripAllAliases(context, value.typeTableIdx));
	bool isXMM;
	size = typeInfo.size;
	Value v;
	s64 offset = 0;

	int bitWidth = (int)size * 8;

	if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		return xed_imm0(value.immediate, bitWidth);

	if (value.valueType == IRVALUETYPE_PROCEDURE) {
		// @Improve: relocation here, don't do special handling for CALL

		// There can only be one displacement per instruction
		ASSERT(displacementRelocation->type == RELOCATIONTYPE_INVALID);
		displacementRelocation->type = RELOCATIONTYPE_PROCEDURE;
		displacementRelocation->procedureIdx = value.procedureIdx;

		return xed_mem_bd(XED_REG_RIP, xed_disp(0, 32), 64);
	}

	if (value.valueType == IRVALUETYPE_MEMORY)
		offset = value.mem.offset;

	if (value.valueIdx & VALUE_GLOBAL_BIT)
		v = GetGlobalValue(context, value.valueIdx);
	else
		v = (*localValues)[value.valueIdx];

	if (v.flags & (VALUEFLAGS_ON_STATIC_STORAGE | VALUEFLAGS_IS_EXTERNAL)) {
		if (!(v.flags & VALUEFLAGS_IS_EXTERNAL)) {
			// There can only be one displacement per instruction
			ASSERT(displacementRelocation->type == RELOCATIONTYPE_INVALID);
			displacementRelocation->type = RELOCATIONTYPE_STATIC_DATA;

			u8 *ptrToStaticData = *(u8 **)HashMapGet(context->globalValueContents,
					value.valueIdx & VALUE_GLOBAL_MASK);
			ASSERT(ptrToStaticData >= STATIC_DATA_VIRTUAL_ADDRESS &&
					ptrToStaticData < STATIC_DATA_VIRTUAL_ADDRESS_END);
			offset += (u64)(ptrToStaticData - STATIC_DATA_VIRTUAL_ADDRESS);
		}

		result = xed_mem_bd(XED_REG_RIP, xed_disp(offset, 32), bitWidth);
		goto doIndexScale;
	}

	isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;

	ASSERT(v.flags & VALUEFLAGS_IS_ALLOCATED);
	if (v.flags & VALUEFLAGS_IS_MEMORY) {
		ASSERT(!(v.flags & VALUEFLAGS_FORCE_REGISTER));
		offset += v.stackOffset;
		xed_reg_enum_t base;
		if (v.flags & VALUEFLAGS_BASE_RELATIVE)
			base = XED_REG_RBP;
		else
			base = XED_REG_RSP;

		int offsetWidth = 32;
		if ((s8)offset == offset)
			offsetWidth = 8;

		result = xed_mem_bd(base, xed_disp(offset, offsetWidth), bitWidth);
	}
	else if (value.valueType == IRVALUETYPE_MEMORY) {
		ASSERTF(v.allocatedRegister <= R15_idx, "Value \"%S\" not allocated to GP register!", v.name);
		xed_reg_enum_t base = x64RegisterToXED[v.allocatedRegister];

		int offsetWidth = 32;
		if ((s8)offset == offset)
			offsetWidth = 8;

		result = xed_mem_bd(base, xed_disp(offset, offsetWidth), bitWidth);
	}
	else if (!isXMM) {
		if (v.allocatedRegister >= XMM0_idx)
#if DEBUG_BUILD
			LogCompilerError(context, loc, TPrintF("Value \"%S\" not allocated to GP register!",
						v.name));
#else
			LogCompilerError(context, loc, "Value not allocated to GP register!"_s);
#endif
		xed_reg_enum_t reg;
		switch (size) {
		case 8:
			reg = x64RegisterToXED  [v.allocatedRegister]; break;
		case 4:
			reg = x64RegisterToXED32[v.allocatedRegister]; break;
		case 2:
			reg = x64RegisterToXED16[v.allocatedRegister]; break;
		case 1:
			reg = x64RegisterToXED8 [v.allocatedRegister]; break;
		default:
			ASSERTF(false, "Invalid size for a register!");
		}
		ASSERT(offset == 0);
		result = xed_reg(reg);
	}
	else {
#if DEBUG_BUILD
		ASSERTF(v.allocatedRegister >= XMM0_idx && v.allocatedRegister <= XMM15_idx,
				"Value \"%S\" not allocated to XMM register!", v.name);
#else
		ASSERTF(v.allocatedRegister >= XMM0_idx && v.allocatedRegister <= XMM15_idx,
				"Value not allocated to XMM register!");
#endif
		result = xed_reg(x64RegisterToXED[v.allocatedRegister]);
	}

doIndexScale:
	if (result.u.mem.disp.displacement == 0)
		result.u.mem.disp.displacement_bits = 0;
	if (value.valueType == IRVALUETYPE_MEMORY && value.mem.elementSize > 0) {
		ASSERT(!(value.valueIdx & VALUE_GLOBAL_BIT));
		Value indexValue = (*localValues)[value.mem.indexValueIdx];
		ASSERT(indexValue.flags & VALUEFLAGS_IS_ALLOCATED);
		ASSERT(!(indexValue.flags & VALUEFLAGS_IS_MEMORY));
		xed_reg_enum_t xedIndex = x64RegisterToXED[indexValue.allocatedRegister];

		ASSERT(result.type == XED_ENCODER_OPERAND_TYPE_MEM);
		result.u.mem.index = xedIndex;
		result.u.mem.scale = (u32)value.mem.elementSize;
	}

	return result;
}

int X64InstructionToBytes(Context *context, X64Instruction x64Inst,
	BucketArray<Value, LinearAllocator, 256> *localValues, u8 *buffer)
{
	Relocation displacementRelocation = {};

	xed_iclass_enum_t xedIClass = x64InstructionInfos[x64Inst.type].xedIClass;

	xed_state_t dstate64 = {};
	dstate64.stack_addr_width = XED_ADDRESS_WIDTH_64b;
	dstate64.mmode = XED_MACHINE_MODE_LONG_64;

	xed_encoder_instruction_t inst;

	switch (x64Inst.type)
	{
	case X64_CALL:
	{
		if (!(x64Inst.procedureIdx & PROCEDURE_EXTERNAL_BIT)) {
			// I believe all near call instructions only have 1 byte before the displacement
			*DynamicArrayAdd(&g_relocations) = {
				.type = RELOCATIONTYPE_PROCEDURE,
				.procedureIdx = x64Inst.procedureIdx,
				.destOffset = context->outputBufferOffset + 1
			};
		}
		else {
			*DynamicArrayAdd(&g_relocations) = {
				.type = RELOCATIONTYPE_EXTERNAL_PROCEDURE,
				.procedureIdx = x64Inst.procedureIdx & PROCEDURE_EXTERNAL_MASK,
				.destOffset = context->outputBufferOffset + 1
			};
		}

		xed_encoder_operand_t dst = xed_relbr(0, 32);
		xed_inst1(&inst, dstate64, xedIClass, 0, dst);
		goto encode;
	}
	case X64_CALL_Indirect:
	{
		xed_encoder_operand_t dst = X64IRValueToXEDOperand(context, x64Inst.loc,
				x64Inst.procedureIRValue, &displacementRelocation, localValues);
		xed_inst1(&inst, dstate64, xedIClass, 64, dst);
		goto encode;
	}
	case X64_JMP:
	{
		// Jmp has 1 byte of opcode before the offset. Same as CALL, only 64 bit jump far has a
		// REX.W prefix.
		*DynamicArrayAdd(&g_relocations) = {
			.type = RELOCATIONTYPE_LABEL,
			.label = x64Inst.label,
			.destOffset = context->outputBufferOffset + 1
		};
		// @Todo: smaller displacement bitwidth when possible
		xed_encoder_operand_t disp = xed_relbr(0xCCCCCCCC, 32);
		xed_inst1(&inst, dstate64, xedIClass, 0, disp);
		goto encode;
	}
	case X64_JE:
	case X64_JNE:
	case X64_JG:
	case X64_JL:
	case X64_JGE:
	case X64_JLE:
	case X64_JA:
	case X64_JB:
	case X64_JAE:
	case X64_JBE:
	{
		// All jumps with 32 bit offsets seem to have a 2-byte opcode.
		// @Important: if we implement short jumps (8bit displacement) the opcode is 1 byte! Change
		// it on the relocation record!
		*DynamicArrayAdd(&g_relocations) = {
			.type = RELOCATIONTYPE_LABEL,
			.label = x64Inst.label,
			.destOffset = context->outputBufferOffset + 2
		};
		// @Todo: smaller displacement bitwidth when possible
		xed_encoder_operand_t disp = xed_relbr(0xCCCCCCCC, 32);
		xed_inst1(&inst, dstate64, xedIClass, 0, disp);
		goto encode;
	}
	case X64_Label:
		x64Inst.label->address = context->outputBufferOffset;
		return 0;
	case X64_Comment:
	case X64_Ignore:
	case X64_Push_Scope:
	case X64_Pop_Scope:
	case X64_Push_Value:
		return 0;
	case X64_LEAVE:
	case X64_RET:
		// inst0 with 64 bit width
		xed_inst0(&inst, dstate64, xedIClass, 64);
		goto encode;
	default:
	{
		X64InstructionInfo instInfo = x64InstructionInfos[x64Inst.type];
		if (instInfo.operandTypesLeft != OPERANDTYPE_NONE) {
			if (instInfo.operandTypesRight != OPERANDTYPE_NONE) {
				if (instInfo.operandTypesDest != OPERANDTYPE_NONE)
					goto inst3;
				else
					goto inst2;
			}
			else
				goto inst1;
		}
		else
			goto inst0;
	}
	}

inst0:
	{
		int bitWidth = 0;
		xed_inst0(&inst, dstate64, xedIClass, bitWidth);
		goto encode;
	}
inst1:
	{
		int bitWidth = (int)GetTypeInfo(context, x64Inst.dst.typeTableIdx).size * 8;
		xed_encoder_operand_t dst = X64IRValueToXEDOperand(context, x64Inst.loc, x64Inst.dst,
				&displacementRelocation, localValues);
		xed_inst1(&inst, dstate64, xedIClass, bitWidth, dst);
		goto encode;
	}
inst2:
	{
		int bitWidth = (int)GetTypeInfo(context, x64Inst.dst.typeTableIdx).size * 8;
		xed_encoder_operand_t dst = X64IRValueToXEDOperand(context, x64Inst.loc, x64Inst.dst,
				&displacementRelocation, localValues);
		xed_encoder_operand_t src = X64IRValueToXEDOperand(context, x64Inst.loc, x64Inst.src,
				&displacementRelocation, localValues);
		xed_inst2(&inst, dstate64, xedIClass, bitWidth, dst, src);
		goto encode;
	}
inst3:
	{
		int bitWidth = (int)GetTypeInfo(context, x64Inst.dst.typeTableIdx).size * 8;
		xed_encoder_operand_t dst  = X64IRValueToXEDOperand(context, x64Inst.loc, x64Inst.dst,
				&displacementRelocation, localValues);
		xed_encoder_operand_t src  = X64IRValueToXEDOperand(context, x64Inst.loc, x64Inst.src,
				&displacementRelocation, localValues);
		xed_encoder_operand_t src2 = X64IRValueToXEDOperand(context, x64Inst.loc, x64Inst.src2,
				&displacementRelocation, localValues);
		xed_inst3(&inst, dstate64, xedIClass, bitWidth, dst, src, src2);
		goto encode;
	}
encode:
	{
		xed_encoder_request_t req;
		xed_convert_to_encoder_request(&req, &inst);
		unsigned int len;
		xed_error_enum_t error = xed_encode(&req, buffer, 16, &len);
		if (error != XED_ERROR_NONE)
			LogCompilerError(context, x64Inst.loc, "Could not encode instruction"_s);
		ASSERT(len > 0 && len < 16);

		if (displacementRelocation.type != RELOCATIONTYPE_INVALID) {
			s32 sizeOfImmediate = xed_decoded_inst_get_immediate_width(&req);
			displacementRelocation.destOffset = context->outputBufferOffset + len;
			displacementRelocation.destOffset -= sizeOfImmediate;
			displacementRelocation.destOffset -= 4; // Size of displacement
			displacementRelocation.offsetShift = sizeOfImmediate;
			*DynamicArrayAdd(&g_relocations) = displacementRelocation;
		}

		return len;
	}
}

inline s64 X64PrintInstruction(Context *context, X64Instruction inst,
	BucketArray<Value, LinearAllocator, 256> *localValues)
{
	String instructionStr = X64InstructionToStr(context, inst, localValues);
	return OutputBufferPut(context, instructionStr.size, instructionStr.data);
}

#include "X64RegisterAllocation.cpp"

void X64PrintInstructions(Context *context)
{
	auto beFinalProcedureData = context->beFinalProcedureData.GetForRead();
	int procCount = (int)beFinalProcedureData->size;
	for (int finalProcIdx = 0; finalProcIdx < procCount; ++finalProcIdx)
	{
		X64FinalProcedure finalProc = beFinalProcedureData[finalProcIdx];
		Procedure proc = GetProcedureRead(context, finalProc.procedureIdx);
#if IS_WINDOWS
		if (proc.isExported)
			OutputBufferPrint(context, "\n%S PROC PUBLIC\n", proc.name);
		else
			OutputBufferPrint(context, "\n%S PROC PRIVATE\n", proc.name);
#else
		OutputBufferPrint(context, "\n%S:\n", proc.name);
#endif
		OutputBufferPrint(context, "push rbp\n");
		OutputBufferPrint(context, "mov rbp, rsp\n");
		if (finalProc.stackSize > 0)
			OutputBufferPrint(context, "sub rsp, 0%llxh\n", finalProc.stackSize);

		X64InstructionStream stream = X64InstructionStreamBegin(&finalProc.instructions);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			if (X64PrintInstruction(context, *inst, &finalProc.localValues))
				OutputBufferPrint(context, "\n");
			inst = X64InstructionStreamAdvance(&stream);
		}

		OutputBufferPrint(context, "leave\n");
		OutputBufferPrint(context, "ret\n");
#if IS_WINDOWS
		OutputBufferPut(context, proc.name.size, proc.name.data);
		OutputBufferPrint(context, " ENDP\n");
#endif
	}
}

void X64EncodeInstructions(Context *context)
{
	auto beFinalProcedureData = context->beFinalProcedureData.GetForRead();
	int procCount = (int)beFinalProcedureData->size;
	for (int finalProcIdx = 0; finalProcIdx < procCount; ++finalProcIdx)
	{
		X64FinalProcedure finalProc = beFinalProcedureData[finalProcIdx];
		Procedure proc = GetProcedureRead(context, finalProc.procedureIdx);

		g_procedureAddresses[finalProc.procedureIdx] = context->outputBufferOffset;

		u8 buffer[16];
		int bytes;

		// push rbp
		bytes = X64InstructionToBytes(context, { {}, X64_PUSH, RBP }, nullptr, buffer);
		OutputBufferPut(context, bytes, buffer);
		// mov rbp, rsp
		bytes = X64InstructionToBytes(context, { {}, X64_MOV, RBP, RSP }, nullptr, buffer);
		OutputBufferPut(context, bytes, buffer);
		if (finalProc.stackSize > 0) {
			// sub rsp, $stack_size
			bytes = X64InstructionToBytes(context, { {}, X64_SUB, RSP,
					IRValueImmediate(finalProc.stackSize, TYPETABLEIDX_U32) },
					nullptr, buffer);
			OutputBufferPut(context, bytes, buffer);
		}

		X64InstructionStream stream = X64InstructionStreamBegin(&finalProc.instructions);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			bytes = X64InstructionToBytes(context, *inst, &finalProc.localValues, buffer);
			OutputBufferPut(context, bytes, buffer);
			inst = X64InstructionStreamAdvance(&stream);
		}

		// leave
		bytes = X64InstructionToBytes(context, { {}, X64_LEAVE }, nullptr, buffer);
		OutputBufferPut(context, bytes, buffer);
		// ret
		bytes = X64InstructionToBytes(context, { {}, X64_RET }, nullptr, buffer);
		OutputBufferPut(context, bytes, buffer);
	}
}

void PrintOutEscapedString(Context *context, String str)
{
	bool first = true;
	char *buffer = (char *)t_threadMemPtr;
	char *out = buffer;
	const u8 *in = (const u8 *)str.data;
	for (int i = 0; i < str.size; ++i) {
		// MASM uses ' as string delimiters, so we escape them
		if (*in < 32 || *in == '\'') {
			if (!first) OutputBufferPrint(context, ", ");

			char number[3];
			char digit0 = (*in >> 4) & 0xF;
			number[0] = digit0 > 0x9 ? 'A'-0xA+digit0 : '0'+digit0;
			char digit1 = *in & 0xF;
			number[1] = digit1 > 0x9 ? 'A'-0xA+digit1 : '0'+digit1;
			number[2] = 'h';
			OutputBufferPut(context, 3, number);
			++in;
			first = false;
		}
		else {
			*out++ = *in++;
			if (i == str.size - 1 || *in < 32 || *in == '\'') {
				*out++ = 0;
				t_threadMemPtr = out;

				if (!first) OutputBufferPrint(context, ", ");
				OutputBufferPrint(context, "'%s'", buffer);
				out = buffer;

				first = false;
			}
		}
	}
#if DEBUG_BUILD
	ASSERT(buffer <= t_threadMemPtr);
	memset(buffer, 0x00, (char *)t_threadMemPtr - buffer);
#endif
	t_threadMemPtr = buffer;
}

void BackendMain(Context *context)
{
	{
		auto finalProcs = context->beFinalProcedureData.GetForWrite();
		DynamicArrayInit(&finalProcs, 256);
	}

	x64InstructionInfos[X64_INT] =       { "int"_s,       OPERANDTYPE_IMMEDIATE, OPERANDACCESS_READ };
	x64InstructionInfos[X64_INT1] =      { "int 1"_s };
	x64InstructionInfos[X64_INT3] =      { "int 3"_s };
	x64InstructionInfos[X64_INTO] =      { "into"_s };
	x64InstructionInfos[X64_MOV] =       { "mov"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVZX] =     { "movzx"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSX] =     { "movsx"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSXD] =    { "movsxd"_s,    OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CQO] =       { "cqo"_s };
	x64InstructionInfos[X64_PUSH] =      { "push"_s,      OPERANDTYPE_ALL,       OPERANDACCESS_READ };
	x64InstructionInfos[X64_POP] =       { "pop"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_JMP] =       { "jmp"_s };
	x64InstructionInfos[X64_JE] =        { "je"_s };
	x64InstructionInfos[X64_JNE] =       { "jne"_s };
	x64InstructionInfos[X64_JG] =        { "jg"_s };
	x64InstructionInfos[X64_JL] =        { "jl"_s };
	x64InstructionInfos[X64_JGE] =       { "jge"_s };
	x64InstructionInfos[X64_JLE] =       { "jle"_s };
	x64InstructionInfos[X64_JA] =        { "ja"_s };
	x64InstructionInfos[X64_JB] =        { "jb"_s };
	x64InstructionInfos[X64_JAE] =       { "jae"_s };
	x64InstructionInfos[X64_JBE] =       { "jbe"_s };
	x64InstructionInfos[X64_CALL] =      { "call"_s };
	x64InstructionInfos[X64_CALL_Indirect] = { "call"_s,  OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_SYSCALL] =   { "syscall"_s };
	x64InstructionInfos[X64_LEAVE] =     { "leave"_s };
	x64InstructionInfos[X64_RET] =       { "ret"_s };
	x64InstructionInfos[X64_LEA] =       { "lea"_s,       OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_MEMORY, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CMP] =       { "cmp"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READ,      OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_TEST] =      { "test"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_READ,      OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_SETG] =      { "setg"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETL] =      { "setl"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETGE] =     { "setge"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETLE] =     { "setle"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETA] =      { "seta"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETB] =      { "setb"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETAE] =     { "setae"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETBE] =     { "setbe"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETE] =      { "sete"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETNE] =     { "setne"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_ADD] =       { "add"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUB] =       { "sub"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MUL] =       { "mul"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_IMUL] =      { "imul"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIV] =       { "div"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_IDIV] =      { "idiv"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_SAR] =       { "sar"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SAL] =       { "sal"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SHR] =       { "shr"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SHL] =       { "shl"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_AND] =       { "and"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_OR] =        { "or"_s,        OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XOR] =       { "xor"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_NOT] =       { "not"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE };
	x64InstructionInfos[X64_NEG] =       { "neg"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE };
	x64InstructionInfos[X64_MOVSS] =     { "movss"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSD] =     { "movsd"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_ADDSS] =     { "addss"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_ADDSD] =     { "addsd"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUBSS] =     { "subss"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUBSD] =     { "subsd"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MULSS] =     { "mulss"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MULSD] =     { "mulsd"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIVSS] =     { "divss"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIVSD] =     { "divsd"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XORPS] =     { "xorps"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XORPD] =     { "xorpd"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SQRTSS] =    { "sqrtss"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SQRTSD] =    { "sqrtsd"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_VXORPS] =    { "vxorps"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ, OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_VXORPD] =    { "vxorpd"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ, OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_COMISS] =    { "comiss"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_COMISD] =    { "comisd"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSI2SS] =  { "cvtsi2ss"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSI2SD] =  { "cvtsi2sd"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTTSS2SI] = { "cvttss2si"_s, OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTTSD2SI] = { "cvttsd2si"_s, OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSS2SD] =  { "cvtss2sd"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSD2SS] =  { "cvtsd2ss"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVUPS] =    { "movups"_s,    OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVAPS] =    { "movaps"_s,    OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };

	x64InstructionInfos[X64_INT].xedIClass =        XED_ICLASS_INT;
	x64InstructionInfos[X64_INT1].xedIClass =       XED_ICLASS_INT1;
	x64InstructionInfos[X64_INT3].xedIClass =       XED_ICLASS_INT3;
	x64InstructionInfos[X64_INTO].xedIClass =       XED_ICLASS_INTO;
	x64InstructionInfos[X64_MOV].xedIClass =        XED_ICLASS_MOV;
	x64InstructionInfos[X64_MOVZX].xedIClass =      XED_ICLASS_MOVZX;
	x64InstructionInfos[X64_MOVSX].xedIClass =      XED_ICLASS_MOVSX;
	x64InstructionInfos[X64_MOVSXD].xedIClass =     XED_ICLASS_MOVSXD;
	x64InstructionInfos[X64_CQO].xedIClass =        XED_ICLASS_CQO;
	x64InstructionInfos[X64_PUSH].xedIClass =       XED_ICLASS_PUSH;
	x64InstructionInfos[X64_POP].xedIClass =        XED_ICLASS_POP;
	x64InstructionInfos[X64_JMP].xedIClass =        XED_ICLASS_JMP;
	x64InstructionInfos[X64_JE].xedIClass =         XED_ICLASS_JZ;
	x64InstructionInfos[X64_JNE].xedIClass =        XED_ICLASS_JNZ;
	x64InstructionInfos[X64_JG].xedIClass =         XED_ICLASS_JNLE;
	x64InstructionInfos[X64_JL].xedIClass =         XED_ICLASS_JL;
	x64InstructionInfos[X64_JGE].xedIClass =        XED_ICLASS_JNL;
	x64InstructionInfos[X64_JLE].xedIClass =        XED_ICLASS_JLE;
	x64InstructionInfos[X64_JA].xedIClass =         XED_ICLASS_JNBE;
	x64InstructionInfos[X64_JB].xedIClass =         XED_ICLASS_JB;
	x64InstructionInfos[X64_JAE].xedIClass =        XED_ICLASS_JNB;
	x64InstructionInfos[X64_JBE].xedIClass =        XED_ICLASS_JBE;
	x64InstructionInfos[X64_CALL].xedIClass =       XED_ICLASS_CALL_NEAR;
	x64InstructionInfos[X64_CALL_Indirect].xedIClass = XED_ICLASS_CALL_NEAR;
	x64InstructionInfos[X64_SYSCALL].xedIClass =    XED_ICLASS_SYSCALL;
	x64InstructionInfos[X64_LEAVE].xedIClass =      XED_ICLASS_LEAVE;
	x64InstructionInfos[X64_RET].xedIClass =        XED_ICLASS_RET_NEAR;
	x64InstructionInfos[X64_LEA].xedIClass =        XED_ICLASS_LEA;
	x64InstructionInfos[X64_CMP].xedIClass =        XED_ICLASS_CMP;
	x64InstructionInfos[X64_TEST].xedIClass =       XED_ICLASS_TEST;
	x64InstructionInfos[X64_SETG].xedIClass =       XED_ICLASS_SETNLE;
	x64InstructionInfos[X64_SETL].xedIClass =       XED_ICLASS_SETL;
	x64InstructionInfos[X64_SETGE].xedIClass =      XED_ICLASS_SETNL;
	x64InstructionInfos[X64_SETLE].xedIClass =      XED_ICLASS_SETLE;
	x64InstructionInfos[X64_SETA].xedIClass =       XED_ICLASS_SETNBE;
	x64InstructionInfos[X64_SETB].xedIClass =       XED_ICLASS_SETB;
	x64InstructionInfos[X64_SETAE].xedIClass =      XED_ICLASS_SETNB;
	x64InstructionInfos[X64_SETBE].xedIClass =      XED_ICLASS_SETBE;
	x64InstructionInfos[X64_SETE].xedIClass =       XED_ICLASS_SETZ;
	x64InstructionInfos[X64_SETNE].xedIClass =      XED_ICLASS_SETNZ;
	x64InstructionInfos[X64_ADD].xedIClass =        XED_ICLASS_ADD;
	x64InstructionInfos[X64_SUB].xedIClass =        XED_ICLASS_SUB;
	x64InstructionInfos[X64_MUL].xedIClass =        XED_ICLASS_MUL;
	x64InstructionInfos[X64_IMUL].xedIClass =       XED_ICLASS_IMUL;
	x64InstructionInfos[X64_DIV].xedIClass =        XED_ICLASS_DIV;
	x64InstructionInfos[X64_IDIV].xedIClass =       XED_ICLASS_IDIV;
	x64InstructionInfos[X64_SAR].xedIClass =        XED_ICLASS_SAR;
	x64InstructionInfos[X64_SAL].xedIClass =        XED_ICLASS_SHL;
	x64InstructionInfos[X64_SHR].xedIClass =        XED_ICLASS_SHR;
	x64InstructionInfos[X64_SHL].xedIClass =        XED_ICLASS_SHL;
	x64InstructionInfos[X64_AND].xedIClass =        XED_ICLASS_AND;
	x64InstructionInfos[X64_OR].xedIClass =         XED_ICLASS_OR;
	x64InstructionInfos[X64_XOR].xedIClass =        XED_ICLASS_XOR;
	x64InstructionInfos[X64_NOT].xedIClass =        XED_ICLASS_NOT;
	x64InstructionInfos[X64_NEG].xedIClass =        XED_ICLASS_NEG;
	x64InstructionInfos[X64_MOVSS].xedIClass =      XED_ICLASS_MOVSS;
	x64InstructionInfos[X64_MOVSD].xedIClass =      XED_ICLASS_MOVSD_XMM;
	x64InstructionInfos[X64_ADDSS].xedIClass =      XED_ICLASS_ADDSS;
	x64InstructionInfos[X64_ADDSD].xedIClass =      XED_ICLASS_ADDSD;
	x64InstructionInfos[X64_SUBSS].xedIClass =      XED_ICLASS_SUBSS;
	x64InstructionInfos[X64_SUBSD].xedIClass =      XED_ICLASS_SUBSD;
	x64InstructionInfos[X64_MULSS].xedIClass =      XED_ICLASS_MULSS;
	x64InstructionInfos[X64_MULSD].xedIClass =      XED_ICLASS_MULSD;
	x64InstructionInfos[X64_DIVSS].xedIClass =      XED_ICLASS_DIVSS;
	x64InstructionInfos[X64_DIVSD].xedIClass =      XED_ICLASS_DIVSD;
	x64InstructionInfos[X64_XORPS].xedIClass =      XED_ICLASS_XORPS;
	x64InstructionInfos[X64_XORPD].xedIClass =      XED_ICLASS_XORPD;
	x64InstructionInfos[X64_SQRTSS].xedIClass =     XED_ICLASS_SQRTSS;
	x64InstructionInfos[X64_SQRTSD].xedIClass =     XED_ICLASS_SQRTSD;
	x64InstructionInfos[X64_VXORPS].xedIClass =     XED_ICLASS_VXORPS;
	x64InstructionInfos[X64_VXORPD].xedIClass =     XED_ICLASS_VXORPD;
	x64InstructionInfos[X64_COMISS].xedIClass =     XED_ICLASS_COMISS;
	x64InstructionInfos[X64_COMISD].xedIClass =     XED_ICLASS_COMISD;
	x64InstructionInfos[X64_CVTSI2SS].xedIClass =   XED_ICLASS_CVTSI2SS;
	x64InstructionInfos[X64_CVTSI2SD].xedIClass =   XED_ICLASS_CVTSI2SD;
	x64InstructionInfos[X64_CVTTSS2SI].xedIClass =  XED_ICLASS_CVTTSS2SI;
	x64InstructionInfos[X64_CVTTSD2SI].xedIClass =  XED_ICLASS_CVTTSD2SI;
	x64InstructionInfos[X64_CVTSS2SD].xedIClass =   XED_ICLASS_CVTSS2SD;
	x64InstructionInfos[X64_CVTSD2SS].xedIClass =   XED_ICLASS_CVTSD2SS;
	x64InstructionInfos[X64_MOVUPS].xedIClass =     XED_ICLASS_MOVUPS;
	x64InstructionInfos[X64_MOVAPS].xedIClass =     XED_ICLASS_MOVAPS;

	const u8 regValueFlags = VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED;
	u32 RAX_valueIdx = NewGlobalValue(context, "RAX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RCX_valueIdx = NewGlobalValue(context, "RCX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RDX_valueIdx = NewGlobalValue(context, "RDX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RBX_valueIdx = NewGlobalValue(context, "RBX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RSI_valueIdx = NewGlobalValue(context, "RSI"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RDI_valueIdx = NewGlobalValue(context, "RDI"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RSP_valueIdx = NewGlobalValue(context, "RSP"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RBP_valueIdx = NewGlobalValue(context, "RBP"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R8_valueIdx  = NewGlobalValue(context, "R8"_s,  TYPETABLEIDX_S64, regValueFlags);
	u32 R9_valueIdx  = NewGlobalValue(context, "R9"_s,  TYPETABLEIDX_S64, regValueFlags);
	u32 R10_valueIdx = NewGlobalValue(context, "R10"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R11_valueIdx = NewGlobalValue(context, "R11"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R12_valueIdx = NewGlobalValue(context, "R12"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R13_valueIdx = NewGlobalValue(context, "R13"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R14_valueIdx = NewGlobalValue(context, "R14"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R15_valueIdx = NewGlobalValue(context, "R15"_s, TYPETABLEIDX_S64, regValueFlags);

	u32 XMM0_valueIdx =  NewGlobalValue(context, "XMM0"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM1_valueIdx =  NewGlobalValue(context, "XMM1"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM2_valueIdx =  NewGlobalValue(context, "XMM2"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM3_valueIdx =  NewGlobalValue(context, "XMM3"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM4_valueIdx =  NewGlobalValue(context, "XMM4"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM5_valueIdx =  NewGlobalValue(context, "XMM5"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM6_valueIdx =  NewGlobalValue(context, "XMM6"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM7_valueIdx =  NewGlobalValue(context, "XMM7"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM8_valueIdx =  NewGlobalValue(context, "XMM8"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM9_valueIdx =  NewGlobalValue(context, "XMM9"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM10_valueIdx = NewGlobalValue(context, "XMM10"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM11_valueIdx = NewGlobalValue(context, "XMM11"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM12_valueIdx = NewGlobalValue(context, "XMM12"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM13_valueIdx = NewGlobalValue(context, "XMM13"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM14_valueIdx = NewGlobalValue(context, "XMM14"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM15_valueIdx = NewGlobalValue(context, "XMM15"_s, TYPETABLEIDX_F64, regValueFlags);

	for (int i = 0; i < X64REGISTER_Count; ++i)
	{
		Value v = GetGlobalValue(context, RAX_valueIdx + i);
		v.allocatedRegister = i;
		UpdateGlobalValue(context, RAX_valueIdx + i, &v);
	}

	RAX  = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S64);
	RCX  = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S64);
	RDX  = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S64);
	RBX  = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S64);
	RSI  = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S64);
	RDI  = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S64);
	RSP  = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S64);
	RBP  = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S64);
	R8   = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S64);
	R9   = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S64);
	R10  = IRValueValue(R10_valueIdx, TYPETABLEIDX_S64);
	R11  = IRValueValue(R11_valueIdx, TYPETABLEIDX_S64);
	R12  = IRValueValue(R12_valueIdx, TYPETABLEIDX_S64);
	R13  = IRValueValue(R13_valueIdx, TYPETABLEIDX_S64);
	R14  = IRValueValue(R14_valueIdx, TYPETABLEIDX_S64);
	R15  = IRValueValue(R15_valueIdx, TYPETABLEIDX_S64);

	EAX  = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S32);
	ECX  = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S32);
	EDX  = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S32);
	EBX  = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S32);
	ESI  = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S32);
	EDI  = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S32);
	ESP  = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S32);
	EBP  = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S32);
	R8D  = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S32);
	R9D  = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S32);
	R10D = IRValueValue(R10_valueIdx, TYPETABLEIDX_S32);
	R11D = IRValueValue(R11_valueIdx, TYPETABLEIDX_S32);
	R12D = IRValueValue(R12_valueIdx, TYPETABLEIDX_S32);
	R13D = IRValueValue(R13_valueIdx, TYPETABLEIDX_S32);
	R14D = IRValueValue(R14_valueIdx, TYPETABLEIDX_S32);
	R15D = IRValueValue(R15_valueIdx, TYPETABLEIDX_S32);

	AX   = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S16);
	CX   = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S16);
	DX   = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S16);
	BX   = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S16);
	SI   = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S16);
	DI   = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S16);
	SP   = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S16);
	BP   = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S16);
	R8W  = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S16);
	R9W  = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S16);
	R10W = IRValueValue(R10_valueIdx, TYPETABLEIDX_S16);
	R11W = IRValueValue(R11_valueIdx, TYPETABLEIDX_S16);
	R12W = IRValueValue(R12_valueIdx, TYPETABLEIDX_S16);
	R13W = IRValueValue(R13_valueIdx, TYPETABLEIDX_S16);
	R14W = IRValueValue(R14_valueIdx, TYPETABLEIDX_S16);
	R15W = IRValueValue(R15_valueIdx, TYPETABLEIDX_S16);

	AL   = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S8);
	CL   = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S8);
	DL   = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S8);
	BL   = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S8);
	SIL  = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S8);
	DIL  = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S8);
	SPL  = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S8);
	BPL  = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S8);
	R8B  = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S8);
	R9B  = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S8);
	R10B = IRValueValue(R10_valueIdx, TYPETABLEIDX_S8);
	R11B = IRValueValue(R11_valueIdx, TYPETABLEIDX_S8);
	R12B = IRValueValue(R12_valueIdx, TYPETABLEIDX_S8);
	R13B = IRValueValue(R13_valueIdx, TYPETABLEIDX_S8);
	R14B = IRValueValue(R14_valueIdx, TYPETABLEIDX_S8);
	R15B = IRValueValue(R15_valueIdx, TYPETABLEIDX_S8);

	XMM0 =  IRValueValue(XMM0_valueIdx,  TYPETABLEIDX_F64);
	XMM1 =  IRValueValue(XMM1_valueIdx,  TYPETABLEIDX_F64);
	XMM2 =  IRValueValue(XMM2_valueIdx,  TYPETABLEIDX_F64);
	XMM3 =  IRValueValue(XMM3_valueIdx,  TYPETABLEIDX_F64);
	XMM4 =  IRValueValue(XMM4_valueIdx,  TYPETABLEIDX_F64);
	XMM5 =  IRValueValue(XMM5_valueIdx,  TYPETABLEIDX_F64);
	XMM6 =  IRValueValue(XMM6_valueIdx,  TYPETABLEIDX_F64);
	XMM7 =  IRValueValue(XMM7_valueIdx,  TYPETABLEIDX_F64);
	XMM8 =  IRValueValue(XMM8_valueIdx,  TYPETABLEIDX_F64);
	XMM9 =  IRValueValue(XMM9_valueIdx,  TYPETABLEIDX_F64);
	XMM10 = IRValueValue(XMM10_valueIdx, TYPETABLEIDX_F64);
	XMM11 = IRValueValue(XMM11_valueIdx, TYPETABLEIDX_F64);
	XMM12 = IRValueValue(XMM12_valueIdx, TYPETABLEIDX_F64);
	XMM13 = IRValueValue(XMM13_valueIdx, TYPETABLEIDX_F64);
	XMM14 = IRValueValue(XMM14_valueIdx, TYPETABLEIDX_F64);
	XMM15 = IRValueValue(XMM15_valueIdx, TYPETABLEIDX_F64);

	x64Registers[0]  = RAX;
	x64Registers[1]  = RCX;
	x64Registers[2]  = RDX;
	x64Registers[3]  = RBX;
	x64Registers[4]  = RSI;
	x64Registers[5]  = RDI;
	x64Registers[6]  = RSP;
	x64Registers[7]  = RBP;
	x64Registers[8]  = R8;
	x64Registers[9]  = R9;
	x64Registers[10] = R10;
	x64Registers[11] = R11;
	x64Registers[12] = R12;
	x64Registers[13] = R13;
	x64Registers[14] = R14;
	x64Registers[15] = R15;
	x64Registers[16] = XMM0;
	x64Registers[17] = XMM1;
	x64Registers[18] = XMM2;
	x64Registers[19] = XMM3;
	x64Registers[20] = XMM4;
	x64Registers[21] = XMM5;
	x64Registers[22] = XMM6;
	x64Registers[23] = XMM7;
	x64Registers[24] = XMM8;
	x64Registers[25] = XMM9;
	x64Registers[26] = XMM10;
	x64Registers[27] = XMM11;
	x64Registers[28] = XMM12;
	x64Registers[29] = XMM13;
	x64Registers[30] = XMM14;
	x64Registers[31] = XMM15;
}

int ComparePointers(const void *lhs, const void *rhs)
{
	u64 lhsNum = (u64)(*(void **)lhs);
	u64 rhsNum = (u64)(*(void **)rhs);
	return (lhsNum > rhsNum) - (lhsNum < rhsNum);
}

void BackendGenerateOutputFile(Context *context)
{
	OutputBufferReset(context);

#if IS_WINDOWS
	OutputBufferPrint(context, "_DATA SEGMENT\n");
#else
	OutputBufferPrint(context, "section .data\n");
#endif

	OutputBufferPrint(context, "ALIGN 16\n");

	{
		auto scope = ProfilerScope("Writing all static variables");

		OutputBufferPrint(context, "__start_of_static_data:\n");

		qsort(context->staticDataPointersToRelocate.data,
				context->staticDataPointersToRelocate.size,
				sizeof(void *), ComparePointers);

		{
			u8 *scan = STATIC_DATA_VIRTUAL_ADDRESS;
			u8 *end  = scan + context->staticDataSize;
			u64 nextPointerIdx = 0;
			void *nextPointerToRelocate = (u8 *)context->staticDataPointersToRelocate[nextPointerIdx];
			while (scan < end) {
				u8 *current = scan;
				if (scan == nextPointerToRelocate) {
					u64 qword = *(u64 *)scan;

					// Assert the pointer points to static data memory (or is null)
					ASSERT(qword == 0 || (
							qword >= (u64)STATIC_DATA_VIRTUAL_ADDRESS &&
							qword < (u64)STATIC_DATA_VIRTUAL_ADDRESS_END));

					if (qword != 0) {
						u64 offset = qword - (u64)STATIC_DATA_VIRTUAL_ADDRESS;
						OutputBufferPrint(context, "DQ __start_of_static_data + 0%llXh", offset);
					}
					else
						OutputBufferPrint(context, "DQ 00h ;nullptr");
					scan += 8;

					// Find next pointer to relocate, skipping duplicates
					++nextPointerIdx;
					u64 pointersCount = context->staticDataPointersToRelocate.size;
					for (void *nextPtr = 0; nextPointerIdx < pointersCount; ++nextPointerIdx) {
						nextPtr = context->staticDataPointersToRelocate[nextPointerIdx];
						if (nextPtr != nextPointerToRelocate) {
							nextPointerToRelocate = nextPtr;
							break;
						}
					}
				}
				// If there's a whole quad word to read...
				else if (scan <= end-8) {
					u64 qword = *(u64 *)scan;

					// @Delete
#if DEBUG_BUILD
					ASSERT(qword < (u64)STATIC_DATA_VIRTUAL_ADDRESS ||
						   qword > (u64)STATIC_DATA_VIRTUAL_ADDRESS_END);
#endif

					OutputBufferPrint(context, "DQ 0%.16llXh", qword);
					scan += 8;
				}
				else {
					OutputBufferPrint(context, "DB 0%.2Xh", *scan++);
				}
				OutputBufferPrint(context, "\t\t; static_data + 0x%llX\n", (u64)(current -
								STATIC_DATA_VIRTUAL_ADDRESS));
			}
		}

#if IS_WINDOWS
		OutputBufferPrint(context, "_DATA ENDS\n");
		OutputBufferPrint(context, "_BSS SEGMENT\n");
#else
		OutputBufferPrint(context, "section .bss\n");
#endif
	}

#if IS_WINDOWS
	OutputBufferPrint(context, "_BSS ENDS\n");
#endif

#if IS_LINUX
	u64 procedureCount = context->procedures.GetForRead(.count);
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx) {
		Procedure proc = GetProcedureRead(context, procedureIdx);
		if (proc.isExported)
			OutputBufferPrint(context, "GLOBAL %S\n", proc.name);
	}
#endif

	{
		auto scope = ProfilerScope("Writing external variables");

		auto externalVars = context->irExternalVariables.GetForRead();
		for (int varIdx = 0; varIdx < externalVars->size; ++varIdx) {
			Value v = GetGlobalValue(context, externalVars[varIdx]);
			s64 size = GetTypeInfo(context, v.typeTableIdx).size;
			String type;
			switch (size) {
				case 1: type = "BYTE"_s; break;
				case 2: type = "WORD"_s; break;
				case 4: type = "DWORD"_s; break;
				case 8: type = "QWORD"_s; break;
				default: type = "QWORD"_s;
			}
			String name = StringExpand(v.externalSymbolName);
#if IS_WINDOWS
			OutputBufferPrint(context, "EXTRN %S:%S\n", name, type);
#else
			OutputBufferPrint(context, "EXTERN %S:%S\n", name, type);
#endif
		}
	}

	{
		auto scope = ProfilerScope("Writing external procedures");

		auto externalProcedures = context->externalProcedures.GetForRead();
		u64 externalProcedureCount = externalProcedures->count;
		for (u32 procedureIdx = 1; procedureIdx < externalProcedureCount; ++procedureIdx) {
			String procName = externalProcedures[procedureIdx].name;
#if IS_WINDOWS
			OutputBufferPrint(context, "EXTRN %S:proc\n", procName);
#else
			OutputBufferPrint(context, "EXTERN %S\n", procName);
#endif
		}
	}

#if IS_WINDOWS
	OutputBufferPrint(context, "_TEXT SEGMENT\n");
#else
	OutputBufferPrint(context, "section .text\n");
#endif

	// Code
	X64PrintInstructions(context);

#if IS_WINDOWS
	OutputBufferPrint(context, "_TEXT ENDS\n");
	OutputBufferPrint(context, "END\n");
#endif

	String outputFilename = "output/out"_s;

	String outputPath = SYSExpandPathWorkingDirectoryRelative("output"_s);
	SYSCreateDirectory(outputPath);

	bool makeLibrary = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = staticDefinitions->count;
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &staticDefinitions[i];
			if (StringEquals("compiler_output_name"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_STRING);
				outputFilename = currentDef->constant.valueAsString;
			}
			if (StringEquals("compiler_output_type"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_INTEGER);
				makeLibrary = currentDef->constant.valueAsInt == 1;
			}
		}
	}

	DynamicArray<String, ThreadAllocator> exportedSymbols;
	if (makeLibrary) {
		DynamicArrayInit(&exportedSymbols, 8);
		auto externalProcedures = context->procedures.GetForRead();
		for (int i = 0; i < externalProcedures->count; ++i) {
			Procedure proc = externalProcedures[i];
			if (proc.isExported)
				*DynamicArrayAdd(&exportedSymbols) = proc.name;
		}
	}

	OutputBufferWriteToFile(context, ChangeFilenameExtension(outputFilename, ".asm"_s));

	if (!context->config.silent)
		TimerSplit("X64 output file write"_s);

	String extraLinkerArguments = {};
	for (int i = 0; i < context->libsToLink.size; ++i)
		extraLinkerArguments = TPrintF("%S %S", extraLinkerArguments,
				context->libsToLink[i]);

#if IS_WINDOWS
	bool useWindowsSubsystem = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = staticDefinitions->count;
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &staticDefinitions[i];
			if (StringEquals("compiler_subsystem"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_INTEGER);
				useWindowsSubsystem = currentDef->constant.valueAsInt == 1;
			}
		}
	}

	String subsystemArgument;
	if (useWindowsSubsystem)
		subsystemArgument = "/subsystem:WINDOWS "_s;
	else
		subsystemArgument = "/subsystem:CONSOLE "_s;

	extraLinkerArguments = TPrintF("%S %S", extraLinkerArguments, subsystemArgument);
#endif

	if (!context->config.dontCallAssembler)
	{
		ProfilerBegin("Calling assembler");
		SYSRunAssembler(outputPath, outputFilename, ""_s, context->config.silent);
		if (!context->config.silent)
			TimerSplit("Calling assembler"_s);
		ProfilerEnd();

		ProfilerBegin("Calling linker");
		SYSRunLinker(outputPath, outputFilename, makeLibrary, exportedSymbols,
				extraLinkerArguments, context->config.silent);
		if (!context->config.silent)
			TimerSplit("Calling linker"_s);
		ProfilerEnd();
	}
}

void BackendGenerateWindowsObj(Context *context)
{
	ProfilerBegin("Generating output image");
	const int dataSectionIdx = 0;
	const int codeSectionIdx = 1;

	OutputBufferReset(context);

	u64 procCount = context->procedures.unsafe.count;
	ArrayInit(&g_procedureAddresses, procCount);
	g_procedureAddresses.size = procCount;

	DynamicArrayInit(&g_relocations, 1024);

	DynamicArray<IMAGE_SYMBOL, ThreadAllocator> symbolTable;
	DynamicArrayInit(&symbolTable, 1024);
	/*
		The symbol table shall have a fixed order, so we can know the symbol table
		index of stuff before actually making the table.

		*	First a symbol for each section
		*	All external procedures, in order
		*	All public procedures, in order
	*/

	DynamicArray<String, ThreadAllocator> stringTable;
	DynamicArrayInit(&stringTable, 1024);
	u32 stringTableOffset = 4; // Start after the string table size

	IMAGE_FILE_HEADER header;
	header.Machine = IMAGE_FILE_MACHINE_AMD64;
	header.NumberOfSections = 2;
	header.PointerToSymbolTable = 0;
	header.NumberOfSymbols = 0;
	header.SizeOfOptionalHeader = 0;
	header.Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE;

	IMAGE_SECTION_HEADER dataSectionHeader;
	memcpy(&dataSectionHeader.Name, "data\0\0\0\0", 8);
	dataSectionHeader.Misc.VirtualSize = 0;
	dataSectionHeader.VirtualAddress = 0;
	dataSectionHeader.SizeOfRawData = 0;
	dataSectionHeader.PointerToRawData = 0;
	dataSectionHeader.PointerToRelocations = 0;
	dataSectionHeader.PointerToLinenumbers = 0;
	dataSectionHeader.NumberOfRelocations = 0;
	dataSectionHeader.NumberOfLinenumbers = 0;
	dataSectionHeader.Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA;

	IMAGE_SECTION_HEADER codeSectionHeader;
	memcpy(&codeSectionHeader.Name, "code\0\0\0\0", 8);
	codeSectionHeader.Misc.VirtualSize = 0;
	codeSectionHeader.VirtualAddress = 0;
	codeSectionHeader.SizeOfRawData = 0;
	codeSectionHeader.PointerToRawData = 0;
	codeSectionHeader.PointerToRelocations = 0;
	codeSectionHeader.PointerToLinenumbers = 0;
	codeSectionHeader.NumberOfRelocations = 0;
	codeSectionHeader.NumberOfLinenumbers = 0;
	codeSectionHeader.Characteristics = IMAGE_SCN_CNT_CODE;

	// We write the headers later, skip for now
	OutputBufferSeek(context, sizeof(header) + sizeof(dataSectionHeader) +
			sizeof(codeSectionHeader));

	// data section
	OutputBufferAlign(context, 16);
	u64 dataSectionOffset = context->outputBufferOffset;
	OutputBufferPut(context, context->staticDataSize, STATIC_DATA_VIRTUAL_ADDRESS);

	u32 startOfStaticDataSymbolIdx = (u32)symbolTable.size;
	{
		// Symbols for sections
		IMAGE_SYMBOL symbol;
		memcpy(&symbol.N.ShortName, "sectdata", 8);
		symbol.Value = 0;
		symbol.SectionNumber = dataSectionIdx + 1; // 1-based
		symbol.Type = 0;
		symbol.StorageClass = IMAGE_SYM_CLASS_STATIC;
		symbol.NumberOfAuxSymbols = 0;
		*DynamicArrayAdd(&symbolTable) = symbol;

		memcpy(&symbol.N.ShortName, "sectcode", 8);
		symbol.SectionNumber = codeSectionIdx + 1; // 1-based
		*DynamicArrayAdd(&symbolTable) = symbol;
	}

	// Remap pointers
	qsort(context->staticDataPointersToRelocate.data,
			context->staticDataPointersToRelocate.size,
			sizeof(void *), ComparePointers);

	u64 staticDataPtrCount = context->staticDataPointersToRelocate.size;
	u64 uniqueStaticDataPtrCount = 0;
	void *lastPtr = nullptr;
	for (int ptrIdx = 0; ptrIdx < staticDataPtrCount; ++ptrIdx) {
		void *ptr = context->staticDataPointersToRelocate.data[ptrIdx];
		// Skip duplicates (this is important cause the linker WILL reallocate these twice
		// additively)
		if (ptr == lastPtr)
			continue;
		lastPtr = ptr;

		++uniqueStaticDataPtrCount;

		u64 fileOffset = (u64)ptr - (u64)STATIC_DATA_VIRTUAL_ADDRESS + dataSectionOffset;

		u64 dataPtr = *(u64 *)ptr;
		if (dataPtr != 0) {
			ASSERT(dataPtr >= (u64)STATIC_DATA_VIRTUAL_ADDRESS &&
				   dataPtr < (u64)STATIC_DATA_VIRTUAL_ADDRESS_END);
			u64 remappedPtr = dataPtr - (u64)STATIC_DATA_VIRTUAL_ADDRESS;

			OutputBufferSeek(context, fileOffset);
			OutputBufferPut(context, 8, &remappedPtr);
		}
	}

	// code section
	OutputBufferSeek(context, context->outputBufferSize);
	OutputBufferAlign(context, 16);
	u64 codeSectionOffset = context->outputBufferOffset;
	xed_tables_init();
	X64EncodeInstructions(context);
	u64 codeSectionSize = context->outputBufferOffset - codeSectionOffset;

	// Add procedures to symbol table
	u32 externalProcCount = (u32)context->externalProcedures.unsafe.count;
	for (u32 procIdx = 1; procIdx < externalProcCount; ++procIdx) {
		Procedure proc = context->externalProcedures.unsafe[procIdx];
		// Add name to string table
		u32 nameStringTableOffset = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = proc.name;
		stringTableOffset += (u32)proc.name.size + 1; // + null terminator

		IMAGE_SYMBOL symbol;
		symbol.N.Name.Short = 0;
		symbol.N.Name.Long = nameStringTableOffset;
		symbol.Value = 0;
		symbol.SectionNumber = 0;
		symbol.Type = 0;
		symbol.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
		symbol.NumberOfAuxSymbols = 0;
		*DynamicArrayAdd(&symbolTable) = symbol;
	}
	for (u32 procIdx = 1; procIdx < procCount; ++procIdx) {
		Procedure proc = context->procedures.unsafe[procIdx];
		// Add name to string table
		u32 nameStringTableOffset = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = proc.name;
		stringTableOffset += (u32)proc.name.size + 1; // + null terminator

		u32 offsetWithinSection = (u32)g_procedureAddresses[procIdx] - (u32)codeSectionOffset;

		IMAGE_SYMBOL symbol;
		symbol.N.Name.Short = 0;
		symbol.N.Name.Long = nameStringTableOffset;
		symbol.Value = offsetWithinSection;
		symbol.SectionNumber = codeSectionIdx + 1; // 1-based
		symbol.Type = 0;
		symbol.StorageClass = proc.isExported ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC;
		symbol.NumberOfAuxSymbols = 0;
		*DynamicArrayAdd(&symbolTable) = symbol;
	}

	// Remap pointers
	for (int relIdx = 0; relIdx < g_relocations.size; ++relIdx) {
		Relocation relocation = g_relocations[relIdx];
		switch (relocation.type) {
		case RELOCATIONTYPE_PROCEDURE:
		{
			u64 procOffset = g_procedureAddresses[relocation.procedureIdx];
			s32 offsetInBuffer;
			OutputBufferSeek(context, relocation.destOffset);
			OutputBufferRead(context, 4, &offsetInBuffer);
			// We need to fit these into u32's
			ASSERT(procOffset <= U32_MAX);
			ASSERT(relocation.destOffset <= U32_MAX);

			s64 finalOffset = (s64)procOffset - (4 + (s64)relocation.destOffset) + offsetInBuffer -
				relocation.offsetShift;
			ASSERT(finalOffset >= S32_MIN && finalOffset <= S32_MAX);
			OutputBufferSeek(context, relocation.destOffset);
			OutputBufferPut(context, 4, &finalOffset);
		} break;
		case RELOCATIONTYPE_LABEL:
		{
#if DEBUG_BUILD
			s32 offsetInBuffer;
			OutputBufferSeek(context, relocation.destOffset);
			OutputBufferRead(context, 4, &offsetInBuffer);
			ASSERT(offsetInBuffer == 0xCCCCCCCC);
#endif
			s64 finalOffset = (s64)relocation.label->address - (4 + (s64)relocation.destOffset) -
				relocation.offsetShift;
			ASSERT(finalOffset >= S32_MIN && finalOffset <= S32_MAX);
			OutputBufferSeek(context, relocation.destOffset);
			OutputBufferPut(context, 4, &finalOffset);
		} break;
		}
	}

	// Static data relocation table
	OutputBufferSeek(context, context->outputBufferSize);
	OutputBufferAlign(context, 16);
	u64 dataRelocationTableOffset = context->outputBufferOffset;
	IMAGE_RELOCATION imageRelocation;
	imageRelocation.Type = IMAGE_REL_AMD64_ADDR64;
	imageRelocation.SymbolTableIndex = startOfStaticDataSymbolIdx;
	lastPtr = nullptr;
	for (int ptrIdx = 0; ptrIdx < staticDataPtrCount; ++ptrIdx) {
		void *ptr = context->staticDataPointersToRelocate.data[ptrIdx];
		if (ptr == lastPtr)
			continue;
		lastPtr = ptr;

		u64 sectionOffset = (u64)ptr - (u64)STATIC_DATA_VIRTUAL_ADDRESS;
		ASSERT(sectionOffset <= U32_MAX);
		imageRelocation.VirtualAddress = (u32)sectionOffset;
		OutputBufferPut(context, sizeof(imageRelocation), &imageRelocation);
	}

	// Code relocation table
	OutputBufferSeek(context, context->outputBufferSize);
	OutputBufferAlign(context, 16);
	u64 codeRelocationTableOffset = context->outputBufferOffset;
	u64 codeRelocationCount = 0;
	for (int relIdx = 0; relIdx < g_relocations.size; ++relIdx) {
		Relocation relocation = g_relocations[relIdx];

		static_assert(IMAGE_REL_AMD64_REL32 + 5 == IMAGE_REL_AMD64_REL32_5);
		ASSERT(relocation.offsetShift >= 0 && relocation.offsetShift <= 5);
		imageRelocation.Type = IMAGE_REL_AMD64_REL32 + (u16)relocation.offsetShift;

		switch (relocation.type) {
		case RELOCATIONTYPE_EXTERNAL_PROCEDURE:
		{
			u64 sectionOffset = relocation.destOffset - codeSectionOffset;
			ASSERT(sectionOffset <= U32_MAX);
			imageRelocation.VirtualAddress = (u32)sectionOffset;
			// +2 for section symbols, -1 because procedure index is 1-based.
			imageRelocation.SymbolTableIndex = 1 + relocation.procedureIdx;
			OutputBufferPut(context, sizeof(imageRelocation), &imageRelocation);
			++codeRelocationCount;
		} break;
		case RELOCATIONTYPE_STATIC_DATA:
		{
			u64 sectionOffset = relocation.destOffset - codeSectionOffset;
			ASSERT(sectionOffset <= U32_MAX);
			imageRelocation.VirtualAddress = (u32)sectionOffset;
			imageRelocation.SymbolTableIndex = dataSectionIdx; // Refer to symbol table order comment
			OutputBufferPut(context, sizeof(imageRelocation), &imageRelocation);
			++codeRelocationCount;
		} break;
		}
	}

	// Symbol table
	OutputBufferAlign(context, 16);
	u64 symbolTableOffset = context->outputBufferOffset;
	for (int symbolIdx = 0; symbolIdx < symbolTable.size; ++symbolIdx)
		OutputBufferPut(context, sizeof(symbolTable.data[0]), &symbolTable[symbolIdx]);

	// String table
	u64 stringTableStart = context->outputBufferOffset;
	u32 stringTableTotalSize = stringTableOffset;
	OutputBufferPut(context, 4, &stringTableTotalSize);
	u8 zero = 0;
	for (int stringIdx = 0; stringIdx < stringTable.size; ++stringIdx) {
		String str = stringTable[stringIdx];
		OutputBufferPut(context, str.size, str.data);
		OutputBufferPut(context, 1, &zero);
	}
	// Assert we had the total size right
	ASSERT(context->outputBufferOffset == stringTableStart + stringTableTotalSize);

	// Fix headers
	ASSERT(symbolTableOffset <= U32_MAX);
	header.PointerToSymbolTable = (u32)symbolTableOffset;
	ASSERT(symbolTable.size <= U32_MAX);
	header.NumberOfSymbols = (u32)symbolTable.size;

	ASSERT(context->staticDataSize <= U32_MAX);
	dataSectionHeader.SizeOfRawData = (u32)context->staticDataSize;
	ASSERT(dataSectionOffset <= U32_MAX);
	dataSectionHeader.PointerToRawData = (u32)dataSectionOffset;
	ASSERT(dataRelocationTableOffset <= U32_MAX);
	dataSectionHeader.PointerToRelocations = (u32)dataRelocationTableOffset;
	ASSERT(uniqueStaticDataPtrCount <= U16_MAX);
	dataSectionHeader.NumberOfRelocations = (u16)uniqueStaticDataPtrCount;

	ASSERT(context->staticDataSize <= U32_MAX);
	codeSectionHeader.SizeOfRawData = (u32)codeSectionSize;
	ASSERT(codeSectionOffset <= U32_MAX);
	codeSectionHeader.PointerToRawData = (u32)codeSectionOffset;
	ASSERT(codeRelocationTableOffset <= U32_MAX);
	codeSectionHeader.PointerToRelocations = (u32)codeRelocationTableOffset;
	ASSERT(codeRelocationCount <= U16_MAX);
	codeSectionHeader.NumberOfRelocations = (u16)codeRelocationCount;

	// Write headers
	OutputBufferSeek(context, 0);
	OutputBufferPut(context, sizeof(header), &header);
	OutputBufferPut(context, sizeof(dataSectionHeader), &dataSectionHeader);
	OutputBufferPut(context, sizeof(codeSectionHeader), &codeSectionHeader);

	String outputFilename = "output/out"_s;

	String outputPath = SYSExpandPathWorkingDirectoryRelative("output"_s);
	SYSCreateDirectory(outputPath);

	bool makeLibrary = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = staticDefinitions->count;
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &staticDefinitions[i];
			if (StringEquals("compiler_output_name"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_STRING);
				outputFilename = currentDef->constant.valueAsString;
			}
			if (StringEquals("compiler_output_type"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_INTEGER);
				makeLibrary = currentDef->constant.valueAsInt == 1;
			}
		}
	}

	DynamicArray<String, ThreadAllocator> exportedSymbols = {};
	if (makeLibrary) {
		DynamicArrayInit(&exportedSymbols, 8);
		auto externalProcedures = context->procedures.GetForRead();
		for (int i = 0; i < externalProcedures->count; ++i) {
			Procedure proc = externalProcedures[i];
			if (proc.isExported)
				*DynamicArrayAdd(&exportedSymbols) = proc.name;
		}
	}

	OutputBufferWriteToFile(context, ChangeFilenameExtension(outputFilename, ".obj"_s));

	if (!context->config.silent)
		TimerSplit("Generating output image"_s);

	// Call linker
	String extraLinkerArguments = {};
	for (int i = 0; i < context->libsToLink.size; ++i)
		extraLinkerArguments = TPrintF("%S %S", extraLinkerArguments,
				context->libsToLink[i]);

#if IS_WINDOWS
	bool useWindowsSubsystem = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = staticDefinitions->count;
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &staticDefinitions[i];
			if (StringEquals("compiler_subsystem"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_INTEGER);
				useWindowsSubsystem = currentDef->constant.valueAsInt == 1;
			}
		}
	}

	String subsystemArgument;
	if (useWindowsSubsystem)
		subsystemArgument = "/subsystem:WINDOWS "_s;
	else
		subsystemArgument = "/subsystem:CONSOLE "_s;

	extraLinkerArguments = TPrintF("%S %S", extraLinkerArguments, subsystemArgument);
#endif

	ProfilerBegin("Calling linker");
	SYSRunLinker(outputPath, outputFilename, makeLibrary, exportedSymbols, extraLinkerArguments,
			context->config.silent);
	if (!context->config.silent)
		TimerSplit("Calling linker"_s);
	ProfilerEnd();
}

void BackendJobProc(Context *context, u32 procedureIdx)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	static const String paramNames[] = {
		"_param0"_s,
		"_param1"_s,
		"_param2"_s,
		"_param3"_s,
		"_param4"_s,
		"_param5"_s,
		"_param6"_s,
		"_param7"_s,
		"_param8"_s,
		"_param9"_s,
		"_param10"_s,
		"_param11"_s,
		"_param12"_s,
		"_param13"_s,
		"_param14"_s,
		"_param15"_s,
		"_param16"_s,
		"_param17"_s,
		"_param18"_s,
		"_param19"_s,
		"_param20"_s,
		"_param21"_s,
		"_param22"_s,
		"_param23"_s,
		"_param24"_s,
		"_param25"_s,
		"_param26"_s,
		"_param27"_s,
		"_param28"_s,
		"_param29"_s,
		"_param30"_s,
		"_param31"_s,
	};

	// We need these builtin procedures to be declared
	{
		TCScopeName name = FindGlobalName(context, {}, "CopyMemory"_s);
		ASSERT(name.type == NAMETYPE_STATIC_DEFINITION);
		StaticDefinition staticDef = GetStaticDefinition(context, name.staticDefinitionIdx);
		ASSERT(staticDef.definitionType == STATICDEFINITIONTYPE_PROCEDURE);
		g_copyMemoryProcIdx = staticDef.procedureIdx;

		name = FindGlobalName(context, {}, "ZeroMemory"_s);
		ASSERT(name.type == NAMETYPE_STATIC_DEFINITION);
		staticDef = GetStaticDefinition(context, name.staticDefinitionIdx);
		ASSERT(staticDef.definitionType == STATICDEFINITIONTYPE_PROCEDURE);
		g_zeroMemoryProcIdx = staticDef.procedureIdx;
	}

	// Initialize generic parameter values
	for (int paramIdx = 0; paramIdx < 32; ++paramIdx)
	{
		Value newValue = {};
#if DEBUG_BUILD
		newValue.name = paramNames[paramIdx];
#endif
		newValue.typeTableIdx = TYPETABLEIDX_S64;
		newValue.flags = VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY | VALUEFLAGS_BASE_RELATIVE;
		newValue.stackOffset = 16 + paramIdx * 8; // Add 16, 8 for return address, and 8 because we push RBP
		u32 newValueIdx = IRNewValue(context, newValue);
		jobData->x64SpilledParametersRead[paramIdx] = newValueIdx;
	}

	for (int paramIdx = 0; paramIdx < 32; ++paramIdx)
	{
		Value newValue = {};
#if DEBUG_BUILD
		newValue.name = paramNames[paramIdx];
#endif
		newValue.typeTableIdx = TYPETABLEIDX_S64;
		newValue.flags = VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY;
		newValue.stackOffset = paramIdx * 8; // Add 16, 8 for return address, and 8 because we push RBP
		u32 newValueIdx = IRNewValue(context, newValue);
		jobData->x64SpilledParametersWrite[paramIdx] = newValueIdx;
	}

	Procedure proc = GetProcedureRead(context, procedureIdx);
	ASSERT(proc.astBody);
	ASSERT(GetTypeInfo(context, proc.typeTableIdx).typeCategory == TYPECATEGORY_PROCEDURE);
	TypeInfoProcedure procTypeInfo = GetTypeInfo(context, proc.typeTableIdx).procedureInfo;

	BucketArrayInit(&jobData->beInstructions);
	jobData->allocatedParameterCount = 0;
	jobData->returnValueIndices = proc.returnValueIndices;
	jobData->stackSize = 0;
	DynamicArrayInit(&jobData->spilledValues, 8);
	BucketArrayInit(&jobData->bePatchedInstructions);

	// Allocate parameters
	int paramCount = (int)proc.parameterValues.size;
	Array<IRValue, ThreadAllocator> params;
	ArrayInit(&params, paramCount + 1);

	if (procTypeInfo.callingConvention != CC_DEFAULT)
	{
		bool returnByCopy = procTypeInfo.returnTypeIndices.size &&
				IRShouldPassByCopy(context, procTypeInfo.returnTypeIndices[0]);

		// Pointer to return value
		if (returnByCopy)
			*ArrayAdd(&params) = IRValueValue(proc.returnValueIndices[0], TYPETABLEIDX_S64);
	}

	for (int paramIdx = 0; paramIdx < paramCount; ++paramIdx)
		*ArrayAdd(&params) = IRValueValue(context, proc.parameterValues[paramIdx]);

	switch (procTypeInfo.callingConvention)
	{
		case CC_WIN64:
			X64ReadyWin64Parameters(context, {}, params, false);
			break;
		case CC_DEFAULT:
		case CC_LINUX64:
			X64ReadyLinuxParameters(context, {}, params, false);
	}

#if DEBUG_BUILD
	u32 lastFileIdx = U32_MAX;
	u32 lastLine = U32_MAX;
#endif

	u64 instructionCount = proc.irInstructions.count;
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
	{
		IRInstruction inst = proc.irInstructions[instructionIdx];

#if DEBUG_BUILD
		if (inst.loc.fileIdx != 0) {
			FatSourceLocation fatLoc = ExpandSourceLocation(context, inst.loc);
			if (inst.loc.fileIdx != lastFileIdx || fatLoc.line != lastLine)
				*BucketArrayAdd(&jobData->beInstructions) = {
					.loc = inst.loc,
					.type = X64_Comment,
					.comment = { fatLoc.lineSize, fatLoc.beginingOfLine }
				};

			lastFileIdx = inst.loc.fileIdx;
			lastLine = fatLoc.line;
		}
#endif

		X64ConvertInstruction(context, inst);
	}

	u64 returnValueCount = procTypeInfo.returnTypeIndices.size;
	if (returnValueCount)
	{
		if (procTypeInfo.callingConvention == CC_DEFAULT)
		{
			static IRValue integerReturnRegisters[]  = { RAX, RDI, RSI, RDX, RCX, R8, R9 };
			static IRValue floatingReturnRegisters[] = {
				XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8 };

			int integerIdx = 0;
			int floatingIdx = 0;
			for (int i = 0; i < returnValueCount; ++i) {
				IRValue slot;
				u32 returnTypeIdx = procTypeInfo.returnTypeIndices[i];
				IRValue out = IRValueValue(proc.returnValueIndices[i], returnTypeIdx);
				TypeInfo returnTypeInfo = GetTypeInfo(context, returnTypeIdx);
				if (returnTypeInfo.typeCategory == TYPECATEGORY_FLOATING) {
					slot = floatingReturnRegisters[floatingIdx++];
					slot.typeTableIdx = out.typeTableIdx;
					X64Mov(context, {}, slot, out);
				}
				else {
					slot = integerReturnRegisters[integerIdx++];

					if (IRShouldPassByCopy(context, returnTypeIdx)) {
						IRValue outButS64 = out;
						outButS64.typeTableIdx = TYPETABLEIDX_S64;
						X64AddInstruction2(context, {}, X64_LEA, slot, outButS64);
					}
					else {
						slot.typeTableIdx = out.typeTableIdx;
						X64Mov(context, {}, slot, out);
					}
				}
			}
		}
		else
		{
			u32 returnTypeIdx = procTypeInfo.returnTypeIndices[0];
			IRValue returnValue = IRValueValue(proc.returnValueIndices[0], returnTypeIdx);
			bool returnByCopy = IRShouldPassByCopy(context, returnTypeIdx);
			if (returnByCopy && procTypeInfo.callingConvention != CC_DEFAULT)
				X64AddInstruction2(context, {}, X64_LEA, RAX, returnValue);
			else if (!returnByCopy) {
				if (GetTypeInfo(context, returnTypeIdx).typeCategory == TYPECATEGORY_FLOATING) {
					IRValue typedXmm0 = XMM0;
					typedXmm0.typeTableIdx = returnTypeIdx;
					X64Mov(context, {}, typedXmm0, returnValue);
				}
				else {
					IRValue typedRax = RAX;
					typedRax.typeTableIdx = returnTypeIdx;
					X64Mov(context, {}, typedRax, returnValue);
				}
			}
		}
	}

	X64AllocateRegisters(context);

	// Remove instructions that reference unused values
	X64InstructionStream stream = X64InstructionStreamBegin(&jobData->beInstructions);
	X64Instruction *inst = X64InstructionStreamAdvance(&stream);
	X64Instruction *nextInst  = X64InstructionStreamAdvance(&stream);
	X64Instruction *nextInst2 = X64InstructionStreamAdvance(&stream);
	while (inst) {
		// Replace LEAs with a register as a source with a MOV.
		if (inst->type == X64_LEA) {
			if ((inst->src.valueType == IRVALUETYPE_VALUE ||
				inst->src.valueType == IRVALUETYPE_MEMORY) &&
				(inst->dst.valueType == IRVALUETYPE_VALUE ||
				inst->dst.valueType == IRVALUETYPE_MEMORY))
			{
				if (inst->src.valueType != IRVALUETYPE_MEMORY ||
						(inst->src.mem.offset == 0 && inst->src.mem.elementSize == 0)) {
					Value v = IRGetValue(context, inst->src.valueIdx);
					if ((v.flags & VALUEFLAGS_IS_ALLOCATED) && !(v.flags & VALUEFLAGS_IS_MEMORY)) {
						inst->type = X64_MOV;
						inst->src.valueType = IRVALUETYPE_VALUE;
					}
				}
			}
		}

		switch (inst->type) {
		// dst write, src read
		case X64_MOVUPS:
		{
			// If aligned change to MOVAPS
			ASSERT((inst->dst.valueType == IRVALUETYPE_VALUE ||
				 inst->dst.valueType == IRVALUETYPE_MEMORY) &&
				(inst->src.valueType == IRVALUETYPE_VALUE ||
				 inst->src.valueType == IRVALUETYPE_MEMORY));

			Value dst = IRGetValue(context, inst->dst.valueIdx);
			Value src = IRGetValue(context, inst->src.valueIdx);
			if (dst.flags & VALUEFLAGS_IS_ALLOCATED && src.flags & VALUEFLAGS_IS_ALLOCATED) {
				if (!(dst.flags & VALUEFLAGS_IS_MEMORY) ||
					(dst.stackOffset & 15))
					goto unalignedMovups;

				if (!(src.flags & VALUEFLAGS_IS_MEMORY) ||
					(src.stackOffset & 15))
					goto unalignedMovups;

				inst->type = X64_MOVAPS;
			}
unalignedMovups:;
		} // fall through
		case X64_MOV:
		case X64_MOVSS:
		case X64_MOVSD:
		{
			// Ignore mov thing into itself
			if (inst->dst.valueType == IRVALUETYPE_VALUE &&
				inst->src.valueType == IRVALUETYPE_VALUE)
			{
				Value dst = IRGetValue(context, inst->dst.valueIdx);
				Value src = IRGetValue(context, inst->src.valueIdx);
				if (dst.flags & VALUEFLAGS_IS_ALLOCATED && src.flags & VALUEFLAGS_IS_ALLOCATED) {
					// Value::stackOffset is alias of Value::allocatedRegister
					if (dst.allocatedRegister == src.allocatedRegister) {
						inst->type = X64_Ignore;
						break;
					}
				}
			}
		} // fall through
		case X64_MOVZX:
		case X64_MOVSX:
		case X64_MOVSXD:
		case X64_LEA:
		case X64_SETE:
		case X64_SETNE:
		case X64_SETL:
		case X64_SETLE:
		case X64_SETG:
		case X64_SETGE:
		case X64_SETA:
		case X64_SETAE:
		case X64_SETB:
		case X64_SETBE:
		case X64_CVTSI2SS:
		case X64_CVTSI2SD:
		case X64_CVTTSS2SI:
		case X64_CVTTSD2SI:
		case X64_CVTSS2SD:
		case X64_CVTSD2SS:
		{
			if (inst->dst.valueType == IRVALUETYPE_VALUE ||
				inst->dst.valueType == IRVALUETYPE_MEMORY)
			{
				Value v = IRGetValue(context, inst->dst.valueIdx);
				if (!(v.flags & (VALUEFLAGS_IS_USED | VALUEFLAGS_ON_STATIC_STORAGE)))
					inst->type = X64_Ignore;
			}
		} break;
		}

		// Zero idioms
		if (inst->type == X64_MOVSS || inst->type == X64_MOVSD) {
			if (inst->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
				inst->src.immediate == 0)
			{
				if (IsValueInMemory(context, inst->dst))
					inst->type = X64_MOV;
				else
					*inst = { inst->loc, inst->type == X64_MOVSS ? X64_XORPS : X64_XORPD,
						inst->dst, inst->dst };
			}
		}
		else if (inst->type == X64_MOV) {
			if (inst->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
				inst->src.immediate == 0)
			{
				if (!IsValueInMemory(context, inst->dst))
					*inst = { inst->loc, X64_XOR, inst->dst, inst->dst };
			}
		}

		// Unnecessary jumps
		if (nextInst && inst->type >= X64_Jump_Begin && inst->type <= X64_Jump_End &&
			nextInst->type == X64_Label)
		{
			if (inst->label == nextInst->label)
				inst->type = X64_Ignore;
		}

		// Replace CMP 0 with TEST
		if (nextInst2 && nextInst2->type == X64_CMP) {
			if (nextInst2->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
				nextInst2->src.immediate == 0 && !IsValueInMemory(context, nextInst2->dst))
			{
				nextInst2->type = X64_TEST;
				nextInst2->src = nextInst2->dst;
			}
			else if (nextInst2->dst.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
				nextInst2->dst.immediate == 0 && !IsValueInMemory(context, nextInst2->src))
			{
				nextInst2->type = X64_TEST;
				nextInst2->dst = nextInst2->src;
			}
		}

		// Avoid saving to bool then testing
		// @Todo: do this sort of thing with IR, should catch a lot more cases.
		if (nextInst2 && inst->type == X64_SETE && nextInst->type == X64_TEST &&
				nextInst2->type == X64_JE) {
			if (inst->dst.valueIdx == nextInst->dst.valueIdx &&
				inst->dst.valueIdx == nextInst->src.valueIdx)
			{
				inst->type = X64_Ignore;
				nextInst->type = X64_Ignore;
				nextInst2->type = X64_JE;
			}
		}

		inst = nextInst;
		nextInst = nextInst2;
		nextInst2 = X64InstructionStreamAdvance(&stream);
		while (nextInst2 && nextInst2->type >= X64_Count)
			nextInst2 = X64InstructionStreamAdvance(&stream);
	}

	X64FinalProcedure finalProc;
	finalProc.procedureIdx = procedureIdx;
	finalProc.localValues = *jobData->localValues;
	finalProc.instructions = jobData->beInstructions;
	finalProc.stackSize = jobData->stackSize;
	auto finalProcs = context->beFinalProcedureData.GetForWrite();
	*DynamicArrayAdd(&finalProcs) = finalProc;
}
