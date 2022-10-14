X64InstructionStream X64InstructionStreamBegin(
		BucketArray<X64Instruction, HeapAllocator, 1024> *instructionArray)
{
	X64InstructionStream stream;
	stream.idx = -1;
	stream.instructionArray = instructionArray;
	stream.instructionArrayCount = BucketArrayCount(instructionArray);
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

s64 PrintOut(Context *context, const char *format, ...) {
	ThreadDataCommon *jobData = (ThreadDataCommon *)SYSGetThreadData(g_memory->tlsIndex);
	char *buffer = (char *)jobData->threadMemPtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);

	s64 bytesToWrite = size;
	const char *in = buffer;
	while (bytesToWrite > 0) {
		auto *lastBucket = DynamicArrayBack(&context->outputBuffer.buckets);
		s64 bytesLeftInBucket = OUTPUT_BUFFER_BUCKET_SIZE - lastBucket->size;
		u8 *bufferCursor = lastBucket->data + lastBucket->size;
		if (bytesToWrite > bytesLeftInBucket) {
			memcpy(bufferCursor, in, bytesLeftInBucket);
			in += bytesLeftInBucket;
			lastBucket->size += bytesLeftInBucket;
			bytesToWrite -= bytesLeftInBucket;

			lastBucket = DynamicArrayAdd(&context->outputBuffer.buckets);
			ArrayInit(lastBucket, OUTPUT_BUFFER_BUCKET_SIZE);
		}
		else {
			memcpy(bufferCursor, in, size);
			in += bytesLeftInBucket;
			lastBucket->size += bytesToWrite;
			bytesToWrite -= bytesToWrite;
		}
	}

#if DEBUG_BUILD
	memset(jobData->threadMemPtr, 0x00, size + 1);
#endif

	va_end(args);
	return size;
}

String X64IRValueToStr(Context *context, IRValue value,
		BucketArray<Value, HeapAllocator, 1024> *localValues) {
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

	if (value.valueType == IRVALUETYPE_VALUE || value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		offset = value.value.offset;

	if (value.value.valueIdx & VALUE_GLOBAL_BIT)
		v = GetGlobalValue(context, value.value.valueIdx);
	else
		v = (*localValues)[value.value.valueIdx];

	if (v.flags & (VALUEFLAGS_ON_STATIC_STORAGE | VALUEFLAGS_IS_EXTERNAL)) {
		if (v.flags & VALUEFLAGS_IS_EXTERNAL)
			result = v.name;
		else
			result = TStringConcat("g_"_s, v.name);

		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);

		// Array indexing
		if (value.value.elementSize > 0) {
			String indexRegisterStr = X64IRValueToStr(context,
					IRValueValue(context, value.value.indexValueIdx), localValues);
			result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
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
		else if (value.valueType == IRVALUETYPE_VALUE_DEREFERENCE) {
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
		if (v.name)
			result = TPrintF("$vr%d\"%S\"", value.value.valueIdx, v.name);
		else
			result = TPrintF("$vr%d", value.value.valueIdx);

		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);

		// Array indexing
		if (value.value.elementSize > 0) {
			String indexRegisterStr = X64IRValueToStr(context,
					IRValueValue(context, value.value.indexValueIdx), localValues);
			result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
		}
	}

	// Array indexing
	if (value.value.elementSize > 0) {
		String indexRegisterStr = X64IRValueToStr(context,
				IRValueValue(value.value.indexValueIdx, TYPETABLEIDX_S64), localValues);
		result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
	}

	if (value.valueType != IRVALUETYPE_VALUE_DEREFERENCE && !(v.flags & VALUEFLAGS_IS_MEMORY))
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

bool IsValueInMemory(Context *context, IRValue irValue) {
	if (irValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		return true;
	if (irValue.valueType != IRVALUETYPE_VALUE)
		return false;
	Value value = IRGetValue(context, irValue.value.valueIdx);
	if (value.flags & (VALUEFLAGS_FORCE_MEMORY | VALUEFLAGS_IS_MEMORY |
				VALUEFLAGS_ON_STATIC_STORAGE))
		return true;
	return false;
}

bool FitsInOperand(Context *context, u8 acceptableOperands, IRValue value) {
	bool isImmediate = value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER ||
					   value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT;
	if (isImmediate)
		return acceptableOperands & OPERANDTYPE_IMMEDIATE;
	if (!IsValueInMemory(context, value))
		return acceptableOperands & OPERANDTYPE_REGISTER;
	return acceptableOperands & OPERANDTYPE_MEMORY;
}

bool CanValueBeMemory(Context *context, IRValue value) {
	if (value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		return true;
	bool isImmediate = value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER ||
					   value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT;
	if (isImmediate)
		return false;
	if (IRGetValue(context, value.value.valueIdx).flags & VALUEFLAGS_FORCE_REGISTER)
		return false;
	if ((IRGetValue(context, value.value.valueIdx).flags & (VALUEFLAGS_IS_ALLOCATED |
			VALUEFLAGS_IS_MEMORY)) == VALUEFLAGS_IS_ALLOCATED)
		return false;
	return true;
}

void X64Mov(Context *context, IRValue dst, IRValue src);
void X64MovNoTmp(Context *context, IRValue dst, IRValue src) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	X64Instruction result;
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

	if (dstType.typeCategory != TYPECATEGORY_FLOATING) {
		if (srcType.typeCategory != TYPECATEGORY_FLOATING) {
			result.type = X64_MOV;
			bool isSigned = srcType.typeCategory == TYPECATEGORY_INTEGER &&
				srcType.integerInfo.isSigned;
			if (srcType.size == 4) {
				if (isSigned && dstType.size > 4 && src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER) {
					// MOVSXD is R-RM
					IRValue newValue = IRValueNewValue(context, "_movsxd_tmp"_s, dst.typeTableIdx,
							VALUEFLAGS_FORCE_REGISTER);
					*BucketArrayAdd(&jobData->beInstructions) = { X64_MOVSXD, newValue, src };
					src = newValue;
				}
				ASSERT(dstType.size >= 4);
				dst.typeTableIdx = src.typeTableIdx;
			}
			else if (srcType.size < dstType.size && src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER) {
				X64InstructionType extendType = isSigned ? X64_MOVSX : X64_MOVZX;
				// MOVSX and MOVZX are R-RM
				IRValue newValue = IRValueNewValue(context, "_movzx_tmp"_s, dst.typeTableIdx,
						VALUEFLAGS_FORCE_REGISTER);
				*BucketArrayAdd(&jobData->beInstructions) = { extendType, newValue, src };
				src = newValue;
			}
			else if (srcType.size > dstType.size)
				src.typeTableIdx = dst.typeTableIdx;
		}
		else {
			// X64_CVTTSD2SI and CVTTSD2SI are R-RM
			ASSERT(dst.valueType == IRVALUETYPE_VALUE ||
				   dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
			IRValue newValue = IRValueNewValue(context, "_cvttsd2si_tmp"_s, dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, dst.value.valueIdx);
			X64InstructionType type;

			if (srcType.size == 4)
				type = X64_CVTTSS2SI;
			else {
				ASSERT(srcType.size == 8);
				type = X64_CVTTSD2SI;
			}
			*BucketArrayAdd(&jobData->beInstructions) = { type, newValue, src };

			result.type = X64_MOV;
			src = newValue;
		}
	}
	else if (dstType.size == 4) {
		if (srcType.typeCategory != TYPECATEGORY_FLOATING) {
			// Immediates should be converted to float in previous stages.
			ASSERT(src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER);
			if (srcType.size < 4) {
				bool isSigned = srcType.typeCategory == TYPECATEGORY_INTEGER &&
					srcType.integerInfo.isSigned;
				IRValue newValue = IRValueNewValue(context, "_cvt_tmp"_s, TYPETABLEIDX_U32,
						VALUEFLAGS_FORCE_REGISTER);
				X64InstructionType extendType = isSigned ? X64_MOVSX : X64_MOVZX;
				*BucketArrayAdd(&jobData->beInstructions) = { extendType, newValue, src };
				src = newValue;
			}
			result.type = X64_CVTSI2SS;
		}
		else if (srcType.size == 4)
			result.type = X64_MOVSS;
		else {
			ASSERT(srcType.size == 8);
			result.type = X64_CVTSD2SS;
		}
	}
	else {
		ASSERT(dstType.size == 8);
		if (srcType.typeCategory != TYPECATEGORY_FLOATING) {
			// Immediates should be converted to float in previous stages.
			ASSERT(src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER);
			if (srcType.size < 4) {
				bool isSigned = srcType.typeCategory == TYPECATEGORY_INTEGER &&
					srcType.integerInfo.isSigned;
				IRValue newValue = IRValueNewValue(context, "_cvt_tmp"_s, TYPETABLEIDX_U32,
						VALUEFLAGS_FORCE_REGISTER);
				X64InstructionType extendType = isSigned ? X64_MOVSX : X64_MOVZX;
				*BucketArrayAdd(&jobData->beInstructions) = { extendType, newValue, src };
				src = newValue;
			}
			result.type = X64_CVTSI2SD;
		}
		else if (srcType.size == 4)
			result.type = X64_CVTSS2SD;
		else {
			ASSERT(srcType.size == 8);
			result.type = X64_MOVSD;
		}
	}

	result.dst = dst;
	result.src = src;
	*BucketArrayAdd(&jobData->beInstructions) = result;
}

void X64Mov(Context *context, IRValue dst, IRValue src) {
	if (CanValueBeMemory(context, dst) && CanValueBeMemory(context, src)) {
		Value srcValue = IRGetValue(context, src.value.valueIdx);
		u32 srcUsedFlag = srcValue.flags & VALUEFLAGS_IS_USED;
		u32 immitateFlag = src.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, "_movtmp"_s, dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | srcUsedFlag | immitateFlag, src.value.valueIdx);

		X64MovNoTmp(context, tmp, src);
		src = tmp;
	}
	// Can't directly mov a 64 bit immediate to a memory location
	else if (CanValueBeMemory(context, dst) &&
			src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			src.immediate & 0xFFFFFFFF00000000) {
		IRValue tmp = IRValueNewValue(context, "_movimmtmp"_s, dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER);
		X64MovNoTmp(context, tmp, src);
		src = tmp;
	}

	X64MovNoTmp(context, dst, src);
}

void X64Test(Context *context, IRValue value) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	TypeInfo typeInfo = GetTypeInfo(context, value.typeTableIdx);
	if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
		floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));

	X64Instruction cmpInst;
	cmpInst.dst = value;
	switch (floatingType) {
	case X64FLOATINGTYPE_NONE:
	{
		cmpInst.type = X64_CMP;
		cmpInst.src = IRValueImmediate(0);
	} break;
	case X64FLOATINGTYPE_F32:
	{
		cmpInst.type = X64_COMISS;
		IRValue zero = IRValueNewValue(context, "_zero"_s, cmpInst.dst.typeTableIdx, 0);
		*BucketArrayAdd(&jobData->beInstructions) = { X64_XORPS, zero, zero };
		cmpInst.src = zero;
	} break;
	case X64FLOATINGTYPE_F64:
	{
		cmpInst.type = X64_COMISD;
		IRValue zero = IRValueNewValue(context, "_zero"_s, cmpInst.dst.typeTableIdx, 0);
		*BucketArrayAdd(&jobData->beInstructions) = { X64_XORPD, zero, zero };
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
		X64Mov(context, newValue, cmpInst.dst);
		cmpInst.dst = newValue;
	}

	*BucketArrayAdd(&jobData->beInstructions) = cmpInst;
}

IRValue X64PushRegisterParameter(u32 typeTableIdx, s32 *numberOfGPR, s32 *numberOfXMM) {
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
		return IRValueValue(XMM0.value.valueIdx + (*numberOfXMM)++, typeTableIdx);
	}
	return { IRVALUETYPE_INVALID };
}

void X64CopyMemory(Context *context, IRValue dst, IRValue src, IRValue size) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	ASSERT(dst.valueType == IRVALUETYPE_VALUE ||
		   dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
	ASSERT(src.valueType == IRVALUETYPE_VALUE ||
		   src.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
	u32 dstIdx = dst.value.valueIdx;
	u32 srcIdx = src.value.valueIdx;

	// First attempt to copy manually
	if (size.valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
		TypeInfo dstTypeInfo = GetTypeInfo(context, IRGetValue(context, dstIdx).typeTableIdx);
		TypeInfo srcTypeInfo = GetTypeInfo(context, IRGetValue(context, srcIdx).typeTableIdx);
		s64 sizeImm = size.immediate;

		s64 copiedBytes = 0;
		while (sizeImm - copiedBytes >= 16) {
			X64Mov(context,
					IRValueDereference(dstIdx, TYPETABLEIDX_128, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_128, copiedBytes));
			copiedBytes += 16;
		}
		while (sizeImm - copiedBytes >= 8) {
			X64Mov(context,
					IRValueDereference(dstIdx, TYPETABLEIDX_S64, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_S64, copiedBytes));
			copiedBytes += 8;
		}
		while (sizeImm - copiedBytes >= 4) {
			X64Mov(context,
					IRValueDereference(dstIdx, TYPETABLEIDX_S32, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_S32, copiedBytes));
			copiedBytes += 4;
		}
		while (sizeImm - copiedBytes >= 1) {
			X64Mov(context,
					IRValueDereference(dstIdx, TYPETABLEIDX_S8, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_S8, copiedBytes));
			++copiedBytes;
		}
		return;
	}

	X64Mov(context, RCX, dst);
	X64Mov(context, RDX, src);
	X64Mov(context, R8,  size);
	X64Instruction result = { X64_CALL };
	result.procedureIdx = copyMemoryProcIdx;
	ArrayInit(&result.parameterValues, 3);
	*ArrayAdd(&result.parameterValues) = RCX.value.valueIdx;
	*ArrayAdd(&result.parameterValues) = RDX.value.valueIdx;
	*ArrayAdd(&result.parameterValues) = R8.value.valueIdx;
	*BucketArrayAdd(&jobData->beInstructions) = result;
}

bool X64WinABIShouldPassByCopy(Context *context, u32 typeTableIdx) {
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

Array<u32, JobAllocator> X64ReadyWin64Parameters(Context *context,
		ArrayView<IRValue> parameters, bool isCaller) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	int parameterCount = (int)parameters.size;

	Array<u32, JobAllocator> parameterValues;
	ArrayInit(&parameterValues, parameterCount * 2);

	for (int i = 0; i < parameterCount; ++i) {
		IRValue param = parameters[i];
		u32 paramTypeIdx = StripAllAliases(context, param.typeTableIdx);
		TypeInfo paramType = GetTypeInfo(context, paramTypeIdx);

		if (isCaller && X64WinABIShouldPassByCopy(context, paramTypeIdx)) {
			static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

			u32 tmpValueIdx = IRNewValue(context, "paramcpy"_s, paramTypeIdx, 0);
			IRValue tmpValue = IRValueValue(tmpValueIdx, voidPtrTypeIdx);

			X64Instruction pushInst = { X64_Push_Value };
			pushInst.valueIdx = tmpValueIdx;
			*BucketArrayAdd(&jobData->beInstructions) = pushInst;

			IRValue ptr = IRValueNewValue(context, "paramptr"_s, TYPETABLEIDX_S64, 0);
			*BucketArrayAdd(&jobData->beInstructions) = { X64_LEA, ptr, tmpValue };

			X64CopyMemory(context, ptr, param, IRValueImmediate(paramType.size));
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
			X64Mov(context, slot, param);
		else {
			if (X64WinABIShouldPassByCopy(context, paramTypeIdx)) {
				u32 ptrTypeIdx = GetTypeInfoPointerOf(context, paramTypeIdx);
				param.typeTableIdx = ptrTypeIdx;
				slot.typeTableIdx = ptrTypeIdx;
				X64CopyMemory(context, param, slot,
						IRValueImmediate(paramType.size));
			}
			else
				X64Mov(context, param, slot);
		}

		*ArrayAdd(&parameterValues) = slot.value.valueIdx;
	}

	if (isCaller) {
		if (jobData->allocatedParameterCount < parameterCount)
			jobData->allocatedParameterCount = parameterCount;
	}

	return parameterValues;
}

Array<u32, JobAllocator> X64ReadyLinuxParameters(Context *context,
		ArrayView<IRValue> parameters, bool isCaller) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	int parameterCount = (int)parameters.size;

	Array<u32, JobAllocator> parameterValues;
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

			first = IRValueDereference(param.value.valueIdx, TYPETABLEIDX_S64, 0);
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
				second = IRValueDereference(param.value.valueIdx, TYPETABLEIDX_S64, 8);
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
						X64Mov(context, firstSlot,  first);
						if (secondSlot.valueType != IRVALUETYPE_INVALID)
							X64Mov(context, secondSlot, second);
					}
					else {
						X64Mov(context, first,  firstSlot);
						if (secondSlot.valueType != IRVALUETYPE_INVALID)
							X64Mov(context, second, secondSlot);
					}

					*ArrayAdd(&parameterValues) = firstSlot.value.valueIdx;
					*ArrayAdd(&parameterValues) = secondSlot.value.valueIdx;

					continue;
				}
				else {
					// Restore number of used registers and keep going.
					numberOfGPR = oldNumberOfGPR;
					numberOfXMM = oldNumberOfXMM;
				}
			}
		}

		if (paramTypeInfo.size > 8)
		{
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

				if (isCaller)
				{
					IRValue slot = IRValueDereference(jobData->x64SpilledParametersWrite[numberOfSpilled++],
							typeTableIdx);
					X64Mov(context, slot, param);
				}
				else
				{
					IRValue slot = IRValueDereference(jobData->x64SpilledParametersRead[numberOfSpilled++],
							typeTableIdx);
					X64Mov(context, param, slot);
				}
				param.value.offset += 8;
				sizeLeft -= 8;
			}
		}
		else
		{
			IRValue slot;
			if (isCaller)
			{
				slot = X64PushRegisterParameter(param.typeTableIdx, &numberOfGPR, &numberOfXMM);
				if (slot.valueType == IRVALUETYPE_INVALID)
					slot = IRValueDereference(jobData->x64SpilledParametersWrite[numberOfSpilled++],
							TYPETABLEIDX_S64);

				X64Mov(context, slot, param);
			}
			else
			{
				slot = X64PushRegisterParameter(param.typeTableIdx, &numberOfGPR, &numberOfXMM);
				if (slot.valueType == IRVALUETYPE_INVALID)
					slot = IRValueDereference(jobData->x64SpilledParametersRead[numberOfSpilled++],
							TYPETABLEIDX_S64);

				X64Mov(context, param, slot);
			}

			*ArrayAdd(&parameterValues) = slot.value.valueIdx;
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
	X64Instruction result = {};

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	bool isSigned = false;
	{
		u32 typeTableIdx = TYPETABLEIDX_S64;
		if      (inst.type >= IRINSTRUCTIONTYPE_COMPARE_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_COMPARE_END)
			typeTableIdx = inst.binaryOperation.left.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_BINARY_END)
			typeTableIdx = inst.binaryOperation.left.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_UNARY_END)
			typeTableIdx = inst.unaryOperation.in.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_COMPARE_JUMP_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_COMPARE_JUMP_END)
			typeTableIdx = inst.conditionalJump2.left.typeTableIdx;
		else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
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
		X64Mov(context, inst.assignment.dst, inst.assignment.src);
		return;
	}
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		src.typeTableIdx = voidPtrTypeIdx;
		if (IsValueInMemory(context, dst))
		{
			IRValue tmp = IRValueNewValue(context, "_lea_mm_tmp"_s, dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			*BucketArrayAdd(&jobData->beInstructions) = { X64_LEA, tmp, src };
			X64Mov(context, dst, tmp);
			src = tmp;
		}
		else
			*BucketArrayAdd(&jobData->beInstructions) = { X64_LEA, dst, src };
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
		X64Mov(context, inst.unaryOperation.out, inst.unaryOperation.in);
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_NEG;
			goto doRM;
		case X64FLOATINGTYPE_F32:
			result.type = X64_XORPS;
			result.src = IRValueImmediateFloat(context, -0.0, TYPETABLEIDX_F32);
			result.src.typeTableIdx = TYPETABLEIDX_128;
			break;
		case X64FLOATINGTYPE_F64:
			result.type = X64_XORPD;
			result.src = IRValueImmediateFloat(context, -0.0, TYPETABLEIDX_128);
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
						immitateFlag, left.value.valueIdx);

				X64Mov(context, tmp, left);
				*BucketArrayAdd(&jobData->beInstructions) = { X64_SAL, tmp,
						IRValueImmediate(63 - Nlz64(right.immediate)) };
				X64Mov(context, out, tmp);
				return;
			}
			else
			{
				if (isSigned)
				{
					result.type = X64_IMUL;
					goto doRM_RMI;
				}
				else
				{
					X64Mov(context, RAX, left);

					*BucketArrayAdd(&jobData->beInstructions) = { X64_XOR, RDX, RDX };

					IRValue multiplier = right;
					u8 accepted = x64InstructionInfos[X64_MUL].operandTypesLeft;
					if (!FitsInOperand(context, accepted, multiplier))
					{
						ASSERT(accepted & OPERANDTYPE_REGISTER);
						IRValue newValue = IRValueNewValue(context, multiplier.typeTableIdx,
								VALUEFLAGS_FORCE_REGISTER);
						X64Mov(context, newValue, multiplier);
						multiplier = newValue;
					}
					result.type = X64_MUL;
					result.dst = multiplier;
					*BucketArrayAdd(&jobData->beInstructions) = result;

					X64Mov(context, out, RAX);
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

			if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && IsPowerOf264(right.immediate))
			{
				u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
				IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx,
						immitateFlag, left.value.valueIdx);

				TypeInfo leftType = GetTypeInfo(context, left.typeTableIdx);
				X64InstructionType shiftType = isSigned ? X64_SAR : X64_SHR;

				X64Mov(context, tmp, left);
				*BucketArrayAdd(&jobData->beInstructions) = { shiftType, tmp,
						IRValueImmediate(63 - Nlz64(right.immediate)) };
				X64Mov(context, out, tmp);
			}
			else
			{
				X64Mov(context, RAX, left);

				if (isSigned)
					*BucketArrayAdd(&jobData->beInstructions) = { X64_CQO };
				else
					*BucketArrayAdd(&jobData->beInstructions) = { X64_XOR, RDX, RDX };

				IRValue divisor = right;
				u8 accepted = x64InstructionInfos[X64_DIV].operandTypesLeft;
				if (!FitsInOperand(context, accepted, divisor))
				{
					ASSERT(accepted & OPERANDTYPE_REGISTER);
					IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
							VALUEFLAGS_FORCE_REGISTER);
					X64Mov(context, newValue, divisor);
					divisor = newValue;
				}
				result.type = isSigned ? X64_IDIV : X64_DIV;
				result.dst = divisor;
				*BucketArrayAdd(&jobData->beInstructions) = result;

				X64Mov(context, out, RAX);
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
					left.value.valueIdx);

			X64Mov(context, tmp, left);
			*BucketArrayAdd(&jobData->beInstructions) = { X64_AND, tmp,
					IRValueImmediate(right.immediate - 1) };
			X64Mov(context, out, tmp);
		}
		else
		{
			X64Mov(context, RAX, left);
			if (isSigned)
				*BucketArrayAdd(&jobData->beInstructions) = { X64_CQO };
			else
				*BucketArrayAdd(&jobData->beInstructions) = { X64_XOR, RDX, RDX };

			IRValue divisor = right;
			u8 accepted = x64InstructionInfos[X64_DIV].operandTypesLeft;
			if (!FitsInOperand(context, accepted, divisor))
			{
				ASSERT(accepted & OPERANDTYPE_REGISTER);
				IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
						VALUEFLAGS_FORCE_REGISTER);
				X64Mov(context, newValue, divisor);
				divisor = newValue;
			}
			result.type = isSigned ? X64_IDIV : X64_DIV;
			result.dst = divisor;

			*BucketArrayAdd(&jobData->beInstructions) = result;
			X64Mov(context, out, RDX);
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
		X64Test(context, inst.unaryOperation.in);

		result.type = X64_SETE;
		result.dst = inst.unaryOperation.out;
		if (GetTypeInfo(context, result.dst.typeTableIdx).size != 1)
		{
			X64Mov(context, result.dst, IRValueImmediate(0));
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

				X64Instruction pushInst = { X64_Push_Value };
				pushInst.valueIdx = tmpValueIdx;
				*BucketArrayAdd(&jobData->beInstructions) = pushInst;

				IRValue ptr = IRValueNewValue(context, "returnptr"_s, TYPETABLEIDX_S64, 0);
				*BucketArrayAdd(&jobData->beInstructions) = { X64_LEA, ptr, tmpValue };

				*FixedArrayAdd(&paramSources) = ptr;
			}
		}

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
			*FixedArrayAdd(&paramSources) = inst.procedureCall.parameters[i];

		Array<u32, JobAllocator> paramValues;
		switch (callingConvention)
		{
			case CC_WIN64:
				paramValues =
					X64ReadyWin64Parameters(context, paramSources, true);
				break;
			case CC_DEFAULT:
			case CC_LINUX64:
			default:
				paramValues =
					X64ReadyLinuxParameters(context, paramSources, true);
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
					X64Mov(context, RAX, IRValueImmediate(i));
					break;
				}
			}
		}
#endif

		*BucketArrayAdd(&jobData->beInstructions) = result;

		u64 returnValueCount = inst.procedureCall.returnValues.size;
		if (returnValueCount)
		{
			if (callingConvention == CC_DEFAULT)
			{
				static IRValue integerReturnRegisters[]  = { RAX, RDI, RSI, RDX, RCX, R8, R9 };
				static IRValue floatingReturnRegisters[] = {
					XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8 };

				int integerIdx = 0;
				int floatingIdx = 0;
				for (int i = 0; i < returnValueCount; ++i)
				{
					IRValue slot;
					IRValue out = inst.procedureCall.returnValues[i];
					u32 returnTypeIdx = out.typeTableIdx;
					TypeInfo returnTypeInfo = GetTypeInfo(context, returnTypeIdx);
					if (returnTypeInfo.typeCategory == TYPECATEGORY_FLOATING)
					{
						slot = floatingReturnRegisters[floatingIdx++];
						X64Mov(context, out, slot);
					}
					else
					{
						slot = integerReturnRegisters[integerIdx++];

						if (IRShouldPassByCopy(context, returnTypeIdx))
						{
							IRValue ptr = IRValueNewValue(context, "paramptr"_s, TYPETABLEIDX_S64, 0);
							IRValue outButS64 = out;
							outButS64.typeTableIdx = TYPETABLEIDX_S64;
							*BucketArrayAdd(&jobData->beInstructions) = { X64_LEA, ptr, outButS64 };

							X64CopyMemory(context, ptr, slot, IRValueImmediate(returnTypeInfo.size));
						}
						else
							X64Mov(context, out, slot);
					}
				}
			}
			else
			{
				ASSERT(returnValueCount == 1);
				IRValue out = inst.procedureCall.returnValues[0];
				u32 returnTypeIdx = out.typeTableIdx;
				if (GetTypeInfo(context, returnTypeIdx).typeCategory == TYPECATEGORY_FLOATING)
				{
					IRValue typedXmm0 = XMM0;
					typedXmm0.typeTableIdx = returnTypeIdx;
					X64Mov(context, out, typedXmm0);
				}
				else
					X64Mov(context, out, RAX);
			}
		}
		return;
	}
	case IRINSTRUCTIONTYPE_INTRINSIC:
	{
		switch (inst.intrinsic.type)
		{
		case INTRINSIC_BREAKPOINT:
			*BucketArrayAdd(&jobData->beInstructions) = { X64_INT, IRValueImmediate(3) };
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
		X64CopyMemory(context, inst.copyMemory.dst, inst.copyMemory.src,
				inst.copyMemory.size);
		return;
	}
	case IRINSTRUCTIONTYPE_ZERO_MEMORY:
	{
		ASSERT(inst.zeroMemory.dst.valueType  == IRVALUETYPE_VALUE ||
			   inst.zeroMemory.dst.valueType  == IRVALUETYPE_VALUE_DEREFERENCE);
		u32 dstIdx = inst.zeroMemory.dst.value.valueIdx;

		// First attempt to zero manually
		if (inst.zeroMemory.size.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			TypeInfo dstTypeInfo = GetTypeInfo(context, IRGetValue(context, dstIdx).typeTableIdx);
			s64 size = inst.zeroMemory.size.immediate;

			s64 copiedBytes = 0;
			if (size - copiedBytes >= 16)
			{
				IRValue zeroXmmReg = IRValueNewValue(context, "_zeroxmm"_s, TYPETABLEIDX_128,
						VALUEFLAGS_FORCE_REGISTER);
				*BucketArrayAdd(&jobData->beInstructions) = { X64_XORPS, zeroXmmReg, zeroXmmReg };
				while (size - copiedBytes >= 16)
				{
					X64Mov(context,
							IRValueDereference(dstIdx, TYPETABLEIDX_128, copiedBytes), zeroXmmReg);
					copiedBytes += 16;
				}
			}
			if (size - copiedBytes >= 1)
			{
				IRValue zeroReg = IRValueNewValue(context, "_zeroreg"_s, TYPETABLEIDX_S64,
						VALUEFLAGS_FORCE_REGISTER);
				*BucketArrayAdd(&jobData->beInstructions) = { X64_XOR, zeroReg, zeroReg };
				while (size - copiedBytes >= 8)
				{
					X64Mov(context,
							IRValueDereference(dstIdx, TYPETABLEIDX_S64, copiedBytes), zeroReg);
					copiedBytes += 8;
				}
				while (size - copiedBytes >= 4)
				{
					X64Mov(context,
							IRValueDereference(dstIdx, TYPETABLEIDX_S32, copiedBytes), zeroReg);
					copiedBytes += 4;
				}
				while (size - copiedBytes >= 1)
				{
					X64Mov(context,
							IRValueDereference(dstIdx, TYPETABLEIDX_S8, copiedBytes), zeroReg);
					++copiedBytes;
				}
			}
			return;
		}

		X64Mov(context, RCX, inst.zeroMemory.dst);
		X64Mov(context, RDX,  inst.zeroMemory.size);
		result.type = X64_CALL;
		result.procedureIdx = zeroMemoryProcIdx;
		ArrayInit(&result.parameterValues, 2);
		*ArrayAdd(&result.parameterValues) = RCX.value.valueIdx;
		*ArrayAdd(&result.parameterValues) = RDX.value.valueIdx;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
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
			X64Mov(context, tmp, operand);
			operand = tmp;
		}

		X64Mov(context, operand, inst.unaryOperation.in);

		result.dst = operand;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		return;
	}
doRM_RMI:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			(right.immediate & 0xFFFFFFFF00000000))
		{
			IRValue tmp = IRValueNewValue(context, "_biglittmp"_s, right.typeTableIdx, 0);
			X64Mov(context, tmp, right);
			right = tmp;
		}

		u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, "_rmrmitmp"_s, left.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | immitateFlag, left.value.valueIdx);

		X64MovNoTmp(context, tmp, left);

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		X64Mov(context, out, tmp);

		return;
	}
doX_XM:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out = inst.binaryOperation.out;

		u32 immitateFlagLeft = out.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, left.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | immitateFlagLeft, out.value.valueIdx);

		X64MovNoTmp(context, tmp, left);

		u8 accepted = x64InstructionInfos[result.type].operandTypesRight;
		if (!FitsInOperand(context, accepted, right) ||
			right.typeTableIdx != left.typeTableIdx)
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagRight = right.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, out.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagRight, right.value.valueIdx);
			X64Mov(context, newValue, right);
			right = newValue;
		}

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		X64Mov(context, out, tmp);

		return;
	}
doShift:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, 0);

		X64Mov(context, tmp, left);

		if (right.valueType != IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			X64Mov(context, RCX, right);
			right = CL;
		}

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&jobData->beInstructions) = result;

		X64Mov(context, out, tmp);

		return;
	}
doConditionalJump:
	{
		X64Test(context, inst.conditionalJump.condition);

		result.label = inst.conditionalJump.label;
		*BucketArrayAdd(&jobData->beInstructions) = result;
		return;
	}
doConditionalJump2:
	{
		X64Instruction cmpInst;

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
					VALUEFLAGS_FORCE_REGISTER | immitateFlagLeft, left.value.valueIdx);
			X64Mov(context, newValue, left);
			left = newValue;
		}

		IRValue right = inst.conditionalJump2.right;
		accepted = x64InstructionInfos[cmpInst.type].operandTypesRight;
		if (!FitsInOperand(context, accepted, right) ||
			right.typeTableIdx != left.typeTableIdx ||
			(IsValueInMemory(context, left) && IsValueInMemory(context, right)) ||
			(right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && (right.immediate & 0xFFFFFFFF00000000)))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagRight = right.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, "_jump_hlp"_s, left.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagRight, right.value.valueIdx);
			X64Mov(context, newValue, right);
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

		if (cmpInst.src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			(cmpInst.src.immediate & 0xFFFFFFFF00000000))
		{
			IRValue tmp = IRValueNewValue(context, cmpInst.src.typeTableIdx, 0);
			X64Mov(context, tmp, cmpInst.src);
			cmpInst.src = tmp;
		}

		u8 accepted = x64InstructionInfos[cmpInst.type].operandTypesLeft;
		if (!FitsInOperand(context, accepted, cmpInst.dst) ||
			(IsValueInMemory(context, cmpInst.dst) && IsValueInMemory(context, cmpInst.src)))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			IRValue newValue = IRValueNewValue(context, "_setcc_hlp"_s, cmpInst.dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, newValue, cmpInst.dst);
			cmpInst.dst = newValue;
		}

		*BucketArrayAdd(&jobData->beInstructions) = cmpInst;

		result.dst = inst.binaryOperation.out;
		if (GetTypeInfo(context, result.dst.typeTableIdx).size != 1)
		{
			X64Mov(context, result.dst, IRValueImmediate(0));
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

		X64Mov(context, out, tmp);
		return;
	}
}

String X64InstructionToStr(Context *context, X64Instruction inst,
	BucketArray<Value, HeapAllocator, 1024> *localValues)
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
		if (instInfo.operandTypesLeft != OPERANDTYPE_NONE)
		{
			if (instInfo.operandTypesRight != OPERANDTYPE_NONE)
				goto printDstSrc;
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
printLabel:
	{
		return TPrintF("%S L_%S_%llx", mnemonic, inst.label->name, (u64)inst.label);
	}
}

inline s64 X64PrintInstruction(Context *context, X64Instruction inst,
	BucketArray<Value, HeapAllocator, 1024> *localValues)
{
	return PrintOut(context, "%S", X64InstructionToStr(context, inst, localValues));
}

#include "X64RegisterAllocation.cpp"

void X64PrintInstructions(Context *context)
{
	auto beFinalProcedureData = context->beFinalProcedureData.GetForRead();
	int procCount = (int)beFinalProcedureData->size;
	for (int finalProcIdx = 0; finalProcIdx < procCount; ++finalProcIdx)
	{
		X64FinalProcedure finalProc = (*beFinalProcedureData)[finalProcIdx];
		Procedure proc = GetProcedureRead(context, finalProc.procedureIdx);
#if IS_WINDOWS
		if (proc.isExported)
			PrintOut(context, "\n%S PROC PUBLIC\n", proc.name);
		else
			PrintOut(context, "\n%S PROC PRIVATE\n", proc.name);
#else
		PrintOut(context, "\n%S:\n", proc.name);
#endif
		PrintOut(context, "push rbp\n");
		PrintOut(context, "mov rbp, rsp\n");
		if (finalProc.stackSize > 0)
			PrintOut(context, "sub rsp, 0%llxh\n", finalProc.stackSize);

		X64InstructionStream stream = X64InstructionStreamBegin(&finalProc.instructions);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			if (X64PrintInstruction(context, *inst, &finalProc.localValues))
				PrintOut(context, "\n");
			inst = X64InstructionStreamAdvance(&stream);
		}

		PrintOut(context, "leave\n");
		PrintOut(context, "ret\n");
#if IS_WINDOWS
		PrintOut(context, "%S ENDP\n", proc.name);
#endif
	}
}

inline void X64StaticDataAlignTo(Context *context, s64 alignment, bool initialize)
{
#if IS_WINDOWS
	PrintOut(context, "ALIGN %d\n", alignment);
#else
	if (initialize)
		PrintOut(context, "ALIGN %d\n", alignment);
	else
		PrintOut(context, "ALIGNB %d\n", alignment);
#endif
}

void X64PrintStaticData(Context *context, String name, IRValue value, u32 typeTableIdx,
		int alignmentOverride = -1)
{
	switch (value.valueType)
	{
	case IRVALUETYPE_IMMEDIATE_STRING:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);

		String str;
		{
			auto stringLiterals = context->stringLiterals.GetForRead();
			str = (*stringLiterals)[value.immediateStringIdx];
		}
		s64 size = str.size;
		if (size == 0)
			PrintOut(context, "%S DQ 0H, 0H\n", name);
		else
		{
			for (int i = 0; i < str.size; ++i)
				if (str.data[i] == '\\') --size;
			PrintOut(context, "%S DQ %.16llxH, _str_%d\n", name, size, value.immediateStringIdx);
		}
	} break;
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	{
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);
		switch (typeInfo.size)
		{
		case 4:
		{
			union { u32 asU32; f32 asF32; };
			asF32 = (f32)value.immediateFloat;
			PrintOut(context, "%S DD 0%.8xH\n", name, asU32);
		} break;
		case 8:
		default:
			PrintOut(context, "%S DQ 0%.16llxH\n", name,
					value.immediate);
			break;
		}
	} break;
	case IRVALUETYPE_TUPLE:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);

		bool isArray = false;
		u32 elementTypeIdx = TYPETABLEIDX_Unset;
		if (typeTableIdx > TYPETABLEIDX_Unset)
		{
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
			isArray = typeInfo.typeCategory == TYPECATEGORY_ARRAY;
			elementTypeIdx = typeInfo.arrayInfo.elementTypeTableIdx;
		}

		ArrayView<IRValue> members = value.tuple;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx)
		{
			String memberName = memberIdx ? "   "_s : name;
			u32 memberTypeIdx = isArray ? elementTypeIdx : members[memberIdx].typeTableIdx;
			X64PrintStaticData(context, memberName, members[memberIdx], memberTypeIdx);
		}
	} break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
	{
		// @Improve: We are kinda using this to mean 'this is a value in data section, just put the
		// name in' which has nothing to do with 'VALUE_DEREFERENCE'...
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);

		Value v = IRGetValue(context, value.value.valueIdx);
		ASSERT(v.flags & VALUEFLAGS_ON_STATIC_STORAGE);
		ASSERT(v.name.size);
		PrintOut(context, "%S DQ g_%S\n", name, v.name);
	} break;
	case IRVALUETYPE_INVALID:
	{
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
#if IS_WINDOWS
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name, typeInfo.size);
#else
		PrintOut(context, "%S: RESB %llxH\n", name, typeInfo.size);
#endif
	} break;
	default:
	{
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);
		switch (typeInfo.size)
		{
		case 1:
			PrintOut(context, "%S DB %.2llxH\n", name,
					value.immediate);
			break;
		case 2:
			PrintOut(context, "%S DW %.4llxH\n", name,
					value.immediate);
			break;
		case 4:
			PrintOut(context, "%S DD %.8llxH\n", name,
					value.immediate);
			break;
		case 8:
			PrintOut(context, "%S DQ %.16llxH\n", name,
					value.immediate);
			break;
		default:
			ASSERT(!"Invalid immediate size");
		}
	}
	}
}

void X64PrintStaticDataUninitialized(Context *context, String name, IRValue value, u32 typeTableIdx,
		int alignmentOverride = -1)
{
	switch (value.valueType)
	{
	case IRVALUETYPE_IMMEDIATE_STRING:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, false);

		String str;
		{
			auto stringLiterals = context->stringLiterals.GetForRead();
			str = (*stringLiterals)[value.immediateStringIdx];
		}
		ASSERT(str.size == 0);
#if IS_WINDOWS
		PrintOut(context, "COMM %S:QWORD:02H\n", name);
#else
		PrintOut(context, "%S: RESQ 02H\n", name);
#endif
	} break;
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	{
		ASSERT(value.immediateFloat == 0);
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, false);
#if IS_WINDOWS
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name, typeInfo.size);
#else
		PrintOut(context, "%S: RESB 0%llxH\n", name, typeInfo.size);
#endif
	} break;
	case IRVALUETYPE_TUPLE:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, false);

		bool isArray = false;
		u32 elementTypeIdx = TYPETABLEIDX_Unset;
		if (typeTableIdx > 0)
		{
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
			isArray = typeInfo.typeCategory == TYPECATEGORY_ARRAY;
			elementTypeIdx = typeInfo.arrayInfo.elementTypeTableIdx;
		}

		ArrayView<IRValue> members = value.tuple;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx)
		{
			String memberName = memberIdx ? "   "_s : name;
			u32 memberTypeIdx = isArray ? elementTypeIdx : members[memberIdx].typeTableIdx;
			X64PrintStaticDataUninitialized(context, memberName, members[memberIdx], memberTypeIdx);
		}
	} break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
	{
		ASSERT(!"Shouldn't be calling this with a dereference value");
	} break;
	case IRVALUETYPE_INVALID:
	default:
	{
		ASSERT(value.immediate == 0);
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
#if IS_WINDOWS
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name, typeInfo.size);
#else
		PrintOut(context, "%S: RESB 0%llxH\n", name, typeInfo.size);
#endif
	} break;
	}
}

void WriteOutOutputBuffer(Context *context, String filename)
{
	FileHandle outputFile = SYSOpenFileWrite(filename);
	for (int i = 0; i < context->outputBuffer.buckets.size; ++i)
	{
		SYSWriteFile(outputFile,
				context->outputBuffer.buckets[i].data,
				context->outputBuffer.buckets[i].size);
	}
	SYSCloseFile(outputFile);
}

void BackendMain(Context *context)
{
	{
		auto finalProcs = context->beFinalProcedureData.GetForWrite();
		DynamicArrayInit(&finalProcs, 256);
	}

	// Hard coded CopyMemory and ZeroMemory external procedures
	{
		u32 voidPtrIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

		TypeInfo t = { TYPECATEGORY_PROCEDURE };
		ArrayInit(&t.procedureInfo.parameters, 3);
		t.procedureInfo.parameters.size = 3;
		t.procedureInfo.parameters[0] = { voidPtrIdx, {} };
		t.procedureInfo.parameters[1] = { voidPtrIdx, {} };
		t.procedureInfo.parameters[2] = { TYPETABLEIDX_S64, {} };
		u32 typeTableIdx = FindOrAddTypeTableIdx(context, t);

		auto externalProcedures = context->externalProcedures.GetForWrite();
		copyMemoryProcIdx = (u32)BucketArrayCount(&externalProcedures) | PROCEDURE_EXTERNAL_BIT;
		Procedure *copyMemory = BucketArrayAdd(&externalProcedures);
		*copyMemory = {};
		copyMemory->name = "CopyMemory"_s;
		copyMemory->typeTableIdx = typeTableIdx;

		zeroMemoryProcIdx = (u32)BucketArrayCount(&externalProcedures) | PROCEDURE_EXTERNAL_BIT;
		Procedure *zeroMemory = BucketArrayAdd(&externalProcedures);
		*zeroMemory = {};
		zeroMemory->name = "ZeroMemory"_s;
		zeroMemory->typeTableIdx = typeTableIdx;
	}

	x64InstructionInfos[X64_INT] =       { "int"_s,       OPERANDTYPE_IMMEDIATE, OPERANDACCESS_READ };
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
	x64InstructionInfos[X64_IMUL] =      { "imul"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
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

void BackendGenerateOutputFile(Context *context)
{
	// TypeInfo immediate structs
	{
		u32 pointerToStructMemberInfoIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT);
		u32 pointerToStringIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_STRING_STRUCT);
		u32 pointerToS64Idx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_S64);
		u32 pointerToTypeInfoIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT);

		const auto &typeTable = context->typeTable.LockForRead();
		u64 tableSize = BucketArrayCount(&typeTable);
		for (u32 typeTableIdx = TYPETABLEIDX_Begin; typeTableIdx < tableSize; ++typeTableIdx)
		{
			TypeInfo typeInfo = typeTable[typeTableIdx];

			Value v = GetGlobalValue(context, typeInfo.valueIdx);
			v.typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;
			IRUpdateValue(context, typeInfo.valueIdx, &v);

			IRStaticVariable newStaticVar = { typeInfo.valueIdx };
			newStaticVar.initialValue.valueType = IRVALUETYPE_TUPLE;
			newStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_Unset;

			switch (typeInfo.typeCategory)
			{
			case TYPECATEGORY_INTEGER:
			{
				ArrayInit(&newStaticVar.initialValue.tuple, 3);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(0, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(typeInfo.integerInfo.isSigned, TYPETABLEIDX_S32) };
			} break;
			case TYPECATEGORY_FLOATING:
			{
				newStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;
				ArrayInit(&newStaticVar.initialValue.tuple, 2);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(1, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
			} break;
			case TYPECATEGORY_STRUCT:
			case TYPECATEGORY_UNION:
			{
				String structName = typeInfo.structInfo.name;
				if (!structName.size)
					structName = "<anonymous struct>"_s;

				u32 membersValueIdx = NewGlobalValue(context, SNPrintF("_members_%lld", 16, typeTableIdx),
						TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable membersStaticVar = { membersValueIdx };
				membersStaticVar.initialValue.valueType = IRVALUETYPE_TUPLE;
				membersStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_Unset;
				ArrayInit(&membersStaticVar.initialValue.tuple,
						typeInfo.structInfo.members.size);
				for (s64 memberIdx = 0; memberIdx < (s64)typeInfo.structInfo.members.size; ++memberIdx)
				{
					StructMember member = typeInfo.structInfo.members[memberIdx];
					TypeInfo memberType = typeTable[member.typeTableIdx];

					IRValue memberImm;
					memberImm.valueType = IRVALUETYPE_TUPLE;
					memberImm.typeTableIdx = TYPETABLEIDX_Unset;
					ArrayInit(&memberImm.tuple, 4);
					*ArrayAdd(&memberImm.tuple) =
						{ IRValueImmediateString(context, member.name) };
					*ArrayAdd(&memberImm.tuple) =
						{ IRValueDereference(memberType.valueIdx, pointerToTypeInfoIdx) };
					*ArrayAdd(&memberImm.tuple) =
						{ IRValueImmediate(member.offset, TYPETABLEIDX_S64) };

					*ArrayAdd(&membersStaticVar.initialValue.tuple) = memberImm;
				}
				*DynamicArrayAdd(&context->irStaticVariables.GetForWrite()) = membersStaticVar;

				ArrayInit(&newStaticVar.initialValue.tuple, 6);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(2, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediateString(context, structName) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(typeInfo.typeCategory == TYPECATEGORY_UNION, TYPETABLEIDX_S32) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(typeInfo.structInfo.members.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueDereference(membersValueIdx, pointerToStructMemberInfoIdx) };
			} break;
			case TYPECATEGORY_ENUM:
			{
				String enumName = typeInfo.enumInfo.name;
				if (!enumName.size)
					enumName = "<anonymous enum>"_s;

				TypeInfo enumType = typeTable[typeInfo.enumInfo.typeTableIdx];

				u32 namesValueIdx = NewGlobalValue(context, SNPrintF("_names_%lld", 12, typeTableIdx),
						TYPETABLEIDX_STRING_STRUCT, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable namesStaticVar = { namesValueIdx };
				namesStaticVar.initialValue.valueType = IRVALUETYPE_TUPLE;
				ArrayInit(&namesStaticVar.initialValue.tuple,
						typeInfo.enumInfo.names.size);
				for (s64 nameIdx = 0; nameIdx < (s64)typeInfo.enumInfo.names.size; ++nameIdx)
				{
					IRValue nameImm = IRValueImmediateString(context,
							typeInfo.enumInfo.names[nameIdx]);
					*ArrayAdd(&namesStaticVar.initialValue.tuple) = nameImm;
				}
				*DynamicArrayAdd(&context->irStaticVariables.GetForWrite()) = namesStaticVar;

				u32 valuesValueIdx = NewGlobalValue(context, SNPrintF("_values_%lld", 14, typeTableIdx),
						TYPETABLEIDX_S64, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable valuesStaticVar = { valuesValueIdx };
				valuesStaticVar.initialValue.valueType = IRVALUETYPE_TUPLE;
				ArrayInit(&valuesStaticVar.initialValue.tuple,
						typeInfo.enumInfo.values.size);
				for (s64 valueIdx = 0; valueIdx < (s64)typeInfo.enumInfo.values.size; ++valueIdx)
				{
					IRValue valueImm = IRValueImmediate(typeInfo.enumInfo.values[valueIdx],
							TYPETABLEIDX_S64);
					*ArrayAdd(&valuesStaticVar.initialValue.tuple) = valueImm;
				}
				*DynamicArrayAdd(&context->irStaticVariables.GetForWrite()) = valuesStaticVar;

				ArrayInit(&newStaticVar.initialValue.tuple, 8);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(3, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediateString(context, enumName) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueDereference(enumType.valueIdx, pointerToTypeInfoIdx) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.enumInfo.names.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueDereference(namesValueIdx, pointerToStringIdx) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.enumInfo.values.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueDereference(valuesValueIdx, pointerToS64Idx) };
			} break;
			case TYPECATEGORY_POINTER:
			{
				TypeInfo pointedType = typeTable[typeInfo.pointerInfo.pointedTypeTableIdx];

				ArrayInit(&newStaticVar.initialValue.tuple, 3);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(4, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueDereference(pointedType.valueIdx, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_ARRAY:
			{
				TypeInfo elementType = typeTable[typeInfo.arrayInfo.elementTypeTableIdx];

				ArrayInit(&newStaticVar.initialValue.tuple, 4);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(5, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.arrayInfo.count, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueDereference(elementType.valueIdx, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_PROCEDURE:
			{
				u32 parametersValueIdx = 0;
				if (typeInfo.procedureInfo.parameters.size > 0)
				{
					parametersValueIdx = NewGlobalValue(context, SNPrintF("_params_%lld", 14, typeTableIdx),
							pointerToTypeInfoIdx, VALUEFLAGS_ON_STATIC_STORAGE);
					IRStaticVariable paramsStaticVar = { parametersValueIdx };
					paramsStaticVar.initialValue.valueType = IRVALUETYPE_TUPLE;
					ArrayInit(&paramsStaticVar.initialValue.tuple,
							typeInfo.procedureInfo.parameters.size);
					for (s64 paramIdx = 0; paramIdx < (s64)typeInfo.procedureInfo.parameters.size;
							++paramIdx)
					{
						TypeInfo paramType =
							typeTable[typeInfo.procedureInfo.parameters[paramIdx].typeTableIdx];
						IRValue paramImm = IRValueDereference(paramType.valueIdx, pointerToTypeInfoIdx);
						*ArrayAdd(&paramsStaticVar.initialValue.tuple) = paramImm;
					}
					*DynamicArrayAdd(&context->irStaticVariables.GetForWrite()) = paramsStaticVar;
				}

				ArrayInit(&newStaticVar.initialValue.tuple, 5);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(6, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.procedureInfo.parameters.size, TYPETABLEIDX_S64) };
				if (parametersValueIdx)
					*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueDereference(parametersValueIdx, pointerToTypeInfoIdx) };
				else
					*ArrayAdd(&newStaticVar.initialValue.tuple) =
					{ IRValueImmediate(0, pointerToTypeInfoIdx) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.procedureInfo.isVarargs, TYPETABLEIDX_BOOL) };
			} break;
			case TYPECATEGORY_ALIAS:
			{
				TypeInfo aliasedType = typeTable[typeInfo.aliasInfo.aliasedTypeIdx];

				ArrayInit(&newStaticVar.initialValue.tuple, 3);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(7, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueDereference(aliasedType.valueIdx, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_INVALID:
			{
				ArrayInit(&newStaticVar.initialValue.tuple, 1);
				*ArrayAdd(&newStaticVar.initialValue.tuple) =
				{ IRValueImmediate(8, TYPETABLEIDX_S8) };
			} break;
			default:
				ASSERT(false);
			}
			auto staticVars = context->irStaticVariables.GetForWrite();
			*DynamicArrayAdd(&staticVars) = newStaticVar;
		}
		context->typeTable.UnlockForRead();
	}

	BucketArrayInit(&context->outputBuffer);

#if IS_WINDOWS
	String memoryUtilFullpath = SYSExpandPathCompilerRelative("core\\memory_masm.asm"_s);
	PrintOut(context, "include <%S>\n\n", memoryUtilFullpath);
#else
	String memoryUtilFullpath = SYSExpandPathCompilerRelative("core/memory_nasm.asm"_s);
	PrintOut(context, "%cinclude \"%S\"\n\n", '%', memoryUtilFullpath);
#endif

#if IS_WINDOWS
	PrintOut(context, "_DATA SEGMENT\n");
#else
	PrintOut(context, "section .data\n");
#endif

	// String literals
	IRJobData *jobData = (IRJobData *)SYSGetThreadData(context->tlsIndex);
	{
		auto stringLiterals = context->stringLiterals.GetForRead();
		s64 strCount = (s64)BucketArrayCount(&stringLiterals);
		s64 bytesWritten = 0;
		for (s64 stringLiteralIdx = 1; stringLiteralIdx < strCount; ++stringLiteralIdx)
		{
			PrintOut(context, "_str_%lld DB ", stringLiteralIdx);
			String str = (*stringLiterals)[stringLiteralIdx];
			s64 size = str.size;
			bool first = true;
			char *buffer = (char *)jobData->jobMemPtr;
			char *out = buffer;
			const u8 *in = (const u8 *)str.data;
			for (int i = 0; i < str.size; ++i)
			{
				if (*in == '\\')
				{
					if (!first) PrintOut(context, ", ");

					++in;
					switch (*in)
					{
					case 'n':
						PrintOut(context, "0AH");
						break;
					case '0':
						PrintOut(context, "00H");
						break;
					case '"':
						PrintOut(context, "22H");
						break;
					}
					++in;
					++i;
					--size; // Don't count backslash for string size.
					first = false;
				}
				else if (*in == '\'')
				{
					// MASM uses ' as string delimiters
					if (!first) PrintOut(context, ", ");
					PrintOut(context, "27H");
					++in;
					first = false;
				}
				else
				{
					*out++ = *in++;

					if (i == str.size - 1 || *in == '\\' || *in == '\'')
					{
						*out++ = 0;
						jobData->jobMemPtr = out;

						if (!first) PrintOut(context, ", ");
						PrintOut(context, "'%s'", buffer);
						out = buffer;

						first = false;
					}
				}
			}
			PrintOut(context, "\n");
			jobData->jobMemPtr = buffer;
			bytesWritten += size;
		}
	}

	X64StaticDataAlignTo(context, 16, true);

	// Initialized
	{
		auto staticVars = context->irStaticVariables.GetForRead();
		const u64 staticVariableCount = staticVars->size;
		for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
		{
			IRStaticVariable staticVar = (*staticVars)[staticVariableIdx];
			if (staticVar.initialValue.valueType != IRVALUETYPE_INVALID &&
					staticVar.initialValue.immediate != 0)
			{
				Value value = GetGlobalValue(context, staticVar.valueIdx);

				String name = value.name;
				if (!(value.flags & VALUEFLAGS_IS_EXTERNAL))
					name = TStringConcat("g_"_s, name);

				X64PrintStaticData(context, name, staticVar.initialValue, value.typeTableIdx, 16);
			}
		}

#if IS_WINDOWS
		PrintOut(context, "_DATA ENDS\n");
		PrintOut(context, "_BSS SEGMENT\n");
#else
		PrintOut(context, "section .bss\n");
#endif

		// Uninitialized
		// @Speed: don't iterate this twice...
		for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
		{
			IRStaticVariable staticVar = (*staticVars)[staticVariableIdx];
			if (staticVar.initialValue.valueType == IRVALUETYPE_INVALID ||
					staticVar.initialValue.immediate == 0)
			{
				Value value = GetGlobalValue(context, staticVar.valueIdx);

				String name = value.name;
				if (!(value.flags & VALUEFLAGS_IS_EXTERNAL))
					name = TStringConcat("g_"_s, name);

				X64PrintStaticDataUninitialized(context, name,
						staticVar.initialValue, value.typeTableIdx, 16);
			}
		}
	}

#if IS_WINDOWS
	PrintOut(context, "_BSS ENDS\n");
#endif

#if IS_LINUX
	u64 procedureCount = BucketArrayCount(&context->procedures.GetForRead());
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure proc = GetProcedureRead(context, procedureIdx);
		if (proc.isExported)
			PrintOut(context, "GLOBAL %S\n", proc.name);
	}
#endif

	{
		auto externalVars = context->irExternalVariables.GetForRead();
		for (int varIdx = 0; varIdx < externalVars->size; ++varIdx)
		{
			Value v = GetGlobalValue(context, (*externalVars)[varIdx]);
			s64 size = GetTypeInfo(context, v.typeTableIdx).size;
			String type;
			switch (size)
			{
				case 1: type = "BYTE"_s; break;
				case 2: type = "WORD"_s; break;
				case 4: type = "DWORD"_s; break;
				case 8: type = "QWORD"_s; break;
				default: type = "QWORD"_s;
			}
#if IS_WINDOWS
			PrintOut(context, "EXTRN %S:%S\n", v.name, type);
#else
			PrintOut(context, "EXTERN %S:%S\n", v.name, type);
#endif
		}
	}

	{
		auto externalProcedures = context->externalProcedures.GetForRead();
		u64 externalProcedureCount = BucketArrayCount(&externalProcedures);
		for (u32 procedureIdx = 1; procedureIdx < externalProcedureCount; ++procedureIdx)
		{
			// Don't declare hard-coded procedures that are already included in the asm file.
			u32 externalProcIdx = procedureIdx | PROCEDURE_EXTERNAL_BIT;
			if (externalProcIdx == copyMemoryProcIdx || externalProcIdx == zeroMemoryProcIdx)
				continue;

			String procName = (*externalProcedures)[procedureIdx].name;
#if IS_WINDOWS
			PrintOut(context, "EXTRN %S:proc\n", procName);
#else
			PrintOut(context, "EXTERN %S\n", procName);
#endif
		}
	}

#if IS_WINDOWS
	PrintOut(context, "_TEXT SEGMENT\n");
#else
	PrintOut(context, "section .text\n");
#endif

	// Code
	X64PrintInstructions(context);

#if IS_WINDOWS
	PrintOut(context, "_TEXT ENDS\n");
	PrintOut(context, "END\n");
#endif

	String outputPath = SYSExpandPathWorkingDirectoryRelative("output"_s);
	SYSCreateDirectory(outputPath);

	WriteOutOutputBuffer(context, "output/out.asm"_s);

	TimerSplit("X64 output file write"_s);

	bool makeLibrary = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = BucketArrayCount(&staticDefinitions);
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &(*staticDefinitions)[i];
			if (StringEquals("compiler_output_type"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_INTEGER);
				makeLibrary = currentDef->constant.valueAsInt == 1;
			}
		}
	}

	String extraLinkerArguments = {};
	for (int i = 0; i < context->libsToLink.size; ++i)
		extraLinkerArguments = TPrintF("%S %S", extraLinkerArguments,
				context->libsToLink[i]);

#if IS_WINDOWS
	bool useWindowsSubsystem = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = BucketArrayCount(&staticDefinitions);
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &(*staticDefinitions)[i];
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
		SYSRunAssembler(outputPath, ""_s);
		TimerSplit("Calling assembler"_s);

		SYSRunLinker(outputPath, makeLibrary, extraLinkerArguments);
		TimerSplit("Calling linker"_s);
	}
}

void BackendJobProc(Context *context, u32 procedureIdx)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	// Initialize generic parameter values
	for (int paramIdx = 0; paramIdx < 32; ++paramIdx)
	{
		Value newValue = {};
		newValue.name = SNPrintF("_param%d", 8, paramIdx);
		newValue.typeTableIdx = TYPETABLEIDX_S64;
		newValue.flags = VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY | VALUEFLAGS_BASE_RELATIVE;
		newValue.stackOffset = 16 + paramIdx * 8; // Add 16, 8 for return address, and 8 because we push RBP
		u32 newValueIdx = IRNewValue(context, newValue);
		jobData->x64SpilledParametersRead[paramIdx] = newValueIdx;
	}

	for (int paramIdx = 0; paramIdx < 32; ++paramIdx)
	{
		Value newValue = {};
		newValue.name = SNPrintF("_param%d", 8, paramIdx);
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
	Array<IRValue, JobAllocator> params;
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
			X64ReadyWin64Parameters(context, params, false);
			break;
		case CC_DEFAULT:
		case CC_LINUX64:
			X64ReadyLinuxParameters(context, params, false);
	}

	u64 instructionCount = BucketArrayCount(&jobData->irInstructions);
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
	{
		IRInstruction inst = jobData->irInstructions[instructionIdx];
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
			for (int i = 0; i < returnValueCount; ++i)
			{
				IRValue slot;
				u32 returnTypeIdx = procTypeInfo.returnTypeIndices[i];
				IRValue out = IRValueValue(proc.returnValueIndices[i], returnTypeIdx, 0);
				TypeInfo returnTypeInfo = GetTypeInfo(context, returnTypeIdx);
				if (returnTypeInfo.typeCategory == TYPECATEGORY_FLOATING)
				{
					slot = floatingReturnRegisters[floatingIdx++];
					X64Mov(context, slot, out);
				}
				else
				{
					slot = integerReturnRegisters[integerIdx++];

					if (IRShouldPassByCopy(context, returnTypeIdx))
					{
						IRValue outButS64 = out;
						outButS64.typeTableIdx = TYPETABLEIDX_S64;
						*BucketArrayAdd(&jobData->beInstructions) = { X64_LEA, slot, outButS64 };
					}
					else
						X64Mov(context, slot, out);
				}
			}
		}
		else
		{
			u32 returnTypeIdx = procTypeInfo.returnTypeIndices[0];
			IRValue returnValue = IRValueValue(proc.returnValueIndices[0], returnTypeIdx);
			bool returnByCopy = IRShouldPassByCopy(context, returnTypeIdx);
			if (returnByCopy && procTypeInfo.callingConvention != CC_DEFAULT)
				*BucketArrayAdd(&jobData->beInstructions) = { X64_LEA, RAX, returnValue };
			else if (!returnByCopy)
			{
				if (GetTypeInfo(context, returnTypeIdx).typeCategory == TYPECATEGORY_FLOATING)
				{
					IRValue typedXmm0 = XMM0;
					typedXmm0.typeTableIdx = returnTypeIdx;
					X64Mov(context, typedXmm0, returnValue);
				}
				else
					X64Mov(context, RAX, returnValue);
			}
		}
	}

	X64AllocateRegisters(context);

	// Remove instructions that reference unused values
	X64InstructionStream stream = X64InstructionStreamBegin(&jobData->beInstructions);
	X64Instruction *inst = X64InstructionStreamAdvance(&stream);
	X64Instruction *nextInst  = X64InstructionStreamAdvance(&stream);
	X64Instruction *nextInst2 = X64InstructionStreamAdvance(&stream);
	while (inst)
	{
		// Replace LEAs with a register as a source with a MOV.
		if (inst->type == X64_LEA)
		{
			if ((inst->src.valueType == IRVALUETYPE_VALUE ||
				inst->src.valueType == IRVALUETYPE_VALUE_DEREFERENCE) &&
				(inst->dst.valueType == IRVALUETYPE_VALUE ||
				inst->dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE) &&
				inst->src.value.offset == 0 && inst->src.value.elementSize == 0)
			{
				Value v = IRGetValue(context, inst->src.value.valueIdx);
				if ((v.flags & VALUEFLAGS_IS_ALLOCATED) && !(v.flags & VALUEFLAGS_IS_MEMORY))
				{
					inst->type = X64_MOV;
					inst->src.valueType = IRVALUETYPE_VALUE;
				}
			}
		}

		switch (inst->type)
		{
		// dst write, src read
		case X64_MOVUPS:
		{
			// If aligned change to MOVAPS
			ASSERT((inst->dst.valueType == IRVALUETYPE_VALUE ||
				 inst->dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE) &&
				(inst->src.valueType == IRVALUETYPE_VALUE ||
				 inst->src.valueType == IRVALUETYPE_VALUE_DEREFERENCE));

			Value dst = IRGetValue(context, inst->dst.value.valueIdx);
			Value src = IRGetValue(context, inst->src.value.valueIdx);
			if (dst.flags & VALUEFLAGS_IS_ALLOCATED && src.flags & VALUEFLAGS_IS_ALLOCATED)
			{
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
				Value dst = IRGetValue(context, inst->dst.value.valueIdx);
				Value src = IRGetValue(context, inst->src.value.valueIdx);
				if (dst.flags & VALUEFLAGS_IS_ALLOCATED && src.flags & VALUEFLAGS_IS_ALLOCATED)
				{
					// Value::stackOffset is alias of Value::allocatedRegister
					if (dst.allocatedRegister == src.allocatedRegister)
					{
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
				inst->dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
			{
				Value v = IRGetValue(context, inst->dst.value.valueIdx);
				if (!(v.flags & (VALUEFLAGS_IS_USED | VALUEFLAGS_ON_STATIC_STORAGE)))
					inst->type = X64_Ignore;
			}
		} break;
		}

		// Zero idioms
		if (inst->type == X64_MOVSS || inst->type == X64_MOVSD)
		{
			if (inst->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
				inst->src.immediate == 0)
			{
				if (IsValueInMemory(context, inst->dst))
					inst->type = X64_MOV;
				else
					*inst = { inst->type == X64_MOVSS ? X64_XORPS : X64_XORPD,
						inst->dst, inst->dst };
			}
		}
		else if (inst->type == X64_MOV)
		{
			if (inst->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
				inst->src.immediate == 0)
			{
				if (!IsValueInMemory(context, inst->dst))
					*inst = { X64_XOR, inst->dst, inst->dst };
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
		if (nextInst2 && nextInst2->type == X64_CMP)
		{
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
				nextInst2->type == X64_JE)
		{
			if (inst->dst.value.valueIdx == nextInst->dst.value.valueIdx &&
				inst->dst.value.valueIdx == nextInst->src.value.valueIdx)
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
	finalProc.instructions = jobData->beInstructions;
	finalProc.localValues = jobData->localValues;
	finalProc.stackSize = jobData->stackSize;
	auto finalProcs = context->beFinalProcedureData.GetForWrite();
	*DynamicArrayAdd(&finalProcs) = finalProc;
}
