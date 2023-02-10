#define CT_ENABLE_VERBOSE_LOGGING 0

#if CT_ENABLE_VERBOSE_LOGGING
#define CTVerboseLog(_context, _loc, _string) LogNote(_context, _loc, _string)
#else
#define CTVerboseLog(...)
#endif

Value CTGetValue(CTContext *context, u32 valueIdx)
{
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = g_context->globalValues.GetForRead();
		return globalValues[valueIdx & VALUE_GLOBAL_MASK];
	}
	else
		return context->localValues[valueIdx];
}

CTRegister *CTGetValueContent(CTContext *context, u32 valueIdx)
{
	Value v = CTGetValue(context, valueIdx);
	if (valueIdx & VALUE_GLOBAL_BIT) {
		if (v.flags & VALUEFLAGS_IS_EXTERNAL) {
#if DEBUG_BUILD
			LogError(context->currentLoc, TPrintF("Can't access "
					"external variable \"%S\" during compile time", v.name));
#else
			LogError(context->currentLoc, "Can't access "
					"external variable during compile time"_s);
#endif
		}

		SpinlockLock(&g_context->globalValuesLock);
		auto globalValues = g_context->globalValueContents;
		void **ptr = HashMapGet(globalValues, valueIdx & VALUE_GLOBAL_MASK);
		if  (!ptr) {
			SwitchJob(YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED,
					{ .index = valueIdx });
			SpinlockLock(&g_context->globalValuesLock);
			ptr = HashMapGet(globalValues, valueIdx & VALUE_GLOBAL_MASK);
			if (!ptr)
				LogCompilerError({}, "Bad job resume"_s);
		}
		SpinlockUnlock(&g_context->globalValuesLock);
		CTRegister *value = (CTRegister *)*ptr;
		ASSERT(value);
		return value;
	}
	else {
		CTRegister **value = HashMapGet(context->values, valueIdx);
		if (!value) {
			value = HashMapGetOrAdd(&context->values, valueIdx);
			u32 typeTableIdx = v.typeTableIdx;
			u64 size = GetTypeInfo(typeTableIdx).size;
			*value = (CTRegister *)LinearAllocator::Alloc(Max(8, size), 8);
		}
		ASSERT(*value);
		return *value;
	}
}

CTRegister CTGetIRValueContentRead(CTContext *ctContext, IRValue irValue);

CTRegister *CTRegisterFromIRValue(CTContext *ctContext, IRValue irValue, bool dereference)
{
	ASSERT(irValue.valueType == IRVALUETYPE_VALUE || irValue.valueType == IRVALUETYPE_MEMORY);
	CTRegister *reg = CTGetValueContent(ctContext, irValue.valueIdx);
	if (dereference)
		reg = reg->asPtr;
	u8 *ptr = (u8 *)reg;
	if (irValue.valueType == IRVALUETYPE_MEMORY) {
		ptr += irValue.mem.offset;
		if (irValue.mem.elementSize) {
			u32 indexTypeIdx = CTGetValue(ctContext, irValue.mem.indexValueIdx).typeTableIdx;
			IRValue indexIRValue = {
				.valueType = IRVALUETYPE_VALUE,
				.valueIdx = irValue.mem.indexValueIdx,
				.typeTableIdx = indexTypeIdx
			};
			CTRegister index = CTGetIRValueContentRead(ctContext, indexIRValue);
			ptr += index.asS64;
		}
	}
	reg = (CTRegister *)ptr;
	return reg;
}

CTRegister CTGetIRValueContentRead(CTContext *context, IRValue irValue)
{
	CTRegister result;
	switch (irValue.valueType) {
	case IRVALUETYPE_VALUE:
		result = *CTRegisterFromIRValue(context, irValue, false);
		break;
	case IRVALUETYPE_MEMORY:
		result = *CTRegisterFromIRValue(context, irValue, true);
		break;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	case IRVALUETYPE_IMMEDIATE_FLOAT:
		result.asU64 = irValue.immediate;
		break;
	default:
		LogError({}, "Invalid value type to read"_s);
	}

	// Clip
	TypeInfo typeInfo = GetTypeInfo(StripAllAliases(irValue.typeTableIdx));
	if (typeInfo.typeCategory == TYPECATEGORY_ENUM)
		typeInfo = GetTypeInfo(typeInfo.enumInfo.typeTableIdx);

	if (typeInfo.typeCategory == TYPECATEGORY_INTEGER) {
		if (typeInfo.integerInfo.isSigned) {
			// Sign extend
			switch (typeInfo.size) {
			case 1:
				result.asS64 = result.asS8;  break;
			case 2:
				result.asS64 = result.asS16; break;
			case 4:
				result.asS64 = result.asS32; break;
			}
		}
		else {
			// Zero extend
			switch (typeInfo.size) {
			case 1:
				result.asU64 = result.asU8;  break;
			case 2:
				result.asU64 = result.asU16; break;
			case 4:
				result.asU64 = result.asU32; break;
			}
		}
	}

	return result;
}

void CTStore(CTRegister *dst, const CTRegister *src, u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(StripAllAliases(typeTableIdx));
	memcpy(dst, src, typeInfo.size);
}

CTRegister *CTGetIRValueContentWrite(CTContext *context, IRValue irValue)
{
	CTRegister *result;
	switch (irValue.valueType) {
	case IRVALUETYPE_VALUE:
		result = CTRegisterFromIRValue(context, irValue, false);
		break;
	case IRVALUETYPE_MEMORY:
		result = CTRegisterFromIRValue(context, irValue, true);
		break;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	case IRVALUETYPE_IMMEDIATE_STRING:
		LogError({}, "Trying to write to immediate"_s);
	default:
		LogError({}, "Invalid value type to write to"_s);
	}
	return result;
}

void CTCopyIRValue(CTContext *context, CTRegister *dst, IRValue irValue)
{
	switch (irValue.valueType) {
	case IRVALUETYPE_VALUE:
	{
		CTStore(dst, CTRegisterFromIRValue(context, irValue, false),
				irValue.typeTableIdx);
	} break;
	case IRVALUETYPE_MEMORY:
	{
		CTStore(dst, CTRegisterFromIRValue(context, irValue, true),
				irValue.typeTableIdx);
	} break;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	case IRVALUETYPE_IMMEDIATE_STRING:
	{
		dst->asS64 = irValue.immediate;
	} break;
	default:
		LogError({}, "Invalid value type to write to"_s);
	}
}

void PrintIRInstruction(BucketArrayView<Value> localValues, IRInstruction inst);

void *GetExternalProcedureAddress(String name)
{
	const char *procCStr = StringToCStr(name, ThreadAllocator::Alloc);
	auto ctLibs = g_context->ctExternalLibraries.Get();
	while (true) {
		for (int i = 0; i < ctLibs->size; ++i) {
			CTLibrary *lib = &ctLibs[i];
			if (!lib->address) {
				lib->address = SYSLoadDynamicLibrary(lib->name);
				if (!lib->address)
					LogWarning(lib->loc, TPrintF("Could not load \"%S\" at "
								"compile time", lib->name));
			}
			void *procStart = SYSGetProcAddress(lib->address, procCStr);
			if (procStart)
				return procStart;
		}
		SwitchJob(YIELDREASON_NEED_DYNAMIC_LIBRARY, { .identifier = name });
		// Lock again!
		SYSMutexLock(g_context->ctExternalLibraries.lock);
	}
}

u64 CallProcedureDynamically(void *procStart, u32 typeTableIdx, ArrayView<CTRegister *> arguments)
{
	TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
	ASSERT(typeInfo.typeCategory == TYPECATEGORY_PROCEDURE);
	CallingConvention cc = typeInfo.procedureInfo.callingConvention;

	// Windows calling convention...
	if (cc == CC_WIN64) {
		// Right now we only support 16 arguments just because I'm lazy.
		ASSERT(arguments.size <= 16);

		u64 derefArguments[16];
		for (int i = 0; i < arguments.size; ++i)
			derefArguments[i] = *(u64 *)arguments[i];

		return SYSCallProcedureDynamically_windowscc(procStart, arguments.size, derefArguments);
	}

	// Linux calling convention...
	ArrayView<ProcedureParameter> parameters = typeInfo.procedureInfo.parameters;

	Array<u64, ThreadAllocator> gpArgs;
	Array<f64, ThreadAllocator> xmmArgs;
	Array<u64, ThreadAllocator> stackArgs;

	ArrayInit(&gpArgs, 6);
	ArrayInit(&xmmArgs, 16);
	ArrayInit(&stackArgs, 16);

	for (int i = 0; i < arguments.size; ++i) {
		void *arg = arguments[i];
		u32 paramTypeIdx = parameters[i].typeTableIdx;

		TypeInfo paramTypeInfo = GetTypeInfo(paramTypeIdx);
		bool isStruct = paramTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			(paramTypeInfo.typeCategory == TYPECATEGORY_ARRAY && paramTypeInfo.arrayInfo.count == 0);
		if (isStruct && paramTypeInfo.size <= 16) {
			ArrayView<StructMember> members;
			if (paramTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
				members = GetTypeInfo(TYPETABLEIDX_ARRAY_STRUCT).structInfo.members;
			else
				members = paramTypeInfo.structInfo.members;

			void *first = nullptr, *second = nullptr;
			bool isFirstXmm = false, isSecondXmm = false;
			int regCount = 1;

			first = arg;
			if (members[0].typeTableIdx == TYPETABLEIDX_F64 ||
			   (members.size == 2 &&
				members[0].typeTableIdx == TYPETABLEIDX_F32 &&
				members[1].typeTableIdx == TYPETABLEIDX_F32))
			{
				// An F64 or two consecutive F32s into an XMM register.
				isFirstXmm = true;
			}
			else if (members[0].typeTableIdx == TYPETABLEIDX_F32 &&
					(members.size == 1 || members[1].offset >= 8)) {
				// An only F32 into XMM register
				isFirstXmm = true;
			}
			else
				isFirstXmm = false;

			int firstMember = 0;
			for (; firstMember < members.size; ++firstMember) {
				if (members[firstMember].offset >= 8)
					break;
			}
			if (firstMember > 0 && firstMember < members.size) {
				second = arg;
				regCount = 2;

				if (members[firstMember].typeTableIdx == TYPETABLEIDX_F64 ||
				   (firstMember < members.size - 1 &&
					members[firstMember].typeTableIdx   == TYPETABLEIDX_F32 &&
					members[firstMember+1].typeTableIdx == TYPETABLEIDX_F32)) {
					// An F64 or two consecutive F32s into an XMM register.
					isSecondXmm = true;
				}
				else if (members[firstMember].typeTableIdx == TYPETABLEIDX_F32 &&
						(firstMember == members.size - 1 ||
						 members[firstMember+1].offset >= 8)) {
					// An only F32 into XMM register
					isSecondXmm = true;
				}
				else
					isSecondXmm = false;
			}

			u64 theoGPCount = gpArgs.size, theoXMMCount = xmmArgs.size;
			if (regCount >= 1) {
				bool fits = true;
				if (isFirstXmm && theoXMMCount++ >= 16)
					fits = false;
				else if (theoGPCount++ >= 6)
					fits = false;

				if (regCount >= 2) {
					if (isSecondXmm && theoXMMCount++ >= 16)
						fits = false;
					else if (theoGPCount++ >= 6)
						fits = false;
				}

				if (fits) {
					if (isFirstXmm)
						*ArrayAdd(&xmmArgs) = *(f64 *)first;
					else
						*ArrayAdd(&gpArgs) = *(u64 *)first;
					if (regCount >= 2) {
						if (isSecondXmm)
							*ArrayAdd(&xmmArgs) = *(f64 *)second;
						else
							*ArrayAdd(&gpArgs) = *(u64 *)second;
					}
					continue;
				}
			}
		}

		if (paramTypeInfo.size > 8) {
			u64 *scan = (u64 *)arg;
			int sizeLeft = (int)paramTypeInfo.size;
			while (sizeLeft > 0) {
				*ArrayAdd(&stackArgs) = *scan;
				++scan;
				sizeLeft -= 8;
			}
		}
		else {
			bool isXmm = paramTypeInfo.typeCategory == TYPECATEGORY_FLOATING;
			if (isXmm) {
				if (xmmArgs.size < 16)
					*ArrayAdd(&xmmArgs) = *(f64 *)arg;
				else
					*ArrayAdd(&stackArgs) = *(u64 *)arg;
			}
			else {
				if (gpArgs.size < 6)
					*ArrayAdd(&gpArgs) = *(u64 *)arg;
				else
					*ArrayAdd(&stackArgs) = *(u64 *)arg;
			}
		}
	}
	// Paranoid checks...
	ASSERT(gpArgs.size <= 6);
	ASSERT(xmmArgs.size <= 16);
	ASSERT(stackArgs.size <= 16);
	return SYSCallProcedureDynamically_linuxcc(procStart, gpArgs.size, gpArgs.data, xmmArgs.size,
			xmmArgs.data, stackArgs.size, stackArgs.data);
}

ArrayView<const CTRegister *> CTInternalRunInstructions(CTContext *context,
		BucketArrayView<IRInstruction> irInstructions)
{
#if CT_ENABLE_VERBOSE_LOGGING
	OutputBufferReset(g_context);
#endif

	Array<const CTRegister *, ThreadAllocator> returnValues;

	u64 instructionCount = irInstructions.count;
	for (u64 instIdx = 0; instIdx < instructionCount; ++instIdx) {
		IRInstruction inst = irInstructions[instIdx];
		context->currentLoc = inst.loc;

		if (inst.type == IRINSTRUCTIONTYPE_LABEL ||
			inst.type == IRINSTRUCTIONTYPE_PUSH_VALUE ||
			inst.type == IRINSTRUCTIONTYPE_PUSH_SCOPE ||
			inst.type == IRINSTRUCTIONTYPE_POP_SCOPE)
			continue;

#if CT_ENABLE_VERBOSE_LOGGING
		PrintIRInstruction(ctContext->localValues, inst);
		String instructionStr = {
			.size = g_context->outputBufferSize,
			.data = (const char *)g_context->outputBufferMem };
		CTVerboseLog(inst.loc, TPrintF("Running IR instruction: %S", instructionStr));
		OutputBufferReset(g_context);
#endif

		if (inst.type >= IRINSTRUCTIONTYPE_BinaryBegin && inst.type <= IRINSTRUCTIONTYPE_BinaryEnd) {
			IRValue lhs = inst.binaryOperation.left;
			IRValue rhs = inst.binaryOperation.right;
			IRValue out = inst.binaryOperation.out;

			CTRegister left  = CTGetIRValueContentRead(context, lhs);
			CTRegister right = CTGetIRValueContentRead(context, rhs);

			CTRegister result;

			u32 typeTableIdx = lhs.typeTableIdx;
			TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
			switch (typeInfo.typeCategory) {
			case TYPECATEGORY_POINTER:
				typeTableIdx = TYPETABLEIDX_U64; break;
			case TYPECATEGORY_ENUM:
				typeTableIdx = typeInfo.enumInfo.typeTableIdx; break;
			}

			switch (typeTableIdx) {
			case TYPETABLEIDX_S8:
			case TYPETABLEIDX_S16:
			case TYPETABLEIDX_S32:
			case TYPETABLEIDX_S64:
			{
				// Hopefully all values are properly zero/sign extended!
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_ADD:
					result.asS64 = left.asS64 + right.asS64;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT:
					result.asS64 = left.asS64 - right.asS64;
					break;
				case IRINSTRUCTIONTYPE_MULTIPLY:
					result.asS64 = left.asS64 * right.asS64;
					break;
				case IRINSTRUCTIONTYPE_DIVIDE:
					result.asS64 = left.asS64 / right.asS64;
					break;
				case IRINSTRUCTIONTYPE_MODULO:
					result.asS64 = left.asS64 % right.asS64;
					break;
				case IRINSTRUCTIONTYPE_SHIFT_LEFT:
					result.asS64 = left.asS64 << right.asS64;
					break;
				case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
					result.asS64 = left.asS64 >> right.asS64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_AND:
					result.asS64 = left.asS64 & right.asS64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_OR:
					result.asS64 = left.asS64 | right.asS64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_XOR:
					result.asS64 = left.asS64 ^ right.asS64;
					break;
				case IRINSTRUCTIONTYPE_EQUALS:
					result.asS64 = left.asS64 == right.asS64;
					break;
				case IRINSTRUCTIONTYPE_NOT_EQUALS:
					result.asS64 = left.asS64 == right.asS64;
					break;
				case IRINSTRUCTIONTYPE_GREATER_THAN:
					result.asS64 = left.asS64 > right.asS64;
					break;
				case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
					result.asS64 = left.asS64 >= right.asS64;
					break;
				case IRINSTRUCTIONTYPE_LESS_THAN:
					result.asS64 = left.asS64 < right.asS64;
					break;
				case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
					result.asS64 = left.asS64 <= right.asS64;
					break;
				default:
					LogError(inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("Lhs: 0x%llX, "
							"Rhs: 0x%llX, Out: 0x%llX\n", left.asS64, right.asS64, result.asS64));
			} break;
			case TYPETABLEIDX_U8:
			case TYPETABLEIDX_U16:
			case TYPETABLEIDX_U32:
			case TYPETABLEIDX_U64:
			case TYPETABLEIDX_BOOL:
			{
				// Hopefully all values are properly zero/sign extended!
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_ADD:
					result.asU64 = left.asU64 + right.asU64;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT:
					result.asU64 = left.asU64 - right.asU64;
					break;
				case IRINSTRUCTIONTYPE_MULTIPLY:
					result.asU64 = left.asU64 * right.asU64;
					break;
				case IRINSTRUCTIONTYPE_DIVIDE:
					result.asU64 = left.asU64 / right.asU64;
					break;
				case IRINSTRUCTIONTYPE_MODULO:
					result.asU64 = left.asU64 % right.asU64;
					break;
				case IRINSTRUCTIONTYPE_SHIFT_LEFT:
					result.asU64 = left.asU64 << right.asU64;
					break;
				case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
					result.asU64 = left.asU64 >> right.asU64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_AND:
					result.asU64 = left.asU64 & right.asU64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_OR:
					result.asU64 = left.asU64 | right.asU64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_XOR:
					result.asU64 = left.asU64 ^ right.asU64;
					break;
				case IRINSTRUCTIONTYPE_EQUALS:
					result.asU64 = left.asU64 == right.asU64;
					break;
				case IRINSTRUCTIONTYPE_NOT_EQUALS:
					result.asU64 = left.asU64 == right.asU64;
					break;
				case IRINSTRUCTIONTYPE_GREATER_THAN:
					result.asU64 = left.asU64 > right.asU64;
					break;
				case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
					result.asU64 = left.asU64 >= right.asU64;
					break;
				case IRINSTRUCTIONTYPE_LESS_THAN:
					result.asU64 = left.asU64 < right.asU64;
					break;
				case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
					result.asU64 = left.asU64 <= right.asU64;
					break;
				default:
					LogError(inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("Lhs: %llX, Rhs: %llX, Out: %llX\n",
							left.asU64, right.asU64, result.asU64));
			} break;
			case TYPETABLEIDX_F32:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_ADD:
					result.asF32 = left.asF32 + right.asF32;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT:
					result.asF32 = left.asF32 - right.asF32;
					break;
				case IRINSTRUCTIONTYPE_MULTIPLY:
					result.asF32 = left.asF32 * right.asF32;
					break;
				case IRINSTRUCTIONTYPE_DIVIDE:
					result.asF32 = left.asF32 / right.asF32;
					break;
				default:
					LogError(inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("Lhs: %f, Rhs: %f, Out: %f\n",
							left.asF32, right.asF32, result.asF32));
			} break;
			case TYPETABLEIDX_F64:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_ADD:
					result.asF64 = left.asF64 + right.asF64;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT:
					result.asF64 = left.asF64 - right.asF64;
					break;
				case IRINSTRUCTIONTYPE_MULTIPLY:
					result.asF64 = left.asF64 * right.asF64;
					break;
				case IRINSTRUCTIONTYPE_DIVIDE:
					result.asF64 = left.asF64 / right.asF64;
					break;
				default:
					LogError(inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("Lhs: %f, Rhs: %f, Out: %f\n",
							left.asF64, right.asF64, result.asF64));
			} break;
			default:
				LogError(inst.loc, "Invalid types on binary operation"_s);
			};

			CTRegister *outContent = CTGetIRValueContentWrite(context, out);
			CTStore(outContent, &result, out.typeTableIdx);
		}
		else if (inst.type >= IRINSTRUCTIONTYPE_UnaryBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_UnaryEnd &&
				 inst.type != IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS) {
			IRValue in  = inst.unaryOperation.in;
			IRValue out = inst.unaryOperation.out;

			CTRegister inValue  = CTGetIRValueContentRead(context, in);

			CTRegister result;

			u32 typeTableIdx = in.typeTableIdx;
			TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
			switch (typeInfo.typeCategory) {
			case TYPECATEGORY_POINTER:
				typeTableIdx = TYPETABLEIDX_U64; break;
			case TYPECATEGORY_ENUM:
				typeTableIdx = typeInfo.enumInfo.typeTableIdx; break;
			}

			switch (typeTableIdx) {
			case TYPETABLEIDX_S8:
			case TYPETABLEIDX_S16:
			case TYPETABLEIDX_S32:
			case TYPETABLEIDX_S64:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_NOT:
					result.asS64 = !inValue.asS64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_NOT:
					result.asS64 = ~inValue.asS64;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
					result.asS64 = -inValue.asS64;
					break;
				default:
					LogError(inst.loc, "Unary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("In: 0x%llX, Out: 0x%llX\n", in.asS64,
							result.asS64));
			} break;
			case TYPETABLEIDX_U8:
			case TYPETABLEIDX_U16:
			case TYPETABLEIDX_U32:
			case TYPETABLEIDX_U64:
			case TYPETABLEIDX_BOOL:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_NOT:
					result.asU64 = !inValue.asU64;
					break;
				case IRINSTRUCTIONTYPE_BITWISE_NOT:
					result.asU64 = ~inValue.asU64;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
					result.asS64 = -inValue.asS64;
					break;
				default:
					LogError(inst.loc, "Unary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("In: 0x%llX, Out: 0x%llX\n", in.asU64,
							result.asU64));
			} break;
			case TYPETABLEIDX_F32:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_NOT:
					result.asF32 = !inValue.asF32;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
					result.asF32 = -inValue.asF32;
					break;
				default:
					LogError(inst.loc, "Unary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("In: 0x%llf, Out: 0x%llf\n", (f64)in.asF32,
							(f64)result.asF32));
			} break;
			case TYPETABLEIDX_F64:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_NOT:
					result.asF64 = !inValue.asF64;
					break;
				case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
					result.asF64 = -inValue.asF64;
					break;
				default:
					LogError(inst.loc, "Unary operation not implemented"_s);
				}
				CTVerboseLog(inst.loc, TPrintF("In: 0x%llf, Out: 0x%llf\n", in.asF64,
							result.asF64));
			} break;
			default:
				LogError(inst.loc, "Invalid types on unary operation"_s);
			};

			CTRegister *outContent = CTGetIRValueContentWrite(context, out);
			CTStore(outContent, &result, out.typeTableIdx);
		}
		else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO ||
				 inst.type == IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO) {
			IRValue condition = inst.conditionalJump.condition;

			CTRegister conditionValue = CTGetIRValueContentRead(context, condition);

			bool doJump;

			u32 typeTableIdx = condition.typeTableIdx;
			TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
			switch (typeInfo.typeCategory) {
			case TYPECATEGORY_POINTER:
				typeTableIdx = TYPETABLEIDX_U64; break;
			case TYPECATEGORY_ENUM:
				typeTableIdx = typeInfo.enumInfo.typeTableIdx; break;
			}

			switch (inst.type) {
			case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
			{
				// Hopefully all values are properly zero/sign extended!
				switch (typeTableIdx) {
				case TYPETABLEIDX_S8:
				case TYPETABLEIDX_S16:
				case TYPETABLEIDX_S32:
				case TYPETABLEIDX_S64:
					doJump = conditionValue.asS64 == 0;
					break;
				case TYPETABLEIDX_U8:
				case TYPETABLEIDX_U16:
				case TYPETABLEIDX_U32:
				case TYPETABLEIDX_U64:
				case TYPETABLEIDX_BOOL:
					doJump = conditionValue.asU64 == 0;
					break;
				case TYPETABLEIDX_F32:
					doJump = conditionValue.asF32 == 0;
					break;
				case TYPETABLEIDX_F64:
					doJump = conditionValue.asF64 == 0;
					break;
				default:
					LogError(inst.loc, "Invalid types on conditional jump instruction"_s);
				}
			} break;
			case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
			{
				// Hopefully all values are properly zero/sign extended!
				switch (typeTableIdx) {
				case TYPETABLEIDX_S8:
				case TYPETABLEIDX_S16:
				case TYPETABLEIDX_S32:
				case TYPETABLEIDX_S64:
					doJump = conditionValue.asS64 != 0;
					break;
				case TYPETABLEIDX_U8:
				case TYPETABLEIDX_U16:
				case TYPETABLEIDX_U32:
				case TYPETABLEIDX_U64:
				case TYPETABLEIDX_BOOL:
					doJump = conditionValue.asU64 != 0;
					break;
				case TYPETABLEIDX_F32:
					doJump = conditionValue.asF32 != 0;
					break;
				case TYPETABLEIDX_F64:
					doJump = conditionValue.asF64 != 0;
					break;
				default:
					LogError(inst.loc, "Invalid types on conditional jump instruction"_s);
				}
			} break;
			default:
				LogError(inst.loc, "Jump not implemented"_s);
			};
			if (doJump)
				instIdx = inst.conditionalJump.label->instructionIdx - 1;
		}
		else if (inst.type >= IRINSTRUCTIONTYPE_CompareJumpBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_CompareJumpEnd) {
			IRValue lhs = inst.conditionalJump2.left;
			IRValue rhs = inst.conditionalJump2.right;

			CTRegister left  = CTGetIRValueContentRead(context, lhs);
			CTRegister right = CTGetIRValueContentRead(context, rhs);

			bool doJump;

			u32 typeTableIdx = lhs.typeTableIdx;
			TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
			switch (typeInfo.typeCategory) {
			case TYPECATEGORY_POINTER:
				typeTableIdx = TYPETABLEIDX_U64; break;
			case TYPECATEGORY_ENUM:
				typeTableIdx = typeInfo.enumInfo.typeTableIdx; break;
			}

			switch (typeTableIdx) {
			case TYPETABLEIDX_S8:
			case TYPETABLEIDX_S16:
			case TYPETABLEIDX_S32:
			case TYPETABLEIDX_S64:
			{
				// Hopefully all values are properly zero/sign extended!
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
					doJump = left.asS64 == right.asS64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
					doJump = left.asS64 != right.asS64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
					doJump = left.asS64 > right.asS64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
					doJump = left.asS64 >= right.asS64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
					doJump = left.asS64 < right.asS64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
					doJump = left.asS64 <= right.asS64;
					break;
				default:
					LogError(inst.loc, "Jump not implemented"_s);
				}
			} break;
			case TYPETABLEIDX_U8:
			case TYPETABLEIDX_U16:
			case TYPETABLEIDX_U32:
			case TYPETABLEIDX_U64:
			case TYPETABLEIDX_BOOL:
			{
				// Hopefully all values are properly zero/sign extended!
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
					doJump = left.asU64 == right.asU64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
					doJump = left.asU64 != right.asU64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
					doJump = left.asU64 > right.asU64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
					doJump = left.asU64 >= right.asU64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
					doJump = left.asU64 < right.asU64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
					doJump = left.asU64 <= right.asU64;
					break;
				default:
					LogError(inst.loc, "Jump not implemented"_s);
				}
			} break;
			case TYPETABLEIDX_F32:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
					doJump = left.asF32 == right.asF32;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
					doJump = left.asF32 != right.asF32;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
					doJump = left.asF32 > right.asF32;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
					doJump = left.asF32 >= right.asF32;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
					doJump = left.asF32 < right.asF32;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
					doJump = left.asF32 <= right.asF32;
					break;
				default:
					LogError(inst.loc, "Jump not implemented"_s);
				}
			} break;
			case TYPETABLEIDX_F64:
			{
				switch (inst.type) {
				case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
					doJump = left.asF64 == right.asF64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
					doJump = left.asF64 != right.asF64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
					doJump = left.asF64 > right.asF64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
					doJump = left.asF64 >= right.asF64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
					doJump = left.asF64 < right.asF64;
					break;
				case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
					doJump = left.asF64 <= right.asF64;
					break;
				default:
					LogError(inst.loc, "Jump not implemented"_s);
				}
			} break;
			default:
				LogError(inst.loc, "Invalid types on conditional jump instruction"_s);
			};

			CTVerboseLog(inst.loc, TPrintF("Lhs: %llX, Rhs: %llX, Jump taken?: %s\n",
						left.asS64, right.asS64, doJump ? "true" : "false"));

			if (doJump)
				instIdx = inst.conditionalJump2.label->instructionIdx - 1;
		}
		else switch (inst.type) {
		case IRINSTRUCTIONTYPE_ASSIGNMENT:
		case IRINSTRUCTIONTYPE_TRUNCATE:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(context, dst);
			CTRegister  srcContent = CTGetIRValueContentRead(context, src);

			CTVerboseLog(inst.loc, TPrintF("Assigned: 0x%llX, to 0x%llX",
						srcContent.asU64, dstContent));

			CTStore(dstContent, &srcContent, dst.typeTableIdx);
		} break;
		case IRINSTRUCTIONTYPE_CONVERT_INT_TO_FLOAT:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(context, dst);
			CTRegister  srcContent = CTGetIRValueContentRead(context, src);

			CTVerboseLog(inst.loc, TPrintF("Converted int to float: 0x%llX, to 0x%llX",
						srcContent.asU64, dstContent));

			u32 dstTypeIdx = StripAllAliases(dst.typeTableIdx);
			u32 srcTypeIdx = StripAllAliases(src.typeTableIdx);
			TypeInfo srcTypeInfo = GetTypeInfo(srcTypeIdx);
			if (srcTypeInfo.typeCategory != TYPECATEGORY_INTEGER)
				LogCompilerError(inst.loc, "CONVERT_INT_TO_FLOAT does not have an "
						"int as source"_s);

			if (dstTypeIdx == TYPETABLEIDX_F32) {
				if (srcTypeInfo.integerInfo.isSigned)
					dstContent->asF32 = (f32)srcContent.asS64;
				else
					dstContent->asF32 = (f32)srcContent.asU64;
			}
			else if (dstTypeIdx == TYPETABLEIDX_F64) {
				if (srcTypeInfo.integerInfo.isSigned)
					dstContent->asF64 = (f64)srcContent.asS64;
				else
					dstContent->asF64 = (f64)srcContent.asU64;
			}
			else
				LogCompilerError(inst.loc, "CONVERT_INT_TO_FLOAT does not have a "
						"floating point value as destination"_s);
		} break;
		case IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(context, dst);
			CTRegister  srcContent = CTGetIRValueContentRead(context, src);

			CTVerboseLog(inst.loc, TPrintF("Converted int to float: 0x%llX, to 0x%llX",
						srcContent.asU64, dstContent));

			u32 dstTypeIdx = StripAllAliases(dst.typeTableIdx);
			u32 srcTypeIdx = StripAllAliases(src.typeTableIdx);
			TypeInfo dstTypeInfo = GetTypeInfo(dstTypeIdx);
			if (dstTypeInfo.typeCategory != TYPECATEGORY_INTEGER)
				LogCompilerError(inst.loc, "CONVERT_INT_TO_FLOAT does not have an "
						"integer as destination"_s);

			if (srcTypeIdx == TYPETABLEIDX_F32) {
				if (dstTypeInfo.integerInfo.isSigned)
					dstContent->asS64 = (s64)srcContent.asF32;
				else
					dstContent->asU64 = (u64)srcContent.asF32;
			}
			else if (srcTypeIdx == TYPETABLEIDX_F64) {
				if (dstTypeInfo.integerInfo.isSigned)
					dstContent->asS64 = (s64)srcContent.asF64;
				else
					dstContent->asU64 = (u64)srcContent.asF64;
			}
			else
				LogCompilerError(inst.loc, "CONVERT_FLOAT_TO_INT does not have a "
						"floating point value as source"_s);
		} break;
		case IRINSTRUCTIONTYPE_CONVERT_PRECISION:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(context, dst);
			CTRegister  srcContent = CTGetIRValueContentRead(context, src);

			CTVerboseLog(inst.loc, TPrintF("Converted precision: 0x%llX, to 0x%llX",
						srcContent.asU64, dstContent));

			u32 dstTypeIdx = StripAllAliases(dst.typeTableIdx);
			u32 srcTypeIdx = StripAllAliases(src.typeTableIdx);

			if (dstTypeIdx == TYPETABLEIDX_F32) {
				if (srcTypeIdx == TYPETABLEIDX_F32)
					LogCompilerError(inst.loc, "CONVERT_PRECISION instruction has both "
							"sides of the same precision"_s);
				ASSERT(srcTypeIdx == TYPETABLEIDX_F64);
				dstContent->asF32 = (f32)srcContent.asF64;
			}
			else if (dstTypeIdx == TYPETABLEIDX_F64) {
				if (srcTypeIdx == TYPETABLEIDX_F64)
					LogCompilerError(inst.loc, "CONVERT_PRECISION instruction has both "
							"sides of the same precision"_s);
				ASSERT(srcTypeIdx == TYPETABLEIDX_F32);
				dstContent->asF64 = (f64)srcContent.asF32;
			}
			else
				LogCompilerError(inst.loc, "Invalid types on CONVERT_PRECISION "
						"instruction"_s);
		} break;
		case IRINSTRUCTIONTYPE_SIGN_EXTEND:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(context, dst);
			CTRegister  srcContent = CTGetIRValueContentRead(context, src);

			CTVerboseLog(inst.loc, TPrintF("Sign extending: 0x%llX, to 0x%llX",
						srcContent.asU64, dstContent));

			TypeInfo srcTypeInfo = GetTypeInfo(src.typeTableIdx);

			CTRegister result;
			switch (srcTypeInfo.size) {
			case 1:
				result.asS64 = srcContent.asS8;
				break;
			case 2:
				result.asS64 = srcContent.asS16;
				break;
			case 4:
				result.asS64 = srcContent.asS32;
				break;
			default:
				LogCompilerError(inst.loc, "SIGN_EXTEND source has "
						"invalid size"_s);
			}
			CTStore(dstContent, &result, dst.typeTableIdx);
		} break;
		case IRINSTRUCTIONTYPE_ZERO_EXTEND:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(context, dst);
			CTRegister  srcContent = CTGetIRValueContentRead(context, src);

			CTVerboseLog(inst.loc, TPrintF("Zero extending: 0x%llX, to 0x%llX",
						srcContent.asU64, dstContent));

			TypeInfo srcTypeInfo = GetTypeInfo(src.typeTableIdx);

			CTRegister result;
			switch (srcTypeInfo.size) {
			case 1:
				result.asU64 = srcContent.asU8;
				break;
			case 2:
				result.asU64 = srcContent.asU16;
				break;
			case 4:
				result.asU64 = srcContent.asU32;
				break;
			default:
				LogCompilerError(inst.loc, "ZERO_EXTEND source has "
						"invalid size"_s);
			}
			CTStore(dstContent, &result, dst.typeTableIdx);
		} break;
		case IRINSTRUCTIONTYPE_COPY_MEMORY:
		{
			IRValue dst  = inst.copyMemory.dst;
			IRValue src  = inst.copyMemory.src;
			IRValue size = inst.copyMemory.size;

			CTRegister *dstContent = nullptr;
			switch (dst.valueType) {
			case IRVALUETYPE_VALUE:
			case IRVALUETYPE_MEMORY:
				dstContent = CTRegisterFromIRValue(context, dst, true);
				break;
			default:
				LogError(inst.loc, "Invalid value type to copy to"_s);
			}

			CTRegister *srcContent = nullptr;
			switch (src.valueType) {
			case IRVALUETYPE_VALUE:
			case IRVALUETYPE_MEMORY:
				srcContent = CTRegisterFromIRValue(context, src, true);
				break;
			default:
				LogError(inst.loc, "Invalid value type to copy from"_s);
			}

			CTRegister sizeContent = CTGetIRValueContentRead(context, size);

			memcpy(dstContent, srcContent, sizeContent.asU64);
		} break;
		case IRINSTRUCTIONTYPE_ZERO_MEMORY:
		{
			IRValue dst  = inst.zeroMemory.dst;
			IRValue size = inst.zeroMemory.size;

			CTRegister *dstContent = nullptr;
			switch (dst.valueType) {
			case IRVALUETYPE_VALUE:
			case IRVALUETYPE_MEMORY:
				dstContent = CTRegisterFromIRValue(context, dst, true);
				break;
			default:
				LogError(inst.loc, "Invalid value type to copy to"_s);
			}

			CTRegister sizeContent = CTGetIRValueContentRead(context, size);

			memset(dstContent, 0, sizeContent.asU64);
		} break;
		case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(context, dst);

			CTRegister *srcContent = nullptr;
			switch (src.valueType) {
			case IRVALUETYPE_VALUE:
				srcContent = CTRegisterFromIRValue(context, src, false);
				break;
			case IRVALUETYPE_MEMORY:
				srcContent = CTRegisterFromIRValue(context, src, true);
				break;
			case IRVALUETYPE_IMMEDIATE_STRING:
			case IRVALUETYPE_IMMEDIATE_INTEGER:
			case IRVALUETYPE_IMMEDIATE_FLOAT:
				LogError(inst.loc, "Trying to get pointer to immediate"_s);
			case IRVALUETYPE_PROCEDURE:
				LogError(inst.loc, "Procedure pointers not implemented on compile time"_s);
			default:
				LogError(inst.loc, "Invalid value type to get pointer from"_s);
			}

			CTVerboseLog(inst.loc, TPrintF("Pointer is 0x%llX", srcContent));

			dstContent->asPtr = srcContent;
		} break;
		case IRINSTRUCTIONTYPE_RETURN:
		{
			u64 returnCount = inst.returnInst.returnValueIndices.size;
			ArrayInit(&returnValues, returnCount);
			for (int i = 0; i < returnCount; ++i) {
				u32 valueIdx = inst.returnInst.returnValueIndices[i];
				CTRegister *value = CTGetValueContent(context, valueIdx);
				// @Improve: copy maybe
				*ArrayAdd(&returnValues) = value;
			}
		} break;
		case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
		{
			Array<CTRegister *, ThreadAllocator> arguments;
			u64 paramCount = inst.procedureCall.parameters.size;
			ArrayInit(&arguments, paramCount);

			for (int paramIdx = 0; paramIdx < paramCount; ++paramIdx) {
				IRValue irValue = inst.procedureCall.parameters[paramIdx];
				TypeInfo typeInfo = GetTypeInfo(irValue.typeTableIdx);
				CTRegister *copy = (CTRegister *)ThreadAllocator::Alloc(typeInfo.size, 8);
				CTCopyIRValue(context, copy, irValue);
				*ArrayAdd(&arguments) = copy;
			}

			u32 procIdx = inst.procedureCall.procedureIdx;
			if (!(procIdx & PROCEDURE_EXTERNAL_BIT))
				CTRunProcedure(procIdx, arguments);
			else {
				Procedure calleeProc = GetProcedureRead(procIdx);
				void *procStart = GetExternalProcedureAddress(calleeProc.name);
				ASSERT(procStart);

				u64 returnValue = CallProcedureDynamically(procStart, calleeProc.typeTableIdx, arguments);

				if (inst.procedureCall.returnValues.size) {
					ASSERT(inst.procedureCall.returnValues.size == 1);
					CTRegister *returnReg = CTGetIRValueContentWrite(context,
							inst.procedureCall.returnValues[0]);
					returnReg->asU64 = returnValue;
				}
			}
		} break;
		case IRINSTRUCTIONTYPE_JUMP:
		{
			instIdx = inst.label->instructionIdx - 1;
		} break;
		case IRINSTRUCTIONTYPE_COMMENT:
		{
		} break;
		case IRINSTRUCTIONTYPE_INTRINSIC:
		{
			switch (inst.intrinsic.type) {
			case INTRINSIC_BREAKPOINT:
				BREAK;
				break; // lol
			case INTRINSIC_SQRT32:
			case INTRINSIC_SQRT64:
			{
				IRValue lhs = inst.intrinsic.parameters[0];
				IRValue rhs = inst.intrinsic.parameters[1];

				CTRegister right = CTGetIRValueContentRead(context, rhs);

				CTRegister result;

				if (inst.intrinsic.type == INTRINSIC_SQRT32) {
					ASSERT(lhs.typeTableIdx == TYPETABLEIDX_F32);
					ASSERT(rhs.typeTableIdx == TYPETABLEIDX_F32);
					result.asF32 = Sqrt(right.asF32);
				}
				else if (inst.intrinsic.type == INTRINSIC_SQRT64) {
					ASSERT(lhs.typeTableIdx == TYPETABLEIDX_F64);
					ASSERT(rhs.typeTableIdx == TYPETABLEIDX_F64);
					result.asF64 = Sqrt64(right.asF64);
				}
				else ASSERT(false);

				CTRegister *outContent = CTGetIRValueContentWrite(context, lhs);
				CTStore(outContent, &result, lhs.typeTableIdx);
			}
			default:
				LogCompilerError(inst.loc, "Intrinsic not implemented"_s);
			}
		} break;
		default:
			LogCompilerError(inst.loc, "Instruction not implemented"_s);
		}
	}
	return returnValues;
}

CTRegister CTRunInstructions(BucketArrayView<Value> localValues,
		BucketArrayView<IRInstruction> irInstructions, IRValue resultIRValue)
{
	CTContext *context = ALLOC(LinearAllocator, CTContext);
	context->localValues = localValues;
	HashMapInit(&context->values, 32);
	CTInternalRunInstructions(context, irInstructions);

	if (resultIRValue.valueType != IRVALUETYPE_INVALID)
		return CTGetIRValueContentRead(context, resultIRValue);
	else
		return {};
}

ArrayView<const CTRegister *> CTRunProcedure(u32 procedureIdx, ArrayView<CTRegister *> parameters)
{
	ASSERT(!(procedureIdx & PROCEDURE_EXTERNAL_BIT));
	Procedure proc = GetProcedureRead(procedureIdx);
	if (!proc.isIRReady) {
		auto procedures = g_context->procedures.GetForRead();
		proc = procedures[procedureIdx];
		if (!proc.isIRReady) {
			SwitchJob(YIELDREASON_PROC_IR_NOT_READY, { .index = procedureIdx });
			// Lock again!
			SYSLockForRead(&g_context->procedures.rwLock);
			proc = procedures[procedureIdx];
			if (!proc.isIRReady)
				LogCompilerError({}, "Bad job resume"_s);
		}
	}

	CTContext *context = ALLOC(LinearAllocator, CTContext);
	context->procedureIdx = procedureIdx;
	context->localValues = proc.localValues;
	HashMapInit(&context->values, Max(32, NextPowerOf2((u32)proc.localValues.count)));

	// Read parameters
	ASSERT(parameters.size == proc.parameterValues.size);
	for (int i = 0; i < parameters.size; ++i) {
		CTRegister *varContent = CTGetValueContent(context, proc.parameterValues[i]);
		u32 paramTypeIdx = CTGetValue(context, proc.parameterValues[i]).typeTableIdx;
		CTStore(varContent, parameters[i], paramTypeIdx);
	}

	ArrayView<const CTRegister *> result = CTInternalRunInstructions(context, proc.irInstructions);
	ASSERT(result.size == proc.returnValueIndices.size);
	return result;
}
