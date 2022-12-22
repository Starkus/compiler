#define CT_ENABLE_VERBOSE_LOGGING 0

#if CT_ENABLE_VERBOSE_LOGGING
#define CTVerboseLog(_context, _loc, _string) LogNote(_context, _loc, _string)
#else
#define CTVerboseLog(...)
#endif

struct CTContext
{
	Context *globalContext;
	u32 procedureIdx;
	SourceLocation currentLoc;
	HashMap<u32, CTRegister *, ThreadAllocator> values;
};

u32 CTGetValueTypeIdx(CTContext *ctContext, u32 valueIdx)
{
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = ctContext->globalContext->globalValues.GetForRead();
		return globalValues[valueIdx].typeTableIdx;
	}
	else {
		Procedure *proc = &ctContext->globalContext->procedures.unsafe[ctContext->procedureIdx];
		return proc->localValues[valueIdx].typeTableIdx;
	}
}

CTRegister *CTGetValueContent(CTContext *ctContext, u32 valueIdx)
{
	if (valueIdx & VALUE_GLOBAL_BIT) {
		ScopedLockSpin lock(&ctContext->globalContext->ctGlobalValuesLock);
		auto globalValues = ctContext->globalContext->ctGlobalValueContents;
		CTRegister *value = *HashMapGet(globalValues, valueIdx & VALUE_GLOBAL_MASK);
		ASSERT(value);
		return value;
	}
	else {
		CTRegister **value = HashMapGet(ctContext->values, valueIdx);
		if (!value) {
			value = HashMapGetOrAdd(&ctContext->values, valueIdx);
			u32 typeTableIdx = CTGetValueTypeIdx(ctContext, valueIdx);
			u64 size = GetTypeInfo(ctContext->globalContext, typeTableIdx).size;
			*value = (CTRegister *)LinearAllocator::Alloc(Max(8, size), 8);
		}
		ASSERT(*value);
		return *value;
	}
}

CTRegister CTGetIRValueContentRead(CTContext *ctContext, IRValue irValue);

CTRegister *CTRegisterFromIRValue(CTContext *ctContext, IRValue irValue, bool dereference)
{
	CTRegister *reg = CTGetValueContent(ctContext, irValue.value.valueIdx);
	if (dereference)
		reg = reg->asPtr;
	u8 *ptr = ((u8 *)reg + irValue.value.offset);
	if (irValue.value.elementSize) {
		u32 indexTypeIdx = CTGetValueTypeIdx(ctContext, irValue.value.indexValueIdx);
		IRValue indexIRValue = {
			.valueType = IRVALUETYPE_VALUE,
			.value = { .valueIdx = irValue.value.indexValueIdx },
			.typeTableIdx = indexTypeIdx
		};
		CTRegister index = CTGetIRValueContentRead(ctContext, indexIRValue);
		ptr += index.asS64;
	}
	reg = (CTRegister *)ptr;
	return reg;
}

CTRegister CTGetIRValueContentRead(CTContext *ctContext, IRValue irValue)
{
	CTRegister result;
	switch (irValue.valueType) {
	case IRVALUETYPE_VALUE:
		result = *CTRegisterFromIRValue(ctContext, irValue, false);
		break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
		result = *CTRegisterFromIRValue(ctContext, irValue, true);
		break;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	case IRVALUETYPE_IMMEDIATE_FLOAT:
		result.asU64 = irValue.immediate;
		break;
	default:
		LogError(ctContext->globalContext, {}, "Invalid value type to read"_s);
	}

	// Clip
	TypeInfo typeInfo = GetTypeInfo(ctContext->globalContext,
			StripAllAliases(ctContext->globalContext, irValue.typeTableIdx));
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

void CTStore(CTContext *ctContext, CTRegister *dst, const CTRegister *src, u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(ctContext->globalContext,
			StripAllAliases(ctContext->globalContext, typeTableIdx));
	memcpy(dst, src, typeInfo.size);
}

CTRegister *CTGetIRValueContentWrite(CTContext *ctContext, IRValue irValue)
{
	CTRegister *result;
	switch (irValue.valueType) {
	case IRVALUETYPE_VALUE:
		result = CTRegisterFromIRValue(ctContext, irValue, false);
		break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
		result = CTRegisterFromIRValue(ctContext, irValue, true);
		break;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	case IRVALUETYPE_IMMEDIATE_STRING:
		LogError(ctContext->globalContext, {}, "Trying to write to immediate"_s);
	default:
		LogError(ctContext->globalContext, {}, "Invalid value type to write to"_s);
	}
	return result;
}

void CTCopyIRValue(CTContext *ctContext, CTRegister *dst, IRValue irValue)
{
	switch (irValue.valueType) {
	case IRVALUETYPE_VALUE:
	{
		CTStore(ctContext, dst, CTRegisterFromIRValue(ctContext, irValue, false),
				irValue.typeTableIdx);
	} break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
	{
		CTRegister *src = CTRegisterFromIRValue(ctContext, irValue, true);

		u32 typeTableIdx = irValue.typeTableIdx;
		TypeInfo pointerTypeInfo = GetTypeInfo(ctContext->globalContext, typeTableIdx);
		if (pointerTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			typeTableIdx = pointerTypeInfo.pointerInfo.pointedTypeTableIdx;

		CTStore(ctContext, dst, src, typeTableIdx);
	} break;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	case IRVALUETYPE_IMMEDIATE_STRING:
	{
		dst->asS64 = irValue.immediate;
	} break;
	default:
		LogError(ctContext->globalContext, {}, "Invalid value type to write to"_s);
	}
}

void PrintIRInstruction(Context *context, u32 procedureIdx, IRInstruction inst);

ArrayView<const CTRegister *> CTRunProcedure(Context *context, u32 procedureIdx,
		ArrayView<CTRegister *> parameters)
{
	ASSERT(!(procedureIdx & PROCEDURE_EXTERNAL_BIT));

	CTContext ctContext = {
		.globalContext = context,
		.procedureIdx = procedureIdx,
	};
	Procedure proc = GetProcedureRead(context, procedureIdx);

	while (!proc.isIRReady) {
		if (!TCIsAnyOtherJobRunningOrWaiting(context))
			LogError(context, {}, TPrintF("COMPILER ERROR! IR of procedure "
					"\"%S\" never generated", proc.name));
		SwitchJob(context, TCYIELDREASON_PROC_IR_NOT_READY, { .index = procedureIdx });
		proc = GetProcedureRead(context, procedureIdx);
	}

	HashMapInit(&ctContext.values, Max(32, NextPowerOf2((u32)BucketArrayCount(&proc.localValues))));

	Array<const CTRegister *, ThreadAllocator> returnValues;
	ArrayInit(&returnValues, proc.returnValueIndices.size);

#if CT_ENABLE_VERBOSE_LOGGING
	BucketArrayInit(&context->outputBuffer);
#endif

	// Read parameters
	ASSERT(parameters.size == proc.parameterValues.size);
	for (int i = 0; i < parameters.size; ++i) {
		CTRegister *varContent = CTGetValueContent(&ctContext, proc.parameterValues[i]);
		u32 paramTypeIdx = CTGetValueTypeIdx(&ctContext, proc.parameterValues[i]);
		CTStore(&ctContext, varContent, parameters[i], paramTypeIdx);
	}

	u64 instructionCount = BucketArrayCount(&proc.irInstructions);
	for (u64 instIdx = 0; instIdx < instructionCount; ++instIdx) {
		IRInstruction inst = proc.irInstructions[instIdx];
		ctContext.currentLoc = inst.loc;

		if (inst.type == IRINSTRUCTIONTYPE_LABEL ||
			inst.type == IRINSTRUCTIONTYPE_PUSH_VALUE ||
			inst.type == IRINSTRUCTIONTYPE_PUSH_SCOPE ||
			inst.type == IRINSTRUCTIONTYPE_POP_SCOPE)
			continue;

#if CT_ENABLE_VERBOSE_LOGGING
		PrintIRInstruction(context, procedureIdx, inst);
		String instructionStr = {
			.size = context->outputBuffer.buckets[0].size,
			.data = (const char *)context->outputBuffer.buckets[0].data };
		CTVerboseLog(context, inst.loc, TPrintF("Running IR instruction: %S", instructionStr));
		context->outputBuffer.buckets[0].size = 0;
#endif

		if (inst.type >= IRINSTRUCTIONTYPE_BinaryBegin && inst.type <= IRINSTRUCTIONTYPE_BinaryEnd) {
			IRValue lhs = inst.binaryOperation.left;
			IRValue rhs = inst.binaryOperation.right;
			IRValue out = inst.binaryOperation.out;

			CTRegister left  = CTGetIRValueContentRead(&ctContext, lhs);
			CTRegister right = CTGetIRValueContentRead(&ctContext, rhs);

			CTRegister result;

			u32 typeTableIdx = lhs.typeTableIdx;
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
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
				default:
					LogError(context, inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(context, inst.loc, TPrintF("Lhs: %lld, Rhs: %lld, Out: %lld\n",
							left.asS64, right.asS64, result.asS64));
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
				default:
					LogError(context, inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(context, inst.loc, TPrintF("Lhs: %llu, Rhs: %llu, Out: %llu\n",
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
					LogError(context, inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(context, inst.loc, TPrintF("Lhs: %f, Rhs: %f, Out: %f\n",
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
					LogError(context, inst.loc, "Binary operation not implemented"_s);
				}
				CTVerboseLog(context, inst.loc, TPrintF("Lhs: %f, Rhs: %f, Out: %f\n",
							left.asF64, right.asF64, result.asF64));
			} break;
			default:
				LogError(context, inst.loc, "Invalid types on binary operation"_s);
			};

			CTRegister *outContent = CTGetIRValueContentWrite(&ctContext, out);
			CTStore(&ctContext, outContent, &result, out.typeTableIdx);
		}
		else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO ||
				 inst.type == IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO) {
			IRValue condition = inst.conditionalJump.condition;

			CTRegister conditionValue = CTGetIRValueContentRead(&ctContext, condition);

			bool doJump;

			u32 typeTableIdx = condition.typeTableIdx;
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
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
					doJump = conditionValue.asF32 == 0;
					break;
				default:
					LogError(context, inst.loc, "Invalid types on conditional jump instruction"_s);
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
					doJump = conditionValue.asF32 != 0;
					break;
				default:
					LogError(context, inst.loc, "Invalid types on conditional jump instruction"_s);
				}
			} break;
			default:
				LogError(context, inst.loc, "Jump not implemented"_s);
			};
			if (doJump)
				instIdx = inst.conditionalJump.label->instructionIdx - 1;
		}
		else if (inst.type >= IRINSTRUCTIONTYPE_CompareJumpBegin &&
				 inst.type <= IRINSTRUCTIONTYPE_CompareJumpEnd) {
			IRValue lhs = inst.conditionalJump2.left;
			IRValue rhs = inst.conditionalJump2.right;

			CTRegister left  = CTGetIRValueContentRead(&ctContext, lhs);
			CTRegister right = CTGetIRValueContentRead(&ctContext, rhs);

			bool doJump;

			u32 typeTableIdx = lhs.typeTableIdx;
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
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
					LogError(context, inst.loc, "Jump not implemented"_s);
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
					LogError(context, inst.loc, "Jump not implemented"_s);
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
					LogError(context, inst.loc, "Jump not implemented"_s);
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
					LogError(context, inst.loc, "Jump not implemented"_s);
				}
			} break;
			default:
				LogError(context, inst.loc, "Invalid types on conditional jump instruction"_s);
			};

			CTVerboseLog(context, inst.loc, TPrintF("Lhs: %lld, Rhs: %lld, Jump taken?: %s\n",
						left.asS64, right.asS64, doJump ? "true" : "false"));

			if (doJump)
				instIdx = inst.conditionalJump2.label->instructionIdx - 1;
		}
		else switch (inst.type) {
		case IRINSTRUCTIONTYPE_ASSIGNMENT:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(&ctContext, dst);
			CTRegister  srcContent = CTGetIRValueContentRead(&ctContext, src);

			CTVerboseLog(context, inst.loc, TPrintF("Assigned: 0x%llX, to 0x%llX",
						srcContent.asU64, dstContent));

			CTStore(&ctContext, dstContent, &srcContent, dst.typeTableIdx);
		} break;
		case IRINSTRUCTIONTYPE_COPY_MEMORY:
		{
			IRValue dst  = inst.copyMemory.dst;
			IRValue src  = inst.copyMemory.src;
			IRValue size = inst.copyMemory.size;

			CTRegister *dstContent = nullptr;
			switch (dst.valueType) {
			case IRVALUETYPE_VALUE:
			case IRVALUETYPE_VALUE_DEREFERENCE:
				dstContent = CTRegisterFromIRValue(&ctContext, dst, true);
				break;
			default:
				LogError(context, inst.loc, "Invalid value type to copy to"_s);
			}

			CTRegister *srcContent = nullptr;
			switch (src.valueType) {
			case IRVALUETYPE_VALUE:
			case IRVALUETYPE_VALUE_DEREFERENCE:
				srcContent = CTRegisterFromIRValue(&ctContext, src, true);
				break;
			default:
				LogError(context, inst.loc, "Invalid value type to copy from"_s);
			}

			CTRegister sizeContent = CTGetIRValueContentRead(&ctContext, size);

			memcpy(dstContent, srcContent, sizeContent.asU64);
		} break;
		case IRINSTRUCTIONTYPE_ZERO_MEMORY:
		{
			IRValue dst  = inst.zeroMemory.dst;
			IRValue size = inst.zeroMemory.size;

			CTRegister *dstContent = nullptr;
			switch (dst.valueType) {
			case IRVALUETYPE_VALUE:
			case IRVALUETYPE_VALUE_DEREFERENCE:
				dstContent = CTRegisterFromIRValue(&ctContext, dst, true);
				break;
			default:
				LogError(context, inst.loc, "Invalid value type to copy to"_s);
			}

			CTRegister sizeContent = CTGetIRValueContentRead(&ctContext, size);

			memset(dstContent, 0, sizeContent.asU64);
		} break;
		case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
		{
			IRValue dst = inst.assignment.dst;
			IRValue src = inst.assignment.src;

			CTRegister *dstContent = CTGetIRValueContentWrite(&ctContext, dst);

			CTRegister *srcContent = nullptr;
			switch (src.valueType) {
			case IRVALUETYPE_VALUE:
				srcContent = CTRegisterFromIRValue(&ctContext, src, false);
				break;
			case IRVALUETYPE_VALUE_DEREFERENCE:
				srcContent = CTRegisterFromIRValue(&ctContext, src, true);
				break;
			case IRVALUETYPE_IMMEDIATE_STRING:
			{
				String literal;
				{
					auto stringLiterals = context->stringLiterals.GetForRead();
					literal = stringLiterals[src.immediateStringIdx];
				}
				// @Todo: copy the strings or add some protection so string literals can't be
				// changed by the code...
				srcContent = (CTRegister *)literal.data;
			} break;
			case IRVALUETYPE_IMMEDIATE_INTEGER:
			case IRVALUETYPE_IMMEDIATE_FLOAT:
			{
				LogError(ctContext.globalContext, inst.loc, "Trying to get pointer to immediate"_s);
			} break;
			default:
				LogError(ctContext.globalContext, inst.loc, "Invalid value type to get pointer from"_s);
			}

			CTVerboseLog(context, inst.loc, TPrintF("Pointer is 0x%llX", srcContent));

			dstContent->asPtr = srcContent;
		} break;
		case IRINSTRUCTIONTYPE_RETURN:
		{
			u64 returnCount = proc.returnValueIndices.size;
			for (int i = 0; i < returnCount; ++i) {
				u32 valueIdx = inst.returnInst.returnValueIndices[i];
				CTRegister *value = CTGetValueContent(&ctContext, valueIdx);
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
				TypeInfo typeInfo = GetTypeInfo(context, irValue.typeTableIdx);
				CTRegister *copy = (CTRegister *)ThreadAllocator::Alloc(typeInfo.size, 8);
				CTCopyIRValue(&ctContext, copy, irValue);
				*ArrayAdd(&arguments) = copy;
			}

			u32 procIdx = inst.procedureCall.procedureIdx;
			if (!(procIdx & PROCEDURE_EXTERNAL_BIT))
				CTRunProcedure(context, procIdx, arguments);
			else {
				Procedure calleeProc = GetProcedureRead(context, procIdx);
				// @Todo: iterate default dlls and #linklib-s
				HMODULE dll = LoadLibraryA("Kernel32.dll");
				void *procStart = GetProcAddress(dll, StringToCStr(calleeProc.name,
							ThreadAllocator::Alloc));
				ASSERT(procStart);
				u64 returnValue = SYSCallProcedureDynamically(procStart, arguments.size, arguments.data);

				if (inst.procedureCall.returnValues.size) {
					ASSERT(inst.procedureCall.returnValues.size == 1);
					CTRegister *returnReg = CTGetIRValueContentWrite(&ctContext,
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
		default:
			LogError(context, inst.loc, "Instruction not implemented"_s);
		}
	}
	return returnValues;
}

void CompileTimeMain(Context *context)
{
	HashMapInit(&context->ctGlobalValueContents, 64);
	context->ctGlobalValuesLock = 0;
}
