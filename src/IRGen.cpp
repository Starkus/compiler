u32 IRNewValue(IRContext *irContext, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	u64 idx = irContext->localValues->count;
	Value *result = BucketArrayAdd(irContext->localValues);
#if DEBUG_BUILD
	result->name = {};
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;
	result->allocatedRegister = U32_MAX;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 IRNewValue(IRContext *irContext, String name, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	u64 idx = irContext->localValues->count;
	Value *result = BucketArrayAdd(irContext->localValues);
#if DEBUG_BUILD
	result->name = name;
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;
	result->allocatedRegister = U32_MAX;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 IRNewValue(IRContext *irContext, Value value)
{
	ASSERT(value.typeTableIdx != 0);
	ASSERT(!(value.flags & VALUEFLAGS_TRY_IMMITATE) || value.tryImmitateValueIdx != U32_MAX);

	u64 idx = irContext->localValues->count;
	Value *result = BucketArrayAdd(irContext->localValues);
	*result = value;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

inline Value IRGetValue(IRContext *irContext, u32 valueIdx)
{
	ASSERT(valueIdx > 0);
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(valueIdx);
	else
		return (*irContext->localValues)[valueIdx];
}

inline Value *IRGetLocalValue(IRContext *irContext, u32 valueIdx)
{
	ASSERT(valueIdx > 0);
	ASSERT(!(valueIdx & VALUE_GLOBAL_BIT));
	return &(*irContext->localValues)[valueIdx];
}

inline void IRUpdateValue(IRContext *irContext, u32 valueIdx, Value *value)
{
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = g_context->globalValues.GetForWrite();
		globalValues[valueIdx & VALUE_GLOBAL_MASK] = *value;
	}
	else
		(*irContext->localValues)[valueIdx] = *value;
}

inline void IRSetValueFlags(IRContext *irContext, u32 valueIdx, u32 flags)
{
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = g_context->globalValues.GetForWrite();
		globalValues[valueIdx & VALUE_GLOBAL_MASK].flags |= flags;
	}
	else
		(*irContext->localValues)[valueIdx].flags |= flags;
}

IRLabel *IRNewLabel(IRContext *irContext, String name)
{
	IRLabel result = {};

	result.name = name;
	result.instructionIdx = -1;

	IRLabel *newLabel = BucketArrayAdd(&irContext->irLabels);
	*newLabel = result;
	return newLabel;
}

void PushIRScope(IRContext *irContext)
{
	IRScope newScope = {};
	DynamicArrayInit(&newScope.deferredStatements, 4);
	*DynamicArrayAdd(&irContext->irStack) = newScope;
}

inline void PopIRScope(IRContext *irContext)
{
	ASSERT(irContext->irStack.size);
	--irContext->irStack.size;
}

inline IRInstruction *IRAddInstruction(IRContext *irContext)
{
	return BucketArrayAdd(irContext->irInstructions);
}

inline void IRAddComment(IRContext *irContext, SourceLocation loc, String comment)
{
	IRInstruction result = {
		.type = IRINSTRUCTIONTYPE_COMMENT,
		.loc = loc,
		.comment = comment
	};
	*IRAddInstruction(irContext) = result;
}

bool IRShouldPassByCopy(u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
	// @Speed
	return  typeInfo.typeCategory == TYPECATEGORY_ARRAY ||
		  ((typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			typeInfo.typeCategory == TYPECATEGORY_UNION) &&
			typeInfo.size != 1 &&
			typeInfo.size != 2 &&
			typeInfo.size != 4 &&
			typeInfo.size != 8);
}

inline IRValue IRValueValue(u32 valueIdx, u32 typeTableIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueValue(IRContext *irContext, u32 valueIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = IRGetValue(irContext, valueIdx).typeTableIdx;
	return result;
}

inline IRValue IRValueMemory(u32 baseValueIdx, u32 typeTableIdx, s64 offset = 0)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_MEMORY;
	result.mem.baseValueIdx = baseValueIdx;
	result.mem.offset = offset;
	result.mem.elementSize = 0;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueImmediate(s64 immediate, u32 typeTableIdx = TYPETABLEIDX_S64)
{
	IRValue result;
	result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
	result.immediate = immediate;
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueImmediateString(String string)
{
	IRValue result;
	result.valueType = IRVALUETYPE_IMMEDIATE_STRING;
	result.typeTableIdx = TYPETABLEIDX_Unset;
	if (string.size == 0)
		result.immediateStringIdx = 0;
	else {
		auto stringLiterals = g_context->stringLiterals.GetForWrite();
		s64 stringCount = stringLiterals->count;
		ASSERT(stringCount < U32_MAX);
		for (u32 stringIdx = 0; stringIdx < stringCount; ++stringIdx) {
			if (StringEquals(stringLiterals[stringIdx].string, string)) {
				result.immediateStringIdx = stringIdx;
				goto done;
			}
		}
		// Create a new one
		u32 idx = (u32)stringCount;
		u32 globalValueIdx = NewGlobalValue(SNPrintF(8, "_str%u", idx),
				GetTypeInfoPointerOf(TYPETABLEIDX_U8), VALUEFLAGS_ON_STATIC_STORAGE);
		String staticDataStr = CopyStringToStaticData(string);

		{
			ScopedLockSpin lock(&g_context->globalValuesLock);
			*HashMapGetOrAdd(&g_context->globalValueContents, globalValueIdx & VALUE_GLOBAL_MASK) =
				(u8 *)staticDataStr.data;
		}

		result.immediateStringIdx = idx;
		*BucketArrayAdd(&stringLiterals) = { globalValueIdx, staticDataStr };
	}
done:
	return result;
}

IRValue IRPointerToValue(IRContext *irContext, SourceLocation loc, IRValue in);

IRValue IRValueImmediateCStr(IRContext *irContext, String string)
{
	u32 charPtrTypeIdx = GetTypeInfoPointerOf(TYPETABLEIDX_U8);
	if (string.size == 0)
		return IRValueImmediate(0, charPtrTypeIdx);

	u32 globalValueIdx;
	{
		auto stringLiterals = g_context->cStringLiterals.GetForWrite();
		s64 stringCount = stringLiterals->count;
		ASSERT(stringCount < U32_MAX);
		for (u32 stringIdx = 0; stringIdx < stringCount; ++stringIdx) {
			if (StringEquals(stringLiterals[stringIdx].string, string)) {
				globalValueIdx = stringLiterals[stringIdx].globalValueIdx;
				goto done;
			}
		}
		// Create a new one
		u32 idx = (u32)stringCount;
		globalValueIdx = NewGlobalValue(SNPrintF(8, "_cstr%u", idx),
				charPtrTypeIdx, VALUEFLAGS_ON_STATIC_STORAGE);
		String staticDataStr = CopyStringToStaticData(string, true);

		{
			ScopedLockSpin lock(&g_context->globalValuesLock);
			*HashMapGetOrAdd(&g_context->globalValueContents, globalValueIdx & VALUE_GLOBAL_MASK) =
				(u8 *)staticDataStr.data;
		}

		*BucketArrayAdd(&stringLiterals) = { globalValueIdx, staticDataStr };
	}
done:
	return IRPointerToValue(irContext, {}, IRValueValue(globalValueIdx, charPtrTypeIdx));
}

IRValue IRValueImmediateF64(f64 f)
{
	u32 globalValueIdx;
	{
		union { f64 in; u64 inQWord; };
		in = f;

		auto floatLiterals = g_context->f64Literals.GetForWrite();
		s64 count = floatLiterals->count;
		for (u32 floatIdx = 0; floatIdx < count; ++floatIdx) {
			// Compare as quad word, not float (for example to distinguish 0 from -0
			union { f64 current; u64 currentQWord; };
			current = *floatLiterals[floatIdx].asF64;

			if (inQWord == currentQWord) {
				globalValueIdx = floatLiterals[floatIdx].globalValueIdx;
				goto done;
			}
		}
		// Create a new one
		u32 idx = (u32)count;
		globalValueIdx = NewGlobalValue(SNPrintF(12, "_staticF64%u", idx), TYPETABLEIDX_F64,
				VALUEFLAGS_ON_STATIC_STORAGE);

		f64 *staticData = (f64 *)AllocateStaticData(globalValueIdx, 8, 8);
		*staticData = f;

		*BucketArrayAdd(&floatLiterals) = { .globalValueIdx = globalValueIdx, .asF64 = staticData };
	}
done:
	return IRValueValue(globalValueIdx, TYPETABLEIDX_F64);
}

IRValue IRValueImmediateF32(f32 f)
{
	u32 globalValueIdx;
	{
		union { f32 in; u32 inDWord; };
		in = f;

		auto floatLiterals = g_context->f32Literals.GetForWrite();
		s64 count = floatLiterals->count;
		for (u32 floatIdx = 0; floatIdx < count; ++floatIdx) {
			// Compare as quad word, not float (for example to distinguish 0 from -0
			union { f32 current; u32 currentDWord; };
			current = *floatLiterals[floatIdx].asF32;

			if (inDWord == currentDWord) {
				globalValueIdx = floatLiterals[floatIdx].globalValueIdx;
				goto done;
			}
		}
		// Create a new one
		u32 idx = (u32)count;
		globalValueIdx = NewGlobalValue(SNPrintF(12, "_staticF32%u", idx),
				TYPETABLEIDX_F32, VALUEFLAGS_ON_STATIC_STORAGE);

		f32 *staticData = (f32 *)AllocateStaticData(globalValueIdx, 4, 4);
		*staticData = f;

		*BucketArrayAdd(&floatLiterals) = { .globalValueIdx = globalValueIdx, .asF32 = staticData };
	}
done:
	return IRValueValue(globalValueIdx, TYPETABLEIDX_F32);
}

inline IRValue IRValueImmediateFloat(f64 f, u32 typeTableIdx)
{
	switch (typeTableIdx) {
	case TYPETABLEIDX_F32:
	case TYPETABLEIDX_FLOATING: // @Improve: maybe have type checker always collapse these?
		return IRValueImmediateF32((f32)f);
	case TYPETABLEIDX_F64:
		return IRValueImmediateF64(f);
	default:
		ASSERT(false);
	}
}

inline IRValue IRValueProcedure(u32 procedureIdx)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_PROCEDURE;
	result.procedureIdx = procedureIdx;
	result.typeTableIdx = GetProcedureRead(procedureIdx).typeTableIdx;
	return result;
}

inline IRValue IRValueNewValue(IRContext *irContext, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = 0)
{
	u32 newValueIdx = IRNewValue(irContext, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = newValueIdx;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueNewValue(IRContext *irContext, String name, u32 typeTableIdx, u32 flags,
		u32 immitateValueIdx = 0)
{
	u32 newValueIdx = IRNewValue(irContext, name, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = newValueIdx;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueTypeOf(u32 typeTableIdx)
{
	static u32 typeInfoPointerTypeIdx = GetTypeInfoPointerOf(TYPETABLEIDX_TYPE_INFO_STRUCT);
	u32 typeValueIdx = GetTypeInfo(typeTableIdx).valueIdx;
	return IRValueValue(typeValueIdx, typeInfoPointerTypeIdx);
}

IRValue IRDereferenceValue(IRContext *irContext, SourceLocation loc, IRValue in)
{
	TypeInfo pointerTypeInfo = GetTypeInfo(in.typeTableIdx);
	ASSERT(pointerTypeInfo.typeCategory == TYPECATEGORY_POINTER);
	u32 pointedTypeIdx = pointerTypeInfo.pointerInfo.pointedTypeTableIdx;

	ASSERTF(in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_MEMORY,
			"Dereferenced value must be either VALUE or MEMORY");

	if (in.valueType == IRVALUETYPE_VALUE)
		return IRValueMemory(in.valueIdx, pointedTypeIdx);
	else if (in.valueType == IRVALUETYPE_MEMORY) {
		u32 newValueIdx = IRNewValue(irContext, in.typeTableIdx, VALUEFLAGS_TRY_IMMITATE,
				in.mem.baseValueIdx);
		IRValue value = IRValueValue(newValueIdx, in.typeTableIdx);
#if DEBUG_BUILD
		String name = SStringConcat("_deref_"_s, IRGetValue(irContext, in.mem.baseValueIdx).name);
		IRGetLocalValue(irContext, newValueIdx)->name = name;
#endif

		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
			.loc = loc,
			.assignment = { .src = in, .dst = value }
		};

		IRValue result = IRValueMemory(newValueIdx, pointedTypeIdx);
		return result;
	}
	else
		ASSERT(false);
}

IRValue IRPointerToValue(IRContext *irContext, SourceLocation loc, IRValue in)
{
	ASSERT(in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_MEMORY);
	u32 pointerTypeIdx = GetTypeInfoPointerOf(in.typeTableIdx);

	IRValue result = IRValueNewValue(irContext, pointerTypeIdx, 0);
#if DEBUG_BUILD
	String name = SStringConcat("_pointerof_"_s, IRGetValue(irContext, in.valueIdx).name);
	IRGetLocalValue(irContext, result.valueIdx)->name = name;
#endif

	IRInstruction addressInst = {};
	addressInst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;
	addressInst.loc = loc;
	addressInst.unaryOperation.in = in;
	addressInst.unaryOperation.out = result;
	*IRAddInstruction(irContext) = addressInst;

	return result;
}

void IRDoAssignment(IRContext *irContext, SourceLocation loc, IRValue dstValue, IRValue srcValue);
IRValue IRDoMemberAccess(IRContext *irContext, SourceLocation loc, u32 structPtrValueIdx,
		StructMember structMember)
{
#if DEBUG_BUILD
	String structValueName = IRGetValue(irContext, structPtrValueIdx).name;
	IRAddComment(irContext, loc, SNPrintF(28 + (int)structValueName.size + (int)structMember.name.size,
				"Accessing struct member \"%S.%S\"",
				structValueName, structMember.name));
#endif

	s64 offset = structMember.offset;
	IRValue result = IRValueMemory(structPtrValueIdx, structMember.typeTableIdx,
			offset);
	result = IRPointerToValue(irContext, loc, result);
	result.valueType = IRVALUETYPE_MEMORY;
	result.typeTableIdx = structMember.typeTableIdx;
	return result;
}

IRValue IRDoArrayAccess(IRContext *irContext, SourceLocation loc, u32 arrayPtrValueIdx,
		u32 arrayTypeIdx, IRValue indexValue)
{
	TypeInfo arrayTypeInfo = GetTypeInfo(arrayTypeIdx);

	// arrayValue must be an array (or string). If it's a pointer, it should be dereferenced before
	// calling this procedure.
	ASSERT(arrayTypeInfo.typeCategory == TYPECATEGORY_ARRAY ||
		   arrayTypeIdx == TYPETABLEIDX_STRING_STRUCT);

	u32 elementTypeIdx;
	if (arrayTypeIdx == TYPETABLEIDX_STRING_STRUCT)
		elementTypeIdx = TYPETABLEIDX_U8;
	else
		elementTypeIdx = arrayTypeInfo.arrayInfo.elementTypeTableIdx;
	u32 pointerToElementTypeIdx = GetTypeInfoPointerOf(elementTypeIdx);

	// Dynamic arrays
	if (arrayTypeIdx == TYPETABLEIDX_STRING_STRUCT || arrayTypeInfo.arrayInfo.count == 0) {
		// Access the 'data' pointer
		IRAddComment(irContext, loc, "Addressing dynamic array"_s);

		TypeInfo arrayStructTypeInfo = GetTypeInfo(TYPETABLEIDX_ARRAY_STRUCT);
		StructMember dataMember = arrayStructTypeInfo.structInfo.members[1];

		IRValue dataValue = IRDoMemberAccess(irContext, loc, arrayPtrValueIdx, dataMember);
		dataValue = IRDereferenceValue(irContext, loc, dataValue);
		arrayPtrValueIdx = dataValue.valueIdx;
	}

	s64 elementSize = GetTypeInfo(elementTypeIdx).size;

	if (indexValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
		return IRValueMemory(arrayPtrValueIdx, elementTypeIdx,
				indexValue.immediate * elementSize);
	}
	else if ((indexValue.valueType == IRVALUETYPE_VALUE ||
			indexValue.valueType == IRVALUETYPE_MEMORY) &&
			CountOnes64(elementSize) == 1 && elementSize <= 8) {
		// @Todo: move x64 specifics like element size limitations and force to register to x64
		// backend.
		IRValue indexForceReg = IRValueNewValue(irContext, "_idx"_s, TYPETABLEIDX_S64,
				VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, indexValue.valueIdx);
		IRDoAssignment(irContext, loc, indexForceReg, indexValue);

		IRValue result = IRValueMemory(arrayPtrValueIdx, elementTypeIdx);
		result.mem.indexValueIdx = indexForceReg.valueIdx;
		result.mem.elementSize = elementSize;
		return result;
	}

	// Fall back to simple add instruction
	IRValue offsetValue;
	offsetValue = IRValueNewValue(irContext, "_array_offset"_s, TYPETABLEIDX_S64, 0);
	if (elementSize == 1)
		IRDoAssignment(irContext, loc, offsetValue, indexValue);
	else
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_MULTIPLY,
			.loc = loc,
			.binaryOperation = {
				.left  = indexValue,
				.right = IRValueImmediate(elementSize, TYPETABLEIDX_S64),
				.out   = offsetValue
			}
		};

	IRValue pointerToElementValue = IRValueNewValue(irContext, "_array_element"_s,
			pointerToElementTypeIdx, 0);
	IRValue pointerToArray = IRValueValue(arrayPtrValueIdx, pointerToElementTypeIdx);
	*IRAddInstruction(irContext) = {
		.type = IRINSTRUCTIONTYPE_ADD,
		.loc = loc,
		.binaryOperation = {
			.left = pointerToArray,
			.right = offsetValue,
			.out = pointerToElementValue
		}
	};

	return IRValueMemory(pointerToElementValue.valueIdx, elementTypeIdx);
}

inline void IRPushValueIntoStack(IRContext *irContext, SourceLocation loc, u32 valueIdx)
{
	*IRAddInstruction(irContext) = {
		.type = IRINSTRUCTIONTYPE_PUSH_VALUE,
		.loc = loc,
		.pushValue = { .valueIdx = valueIdx }
	};
}

inline u32 IRAddTempValue(IRContext *irContext, SourceLocation loc, u32 typeTableIdx, u8 flags)
{
	u32 valueIdx = IRNewValue(irContext, typeTableIdx, flags);
	IRPushValueIntoStack(irContext, loc, valueIdx);
	return valueIdx;
}

IRValue IRDoCast(IRContext *irContext, SourceLocation loc, IRValue srcValue, u32 typeTableIdx)
{
	ASSERT(srcValue.valueType != IRVALUETYPE_TUPLE);
	// Cast string literal to string struct
	if (srcValue.valueType == IRVALUETYPE_IMMEDIATE_STRING) {
		u32 tempValueIdx = IRAddTempValue(irContext, loc, typeTableIdx, 0);
#if DEBUG_BUILD
		if (srcValue.valueType == IRVALUETYPE_VALUE) {
			String name = SStringConcat("_cast_"_s, IRGetValue(irContext, srcValue.valueIdx).name);
			IRGetLocalValue(irContext, tempValueIdx)->name = name;
		}
		else
			IRGetLocalValue(irContext, tempValueIdx)->name = "_cast"_s;
#endif
		IRValue result = IRValueValue(tempValueIdx, typeTableIdx);

		StringLiteral literal;
		{
			auto stringLiterals = g_context->stringLiterals.GetForRead();
			literal = stringLiterals[srcValue.immediateStringIdx];
		}

		TypeInfo stringTypeInfo = GetTypeInfo(TYPETABLEIDX_STRING_STRUCT);
		IRValue dstPtr = IRPointerToValue(irContext, loc, result);
		IRValue sizeMember = IRDoMemberAccess(irContext, loc, dstPtr.valueIdx,
				stringTypeInfo.structInfo.members[0]);

		IRInstruction sizeSetInst = {
			.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
			.loc = loc,
			.assignment = {
				.src = IRValueImmediate(literal.string.size),
				.dst = sizeMember
			}
		};
		*IRAddInstruction(irContext) = sizeSetInst;

		IRValue dataMember = IRDoMemberAccess(irContext, loc, dstPtr.valueIdx,
				stringTypeInfo.structInfo.members[1]);

		u32 charPtrTypeIdx = GetTypeInfoPointerOf(TYPETABLEIDX_U8);
		srcValue.typeTableIdx = charPtrTypeIdx;
		IRInstruction dataSetInst = {
			.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
			.loc = loc,
			.assignment = {
				.src = IRValueValue(literal.globalValueIdx, charPtrTypeIdx),
				.dst = dataMember
			}
		};
		*IRAddInstruction(irContext) = dataSetInst;

		return result;
	}
	// Cast to Any
	else if (typeTableIdx == TYPETABLEIDX_ANY_STRUCT &&
		srcValue.typeTableIdx != TYPETABLEIDX_ANY_STRUCT)
	{
		IRAddComment(irContext, loc, "Wrapping in Any"_s);
		TypeInfo anyTypeInfo = GetTypeInfo(TYPETABLEIDX_ANY_STRUCT);

		u32 tempValueIdx = IRAddTempValue(irContext, loc, typeTableIdx, 0);
#if DEBUG_BUILD
		if (srcValue.valueType == IRVALUETYPE_VALUE) {
			String name = SStringConcat("_any_"_s, IRGetValue(irContext, srcValue.valueIdx).name);
			IRGetLocalValue(irContext, tempValueIdx)->name = name;
		}
		else
			IRGetLocalValue(irContext, tempValueIdx)->name = "_any"_s;
#endif
		IRValue result = IRValueValue(tempValueIdx, typeTableIdx);

		// Access typeInfo member
		IRValue resultPtr = IRPointerToValue(irContext, loc, result);
		IRValue typeInfoMember = IRDoMemberAccess(irContext, loc, resultPtr.valueIdx,
				anyTypeInfo.structInfo.members[0]);

		// Write pointer to typeInfo to it
		IRInstruction typeAssignInst = {
			.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
			.loc = loc,
			.unaryOperation = {
				.in = IRValueTypeOf(srcValue.typeTableIdx),
				.out = typeInfoMember
			}
		};
		*IRAddInstruction(irContext) = typeAssignInst;

		// Access data member
		static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(TYPETABLEIDX_VOID);
		IRValue dataMember = IRDoMemberAccess(irContext, loc, resultPtr.valueIdx,
				anyTypeInfo.structInfo.members[1]);
		dataMember.typeTableIdx = voidPtrTypeIdx;

		IRValue dataValue = srcValue;
		TypeInfo dataTypeInfo = GetTypeInfo(srcValue.typeTableIdx);

		// If data isn't in memory, copy to a variable
		if (dataTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
			dataTypeInfo.typeCategory != TYPECATEGORY_UNION &&
			dataTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
		{
			if (IRShouldPassByCopy(dataValue.typeTableIdx)) {
				u32 anyContentValueIdx = IRAddTempValue(irContext, loc, srcValue.typeTableIdx,
						VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
				IRGetLocalValue(irContext, anyContentValueIdx)->name = "_tempVarForAny"_s;
#endif
				IRValue tempVarIRValue = IRValueValue(anyContentValueIdx, srcValue.typeTableIdx);

				*IRAddInstruction(irContext) = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = dataValue,
						.dst = tempVarIRValue
					}
				};

				dataValue = tempVarIRValue;

				*IRAddInstruction(irContext) = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = IRPointerToValue(irContext, loc, dataValue),
						.dst = dataMember
					}
				};
			}
			// Small primitives are stored directly on the Any struct.
			else {
				dataMember.typeTableIdx = dataValue.typeTableIdx;

				*IRAddInstruction(irContext) = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = dataValue,
						.dst = dataMember
					}
				};
			}
		}
		// These are already in memory, just save a pointer
		else {
			*IRAddInstruction(irContext) = {
				.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
				.loc = loc,
				.assignment = {
					.src = IRPointerToValue(irContext, loc, dataValue),
					.dst = dataMember
				}
			};
		}

		return result;
	}
	else {
		TypeInfo dstTypeInfo = GetTypeInfo(StripAllAliases(typeTableIdx));
		TypeInfo srcTypeInfo = GetTypeInfo(StripAllAliases(srcValue.typeTableIdx));

		bool isSrcFloat = srcTypeInfo.typeCategory == TYPECATEGORY_FLOATING;
		bool isDstFloat = dstTypeInfo.typeCategory == TYPECATEGORY_FLOATING;

		if (srcTypeInfo.size == dstTypeInfo.size && isSrcFloat == isDstFloat) {
			// No cast needed
			srcValue.typeTableIdx = typeTableIdx;
			return srcValue;
		}

		u32 tempValueIdx = IRAddTempValue(irContext, loc, typeTableIdx, 0);
#if DEBUG_BUILD
		if (srcValue.valueType == IRVALUETYPE_VALUE) {
			String name = SStringConcat("_cast_"_s, IRGetValue(irContext, srcValue.valueIdx).name);
			IRGetLocalValue(irContext, tempValueIdx)->name = name;
		}
		else
			IRGetLocalValue(irContext, tempValueIdx)->name = "_cast"_s;
#endif
		IRValue result = IRValueValue(tempValueIdx, typeTableIdx);

		// Cast static array to dynamic array
		if (dstTypeInfo.typeCategory  == TYPECATEGORY_ARRAY &&
			srcTypeInfo.typeCategory == TYPECATEGORY_ARRAY &&
			dstTypeInfo.arrayInfo.count  == 0 &&
			srcTypeInfo.arrayInfo.count != 0)
		{
			TypeInfo dynamicArrayTypeInfo = GetTypeInfo(TYPETABLEIDX_ARRAY_STRUCT);

			IRValue resultPtr = IRPointerToValue(irContext, loc, result);

			// Size
			StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
			IRValue sizeMember = IRDoMemberAccess(irContext, loc, resultPtr.valueIdx, sizeStructMember);
			IRValue sizeValue = IRValueImmediate(srcTypeInfo.arrayInfo.count);
			IRDoAssignment(irContext, loc, sizeMember, sizeValue);

			// Data
			StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
			IRValue dataMember = IRDoMemberAccess(irContext, loc, resultPtr.valueIdx, dataStructMember);
			IRValue dataValue = IRPointerToValue(irContext, loc, srcValue);
			IRDoAssignment(irContext, loc, dataMember, dataValue);
		}
		else if (isSrcFloat && !isDstFloat) {
			*IRAddInstruction(irContext) = {
				.type = IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT,
				.loc = loc,
				.assignment = { .src = srcValue, .dst = result }
			};
		}
		else if (!isSrcFloat && isDstFloat) {
			*IRAddInstruction(irContext) = {
				.type = IRINSTRUCTIONTYPE_CONVERT_INT_TO_FLOAT,
				.loc = loc,
				.assignment = { .src = srcValue, .dst = result }
			};
		}
		else if (isSrcFloat) {
			*IRAddInstruction(irContext) = {
				.type = IRINSTRUCTIONTYPE_CONVERT_PRECISION,
				.loc = loc,
				.assignment = { .src = srcValue, .dst = result }
			};
		}
		else if (srcTypeInfo.size < dstTypeInfo.size) {
			if (dstTypeInfo.typeCategory == TYPECATEGORY_INTEGER && dstTypeInfo.integerInfo.isSigned)
				*IRAddInstruction(irContext) = {
					.type = IRINSTRUCTIONTYPE_SIGN_EXTEND,
					.loc = loc,
					.assignment = { .src = srcValue, .dst = result }
				};
			else
				*IRAddInstruction(irContext) = {
					.type = IRINSTRUCTIONTYPE_ZERO_EXTEND,
					.loc = loc,
					.assignment = { .src = srcValue, .dst = result }
				};
		}
		else if (srcTypeInfo.size > dstTypeInfo.size) {
			*IRAddInstruction(irContext) = {
				.type = IRINSTRUCTIONTYPE_TRUNCATE,
				.loc = loc,
				.assignment = { .src = srcValue, .dst = result }
			};
		}
		else
			ASSERT(false);

		return result;
	}
}

void IRDoAssignment(IRContext *irContext, SourceLocation loc, IRValue dstValue, IRValue srcValue)
{
	ASSERT(dstValue.valueType != IRVALUETYPE_IMMEDIATE_INTEGER);
	ASSERT(dstValue.valueType != IRVALUETYPE_IMMEDIATE_FLOAT);
	ASSERT(dstValue.valueType != IRVALUETYPE_IMMEDIATE_STRING);

	// Tuples
	if (dstValue.valueType == IRVALUETYPE_TUPLE)
	{
		// Tuples of different sizes should be caught on type checking
		ASSERT(srcValue.valueType == IRVALUETYPE_TUPLE);
		ASSERT(srcValue.tuple.size == dstValue.tuple.size);

		for (int i = 0; i < dstValue.tuple.size; ++i)
			IRDoAssignment(irContext, loc, dstValue.tuple[i], srcValue.tuple[i]);
		return;
	}

	srcValue = IRDoCast(irContext, loc, srcValue, dstValue.typeTableIdx);

	dstValue.typeTableIdx = StripAllAliases(dstValue.typeTableIdx);
	srcValue.typeTableIdx = StripAllAliases(srcValue.typeTableIdx);

	TypeInfo dstTypeInfo = GetTypeInfo(dstValue.typeTableIdx);
	TypeInfo srcTypeInfo = GetTypeInfo(srcValue.typeTableIdx);

	// Copy structs/arrays
	if (srcTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		srcTypeInfo.typeCategory == TYPECATEGORY_UNION ||
		srcTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
	{
		ASSERT(dstTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			   dstTypeInfo.typeCategory == TYPECATEGORY_UNION ||
			   dstTypeInfo.typeCategory == TYPECATEGORY_ARRAY);

		u64 size = GetTypeInfo(srcValue.typeTableIdx).size;
		IRValue sizeValue = IRValueImmediate(size);

		IRInstruction inst = {
			.type = IRINSTRUCTIONTYPE_COPY_MEMORY,
			.loc = loc,
			.copyMemory = {
				.src = IRPointerToValue(irContext, loc, srcValue),
				.dst = IRPointerToValue(irContext, loc, dstValue),
				.size = sizeValue
			}
		};

		*IRAddInstruction(irContext) = inst;
	}
	else if (dstTypeInfo.typeCategory == TYPECATEGORY_FLOATING &&
			 srcValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
		// Can't just pretend we can assign an integer immediate to a floating point value. We need
		// to convert it to a floating point number first.
		// @Check: Maybe we should do this on backend?
		IRInstruction inst = {
			.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
			.loc = loc,
			.assignment = {
				.src = IRValueImmediateFloat((f64)srcValue.immediate, dstValue.typeTableIdx),
				.dst = dstValue
			}
		};

		*IRAddInstruction(irContext) = inst;

	}
	else {
		IRInstruction inst = {
			.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
			.loc = loc,
			.assignment = {
				.src = srcValue,
				.dst = dstValue
			}
		};

		if (srcValue.valueType == IRVALUETYPE_PROCEDURE)
			inst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;

		*IRAddInstruction(irContext) = inst;
	}
}

inline void IRInsertLabelInstruction(IRContext *irContext, SourceLocation loc, IRLabel *label)
{
	label->instructionIdx = irContext->irInstructions->count;
	*IRAddInstruction(irContext) = {
		.type = IRINSTRUCTIONTYPE_LABEL,
		.loc = loc,
		.label = label
	};
}

IRValue IRInstructionFromBinaryOperation(IRContext *irContext, const ASTExpression *expression,
		IRValue outValue)
{
	SourceLocation loc = expression->any.loc;
	IRValue result = {};

	ASTExpression *leftHand  = expression->binaryOperation.leftHand;
	ASTExpression *rightHand = expression->binaryOperation.rightHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS) {
		IRValue irValue = IRGenFromExpression(irContext, leftHand);

		IRValue structPtr;

		TypeInfo structTypeInfo = GetTypeInfo(irValue.typeTableIdx);
		if (structTypeInfo.typeCategory == TYPECATEGORY_POINTER) {
			// Dereference the pointer to the struct
			u32 newValueIdx = IRNewValue(irContext, irValue.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			IRValue newValue = IRValueValue(newValueIdx, irValue.typeTableIdx);

#if DEBUG_BUILD
			String valueName = IRGetValue(irContext, irValue.valueIdx).name; 
			IRAddComment(irContext, loc, SNPrintF(64, "Dereference struct pointer \"%S\"", valueName));
			String name = SStringConcat("_derefstrctptr_"_s, valueName);
			IRGetLocalValue(irContext, newValueIdx)->name = name;
#endif

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			inst.loc = expression->any.loc;
			inst.assignment.dst = newValue;
			inst.assignment.src = irValue;
			*IRAddInstruction(irContext) = inst;

			u32 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
			structPtr = IRValueValue(newValueIdx, pointedTypeIdx);
		}
		else
			structPtr = IRPointerToValue(irContext, loc, irValue);

		ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
		ASSERT(rightHand->identifier.type == NAMETYPE_STRUCT_MEMBER);
		StructMember structMember = *rightHand->identifier.structMember;

		result = IRDoMemberAccess(irContext, loc, structPtr.valueIdx, structMember);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS) {
		IRValue arrayValue = IRGenFromExpression(irContext, leftHand);
		IRValue indexValue = IRGenFromExpression(irContext, rightHand);

		if (GetTypeInfo(arrayValue.typeTableIdx).typeCategory == TYPECATEGORY_POINTER) {
			// Dereference the pointer to the array
			arrayValue = IRDereferenceValue(irContext, loc, arrayValue);
		}

		IRValue ptrToArray = IRPointerToValue(irContext, loc, arrayValue);

		ASSERT(arrayValue.valueType == IRVALUETYPE_VALUE ||
				arrayValue.valueType == IRVALUETYPE_MEMORY);
		result = IRDoArrayAccess(irContext, loc, ptrToArray.valueIdx, arrayValue.typeTableIdx,
				indexValue);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_AND) {
		IRLabel *assignZeroLabel = IRNewLabel(irContext, "assignZero"_s);

		IRValue leftValue  = IRGenFromExpression(irContext, leftHand);

		// Short-circuit jump
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = assignZeroLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(irContext, rightHand);

		// Second jump
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = assignZeroLabel,
				.condition = rightValue
			}
		};

		IRDoAssignment(irContext, loc, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = IRNewLabel(irContext, "skipAssignZero"_s);

		// Skip-assigning-zero jump
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP,
			.loc = loc,
			.jump = { .label = skipAssignZeroLabel }
		};

		IRInsertLabelInstruction(irContext, loc, assignZeroLabel);

		IRDoAssignment(irContext, loc, outValue, IRValueImmediate(0));

		IRInsertLabelInstruction(irContext, loc, skipAssignZeroLabel);

		result = outValue;
	}
	else if (expression->binaryOperation.op == TOKEN_OP_OR) {
		IRLabel *assignZeroLabel = IRNewLabel(irContext, "assignZero"_s);
		IRLabel *skipRightLabel = IRNewLabel(irContext, "skipRight"_s);

		IRValue leftValue  = IRGenFromExpression(irContext, leftHand);

		// Short-circuit jump
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipRightLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(irContext, rightHand);

		// Second jump
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = assignZeroLabel,
				.condition = rightValue
			}
		};

		IRInsertLabelInstruction(irContext, loc, skipRightLabel);

		IRDoAssignment(irContext, loc, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = IRNewLabel(irContext, "skipAssignZero"_s);

		// Skip-assign-zero jump
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP,
			.loc = loc,
			.jump = { .label = skipAssignZeroLabel }
		};

		IRInsertLabelInstruction(irContext, loc, assignZeroLabel);

		IRDoAssignment(irContext, loc, outValue, IRValueImmediate(0));

		IRInsertLabelInstruction(irContext, loc, skipAssignZeroLabel);

		result = outValue;
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT_AND) {
		IRLabel *skipAssignZeroLabel = IRNewLabel(irContext, "skipAssignZero"_s);

		IRValue leftValue  = IRGenFromExpression(irContext, leftHand);

		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignZeroLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(irContext, rightHand);

		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignZeroLabel,
				.condition = rightValue
			}
		};

		IRDoAssignment(irContext, loc, leftValue, IRValueImmediate(0));

		IRInsertLabelInstruction(irContext, loc, skipAssignZeroLabel);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT_OR) {
		IRLabel *skipAssignOneLabel = IRNewLabel(irContext, "skipAssignOne"_s);

		IRValue leftValue  = IRGenFromExpression(irContext, leftHand);

		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignOneLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(irContext, rightHand);

		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignOneLabel,
				.condition = rightValue
			}
		};

		IRDoAssignment(irContext, loc, leftValue, IRValueImmediate(1));

		IRInsertLabelInstruction(irContext, loc, skipAssignOneLabel);
	}
	else {
#if DEBUG_BUILD
		TypeInfo leftTypeInfo = GetTypeInfo(leftHand->typeTableIdx);
		ASSERT(leftTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
			   leftTypeInfo.typeCategory != TYPECATEGORY_UNION);
#endif

		IRValue left  = IRGenFromExpression(irContext, leftHand);
		IRValue right = IRGenFromExpression(irContext, rightHand);

		if (left.typeTableIdx != right.typeTableIdx)
			right = IRDoCast(irContext, loc, right, left.typeTableIdx);

		IRInstruction inst = {};
		inst.loc = loc;
		inst.binaryOperation.left  = left;
		inst.binaryOperation.right = right;

		switch (expression->binaryOperation.op) {
		case TOKEN_OP_PLUS:
		case TOKEN_OP_ASSIGNMENT_PLUS:
		{
			inst.type = IRINSTRUCTIONTYPE_ADD;
		} break;
		case TOKEN_OP_MINUS:
		case TOKEN_OP_ASSIGNMENT_MINUS:
		{
			inst.type = IRINSTRUCTIONTYPE_SUBTRACT;
		} break;
		case TOKEN_OP_MULTIPLY:
		case TOKEN_OP_ASSIGNMENT_MULTIPLY:
		{
			inst.type = IRINSTRUCTIONTYPE_MULTIPLY;
		} break;
		case TOKEN_OP_DIVIDE:
		case TOKEN_OP_ASSIGNMENT_DIVIDE:
		{
			inst.type = IRINSTRUCTIONTYPE_DIVIDE;
		} break;
		case TOKEN_OP_MODULO:
		case TOKEN_OP_ASSIGNMENT_MODULO:
		{
			inst.type = IRINSTRUCTIONTYPE_MODULO;
		} break;
		case TOKEN_OP_SHIFT_LEFT:
		case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
		{
			inst.type = IRINSTRUCTIONTYPE_SHIFT_LEFT;
		} break;
		case TOKEN_OP_SHIFT_RIGHT:
		case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
		{
			inst.type = IRINSTRUCTIONTYPE_SHIFT_RIGHT;
		} break;
		case TOKEN_OP_BITWISE_AND:
		case TOKEN_OP_ASSIGNMENT_BITWISE_AND:
		{
			inst.type = IRINSTRUCTIONTYPE_BITWISE_AND;
		} break;
		case TOKEN_OP_BITWISE_OR:
		case TOKEN_OP_ASSIGNMENT_BITWISE_OR:
		{
			inst.type = IRINSTRUCTIONTYPE_BITWISE_OR;
		} break;
		case TOKEN_OP_BITWISE_XOR:
		case TOKEN_OP_ASSIGNMENT_BITWISE_XOR:
		{
			inst.type = IRINSTRUCTIONTYPE_BITWISE_XOR;
		} break;
		case TOKEN_OP_EQUALS:
		{
			inst.type = IRINSTRUCTIONTYPE_EQUALS;
		} break;
		case TOKEN_OP_NOT_EQUALS:
		{
			inst.type = IRINSTRUCTIONTYPE_NOT_EQUALS;
		} break;
		case TOKEN_OP_GREATER_THAN:
		{
			inst.type = IRINSTRUCTIONTYPE_GREATER_THAN;
		} break;
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
		{
			inst.type = IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS;
		} break;
		case TOKEN_OP_LESS_THAN:
		{
			inst.type = IRINSTRUCTIONTYPE_LESS_THAN;
		} break;
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
		{
			inst.type = IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS;
		} break;
		case TOKEN_OP_RANGE:
		{
			LogError(expression->any.loc, "Range operator used in invalid irContext"_s);
		} break;
		default:
		{
			LogError(expression->any.loc, "Binary operator unrecognized during IR generation"_s);
		} break;
		}

		IRValue out = outValue;
		bool castOutput = out.typeTableIdx != expression->typeTableIdx;
		if (castOutput) {
			u32 tempValueIdx = IRAddTempValue(irContext, loc, expression->typeTableIdx, 0);
#if DEBUG_BUILD
			IRGetLocalValue(irContext, tempValueIdx)->name = "_binaryop_cast"_s;
#endif
			out = IRValueValue(tempValueIdx, expression->typeTableIdx);
		}

		// Hint for register allocation to try and allocate left and out in the same register
		if (out.valueType == IRVALUETYPE_VALUE &&
			left.valueType == IRVALUETYPE_VALUE)
		{
			Value v = IRGetValue(irContext, left.valueIdx);
			v.flags |= VALUEFLAGS_TRY_IMMITATE;
			v.tryImmitateValueIdx = out.valueIdx;
			IRUpdateValue(irContext, left.valueIdx, &v);
		}

		inst.binaryOperation.out = out;
		*IRAddInstruction(irContext) = inst;

		if (castOutput)
			IRDoAssignment(irContext, loc, outValue, out);

		if (expression->binaryOperation.op >= TOKEN_OP_ASSIGNMENT_Begin &&
			expression->binaryOperation.op <= TOKEN_OP_ASSIGNMENT_End)
		{
			IRDoAssignment(irContext, loc, left, outValue);
			result = left;
		}
		else
			result = outValue;
	}

	return result;
}

void IRConditionalJumpFromExpression(IRContext *irContext, ASTExpression *conditionExp, IRLabel *label, bool jumpIfTrue)
{
	// The following tries to avoid saving condition to a bool, then comparing the bool with
	// 0 in the conditional jump.
	if (conditionExp->nodeType == ASTNODETYPE_UNARY_OPERATION &&
		conditionExp->binaryOperation.op == TOKEN_OP_NOT)
	{
		IRConditionalJumpFromExpression(irContext, conditionExp->unaryOperation.expression, label, !jumpIfTrue);
		return;
	}

	if (conditionExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
		conditionExp->binaryOperation.op == (jumpIfTrue ? TOKEN_OP_OR : TOKEN_OP_AND))
	{
		IRConditionalJumpFromExpression(irContext, conditionExp->binaryOperation.leftHand, label, jumpIfTrue);
		IRConditionalJumpFromExpression(irContext, conditionExp->binaryOperation.rightHand, label, jumpIfTrue);
		return;
	}
	else if (conditionExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			 conditionExp->binaryOperation.op == (jumpIfTrue ? TOKEN_OP_AND : TOKEN_OP_OR))
	{
		IRLabel *skipRightHandLabel = IRNewLabel(irContext, "skipRightHand"_s);

		IRConditionalJumpFromExpression(irContext, conditionExp->binaryOperation.leftHand, skipRightHandLabel, !jumpIfTrue);
		IRConditionalJumpFromExpression(irContext, conditionExp->binaryOperation.rightHand, label, jumpIfTrue);

		IRInsertLabelInstruction(irContext, conditionExp->any.loc, skipRightHandLabel);
		return;
	}

	if (conditionExp->nodeType == ASTNODETYPE_BINARY_OPERATION)
	{
		IRInstruction jump = {};
		jump.loc = conditionExp->any.loc;
		switch (conditionExp->binaryOperation.op)
		{
		case TOKEN_OP_EQUALS:
			jump.type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_EQUALS : IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS;
			break;
		case TOKEN_OP_NOT_EQUALS:
			jump.type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS : IRINSTRUCTIONTYPE_JUMP_IF_EQUALS;
			break;
		case TOKEN_OP_GREATER_THAN:
			jump.type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN : IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS;
			break;
		case TOKEN_OP_LESS_THAN:
			jump.type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN : IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS;
			break;
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
			jump.type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS : IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN;
			break;
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
			jump.type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS : IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN;
			break;
		default:
			goto defaultConditionEvaluation;
		}

		IRValue leftResult  = IRGenFromExpression(irContext,
				conditionExp->binaryOperation.leftHand);
		IRValue rightResult = IRGenFromExpression(irContext,
				conditionExp->binaryOperation.rightHand);

		if (leftResult.typeTableIdx != rightResult.typeTableIdx)
			rightResult = IRDoCast(irContext, conditionExp->any.loc, rightResult, leftResult.typeTableIdx);

		if (leftResult.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			rightResult.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			switch (conditionExp->binaryOperation.op)
			{
			case TOKEN_OP_EQUALS:
				if ((leftResult.immediate != rightResult.immediate) != jumpIfTrue)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_NOT_EQUALS:
				if ((leftResult.immediate == rightResult.immediate) != jumpIfTrue)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_GREATER_THAN:
				if ((leftResult.immediate <= rightResult.immediate) != jumpIfTrue)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_LESS_THAN:
				if ((leftResult.immediate >= rightResult.immediate) != jumpIfTrue)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_GREATER_THAN_OR_EQUAL:
				if ((leftResult.immediate < rightResult.immediate) != jumpIfTrue)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_LESS_THAN_OR_EQUAL:
				if ((leftResult.immediate > rightResult.immediate) != jumpIfTrue)
					goto insertSimpleJump;
				else return;
			}
			ASSERT(false);
		}

		jump.conditionalJump2.label = label;
		jump.conditionalJump2.left  = leftResult;
		jump.conditionalJump2.right = rightResult;
		*IRAddInstruction(irContext) = jump;
		return;
	}

defaultConditionEvaluation:
	{
		// Fallback path. Just save the condition to a bool, then evaluate that bool.
		IRValue conditionResult = IRGenFromExpression(irContext, conditionExp);

		if (conditionResult.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			if (conditionResult.immediate == 0)
				goto insertSimpleJump;
			else return;
		}

		IRInstruction *jump = IRAddInstruction(irContext);
		jump->type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO : IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->loc = conditionExp->any.loc;
		jump->conditionalJump.label = label;
		jump->conditionalJump.condition = conditionResult;
		return;
	}
insertSimpleJump:
	{
		IRInstruction *jump = IRAddInstruction(irContext);
		jump->type = IRINSTRUCTIONTYPE_JUMP;
		jump->jump.label = label;
		return;
	}
}

IRValue IRDoInlineProcedureCall(IRContext *irContext, ASTProcedureCall astProcCall)
{
	SourceLocation loc = astProcCall.loc;

	ASSERT(astProcCall.callType == CALLTYPE_STATIC);
	Procedure procedure = GetProcedureRead(astProcCall.procedureIdx);

	IRLabel *oldReturnLabel = irContext->returnLabel;
	ArrayView<u32> oldReturnValueIndices = irContext->returnValueIndices;

	TypeInfoProcedure procTypeInfo = GetTypeInfo(procedure.typeTableIdx).procedureInfo;
	u64 returnValueCount = procTypeInfo.returnTypeIndices.size;

	Array<u32, ThreadAllocator> inlineReturnValues;
	ArrayInit(&inlineReturnValues, returnValueCount);
	for (int i = 0; i < returnValueCount; ++i) {
		IRValue returnValue = IRValueNewValue(irContext, "_inline_return"_s,
				procTypeInfo.returnTypeIndices[i], 0);
		IRPushValueIntoStack(irContext, loc, returnValue.valueIdx);
		*ArrayAdd(&inlineReturnValues) = returnValue.valueIdx;
	}
	bool isVarargs = procTypeInfo.isVarargs;

	IRValue returnValue = {};
	if (returnValueCount == 1)
		returnValue = IRValueValue(inlineReturnValues[0], procTypeInfo.returnTypeIndices[0]);
	else if (returnValueCount > 1) {
		returnValue.valueType = IRVALUETYPE_TUPLE;
		ArrayInit(&returnValue.tuple, returnValueCount);
		for (int i = 0; i < returnValueCount; ++i)
			*ArrayAdd(&returnValue.tuple) =
				IRValueValue(inlineReturnValues[i], procTypeInfo.returnTypeIndices[i]);
	}

	irContext->returnValueIndices = inlineReturnValues;

	// Support both varargs and default parameters here
	s32 procParamCount = (s32)procTypeInfo.parameters.size;
	s32 callParamCount = (s32)astProcCall.arguments.size;

	// Set up parameters
	s64 normalArgumentsCount = Min(callParamCount, procParamCount);
	for (int argIdx = 0; argIdx < normalArgumentsCount; ++argIdx) {
		ASTExpression *arg = astProcCall.arguments[argIdx];
		IRValue argValue = IRGenFromExpression(irContext, arg);

		u32 paramValueIdx = astProcCall.inlineParameterValues[argIdx];
		IRPushValueIntoStack(irContext, loc, paramValueIdx);
		IRValue param = IRValueValue(irContext, paramValueIdx);

		IRDoAssignment(irContext, loc, param, argValue);
	}

	// Default parameters
	for (u64 argIdx = astProcCall.arguments.size; argIdx < procParamCount; ++argIdx) {
		ProcedureParameter procParam = procTypeInfo.parameters[argIdx];
		Constant constant = procParam.defaultValue;
		IRValue arg = {};
		if (constant.type == CONSTANTTYPE_INTEGER)
			arg = IRValueImmediate(constant.valueAsInt, procParam.typeTableIdx);
		else if (constant.type == CONSTANTTYPE_FLOATING)
			arg = IRValueImmediateFloat(constant.valueAsFloat,
					procParam.typeTableIdx);
		else
			ASSERT(!"Invalid constant type");

		u32 paramValueIdx = astProcCall.inlineParameterValues[argIdx];
		IRPushValueIntoStack(irContext, loc, paramValueIdx);
		IRValue param = IRValueValue(irContext, paramValueIdx);

		IRDoAssignment(irContext, loc, param, arg);
	}

	// Varargs
	if (isVarargs) {
		static u32 anyPointerTypeIdx = GetTypeInfoPointerOf(TYPETABLEIDX_ANY_STRUCT);
		static u32 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, 0);

		s64 varargsCount = astProcCall.arguments.size - procParamCount;

		if (varargsCount == 1) {
			ASTExpression *varargsArrayExp = astProcCall.arguments[procParamCount];
			if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx) {
				IRAddComment(irContext, loc, "Forwarding varargs array"_s);

				IRValue varargsArray = IRGenFromExpression(irContext, varargsArrayExp);

				u32 paramValueIdx = astProcCall.inlineParameterValues[procParamCount];
				IRPushValueIntoStack(irContext, loc, paramValueIdx);
				IRValue param = IRValueValue(irContext, paramValueIdx);

				IRDoAssignment(irContext, loc, param, varargsArray);

				goto skipGeneratingVarargsArray;
			}
		}

		IRAddComment(irContext, loc, "Build varargs array"_s);

		IRValue pointerToBuffer;
		if (varargsCount > 0) {
			// Allocate stack space for buffer
			u32 bufferValueIdx = IRAddTempValue(irContext, loc,
					GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, varargsCount),
					VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
			IRGetLocalValue(irContext, bufferValueIdx)->name = "_varargsBuffer"_s;
#endif
			IRValue bufferIRValue = IRValueValue(irContext, bufferValueIdx);
			pointerToBuffer = IRPointerToValue(irContext, loc, bufferIRValue);

			// Fill the buffer
			int nonVarargs = (int)procParamCount;
			for (int argIdx = 0; argIdx < varargsCount; ++argIdx) {
				ASTExpression *arg = astProcCall.arguments[argIdx + nonVarargs];

				IRValue bufferIndexValue = IRValueImmediate(argIdx);
				IRValue bufferSlotValue = IRDoArrayAccess(irContext, loc,
						pointerToBuffer.valueIdx, bufferIRValue.typeTableIdx, bufferIndexValue);

				IRValue rightValue = IRGenFromExpression(irContext, arg);
				IRDoAssignment(irContext, loc, bufferSlotValue, rightValue);
			}
		}
		else
			pointerToBuffer = IRValueImmediate(0, GetTypeInfoPointerOf(anyPointerTypeIdx));

		// By now we should have the buffer with all the varargs as Any structs.
		// Now we put it into a dynamic array struct.

		// Allocate stack space for array
		u32 arrayValueIdx = IRAddTempValue(irContext, loc, arrayOfAnyTypeIdx,
				VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
		IRGetLocalValue(irContext, arrayValueIdx)->name = "_varargsArray"_s;
#endif
		IRValue arrayIRValue = IRValueValue(irContext, arrayValueIdx);
		IRValue arrayPtr = IRPointerToValue(irContext, loc, arrayIRValue);

		TypeInfo dynamicArrayTypeInfo = GetTypeInfo(TYPETABLEIDX_ARRAY_STRUCT);
		// Size
		{
			StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
			IRValue sizeMember = IRDoMemberAccess(irContext, loc, arrayPtr.valueIdx, sizeStructMember);
			IRValue sizeValue = IRValueImmediate(varargsCount);
			IRDoAssignment(irContext, loc, sizeMember, sizeValue);
		}

		// Data
		{
			StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
			IRValue dataMember = IRDoMemberAccess(irContext, loc, arrayPtr.valueIdx, dataStructMember);
			IRValue dataValue = pointerToBuffer;
			IRDoAssignment(irContext, loc, dataMember, dataValue);
		}

		// Pass array as parameter!
		u32 paramValueIdx = astProcCall.inlineParameterValues[procParamCount];
		IRPushValueIntoStack(irContext, loc, paramValueIdx);
		IRValue param = IRValueValue(irContext, paramValueIdx);

		IRDoAssignment(irContext, loc, param, arrayIRValue);
	}
skipGeneratingVarargsArray:

	// IRGen
	IRLabel *returnLabel = IRNewLabel(irContext, "inline_return"_s);
	irContext->returnLabel = returnLabel;

	ASSERT(astProcCall.astBodyInlineCopy);
	IRGenFromExpression(irContext, astProcCall.astBodyInlineCopy);

	IRInsertLabelInstruction(irContext, loc, returnLabel);

	irContext->returnLabel = oldReturnLabel;
	irContext->returnValueIndices = oldReturnValueIndices;

	return returnValue;
}

IRValue IRValueFromConstant(Constant constant)
{
	IRValue result = {};
	switch (constant.type) {
	case CONSTANTTYPE_INTEGER:
		result = IRValueImmediate(constant.valueAsInt, constant.typeTableIdx);
		break;
	case CONSTANTTYPE_FLOATING:
		result.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
		result.immediateFloat = constant.valueAsFloat;
		result.typeTableIdx = constant.typeTableIdx;
		break;
	case CONSTANTTYPE_GROUP:
	{
		result.valueType = IRVALUETYPE_TUPLE;
		result.typeTableIdx = constant.typeTableIdx;
		u64 membersCount = constant.valueAsGroup.size;
		ArrayInit(&result.tuple, membersCount);
		result.tuple.size = membersCount;
		for (int i = 0; i < membersCount; ++i)
			result.tuple[i] =
				IRValueFromConstant(constant.valueAsGroup[i]);
	} break;
	default:
		ASSERT(!"Unknown constant type");
	}
	return result;
}

void IRFillValueWithGroupLiteral(IRContext *irContext, IRValue value, ASTLiteral astLiteral)
{
	SourceLocation loc = astLiteral.loc;

	u32 groupTypeIdx = value.typeTableIdx;
	ASSERT(groupTypeIdx >= TYPETABLEIDX_Begin);
	TypeInfo groupTypeInfo = GetTypeInfo(groupTypeIdx);

	if (groupTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		groupTypeInfo.typeCategory == TYPECATEGORY_UNION)
	{
		u64 nonNamedCount = astLiteral.members.size;
		struct NamedMember {
			String name;
			ASTExpression *expr;
		};
		DynamicArray<NamedMember, ThreadAllocator> namedMembers;
		DynamicArrayInit(&namedMembers, 8);
		{
			// Save the number of non-named members, and build an array with named ones
			int i = 0;
			for (; i < astLiteral.members.size; ++i) {
				ASTExpression *literalMemberExp = astLiteral.members[i];
				if (literalMemberExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT) {
					nonNamedCount = i;
					break;
				}
			}
			for (; i < astLiteral.members.size; ++i) {
				ASTExpression *literalMemberExp = astLiteral.members[i];
				ASSERT(literalMemberExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
					   literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT);
				String name = literalMemberExp->binaryOperation.leftHand->identifier.string;
				ASTExpression *expr = literalMemberExp->binaryOperation.rightHand;
				*DynamicArrayAdd(&namedMembers) = { name, expr };
			}
		}

		struct StructStackFrame
		{
			IRValue irValue;
			u32 structTypeIdx;
			int idx;
		};
		DynamicArray<StructStackFrame, ThreadAllocator> structStack;
		DynamicArrayInit(&structStack, 8);
		*DynamicArrayAdd(&structStack) = { value, groupTypeIdx, 0 };

		int memberIdx = 0;
		while (structStack.size > 0) {
			StructStackFrame currentFrame = structStack[structStack.size - 1];
			TypeInfo currentStructTypeInfo = GetTypeInfo(currentFrame.structTypeIdx);
			IRValue innerStructPtr = IRPointerToValue(irContext, loc, currentFrame.irValue);

			if ((currentFrame.idx >= currentStructTypeInfo.structInfo.members.size) ||
				(currentStructTypeInfo.typeCategory == TYPECATEGORY_UNION && currentFrame.idx > 0)) // Only first member for unions
			{
				// Pop struct frame
				--structStack.size;
				continue;
			}

			StructMember currentMember = currentStructTypeInfo.structInfo.members[currentFrame.idx];
			TypeCategory memberTypeCat = GetTypeInfo(currentMember.typeTableIdx).typeCategory;

			if (memberTypeCat == TYPECATEGORY_STRUCT || memberTypeCat == TYPECATEGORY_UNION) {
				// Push struct frame
				++structStack[structStack.size - 1].idx;
				IRValue innerStructValue = IRDoMemberAccess(irContext, loc,
						innerStructPtr.valueIdx, currentMember);
				*DynamicArrayAdd(&structStack) = { innerStructValue, currentMember.typeTableIdx, 0 };
				continue;
			}

			IRValue memberValue = IRDoMemberAccess(irContext, loc, innerStructPtr.valueIdx, currentMember);
			IRValue src;
			if (memberIdx < nonNamedCount) {
				ASTExpression *literalMemberExp = astLiteral.members[memberIdx];
				src = IRGenFromExpression(irContext, literalMemberExp);
			}
			else {
				src = IRValueImmediate(0, currentMember.typeTableIdx); // @Check: floats

				for (int i = 0; i < namedMembers.size; ++i)
					if (StringEquals(namedMembers[i].name, currentMember.name))
						src = IRGenFromExpression(irContext, namedMembers[i].expr);
			}
			IRDoAssignment(irContext, loc, memberValue, src);

			++structStack[structStack.size - 1].idx;
			++memberIdx;
		}
	}
	else if (groupTypeInfo.typeCategory == TYPECATEGORY_ARRAY) {
		IRValue ptrToArray = IRPointerToValue(irContext, loc, value);
		for (int memberIdx = 0; memberIdx < astLiteral.members.size; ++memberIdx) {
			ASTExpression *literalMemberExp = astLiteral.members[memberIdx];

			IRValue indexIRValue = IRValueImmediate(memberIdx);
			IRValue elementValue = IRDoArrayAccess(irContext, loc, ptrToArray.valueIdx,
					value.typeTableIdx, indexIRValue);
			IRValue src = IRGenFromExpression(irContext, literalMemberExp);
			IRDoAssignment(irContext, loc, elementValue, src);
		}
	}
	else
		ASSERT(!"Invalid type to the left of group literal. Type checking should catch this");
}

void IRAssignmentFromExpression(IRContext *irContext, IRValue dstValue,
		ASTExpression *srcExpression)
{
	// We do some things here to try to avoid intermediate values
	// First, if right hand is a binary op, we assign the result directly to the left hand
	// side instead of to an intermediate value.
	if (srcExpression->nodeType == ASTNODETYPE_BINARY_OPERATION &&
		srcExpression->binaryOperation.op != TOKEN_OP_MEMBER_ACCESS &&
		srcExpression->binaryOperation.op != TOKEN_OP_ARRAY_ACCESS &&
		srcExpression->binaryOperation.op != TOKEN_OP_ASSIGNMENT)
	{
		IRInstructionFromBinaryOperation(irContext, srcExpression, dstValue);
	}
	// Second, if right hand is a group literal, fill left hand directly with it's values.
	else if (srcExpression->nodeType == ASTNODETYPE_LITERAL &&
			 srcExpression->literal.type == LITERALTYPE_GROUP)
		IRFillValueWithGroupLiteral(irContext, dstValue, srcExpression->literal);
	else {
		IRValue srcValue = IRGenFromExpression(irContext, srcExpression);
		IRDoAssignment(irContext, srcExpression->any.loc, dstValue, srcValue);
	}
}

void IRGenProcedure(IRContext *irContext, u32 procedureIdx, SourceLocation loc,
		BucketArray<Value, LinearAllocator, 256> *localValues)
{
	Procedure procedure = GetProcedureRead(procedureIdx);

	ASSERT(procedure.astBody);

	Procedure *proc = &g_context->procedures.unsafe[procedureIdx];
	proc->localValues = *localValues;
	BucketArrayInit(&proc->irInstructions);
	irContext->irInstructions = &proc->irInstructions;
	irContext->localValues = &proc->localValues;

	IRLabel *returnLabel = IRNewLabel(irContext, "return"_s);
	irContext->returnLabel = returnLabel;

	for (int i = 0; i < procedure.parameterValues.size; ++i) {
		s32 paramValueIdx = procedure.parameterValues[i];
		IRPushValueIntoStack(irContext, loc, paramValueIdx);
	}

	u64 returnValueCount = procedure.returnValueIndices.size;
	for (int i = 0; i < returnValueCount; ++i)
	{
		u32 returnValueIdx = procedure.returnValueIndices[i];
		Value returnValue = IRGetValue(irContext, returnValueIdx);
		IRPushValueIntoStack(irContext, loc, returnValueIdx);
	}

	IRGenFromExpression(irContext, procedure.astBody);

	IRInsertLabelInstruction(irContext, loc, returnLabel);

	// Return
	IRInstruction returnInst;
	returnInst.type = IRINSTRUCTIONTYPE_RETURN;
	returnInst.loc = {};
	ArrayInit(&returnInst.returnInst.returnValueIndices, irContext->returnValueIndices.size);
	returnInst.returnInst.returnValueIndices.size = irContext->returnValueIndices.size;
	for (int i = 0; i < irContext->returnValueIndices.size; ++i)
		returnInst.returnInst.returnValueIndices[i] = irContext->returnValueIndices[i];
	*IRAddInstruction(irContext) = returnInst;
}

IRValue IRGenFromExpression(IRContext *irContext, const ASTExpression *expression)
{
	IRValue result = {};

	switch (expression->nodeType) {
	case ASTNODETYPE_BLOCK:
	{
		PushIRScope(irContext);
		int currentScopeIdx = (int)irContext->irStack.size - 1;
		IRScope *currentScope = &irContext->irStack[currentScopeIdx];
		int parentScopeIdx = currentScopeIdx - 1;
		currentScope->closeLabel = IRNewLabel(irContext, "closeScope"_s);

		*IRAddInstruction(irContext) = { IRINSTRUCTIONTYPE_PUSH_SCOPE, expression->any.loc };

		for (int i = 0; i < expression->block.statements.size; ++i)
			IRGenFromExpression(irContext, &expression->block.statements[i]);

		*IRAddInstruction(irContext) = { IRINSTRUCTIONTYPE_POP_SCOPE, expression->any.loc };

		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = currentScopeIdx; stackIdx >= 0; --stackIdx) {
			if (irContext->irStack[stackIdx].deferredStatements.size) {
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo) {
			if (irContext->shouldReturnValueIdx == U32_MAX)
				irContext->shouldReturnValueIdx = IRNewValue(irContext, TYPETABLEIDX_U8, 0);

			// Set should-return register to 0
			IRValue shouldReturnRegister = IRValueValue(irContext, irContext->shouldReturnValueIdx);
			IRValue zero = IRValueImmediate(0);
			IRDoAssignment(irContext, {}, shouldReturnRegister, zero);

			// Add close label
			IRInstruction *closeScopeLabelInst = IRAddInstruction(irContext);
			closeScopeLabelInst->type  = IRINSTRUCTIONTYPE_LABEL;
			closeScopeLabelInst->loc   = {};
			closeScopeLabelInst->label = currentScope->closeLabel;

			// Run deferred statements
			for (s64 j = currentScope->deferredStatements.size - 1; j >= 0; --j)
				IRGenFromExpression(irContext, currentScope->deferredStatements[j]);

			// If should-return register is set, return
			if (parentScopeIdx > 0) {
				IRLabel *skipLabel = IRNewLabel(irContext, "skipReturn"_s);

				IRInstruction jumpIfShouldntReturnInst;
				jumpIfShouldntReturnInst.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
				jumpIfShouldntReturnInst.loc = expression->any.loc;
				jumpIfShouldntReturnInst.conditionalJump.label = skipLabel;
				jumpIfShouldntReturnInst.conditionalJump.condition = shouldReturnRegister;
				*IRAddInstruction(irContext) = jumpIfShouldntReturnInst;

				// Jump to closing of next scope with deferred statements
				IRInstruction jumpInst;
				jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
				jumpInst.loc = {};
				jumpInst.jump.label = irContext->returnLabel;
				for (int scopeIdx = parentScopeIdx; scopeIdx >= 0; --scopeIdx) {
					IRScope *scope = &irContext->irStack[scopeIdx];
					if (scope->deferredStatements.size > 0) {
						jumpInst.jump.label = scope->closeLabel;
						break;
					}
				}
				*IRAddInstruction(irContext) = jumpInst;

				IRInsertLabelInstruction(irContext, {}, skipLabel);
			}
		}

		PopIRScope(irContext);
	} break;
	case ASTNODETYPE_MULTIPLE_EXPRESSIONS:
	{
		result.valueType = IRVALUETYPE_TUPLE;
		ArrayInit(&result.tuple, expression->multipleExpressions.array.size);
		for (int i = 0; i < expression->multipleExpressions.array.size; ++i) {
			IRValue v = IRGenFromExpression(irContext, expression->multipleExpressions.array[i]);
			*ArrayAdd(&result.tuple) = v;
		}
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		auto *oldIRInstructions = irContext->irInstructions;
		auto *oldLocalValues = irContext->localValues;
		BucketArray<IRInstruction, LinearAllocator, 256> staticVarIRInstructions;
		BucketArray<Value, LinearAllocator, 256> staticVarLocalValues;
		if (varDecl.isStatic) {
			// IR gen into a separate array
			irContext->irInstructions = &staticVarIRInstructions;
			irContext->localValues = &staticVarLocalValues;
			BucketArrayInit(&staticVarIRInstructions);
			BucketArrayInit(&staticVarLocalValues);
			// Value 0 is invalid
			BucketArrayAdd(&staticVarLocalValues);
		}

		IRValue initialValue = { IRVALUETYPE_INVALID };
		if (varDecl.astInitialValue && varDecl.astInitialValue->nodeType != ASTNODETYPE_GARBAGE)
			initialValue = IRGenFromExpression(irContext, varDecl.astInitialValue);

		u64 varCount = varDecl.nameCount;
		for (int varIdx = 0; varIdx < varCount; ++varIdx) {
			u32 valueIdx = *GetVariableValueIdx(&varDecl, varIdx);
			if (varDecl.isStatic) {
				// Set up static data for variable
				u32 varValueIdx = *GetVariableValueIdx(&varDecl, varIdx);
				u32 varTypeIdx  = *GetVariableTypeIdx(&varDecl, varIdx);

				TypeInfo varTypeInfo = GetTypeInfo(varTypeIdx);
				void *staticData = AllocateStaticData(varValueIdx, varTypeInfo.size, 8);
				memset(staticData, 0, varTypeInfo.size);
				AddStaticDataPointersToRelocateInType(staticData, varTypeIdx);
			}
			else if (varDecl.isExternal) {
				ASSERT(varDecl.astInitialValue == nullptr);
				auto externalVars = g_context->irExternalVariables.GetForWrite();
				*DynamicArrayAdd(&externalVars) = valueIdx;
			}

			if (!varDecl.isExternal) {
				IRPushValueIntoStack(irContext, varDecl.loc, valueIdx);

				// Initial value
				if (varDecl.astInitialValue) {
					if (varDecl.astInitialValue->nodeType != ASTNODETYPE_GARBAGE) {
						IRValue dstValue = IRValueValue(irContext, valueIdx);
						if (initialValue.valueType == IRVALUETYPE_TUPLE)
							IRDoAssignment(irContext, varDecl.loc, dstValue, initialValue.tuple[varIdx]);
						else
							IRDoAssignment(irContext, varDecl.loc, dstValue, initialValue);
					}
				}
				else {
					// Initialize to zero
					u32 typeIdx = *GetVariableTypeIdx(&varDecl, varIdx);
					TypeInfo dstTypeInfo = GetTypeInfo(typeIdx);
					if (dstTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
						dstTypeInfo.typeCategory == TYPECATEGORY_UNION ||
						dstTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
					{
						IRValue dstValue = IRValueValue(irContext, valueIdx);
						u64 size = dstTypeInfo.size;
						IRValue sizeValue = IRValueImmediate(size);

						IRInstruction inst = {};
						inst.type = IRINSTRUCTIONTYPE_ZERO_MEMORY;
						inst.loc = varDecl.loc;
						inst.zeroMemory.dst = IRPointerToValue(irContext, varDecl.loc, dstValue);
						inst.zeroMemory.size = sizeValue;

						*IRAddInstruction(irContext) = inst;
					}
					else if (dstTypeInfo.typeCategory == TYPECATEGORY_FLOATING) {
						IRValue dstValue = IRValueValue(irContext, valueIdx);
						IRValue srcValue = IRValueImmediateFloat(0, typeIdx);
						IRDoAssignment(irContext, varDecl.loc, dstValue, srcValue);
					}
					else {
						IRValue dstValue = IRValueValue(irContext, valueIdx);
						IRValue srcValue = IRValueImmediate(0, typeIdx);
						IRDoAssignment(irContext, varDecl.loc, dstValue, srcValue);
					}
				}
			}
		}
		if (varDecl.anonymousVariableValueIdx != U32_MAX)
			IRPushValueIntoStack(irContext, varDecl.loc, varDecl.anonymousVariableValueIdx);

		if (varDecl.isStatic) {
			CTRunInstructions(*irContext->localValues, staticVarIRInstructions,
					{ IRVALUETYPE_INVALID });
			irContext->irInstructions = oldIRInstructions;
			irContext->localValues = oldLocalValues;
		}
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		switch (expression->identifier.type) {
		case NAMETYPE_STATIC_DEFINITION:
		{
			StaticDefinition staticDefinition = GetStaticDefinition(
					expression->identifier.staticDefinitionIdx);
			switch (staticDefinition.definitionType) {
			case STATICDEFINITIONTYPE_CONSTANT:
			{
				Constant constant = staticDefinition.constant;
				u32 typeTableIdx = StripAllAliases(expression->typeTableIdx);
				TypeCategory typeCat = GetTypeInfo(typeTableIdx).typeCategory;
				if (typeCat == TYPECATEGORY_FLOATING) {
					f64 f;
					if (constant.type == CONSTANTTYPE_INTEGER)
						f = (f64)constant.valueAsInt;
					else if (constant.type == CONSTANTTYPE_FLOATING)
						f = constant.valueAsFloat;
					else
						ASSERT(false);
					result = IRValueImmediateFloat(f, typeTableIdx);
				}
				else
					result = IRValueImmediate(constant.valueAsInt, typeTableIdx);
			} break;
			case STATICDEFINITIONTYPE_PROCEDURE:
			{
				result = IRValueProcedure(staticDefinition.procedureIdx);
			} break;
			default:
				ASSERT(!"Invalid static definition type found while generating IR");
			}
		} break;
		case NAMETYPE_ASTEXPRESSION:
		{
			// ASTExpression name types are used by 'using' to remember what the struct is during
			// type checking.
			result = IRGenFromExpression(irContext, expression->identifier.expression);
		} break;
		case NAMETYPE_VARIABLE:
		{
			result = IRValueValue(irContext, expression->identifier.valueIdx);
		} break;
		default:
			ASSERT(false);
		}
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		const ASTProcedureCall *astProcCall = &expression->procedureCall;
		IRInstruction procCallInst = {};
		procCallInst.loc = astProcCall->loc;
		u32 procTypeIdx;
		switch (astProcCall->callType) {
		case CALLTYPE_STATIC:
		{
			Procedure proc = GetProcedureRead(astProcCall->procedureIdx);
			if ((proc.isInline || astProcCall->inlineType == CALLINLINETYPE_ALWAYS_INLINE) &&
					astProcCall->inlineType != CALLINLINETYPE_NEVER_INLINE)
				return IRDoInlineProcedureCall(irContext, *astProcCall);

			procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
			procCallInst.procedureCall.procedureIdx = astProcCall->procedureIdx;
			procTypeIdx = proc.typeTableIdx;
		} break;
		case CALLTYPE_ASTEXPRESSION:
		{
			procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT;
			IRValue irValue = IRGenFromExpression(irContext, astProcCall->procedureExpression);
			procCallInst.procedureCall.procIRValue = irValue;
			procTypeIdx = irValue.typeTableIdx;
		} break;
		default:
			ASSERT(false);
		}

		ASSERT(GetTypeInfo(procTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = GetTypeInfo(procTypeIdx).procedureInfo;
		bool isVarargs = procTypeInfo.isVarargs;

		// Support both varargs and default parameters here
		s32 procParamCount = (s32)procTypeInfo.parameters.size;
		s32 callParamCount = (s32)astProcCall->arguments.size;
		s32 paramCount = Max(procParamCount, callParamCount) + isVarargs;
		if (paramCount)
			DynamicArrayInit(&procCallInst.procedureCall.parameters, paramCount);

		// Return value(s)
		u64 returnValueCount = procTypeInfo.returnTypeIndices.size;
		if (returnValueCount) {
			DynamicArrayInit(&procCallInst.procedureCall.returnValues, returnValueCount);
			if (returnValueCount > 1) {
				result.valueType = IRVALUETYPE_TUPLE;
				ArrayInit(&result.tuple, returnValueCount);
				for (int i = 0; i < returnValueCount; ++i) {
					u32 returnTypeIdx = procTypeInfo.returnTypeIndices[i];
					u32 returnValueIdx = IRAddTempValue(irContext, {}, returnTypeIdx, 0);
#if DEBUG_BUILD
					IRGetLocalValue(irContext, returnValueIdx)->name = "_return"_s;
#endif
					IRValue value = IRValueValue(returnValueIdx, returnTypeIdx);
					*DynamicArrayAdd(&procCallInst.procedureCall.returnValues) = value;
					*ArrayAdd(&result.tuple) = value;
				}
			}
			else {
				u32 returnTypeIdx = procTypeInfo.returnTypeIndices[0];
				u32 returnValueIdx = IRAddTempValue(irContext, {}, returnTypeIdx, 0);
#if DEBUG_BUILD
				IRGetLocalValue(irContext, returnValueIdx)->name = "_return"_s;
#endif
				IRValue value = IRValueValue(returnValueIdx, returnTypeIdx);
				*DynamicArrayAdd(&procCallInst.procedureCall.returnValues) = value;
				result = value;
			}
		}

		// Set up parameters
		s64 normalArgumentsCount = Min(callParamCount, procParamCount);
		for (int argIdx = 0; argIdx < normalArgumentsCount; ++argIdx)
		{
			const ASTExpression *arg = astProcCall->arguments[argIdx];
			u32 argTypeTableIdx = procTypeInfo.parameters[argIdx].typeTableIdx;

			IRValue param = IRGenFromExpression(irContext, arg);
			if (param.typeTableIdx != argTypeTableIdx)
				param = IRDoCast(irContext, arg->any.loc, param, argTypeTableIdx);
			*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = param;
		}

		// Default parameters
		for (u64 argIdx = astProcCall->arguments.size; argIdx < procParamCount;
				++argIdx)
		{
			ProcedureParameter procParam = procTypeInfo.parameters[argIdx];
			Constant constant = procParam.defaultValue;
			IRValue param = {};
			if (constant.type == CONSTANTTYPE_INTEGER)
				param = IRValueImmediate(constant.valueAsInt, procParam.typeTableIdx);
			else if (constant.type == CONSTANTTYPE_FLOATING)
				param = IRValueImmediateFloat(constant.valueAsFloat,
						procParam.typeTableIdx);
			else
				ASSERT(!"Invalid constant type");
			*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = param;
		}

		// Varargs
		if (isVarargs)
		{
			s64 varargsCount = astProcCall->arguments.size - procParamCount;

			static u32 anyPointerTypeIdx = GetTypeInfoPointerOf(TYPETABLEIDX_ANY_STRUCT);
			static u32 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, 0);

			if (varargsCount == 1)
			{
				const ASTExpression *varargsArrayExp = astProcCall->arguments[procParamCount];
				if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx)
				{
					IRAddComment(irContext, astProcCall->loc, "Forwarding varargs array"_s);

					IRValue varargsArray = IRGenFromExpression(irContext, varargsArrayExp);

					ASSERT(varargsArray.valueType == IRVALUETYPE_VALUE ||
						   varargsArray.valueType == IRVALUETYPE_MEMORY);
					*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = varargsArray;

					goto skipGeneratingVarargsArray;
				}
			}

			IRAddComment(irContext, astProcCall->loc, "Build varargs array"_s);

			IRValue pointerToBuffer;
			if (varargsCount > 0)
			{
				// Allocate stack space for buffer
				u32 bufferValueIdx = IRAddTempValue(irContext, {},
						GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, varargsCount),
						VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
				IRGetLocalValue(irContext, bufferValueIdx)->name = "_varargsBuffer"_s;
#endif
				IRValue bufferIRValue = IRValueValue(irContext, bufferValueIdx);
				IRValue ptrToBuffer = IRPointerToValue(irContext, astProcCall->loc, bufferIRValue);

				// Fill the buffer
				int nonVarargs = (int)procParamCount;
				for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
				{
					const ASTExpression *arg = astProcCall->arguments[argIdx + nonVarargs];

					IRValue bufferIndexValue = IRValueImmediate(argIdx);
					IRValue bufferSlotValue = IRDoArrayAccess(irContext, arg->any.loc,
							ptrToBuffer.valueIdx, bufferIRValue.typeTableIdx,
							bufferIndexValue);

					IRValue rightValue = IRGenFromExpression(irContext, arg);
					IRDoAssignment(irContext, arg->any.loc, bufferSlotValue, rightValue);
				}

				pointerToBuffer = IRPointerToValue(irContext, astProcCall->loc, bufferIRValue);
			}
			else {
				varargsCount = 0; // Can be negative
				pointerToBuffer = IRValueImmediate(0, anyPointerTypeIdx);
			}

			// By now we should have the buffer with all the varargs as Any structs.
			// Now we put it into a dynamic array struct.

			// Allocate stack space for array
			u32 arrayValueIdx = IRAddTempValue(irContext, {}, arrayOfAnyTypeIdx,
					VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
			IRGetLocalValue(irContext, arrayValueIdx)->name = "_varargsArray"_s;
#endif
			IRValue arrayIRValue = IRValueValue(irContext, arrayValueIdx);
			IRValue arrayPtr = IRPointerToValue(irContext, astProcCall->loc, arrayIRValue);

			// Size
			{
				StructMember sizeStructMember = {
					.typeTableIdx = GetTypeInfoPointerOf(TYPETABLEIDX_U8),
					.offset = 0 };
				IRValue sizeMember = IRDoMemberAccess(irContext, astProcCall->loc,
						arrayPtr.valueIdx, sizeStructMember);
				IRValue sizeValue = IRValueImmediate(varargsCount);
				IRDoAssignment(irContext, astProcCall->loc, sizeMember, sizeValue);
			}

			// Data
			{
				StructMember dataStructMember = {
					.typeTableIdx = TYPETABLEIDX_U64,
					.offset = g_pointerSize };
				IRValue dataMember = IRDoMemberAccess(irContext, astProcCall->loc,
						arrayPtr.valueIdx, dataStructMember);
				IRValue dataValue = pointerToBuffer;
				IRDoAssignment(irContext, astProcCall->loc, dataMember, dataValue);
			}

			// Pass array as parameter!
			*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = arrayIRValue;
		}

skipGeneratingVarargsArray:
		*IRAddInstruction(irContext) = procCallInst;
		break;
	}
	case ASTNODETYPE_INTRINSIC:
	{
		ASTIntrinsic astIntrinsic = expression->intrinsic;
		IRInstruction inst = { IRINSTRUCTIONTYPE_INTRINSIC, astIntrinsic.loc };
		inst.intrinsic.type = astIntrinsic.type;
		ArrayInit(&inst.intrinsic.parameters, astIntrinsic.arguments.size);

		// Set up parameters
		for (int argIdx = 0; argIdx < astIntrinsic.arguments.size; ++argIdx)
		{
			ASTExpression *arg = &astIntrinsic.arguments[argIdx];
			IRValue param = IRGenFromExpression(irContext, arg);
			*ArrayAdd(&inst.intrinsic.parameters) = param;
		}

		*IRAddInstruction(irContext) = inst;
		break;
	}
	case ASTNODETYPE_UNARY_OPERATION:
	{
		if (expression->unaryOperation.op == TOKEN_OP_POINTER_TO)
		{
			result = IRGenFromExpression(irContext, expression->unaryOperation.expression);
			result = IRPointerToValue(irContext, expression->any.loc, result);
		}
		else if (expression->unaryOperation.op == TOKEN_OP_DEREFERENCE)
		{
			result = IRGenFromExpression(irContext, expression->unaryOperation.expression);
			result = IRDereferenceValue(irContext, expression->any.loc, result);
		}
		else
		{
			IRInstruction inst = {};
			inst.loc = expression->any.loc;
			inst.unaryOperation.in  = IRGenFromExpression(irContext, expression->unaryOperation.expression);

			inst.unaryOperation.out = IRValueNewValue(irContext, "unaryop_result"_s,
					expression->typeTableIdx, 0);

			switch (expression->unaryOperation.op)
			{
			case TOKEN_OP_NOT:
			{
				inst.type = IRINSTRUCTIONTYPE_NOT;
			} break;
			case TOKEN_OP_BITWISE_NOT:
			{
				inst.type = IRINSTRUCTIONTYPE_BITWISE_NOT;
			} break;
			case TOKEN_OP_MINUS:
			{
				inst.type = IRINSTRUCTIONTYPE_SUBTRACT_UNARY;
			} break;
			default:
			{
				inst.type = IRINSTRUCTIONTYPE_INVALID;
			} break;
			}

			*IRAddInstruction(irContext) = inst;
			result = inst.unaryOperation.out;
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *rightHand = expression->binaryOperation.rightHand;
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;

		if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT) {
			IRValue srcValue = IRGenFromExpression(irContext, rightHand);
			IRValue dstValue = IRGenFromExpression(irContext, leftHand);
			IRDoAssignment(irContext, expression->any.loc, dstValue, srcValue);
			result = dstValue;
		}
		else {
			IRValue outValue = IRValueNewValue(irContext, "_binaryop_result"_s, expression->typeTableIdx, 0);
			result = IRInstructionFromBinaryOperation(irContext, expression, outValue);
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type) {
		case LITERALTYPE_INTEGER:
		{
			u32 typeTableIdx = StripAllAliases(expression->typeTableIdx);
			TypeCategory typeCat = GetTypeInfo(typeTableIdx).typeCategory;
			if (typeCat == TYPECATEGORY_FLOATING)
				result = IRValueImmediateFloat((f64)expression->literal.integer,
						typeTableIdx);
			else
				result = IRValueImmediate(expression->literal.integer, typeTableIdx);
		} break;
		case LITERALTYPE_CHARACTER:
			result = IRValueImmediate(expression->literal.character, expression->typeTableIdx);
			break;
		case LITERALTYPE_FLOATING:
		{
			u32 typeTableIdx = StripAllAliases(expression->typeTableIdx);
			result = IRValueImmediateFloat(expression->literal.floating,
					typeTableIdx);
		} break;
		case LITERALTYPE_STRING:
		{
			result = IRValueImmediateString(expression->literal.string);
		} break;
		case LITERALTYPE_GROUP:
		{
			IRValue groupIRValue = IRValueNewValue(irContext, "_groupLiteral"_s,
					expression->typeTableIdx, 0);
			IRPushValueIntoStack(irContext, expression->any.loc, groupIRValue.valueIdx);
			IRFillValueWithGroupLiteral(irContext, groupIRValue, expression->literal);
			result = groupIRValue;
		} break;
		case LITERALTYPE_CSTR:
		{
			result = IRValueImmediateCStr(irContext, expression->literal.string);
		} break;
		default:
			ASSERT(!"Unexpected literal type");
		}
		break;
	} break;
	case ASTNODETYPE_IF:
	{
		IRLabel *skipLabel = IRNewLabel(irContext, "skipIf"_s);
		IRConditionalJumpFromExpression(irContext, expression->ifNode.condition, skipLabel, false);

		// Body!
		IRGenFromExpression(irContext, expression->ifNode.body);

		IRInstruction *jumpAfterElse = nullptr;
		if (expression->ifNode.elseBody)
			// If we have an else, add a jump instruction here.
			jumpAfterElse = IRAddInstruction(irContext);

		IRInsertLabelInstruction(irContext, expression->any.loc, skipLabel);

		IRLabel *afterElseLabel = IRNewLabel(irContext, "afterElse"_s);

		if (expression->ifNode.elseBody)
		{
			jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
			jumpAfterElse->loc = expression->any.loc;
			jumpAfterElse->jump.label = afterElseLabel;

			IRGenFromExpression(irContext, expression->ifNode.elseBody);

			IRInsertLabelInstruction(irContext, expression->any.loc, afterElseLabel);
		}

	} break;
	case ASTNODETYPE_IF_STATIC:
	{
		if (expression->ifStaticNode.evaluatesToTrue)
			IRGenFromExpression(irContext, expression->ifStaticNode.body);
		else if (expression->ifStaticNode.elseBody)
			IRGenFromExpression(irContext, expression->ifStaticNode.elseBody);
	} break;
	case ASTNODETYPE_WHILE:
	{
		IRLabel *loopLabel     = IRNewLabel(irContext, "loop"_s);
		IRLabel *breakLabel    = IRNewLabel(irContext, "break"_s);
		IRInsertLabelInstruction(irContext, expression->any.loc, loopLabel);

		IRLabel *oldBreakLabel    = irContext->currentBreakLabel;
		IRLabel *oldContinueLabel = irContext->currentContinueLabel;
		irContext->currentBreakLabel    = breakLabel;
		irContext->currentContinueLabel = loopLabel;

		IRConditionalJumpFromExpression(irContext, expression->whileNode.condition, breakLabel, false);

		IRGenFromExpression(irContext, expression->whileNode.body);

		IRInstruction *loopJump = IRAddInstruction(irContext);
		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->loc = expression->any.loc;
		loopJump->jump.label = loopLabel;

		IRInsertLabelInstruction(irContext, expression->any.loc, breakLabel);

		irContext->currentBreakLabel    = oldBreakLabel;
		irContext->currentContinueLabel = oldContinueLabel;
	} break;
	case ASTNODETYPE_FOR:
	{
		PushIRScope(irContext);

		const ASTFor *astFor = &expression->forNode;

		u32 indexValueIdx = astFor->indexValueIdx;
		IRPushValueIntoStack(irContext, astFor->loc, indexValueIdx);
		IRValue indexValue = IRValueValue(irContext, indexValueIdx);

		bool isThereItVariable = false;
		u32 elementTypeIdx = TYPETABLEIDX_Unset;

		IRValue from = {}, to = {}, arrayValue = {}, ptrToArray = {};
		if (astFor->range->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			astFor->range->binaryOperation.op == TOKEN_OP_RANGE)
		{
			ASTBinaryOperation binaryOp = astFor->range->binaryOperation;

			from = IRGenFromExpression(irContext, binaryOp.leftHand);
			to =   IRGenFromExpression(irContext, binaryOp.rightHand);
			to = IRDoCast(irContext, astFor->loc, to, from.typeTableIdx);

			// Assign 'i'
			IRAddComment(irContext, astFor->loc, "Assign 'i'"_s);
			IRDoAssignment(irContext, astFor->loc, indexValue, from);
		}
		else {
			arrayValue = IRGenFromExpression(irContext, astFor->range);

			TypeInfo rangeTypeInfo = GetTypeInfo(arrayValue.typeTableIdx);
			if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER) {
				arrayValue = IRDereferenceValue(irContext, astFor->loc, arrayValue);
				rangeTypeInfo = GetTypeInfo(arrayValue.typeTableIdx);
			}

			ASSERT(arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT ||
				   rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY);

			ptrToArray = IRPointerToValue(irContext, astFor->loc, arrayValue);

			isThereItVariable = true;
			u32 elementValueIdx = astFor->elementValueIdx;
			// Allocate 'it' variable
			IRPushValueIntoStack(irContext, astFor->loc, elementValueIdx);

			elementTypeIdx = TYPETABLEIDX_U8;
			if (arrayValue.typeTableIdx != TYPETABLEIDX_STRING_STRUCT)
				elementTypeIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			u32 pointerToElementTypeTableIdx = GetTypeInfoPointerOf(elementTypeIdx);

			from = IRValueImmediate(0);
			if (rangeTypeInfo.arrayInfo.count == 0 || arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT) {
				// Compare with size member
				StructMember sizeMember = {
					.typeTableIdx = GetTypeInfoPointerOf(TYPETABLEIDX_U8),
					.offset = 0 };
				to = IRDoMemberAccess(irContext, astFor->loc, ptrToArray.valueIdx, sizeMember);
			}
			else
				to = IRValueImmediate(rangeTypeInfo.arrayInfo.count);

			// Assign 'i'
			IRAddComment(irContext, astFor->loc, "Assign 'i'"_s);
			IRDoAssignment(irContext, astFor->loc, indexValue, from);

			// Assign 'it'
			IRAddComment(irContext, astFor->loc, "Assign 'it'"_s);
			IRValue elementVarValue = IRValueValue(elementValueIdx, pointerToElementTypeTableIdx);
			IRValue elementValue = IRDoArrayAccess(irContext, astFor->loc, ptrToArray.valueIdx,
					arrayValue.typeTableIdx, indexValue);
			elementValue = IRPointerToValue(irContext, astFor->loc, elementValue);
			IRDoAssignment(irContext, astFor->loc, elementVarValue, elementValue);
		}

		IRLabel *loopLabel     = IRNewLabel(irContext, "loop"_s);
		IRLabel *breakLabel    = IRNewLabel(irContext, "break"_s);
		IRLabel *continueLabel = IRNewLabel(irContext, "continue"_s);
		IRLabel *continueSkipIncrementLabel = IRNewLabel(irContext, "continueSkipIncrement"_s);

		IRInsertLabelInstruction(irContext, astFor->loc, loopLabel);

		IRLabel *oldBreakLabel    = irContext->currentBreakLabel;
		IRLabel *oldContinueLabel = irContext->currentContinueLabel;
		irContext->currentBreakLabel    = breakLabel;
		irContext->currentContinueLabel = continueLabel;
		irContext->currentContinueSkipIncrementLabel = continueSkipIncrementLabel;

		IRInstruction *breakJump = IRAddInstruction(irContext);
		breakJump->type = IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS;
		breakJump->loc = astFor->loc;
		breakJump->conditionalJump2.label = breakLabel;
		breakJump->conditionalJump2.left  = indexValue;
		breakJump->conditionalJump2.right = to;

		IRValue oldArrayValue = irContext->irCurrentForLoopInfo.arrayValue;
		IRValue oldIndexValue = irContext->irCurrentForLoopInfo.indexValue;
		irContext->irCurrentForLoopInfo.arrayValue = arrayValue;
		irContext->irCurrentForLoopInfo.indexValue = indexValue;

		IRGenFromExpression(irContext, astFor->body);

		IRInsertLabelInstruction(irContext, astFor->loc, continueLabel);

		// Increment 'i'
		IRInstruction incrementInst = {};
		incrementInst.type = IRINSTRUCTIONTYPE_ADD;
		incrementInst.loc = astFor->loc;
		incrementInst.binaryOperation.left = indexValue;
		incrementInst.binaryOperation.right = IRValueImmediate(1);
		incrementInst.binaryOperation.out = indexValue;
		*IRAddInstruction(irContext) = incrementInst;

		if (isThereItVariable) {
			// Update 'it'
			u32 elementValueIdx = astFor->elementValueIdx;
			IRValue elementVarValue = IRValueValue(irContext, elementValueIdx);
			IRValue elementValue = IRDoArrayAccess(irContext, astFor->loc, ptrToArray.valueIdx,
					arrayValue.typeTableIdx, indexValue);
			elementValue = IRPointerToValue(irContext, astFor->loc, elementValue);
			IRDoAssignment(irContext, astFor->loc, elementVarValue, elementValue);
		}

		IRInsertLabelInstruction(irContext, astFor->loc, continueSkipIncrementLabel);

		IRInstruction *loopJump = IRAddInstruction(irContext);
		IRInsertLabelInstruction(irContext, astFor->loc, breakLabel);

		irContext->currentBreakLabel    = oldBreakLabel;
		irContext->currentContinueLabel = oldContinueLabel;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->loc = astFor->loc;
		loopJump->jump.label = loopLabel;

		irContext->irCurrentForLoopInfo.arrayValue = oldArrayValue;
		irContext->irCurrentForLoopInfo.indexValue = oldIndexValue;

		PopIRScope(irContext);
	} break;
	case ASTNODETYPE_CONTINUE:
	{
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_JUMP,
			.loc = expression->any.loc,
			.jump = { .label = irContext->currentContinueLabel }
		};
	} break;
	case ASTNODETYPE_REMOVE:
	{
		SourceLocation loc = expression->any.loc;
		IRValue arrayValue = irContext->irCurrentForLoopInfo.arrayValue;
		IRValue indexValue = irContext->irCurrentForLoopInfo.indexValue;
		IRValue sizeValue;

		TypeInfo arrayType = GetTypeInfo(arrayValue.typeTableIdx);
		if (arrayType.typeCategory == TYPECATEGORY_POINTER) {
			arrayValue = IRDereferenceValue(irContext, loc, arrayValue);
			arrayType = GetTypeInfo(arrayValue.typeTableIdx);
		}
		IRValue ptrToArray = IRPointerToValue(irContext, loc, arrayValue);

		{
			StructMember sizeMember = {
				.typeTableIdx = GetTypeInfoPointerOf(TYPETABLEIDX_U8),
				.offset = 0 };

			sizeValue = IRDoMemberAccess(irContext, loc, ptrToArray.valueIdx, sizeMember);
		}

		// Decrement size
		*IRAddInstruction(irContext) = {
			.type = IRINSTRUCTIONTYPE_SUBTRACT,
			.loc = loc,
			.binaryOperation = {
				.left = sizeValue,
				.right = IRValueImmediate(1),
				.out = sizeValue
			}
		};

		IRValue current = IRDoArrayAccess(irContext, loc, ptrToArray.valueIdx,
				arrayValue.typeTableIdx, indexValue);
		IRValue last    = IRDoArrayAccess(irContext, loc, ptrToArray.valueIdx,
				arrayValue.typeTableIdx, sizeValue);
		IRDoAssignment(irContext, loc, current, last);

		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.loc = loc;
		inst.jump.label = irContext->currentContinueSkipIncrementLabel;
		*IRAddInstruction(irContext) = inst;
	} break;
	case ASTNODETYPE_BREAK:
	{
		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.loc = expression->any.loc;
		inst.jump.label = irContext->currentBreakLabel;
		*IRAddInstruction(irContext) = inst;
	} break;
	case ASTNODETYPE_RETURN:
	{
		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = irContext->irStack.size - 1; stackIdx >= 0; --stackIdx) {
			if (irContext->irStack[stackIdx].deferredStatements.size) {
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo) {
			if (irContext->shouldReturnValueIdx == U32_MAX)
				irContext->shouldReturnValueIdx = IRNewValue(irContext, TYPETABLEIDX_U8, 0);

			// Set should return to one
			IRValue shouldReturnRegister = IRValueValue(irContext->shouldReturnValueIdx,
					TYPETABLEIDX_U8);
			IRValue one = IRValueImmediate(1);
			IRDoAssignment(irContext, expression->any.loc, shouldReturnRegister, one);
		}

		if (expression->returnNode.expression != nullptr) {
			ArrayView<ASTExpression *const> returnExps;
			if (expression->returnNode.expression->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				returnExps.size = expression->returnNode.expression->multipleExpressions.array.size;
				returnExps.data = expression->returnNode.expression->multipleExpressions.array.data;
			}
			else {
				returnExps.size = 1;
				returnExps.data = &expression->returnNode.expression;
			}

			for (int i = 0; i < returnExps.size; ++i) {
				IRValue returnValue = IRGenFromExpression(irContext, returnExps[i]);
				u32 returnTypeTableIdx = returnExps[i]->typeTableIdx;
				ASSERT(returnTypeTableIdx >= TYPETABLEIDX_Begin);

				if (IRShouldPassByCopy(returnTypeTableIdx)) {
					u64 size = GetTypeInfo(returnTypeTableIdx).size;
					IRValue sizeValue = IRValueImmediate(size);

					IRInstruction memcpyInst = {};
					memcpyInst.type = IRINSTRUCTIONTYPE_COPY_MEMORY;
					memcpyInst.copyMemory.src = IRPointerToValue(irContext, expression->any.loc, returnValue);
					memcpyInst.copyMemory.dst = IRPointerToValue(irContext, expression->any.loc,
							IRValueValue(irContext->returnValueIndices[i], returnTypeTableIdx));
					memcpyInst.copyMemory.size = sizeValue;

					*IRAddInstruction(irContext) = memcpyInst;
				}
				else {
					IRValue dst = IRValueValue(irContext->returnValueIndices[i], returnValue.typeTableIdx);
					IRDoAssignment(irContext, expression->any.loc, dst, returnValue);
				}
			}
		}

		if (isThereCleanUpToDo) {
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.loc = {};
			jumpInst.jump.label = irContext->irStack[irContext->irStack.size - 1].closeLabel;
			*IRAddInstruction(irContext) = jumpInst;
		}
		else {
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.loc = {};
			jumpInst.jump.label = irContext->returnLabel;
			*IRAddInstruction(irContext) = jumpInst;
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		IRScope *stackTop = DynamicArrayBack(&irContext->irStack);
		*DynamicArrayAdd(&stackTop->deferredStatements) = expression->deferNode.expression;
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		u32 typeTableIdx = expression->typeOfNode.expression->typeTableIdx;
		IRValue typeInfoValue = IRValueTypeOf(typeTableIdx);
		IRValue outValue = IRValueNewValue(irContext, "_typeof"_s, typeInfoValue.typeTableIdx, 0);

		IRInstruction getPtrInst = {};
		getPtrInst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;
		getPtrInst.unaryOperation.in = typeInfoValue;
		getPtrInst.unaryOperation.out = outValue;
		*IRAddInstruction(irContext) = getPtrInst;

		result = outValue;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		u32 typeTableIdx = expression->sizeOfNode.expression->typeTableIdx;
		s64 size = GetTypeInfo(typeTableIdx).size;

		result = IRValueImmediate(size, TYPETABLEIDX_U64);
	} break;
	case ASTNODETYPE_CAST:
	{
		IRValue src = IRGenFromExpression(irContext, expression->castNode.expression);
		result = IRDoCast(irContext, expression->any.loc, src, expression->typeTableIdx);
	} break;
	case ASTNODETYPE_USING:
	{
		// @Check: only for variable declarations?
		IRGenFromExpression(irContext, expression->usingNode.expression);
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	case ASTNODETYPE_INCLUDE:
	case ASTNODETYPE_LINKLIB:
	{
		// Nothing to do!
	} break;
	case ASTNODETYPE_GARBAGE:
	{
		ASSERT(!"Shouldn't attempt to generate IR from GARBAGE node");
	} break;
	case ASTNODETYPE_TYPE:
	{
		LogError(expression->any.loc, "COMPILER ERROR! Type found while generating IR."_s);
	} break;
	case ASTNODETYPE_COMPILER_BREAKPOINT:
	{
		if (expression->compilerBreakpointType == COMPILERBREAKPOINT_IR_GEN)
			BREAK;
		else if (expression->compilerBreakpointType == COMPILERBREAKPOINT_CODE_GEN) {
			IRInstruction breakInstruction = {
				.type = IRINSTRUCTIONTYPE_COMPILER_BREAKPOINT,
				.loc = expression->any.loc
			};
			*IRAddInstruction(irContext) = breakInstruction;
		}
	} break;
	default:
		ASSERT(!"Unknown ast node found type while generating IR");
	}

	return result;
}

void PrintJobIRInstructions(IRContext *irContext);
void IRGenMain()
{
	{
		auto externalVars = g_context->irExternalVariables.GetForWrite();
		DynamicArrayInit(&externalVars, 32);
	}
	{
		auto &stringLiterals = g_context->stringLiterals.unsafe;
		BucketArrayInit(&stringLiterals);
		// Empty string
		u32 globalValueIdx = NewGlobalValue("_emptystr"_s,
				GetTypeInfoPointerOf(TYPETABLEIDX_U8), VALUEFLAGS_ON_STATIC_STORAGE);
		*BucketArrayAdd(&stringLiterals) = { globalValueIdx, {} };
	}
	{
		auto &cStringLiterals = g_context->cStringLiterals.unsafe;
		BucketArrayInit(&cStringLiterals);
	}
	{
		auto &f32Literals = g_context->f32Literals.unsafe;
		BucketArrayInit(&f32Literals);
		// Empty string
		IRValueImmediateF32(0.0f);
	}
	{
		auto &f64Literals = g_context->f64Literals.unsafe;
		BucketArrayInit(&f64Literals);
		// Empty string
		IRValueImmediateF64(0.0);
	}
}

void IRJobProcedure(u32 jobIdx, void *args)
{
	IRJobArgs *argsStruct = (IRJobArgs *)args;
	u32 procedureIdx = argsStruct->procedureIdx;

	IRContext *irContext = ALLOC(LinearAllocator, IRContext);
	irContext->procedureIdx = procedureIdx;
	irContext->returnValueIndices = GetProcedureRead(procedureIdx).returnValueIndices;
	irContext->shouldReturnValueIdx = U32_MAX;

	Job *runningJob = GetCurrentJob();
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->description = SStringConcat("IR:"_s, GetProcedureRead(procedureIdx).name);
#endif

	ASSERT(GetProcedureRead(procedureIdx).astBody != nullptr);

	DynamicArrayInit(&irContext->irStack, 64);
	BucketArrayInit(&irContext->irLabels);

	IRGenProcedure(irContext, procedureIdx, {}, &argsStruct->localValues);

	{
		Procedure proc = GetProcedureRead(procedureIdx);
		proc.isIRReady = true;
		UpdateProcedure(procedureIdx, &proc);
	}
	// Wake up any jobs that were waiting for this procedure's IR
	WakeUpAllByIndex(YIELDREASON_PROC_IR_NOT_READY, procedureIdx);

	if (g_context->config.logIR)
		PrintJobIRInstructions(irContext);

	BackendJobProc(irContext, procedureIdx);

	FinishCurrentJob();
}

void IRJobExpression(u32 jobIdx, void *args)
{
	IRJobArgs *argsStruct = (IRJobArgs *)args;

	IRContext *irContext = ALLOC(LinearAllocator, IRContext);

	Job *runningJob = GetCurrentJob();
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->description = "IR:Expression"_s;
#endif

	IRGenFromExpression(irContext, argsStruct->expression);

	FinishCurrentJob();
}
