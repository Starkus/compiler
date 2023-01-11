u32 IRNewValue(Context *context, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	u64 idx = jobData->localValues->count;
	Value *result = BucketArrayAdd(jobData->localValues);
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

u32 IRNewValue(Context *context, String name, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	u64 idx = jobData->localValues->count;
	Value *result = BucketArrayAdd(jobData->localValues);
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

u32 IRNewValue(Context *context, Value value)
{
	ASSERT(value.typeTableIdx != 0);
	ASSERT(!(value.flags & VALUEFLAGS_TRY_IMMITATE) || value.tryImmitateValueIdx != U32_MAX);

	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	u64 idx = jobData->localValues->count;
	Value *result = BucketArrayAdd(jobData->localValues);
	*result = value;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

inline Value IRGetValue(Context *context, u32 valueIdx) {
	ASSERT(valueIdx > 0);
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(context, valueIdx);
	else {
		IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
		return (*jobData->localValues)[valueIdx];
	}
}

inline Value *IRGetLocalValue(Context *context, u32 valueIdx) {
	ASSERT(valueIdx > 0);
	ASSERT(!(valueIdx & VALUE_GLOBAL_BIT));
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	return &(*jobData->localValues)[valueIdx];
}

inline void IRUpdateValue(Context *context, u32 valueIdx, Value *value) {
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = context->globalValues.GetForWrite();
		globalValues[valueIdx & VALUE_GLOBAL_MASK] = *value;
	}
	else {
		IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
		(*jobData->localValues)[valueIdx] = *value;
	}
}

inline void IRSetValueFlags(Context *context, u32 valueIdx, u32 flags) {
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = context->globalValues.GetForWrite();
		globalValues[valueIdx & VALUE_GLOBAL_MASK].flags |= flags;
	}
	else {
		IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
		(*jobData->localValues)[valueIdx].flags |= flags;
	}
}

IRLabel *NewLabel(Context *context, String name) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	IRLabel result = {};

	result.name = name;
	result.instructionIdx = -1;

	IRLabel *newLabel = BucketArrayAdd(&jobData->irLabels);
	*newLabel = result;
	return newLabel;
}

void PushIRScope(Context *context) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	IRScope newScope = {};
	DynamicArrayInit(&newScope.deferredStatements, 4);
	*DynamicArrayAdd(&jobData->irStack) = newScope;
}

inline void PopIRScope(Context *context) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	ASSERT(jobData->irStack.size);
	--jobData->irStack.size;
}

inline IRInstruction *AddInstruction(Context *context) {
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	return BucketArrayAdd(jobData->irInstructions);
}

inline void IRAddComment(Context *context, SourceLocation loc, String comment)
{
	IRInstruction result = {
		.type = IRINSTRUCTIONTYPE_COMMENT,
		.loc = loc,
		.comment = comment
	};
	*AddInstruction(context) = result;
}

bool IRShouldPassByCopy(Context *context, u32 typeTableIdx)
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

inline IRValue IRValueValue(u32 valueIdx, u32 typeTableIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueValue(Context *context, u32 valueIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = IRGetValue(context, valueIdx).typeTableIdx;
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

IRValue IRValueImmediateString(Context *context, String string)
{
	IRValue result;
	result.valueType = IRVALUETYPE_IMMEDIATE_STRING;
	result.typeTableIdx = TYPETABLEIDX_Unset;
	if (string.size == 0)
		result.immediateStringIdx = 0;
	else {
		auto stringLiterals = context->stringLiterals.GetForWrite();
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
		u32 globalValueIdx = NewGlobalValue(context, SNPrintF(8, "_str%u", idx),
				GetTypeInfoPointerOf(context, TYPETABLEIDX_U8), VALUEFLAGS_ON_STATIC_STORAGE);
		String staticDataStr = CopyStringToStaticData(context, string);

		{
			ScopedLockSpin lock(&context->globalValuesLock);
			*HashMapGetOrAdd(&context->globalValueContents, globalValueIdx & VALUE_GLOBAL_MASK) =
				(u8 *)staticDataStr.data;
		}

		result.immediateStringIdx = idx;
		*BucketArrayAdd(&stringLiterals) = { globalValueIdx, staticDataStr };
	}
done:
	return result;
}

IRValue IRPointerToValue(Context *context, SourceLocation loc, IRValue in);

IRValue IRValueImmediateCStr(Context *context, String string)
{
	u32 charPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8);
	if (string.size == 0)
		return IRValueImmediate(0, charPtrTypeIdx);

	u32 globalValueIdx;
	{
		auto stringLiterals = context->cStringLiterals.GetForWrite();
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
		globalValueIdx = NewGlobalValue(context, SNPrintF(8, "_cstr%u", idx),
				charPtrTypeIdx, VALUEFLAGS_ON_STATIC_STORAGE);
		String staticDataStr = CopyStringToStaticData(context, string, true);

		{
			ScopedLockSpin lock(&context->globalValuesLock);
			*HashMapGetOrAdd(&context->globalValueContents, globalValueIdx & VALUE_GLOBAL_MASK) =
				(u8 *)staticDataStr.data;
		}

		*BucketArrayAdd(&stringLiterals) = { globalValueIdx, staticDataStr };
	}
done:
	return IRPointerToValue(context, {}, IRValueValue(globalValueIdx, charPtrTypeIdx));
}

IRValue IRValueImmediateF64(Context *context, f64 f)
{
	u32 globalValueIdx;
	{
		union { f64 in; u64 inQWord; };
		in = f;

		auto floatLiterals = context->f64Literals.GetForWrite();
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
		globalValueIdx = NewGlobalValue(context, SNPrintF(12, "_staticF64%u", idx),
				TYPETABLEIDX_F64, VALUEFLAGS_ON_STATIC_STORAGE);

		f64 *staticData = (f64 *)AllocateStaticData(context, globalValueIdx, 8, 8);
		*staticData = f;

		*BucketArrayAdd(&floatLiterals) = { .globalValueIdx = globalValueIdx, .asF64 = staticData };
	}
done:
	return IRValueValue(globalValueIdx, TYPETABLEIDX_F64);
}

IRValue IRValueImmediateF32(Context *context, f32 f)
{
	u32 globalValueIdx;
	{
		union { f32 in; u32 inDWord; };
		in = f;

		auto floatLiterals = context->f32Literals.GetForWrite();
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
		globalValueIdx = NewGlobalValue(context, SNPrintF(12, "_staticF32%u", idx),
				TYPETABLEIDX_F32, VALUEFLAGS_ON_STATIC_STORAGE);

		f32 *staticData = (f32 *)AllocateStaticData(context, globalValueIdx, 4, 4);
		*staticData = f;

		*BucketArrayAdd(&floatLiterals) = { .globalValueIdx = globalValueIdx, .asF32 = staticData };
	}
done:
	return IRValueValue(globalValueIdx, TYPETABLEIDX_F32);
}

inline IRValue IRValueImmediateFloat(Context *context, f64 f, u32 typeTableIdx)
{
	switch (typeTableIdx) {
	case TYPETABLEIDX_F32:
	case TYPETABLEIDX_FLOATING: // @Improve: maybe have type checker always collapse these?
		return IRValueImmediateF32(context, (f32)f);
	case TYPETABLEIDX_F64:
		return IRValueImmediateF64(context, f);
	default:
		ASSERT(false);
	}
}

inline IRValue IRValueProcedure(Context *context, u32 procedureIdx)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_PROCEDURE;
	result.procedureIdx = procedureIdx;
	result.typeTableIdx = GetProcedureRead(context, procedureIdx).typeTableIdx;
	return result;
}

inline IRValue IRValueNewValue(Context *context, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = 0)
{
	u32 newValueIdx = IRNewValue(context, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = newValueIdx;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueNewValue(Context *context, String name, u32 typeTableIdx, u32 flags,
		u32 immitateValueIdx = 0)
{
	u32 newValueIdx = IRNewValue(context, name, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = newValueIdx;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueTypeOf(Context *context, u32 typeTableIdx)
{
	static u32 typeInfoPointerTypeIdx = GetTypeInfoPointerOf(context,
			TYPETABLEIDX_TYPE_INFO_STRUCT);
	u32 typeValueIdx = GetTypeInfo(context, typeTableIdx).valueIdx;
	return IRValueValue(typeValueIdx, typeInfoPointerTypeIdx);
}

IRValue IRGenFromExpression(Context *context, const ASTExpression *expression);

IRValue IRDereferenceValue(Context *context, SourceLocation loc, IRValue in)
{
	TypeInfo pointerTypeInfo = GetTypeInfo(context, in.typeTableIdx);
	ASSERT(pointerTypeInfo.typeCategory == TYPECATEGORY_POINTER);
	u32 pointedTypeIdx = pointerTypeInfo.pointerInfo.pointedTypeTableIdx;

	ASSERTF(in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_MEMORY,
			"Dereferenced value must be either VALUE or MEMORY");

	if (in.valueType == IRVALUETYPE_VALUE) {
		return IRValueMemory(in.valueIdx, pointedTypeIdx);
	}
	else if (in.valueType == IRVALUETYPE_MEMORY) {
		u32 newValueIdx = IRNewValue(context, in.typeTableIdx, VALUEFLAGS_TRY_IMMITATE,
				in.mem.baseValueIdx);
		IRValue value = IRValueValue(newValueIdx, in.typeTableIdx);
#if DEBUG_BUILD
		String name = SStringConcat("_deref_"_s, IRGetValue(context, in.mem.baseValueIdx).name);
		IRGetLocalValue(context, newValueIdx)->name = name;
#endif

		*AddInstruction(context) = {
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

IRValue IRPointerToValue(Context *context, SourceLocation loc, IRValue in)
{
	ASSERT(in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_MEMORY);
	u32 pointerTypeIdx = GetTypeInfoPointerOf(context, in.typeTableIdx);

	IRValue result = IRValueNewValue(context, pointerTypeIdx, 0);
#if DEBUG_BUILD
	String name = SStringConcat("_pointerof_"_s, IRGetValue(context, in.valueIdx).name);
	IRGetLocalValue(context, result.valueIdx)->name = name;
#endif

	IRInstruction addressInst = {};
	addressInst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;
	addressInst.loc = loc;
	addressInst.unaryOperation.in = in;
	addressInst.unaryOperation.out = result;
	*AddInstruction(context) = addressInst;

	return result;
}

void IRDoAssignment(Context *context, SourceLocation loc, IRValue dstValue, IRValue srcValue);
IRValue IRDoMemberAccess(Context *context, SourceLocation loc, u32 structPtrValueIdx,
		StructMember structMember)
{
#if DEBUG_BUILD
	String structValueName = IRGetValue(context, structPtrValueIdx).name;
	IRAddComment(context, loc, SNPrintF(28 + (int)structValueName.size + (int)structMember.name.size,
				"Accessing struct member \"%S.%S\"",
				structValueName, structMember.name));
#endif

	s64 offset = structMember.offset;
	IRValue result = IRValueMemory(structPtrValueIdx, structMember.typeTableIdx,
			offset);
	result = IRPointerToValue(context, loc, result);
	result.valueType = IRVALUETYPE_MEMORY;
	result.typeTableIdx = structMember.typeTableIdx;
	return result;
}

IRValue IRDoArrayAccess(Context *context, SourceLocation loc, u32 arrayPtrValueIdx,
		u32 arrayTypeIdx, IRValue indexValue)
{
	TypeInfo arrayTypeInfo = GetTypeInfo(context, arrayTypeIdx);

	// arrayValue must be an array (or string). If it's a pointer, it should be dereferenced before
	// calling this procedure.
	ASSERT(arrayTypeInfo.typeCategory == TYPECATEGORY_ARRAY ||
		   arrayTypeIdx == TYPETABLEIDX_STRING_STRUCT);

	u32 elementTypeIdx;
	if (arrayTypeIdx == TYPETABLEIDX_STRING_STRUCT)
		elementTypeIdx = TYPETABLEIDX_U8;
	else
		elementTypeIdx = arrayTypeInfo.arrayInfo.elementTypeTableIdx;
	u32 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementTypeIdx);

	// Dynamic arrays
	if (arrayTypeIdx == TYPETABLEIDX_STRING_STRUCT || arrayTypeInfo.arrayInfo.count == 0) {
		// Access the 'data' pointer
		IRAddComment(context, loc, "Addressing dynamic array"_s);

		TypeInfo arrayStructTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);
		StructMember dataMember = arrayStructTypeInfo.structInfo.members[1];

		IRValue dataValue = IRDoMemberAccess(context, loc, arrayPtrValueIdx, dataMember);
		dataValue = IRDereferenceValue(context, loc, dataValue);
		arrayPtrValueIdx = dataValue.valueIdx;
	}

	s64 elementSize = GetTypeInfo(context, elementTypeIdx).size;

	if (indexValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER) {
		return IRValueMemory(arrayPtrValueIdx, elementTypeIdx,
				indexValue.immediate * elementSize);
	}
	else if ((indexValue.valueType == IRVALUETYPE_VALUE ||
			indexValue.valueType == IRVALUETYPE_MEMORY) &&
			CountOnes64(elementSize) == 1 && elementSize <= 8) {
		// @Todo: move x64 specifics like element size limitations and force to register to x64
		// backend.
		IRValue indexForceReg = IRValueNewValue(context, "_idx"_s, TYPETABLEIDX_S64,
				VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, indexValue.valueIdx);
		IRDoAssignment(context, loc, indexForceReg, indexValue);

		IRValue result = IRValueMemory(arrayPtrValueIdx, elementTypeIdx);
		result.mem.indexValueIdx = indexForceReg.valueIdx;
		result.mem.elementSize = elementSize;
		return result;
	}

	// Fall back to simple add instruction
	IRValue offsetValue;
	offsetValue = IRValueNewValue(context, "_array_offset"_s, TYPETABLEIDX_S64, 0);
	if (elementSize == 1)
		IRDoAssignment(context, loc, offsetValue, indexValue);
	else
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_MULTIPLY,
			.loc = loc,
			.binaryOperation = {
				.left  = indexValue,
				.right = IRValueImmediate(elementSize, TYPETABLEIDX_S64),
				.out   = offsetValue
			}
		};

	IRValue pointerToElementValue = IRValueNewValue(context, "_array_element"_s,
			pointerToElementTypeIdx, 0);
	IRValue pointerToArray = IRValueValue(arrayPtrValueIdx, pointerToElementTypeIdx);
	*AddInstruction(context) = {
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

inline void IRPushValueIntoStack(Context *context, SourceLocation loc, u32 valueIdx)
{
	*AddInstruction(context) = {
		.type = IRINSTRUCTIONTYPE_PUSH_VALUE,
		.loc = loc,
		.pushValue = { .valueIdx = valueIdx }
	};
}

inline u32 IRAddTempValue(Context *context, SourceLocation loc, u32 typeTableIdx, u8 flags)
{
	u32 valueIdx = IRNewValue(context, typeTableIdx, flags);
	IRPushValueIntoStack(context, loc, valueIdx);
	return valueIdx;
}

IRValue IRDoCast(Context *context, SourceLocation loc, IRValue srcValue, u32 typeTableIdx)
{
	ASSERT(srcValue.valueType != IRVALUETYPE_TUPLE);
	// Cast string literal to string struct
	if (srcValue.valueType == IRVALUETYPE_IMMEDIATE_STRING) {
		u32 tempValueIdx = IRAddTempValue(context, loc, typeTableIdx, 0);
#if DEBUG_BUILD
		if (srcValue.valueType == IRVALUETYPE_VALUE) {
			String name = SStringConcat("_cast_"_s, IRGetValue(context, srcValue.valueIdx).name);
			IRGetLocalValue(context, tempValueIdx)->name = name;
		}
		else
			IRGetLocalValue(context, tempValueIdx)->name = "_cast"_s;
#endif
		IRValue result = IRValueValue(tempValueIdx, typeTableIdx);

		StringLiteral literal;
		{
			auto stringLiterals = context->stringLiterals.GetForRead();
			literal = stringLiterals[srcValue.immediateStringIdx];
		}

		TypeInfo stringTypeInfo = GetTypeInfo(context, TYPETABLEIDX_STRING_STRUCT);
		IRValue dstPtr = IRPointerToValue(context, loc, result);
		IRValue sizeMember = IRDoMemberAccess(context, loc, dstPtr.valueIdx,
				stringTypeInfo.structInfo.members[0]);

		IRInstruction sizeSetInst = {
			.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
			.loc = loc,
			.assignment = {
				.src = IRValueImmediate(literal.string.size),
				.dst = sizeMember
			}
		};
		*AddInstruction(context) = sizeSetInst;

		IRValue dataMember = IRDoMemberAccess(context, loc, dstPtr.valueIdx,
				stringTypeInfo.structInfo.members[1]);

		u32 charPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8);
		srcValue.typeTableIdx = charPtrTypeIdx;
		IRInstruction dataSetInst = {
			.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
			.loc = loc,
			.assignment = {
				.src = IRValueValue(literal.globalValueIdx, charPtrTypeIdx),
				.dst = dataMember
			}
		};
		*AddInstruction(context) = dataSetInst;

		return result;
	}
	// Cast to Any
	else if (typeTableIdx == TYPETABLEIDX_ANY_STRUCT &&
		srcValue.typeTableIdx != TYPETABLEIDX_ANY_STRUCT)
	{
		IRAddComment(context, loc, "Wrapping in Any"_s);
		TypeInfo anyTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ANY_STRUCT);

		u32 tempValueIdx = IRAddTempValue(context, loc, typeTableIdx, 0);
#if DEBUG_BUILD
		if (srcValue.valueType == IRVALUETYPE_VALUE) {
			String name = SStringConcat("_any_"_s, IRGetValue(context, srcValue.valueIdx).name);
			IRGetLocalValue(context, tempValueIdx)->name = name;
		}
		else
			IRGetLocalValue(context, tempValueIdx)->name = "_any"_s;
#endif
		IRValue result = IRValueValue(tempValueIdx, typeTableIdx);

		// Access typeInfo member
		IRValue resultPtr = IRPointerToValue(context, loc, result);
		IRValue typeInfoMember = IRDoMemberAccess(context, loc, resultPtr.valueIdx,
				anyTypeInfo.structInfo.members[0]);

		// Write pointer to typeInfo to it
		IRInstruction typeAssignInst = {
			.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
			.loc = loc,
			.unaryOperation = {
				.in = IRValueTypeOf(context, srcValue.typeTableIdx),
				.out = typeInfoMember
			}
		};
		*AddInstruction(context) = typeAssignInst;

		// Access data member
		static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);
		IRValue dataMember = IRDoMemberAccess(context, loc, resultPtr.valueIdx,
				anyTypeInfo.structInfo.members[1]);
		dataMember.typeTableIdx = voidPtrTypeIdx;

		IRValue dataValue = srcValue;
		TypeInfo dataTypeInfo = GetTypeInfo(context, srcValue.typeTableIdx);

		// If data isn't in memory, copy to a variable
		if (dataTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
			dataTypeInfo.typeCategory != TYPECATEGORY_UNION &&
			dataTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
		{
			if (IRShouldPassByCopy(context, dataValue.typeTableIdx)) {
				u32 anyContentValueIdx = IRAddTempValue(context, loc, srcValue.typeTableIdx,
						VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
				IRGetLocalValue(context, anyContentValueIdx)->name = "_tempVarForAny"_s;
#endif
				IRValue tempVarIRValue = IRValueValue(anyContentValueIdx, srcValue.typeTableIdx);

				*AddInstruction(context) = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = dataValue,
						.dst = tempVarIRValue
					}
				};

				dataValue = tempVarIRValue;

				*AddInstruction(context) = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = IRPointerToValue(context, loc, dataValue),
						.dst = dataMember
					}
				};
			}
			// Small primitives are stored directly on the Any struct.
			else {
				dataMember.typeTableIdx = dataValue.typeTableIdx;

				*AddInstruction(context) = {
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
			*AddInstruction(context) = {
				.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
				.loc = loc,
				.assignment = {
					.src = IRPointerToValue(context, loc, dataValue),
					.dst = dataMember
				}
			};
		}

		return result;
	}
	else {
		TypeInfo dstTypeInfo = GetTypeInfo(context, StripAllAliases(context, typeTableIdx));
		TypeInfo srcTypeInfo = GetTypeInfo(context, StripAllAliases(context, srcValue.typeTableIdx));

		bool isSrcFloat = srcTypeInfo.typeCategory == TYPECATEGORY_FLOATING;
		bool isDstFloat =   dstTypeInfo.typeCategory == TYPECATEGORY_FLOATING;

		if (srcTypeInfo.size == dstTypeInfo.size && isSrcFloat == isDstFloat) {
			// No cast needed
			srcValue.typeTableIdx = typeTableIdx;
			return srcValue;
		}

		u32 tempValueIdx = IRAddTempValue(context, loc, typeTableIdx, 0);
#if DEBUG_BUILD
		if (srcValue.valueType == IRVALUETYPE_VALUE) {
			String name = SStringConcat("_cast_"_s, IRGetValue(context, srcValue.valueIdx).name);
			IRGetLocalValue(context, tempValueIdx)->name = name;
		}
		else
			IRGetLocalValue(context, tempValueIdx)->name = "_cast"_s;
#endif
		IRValue result = IRValueValue(tempValueIdx, typeTableIdx);

		// Cast static array to dynamic array
		if (dstTypeInfo.typeCategory  == TYPECATEGORY_ARRAY &&
			srcTypeInfo.typeCategory == TYPECATEGORY_ARRAY &&
			dstTypeInfo.arrayInfo.count  == 0 &&
			srcTypeInfo.arrayInfo.count != 0)
		{
			TypeInfo dynamicArrayTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);

			IRValue resultPtr = IRPointerToValue(context, loc, result);

			// Size
			StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
			IRValue sizeMember = IRDoMemberAccess(context, loc, resultPtr.valueIdx, sizeStructMember);
			IRValue sizeValue = IRValueImmediate(srcTypeInfo.arrayInfo.count);
			IRDoAssignment(context, loc, sizeMember, sizeValue);

			// Data
			StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
			IRValue dataMember = IRDoMemberAccess(context, loc, resultPtr.valueIdx, dataStructMember);
			IRValue dataValue = IRPointerToValue(context, loc, srcValue);
			IRDoAssignment(context, loc, dataMember, dataValue);
		}
		else if (isSrcFloat && !isDstFloat) {
			*AddInstruction(context) = {
				.type = IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT,
				.loc = loc,
				.assignment = { .src = srcValue, .dst = result }
			};
		}
		else if (!isSrcFloat && isDstFloat) {
			*AddInstruction(context) = {
				.type = IRINSTRUCTIONTYPE_CONVERT_INT_TO_FLOAT,
				.loc = loc,
				.assignment = { .src = srcValue, .dst = result }
			};
		}
		else if (isSrcFloat) {
			*AddInstruction(context) = {
				.type = IRINSTRUCTIONTYPE_CONVERT_PRECISION,
				.loc = loc,
				.assignment = { .src = srcValue, .dst = result }
			};
		}
		else if (srcTypeInfo.size < dstTypeInfo.size) {
			if (dstTypeInfo.typeCategory == TYPECATEGORY_INTEGER && dstTypeInfo.integerInfo.isSigned)
				*AddInstruction(context) = {
					.type = IRINSTRUCTIONTYPE_SIGN_EXTEND,
					.loc = loc,
					.assignment = { .src = srcValue, .dst = result }
				};
			else
				*AddInstruction(context) = {
					.type = IRINSTRUCTIONTYPE_ZERO_EXTEND,
					.loc = loc,
					.assignment = { .src = srcValue, .dst = result }
				};
		}
		else if (srcTypeInfo.size > dstTypeInfo.size) {
			*AddInstruction(context) = {
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

void IRDoAssignment(Context *context, SourceLocation loc, IRValue dstValue, IRValue srcValue)
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
			IRDoAssignment(context, loc, dstValue.tuple[i], srcValue.tuple[i]);
		return;
	}

	srcValue = IRDoCast(context, loc, srcValue, dstValue.typeTableIdx);

	dstValue.typeTableIdx = StripAllAliases(context, dstValue.typeTableIdx);
	srcValue.typeTableIdx = StripAllAliases(context, srcValue.typeTableIdx);

	TypeInfo dstTypeInfo = GetTypeInfo(context, dstValue.typeTableIdx);
	TypeInfo srcTypeInfo = GetTypeInfo(context, srcValue.typeTableIdx);

	// Copy structs/arrays
	if (srcTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		srcTypeInfo.typeCategory == TYPECATEGORY_UNION ||
		srcTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
	{
		ASSERT(dstTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			   dstTypeInfo.typeCategory == TYPECATEGORY_UNION ||
			   dstTypeInfo.typeCategory == TYPECATEGORY_ARRAY);

		u64 size = GetTypeInfo(context, srcValue.typeTableIdx).size;
		IRValue sizeValue = IRValueImmediate(size);

		IRInstruction inst = {
			.type = IRINSTRUCTIONTYPE_COPY_MEMORY,
			.loc = loc,
			.copyMemory = {
				.src = IRPointerToValue(context, loc, srcValue),
				.dst = IRPointerToValue(context, loc, dstValue),
				.size = sizeValue
			}
		};

		*AddInstruction(context) = inst;
	}
	else if (dstTypeInfo.typeCategory == TYPECATEGORY_FLOATING &&
			 srcValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		// Can't just pretend we can assign an integer immediate to a floating point value. We need
		// to convert it to a floating point number first.
		// @Check: Maybe we should do this on backend?
		IRInstruction inst = {
			.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
			.loc = loc,
			.assignment = {
				.src = IRValueImmediateFloat(context, (f64)srcValue.immediate, dstValue.typeTableIdx),
				.dst = dstValue
			}
		};

		*AddInstruction(context) = inst;

	}
	else
	{
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

		*AddInstruction(context) = inst;
	}
}

inline void IRInsertLabelInstruction(Context *context, SourceLocation loc, IRLabel *label)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	label->instructionIdx = jobData->irInstructions->count;
	*AddInstruction(context) = {
		.type = IRINSTRUCTIONTYPE_LABEL,
		.loc = loc,
		.label = label
	};
}

IRValue IRInstructionFromBinaryOperation(Context *context, const ASTExpression *expression,
		IRValue outValue)
{
	SourceLocation loc = expression->any.loc;
	IRValue result = {};

	ASTExpression *leftHand  = expression->binaryOperation.leftHand;
	ASTExpression *rightHand = expression->binaryOperation.rightHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS) {
		IRValue irValue = IRGenFromExpression(context, leftHand);

		IRValue structPtr;

		TypeInfo structTypeInfo = GetTypeInfo(context, irValue.typeTableIdx);
		if (structTypeInfo.typeCategory == TYPECATEGORY_POINTER) {
			// Dereference the pointer to the struct
			u32 newValueIdx = IRNewValue(context, irValue.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			IRValue newValue = IRValueValue(newValueIdx, irValue.typeTableIdx);

#if DEBUG_BUILD
			String valueName = IRGetValue(context, irValue.valueIdx).name; 
			IRAddComment(context, loc, SNPrintF(64, "Dereference struct pointer \"%S\"", valueName));
			String name = SStringConcat("_derefstrctptr_"_s, valueName);
			IRGetLocalValue(context, newValueIdx)->name = name;
#endif

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			inst.loc = expression->any.loc;
			inst.assignment.dst = newValue;
			inst.assignment.src = irValue;
			*AddInstruction(context) = inst;

			u32 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
			structPtr = IRValueValue(newValueIdx, pointedTypeIdx);
		}
		else
			structPtr = IRPointerToValue(context, loc, irValue);

		ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
		ASSERT(rightHand->identifier.type == NAMETYPE_STRUCT_MEMBER);
		StructMember structMember = *rightHand->identifier.structMember;

		result = IRDoMemberAccess(context, loc, structPtr.valueIdx, structMember);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS) {
		IRValue arrayValue = IRGenFromExpression(context, leftHand);
		IRValue indexValue = IRGenFromExpression(context, rightHand);

		if (GetTypeInfo(context, arrayValue.typeTableIdx).typeCategory == TYPECATEGORY_POINTER) {
			// Dereference the pointer to the array
			arrayValue = IRDereferenceValue(context, loc, arrayValue);
		}

		IRValue ptrToArray = IRPointerToValue(context, loc, arrayValue);

		ASSERT(arrayValue.valueType == IRVALUETYPE_VALUE ||
				arrayValue.valueType == IRVALUETYPE_MEMORY);
		result = IRDoArrayAccess(context, loc, ptrToArray.valueIdx, arrayValue.typeTableIdx,
				indexValue);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_AND) {
		IRLabel *assignZeroLabel = NewLabel(context, "assignZero"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		// Short-circuit jump
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = assignZeroLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		// Second jump
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = assignZeroLabel,
				.condition = rightValue
			}
		};

		IRDoAssignment(context, loc, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		// Skip-assigning-zero jump
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP,
			.loc = loc,
			.jump = { .label = skipAssignZeroLabel }
		};

		IRInsertLabelInstruction(context, loc, assignZeroLabel);

		IRDoAssignment(context, loc, outValue, IRValueImmediate(0));

		IRInsertLabelInstruction(context, loc, skipAssignZeroLabel);

		result = outValue;
	}
	else if (expression->binaryOperation.op == TOKEN_OP_OR) {
		IRLabel *assignZeroLabel = NewLabel(context, "assignZero"_s);
		IRLabel *skipRightLabel = NewLabel(context, "skipRight"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		// Short-circuit jump
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipRightLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		// Second jump
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = assignZeroLabel,
				.condition = rightValue
			}
		};

		IRInsertLabelInstruction(context, loc, skipRightLabel);

		IRDoAssignment(context, loc, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		// Skip-assign-zero jump
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP,
			.loc = loc,
			.jump = { .label = skipAssignZeroLabel }
		};

		IRInsertLabelInstruction(context, loc, assignZeroLabel);

		IRDoAssignment(context, loc, outValue, IRValueImmediate(0));

		IRInsertLabelInstruction(context, loc, skipAssignZeroLabel);

		result = outValue;
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT_AND) {
		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignZeroLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignZeroLabel,
				.condition = rightValue
			}
		};

		IRDoAssignment(context, loc, leftValue, IRValueImmediate(0));

		IRInsertLabelInstruction(context, loc, skipAssignZeroLabel);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT_OR) {
		IRLabel *skipAssignOneLabel = NewLabel(context, "skipAssignOne"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignOneLabel,
				.condition = leftValue
			}
		};

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
			.loc = loc,
			.conditionalJump = {
				.label = skipAssignOneLabel,
				.condition = rightValue
			}
		};

		IRDoAssignment(context, loc, leftValue, IRValueImmediate(1));

		IRInsertLabelInstruction(context, loc, skipAssignOneLabel);
	}
	else {
#if DEBUG_BUILD
		TypeInfo leftTypeInfo = GetTypeInfo(context, leftHand->typeTableIdx);
		ASSERT(leftTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
			   leftTypeInfo.typeCategory != TYPECATEGORY_UNION);
#endif

		IRValue left  = IRGenFromExpression(context, leftHand);
		IRValue right = IRGenFromExpression(context, rightHand);

		if (left.typeTableIdx != right.typeTableIdx)
			right = IRDoCast(context, loc, right, left.typeTableIdx);

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
			LogError(context, expression->any.loc, "Range operator used in invalid context"_s);
		} break;
		default:
		{
			LogError(context, expression->any.loc, "Binary operator unrecognized during IR generation"_s);
		} break;
		}

		IRValue out = outValue;
		bool castOutput = out.typeTableIdx != expression->typeTableIdx;
		if (castOutput) {
			u32 tempValueIdx = IRAddTempValue(context, loc, expression->typeTableIdx, 0);
#if DEBUG_BUILD
			IRGetLocalValue(context, tempValueIdx)->name = "_binaryop_cast"_s;
#endif
			out = IRValueValue(tempValueIdx, expression->typeTableIdx);
		}

		// Hint for register allocation to try and allocate left and out in the same register
		if (out.valueType == IRVALUETYPE_VALUE &&
			left.valueType == IRVALUETYPE_VALUE)
		{
			Value v = IRGetValue(context, left.valueIdx);
			v.flags |= VALUEFLAGS_TRY_IMMITATE;
			v.tryImmitateValueIdx = out.valueIdx;
			IRUpdateValue(context, left.valueIdx, &v);
		}

		inst.binaryOperation.out = out;
		*AddInstruction(context) = inst;

		if (castOutput)
			IRDoAssignment(context, loc, outValue, out);

		if (expression->binaryOperation.op >= TOKEN_OP_ASSIGNMENT_Begin &&
			expression->binaryOperation.op <= TOKEN_OP_ASSIGNMENT_End)
		{
			IRDoAssignment(context, loc, left, outValue);
			result = left;
		}
		else
			result = outValue;
	}

	return result;
}

void IRConditionalJumpFromExpression(Context *context, ASTExpression *conditionExp, IRLabel *label, bool jumpIfTrue)
{
	// The following tries to avoid saving condition to a bool, then comparing the bool with
	// 0 in the conditional jump.
	if (conditionExp->nodeType == ASTNODETYPE_UNARY_OPERATION &&
		conditionExp->binaryOperation.op == TOKEN_OP_NOT)
	{
		IRConditionalJumpFromExpression(context, conditionExp->unaryOperation.expression, label, !jumpIfTrue);
		return;
	}

	if (conditionExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
		conditionExp->binaryOperation.op == (jumpIfTrue ? TOKEN_OP_OR : TOKEN_OP_AND))
	{
		IRConditionalJumpFromExpression(context, conditionExp->binaryOperation.leftHand, label, jumpIfTrue);
		IRConditionalJumpFromExpression(context, conditionExp->binaryOperation.rightHand, label, jumpIfTrue);
		return;
	}
	else if (conditionExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			 conditionExp->binaryOperation.op == (jumpIfTrue ? TOKEN_OP_AND : TOKEN_OP_OR))
	{
		IRLabel *skipRightHandLabel = NewLabel(context, "skipRightHand"_s);

		IRConditionalJumpFromExpression(context, conditionExp->binaryOperation.leftHand, skipRightHandLabel, !jumpIfTrue);
		IRConditionalJumpFromExpression(context, conditionExp->binaryOperation.rightHand, label, jumpIfTrue);

		IRInsertLabelInstruction(context, conditionExp->any.loc, skipRightHandLabel);
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

		IRValue leftResult  = IRGenFromExpression(context,
				conditionExp->binaryOperation.leftHand);
		IRValue rightResult = IRGenFromExpression(context,
				conditionExp->binaryOperation.rightHand);

		if (leftResult.typeTableIdx != rightResult.typeTableIdx)
			rightResult = IRDoCast(context, conditionExp->any.loc, rightResult, leftResult.typeTableIdx);

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
		*AddInstruction(context) = jump;
		return;
	}

defaultConditionEvaluation:
	{
		// Fallback path. Just save the condition to a bool, then evaluate that bool.
		IRValue conditionResult = IRGenFromExpression(context, conditionExp);

		if (conditionResult.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			if (conditionResult.immediate == 0)
				goto insertSimpleJump;
			else return;
		}

		IRInstruction *jump = AddInstruction(context);
		jump->type = jumpIfTrue ? IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO : IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->loc = conditionExp->any.loc;
		jump->conditionalJump.label = label;
		jump->conditionalJump.condition = conditionResult;
		return;
	}
insertSimpleJump:
	{
		IRInstruction *jump = AddInstruction(context);
		jump->type = IRINSTRUCTIONTYPE_JUMP;
		jump->jump.label = label;
		return;
	}
}

IRValue IRDoInlineProcedureCall(Context *context, ASTProcedureCall astProcCall)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	SourceLocation loc = astProcCall.loc;

	ASSERT(astProcCall.callType == CALLTYPE_STATIC);
	Procedure procedure = GetProcedureRead(context, astProcCall.procedureIdx);

	IRLabel *oldReturnLabel = jobData->returnLabel;
	ArrayView<u32> oldReturnValueIndices = jobData->returnValueIndices;

	TypeInfoProcedure procTypeInfo = GetTypeInfo(context, procedure.typeTableIdx).procedureInfo;
	u64 returnValueCount = procTypeInfo.returnTypeIndices.size;

	Array<u32, ThreadAllocator> inlineReturnValues;
	ArrayInit(&inlineReturnValues, returnValueCount);
	for (int i = 0; i < returnValueCount; ++i)
	{
		IRValue returnValue = IRValueNewValue(context, "_inline_return"_s,
				procTypeInfo.returnTypeIndices[i], 0);
		IRPushValueIntoStack(context, loc, returnValue.valueIdx);
		*ArrayAdd(&inlineReturnValues) = returnValue.valueIdx;
	}
	bool isVarargs = procTypeInfo.isVarargs;

	IRValue returnValue = {};
	if (returnValueCount == 1)
		returnValue = IRValueValue(inlineReturnValues[0], procTypeInfo.returnTypeIndices[0]);
	else if (returnValueCount > 1)
	{
		returnValue.valueType = IRVALUETYPE_TUPLE;
		ArrayInit(&returnValue.tuple, returnValueCount);
		for (int i = 0; i < returnValueCount; ++i)
			*ArrayAdd(&returnValue.tuple) =
				IRValueValue(inlineReturnValues[i], procTypeInfo.returnTypeIndices[i]);
	}

	jobData->returnValueIndices = inlineReturnValues;

	// Support both varargs and default parameters here
	s32 procParamCount = (s32)procTypeInfo.parameters.size;
	s32 callParamCount = (s32)astProcCall.arguments.size;

	// Set up parameters
	s64 normalArgumentsCount = Min(callParamCount, procParamCount);
	for (int argIdx = 0; argIdx < normalArgumentsCount; ++argIdx)
	{
		ASTExpression *arg = astProcCall.arguments[argIdx];
		IRValue argValue = IRGenFromExpression(context, arg);

		u32 paramValueIdx = astProcCall.inlineParameterValues[argIdx];
		IRPushValueIntoStack(context, loc, paramValueIdx);
		IRValue param = IRValueValue(context, paramValueIdx);

		IRDoAssignment(context, loc, param, argValue);
	}

	// Default parameters
	for (u64 argIdx = astProcCall.arguments.size; argIdx < procParamCount;
			++argIdx)
	{
		ProcedureParameter procParam = procTypeInfo.parameters[argIdx];
		Constant constant = procParam.defaultValue;
		IRValue arg = {};
		if (constant.type == CONSTANTTYPE_INTEGER)
			arg = IRValueImmediate(constant.valueAsInt, procParam.typeTableIdx);
		else if (constant.type == CONSTANTTYPE_FLOATING)
			arg = IRValueImmediateFloat(context, constant.valueAsFloat,
					procParam.typeTableIdx);
		else
			ASSERT(!"Invalid constant type");

		u32 paramValueIdx = astProcCall.inlineParameterValues[argIdx];
		IRPushValueIntoStack(context, loc, paramValueIdx);
		IRValue param = IRValueValue(context, paramValueIdx);

		IRDoAssignment(context, loc, param, arg);
	}

	// Varargs
	if (isVarargs)
	{
		static u32 anyPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_ANY_STRUCT);
		static u32 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

		s64 varargsCount = astProcCall.arguments.size - procParamCount;

		if (varargsCount == 1)
		{
			ASTExpression *varargsArrayExp = astProcCall.arguments[procParamCount];
			if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx)
			{
				IRAddComment(context, loc, "Forwarding varargs array"_s);

				IRValue varargsArray = IRGenFromExpression(context, varargsArrayExp);

				u32 paramValueIdx = astProcCall.inlineParameterValues[procParamCount];
				IRPushValueIntoStack(context, loc, paramValueIdx);
				IRValue param = IRValueValue(context, paramValueIdx);

				IRDoAssignment(context, loc, param, varargsArray);

				goto skipGeneratingVarargsArray;
			}
		}

		IRAddComment(context, loc, "Build varargs array"_s);

		IRValue pointerToBuffer;
		if (varargsCount > 0)
		{
			// Allocate stack space for buffer
			u32 bufferValueIdx = IRAddTempValue(context, loc,
					GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, varargsCount),
					VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
			IRGetLocalValue(context, bufferValueIdx)->name = "_varargsBuffer"_s;
#endif
			IRValue bufferIRValue = IRValueValue(context, bufferValueIdx);
			pointerToBuffer = IRPointerToValue(context, loc, bufferIRValue);

			// Fill the buffer
			int nonVarargs = (int)procParamCount;
			for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
			{
				ASTExpression *arg = astProcCall.arguments[argIdx + nonVarargs];

				IRValue bufferIndexValue = IRValueImmediate(argIdx);
				IRValue bufferSlotValue = IRDoArrayAccess(context, loc,
						pointerToBuffer.valueIdx, bufferIRValue.typeTableIdx, bufferIndexValue);

				IRValue rightValue = IRGenFromExpression(context, arg);
				IRDoAssignment(context, loc, bufferSlotValue, rightValue);
			}
		}
		else
			pointerToBuffer = IRValueImmediate(0, GetTypeInfoPointerOf(context, anyPointerTypeIdx));

		// By now we should have the buffer with all the varargs as Any structs.
		// Now we put it into a dynamic array struct.

		// Allocate stack space for array
		u32 arrayValueIdx = IRAddTempValue(context, loc, arrayOfAnyTypeIdx,
				VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
		IRGetLocalValue(context, arrayValueIdx)->name = "_varargsArray"_s;
#endif
		IRValue arrayIRValue = IRValueValue(context, arrayValueIdx);
		IRValue arrayPtr = IRPointerToValue(context, loc, arrayIRValue);

		TypeInfo dynamicArrayTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);
		// Size
		{
			StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
			IRValue sizeMember = IRDoMemberAccess(context, loc, arrayPtr.valueIdx, sizeStructMember);
			IRValue sizeValue = IRValueImmediate(varargsCount);
			IRDoAssignment(context, loc, sizeMember, sizeValue);
		}

		// Data
		{
			StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
			IRValue dataMember = IRDoMemberAccess(context, loc, arrayPtr.valueIdx, dataStructMember);
			IRValue dataValue = pointerToBuffer;
			IRDoAssignment(context, loc, dataMember, dataValue);
		}

		// Pass array as parameter!
		u32 paramValueIdx = astProcCall.inlineParameterValues[procParamCount];
		IRPushValueIntoStack(context, loc, paramValueIdx);
		IRValue param = IRValueValue(context, paramValueIdx);

		IRDoAssignment(context, loc, param, arrayIRValue);
	}
skipGeneratingVarargsArray:

	// IRGen
	IRLabel *returnLabel = NewLabel(context, "inline_return"_s);
	jobData->returnLabel = returnLabel;

	ASSERT(astProcCall.astBodyInlineCopy);
	IRGenFromExpression(context, astProcCall.astBodyInlineCopy);

	IRInsertLabelInstruction(context, loc, returnLabel);

	jobData->returnLabel = oldReturnLabel;
	jobData->returnValueIndices = oldReturnValueIndices;

	return returnValue;
}

IRValue IRValueFromConstant(Context *context, Constant constant)
{
	IRValue result = {};
	switch (constant.type)
	{
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
				IRValueFromConstant(context, constant.valueAsGroup[i]);
	} break;
	default:
		ASSERT(!"Unknown constant type");
	}
	return result;
}

void IRFillValueWithGroupLiteral(Context *context, IRValue value, ASTLiteral astLiteral)
{
	SourceLocation loc = astLiteral.loc;

	u32 groupTypeIdx = value.typeTableIdx;
	ASSERT(groupTypeIdx >= TYPETABLEIDX_Begin);
	TypeInfo groupTypeInfo = GetTypeInfo(context, groupTypeIdx);

	if (groupTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		groupTypeInfo.typeCategory == TYPECATEGORY_UNION)
	{
		u64 nonNamedCount = astLiteral.members.size;
		struct NamedMember
		{
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
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
				{
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
		while (structStack.size > 0)
		{
			StructStackFrame currentFrame = structStack[structStack.size - 1];
			TypeInfo currentStructTypeInfo = GetTypeInfo(context, currentFrame.structTypeIdx);
			IRValue innerStructPtr = IRPointerToValue(context, loc, currentFrame.irValue);

			if ((currentFrame.idx >= currentStructTypeInfo.structInfo.members.size) ||
				(currentStructTypeInfo.typeCategory == TYPECATEGORY_UNION && currentFrame.idx > 0)) // Only first member for unions
			{
				// Pop struct frame
				--structStack.size;
				continue;
			}

			StructMember currentMember = currentStructTypeInfo.structInfo.members[currentFrame.idx];
			TypeCategory memberTypeCat = GetTypeInfo(context, currentMember.typeTableIdx).typeCategory;

			if (memberTypeCat == TYPECATEGORY_STRUCT || memberTypeCat == TYPECATEGORY_UNION)
			{
				// Push struct frame
				++structStack[structStack.size - 1].idx;
				IRValue innerStructValue = IRDoMemberAccess(context, loc,
						innerStructPtr.valueIdx, currentMember);
				*DynamicArrayAdd(&structStack) = { innerStructValue, currentMember.typeTableIdx, 0 };
				continue;
			}

			IRValue memberValue = IRDoMemberAccess(context, loc, innerStructPtr.valueIdx, currentMember);
			IRValue src;
			if (memberIdx < nonNamedCount)
			{
				ASTExpression *literalMemberExp = astLiteral.members[memberIdx];
				src = IRGenFromExpression(context, literalMemberExp);
			}
			else
			{
				src = IRValueImmediate(0, currentMember.typeTableIdx); // @Check: floats

				for (int i = 0; i < namedMembers.size; ++i)
					if (StringEquals(namedMembers[i].name, currentMember.name))
						src = IRGenFromExpression(context, namedMembers[i].expr);
			}
			IRDoAssignment(context, loc, memberValue, src);

			++structStack[structStack.size - 1].idx;
			++memberIdx;
		}
	}
	else if (groupTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
	{
		IRValue ptrToArray = IRPointerToValue(context, loc, value);
		for (int memberIdx = 0; memberIdx < astLiteral.members.size; ++memberIdx)
		{
			ASTExpression *literalMemberExp = astLiteral.members[memberIdx];

			IRValue indexIRValue = IRValueImmediate(memberIdx);
			IRValue elementValue = IRDoArrayAccess(context, loc, ptrToArray.valueIdx,
					value.typeTableIdx, indexIRValue);
			IRValue src = IRGenFromExpression(context, literalMemberExp);
			IRDoAssignment(context, loc, elementValue, src);
		}
	}
	else
		ASSERT(!"Invalid type to the left of group literal. Type checking should catch this");
}

void IRAssignmentFromExpression(Context *context, IRValue dstValue,
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
		IRInstructionFromBinaryOperation(context, srcExpression, dstValue);
	}
	// Second, if right hand is a group literal, fill left hand directly with it's values.
	else if (srcExpression->nodeType == ASTNODETYPE_LITERAL &&
			 srcExpression->literal.type == LITERALTYPE_GROUP)
		IRFillValueWithGroupLiteral(context, dstValue, srcExpression->literal);
	else {
		IRValue srcValue = IRGenFromExpression(context, srcExpression);
		IRDoAssignment(context, srcExpression->any.loc, dstValue, srcValue);
	}
}

void IRGenProcedure(Context *context, u32 procedureIdx, SourceLocation loc,
		BucketArray<Value, LinearAllocator, 256> *localValues)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	Procedure procedure = GetProcedureRead(context, procedureIdx);

	ASSERT(procedure.astBody);

	Procedure *proc = &context->procedures.unsafe[procedureIdx];
	proc->localValues = *localValues;
	BucketArrayInit(&proc->irInstructions);
	jobData->irInstructions = &proc->irInstructions;
	jobData->localValues = &proc->localValues;

	IRLabel *returnLabel = NewLabel(context, "return"_s);
	jobData->returnLabel = returnLabel;

	for (int i = 0; i < procedure.parameterValues.size; ++i)
	{
		s32 paramValueIdx = procedure.parameterValues[i];
		IRPushValueIntoStack(context, loc, paramValueIdx);
	}

	u64 returnValueCount = procedure.returnValueIndices.size;
	for (int i = 0; i < returnValueCount; ++i)
	{
		u32 returnValueIdx = procedure.returnValueIndices[i];
		Value returnValue = IRGetValue(context, returnValueIdx);
		IRPushValueIntoStack(context, loc, returnValueIdx);
	}

	IRGenFromExpression(context, procedure.astBody);

	IRInsertLabelInstruction(context, loc, returnLabel);

	// Return
	IRInstruction returnInst;
	returnInst.type = IRINSTRUCTIONTYPE_RETURN;
	returnInst.loc = {};
	ArrayInit(&returnInst.returnInst.returnValueIndices, jobData->returnValueIndices.size);
	returnInst.returnInst.returnValueIndices.size = jobData->returnValueIndices.size;
	for (int i = 0; i < jobData->returnValueIndices.size; ++i)
		returnInst.returnInst.returnValueIndices[i] = jobData->returnValueIndices[i];
	*AddInstruction(context) = returnInst;
}

IRValue IRGenFromExpression(Context *context, const ASTExpression *expression)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	IRValue result = {};

	switch (expression->nodeType)
	{
	case ASTNODETYPE_BLOCK:
	{
		PushIRScope(context);
		int currentScopeIdx = (int)jobData->irStack.size - 1;
		IRScope *currentScope = &jobData->irStack[currentScopeIdx];
		int parentScopeIdx = currentScopeIdx - 1;
		currentScope->closeLabel = NewLabel(context, "closeScope"_s);

		*AddInstruction(context) = { IRINSTRUCTIONTYPE_PUSH_SCOPE, expression->any.loc };

		for (int i = 0; i < expression->block.statements.size; ++i)
			IRGenFromExpression(context, &expression->block.statements[i]);

		*AddInstruction(context) = { IRINSTRUCTIONTYPE_POP_SCOPE, expression->any.loc };

		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = currentScopeIdx; stackIdx >= 0; --stackIdx)
		{
			if (jobData->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			if (jobData->shouldReturnValueIdx == U32_MAX)
				jobData->shouldReturnValueIdx = IRNewValue(context, TYPETABLEIDX_U8, 0);

			// Set should-return register to 0
			IRValue shouldReturnRegister = IRValueValue(context, jobData->shouldReturnValueIdx);
			IRValue zero = IRValueImmediate(0);
			IRDoAssignment(context, {}, shouldReturnRegister, zero);

			// Add close label
			IRInstruction *closeScopeLabelInst = AddInstruction(context);
			closeScopeLabelInst->type  = IRINSTRUCTIONTYPE_LABEL;
			closeScopeLabelInst->loc   = {};
			closeScopeLabelInst->label = currentScope->closeLabel;

			// Run deferred statements
			for (s64 j = currentScope->deferredStatements.size - 1; j >= 0; --j)
			{
				IRGenFromExpression(context, currentScope->deferredStatements[j]);
			}

			// If should-return register is set, return
			if (parentScopeIdx > 0)
			{
				IRLabel *skipLabel = NewLabel(context, "skipReturn"_s);

				IRInstruction jumpIfShouldntReturnInst;
				jumpIfShouldntReturnInst.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
				jumpIfShouldntReturnInst.loc = expression->any.loc;
				jumpIfShouldntReturnInst.conditionalJump.label = skipLabel;
				jumpIfShouldntReturnInst.conditionalJump.condition = shouldReturnRegister;
				*AddInstruction(context) = jumpIfShouldntReturnInst;

				// Jump to closing of next scope with deferred statements
				IRInstruction jumpInst;
				jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
				jumpInst.loc = {};
				jumpInst.jump.label = jobData->returnLabel;
				for (int scopeIdx = parentScopeIdx; scopeIdx >= 0; --scopeIdx)
				{
					IRScope *scope = &jobData->irStack[scopeIdx];
					if (scope->deferredStatements.size > 0)
					{
						jumpInst.jump.label = scope->closeLabel;
						break;
					}
				}
				*AddInstruction(context) = jumpInst;

				IRInsertLabelInstruction(context, {}, skipLabel);
			}
		}

		PopIRScope(context);
	} break;
	case ASTNODETYPE_MULTIPLE_EXPRESSIONS:
	{
		result.valueType = IRVALUETYPE_TUPLE;
		ArrayInit(&result.tuple, expression->multipleExpressions.array.size);
		for (int i = 0; i < expression->multipleExpressions.array.size; ++i)
		{
			IRValue v = IRGenFromExpression(context, expression->multipleExpressions.array[i]);
			*ArrayAdd(&result.tuple) = v;
		}
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		auto *oldIRInstructions = jobData->irInstructions;
		auto *oldLocalValues = jobData->localValues;
		BucketArray<IRInstruction, LinearAllocator, 256> staticVarIRInstructions;
		BucketArray<Value, LinearAllocator, 256> staticVarLocalValues;
		if (varDecl.isStatic) {
			// IR gen into a separate array
			jobData->irInstructions = &staticVarIRInstructions;
			jobData->localValues = &staticVarLocalValues;
			BucketArrayInit(&staticVarIRInstructions);
			BucketArrayInit(&staticVarLocalValues);
			// Value 0 is invalid
			BucketArrayAdd(&staticVarLocalValues);
		}

		IRValue initialValue = { IRVALUETYPE_INVALID };
		if (varDecl.astInitialValue && varDecl.astInitialValue->nodeType != ASTNODETYPE_GARBAGE)
			initialValue = IRGenFromExpression(context, varDecl.astInitialValue);

		u64 varCount = varDecl.nameCount;
		for (int varIdx = 0; varIdx < varCount; ++varIdx) {
			u32 valueIdx = *GetVariableValueIdx(&varDecl, varIdx);
			if (varDecl.isStatic) {
				// Set up static data for variable
				u32 varValueIdx = *GetVariableValueIdx(&varDecl, varIdx);
				u32 varTypeIdx  = *GetVariableTypeIdx(&varDecl, varIdx);

				TypeInfo varTypeInfo = TCGetTypeInfo(context, varTypeIdx);
				void *staticData = AllocateStaticData(context, varValueIdx, varTypeInfo.size, 8);
				memset(staticData, 0, varTypeInfo.size);
				AddStaticDataPointersToRelocateInType(context, staticData, varTypeIdx);
			}
			else if (varDecl.isExternal) {
				ASSERT(varDecl.astInitialValue == nullptr);
				auto externalVars = context->irExternalVariables.GetForWrite();
				*DynamicArrayAdd(&externalVars) = valueIdx;
			}

			if (!varDecl.isExternal) {
				IRPushValueIntoStack(context, varDecl.loc, valueIdx);

				// Initial value
				if (varDecl.astInitialValue) {
					if (varDecl.astInitialValue->nodeType != ASTNODETYPE_GARBAGE) {
						IRValue dstValue = IRValueValue(context, valueIdx);
						if (initialValue.valueType == IRVALUETYPE_TUPLE)
							IRDoAssignment(context, varDecl.loc, dstValue, initialValue.tuple[varIdx]);
						else
							IRDoAssignment(context, varDecl.loc, dstValue, initialValue);
					}
				}
				else {
					// Initialize to zero
					u32 typeIdx = *GetVariableTypeIdx(&varDecl, varIdx);
					TypeInfo dstTypeInfo = GetTypeInfo(context, typeIdx);
					if (dstTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
						dstTypeInfo.typeCategory == TYPECATEGORY_UNION ||
						dstTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
					{
						IRValue dstValue = IRValueValue(context, valueIdx);
						u64 size = dstTypeInfo.size;
						IRValue sizeValue = IRValueImmediate(size);

						IRInstruction inst = {};
						inst.type = IRINSTRUCTIONTYPE_ZERO_MEMORY;
						inst.loc = varDecl.loc;
						inst.zeroMemory.dst = IRPointerToValue(context, varDecl.loc, dstValue);
						inst.zeroMemory.size = sizeValue;

						*AddInstruction(context) = inst;
					}
					else if (dstTypeInfo.typeCategory == TYPECATEGORY_FLOATING) {
						IRValue dstValue = IRValueValue(context, valueIdx);
						IRValue srcValue = IRValueImmediateFloat(context, 0, typeIdx);
						IRDoAssignment(context, varDecl.loc, dstValue, srcValue);
					}
					else {
						IRValue dstValue = IRValueValue(context, valueIdx);
						IRValue srcValue = IRValueImmediate(0, typeIdx);
						IRDoAssignment(context, varDecl.loc, dstValue, srcValue);
					}
				}
			}
		}
		if (varDecl.anonymousVariableValueIdx != U32_MAX)
			IRPushValueIntoStack(context, varDecl.loc, varDecl.anonymousVariableValueIdx);

		if (varDecl.isStatic) {
			CTRunInstructions(context, *jobData->localValues, staticVarIRInstructions,
					{ IRVALUETYPE_INVALID });
			jobData->irInstructions = oldIRInstructions;
			jobData->localValues = oldLocalValues;
		}
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		switch (expression->identifier.type)
		{
		case NAMETYPE_STATIC_DEFINITION:
		{
			StaticDefinition staticDefinition = GetStaticDefinition(context,
					expression->identifier.staticDefinitionIdx);
			switch (staticDefinition.definitionType)
			{
			case STATICDEFINITIONTYPE_CONSTANT:
			{
				Constant constant = staticDefinition.constant;
				u32 typeTableIdx = StripAllAliases(context, expression->typeTableIdx);
				TypeCategory typeCat = GetTypeInfo(context, typeTableIdx).typeCategory;
				if (typeCat == TYPECATEGORY_FLOATING)
				{
					f64 f;
					if (constant.type == CONSTANTTYPE_INTEGER)
						f = (f64)constant.valueAsInt;
					else if (constant.type == CONSTANTTYPE_FLOATING)
						f = constant.valueAsFloat;
					else
						ASSERT(false);
					result = IRValueImmediateFloat(context, f, typeTableIdx);
				}
				else
					result = IRValueImmediate(constant.valueAsInt, typeTableIdx);
			} break;
			case STATICDEFINITIONTYPE_PROCEDURE:
			{
				result = IRValueProcedure(context, staticDefinition.procedureIdx);
			} break;
			default:
				ASSERT(!"Invalid static definition type found while generating IR");
			}
		} break;
		case NAMETYPE_ASTEXPRESSION:
		{
			// ASTExpression name types are used by 'using' to remember what the struct is during
			// type checking.
			result = IRGenFromExpression(context, expression->identifier.expression);
		} break;
		case NAMETYPE_VARIABLE:
		{
			result = IRValueValue(context, expression->identifier.valueIdx);
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
		switch (astProcCall->callType)
		{
		case CALLTYPE_STATIC:
		{
			Procedure proc = GetProcedureRead(context, astProcCall->procedureIdx);
			if ((proc.isInline || astProcCall->inlineType == CALLINLINETYPE_ALWAYS_INLINE) &&
					astProcCall->inlineType != CALLINLINETYPE_NEVER_INLINE)
				return IRDoInlineProcedureCall(context, *astProcCall);

			procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
			procCallInst.procedureCall.procedureIdx = astProcCall->procedureIdx;
			procTypeIdx = proc.typeTableIdx;
		} break;
		case CALLTYPE_ASTEXPRESSION:
		{
			procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT;
			IRValue irValue = IRGenFromExpression(context, astProcCall->procedureExpression);
			procCallInst.procedureCall.procIRValue = irValue;
			procTypeIdx = irValue.typeTableIdx;
		} break;
		default:
			ASSERT(false);
		}

		ASSERT(GetTypeInfo(context, procTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = GetTypeInfo(context, procTypeIdx).procedureInfo;
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
					u32 returnValueIdx = IRAddTempValue(context, {}, returnTypeIdx, 0);
#if DEBUG_BUILD
					IRGetLocalValue(context, returnValueIdx)->name = "_return"_s;
#endif
					IRValue value = IRValueValue(returnValueIdx, returnTypeIdx);
					*DynamicArrayAdd(&procCallInst.procedureCall.returnValues) = value;
					*ArrayAdd(&result.tuple) = value;
				}
			}
			else {
				u32 returnTypeIdx = procTypeInfo.returnTypeIndices[0];
				u32 returnValueIdx = IRAddTempValue(context, {}, returnTypeIdx, 0);
#if DEBUG_BUILD
				IRGetLocalValue(context, returnValueIdx)->name = "_return"_s;
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

			IRValue param = IRGenFromExpression(context, arg);
			if (param.typeTableIdx != argTypeTableIdx)
				param = IRDoCast(context, arg->any.loc, param, argTypeTableIdx);
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
				param = IRValueImmediateFloat(context, constant.valueAsFloat,
						procParam.typeTableIdx);
			else
				ASSERT(!"Invalid constant type");
			*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = param;
		}

		// Varargs
		if (isVarargs)
		{
			s64 varargsCount = astProcCall->arguments.size - procParamCount;

			static u32 anyPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_ANY_STRUCT);
			static u32 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

			if (varargsCount == 1)
			{
				const ASTExpression *varargsArrayExp = astProcCall->arguments[procParamCount];
				if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx)
				{
					IRAddComment(context, astProcCall->loc, "Forwarding varargs array"_s);

					IRValue varargsArray = IRGenFromExpression(context, varargsArrayExp);

					ASSERT(varargsArray.valueType == IRVALUETYPE_VALUE ||
						   varargsArray.valueType == IRVALUETYPE_MEMORY);
					*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = varargsArray;

					goto skipGeneratingVarargsArray;
				}
			}

			IRAddComment(context, astProcCall->loc, "Build varargs array"_s);

			IRValue pointerToBuffer;
			if (varargsCount > 0)
			{
				// Allocate stack space for buffer
				u32 bufferValueIdx = IRAddTempValue(context, {},
						GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, varargsCount),
						VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
				IRGetLocalValue(context, bufferValueIdx)->name = "_varargsBuffer"_s;
#endif
				IRValue bufferIRValue = IRValueValue(context, bufferValueIdx);
				IRValue ptrToBuffer = IRPointerToValue(context, astProcCall->loc, bufferIRValue);

				// Fill the buffer
				int nonVarargs = (int)procParamCount;
				for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
				{
					const ASTExpression *arg = astProcCall->arguments[argIdx + nonVarargs];

					IRValue bufferIndexValue = IRValueImmediate(argIdx);
					IRValue bufferSlotValue = IRDoArrayAccess(context, arg->any.loc,
							ptrToBuffer.valueIdx, bufferIRValue.typeTableIdx,
							bufferIndexValue);

					IRValue rightValue = IRGenFromExpression(context, arg);
					IRDoAssignment(context, arg->any.loc, bufferSlotValue, rightValue);
				}

				pointerToBuffer = IRPointerToValue(context, astProcCall->loc, bufferIRValue);
			}
			else {
				varargsCount = 0; // Can be negative
				pointerToBuffer = IRValueImmediate(0, anyPointerTypeIdx);
			}

			// By now we should have the buffer with all the varargs as Any structs.
			// Now we put it into a dynamic array struct.

			// Allocate stack space for array
			u32 arrayValueIdx = IRAddTempValue(context, {}, arrayOfAnyTypeIdx,
					VALUEFLAGS_FORCE_MEMORY);
#if DEBUG_BUILD
			IRGetLocalValue(context, arrayValueIdx)->name = "_varargsArray"_s;
#endif
			IRValue arrayIRValue = IRValueValue(context, arrayValueIdx);
			IRValue arrayPtr = IRPointerToValue(context, astProcCall->loc, arrayIRValue);

			// Size
			{
				StructMember sizeStructMember = {
					.typeTableIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8),
					.offset = 0 };
				IRValue sizeMember = IRDoMemberAccess(context, astProcCall->loc,
						arrayPtr.valueIdx, sizeStructMember);
				IRValue sizeValue = IRValueImmediate(varargsCount);
				IRDoAssignment(context, astProcCall->loc, sizeMember, sizeValue);
			}

			// Data
			{
				StructMember dataStructMember = {
					.typeTableIdx = TYPETABLEIDX_U64,
					.offset = g_pointerSize };
				IRValue dataMember = IRDoMemberAccess(context, astProcCall->loc,
						arrayPtr.valueIdx, dataStructMember);
				IRValue dataValue = pointerToBuffer;
				IRDoAssignment(context, astProcCall->loc, dataMember, dataValue);
			}

			// Pass array as parameter!
			*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = arrayIRValue;
		}

skipGeneratingVarargsArray:
		*AddInstruction(context) = procCallInst;
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
			IRValue param = IRGenFromExpression(context, arg);
			*ArrayAdd(&inst.intrinsic.parameters) = param;
		}

		*AddInstruction(context) = inst;
		break;
	}
	case ASTNODETYPE_UNARY_OPERATION:
	{
		if (expression->unaryOperation.op == TOKEN_OP_POINTER_TO)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);
			result = IRPointerToValue(context, expression->any.loc, result);
		}
		else if (expression->unaryOperation.op == TOKEN_OP_DEREFERENCE)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);
			result = IRDereferenceValue(context, expression->any.loc, result);
		}
		else
		{
			IRInstruction inst = {};
			inst.loc = expression->any.loc;
			inst.unaryOperation.in  = IRGenFromExpression(context, expression->unaryOperation.expression);

			inst.unaryOperation.out = IRValueNewValue(context, "unaryop_result"_s,
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

			*AddInstruction(context) = inst;
			result = inst.unaryOperation.out;
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *rightHand = expression->binaryOperation.rightHand;
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;

		if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT) {
			IRValue srcValue = IRGenFromExpression(context, rightHand);
			IRValue dstValue = IRGenFromExpression(context, leftHand);
			IRDoAssignment(context, expression->any.loc, dstValue, srcValue);
			result = dstValue;
		}
		else {
			IRValue outValue = IRValueNewValue(context, "_binaryop_result"_s, expression->typeTableIdx, 0);
			result = IRInstructionFromBinaryOperation(context, expression, outValue);
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type) {
		case LITERALTYPE_INTEGER:
		{
			u32 typeTableIdx = StripAllAliases(context, expression->typeTableIdx);
			TypeCategory typeCat = GetTypeInfo(context, typeTableIdx).typeCategory;
			if (typeCat == TYPECATEGORY_FLOATING)
				result = IRValueImmediateFloat(context, (f64)expression->literal.integer,
						typeTableIdx);
			else
				result = IRValueImmediate(expression->literal.integer, typeTableIdx);
		} break;
		case LITERALTYPE_CHARACTER:
			result = IRValueImmediate(expression->literal.character, expression->typeTableIdx);
			break;
		case LITERALTYPE_FLOATING:
		{
			u32 typeTableIdx = StripAllAliases(context, expression->typeTableIdx);
			result = IRValueImmediateFloat(context, expression->literal.floating,
					typeTableIdx);
		} break;
		case LITERALTYPE_STRING:
		{
			result = IRValueImmediateString(context, expression->literal.string);
		} break;
		case LITERALTYPE_GROUP:
		{
			IRValue groupIRValue = IRValueNewValue(context, "_groupLiteral"_s,
					expression->typeTableIdx, 0);
			IRPushValueIntoStack(context, expression->any.loc, groupIRValue.valueIdx);
			IRFillValueWithGroupLiteral(context, groupIRValue, expression->literal);
			result = groupIRValue;
		} break;
		case LITERALTYPE_CSTR:
		{
			result = IRValueImmediateCStr(context, expression->literal.string);
		} break;
		default:
			ASSERT(!"Unexpected literal type");
		}
		break;
	} break;
	case ASTNODETYPE_IF:
	{
		IRLabel *skipLabel = NewLabel(context, "skipIf"_s);
		IRConditionalJumpFromExpression(context, expression->ifNode.condition, skipLabel, false);

		// Body!
		IRGenFromExpression(context, expression->ifNode.body);

		IRInstruction *jumpAfterElse = nullptr;
		if (expression->ifNode.elseBody)
			// If we have an else, add a jump instruction here.
			jumpAfterElse = AddInstruction(context);

		IRInsertLabelInstruction(context, expression->any.loc, skipLabel);

		IRLabel *afterElseLabel = NewLabel(context, "afterElse"_s);

		if (expression->ifNode.elseBody)
		{
			jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
			jumpAfterElse->loc = expression->any.loc;
			jumpAfterElse->jump.label = afterElseLabel;

			IRGenFromExpression(context, expression->ifNode.elseBody);

			IRInsertLabelInstruction(context, expression->any.loc, afterElseLabel);
		}

	} break;
	case ASTNODETYPE_IF_STATIC:
	{
		if (expression->ifStaticNode.evaluatesToTrue)
			IRGenFromExpression(context, expression->ifStaticNode.body);
		else if (expression->ifStaticNode.elseBody)
			IRGenFromExpression(context, expression->ifStaticNode.elseBody);
	} break;
	case ASTNODETYPE_WHILE:
	{
		IRLabel *loopLabel     = NewLabel(context, "loop"_s);
		IRLabel *breakLabel    = NewLabel(context, "break"_s);
		IRInsertLabelInstruction(context, expression->any.loc, loopLabel);

		IRLabel *oldBreakLabel    = jobData->currentBreakLabel;
		IRLabel *oldContinueLabel = jobData->currentContinueLabel;
		jobData->currentBreakLabel    = breakLabel;
		jobData->currentContinueLabel = loopLabel;

		IRConditionalJumpFromExpression(context, expression->whileNode.condition, breakLabel, false);

		IRGenFromExpression(context, expression->whileNode.body);

		IRInstruction *loopJump = AddInstruction(context);
		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->loc = expression->any.loc;
		loopJump->jump.label = loopLabel;

		IRInsertLabelInstruction(context, expression->any.loc, breakLabel);

		jobData->currentBreakLabel    = oldBreakLabel;
		jobData->currentContinueLabel = oldContinueLabel;
	} break;
	case ASTNODETYPE_FOR:
	{
		PushIRScope(context);

		const ASTFor *astFor = &expression->forNode;

		u32 indexValueIdx = astFor->indexValueIdx;
		IRPushValueIntoStack(context, astFor->loc, indexValueIdx);
		IRValue indexValue = IRValueValue(context, indexValueIdx);

		bool isThereItVariable = false;
		u32 elementTypeIdx = TYPETABLEIDX_Unset;

		IRValue from = {}, to = {}, arrayValue = {}, ptrToArray = {};
		if (astFor->range->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			astFor->range->binaryOperation.op == TOKEN_OP_RANGE)
		{
			ASTBinaryOperation binaryOp = astFor->range->binaryOperation;

			from = IRGenFromExpression(context, binaryOp.leftHand);
			to =   IRGenFromExpression(context, binaryOp.rightHand);

			// Assign 'i'
			IRAddComment(context, astFor->loc, "Assign 'i'"_s);
			IRDoAssignment(context, astFor->loc, indexValue, from);
		}
		else
		{
			arrayValue = IRGenFromExpression(context, astFor->range);

			TypeInfo rangeTypeInfo = GetTypeInfo(context, arrayValue.typeTableIdx);
			if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER) {
				arrayValue = IRDereferenceValue(context, astFor->loc, arrayValue);
				rangeTypeInfo = GetTypeInfo(context, arrayValue.typeTableIdx);
			}

			ASSERT(arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT ||
				   rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY);

			ptrToArray = IRPointerToValue(context, astFor->loc, arrayValue);

			isThereItVariable = true;
			u32 elementValueIdx = astFor->elementValueIdx;
			// Allocate 'it' variable
			IRPushValueIntoStack(context, astFor->loc, elementValueIdx);

			elementTypeIdx = TYPETABLEIDX_U8;
			if (arrayValue.typeTableIdx != TYPETABLEIDX_STRING_STRUCT)
				elementTypeIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			u32 pointerToElementTypeTableIdx = GetTypeInfoPointerOf(context, elementTypeIdx);

			from = IRValueImmediate(0);
			if (rangeTypeInfo.arrayInfo.count == 0 || arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT)
			{
				// Compare with size member
				StructMember sizeMember = {
					.typeTableIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8),
					.offset = 0 };
				to = IRDoMemberAccess(context, astFor->loc, ptrToArray.valueIdx, sizeMember);
			}
			else
				to = IRValueImmediate(rangeTypeInfo.arrayInfo.count);

			// Assign 'i'
			IRAddComment(context, astFor->loc, "Assign 'i'"_s);
			IRDoAssignment(context, astFor->loc, indexValue, from);

			// Assign 'it'
			IRAddComment(context, astFor->loc, "Assign 'it'"_s);
			IRValue elementVarValue = IRValueValue(elementValueIdx, pointerToElementTypeTableIdx);
			IRValue elementValue = IRDoArrayAccess(context, astFor->loc, ptrToArray.valueIdx,
					arrayValue.typeTableIdx, indexValue);
			elementValue = IRPointerToValue(context, astFor->loc, elementValue);
			IRDoAssignment(context, astFor->loc, elementVarValue, elementValue);
		}

		IRLabel *loopLabel     = NewLabel(context, "loop"_s);
		IRLabel *breakLabel    = NewLabel(context, "break"_s);
		IRLabel *continueLabel = NewLabel(context, "continue"_s);
		IRLabel *continueSkipIncrementLabel = NewLabel(context, "continueSkipIncrement"_s);

		IRInsertLabelInstruction(context, astFor->loc, loopLabel);

		IRLabel *oldBreakLabel    = jobData->currentBreakLabel;
		IRLabel *oldContinueLabel = jobData->currentContinueLabel;
		jobData->currentBreakLabel    = breakLabel;
		jobData->currentContinueLabel = continueLabel;
		jobData->currentContinueSkipIncrementLabel = continueSkipIncrementLabel;

		IRInstruction *breakJump = AddInstruction(context);
		breakJump->type = IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS;
		breakJump->loc = astFor->loc;
		breakJump->conditionalJump2.label = breakLabel;
		breakJump->conditionalJump2.left  = indexValue;
		breakJump->conditionalJump2.right = to;

		IRValue oldArrayValue = jobData->irCurrentForLoopInfo.arrayValue;
		IRValue oldIndexValue = jobData->irCurrentForLoopInfo.indexValue;
		jobData->irCurrentForLoopInfo.arrayValue = arrayValue;
		jobData->irCurrentForLoopInfo.indexValue = indexValue;

		IRGenFromExpression(context, astFor->body);

		IRInsertLabelInstruction(context, astFor->loc, continueLabel);

		// Increment 'i'
		IRInstruction incrementInst = {};
		incrementInst.type = IRINSTRUCTIONTYPE_ADD;
		incrementInst.loc = astFor->loc;
		incrementInst.binaryOperation.left = indexValue;
		incrementInst.binaryOperation.right = IRValueImmediate(1);
		incrementInst.binaryOperation.out = indexValue;
		*AddInstruction(context) = incrementInst;

		if (isThereItVariable)
		{
			// Update 'it'
			u32 elementValueIdx = astFor->elementValueIdx;
			IRValue elementVarValue = IRValueValue(context, elementValueIdx);
			IRValue elementValue = IRDoArrayAccess(context, astFor->loc, ptrToArray.valueIdx,
					arrayValue.typeTableIdx, indexValue);
			elementValue = IRPointerToValue(context, astFor->loc, elementValue);
			IRDoAssignment(context, astFor->loc, elementVarValue, elementValue);
		}

		IRInsertLabelInstruction(context, astFor->loc, continueSkipIncrementLabel);

		IRInstruction *loopJump = AddInstruction(context);
		IRInsertLabelInstruction(context, astFor->loc, breakLabel);

		jobData->currentBreakLabel    = oldBreakLabel;
		jobData->currentContinueLabel = oldContinueLabel;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->loc = astFor->loc;
		loopJump->jump.label = loopLabel;

		jobData->irCurrentForLoopInfo.arrayValue = oldArrayValue;
		jobData->irCurrentForLoopInfo.indexValue = oldIndexValue;

		PopIRScope(context);
	} break;
	case ASTNODETYPE_CONTINUE:
	{
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_JUMP,
			.loc = expression->any.loc,
			.jump = { .label = jobData->currentContinueLabel }
		};
	} break;
	case ASTNODETYPE_REMOVE:
	{
		SourceLocation loc = expression->any.loc;
		IRValue arrayValue = jobData->irCurrentForLoopInfo.arrayValue;
		IRValue indexValue = jobData->irCurrentForLoopInfo.indexValue;
		IRValue sizeValue;

		TypeInfo arrayType = GetTypeInfo(context, arrayValue.typeTableIdx);
		if (arrayType.typeCategory == TYPECATEGORY_POINTER) {
			arrayValue = IRDereferenceValue(context, loc, arrayValue);
			arrayType = GetTypeInfo(context, arrayValue.typeTableIdx);
		}
		IRValue ptrToArray = IRPointerToValue(context, loc, arrayValue);

		{
			StructMember sizeMember = {
				.typeTableIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8),
				.offset = 0 };

			sizeValue = IRDoMemberAccess(context, loc, ptrToArray.valueIdx, sizeMember);
		}

		// Decrement size
		*AddInstruction(context) = {
			.type = IRINSTRUCTIONTYPE_SUBTRACT,
			.loc = loc,
			.binaryOperation = {
				.left = sizeValue,
				.right = IRValueImmediate(1),
				.out = sizeValue
			}
		};

		IRValue current = IRDoArrayAccess(context, loc, ptrToArray.valueIdx,
				arrayValue.typeTableIdx, indexValue);
		IRValue last    = IRDoArrayAccess(context, loc, ptrToArray.valueIdx,
				arrayValue.typeTableIdx, sizeValue);
		IRDoAssignment(context, loc, current, last);

		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.loc = loc;
		inst.jump.label = jobData->currentContinueSkipIncrementLabel;
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_BREAK:
	{
		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.loc = expression->any.loc;
		inst.jump.label = jobData->currentBreakLabel;
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_RETURN:
	{
		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = jobData->irStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			if (jobData->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			if (jobData->shouldReturnValueIdx == U32_MAX)
				jobData->shouldReturnValueIdx = IRNewValue(context, TYPETABLEIDX_U8, 0);

			// Set should return to one
			IRValue shouldReturnRegister = IRValueValue(jobData->shouldReturnValueIdx,
					TYPETABLEIDX_U8);
			IRValue one = IRValueImmediate(1);
			IRDoAssignment(context, expression->any.loc, shouldReturnRegister, one);
		}

		if (expression->returnNode.expression != nullptr)
		{
			ArrayView<ASTExpression *const> returnExps;
			if (expression->returnNode.expression->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS)
			{
				returnExps.size = expression->returnNode.expression->multipleExpressions.array.size;
				returnExps.data = expression->returnNode.expression->multipleExpressions.array.data;
			}
			else
			{
				returnExps.size = 1;
				returnExps.data = &expression->returnNode.expression;
			}

			for (int i = 0; i < returnExps.size; ++i)
			{
				IRValue returnValue = IRGenFromExpression(context, returnExps[i]);
				u32 returnTypeTableIdx = returnExps[i]->typeTableIdx;
				ASSERT(returnTypeTableIdx >= TYPETABLEIDX_Begin);

				if (IRShouldPassByCopy(context, returnTypeTableIdx))
				{
					u64 size = GetTypeInfo(context, returnTypeTableIdx).size;
					IRValue sizeValue = IRValueImmediate(size);

					IRInstruction memcpyInst = {};
					memcpyInst.type = IRINSTRUCTIONTYPE_COPY_MEMORY;
					memcpyInst.copyMemory.src = IRPointerToValue(context, expression->any.loc, returnValue);
					memcpyInst.copyMemory.dst = IRPointerToValue(context, expression->any.loc,
							IRValueValue(jobData->returnValueIndices[i], returnTypeTableIdx));
					memcpyInst.copyMemory.size = sizeValue;

					*AddInstruction(context) = memcpyInst;
				}
				else
				{
					IRValue dst = IRValueValue(jobData->returnValueIndices[i], returnValue.typeTableIdx);
					IRDoAssignment(context, expression->any.loc, dst, returnValue);
				}
			}
		}

		if (isThereCleanUpToDo)
		{
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.loc = {};
			jumpInst.jump.label = jobData->irStack[jobData->irStack.size - 1].closeLabel;
			*AddInstruction(context) = jumpInst;
		}
		else
		{
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.loc = {};
			jumpInst.jump.label = jobData->returnLabel;
			*AddInstruction(context) = jumpInst;
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		IRScope *stackTop = DynamicArrayBack(&jobData->irStack);
		*DynamicArrayAdd(&stackTop->deferredStatements) = expression->deferNode.expression;
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		u32 typeTableIdx = expression->typeOfNode.expression->typeTableIdx;
		IRValue typeInfoValue = IRValueTypeOf(context, typeTableIdx);
		IRValue outValue = IRValueNewValue(context, "_typeof"_s, typeInfoValue.typeTableIdx, 0);

		IRInstruction getPtrInst = {};
		getPtrInst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;
		getPtrInst.unaryOperation.in = typeInfoValue;
		getPtrInst.unaryOperation.out = outValue;
		*AddInstruction(context) = getPtrInst;

		result = outValue;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		u32 typeTableIdx = expression->sizeOfNode.expression->typeTableIdx;
		s64 size = GetTypeInfo(context, typeTableIdx).size;

		result = IRValueImmediate(size, TYPETABLEIDX_U64);
	} break;
	case ASTNODETYPE_CAST:
	{
		IRValue src = IRGenFromExpression(context, expression->castNode.expression);
		result = IRDoCast(context, expression->any.loc, src, expression->typeTableIdx);
	} break;
	case ASTNODETYPE_USING:
	{
		// @Check: only for variable declarations?
		IRGenFromExpression(context, expression->usingNode.expression);
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
		LogError(context, expression->any.loc, "COMPILER ERROR! Type found while generating IR."_s);
	} break;
	case ASTNODETYPE_COMPILER_BREAKPOINT:
	{
		if (StringEquals(expression->compilerBreakpointType, "irgen"_s))
			BREAK;
		else if (StringEquals(expression->compilerBreakpointType, "backend"_s)) {
			IRInstruction breakInstruction = {
				.type = IRINSTRUCTIONTYPE_COMPILER_BREAKPOINT,
				.loc = expression->any.loc
			};
			*AddInstruction(context) = breakInstruction;
		}
	} break;
	default:
		ASSERT(!"Unknown ast node found type while generating IR");
	}

	return result;
}

void PrintJobIRInstructions(Context *context);
void IRGenMain(Context *context)
{
	{
		auto externalVars = context->irExternalVariables.GetForWrite();
		DynamicArrayInit(&externalVars, 32);
	}
	{
		auto &stringLiterals = context->stringLiterals.unsafe;
		BucketArrayInit(&stringLiterals);
		// Empty string
		IRValueImmediateString(context, {});
	}
	{
		auto &cStringLiterals = context->cStringLiterals.unsafe;
		BucketArrayInit(&cStringLiterals);
		// Empty string
		IRValueImmediateCStr(context, {});
	}
	{
		auto &f32Literals = context->f32Literals.unsafe;
		BucketArrayInit(&f32Literals);
		// Empty string
		IRValueImmediateF32(context, 0.0f);
	}
	{
		auto &f64Literals = context->f64Literals.unsafe;
		BucketArrayInit(&f64Literals);
		// Empty string
		IRValueImmediateF64(context, 0.0);
	}
}

void IRJobProcedure(void *args)
{
	IRJobArgs *argsStruct = (IRJobArgs *)args;
	Context *context = argsStruct->context;
	u32 procedureIdx = argsStruct->procedureIdx;

	IRJobData jobData = {};
	jobData.procedureIdx = procedureIdx;
	jobData.returnValueIndices = GetProcedureRead(context, procedureIdx).returnValueIndices;
	jobData.shouldReturnValueIdx = U32_MAX;
	SYSSetFiberData(context->flsIndex, &jobData);

#if DEBUG_BUILD
	Job *runningJob = GetCurrentJob(context);
	runningJob->description = SStringConcat("IR:"_s, GetProcedureRead(context, procedureIdx).name);
#endif

	ASSERT(GetProcedureRead(context, procedureIdx).astBody != nullptr);

	DynamicArrayInit(&jobData.irStack, 64);
	BucketArrayInit(&jobData.irLabels);

	IRGenProcedure(context, procedureIdx, {}, &argsStruct->localValues);

	{
		Procedure proc = GetProcedureRead(context, procedureIdx);
		proc.isIRReady = true;
		UpdateProcedure(context, procedureIdx, &proc);
	}
	// Wake up any jobs that were waiting for this procedure's IR
	WakeUpAllByIndex(context, YIELDREASON_PROC_IR_NOT_READY, procedureIdx);

	if (context->config.logIR)
		PrintJobIRInstructions(context);

	BackendJobProc(context, procedureIdx);

	SwitchJob(context, YIELDREASON_DONE, {});
}

void IRJobExpression(void *args)
{
	IRJobArgs *argsStruct = (IRJobArgs *)args;
	Context *context = argsStruct->context;

	IRJobData jobData = {};
	SYSSetFiberData(context->flsIndex, &jobData);

#if DEBUG_BUILD
	Job *runningJob = GetCurrentJob(context);
	runningJob->description = "IR:Expression"_s;
#endif

	IRGenFromExpression(context, argsStruct->expression);

	SwitchJob(context, YIELDREASON_DONE, {});
}
