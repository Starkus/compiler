u32 IRNewValue(Context *context, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	u64 idx = BucketArrayCount(jobData->localValues);
	Value *result = BucketArrayAdd(jobData->localValues);
	result->name = {};
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 IRNewValue(Context *context, String name, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	u64 idx = BucketArrayCount(jobData->localValues);
	Value *result = BucketArrayAdd(jobData->localValues);
	result->name = name;
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 IRNewValue(Context *context, Value value)
{
	ASSERT(value.typeTableIdx != 0);
	ASSERT(!(value.flags & VALUEFLAGS_TRY_IMMITATE) || value.tryImmitateValueIdx != U32_MAX);

	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	u64 idx = BucketArrayCount(jobData->localValues);
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

inline IRValue IRValueValue(u32 valueIdx, u32 typeTableIdx, s64 offset = 0)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.value.valueIdx = valueIdx;
	result.value.elementSize = 0;
	result.value.offset = offset;
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueValue(Context *context, u32 valueIdx, s64 offset = 0)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.value.valueIdx = valueIdx;
	result.value.elementSize = 0;
	result.value.offset = offset;
	result.typeTableIdx = IRGetValue(context, valueIdx).typeTableIdx;
	return result;
}

inline IRValue IRValueDereference(u32 valueIdx, u32 typeTableIdx, s64 offset = 0)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE_DEREFERENCE;
	result.value.valueIdx = valueIdx;
	result.value.offset = offset;
	result.value.elementSize = 0;
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
		s64 stringCount = BucketArrayCount(&stringLiterals);
		ASSERT(stringCount < U32_MAX);
		for (u32 stringIdx = 0; stringIdx < stringCount; ++stringIdx) {
			if (StringEquals(stringLiterals[stringIdx], string)) {
				result.immediateStringIdx = stringIdx;
				goto done;
			}
		}
		u32 idx = (u32)stringCount;
		result.immediateStringIdx = idx;
		*BucketArrayAdd(&stringLiterals) = CopyStringToStaticData(context, string);
	}
done:
	return result;
}

IRValue IRValueImmediateCStr(Context *context, String string)
{
	IRValue result;
	result.valueType = IRVALUETYPE_IMMEDIATE_CSTR;
	if (string.size == 0)
		result.immediateStringIdx = 0;
	else {
		auto stringLiterals = context->cStringLiterals.GetForWrite();
		s64 stringCount = BucketArrayCount(&stringLiterals);
		ASSERT(stringCount < U32_MAX);
		for (u32 stringIdx = 0; stringIdx < stringCount; ++stringIdx) {
			if (StringEquals(stringLiterals[stringIdx], string)) {
				result.immediateStringIdx = stringIdx;
				goto done;
			}
		}
		u32 idx = (u32)stringCount;
		result.immediateStringIdx = idx;
		*BucketArrayAdd(&stringLiterals) = string;
	}
done:
	return result;
}

IRValue IRValueImmediateFloat(Context *context, f64 f, u32 typeTableIdx = TYPETABLEIDX_F64)
{
	static u64 floatStaticVarUniqueID = 0;

	auto staticVars = context->irStaticVariables.GetForWrite();
	for (int i = 0; i < staticVars->size; ++i)
	{
		IRStaticVariable staticVar = staticVars[i];
		if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_FLOAT &&
			staticVar.initialValue.typeTableIdx == typeTableIdx)
		{
			// Compare as quad word, not float (for example to distinguish 0 from -0
			union { f64 in; u64 inQWord; };
			union { f64 current; u64 currentQWord; };
			in = f;
			current = staticVar.initialValue.immediateFloat;

			if (inQWord == currentQWord)
				return IRValueValue(staticVar.valueIdx, typeTableIdx);
		}
	}

	IRStaticVariable newStaticVar = {};
	newStaticVar.valueIdx = NewGlobalValue(context,
			SNPrintF("_staticFloat%d", 18, floatStaticVarUniqueID++), typeTableIdx,
			VALUEFLAGS_ON_STATIC_STORAGE);
	newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
	newStaticVar.initialValue.immediateFloat = f;
	newStaticVar.initialValue.typeTableIdx = typeTableIdx;
	*DynamicArrayAdd(&staticVars) = newStaticVar;

	// Set up static data
	CTRegister *staticData = (CTRegister *)AllocateStaticData(context, newStaticVar.valueIdx,
			sizeof(CTRegister), 8);
	if (typeTableIdx == TYPETABLEIDX_F32)
		staticData->asF32 = (f32)f;
	else
		staticData->asF64 = f;

	return IRValueValue(newStaticVar.valueIdx, typeTableIdx);
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
	u32 newValue = IRNewValue(context, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.value = { newValue };
	result.typeTableIdx = typeTableIdx;
	return result;
}

inline IRValue IRValueNewValue(Context *context, String name, u32 typeTableIdx, u32 flags,
		u32 immitateValueIdx = 0)
{
	u32 newValueIdx = IRNewValue(context, name, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.value = { newValueIdx };
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

IRValue IRDereferenceValue(Context *context, IRValue in)
{
	TypeInfo pointerTypeInfo = GetTypeInfo(context, in.typeTableIdx);
	ASSERT(pointerTypeInfo.typeCategory == TYPECATEGORY_POINTER);
	u32 pointedTypeIdx = pointerTypeInfo.pointerInfo.pointedTypeTableIdx;

	// This assert is cool and all, but would mean unnecesarily assigning types to things as we
	// generate IR.
	//ASSERT(pointedTypeIdx != TYPETABLEIDX_VOID);

	if (in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
	{
		// Copy first to a register always. Since this is a very short lived value, we should always
		// be able to allocate it in a machine register.

		// It's important that the value is in a register before dereferencing, because values in
		// memory are already dereferenced. If the value is in memory, first we dereference the
		// memory to get the pointer itself:
		//     mov rax, [rsp+123]
		// then we can dereference that pointer once it's on a register:
		//     mov out, [rax]

		String srcName = IRGetValue(context, in.value.valueIdx).name;

		String name = SStringConcat("_deref_forcereg_"_s, srcName);
		u32 tempValueIdx = IRNewValue(context, name, in.typeTableIdx, VALUEFLAGS_TRY_IMMITATE |
				VALUEFLAGS_FORCE_REGISTER, in.value.valueIdx);
		IRValue tmpValue = IRValueValue(tempValueIdx, in.typeTableIdx);

		IRInstruction inst = { IRINSTRUCTIONTYPE_ASSIGNMENT };
		inst.assignment.dst = tmpValue;
		inst.assignment.src = in;
		*AddInstruction(context) = inst;

		name = SStringConcat("_deref_"_s, srcName);
		u32 newValueIdx = IRNewValue(context, name, in.typeTableIdx, VALUEFLAGS_TRY_IMMITATE,
				in.value.valueIdx);
		IRValue value = IRValueValue(newValueIdx, in.typeTableIdx);

		inst.assignment.dst = value;
		inst.assignment.src = tmpValue;
		*AddInstruction(context) = inst;

		IRValue result = IRValueDereference(newValueIdx, pointedTypeIdx);
		return result;
	}
	ASSERT(!"Dereferenced value must be either VALUE or VALUE_DEREFERENCE");
	return {};
}

IRValue IRPointerToValue(Context *context, SourceLocation loc, IRValue in)
{
	ASSERT(in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
	u32 pointerTypeIdx = GetTypeInfoPointerOf(context, in.typeTableIdx);

	IRValue result = IRValueNewValue(context, "_pointerof"_s, pointerTypeIdx, 0);

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
	IRAddComment(context, loc, SNPrintF("Accessing struct member \"%S.%S\"",
				28 + (int)structValueName.size + (int)structMember.name.size,
				structValueName, structMember.name));
#endif

	s64 offset = structMember.offset;
	IRValue result = IRValueDereference(structPtrValueIdx, structMember.typeTableIdx,
			offset);
	result = IRPointerToValue(context, loc, result);
	result.valueType = IRVALUETYPE_VALUE_DEREFERENCE;
	result.typeTableIdx = structMember.typeTableIdx;
	return result;
}

IRValue IRDoArrayAccess(Context *context, SourceLocation loc, IRValue arrayValue,
		IRValue indexValue, u32 elementTypeIdx)
{
	TypeInfo arrayTypeInfo = GetTypeInfo(context, arrayValue.typeTableIdx);
	u32 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementTypeIdx);

	// arrayValue must be an array (or string). If it's a pointer, it should be dereferenced before
	// calling this procedure.
	ASSERT(arrayTypeInfo.typeCategory == TYPECATEGORY_ARRAY ||
		   arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT);

	// Dynamic arrays
	if (arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT ||
			arrayTypeInfo.arrayInfo.count == 0)
	{
		// Access the 'data' pointer
		IRAddComment(context, loc, "Addressing dynamic array"_s);

		TypeInfo arrayStructTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);
		StructMember dataMember = arrayStructTypeInfo.structInfo.members[1];

		arrayValue = IRPointerToValue(context, loc, arrayValue);
		arrayValue = IRDoMemberAccess(context, loc, arrayValue.value.valueIdx, dataMember);
		arrayValue = IRDereferenceValue(context, arrayValue);
		arrayValue.typeTableIdx = pointerToElementTypeIdx;
	}
	else
		arrayValue = IRPointerToValue(context, loc, arrayValue);

	s64 elementSize = GetTypeInfo(context, elementTypeIdx).size;

	if (indexValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		return IRValueDereference(arrayValue.value.valueIdx, elementTypeIdx,
				indexValue.immediate * elementSize);
	}
	else if ((indexValue.valueType == IRVALUETYPE_VALUE ||
			indexValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE) &&
			CountOnes64(elementSize) == 1 && elementSize <= 8)
	{
		// @Todo: move x64 specifics like element size limitations and force to register to x64
		// backend.
		IRValue indexForceReg = IRValueNewValue(context, "_idx"_s, TYPETABLEIDX_S64,
				VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, indexValue.value.valueIdx);
		IRDoAssignment(context, loc, indexForceReg, indexValue);

		u32 flags = IRGetValue(context, arrayValue.value.valueIdx).flags;
		if (flags & VALUEFLAGS_ON_STATIC_STORAGE)
		{
			// @Improve: This is x64 specific. Move to x64 backend.
			// Apparently with [array+index*size] syntax, array ends up being 32 bit displacement.
			// We can't expect a part of the data segment to be that close.
			IRValue arrayForceReg = IRValueNewValue(context, "_arr"_s,
					GetTypeInfoPointerOf(context, arrayValue.typeTableIdx),
					VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, arrayValue.value.valueIdx);
			IRInstruction leaInst = {
				.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
				.loc = loc,
				.unaryOperation = {
					.in = arrayValue,
					.out = arrayForceReg
				}
			};
			*AddInstruction(context) = leaInst;

			arrayValue = arrayForceReg;
		}

		IRValue result = IRValueDereference(arrayValue.value.valueIdx, elementTypeIdx);
		result.value.indexValueIdx = indexForceReg.value.valueIdx;
		result.value.elementSize = elementSize;
		return result;
	}

	// Fall back to simple add instruction
	IRValue offsetValue;
	offsetValue = IRValueNewValue(context, "_array_offset"_s, TYPETABLEIDX_S64, 0);
	if (elementSize == 1)
		IRDoAssignment(context, loc, offsetValue, indexValue);
	else
	{
		IRInstruction multiplyInst = {
			.type = IRINSTRUCTIONTYPE_MULTIPLY,
			.loc = loc,
			.binaryOperation = {
				.left  = indexValue,
				.right = IRValueImmediate(elementSize),
				.out   = offsetValue
			}
		};
		*AddInstruction(context) = multiplyInst;
	}

	IRValue pointerToElementValue = IRValueNewValue(context, "_array_element"_s,
			pointerToElementTypeIdx, 0);
	IRInstruction addOffsetInst = {};
	addOffsetInst.type = IRINSTRUCTIONTYPE_ADD;
	addOffsetInst.loc = loc;
	addOffsetInst.binaryOperation.left = IRPointerToValue(context, loc, arrayValue);
	addOffsetInst.binaryOperation.right = offsetValue;
	addOffsetInst.binaryOperation.out = pointerToElementValue;
	*AddInstruction(context) = addOffsetInst;

	return IRValueDereference(pointerToElementValue.value.valueIdx, elementTypeIdx);
}

inline void IRPushValueIntoStack(Context *context, SourceLocation loc, u32 valueIdx)
{
	*AddInstruction(context) = {
		.type = IRINSTRUCTIONTYPE_PUSH_VALUE,
		.loc = loc,
		.pushValue = { .valueIdx = valueIdx }
	};
}

u32 IRAddTempValue(Context *context, SourceLocation loc, String name, u32 typeTableIdx, u8 flags)
{
	u32 valueIdx = IRNewValue(context, name, typeTableIdx, flags);
	IRPushValueIntoStack(context, loc, valueIdx);
	return valueIdx;
}

void IRDoAssignment(Context *context, SourceLocation loc, IRValue dstValue, IRValue srcValue)
{
	ASSERT(dstValue.valueType != IRVALUETYPE_IMMEDIATE_INTEGER);
	ASSERT(dstValue.valueType != IRVALUETYPE_IMMEDIATE_FLOAT);
	ASSERT(dstValue.valueType != IRVALUETYPE_IMMEDIATE_STRING);
	ASSERT(dstValue.valueType != IRVALUETYPE_IMMEDIATE_CSTR);

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

	// String literals
	if (srcValue.valueType == IRVALUETYPE_IMMEDIATE_STRING) {
		ASSERT(dstValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT);

		String literal;
		{
			auto stringLiterals = context->stringLiterals.GetForRead();
			literal = stringLiterals[srcValue.immediateStringIdx];
		}

		TypeInfo stringTypeInfo = GetTypeInfo(context, TYPETABLEIDX_STRING_STRUCT);
		IRValue dstPtr = IRPointerToValue(context, loc, dstValue);
		IRValue sizeMember = IRDoMemberAccess(context, loc, dstPtr.value.valueIdx,
				stringTypeInfo.structInfo.members[0]);

		IRInstruction sizeSetInst = {
			.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
			.loc = loc,
			.assignment = {
				.src = IRValueImmediate(literal.size),
				.dst = sizeMember
			}
		};
		*AddInstruction(context) = sizeSetInst;

		IRValue dataMember = IRDoMemberAccess(context, loc, dstPtr.value.valueIdx,
				stringTypeInfo.structInfo.members[1]);

		u32 charPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8);
		srcValue.typeTableIdx = charPtrTypeIdx;
		IRInstruction dataSetInst = {
			.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
			.loc = loc,
			.assignment = {
				.src = srcValue,
				.dst = dataMember
			}
		};
		*AddInstruction(context) = dataSetInst;

		return;
	}

	// Cast to Any
	if (dstValue.typeTableIdx == TYPETABLEIDX_ANY_STRUCT &&
		srcValue.typeTableIdx != TYPETABLEIDX_ANY_STRUCT)
	{
		IRAddComment(context, loc, "Wrapping in Any"_s);
		TypeInfo anyTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ANY_STRUCT);

		// Access typeInfo member
		IRValue dstPtr = IRPointerToValue(context, loc, dstValue);
		IRValue typeInfoMember = IRDoMemberAccess(context, loc, dstPtr.value.valueIdx,
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
		IRValue dataMember = IRDoMemberAccess(context, loc, dstPtr.value.valueIdx,
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
				static u64 tempVarForAnyUniqueID = 0;
				String tempVarName = SNPrintF("_tempVarForAny%llu", 20, tempVarForAnyUniqueID++);
				u32 tempValue = IRAddTempValue(context, loc, tempVarName, srcValue.typeTableIdx,
						VALUEFLAGS_FORCE_MEMORY);
				IRValue tempVarIRValue = IRValueValue(tempValue, srcValue.typeTableIdx);

				IRInstruction dataCopyInst = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = dataValue,
						.dst = tempVarIRValue
					}
				};
				*AddInstruction(context) = dataCopyInst;

				dataValue = tempVarIRValue;

				IRInstruction dataAssignInst = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = IRPointerToValue(context, loc, dataValue),
						.dst = dataMember
					}
				};
				*AddInstruction(context) = dataAssignInst;
			}
			// Small primitives are stored directly on the Any struct.
			else {
				dataMember.typeTableIdx = dataValue.typeTableIdx;

				IRInstruction dataAssignInst = {
					.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
					.loc = loc,
					.assignment = {
						.src = dataValue,
						.dst = dataMember
					}
				};
				*AddInstruction(context) = dataAssignInst;
			}
		}
		// These are already in memory, just save a pointer
		else {
			IRInstruction dataAssignInst = {
				.type = IRINSTRUCTIONTYPE_ASSIGNMENT,
				.loc = loc,
				.assignment = {
					.src = IRPointerToValue(context, loc, dataValue),
					.dst = dataMember
				}
			};
			*AddInstruction(context) = dataAssignInst;
		}

		return;
	}

	dstValue.typeTableIdx = StripAllAliases(context, dstValue.typeTableIdx);
	srcValue.typeTableIdx = StripAllAliases(context, srcValue.typeTableIdx);

	// Cast static array to dynamic array
	TypeInfo dstTypeInfo = GetTypeInfo(context, dstValue.typeTableIdx);
	TypeInfo srcTypeInfo = GetTypeInfo(context, srcValue.typeTableIdx);
	if (dstTypeInfo.typeCategory  == TYPECATEGORY_ARRAY &&
		srcTypeInfo.typeCategory == TYPECATEGORY_ARRAY &&
		dstTypeInfo.arrayInfo.count  == 0 &&
		srcTypeInfo.arrayInfo.count != 0)
	{
		TypeInfo dynamicArrayTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);

		IRValue dstPtr = IRPointerToValue(context, loc, dstValue);

		// Size
		StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
		IRValue sizeMember = IRDoMemberAccess(context, loc, dstPtr.value.valueIdx, sizeStructMember);
		IRValue sizeValue = IRValueImmediate(srcTypeInfo.arrayInfo.count);
		IRDoAssignment(context, loc, sizeMember, sizeValue);

		// Data
		StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
		IRValue dataMember = IRDoMemberAccess(context, loc, dstPtr.value.valueIdx, dataStructMember);
		IRValue dataValue = IRPointerToValue(context, loc, srcValue);
		IRDoAssignment(context, loc, dataMember, dataValue);

		return;
	}

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
				.src = IRValueImmediateFloat(context, (f64)srcValue.immediate),
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
	label->instructionIdx = BucketArrayCount(jobData->irInstructions);
	*AddInstruction(context) = {
		.type = IRINSTRUCTIONTYPE_LABEL,
		.loc = loc,
		.label = label
	};
}

IRValue IRDoCast(Context *context, SourceLocation loc, IRValue value, u32 typeTableIdx)
{
	u32 tempValueIdx = IRAddTempValue(context, loc, "_cast"_s, typeTableIdx, 0);
	IRValue result = IRValueValue(tempValueIdx, typeTableIdx, 0);
	IRDoAssignment(context, loc, result, value);
	return result;
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
			String valueName = IRGetValue(context, irValue.value.valueIdx).name; 
			IRAddComment(context, loc, SNPrintF("Dereference struct pointer \"%S\"", 64, valueName));
			String name = SStringConcat("_derefstrctptr_"_s, valueName);
			u32 newValueIdx = IRNewValue(context, name, irValue.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			IRValue newValue = IRValueValue(newValueIdx, irValue.typeTableIdx);

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

		result = IRDoMemberAccess(context, loc, structPtr.value.valueIdx, structMember);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS) {
		IRValue arrayValue = IRGenFromExpression(context, leftHand);
		IRValue indexValue = IRGenFromExpression(context, rightHand);

		if (GetTypeInfo(context, arrayValue.typeTableIdx).typeCategory == TYPECATEGORY_POINTER) {
			// Dereference the pointer to the array
			arrayValue = IRDereferenceValue(context, arrayValue);
		}

		result = IRDoArrayAccess(context, loc, arrayValue, indexValue, expression->typeTableIdx);
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

		// Hint for register allocation to try and allocate left and out in the same register
		if (outValue.valueType == IRVALUETYPE_VALUE &&
			inst.binaryOperation.left.valueType == IRVALUETYPE_VALUE)
		{
			Value v = IRGetValue(context, inst.binaryOperation.left.value.valueIdx);
			v.flags |= VALUEFLAGS_TRY_IMMITATE;
			v.tryImmitateValueIdx = outValue.value.valueIdx;
			IRUpdateValue(context, inst.binaryOperation.left.value.valueIdx, &v);
		}

		inst.binaryOperation.out = outValue;
		*AddInstruction(context) = inst;

		if (expression->binaryOperation.op >= TOKEN_OP_ASSIGNMENT_Begin &&
			expression->binaryOperation.op <= TOKEN_OP_ASSIGNMENT_End)
		{
			IRDoAssignment(context, loc, inst.binaryOperation.left, outValue);
			result = inst.binaryOperation.left;
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
		IRPushValueIntoStack(context, loc, returnValue.value.valueIdx);
		*ArrayAdd(&inlineReturnValues) = returnValue.value.valueIdx;
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
			u32 bufferValueIdx = IRAddTempValue(context, loc, "_varargsBuffer"_s,
					GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, varargsCount),
					VALUEFLAGS_FORCE_MEMORY);
			IRValue bufferIRValue = IRValueValue(context, bufferValueIdx);

			// Fill the buffer
			int nonVarargs = (int)procParamCount;
			for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
			{
				ASTExpression *arg = astProcCall.arguments[argIdx + nonVarargs];

				IRValue bufferIndexValue = IRValueImmediate(argIdx);
				IRValue bufferSlotValue = IRDoArrayAccess(context, loc, bufferIRValue,
						bufferIndexValue,
						TYPETABLEIDX_ANY_STRUCT);

				IRValue rightValue = IRGenFromExpression(context, arg);
				IRDoAssignment(context, loc, bufferSlotValue, rightValue);
			}

			pointerToBuffer = IRPointerToValue(context, loc, bufferIRValue);
		}
		else
			pointerToBuffer = IRValueImmediate(0, GetTypeInfoPointerOf(context, anyPointerTypeIdx));

		// By now we should have the buffer with all the varargs as Any structs.
		// Now we put it into a dynamic array struct.

		// Allocate stack space for array
		u32 arrayValueIdx = IRAddTempValue(context, loc, "_varargsArray"_s, arrayOfAnyTypeIdx,
				VALUEFLAGS_FORCE_MEMORY);
		IRValue arrayIRValue = IRValueValue(context, arrayValueIdx);
		IRValue arrayPtr = IRPointerToValue(context, loc, arrayIRValue);

		TypeInfo dynamicArrayTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);
		// Size
		{
			StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
			IRValue sizeMember = IRDoMemberAccess(context, loc, arrayPtr.value.valueIdx, sizeStructMember);
			IRValue sizeValue = IRValueImmediate(varargsCount);
			IRDoAssignment(context, loc, sizeMember, sizeValue);
		}

		// Data
		{
			StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
			IRValue dataMember = IRDoMemberAccess(context, loc, arrayPtr.value.valueIdx, dataStructMember);
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
			for (; i < astLiteral.members.size; ++i)
			{
				ASTExpression *literalMemberExp = astLiteral.members[i];
				if (literalMemberExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
				{
					nonNamedCount = i;
					break;
				}
			}
			for (; i < astLiteral.members.size; ++i)
			{
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
						innerStructPtr.value.valueIdx, currentMember);
				*DynamicArrayAdd(&structStack) = { innerStructValue, currentMember.typeTableIdx, 0 };
				continue;
			}

			IRValue memberValue = IRDoMemberAccess(context, loc, innerStructPtr.value.valueIdx, currentMember);
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
		u32 elementTypeIdx = groupTypeInfo.arrayInfo.elementTypeTableIdx;
		for (int memberIdx = 0; memberIdx < astLiteral.members.size; ++memberIdx)
		{
			ASTExpression *literalMemberExp = astLiteral.members[memberIdx];

			IRValue indexIRValue = IRValueImmediate(memberIdx);
			IRValue elementValue = IRDoArrayAccess(context, loc, value, indexIRValue,
					elementTypeIdx);
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
	{
		IRFillValueWithGroupLiteral(context, dstValue, srcExpression->literal);
	}
	else
	{
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
	ArrayInit(&returnInst.returnInst.returnValueIndices, jobData->returnValueIndices.size);
	returnInst.returnInst.returnValueIndices.size = jobData->returnValueIndices.size;
	for (int i = 0; i < jobData->returnValueIndices.size; ++i)
		returnInst.returnInst.returnValueIndices[i] = jobData->returnValueIndices[i];
	*AddInstruction(context) = returnInst;
}

IRValue IRGenFromExpression(Context *context, const ASTExpression *expression)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	// @Todo: use SourceLocation in IR instructions to print source lines when doing PrintIR or
	// outputting backend asm directly, instead of adding these comments.
#if 0
	if (jobData && jobData->procedureIdx != 0)
	{
		SourceLocation loc = expression->any.loc;
		static u32 lastFileIdx = loc.fileIdx;
		static u32 lastLine = ExpandSourceLocation(context, loc).line;

		FatSourceLocation fatLoc = ExpandSourceLocation(context, loc);
		if (loc.fileIdx != lastFileIdx || fatLoc.line != lastLine)
			IRAddComment(context, loc, { fatLoc.lineSize, fatLoc.beginingOfLine });

		lastFileIdx = loc.fileIdx;
		lastLine = fatLoc.line;
	}
#endif

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
				IRStaticVariable newStaticVar = {};
				newStaticVar.valueIdx = valueIdx;
				newStaticVar.initialValue.valueType = IRVALUETYPE_INVALID;

				// Initial value
				if (varDecl.astInitialValue) {
					if (varDecl.astInitialValue->literal.type == LITERALTYPE_STRING) {
						newStaticVar.initialValue = IRValueImmediateString(context,
								varDecl.astInitialValue->literal.string);
					}
					else if (varDecl.astInitialValue->literal.type == LITERALTYPE_CSTR) {
						newStaticVar.initialValue = IRValueImmediateCStr(context,
								varDecl.astInitialValue->literal.string);
					}
					else {
						Constant constant  = TryEvaluateConstant(context, varDecl.astInitialValue);
						if (constant.type == CONSTANTTYPE_INVALID)
							LogError(context, varDecl.astInitialValue->any.loc,
									"Initial value of static variable isn't constant"_s);

						newStaticVar.initialValue = IRValueFromConstant(context, constant);
					}
				}

				auto staticVars = context->irStaticVariables.GetForWrite();
				*DynamicArrayAdd(&staticVars) = newStaticVar;

				u32 varValueIdx = *GetVariableValueIdx(&varDecl, varIdx);
				u32 varTypeIdx  = *GetVariableTypeIdx(&varDecl, varIdx);

				// Set up static data for variable
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
			CTContext ctContext = {
				.globalContext = context,
				.procedureIdx = U32_MAX,
				.localValues = jobData->localValues
			};
			HashMapInit(&ctContext.values, 32);
			CTRunInstructions(&ctContext, &staticVarIRInstructions, 0);
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
					result = IRValueImmediateFloat(context, f);
				}
				else
					result = IRValueImmediate(constant.valueAsInt, constant.typeTableIdx);
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
		s32 paramCount = Max(procParamCount, callParamCount) + /*isReturnByCopy +*/ isVarargs;
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
					u32 returnValueIdx = IRAddTempValue(context, {}, "_return"_s, returnTypeIdx, 0);
					IRValue value = IRValueValue(returnValueIdx, returnTypeIdx);
					*DynamicArrayAdd(&procCallInst.procedureCall.returnValues) = value;
					*ArrayAdd(&result.tuple) = value;
				}
			}
			else {
				u32 returnTypeIdx = procTypeInfo.returnTypeIndices[0];
				u32 returnValueIdx = IRAddTempValue(context, {}, "_return"_s, returnTypeIdx, 0);
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
						   varargsArray.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
					*DynamicArrayAdd(&procCallInst.procedureCall.parameters) = varargsArray;

					goto skipGeneratingVarargsArray;
				}
			}

			IRAddComment(context, astProcCall->loc, "Build varargs array"_s);

			IRValue pointerToBuffer;
			if (varargsCount > 0)
			{
				// Allocate stack space for buffer
				u32 bufferValueIdx = IRAddTempValue(context, {}, "_varargsBuffer"_s,
						GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, varargsCount),
						VALUEFLAGS_FORCE_MEMORY);
				IRValue bufferIRValue = IRValueValue(context, bufferValueIdx);

				// Fill the buffer
				int nonVarargs = (int)procParamCount;
				for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
				{
					const ASTExpression *arg = astProcCall->arguments[argIdx + nonVarargs];

					IRValue bufferIndexValue = IRValueImmediate(argIdx);
					IRValue bufferSlotValue = IRDoArrayAccess(context, arg->any.loc, bufferIRValue,
							bufferIndexValue, TYPETABLEIDX_ANY_STRUCT);

					IRValue rightValue = IRGenFromExpression(context, arg);
					IRDoAssignment(context, arg->any.loc, bufferSlotValue, rightValue);
				}

				pointerToBuffer = IRPointerToValue(context, astProcCall->loc, bufferIRValue);
			}
			else
				pointerToBuffer = IRValueImmediate(0, anyPointerTypeIdx);

			// By now we should have the buffer with all the varargs as Any structs.
			// Now we put it into a dynamic array struct.

			// Allocate stack space for array
			u32 arrayValueIdx = IRAddTempValue(context, {}, "_varargsArray%llu"_s,
					arrayOfAnyTypeIdx, VALUEFLAGS_FORCE_MEMORY);
			IRValue arrayIRValue = IRValueValue(context, arrayValueIdx);
			IRValue arrayPtr = IRPointerToValue(context, astProcCall->loc, arrayIRValue);

			// Size
			{
				StructMember sizeStructMember = {
					.typeTableIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8),
					.offset = 0 };
				IRValue sizeMember = IRDoMemberAccess(context, astProcCall->loc,
						arrayPtr.value.valueIdx, sizeStructMember);
				IRValue sizeValue = IRValueImmediate(varargsCount);
				IRDoAssignment(context, astProcCall->loc, sizeMember, sizeValue);
			}

			// Data
			{
				StructMember dataStructMember = {
					.typeTableIdx = TYPETABLEIDX_U64,
					.offset = g_pointerSize };
				IRValue dataMember = IRDoMemberAccess(context, astProcCall->loc,
						arrayPtr.value.valueIdx, dataStructMember);
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
			result = IRDereferenceValue(context, result);
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

		if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
		{
			IRValue srcValue = IRGenFromExpression(context, rightHand);
			IRValue dstValue = IRGenFromExpression(context, leftHand);
			IRDoAssignment(context, expression->any.loc, dstValue, srcValue);
			result = dstValue;
		}
		else
		{
			IRValue outValue = IRValueNewValue(context, "_binaryop_result"_s, expression->typeTableIdx, 0);
			result = IRInstructionFromBinaryOperation(context, expression, outValue);
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_INTEGER:
		{
			u32 typeTableIdx = StripAllAliases(context, expression->typeTableIdx);
			TypeCategory typeCat = GetTypeInfo(context, typeTableIdx).typeCategory;
			if (typeCat == TYPECATEGORY_FLOATING)
				result = IRValueImmediateFloat(context, (f64)expression->literal.integer,
						expression->typeTableIdx);
			else
				result = IRValueImmediate(expression->literal.integer, expression->typeTableIdx);
		} break;
		case LITERALTYPE_CHARACTER:
			result = IRValueImmediate(expression->literal.character, expression->typeTableIdx);
			break;
		case LITERALTYPE_FLOATING:
			result = IRValueImmediateFloat(context, expression->literal.floating,
					expression->typeTableIdx);
			break;
		case LITERALTYPE_STRING:
		{
#if 0
			static u64 stringStaticVarUniqueID = 0;

			IRStaticVariable newStaticVar = {};
			newStaticVar.valueIdx = NewGlobalValue(context,
					SNPrintF("staticString%d", 18, stringStaticVarUniqueID++),
					TYPETABLEIDX_STRING_STRUCT, VALUEFLAGS_ON_STATIC_STORAGE);
			newStaticVar.initialValue = IRValueImmediateString(context, expression->literal.string);
			newStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_STRING_STRUCT;

			{
				auto staticVars = context->irStaticVariables.GetForWrite();
				*DynamicArrayAdd(&staticVars) = newStaticVar;
			}

			result = IRValueValue(context, newStaticVar.valueIdx);
#else
			result = IRValueImmediateString(context, expression->literal.string);
#endif
		} break;
		case LITERALTYPE_GROUP:
		{
			IRValue groupIRValue = IRValueNewValue(context, "_groupLiteral"_s,
					expression->typeTableIdx, 0);
			IRPushValueIntoStack(context, expression->any.loc, groupIRValue.value.valueIdx);
			IRFillValueWithGroupLiteral(context, groupIRValue, expression->literal);
			result = groupIRValue;
		} break;
		case LITERALTYPE_CSTR:
		{
			static u64 stringStaticVarUniqueID = 0;

			u32 charPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8);
			IRStaticVariable newStaticVar = {};
			newStaticVar.valueIdx = NewGlobalValue(context,
					SNPrintF("staticCString%d", 20, stringStaticVarUniqueID++),
					charPointerTypeIdx, VALUEFLAGS_ON_STATIC_STORAGE);
			newStaticVar.initialValue = IRValueImmediateCStr(context, expression->literal.string);
			newStaticVar.initialValue.typeTableIdx = charPointerTypeIdx;

			{
				auto staticVars = context->irStaticVariables.GetForWrite();
				*DynamicArrayAdd(&staticVars) = newStaticVar;
			}

			result = IRPointerToValue(context, expression->any.loc,
					IRValueValue(context, newStaticVar.valueIdx));
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

		IRValue from = {}, to = {}, arrayValue = {};
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
			if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			{
				arrayValue = IRDereferenceValue(context, arrayValue);
				rangeTypeInfo = GetTypeInfo(context, arrayValue.typeTableIdx);
			}

			ASSERT(arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT ||
				   rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY);

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
				IRValue arrayPtr = IRPointerToValue(context, astFor->loc, arrayValue);
				to = IRDoMemberAccess(context, astFor->loc, arrayPtr.value.valueIdx, sizeMember);
			}
			else
				to = IRValueImmediate(rangeTypeInfo.arrayInfo.count);

			// Assign 'i'
			IRAddComment(context, astFor->loc, "Assign 'i'"_s);
			IRDoAssignment(context, astFor->loc, indexValue, from);

			// Assign 'it'
			IRAddComment(context, astFor->loc, "Assign 'it'"_s);
			IRValue elementVarValue = IRValueValue(elementValueIdx, pointerToElementTypeTableIdx);
			IRValue elementValue = IRDoArrayAccess(context, astFor->loc, arrayValue, indexValue,
					elementTypeIdx);
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
			IRValue elementValue = IRDoArrayAccess(context, astFor->loc, arrayValue, indexValue, elementTypeIdx);
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
		if (arrayType.typeCategory == TYPECATEGORY_POINTER)
			arrayType = GetTypeInfo(context, arrayType.pointerInfo.pointedTypeTableIdx);

		u32 elementTypeIdx = arrayType.arrayInfo.elementTypeTableIdx;

		{
			StructMember sizeMember = {
				.typeTableIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_U8),
				.offset = 0 };
			StructMember dataMember = {
				.typeTableIdx = TYPETABLEIDX_U64,
				.offset = g_pointerSize };

			IRValue arrayPtr = IRPointerToValue(context, loc, arrayValue);
			sizeValue = IRDoMemberAccess(context, loc, arrayPtr.value.valueIdx, sizeMember);
		}

		IRInstruction decrInst = {
			.type = IRINSTRUCTIONTYPE_SUBTRACT,
			.loc = loc,
			.binaryOperation = {
				.left = sizeValue,
				.right = IRValueImmediate(1),
				.out = sizeValue
			}
		};

		IRValue current = IRDoArrayAccess(context, loc, arrayValue, indexValue, elementTypeIdx);
		IRValue last    = IRDoArrayAccess(context, loc, arrayValue, sizeValue,  elementTypeIdx);
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
		auto staticVars = context->irStaticVariables.GetForWrite();
		DynamicArrayInit(&staticVars, 64);
	}
	{
		auto externalVars = context->irExternalVariables.GetForWrite();
		DynamicArrayInit(&externalVars, 32);
	}
	{
		auto stringLiterals = context->stringLiterals.GetForWrite();
		BucketArrayInit(&stringLiterals);
		// Empty string
		*BucketArrayAdd(&stringLiterals) = {};
	}
	{
		auto cStringLiterals = context->cStringLiterals.GetForWrite();
		BucketArrayInit(&cStringLiterals);
		// Empty string
		*BucketArrayAdd(&cStringLiterals) = {};
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

#if 0
	{
#if !FINAL_BUILD
		auto jobs = context->jobs.Get();
		jobs[jobIdx].title = SStringConcat("IR:"_s, GetProcedureRead(context, procedureIdx).name);
#endif

		ASSERT(context->jobs.unsafe[jobIdx].state == TCYIELDREASON_READY);
		ASSERT(context->jobs.unsafe[jobIdx].isRunning);
		threadData->lastJobIdx = U32_MAX;
	}
#endif

	ASSERT(GetProcedureRead(context, procedureIdx).astBody != nullptr);

	DynamicArrayInit(&jobData.irStack, 64);
	BucketArrayInit(&jobData.irLabels);

	IRGenProcedure(context, procedureIdx, {}, &argsStruct->localValues);

	// Wake up any jobs that were waiting for this procedure's IR
	{
		Procedure proc = GetProcedureRead(context, procedureIdx);
		proc.isIRReady = true;
		UpdateProcedure(context, procedureIdx, &proc);

		auto jobsWaiting = context->jobsWaitingForProcedure.Get();
		for (int i = 0; i < jobsWaiting->size; ) {
			TCJob *job = &jobsWaiting[i];
			if (job->context.index == procedureIdx) {
				EnqueueReadyJob(context, job->fiber);
				// Remove
				*job = jobsWaiting[--jobsWaiting->size];
			}
			else
				++i;
		}
	}

	if (context->config.logIR)
		PrintJobIRInstructions(context);

	BackendJobProc(context, procedureIdx);

	SwitchJob(context, TCYIELDREASON_DONE, {});
}

void IRJobExpression(void *args)
{
	IRJobArgs *argsStruct = (IRJobArgs *)args;
	Context *context = argsStruct->context;

	IRJobData jobData = {};
	SYSSetFiberData(context->flsIndex, &jobData);

#if 0
	{
		ASSERT(context->jobs.unsafe[jobIdx].state == TCYIELDREASON_READY);
		ASSERT(context->jobs.unsafe[jobIdx].isRunning);

#if !FINAL_BUILD
		auto jobs = context->jobs.Get();
		jobs[jobIdx].title = "IR:Expression"_s;
#endif
	}
#endif

	IRGenFromExpression(context, argsStruct->expression);

	SwitchJob(context, TCYIELDREASON_DONE, {});
}
