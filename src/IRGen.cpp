IRLabel *NewLabel(Context *context, String prefix)
{
	static u64 currentLabelId = 0;

	IRLabel result = {};
	IRProcedureScope stackTop = context->irProcedureStack[context->irProcedureStack.size - 1];

	result.name = TPrintF("%S%d", prefix, currentLabelId++);
	result.procedureIdx = stackTop.procedureIdx;
	result.instructionIdx = -1;

	IRLabel *newLabel = BucketArrayAdd(&context->irLabels);
	*newLabel = result;
	return newLabel;
}

void PushIRScope(Context *context)
{
	IRScope newScope = {};
	DynamicArrayInit(&newScope.deferredStatements, 4);
	*DynamicArrayAdd(&context->irStack) = newScope;
}

void PopIRScope(Context *context)
{
	ASSERT(context->irStack.size);
	--context->irStack.size;
}

IRProcedureScope *PushIRProcedure(Context *context, SourceLocation definitionLoc, s32 procedureIdx)
{
	IRProcedureScope procScope;
	procScope.procedureIdx = procedureIdx;
	procScope.irStackBase = context->irStack.size;
	procScope.shouldReturnValueIdx = U32_MAX;
	procScope.definitionLoc = definitionLoc;

	IRProcedureScope *newProcScope = DynamicArrayAdd(&context->irProcedureStack);
	*newProcScope = procScope;

	PushIRScope(context);
	return newProcScope;
}

void PopIRProcedure(Context *context)
{
	--context->irProcedureStack.size;
	PopIRScope(context);
}

inline IRInstruction *AddInstruction(Context *context)
{
	IRProcedureScope stackTop = context->irProcedureStack[context->irProcedureStack.size - 1];
	return BucketArrayAdd(&GetProcedure(context, stackTop.procedureIdx)->instructions);
}

void IRAddComment(Context *context, String comment)
{
	IRInstruction result;
	result.type = IRINSTRUCTIONTYPE_COMMENT;
	result.comment = comment;
	*AddInstruction(context) = result;
}

bool IRShouldPassByCopy(Context *context, u32 typeTableIdx)
{
	TypeInfo typeInfo = context->typeTable[typeTableIdx];
	// @Speed
	return  typeInfo.typeCategory == TYPECATEGORY_ARRAY ||
		  ((typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			typeInfo.typeCategory == TYPECATEGORY_UNION) &&
			typeInfo.size != 1 &&
			typeInfo.size != 2 &&
			typeInfo.size != 4 &&
			typeInfo.size != 8);
}

IRValue IRValueValue(u32 valueIdx, u32 typeTableIdx, s64 offset = 0)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.value.valueIdx = valueIdx;
	result.value.elementSize = 0;
	result.value.offset = offset;
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueValue(Context *context, u32 valueIdx, s64 offset = 0)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.value.valueIdx = valueIdx;
	result.value.elementSize = 0;
	result.value.offset = offset;
	result.typeTableIdx = context->values[valueIdx].typeTableIdx;
	return result;
}

IRValue IRValueTCValue(Context *context, TCValue tcValue)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	if (tcValue.type == TCVALUETYPE_VALUE)
	{
		result.value = { tcValue.valueIdx };
		result.typeTableIdx = context->values[tcValue.valueIdx].typeTableIdx;
	}
	else
	{
		u32 procIdx = context->irProcedureStack[context->irProcedureStack.size - 1].procedureIdx;
		u32 paramValueIdx = GetProcedure(context, procIdx)->parameterValues[tcValue.valueIdx];
		result.value = { paramValueIdx };
		result.typeTableIdx = context->values[paramValueIdx].typeTableIdx;
	}
	return result;
}

IRValue IRValueDereference(u32 valueIdx, u32 typeTableIdx, s64 offset = 0)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE_DEREFERENCE;
	result.value.valueIdx = valueIdx;
	result.value.offset = offset;
	result.value.elementSize = 0;
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueImmediate(s64 immediate, u32 typeTableIdx = TYPETABLEIDX_S64)
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
	if (string.size == 0)
		result.immediateStringIdx = 0;
	else
	{
		s64 stringCount = BucketArrayCount(&context->stringLiterals);
		ASSERT(stringCount < U32_MAX);
		for (u32 stringIdx = 0; stringIdx < stringCount; ++stringIdx)
		{
			if (StringEquals(context->stringLiterals[stringIdx], string))
			{
				result.immediateStringIdx = stringIdx;
				goto done;
			}
		}
		u32 idx = (u32)stringCount;
		result.immediateStringIdx = idx;
		*BucketArrayAdd(&context->stringLiterals) = string;
	}
done:
	return result;
}

IRValue IRValueImmediateFloat(Context *context, f64 f, u32 typeTableIdx = TYPETABLEIDX_F64)
{
	static u64 floatStaticVarUniqueID = 0;

	for (int i = 0; i < context->irStaticVariables.size; ++i)
	{
		IRStaticVariable staticVar = context->irStaticVariables[i];
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
	newStaticVar.valueIdx = NewValue(context,
			TPrintF("_staticFloat%d", floatStaticVarUniqueID++), typeTableIdx,
			VALUEFLAGS_ON_STATIC_STORAGE);
	newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
	newStaticVar.initialValue.immediateFloat = f;
	newStaticVar.initialValue.typeTableIdx = typeTableIdx;
	*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;

	return IRValueValue(newStaticVar.valueIdx, typeTableIdx);
}

IRValue IRValueProcedure(Context *context, s32 procedureIdx)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_PROCEDURE;
	result.procedureIdx = procedureIdx;
	result.typeTableIdx = context->procedures[procedureIdx].typeTableIdx;
	return result;
}

IRValue IRValueNewValue(Context *context, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = 0)
{
	u32 newValue = NewValue(context, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.value = { newValue };
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueNewValue(Context *context, String name, u32 typeTableIdx, u32 flags,
		u32 immitateValueIdx = 0)
{
	u32 newValueIdx = NewValue(context, name, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.value = { newValueIdx };
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueTypeOf(Context *context, u32 typeTableIdx)
{
	static u32 typeInfoPointerTypeIdx = GetTypeInfoPointerOf(context,
			TYPETABLEIDX_TYPE_INFO_STRUCT);
	u32 typeValueIdx = context->typeTable[typeTableIdx].valueIdx;
	return IRValueValue(typeValueIdx, typeInfoPointerTypeIdx);
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression);

IRValue IRDereferenceValue(Context *context, IRValue in)
{
	TypeInfo pointerTypeInfo = context->typeTable[in.typeTableIdx];
	ASSERT(pointerTypeInfo.typeCategory == TYPECATEGORY_POINTER);
	u32 pointedTypeIdx = pointerTypeInfo.pointerInfo.pointedTypeTableIdx;

	// This assert is cool and all, but would mean unnecesarily assigning types to things as we
	// generate IR.
	//ASSERT(pointedTypeIdx != TYPETABLEIDX_VOID);

	if (in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
	{
		// Copy first to a register always. Since this is a very short lived value, we should always
		// be able to store it in a register.

		// It's important that the value is in a register before dereferencing, because values in
		// memory are already dereferenced. If the value is in memory, first we dereference the
		// memory to get the pointer itself:
		//     mov rax, [rsp+123]
		// then we can dereference that pointer once it's on a register:
		//     mov out, [rax]

		String name = TPrintF("_deref_forcereg_%S", context->values[in.value.valueIdx].name);
		u32 tempValueIdx = NewValue(context, name, in.typeTableIdx, VALUEFLAGS_TRY_IMMITATE |
				VALUEFLAGS_FORCE_REGISTER, in.value.valueIdx);
		IRValue tmpValue = IRValueValue(tempValueIdx, in.typeTableIdx);

		IRInstruction inst = { IRINSTRUCTIONTYPE_ASSIGNMENT };
		inst.assignment.dst = tmpValue;
		inst.assignment.src = in;
		*AddInstruction(context) = inst;

		name = TPrintF("_deref_%S", context->values[in.value.valueIdx].name);
		u32 newValueIdx = NewValue(context, name, in.typeTableIdx, VALUEFLAGS_TRY_IMMITATE,
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

IRValue IRPointerToValue(Context *context, IRValue in)
{
	ASSERT(in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
	u32 pointerTypeIdx = GetTypeInfoPointerOf(context, in.typeTableIdx);

	IRValue result = IRValueNewValue(context, "_pointerof"_s, pointerTypeIdx, 0);

	IRInstruction addressInst = {};
	addressInst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;
	addressInst.unaryOperation.in = in;
	addressInst.unaryOperation.out = result;
	*AddInstruction(context) = addressInst;

	return result;
}

void IRDoAssignment(Context *context, IRValue dstValue, IRValue srcValue);
IRValue IRDoMemberAccess(Context *context, IRValue structValue, StructMember structMember)
{
	ASSERT(structValue.valueType == IRVALUETYPE_VALUE ||
		   structValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE);

	IRAddComment(context, TPrintF("Accessing struct member \"%S.%S\"",
				context->values[structValue.value.valueIdx].name, structMember.name));

	s64 offset = structMember.offset + structValue.value.offset;
	IRValue result = IRValueDereference(structValue.value.valueIdx, structMember.typeTableIdx,
			offset);
	result = IRPointerToValue(context, result);
	result.valueType = IRVALUETYPE_VALUE_DEREFERENCE;
	result.typeTableIdx = structMember.typeTableIdx;
	return result;
}

IRValue IRDoArrayAccess(Context *context, IRValue arrayValue, IRValue indexValue, u32 elementTypeIdx)
{
	TypeInfo arrayTypeInfo = context->typeTable[arrayValue.typeTableIdx];
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
		IRAddComment(context, "Addressing dynamic array"_s);

		TypeInfo arrayStructTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];
		StructMember dataMember = arrayStructTypeInfo.structInfo.members[1];

		arrayValue = IRDoMemberAccess(context, arrayValue, dataMember);
		arrayValue = IRDereferenceValue(context, arrayValue);
		arrayValue.typeTableIdx = pointerToElementTypeIdx;
	}

	s64 elementSize = context->typeTable[elementTypeIdx].size;

	if (indexValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		return IRValueDereference(arrayValue.value.valueIdx, elementTypeIdx,
				indexValue.immediate * elementSize);
	}
	else if ((indexValue.valueType == IRVALUETYPE_VALUE ||
			indexValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE) &&
			CountOnes(elementSize) == 1 && elementSize <= 8)
	{
		// @Todo: move x64 specifics like element size limitations and force to register to x64
		// backend.
		IRValue indexForceReg = IRValueNewValue(context, "_idx"_s, TYPETABLEIDX_S64,
				VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, indexValue.value.valueIdx);
		IRDoAssignment(context, indexForceReg, indexValue);

		u32 flags = context->values[arrayValue.value.valueIdx].flags;
		if (flags & VALUEFLAGS_ON_STATIC_STORAGE)
		{
			// @Improve: This is x64 specific. Move to x64 backend.
			// Apparently with [array+index*size] syntax, array ends up being 32 bit displacement.
			// We can't expect a part of the data segment to be that close.
			IRValue arrayForceReg = IRValueNewValue(context, "_arr"_s,
					GetTypeInfoPointerOf(context, arrayValue.typeTableIdx),
					VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, arrayValue.value.valueIdx);
			IRInstruction leaInst = { IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS };
			leaInst.unaryOperation.in = arrayValue;
			leaInst.unaryOperation.out = arrayForceReg;
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
		IRDoAssignment(context, offsetValue, indexValue);
	else
	{
		IRInstruction multiplyInst = { IRINSTRUCTIONTYPE_MULTIPLY };
		multiplyInst.binaryOperation.left  = indexValue;
		multiplyInst.binaryOperation.right = IRValueImmediate(elementSize);
		multiplyInst.binaryOperation.out   = offsetValue;
		*AddInstruction(context) = multiplyInst;
	}

	IRValue pointerToElementValue = IRValueNewValue(context, "_array_element"_s,
			pointerToElementTypeIdx, 0);
	IRInstruction addOffsetInst = {};
	addOffsetInst.type = IRINSTRUCTIONTYPE_ADD;
	addOffsetInst.binaryOperation.left = IRPointerToValue(context, arrayValue);
	addOffsetInst.binaryOperation.right = offsetValue;
	addOffsetInst.binaryOperation.out = pointerToElementValue;
	*AddInstruction(context) = addOffsetInst;

	return IRValueDereference(pointerToElementValue.value.valueIdx, elementTypeIdx);
}

inline void IRPushValueIntoStack(Context *context, u32 valueIdx)
{
	IRInstruction inst;
	inst.type = IRINSTRUCTIONTYPE_PUSH_VALUE;
	inst.pushValue.valueIdx = valueIdx;
	*AddInstruction(context) = inst;
}

u32 IRAddTempValue(Context *context, String name, u32 typeTableIdx, u8 flags)
{
	u32 valueIdx = NewValue(context, name, typeTableIdx, flags);
	IRPushValueIntoStack(context, valueIdx);
	return valueIdx;
}

void IRDoAssignment(Context *context, IRValue dstValue, IRValue srcValue)
{
	// Cast to Any
	if (dstValue.typeTableIdx == TYPETABLEIDX_ANY_STRUCT &&
		srcValue.typeTableIdx != TYPETABLEIDX_ANY_STRUCT)
	{
		IRAddComment(context, "Wrapping in Any"_s);
		TypeInfo anyTypeInfo = context->typeTable[TYPETABLEIDX_ANY_STRUCT];

		// Access typeInfo member
		IRValue typeInfoMember = IRDoMemberAccess(context, dstValue,
				anyTypeInfo.structInfo.members[0]);

		// Write pointer to typeInfo to it
		IRInstruction typeAssignInst = {};
		typeAssignInst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;
		typeAssignInst.unaryOperation.in = IRValueTypeOf(context, srcValue.typeTableIdx);
		typeAssignInst.unaryOperation.out = typeInfoMember;
		*AddInstruction(context) = typeAssignInst;

		// Access data member
		static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);
		IRValue dataMember = IRDoMemberAccess(context, dstValue,
				anyTypeInfo.structInfo.members[1]);
		dataMember.typeTableIdx = voidPtrTypeIdx;

		IRValue dataValue = srcValue;
		TypeInfo dataTypeInfo = context->typeTable[srcValue.typeTableIdx];

		if (dataTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
			dataTypeInfo.typeCategory != TYPECATEGORY_UNION &&
			dataTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
		{
			// If data isn't in memory, copy to a variable
			if (IRShouldPassByCopy(context, dataValue.typeTableIdx))
			{
				static u64 tempVarForAnyUniqueID = 0;
				String tempVarName = TPrintF("_tempVarForAny%llu", tempVarForAnyUniqueID++);
				u32 tempValue = IRAddTempValue(context, tempVarName, srcValue.typeTableIdx,
						VALUEFLAGS_FORCE_MEMORY);
				IRValue tempVarIRValue = IRValueValue(tempValue, srcValue.typeTableIdx);

				IRInstruction dataCopyInst = {};
				dataCopyInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				dataCopyInst.assignment.src = dataValue;
				dataCopyInst.assignment.dst = tempVarIRValue;
				*AddInstruction(context) = dataCopyInst;

				dataValue = tempVarIRValue;

				IRInstruction dataAssignInst = {};
				dataAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				dataAssignInst.assignment.src = IRPointerToValue(context, dataValue);
				dataAssignInst.assignment.dst = dataMember;
				*AddInstruction(context) = dataAssignInst;
			}
			// Small primitives are stored directly on the Any struct.
			else
			{
				dataMember.typeTableIdx = dataValue.typeTableIdx;

				IRInstruction dataAssignInst = {};
				dataAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				dataAssignInst.assignment.src = dataValue;
				dataAssignInst.assignment.dst = dataMember;
				*AddInstruction(context) = dataAssignInst;
			}
		}
		// These are already in memory, just save a pointer
		else
		{
			IRInstruction dataAssignInst = {};
			dataAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			dataAssignInst.assignment.src = IRPointerToValue(context, dataValue);
			dataAssignInst.assignment.dst = dataMember;
			*AddInstruction(context) = dataAssignInst;
		}

		return;
	}

	dstValue.typeTableIdx = StripAllAliases(context, dstValue.typeTableIdx);
	srcValue.typeTableIdx = StripAllAliases(context, srcValue.typeTableIdx);

	// Cast static array to dynamic array
	TypeInfo dstTypeInfo = context->typeTable[dstValue.typeTableIdx];
	TypeInfo srcTypeInfo = context->typeTable[srcValue.typeTableIdx];
	if (dstTypeInfo.typeCategory  == TYPECATEGORY_ARRAY &&
		srcTypeInfo.typeCategory == TYPECATEGORY_ARRAY &&
		dstTypeInfo.arrayInfo.count  == 0 &&
		srcTypeInfo.arrayInfo.count != 0)
	{
		TypeInfo dynamicArrayTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];

		// Size
		StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
		IRValue sizeMember = IRDoMemberAccess(context, dstValue, sizeStructMember);
		IRValue sizeValue = IRValueImmediate(srcTypeInfo.arrayInfo.count);
		IRDoAssignment(context, sizeMember, sizeValue);

		// Data
		StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
		IRValue dataMember = IRDoMemberAccess(context, dstValue, dataStructMember);
		IRValue dataValue = IRPointerToValue(context, srcValue);
		IRDoAssignment(context, dataMember, dataValue);

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

		u64 size = context->typeTable[srcValue.typeTableIdx].size;
		IRValue sizeValue = IRValueImmediate(size);

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_COPY_MEMORY;
		inst.copyMemory.src = IRPointerToValue(context, srcValue);
		inst.copyMemory.dst = IRPointerToValue(context, dstValue);
		inst.copyMemory.size = sizeValue;

		*AddInstruction(context) = inst;
	}
	else if (dstTypeInfo.typeCategory == TYPECATEGORY_FLOATING &&
			 srcValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		// Can't just pretend we can assign an integer immediate to a floating point value. We need
		// to convert it to a floating point number first.
		// @Check: Maybe we should do this on backend?
		IRInstruction inst = {};

		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.dst = dstValue;
		inst.assignment.src = IRValueImmediateFloat(context, (f64)srcValue.immediate);

		*AddInstruction(context) = inst;

	}
	else
	{
		IRInstruction inst = {};

		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.dst = dstValue;
		inst.assignment.src = srcValue;

		if (srcValue.valueType == IRVALUETYPE_PROCEDURE)
			inst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;

		*AddInstruction(context) = inst;
	}
}

void IRInsertLabelInstruction(Context *context, IRLabel *label)
{
	label->instructionIdx = BucketArrayCount(&GetProcedure(context, label->procedureIdx)->instructions);
	IRInstruction result;
	result.type = IRINSTRUCTIONTYPE_LABEL;
	result.label = label;
	*AddInstruction(context) = result;
}

IRValue IRDoCast(Context *context, IRValue value, u32 typeTableIdx)
{
	u32 tempValueIdx = IRAddTempValue(context, "_cast"_s, typeTableIdx, 0);
	IRValue result = IRValueValue(tempValueIdx, typeTableIdx, 0);
	IRDoAssignment(context, result, value);
	return result;
}

IRValue IRInstructionFromBinaryOperation(Context *context, ASTExpression *expression, IRValue outValue)
{
	IRValue result = {};

	ASTExpression *leftHand  = expression->binaryOperation.leftHand;
	ASTExpression *rightHand = expression->binaryOperation.rightHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
	{
		IRValue irValue = IRGenFromExpression(context, leftHand);

		TypeInfo structTypeInfo = context->typeTable[irValue.typeTableIdx];
		if (irValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE &&
			structTypeInfo.typeCategory == TYPECATEGORY_POINTER)
		{
			// Dereference the pointer to the struct
			String name = TPrintF("_derefstrctptr_%S", context->values[irValue.value.valueIdx].name);
			u32 newValueIdx = NewValue(context, name, irValue.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			IRValue newValue = IRValueValue(newValueIdx, irValue.typeTableIdx);

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			inst.assignment.dst = newValue;
			inst.assignment.src = irValue;
			*AddInstruction(context) = inst;

			u32 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
			irValue = IRValueValue(newValueIdx, pointedTypeIdx);
		}

		ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
		ASSERT(rightHand->identifier.type == NAMETYPE_STRUCT_MEMBER);
		StructMember structMember = *rightHand->identifier.structMember;

		result = IRDoMemberAccess(context, irValue, structMember);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
	{
		IRValue arrayValue = IRGenFromExpression(context, leftHand);
		IRValue indexValue = IRGenFromExpression(context, rightHand);

		if (context->typeTable[arrayValue.typeTableIdx].typeCategory == TYPECATEGORY_POINTER)
		{
			// Dereference the pointer to the array
			arrayValue = IRDereferenceValue(context, arrayValue);
		}

		result = IRDoArrayAccess(context, arrayValue, indexValue, expression->typeTableIdx);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_AND)
	{
		IRLabel *assignZeroLabel = NewLabel(context, "assignZero"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		IRInstruction *jumpIfZeroInst = AddInstruction(context);
		jumpIfZeroInst->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jumpIfZeroInst->conditionalJump.label = assignZeroLabel;
		jumpIfZeroInst->conditionalJump.condition = leftValue;

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		IRInstruction *jumpIfZeroInst2 = AddInstruction(context);
		jumpIfZeroInst2->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jumpIfZeroInst2->conditionalJump.label = assignZeroLabel;
		jumpIfZeroInst2->conditionalJump.condition = rightValue;

		IRDoAssignment(context, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		IRInstruction *jumpToEndInst = AddInstruction(context);
		jumpToEndInst->type = IRINSTRUCTIONTYPE_JUMP;
		jumpToEndInst->jump.label = skipAssignZeroLabel;

		IRInsertLabelInstruction(context, assignZeroLabel);

		IRDoAssignment(context, outValue, IRValueImmediate(0));

		IRInsertLabelInstruction(context, skipAssignZeroLabel);

		result = outValue;
	}
	else if (expression->binaryOperation.op == TOKEN_OP_OR)
	{
		IRLabel *assignZeroLabel = NewLabel(context, "assignZero"_s);
		IRLabel *skipRightLabel = NewLabel(context, "skipRight"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		IRInstruction *jumpIfNotZeroInst = AddInstruction(context);
		jumpIfNotZeroInst->type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO;
		jumpIfNotZeroInst->conditionalJump.label = skipRightLabel;
		jumpIfNotZeroInst->conditionalJump.condition = leftValue;

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		IRInstruction *jumpIfZeroInst2 = AddInstruction(context);
		jumpIfZeroInst2->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jumpIfZeroInst2->conditionalJump.label = assignZeroLabel;
		jumpIfZeroInst2->conditionalJump.condition = rightValue;

		IRInsertLabelInstruction(context, skipRightLabel);

		IRDoAssignment(context, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		IRInstruction *jumpToEndInst = AddInstruction(context);
		jumpToEndInst->type = IRINSTRUCTIONTYPE_JUMP;
		jumpToEndInst->jump.label = skipAssignZeroLabel;

		IRInsertLabelInstruction(context, assignZeroLabel);

		IRDoAssignment(context, outValue, IRValueImmediate(0));

		IRInsertLabelInstruction(context, skipAssignZeroLabel);

		result = outValue;
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT_AND)
	{
		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		IRInstruction *jumpIfZeroInst = AddInstruction(context);
		jumpIfZeroInst->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jumpIfZeroInst->conditionalJump.label = skipAssignZeroLabel;
		jumpIfZeroInst->conditionalJump.condition = leftValue;

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		IRInstruction *jumpIfZeroInst2 = AddInstruction(context);
		jumpIfZeroInst2->type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO;
		jumpIfZeroInst2->conditionalJump.label = skipAssignZeroLabel;
		jumpIfZeroInst2->conditionalJump.condition = rightValue;

		IRDoAssignment(context, leftValue, IRValueImmediate(0));

		IRInsertLabelInstruction(context, skipAssignZeroLabel);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT_OR)
	{
		IRLabel *skipAssignOneLabel = NewLabel(context, "skipAssignOne"_s);

		IRValue leftValue  = IRGenFromExpression(context, leftHand);

		IRInstruction *jumpIfZeroInst = AddInstruction(context);
		jumpIfZeroInst->type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO;
		jumpIfZeroInst->conditionalJump.label = skipAssignOneLabel;
		jumpIfZeroInst->conditionalJump.condition = leftValue;

		IRValue rightValue = IRGenFromExpression(context, rightHand);

		IRInstruction *jumpIfZeroInst2 = AddInstruction(context);
		jumpIfZeroInst2->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jumpIfZeroInst2->conditionalJump.label = skipAssignOneLabel;
		jumpIfZeroInst2->conditionalJump.condition = rightValue;

		IRDoAssignment(context, leftValue, IRValueImmediate(1));

		IRInsertLabelInstruction(context, skipAssignOneLabel);
	}
	else
	{
#if DEBUG_BUILD
		TypeInfo leftTypeInfo = context->typeTable[leftHand->typeTableIdx];
		ASSERT(leftTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
			   leftTypeInfo.typeCategory != TYPECATEGORY_UNION);
#endif

		IRValue left  = IRGenFromExpression(context, leftHand);
		IRValue right = IRGenFromExpression(context, rightHand);

		if (left.typeTableIdx != right.typeTableIdx)
			right = IRDoCast(context, right, left.typeTableIdx);

		IRInstruction inst = {};
		inst.binaryOperation.left  = left;
		inst.binaryOperation.right = right;

		switch (expression->binaryOperation.op)
		{
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
			Value *v = &context->values[inst.binaryOperation.left.value.valueIdx];
			v->flags |= VALUEFLAGS_TRY_IMMITATE;
			v->tryImmitateValueIdx = outValue.value.valueIdx;
		}

		inst.binaryOperation.out = outValue;
		*AddInstruction(context) = inst;

		if (expression->binaryOperation.op >= TOKEN_OP_ASSIGNMENT_Begin &&
			expression->binaryOperation.op <= TOKEN_OP_ASSIGNMENT_End)
		{
			IRDoAssignment(context, inst.binaryOperation.left, outValue);
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

		IRInsertLabelInstruction(context, skipRightHandLabel);
		return;
	}

	if (conditionExp->nodeType == ASTNODETYPE_BINARY_OPERATION)
	{
		IRInstruction jump = {};
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
#if 0
	// @Check: Don't remember why this wasn't working.
	else if (conditionExp->nodeType == ASTNODETYPE_UNARY_OPERATION)
	{
		IRInstruction jump = {};
		switch (conditionExp->unaryOperation.op)
		{
		case TOKEN_OP_NOT:
			jump.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO;
			break;
		default:
			goto defaultConditionEvaluation;
		}

		IRValue conditionResult = IRGenFromExpression(context,
				conditionExp->unaryOperation.expression);
		jump.conditionalJump.label = label;
		jump.conditionalJump.condition = conditionResult;
		*AddInstruction(context) = jump;
		return;
	}
#endif

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
	ASSERT(astProcCall.callType == CALLTYPE_STATIC);
	Procedure *procedure = GetProcedure(context, astProcCall.procedureIdx);

	// Check for cyclic inline procedure calls
	for (int i = 0; i < context->irProcedureStack.size; ++i)
	{
		if (context->irProcedureStack[i].procedureIdx == astProcCall.procedureIdx)
		{
			LogErrorNoCrash(context, astProcCall.loc, "Cyclic inlined procedure call"_s);
			LogNote(context, context->irProcedureStack[i].definitionLoc, "First called here"_s);
			CRASH;
		}
	}

	TypeInfoProcedure procTypeInfo = context->typeTable[procedure->typeTableIdx].procedureInfo;
	IRValue returnValue = IRValueNewValue(context, "_inline_return"_s, procTypeInfo.returnTypeTableIdx, 0);
	IRPushValueIntoStack(context, returnValue.value.valueIdx);
	bool isVarargs = procTypeInfo.isVarargs;

	IRProcedureScope *stackTop = &context->irProcedureStack[context->irProcedureStack.size - 1];
	Procedure *callingProcedure = GetProcedure(context, stackTop->procedureIdx);

	s32 tempProcIdx = (s32)BucketArrayCount(&context->procedures);
	Procedure *tempProc = BucketArrayAdd(&context->procedures);
	{
		Procedure p = *procedure;
		p.instructions = callingProcedure->instructions;
		p.name = TPrintF("_%S_inline", procedure->name);
		p.returnValueIdx = returnValue.value.valueIdx;
		if (procedure->parameterValues.size)
			DynamicArrayInit(&p.parameterValues, procedure->parameterValues.size);
		*tempProc = p;
	}

	// Support both varargs and default parameters here
	s32 procParamCount = (s32)procTypeInfo.parameters.size;
	s32 callParamCount = (s32)astProcCall.arguments.size;

	// Set up parameters
	s64 normalArgumentsCount = Min(callParamCount, procParamCount);
	for (int argIdx = 0; argIdx < normalArgumentsCount; ++argIdx)
	{
		ASTExpression *arg = &astProcCall.arguments[argIdx];
		IRValue argValue = IRGenFromExpression(context, arg);

		if (argValue.valueType == IRVALUETYPE_VALUE)
			*DynamicArrayAdd(&tempProc->parameterValues) = argValue.value.valueIdx;
		else
		{
			IRValue param = IRValueNewValue(context,
					procTypeInfo.parameters[argIdx].typeTableIdx, 0);
			IRDoAssignment(context, param, argValue);
			*DynamicArrayAdd(&tempProc->parameterValues) = param.value.valueIdx;
		}
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
		IRValue param = IRValueNewValue(context, procParam.typeTableIdx, 0);
		IRDoAssignment(context, param, arg);
		*DynamicArrayAdd(&tempProc->parameterValues) = param.value.valueIdx;
	}

	// Varargs
	if (isVarargs)
	{
		static u32 anyPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_ANY_STRUCT);
		static u32 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

		s64 varargsCount = astProcCall.arguments.size - procParamCount;

		if (varargsCount == 1)
		{
			ASTExpression *varargsArrayExp = &astProcCall.arguments[procParamCount];
			if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx)
			{
				IRAddComment(context, "Forwarding varargs array"_s);

				IRValue varargsArray = IRGenFromExpression(context, varargsArrayExp);

				ASSERT(varargsArray.valueType == IRVALUETYPE_VALUE ||
					   varargsArray.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
				*DynamicArrayAdd(&tempProc->parameterValues) = varargsArray.value.valueIdx;

				goto skipGeneratingVarargsArray;
			}
		}

		IRAddComment(context, "Build varargs array"_s);

		IRValue pointerToBuffer;
		if (varargsCount > 0)
		{
			// Allocate stack space for buffer
			u32 bufferValueIdx = IRAddTempValue(context, "_varargsBuffer"_s,
					GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, varargsCount), VALUEFLAGS_FORCE_MEMORY);
			IRValue bufferIRValue = IRValueValue(context, bufferValueIdx);

			// Fill the buffer
			int nonVarargs = (int)procParamCount;
			for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
			{
				ASTExpression *arg = &astProcCall.arguments[argIdx + nonVarargs];

				IRValue bufferIndexValue = IRValueImmediate(argIdx);
				IRValue bufferSlotValue = IRDoArrayAccess(context, bufferIRValue, bufferIndexValue,
						TYPETABLEIDX_ANY_STRUCT);

				IRValue rightValue = IRGenFromExpression(context, arg);
				IRDoAssignment(context, bufferSlotValue, rightValue);
			}

			pointerToBuffer = IRPointerToValue(context, bufferIRValue);
		}
		else
			pointerToBuffer = IRValueImmediate(0, GetTypeInfoPointerOf(context, anyPointerTypeIdx));

		// By now we should have the buffer with all the varargs as Any structs.
		// Now we put it into a dynamic array struct.

		// Allocate stack space for array
		u32 arrayValueIdx = IRAddTempValue(context, "_varargsArray"_s, arrayOfAnyTypeIdx,
				VALUEFLAGS_FORCE_MEMORY);
		IRValue arrayIRValue = IRValueValue(context, arrayValueIdx);

		TypeInfo dynamicArrayTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];
		// Size
		{
			StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
			IRValue sizeMember = IRDoMemberAccess(context, arrayIRValue, sizeStructMember);
			IRValue sizeValue = IRValueImmediate(varargsCount);
			IRDoAssignment(context, sizeMember, sizeValue);
		}

		// Data
		{
			StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
			IRValue dataMember = IRDoMemberAccess(context, arrayIRValue, dataStructMember);
			IRValue dataValue = pointerToBuffer;
			IRDoAssignment(context, dataMember, dataValue);
		}

		// Pass array as parameter!
		*DynamicArrayAdd(&tempProc->parameterValues) = arrayValueIdx;
	}
skipGeneratingVarargsArray:

	// IRGen
	IRProcedureScope *newScope = PushIRProcedure(context, astProcCall.loc, tempProcIdx);
	IRLabel *returnLabel = NewLabel(context, "inline_return"_s);
	newScope->returnLabel = returnLabel;

	ASSERT(astProcCall.astBodyInlineCopy);
	IRGenFromExpression(context, astProcCall.astBodyInlineCopy);

	IRInstruction *returnLabelInst = AddInstruction(context);
	returnLabel->instructionIdx = BucketArrayCount(&procedure->instructions) - 1;
	returnLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
	returnLabelInst->label = returnLabel;

	PopIRProcedure(context);
	callingProcedure->instructions = tempProc->instructions;

	// Remove temp procedure
	--DynamicArrayBack(&context->procedures.buckets)->size;

	return returnValue;
}

IRValue IRValueFromConstant(Context *context, Constant constant)
{
	IRValue result = {};
	result.typeTableIdx = constant.typeTableIdx;
	switch (constant.type)
	{
	case CONSTANTTYPE_INTEGER:
		result = IRValueImmediate(constant.valueAsInt);
		break;
	case CONSTANTTYPE_FLOATING:
		result.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
		result.immediateFloat = constant.valueAsFloat;
		break;
	case CONSTANTTYPE_GROUP:
	{
		result.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
		u64 membersCount = constant.valueAsGroup.size;
		ArrayInit(&result.immediateStructMembers, membersCount);
		result.immediateStructMembers.size = membersCount;
		for (int i = 0; i < membersCount; ++i)
			result.immediateStructMembers[i] =
				IRValueFromConstant(context, constant.valueAsGroup[i]);
	} break;
	default:
		ASSERT(!"Unknown constant type");
	}
	return result;
}

void IRFillValueWithGroupLiteral(Context *context, IRValue value, ASTLiteral astLiteral)
{
	u32 groupTypeIdx = value.typeTableIdx;
	ASSERT(groupTypeIdx >= TYPETABLEIDX_Begin);
	TypeInfo groupTypeInfo = context->typeTable[groupTypeIdx];

	if (groupTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		groupTypeInfo.typeCategory == TYPECATEGORY_UNION)
	{
		u64 nonNamedCount = astLiteral.members.size;
		struct NamedMember
		{
			String name;
			ASTExpression *expr;
		};
		DynamicArray<NamedMember, PhaseAllocator> namedMembers;
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
		DynamicArray<StructStackFrame, PhaseAllocator> structStack;
		DynamicArrayInit(&structStack, 8);
		*DynamicArrayAdd(&structStack) = { value, groupTypeIdx, 0 };

		int memberIdx = 0;
		while (structStack.size > 0)
		{
			StructStackFrame currentFrame = structStack[structStack.size - 1];
			TypeInfo currentStructTypeInfo = context->typeTable[currentFrame.structTypeIdx];

			if ((currentFrame.idx >= currentStructTypeInfo.structInfo.members.size) ||
				(currentStructTypeInfo.typeCategory == TYPECATEGORY_UNION && currentFrame.idx > 0)) // Only first member for unions
			{
				// Pop struct frame
				--structStack.size;
				continue;
			}

			StructMember currentMember = currentStructTypeInfo.structInfo.members[currentFrame.idx];
			TypeCategory memberTypeCat = context->typeTable[currentMember.typeTableIdx].typeCategory;

			if (memberTypeCat == TYPECATEGORY_STRUCT || memberTypeCat == TYPECATEGORY_UNION)
			{
				// Push struct frame
				++structStack[structStack.size - 1].idx;
				IRValue innerStructValue = IRDoMemberAccess(context, currentFrame.irValue, currentMember);
				*DynamicArrayAdd(&structStack) = { innerStructValue, currentMember.typeTableIdx, 0 };
				continue;
			}

			IRValue memberValue = IRDoMemberAccess(context, currentFrame.irValue, currentMember);
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
			IRDoAssignment(context, memberValue, src);

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
			IRValue elementValue = IRDoArrayAccess(context, value, indexIRValue,
					elementTypeIdx);
			IRValue src = IRGenFromExpression(context, literalMemberExp);
			IRDoAssignment(context, elementValue, src);
		}
	}
	else
		ASSERT(!"Invalid type to the left of group literal. Type checking should catch this");
}

void IRAssignmentFromExpression(Context *context, IRValue dstValue, ASTExpression *srcExpression)
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
		IRDoAssignment(context, dstValue, srcValue);
	}
}

void IRGenProcedure(Context *context, s32 procedureIdx, SourceLocation loc)
{
	Procedure *procedure = GetProcedure(context, procedureIdx);
	BucketArrayInit(&procedure->instructions);

	IRProcedureScope *currentProc = PushIRProcedure(context, loc, procedureIdx);
	IRLabel *returnLabel = NewLabel(context, "return"_s);
	currentProc->returnLabel = returnLabel;

	for (int i = 0; i < procedure->parameterValues.size; ++i)
	{
		s32 paramValueIdx = procedure->parameterValues[i];
		IRPushValueIntoStack(context, paramValueIdx);
	}

	s32 returnValueIdx = procedure->returnValueIdx;
	Value *returnValue = &context->values[returnValueIdx];
	if (IRShouldPassByCopy(context, returnValue->typeTableIdx))
	{
		returnValue->flags |= VALUEFLAGS_PARAMETER_BY_COPY;
		returnValue->typeTableIdx = GetTypeInfoPointerOf(context, returnValue->typeTableIdx);
	}
	IRPushValueIntoStack(context, returnValueIdx);

	if (procedure->astBody)
	{
		IRGenFromExpression(context, procedure->astBody);

		IRInstruction *returnLabelInst = AddInstruction(context);
		returnLabel->instructionIdx = BucketArrayCount(&procedure->instructions) - 1;
		returnLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		returnLabelInst->label = returnLabel;

		// Return
		IRInstruction returnInst;
		returnInst.type = IRINSTRUCTIONTYPE_RETURN;
		*AddInstruction(context) = returnInst;
	}

	PopIRProcedure(context);
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		// @Cleanup
		if (expression->staticDefinition.expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
			IRGenFromExpression(context, expression->staticDefinition.expression);
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		s32 procedureIdx = expression->procedureDeclaration.procedureIdx;
		IRGenProcedure(context, procedureIdx, expression->any.loc);
	} break;
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		s32 procedureIdx = expression->operatorOverload.procedureIdx;
		IRGenProcedure(context, procedureIdx, expression->any.loc);
	} break;
	case ASTNODETYPE_BLOCK:
	{
		PushIRScope(context);
		IRScope *currentScope = &context->irStack[context->irStack.size - 1];
		currentScope->closeLabel = NewLabel(context, "closeScope"_s);

		*AddInstruction(context) = { IRINSTRUCTIONTYPE_PUSH_SCOPE };

		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			SourceLocation loc = expression->block.statements[i].any.loc;
			IRAddComment(context, GetSourceLine(context, loc.fileIdx, loc.line));

			IRGenFromExpression(context, &expression->block.statements[i]);
		}

		*AddInstruction(context) = { IRINSTRUCTIONTYPE_POP_SCOPE };

		IRProcedureScope stackTop = context->irProcedureStack[context->irProcedureStack.size - 1];
		Procedure *currentProc = GetProcedure(context, stackTop.procedureIdx);
		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = context->irStack.size - 1;
				stackIdx >= stackTop.irStackBase; --stackIdx)
		{
			if (context->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			if (stackTop.shouldReturnValueIdx == U32_MAX)
				stackTop.shouldReturnValueIdx = NewValue(context, TYPETABLEIDX_U8, 0);

			// Set should-return register to 0
			IRValue shouldReturnRegister = IRValueValue(context, stackTop.shouldReturnValueIdx);
			IRValue zero = IRValueImmediate(0);
			IRDoAssignment(context, shouldReturnRegister, zero);

			// Add close label
			IRInstruction *closeScopeLabelInst = AddInstruction(context);
			closeScopeLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
			closeScopeLabelInst->label = currentScope->closeLabel;

			// Run deferred statements
			for (s64 j = currentScope->deferredStatements.size - 1; j >= 0; --j)
			{
				IRGenFromExpression(context, currentScope->deferredStatements[j]);
			}

			// If should-return register is set, return
			if ((s64)(context->irStack.size - 2) != stackTop.irStackBase)
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
				for (int scopeIdx = (int)context->irStack.size - 2; ; --scopeIdx)
				{
					IRScope *scope = &context->irStack[scopeIdx];
					if (scopeIdx == stackTop.irStackBase || scope->deferredStatements.size > 0)
					{
						jumpInst.jump.label = scope->closeLabel;
						break;
					}
				}
				if (jumpInst.jump.label == nullptr)
					jumpInst.jump.label = stackTop.returnLabel;
				*AddInstruction(context) = jumpInst;

				IRInstruction *skipLabelInst = AddInstruction(context);
				skipLabel->instructionIdx = BucketArrayCount(&currentProc->instructions) - 1;
				skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
				skipLabelInst->label = skipLabel;
			}
		}

		PopIRScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		bool isGlobalScope = context->irProcedureStack.size == 0;
		if (isGlobalScope && !varDecl.isStatic && !varDecl.isExternal)
			LogError(context, expression->any.loc, "Global variables have to be static or external"_s);

		if (varDecl.isStatic)
		{
			IRStaticVariable newStaticVar = {};
			newStaticVar.valueIdx = varDecl.valueIdx;
			newStaticVar.initialValue.valueType = IRVALUETYPE_INVALID;

			// Initial value
			if (varDecl.astInitialValue)
			{
				if (varDecl.astInitialValue->literal.type == LITERALTYPE_STRING)
				{
					newStaticVar.initialValue = IRValueImmediateString(context,
							varDecl.astInitialValue->literal.string);
				}
				else
				{
					Constant constant  = TryEvaluateConstant(context, varDecl.astInitialValue);
					if (constant.type == CONSTANTTYPE_INVALID)
						LogError(context, varDecl.astInitialValue->any.loc,
								"Initial value of static variable isn't constant"_s);

					newStaticVar.initialValue = IRValueFromConstant(context, constant);
				}
			}

			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;
		}
		else if (varDecl.isExternal)
			*DynamicArrayAdd(&context->irExternalVariables) = varDecl.valueIdx;
		else
		{
			IRPushValueIntoStack(context, varDecl.valueIdx);

			// Initial value
			if (varDecl.astInitialValue)
			{
				if (varDecl.astInitialValue->nodeType != ASTNODETYPE_GARBAGE)
				{
					IRValue dstValue = IRValueValue(context, varDecl.valueIdx);
					IRAssignmentFromExpression(context, dstValue, varDecl.astInitialValue);
				}
			}
			else
			{
				TypeCategory dstTypeCat = context->typeTable[varDecl.typeTableIdx].typeCategory;
				if (dstTypeCat == TYPECATEGORY_STRUCT ||
					dstTypeCat == TYPECATEGORY_UNION ||
					dstTypeCat == TYPECATEGORY_ARRAY)
				{
					IRValue dstValue = IRValueValue(context, varDecl.valueIdx);
					u64 size = context->typeTable[dstValue.typeTableIdx].size;
					IRValue sizeValue = IRValueImmediate(size);

					IRInstruction inst = {};
					inst.type = IRINSTRUCTIONTYPE_ZERO_MEMORY;
					inst.zeroMemory.dst = IRPointerToValue(context, dstValue);
					inst.zeroMemory.size = sizeValue;

					*AddInstruction(context) = inst;
				}
				else
				{
					IRValue dstValue = IRValueValue(context, varDecl.valueIdx);
					IRValue srcValue = IRValueImmediate(0, varDecl.typeTableIdx);
					IRDoAssignment(context, dstValue, srcValue);
				}
			}
		}
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		switch (expression->identifier.type)
		{
		case NAMETYPE_STATIC_DEFINITION:
		{
			switch (expression->identifier.staticDefinition->definitionType)
			{
			case STATICDEFINITIONTYPE_CONSTANT:
			{
				Constant constant = expression->identifier.staticDefinition->constant;
				u32 typeTableIdx = StripAllAliases(context, expression->typeTableIdx);
				TypeCategory typeCat = context->typeTable[typeTableIdx].typeCategory;
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
					result = IRValueImmediate(constant.valueAsInt);
			} break;
			case STATICDEFINITIONTYPE_PROCEDURE:
			{
				result = IRValueProcedure(context,
						expression->identifier.staticDefinition->procedureIdx);
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
			TCValue tcValue = expression->identifier.tcValue;
			result = IRValueTCValue(context, tcValue);
			if (tcValue.type == TCVALUETYPE_PARAMETER &&
				context->values[result.value.valueIdx].flags & VALUEFLAGS_PARAMETER_BY_COPY)
				result = IRDereferenceValue(context, result);
		} break;
		default:
			ASSERT(false);
		}
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall *astProcCall = &expression->procedureCall;
		IRInstruction procCallInst = {};
		u32 procTypeIdx;
		switch (astProcCall->callType)
		{
		case CALLTYPE_STATIC:
		{
			Procedure *proc = GetProcedure(context, astProcCall->procedureIdx);
			if (proc->isInline)
				return IRDoInlineProcedureCall(context, *astProcCall);

			procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
			procCallInst.procedureCall.procedureIdx = astProcCall->procedureIdx;
			procTypeIdx = proc->typeTableIdx;
		} break;
		case CALLTYPE_VALUE:
		{
			procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT;
			IRValue irValue = IRValueTCValue(context, astProcCall->tcValue);
			procCallInst.procedureCall.procIRValue = irValue;
			procTypeIdx = irValue.typeTableIdx;
		} break;
		case CALLTYPE_ASTEXPRESSION:
		{
			procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT;
			IRValue irValue = IRGenFromExpression(context, astProcCall->expression);
			procCallInst.procedureCall.procIRValue = irValue;
			procTypeIdx = irValue.typeTableIdx;
		} break;
		default:
			ASSERT(false);
		}

		bool isReturnByCopy = IRShouldPassByCopy(context, expression->typeTableIdx);

		ASSERT(context->typeTable[procTypeIdx].typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = context->typeTable[procTypeIdx].procedureInfo;
		bool isVarargs = procTypeInfo.isVarargs;

		// Support both varargs and default parameters here
		s32 procParamCount = (s32)procTypeInfo.parameters.size;
		s32 callParamCount = (s32)astProcCall->arguments.size;
		s32 paramCount = Max(procParamCount, callParamCount) + isReturnByCopy + isVarargs;
		ArrayInit(&procCallInst.procedureCall.parameters, paramCount);

		// Remember parameter count because we need space for them in the stack
		IRProcedureScope stackTop = context->irProcedureStack[context->irProcedureStack.size - 1];

		// Return value
		procCallInst.procedureCall.out.valueType = IRVALUETYPE_INVALID;
		if (expression->typeTableIdx != TYPETABLEIDX_VOID)
		{
			if (isReturnByCopy)
			{
				// Allocate stack for return value
				u32 tempValueIdx = IRAddTempValue(context, "_returnByCopy"_s, expression->typeTableIdx,
						VALUEFLAGS_FORCE_MEMORY);
				IRValue tempVarIRValue = IRValueValue(tempValueIdx, expression->typeTableIdx);

				// Add register as parameter
				*ArrayAdd(&procCallInst.procedureCall.parameters) =
					IRPointerToValue(context, tempVarIRValue);

				result = IRValueValue(tempValueIdx, expression->typeTableIdx);
			}
			else
			{
				u32 returnValueIdx = IRAddTempValue(context, "_return"_s, expression->typeTableIdx, 0);
				procCallInst.procedureCall.out = IRValueValue(returnValueIdx, expression->typeTableIdx);
				result = procCallInst.procedureCall.out;
			}
		}

		// Set up parameters
		s64 normalArgumentsCount = Min(callParamCount, procParamCount);
		for (int argIdx = 0; argIdx < normalArgumentsCount; ++argIdx)
		{
			ASTExpression *arg = &astProcCall->arguments[argIdx];
			u32 argTypeTableIdx = procTypeInfo.parameters[argIdx].typeTableIdx;

			IRValue param = IRGenFromExpression(context, arg);
			if (param.typeTableIdx != argTypeTableIdx)
				param = IRDoCast(context, param, argTypeTableIdx);
			*ArrayAdd(&procCallInst.procedureCall.parameters) = param;
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
			*ArrayAdd(&procCallInst.procedureCall.parameters) = param;
		}

		// Varargs
		if (isVarargs)
		{
			s64 varargsCount = astProcCall->arguments.size - procParamCount;

			static u32 anyPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_ANY_STRUCT);
			static u32 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

			if (varargsCount == 1)
			{
				ASTExpression *varargsArrayExp = &astProcCall->arguments[procParamCount];
				if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx)
				{
					IRAddComment(context, "Forwarding varargs array"_s);

					IRValue varargsArray = IRGenFromExpression(context, varargsArrayExp);

					ASSERT(varargsArray.valueType == IRVALUETYPE_VALUE ||
						   varargsArray.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
					*ArrayAdd(&procCallInst.procedureCall.parameters) = varargsArray;

					goto skipGeneratingVarargsArray;
				}
			}

			IRAddComment(context, "Build varargs array"_s);

			IRValue pointerToBuffer;
			if (varargsCount > 0)
			{
				// Allocate stack space for buffer
				u32 bufferValueIdx = IRAddTempValue(context, "_varargsBuffer%llu"_s,
						GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, varargsCount), VALUEFLAGS_FORCE_MEMORY);
				IRValue bufferIRValue = IRValueValue(context, bufferValueIdx);

				// Fill the buffer
				int nonVarargs = (int)procParamCount;
				for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
				{
					ASTExpression *arg = &astProcCall->arguments[argIdx + nonVarargs];

					IRValue bufferIndexValue = IRValueImmediate(argIdx);
					IRValue bufferSlotValue = IRDoArrayAccess(context, bufferIRValue, bufferIndexValue,
							TYPETABLEIDX_ANY_STRUCT);

					IRValue rightValue = IRGenFromExpression(context, arg);
					IRDoAssignment(context, bufferSlotValue, rightValue);
				}

				pointerToBuffer = IRPointerToValue(context, bufferIRValue);
			}
			else
				pointerToBuffer = IRValueImmediate(0, anyPointerTypeIdx);

			// By now we should have the buffer with all the varargs as Any structs.
			// Now we put it into a dynamic array struct.

			// Allocate stack space for array
			u32 arrayValueIdx = IRAddTempValue(context, "_varargsArray%llu"_s,
					arrayOfAnyTypeIdx, VALUEFLAGS_FORCE_MEMORY);
			IRValue arrayIRValue = IRValueValue(context, arrayValueIdx);

			TypeInfo dynamicArrayTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];
			// Size
			{
				StructMember sizeStructMember = dynamicArrayTypeInfo.structInfo.members[0];
				IRValue sizeMember = IRDoMemberAccess(context, arrayIRValue, sizeStructMember);
				IRValue sizeValue = IRValueImmediate(varargsCount);
				IRDoAssignment(context, sizeMember, sizeValue);
			}

			// Data
			{
				StructMember dataStructMember = dynamicArrayTypeInfo.structInfo.members[1];
				IRValue dataMember = IRDoMemberAccess(context, arrayIRValue, dataStructMember);
				IRValue dataValue = pointerToBuffer;
				IRDoAssignment(context, dataMember, dataValue);
			}

			// Pass array as parameter!
			*ArrayAdd(&procCallInst.procedureCall.parameters) = arrayIRValue;
		}

skipGeneratingVarargsArray:
		*AddInstruction(context) = procCallInst;
		break;
	}
	case ASTNODETYPE_INTRINSIC:
	{
		ASTIntrinsic astIntrinsic = expression->intrinsic;
		IRInstruction inst = { IRINSTRUCTIONTYPE_INTRINSIC };
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
			result = IRPointerToValue(context, result);
		}
		else if (expression->unaryOperation.op == TOKEN_OP_DEREFERENCE)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);
			result = IRDereferenceValue(context, result);
		}
		else
		{
			IRInstruction inst = {};
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
			IRDoAssignment(context, dstValue, srcValue);
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
			TypeCategory typeCat = context->typeTable[typeTableIdx].typeCategory;
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
			static u64 stringStaticVarUniqueID = 0;

			IRStaticVariable newStaticVar = {};
			newStaticVar.valueIdx = NewValue(context,
					TPrintF("staticString%d", stringStaticVarUniqueID++),
					TYPETABLEIDX_STRING_STRUCT, VALUEFLAGS_ON_STATIC_STORAGE);
			newStaticVar.initialValue = IRValueImmediateString(context, expression->literal.string);
			newStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_STRING_STRUCT;
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;

			result = IRValueValue(context, newStaticVar.valueIdx);
		} break;
		case LITERALTYPE_GROUP:
		{
			IRValue groupIRValue = IRValueNewValue(context, "_groupLiteral"_s,
					expression->typeTableIdx, 0);
			IRPushValueIntoStack(context, groupIRValue.value.valueIdx);
			IRFillValueWithGroupLiteral(context, groupIRValue, expression->literal);
			result = groupIRValue;
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

		IRInsertLabelInstruction(context, skipLabel);

		IRLabel *afterElseLabel = NewLabel(context, "afterElse"_s);

		if (expression->ifNode.elseBody)
		{
			jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
			jumpAfterElse->jump.label = afterElseLabel;

			IRGenFromExpression(context, expression->ifNode.elseBody);

			IRInsertLabelInstruction(context, afterElseLabel);
		}

	} break;
	case ASTNODETYPE_WHILE:
	{
		IRLabel *loopLabel     = NewLabel(context, "loop"_s);
		IRLabel *breakLabel    = NewLabel(context, "break"_s);
		IRInsertLabelInstruction(context, loopLabel);

		IRLabel *oldBreakLabel    = context->currentBreakLabel;
		IRLabel *oldContinueLabel = context->currentContinueLabel;
		context->currentBreakLabel    = breakLabel;
		context->currentContinueLabel = loopLabel;

		IRConditionalJumpFromExpression(context, expression->whileNode.condition, breakLabel, false);

		IRGenFromExpression(context, expression->whileNode.body);

		IRInstruction *loopJump = AddInstruction(context);
		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->jump.label = loopLabel;

		IRInsertLabelInstruction(context, breakLabel);

		context->currentBreakLabel    = oldBreakLabel;
		context->currentContinueLabel = oldContinueLabel;
	} break;
	case ASTNODETYPE_FOR:
	{
		PushIRScope(context);

		u32 indexValueIdx = expression->forNode.indexValueIdx;
		IRPushValueIntoStack(context, indexValueIdx);
		IRValue indexValue = IRValueValue(context, indexValueIdx);

		bool isThereItVariable = false;
		u32 elementTypeIdx = TYPETABLEIDX_UNSET;

		IRValue from = {}, to = {}, arrayValue = {};
		if (expression->forNode.range->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			expression->forNode.range->binaryOperation.op == TOKEN_OP_RANGE)
		{
			ASTBinaryOperation binaryOp = expression->forNode.range->binaryOperation;

			from = IRGenFromExpression(context, binaryOp.leftHand);
			to =   IRGenFromExpression(context, binaryOp.rightHand);

			// Assign 'i'
			IRAddComment(context, "Assign 'i'"_s);
			IRDoAssignment(context, indexValue, from);
		}
		else
		{
			arrayValue = IRGenFromExpression(context, expression->forNode.range);

			TypeInfo rangeTypeInfo = context->typeTable[arrayValue.typeTableIdx];
			if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			{
				arrayValue = IRDereferenceValue(context, arrayValue);
				rangeTypeInfo = context->typeTable[arrayValue.typeTableIdx];
			}

			ASSERT(arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT ||
				   rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY);

			isThereItVariable = true;
			u32 elementValueIdx = expression->forNode.elementValueIdx;
			// Allocate 'it' variable
			IRPushValueIntoStack(context, elementValueIdx);

			elementTypeIdx = TYPETABLEIDX_U8;
			if (arrayValue.typeTableIdx != TYPETABLEIDX_STRING_STRUCT)
				elementTypeIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			u32 pointerToElementTypeTableIdx = GetTypeInfoPointerOf(context, elementTypeIdx);

			from = IRValueImmediate(0);
			if (rangeTypeInfo.arrayInfo.count == 0 || arrayValue.typeTableIdx == TYPETABLEIDX_STRING_STRUCT)
			{
				// Compare with size member
				TypeInfo dynamicArrayTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];
				to = IRDoMemberAccess(context, arrayValue, dynamicArrayTypeInfo.structInfo.members[0]);
			}
			else
				to = IRValueImmediate(rangeTypeInfo.arrayInfo.count);

			// Assign 'i'
			IRAddComment(context, "Assign 'i'"_s);
			IRDoAssignment(context, indexValue, from);

			// Assign 'it'
			IRAddComment(context, "Assign 'it'"_s);
			IRValue elementVarValue = IRValueValue(elementValueIdx, pointerToElementTypeTableIdx);
			IRValue elementValue = IRDoArrayAccess(context, arrayValue, indexValue, elementTypeIdx);
			elementValue = IRPointerToValue(context, elementValue);
			IRDoAssignment(context, elementVarValue, elementValue);
		}

		IRLabel *loopLabel     = NewLabel(context, "loop"_s);
		IRLabel *breakLabel    = NewLabel(context, "break"_s);
		IRLabel *continueLabel = NewLabel(context, "continue"_s);
		IRLabel *continueSkipIncrementLabel = NewLabel(context, "continueSkipIncrement"_s);

		IRInsertLabelInstruction(context, loopLabel);

		IRLabel *oldBreakLabel    = context->currentBreakLabel;
		IRLabel *oldContinueLabel = context->currentContinueLabel;
		context->currentBreakLabel    = breakLabel;
		context->currentContinueLabel = continueLabel;
		context->currentContinueSkipIncrementLabel = continueSkipIncrementLabel;

		IRInstruction *breakJump = AddInstruction(context);
		breakJump->type = IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS;
		breakJump->conditionalJump2.label = breakLabel;
		breakJump->conditionalJump2.left  = indexValue;
		breakJump->conditionalJump2.right = to;

		IRValue oldArrayValue = context->irCurrentForLoopInfo.arrayValue;
		IRValue oldIndexValue = context->irCurrentForLoopInfo.indexValue;
		context->irCurrentForLoopInfo.arrayValue = arrayValue;
		context->irCurrentForLoopInfo.indexValue = indexValue;

		IRGenFromExpression(context, expression->forNode.body);

		IRInsertLabelInstruction(context, continueLabel);

		// Increment 'i'
		IRInstruction incrementInst = {};
		incrementInst.type = IRINSTRUCTIONTYPE_ADD;
		incrementInst.binaryOperation.left = indexValue;
		incrementInst.binaryOperation.right = IRValueImmediate(1);
		incrementInst.binaryOperation.out = indexValue;
		*AddInstruction(context) = incrementInst;

		if (isThereItVariable)
		{
			// Update 'it'
			u32 elementValueIdx = expression->forNode.elementValueIdx;
			IRValue elementVarValue = IRValueValue(context, elementValueIdx);
			IRValue elementValue = IRDoArrayAccess(context, arrayValue, indexValue, elementTypeIdx);
			elementValue = IRPointerToValue(context, elementValue);
			IRDoAssignment(context, elementVarValue, elementValue);
		}

		IRInsertLabelInstruction(context, continueSkipIncrementLabel);

		IRInstruction *loopJump = AddInstruction(context);
		IRInsertLabelInstruction(context, breakLabel);

		context->currentBreakLabel    = oldBreakLabel;
		context->currentContinueLabel = oldContinueLabel;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->jump.label = loopLabel;

		context->irCurrentForLoopInfo.arrayValue = oldArrayValue;
		context->irCurrentForLoopInfo.indexValue = oldIndexValue;

		PopIRScope(context);
	} break;
	case ASTNODETYPE_CONTINUE:
	{
		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.jump.label = context->currentContinueLabel;
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_REMOVE:
	{
		IRValue arrayValue = context->irCurrentForLoopInfo.arrayValue;
		IRValue indexValue = context->irCurrentForLoopInfo.indexValue;
		IRValue sizeValue;

		TypeInfo arrayType = context->typeTable[arrayValue.typeTableIdx];
		if (arrayType.typeCategory == TYPECATEGORY_POINTER)
			arrayType = context->typeTable[arrayType.pointerInfo.pointedTypeTableIdx];

		u32 elementTypeIdx = arrayType.arrayInfo.elementTypeTableIdx;

		{
			TypeInfo arrayStructTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];
			StructMember sizeMember = arrayStructTypeInfo.structInfo.members[0];
			StructMember dataMember = arrayStructTypeInfo.structInfo.members[1];

			sizeValue = IRDoMemberAccess(context, arrayValue, sizeMember);
		}

		IRInstruction decrInst = { IRINSTRUCTIONTYPE_SUBTRACT };
		decrInst.binaryOperation.left = sizeValue;
		decrInst.binaryOperation.right = IRValueImmediate(1);
		decrInst.binaryOperation.out = sizeValue;
		*AddInstruction(context) = decrInst;

		IRValue current = IRDoArrayAccess(context, arrayValue, indexValue, elementTypeIdx);
		IRValue last    = IRDoArrayAccess(context, arrayValue, sizeValue,  elementTypeIdx);
		IRDoAssignment(context, current, last);

		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.jump.label = context->currentContinueSkipIncrementLabel;
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_BREAK:
	{
		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.jump.label = context->currentBreakLabel;
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_RETURN:
	{
		IRProcedureScope *stackTop = &context->irProcedureStack[context->irProcedureStack.size - 1];
		Procedure *currentProc = GetProcedure(context, stackTop->procedureIdx);
		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = context->irStack.size - 1;
				stackIdx >= stackTop->irStackBase; --stackIdx)
		{
			if (context->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			if (stackTop->shouldReturnValueIdx == U32_MAX)
				stackTop->shouldReturnValueIdx = NewValue(context, TYPETABLEIDX_U8, 0);

			// Set should return to one
			IRValue shouldReturnRegister = IRValueValue(stackTop->shouldReturnValueIdx,
					TYPETABLEIDX_U8);
			IRValue one = IRValueImmediate(1);
			IRDoAssignment(context, shouldReturnRegister, one);
		}

		if (expression->returnNode.expression != nullptr)
		{
			IRValue returnValue = IRGenFromExpression(context, expression->returnNode.expression);
			u32 returnTypeTableIdx = expression->returnNode.expression->typeTableIdx;
			ASSERT(returnTypeTableIdx >= TYPETABLEIDX_Begin);

			if (IRShouldPassByCopy(context, returnTypeTableIdx))
			{
				u64 size = context->typeTable[returnTypeTableIdx].size;
				IRValue sizeValue = IRValueImmediate(size);

				IRInstruction memcpyInst = {};
				memcpyInst.type = IRINSTRUCTIONTYPE_COPY_MEMORY;
				memcpyInst.copyMemory.src = IRPointerToValue(context, returnValue);
				memcpyInst.copyMemory.dst = IRValueValue(currentProc->returnValueIdx,
						GetTypeInfoPointerOf(context, returnTypeTableIdx));
				memcpyInst.copyMemory.size = sizeValue;

				*AddInstruction(context) = memcpyInst;
			}
			else
			{
				IRValue dst = IRValueValue(currentProc->returnValueIdx, returnValue.typeTableIdx);
				IRDoAssignment(context, dst, returnValue);
			}
		}

		if (isThereCleanUpToDo)
		{
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.jump.label = context->irStack[context->irStack.size - 1].closeLabel;
			*AddInstruction(context) = jumpInst;
		}
		else
		{
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.jump.label = stackTop->returnLabel;
			*AddInstruction(context) = jumpInst;
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		IRScope *stackTop = &context->irStack[context->irStack.size - 1];
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
		s64 size = context->typeTable[typeTableIdx].size;

		result = IRValueImmediate(size, TYPETABLEIDX_S64);
	} break;
	case ASTNODETYPE_CAST:
	{
		IRValue src = IRGenFromExpression(context, expression->castNode.expression);
		result = IRDoCast(context, src, expression->typeTableIdx);
	} break;
	case ASTNODETYPE_USING:
	{
		// @Check: only for variable declarations?
		IRGenFromExpression(context, expression->usingNode.expression);
	} break;
	case ASTNODETYPE_ENUM_DECLARATION:
	{
	} break;
	case ASTNODETYPE_GARBAGE:
	{
		ASSERT(!"Shouldn't attempt to generate IR from GARBAGE node");
	} break;
	default:
		ASSERT(!"Unknown ast node found type while generating IR");
	}

	return result;
}

void IRGenMain(Context *context)
{
	DynamicArrayInit(&context->irStaticVariables, 64);
	DynamicArrayInit(&context->irExternalVariables, 32);
	DynamicArrayInit(&context->irStack, 64);
	DynamicArrayInit(&context->irProcedureStack, 8);
	BucketArrayInit(&context->irLabels);

	PushIRScope(context);

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		IRGenFromExpression(context, statement);
	}
}
