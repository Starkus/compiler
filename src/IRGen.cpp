enum IRSpecialRegisters : s64
{
	IRSPECIALREGISTER_BEGIN = S64_MAX - 1,
	IRSPECIALREGISTER_RETURN = IRSPECIALREGISTER_BEGIN,
	IRSPECIALREGISTER_SHOULD_RETURN
};

enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_REGISTER,
	IRVALUETYPE_PARAMETER,
	IRVALUETYPE_STACK_OFFSET,
	IRVALUETYPE_DATA_OFFSET,
	IRVALUETYPE_IMMEDIATE_INTEGER,
	IRVALUETYPE_IMMEDIATE_FLOAT,
	IRVALUETYPE_IMMEDIATE_STRING,
	IRVALUETYPE_SIZEOF,
	IRVALUETYPE_TYPEOF
};
struct IRValue
{
	IRValueType valueType;
	union
	{
		s64 registerIdx;
		s64 parameterIdx;
		s64 stackOffset;
		struct
		{
			Variable *variable;
			s64 offset;
		} dataOffset;
		s64 immediate;
		f64 immediateFloat;
		String immediateString;
		s64 sizeOfTypeTableIdx;
		s64 typeOfTypeTableIdx;
	};
	bool dereference;
	s64 typeTableIdx;
};

struct IRLabel
{
	String name;
	Procedure *procedure;
	s64 instructionIdx;
};

struct IRJump
{
	IRLabel *label;
};

struct IRConditionalJump
{
	IRLabel *label;
	IRValue condition;
};

struct IRProcedureCall
{
	Procedure *procedure;
	Array<IRValue> parameters;
	IRValue out;
};

struct IRAssignment
{
	IRValue src;
	IRValue dst;
};

struct IRMemberAccess
{
	IRValue in;
	IRValue out;

	StructMember *structMember;
};

struct IRArrayAccess
{
	IRValue array;
	IRValue index;
	IRValue out;
	s64 elementTypeTableIdx;
};

struct IRUnaryOperation
{
	IRValue in;
	IRValue out;
};

struct IRBinaryOperation
{
	IRValue left;
	IRValue right;
	IRValue out;
};

struct IRGetParameter
{
	IRValue out;
	s64 parameterIdx;
};

struct IRSetParameter
{
	IRValue in;
	s64 parameterIdx;
};

struct IRVariableDeclaration
{
	Variable *variable;
};

struct IRIntrinsicMemcpy
{
	IRValue src;
	IRValue dst;
	IRValue size;
};

enum IRInstructionType
{
	IRINSTRUCTIONTYPE_INVALID = -1,

	IRINSTRUCTIONTYPE_NOP,

	IRINSTRUCTIONTYPE_COMMENT,

	IRINSTRUCTIONTYPE_LABEL,
	IRINSTRUCTIONTYPE_JUMP,
	IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
	IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,
	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL,

	IRINSTRUCTIONTYPE_ASSIGNMENT,

	IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_NOT = IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_BITWISE_NOT,
	IRINSTRUCTIONTYPE_SUBTRACT_UNARY,
	IRINSTRUCTIONTYPE_UNARY_END,

	IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_ADD = IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_SUBTRACT,
	IRINSTRUCTIONTYPE_MULTIPLY,
	IRINSTRUCTIONTYPE_DIVIDE,
	IRINSTRUCTIONTYPE_MODULO,
	IRINSTRUCTIONTYPE_SHIFT_LEFT,
	IRINSTRUCTIONTYPE_SHIFT_RIGHT,
	IRINSTRUCTIONTYPE_OR,
	IRINSTRUCTIONTYPE_AND,
	IRINSTRUCTIONTYPE_BITWISE_OR,
	IRINSTRUCTIONTYPE_BITWISE_XOR,
	IRINSTRUCTIONTYPE_BITWISE_AND,
	IRINSTRUCTIONTYPE_EQUALS,
	IRINSTRUCTIONTYPE_GREATER_THAN,
	IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_LESS_THAN,
	IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_BINARY_END,

	IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY
};
struct IRInstruction
{
	IRInstructionType type;
	union
	{
		String comment;
		IRLabel *label;
		IRJump jump;
		IRConditionalJump conditionalJump;
		IRProcedureCall procedureCall;
		IRGetParameter getParameter;
		IRSetParameter setParameter;
		IRVariableDeclaration variableDeclaration;
		IRAssignment assignment;
		IRMemberAccess memberAccess;
		IRArrayAccess arrayAccess;
		IRUnaryOperation unaryOperation;
		IRBinaryOperation binaryOperation;

		IRIntrinsicMemcpy memcpy;
	};
};

struct IRScope
{
	IRLabel *closeLabel;
	DynamicArray<ASTExpression *, malloc, realloc> deferredStatements;
	s64 stackCursor;
};

struct IRProcedureScope
{
	Procedure *procedure;
	s64 irStackBase;
	IRLabel *returnLabel;
	u64 currentRegisterId;
};

struct IRProcedure
{
	String name;
	bool isExternal;
	bool isVarargs;
	Array<Variable *> parameters;
	BucketArray<IRInstruction, 256, malloc, realloc> instructions;
	s64 returnTypeTableIdx;
	u64 registerCount;
};

struct IRStaticVariable
{
	Variable *variable;
	IRValue initialValue;
};

s64 NewVirtualRegister(Context *context)
{
	IRProcedureScope *currentProc = &context->irProcedureStack[context->irProcedureStack.size - 1];
	return currentProc->currentRegisterId++;
}

IRLabel *NewLabel(Context *context, String prefix)
{
	static u64 currentLabelId = 0;

	IRLabel result = {};
	IRProcedureScope *currentProc = &context->irProcedureStack[context->irProcedureStack.size - 1];

	result.name = TPrintF("%S%d", prefix, currentLabelId++);
	result.procedure = currentProc->procedure;
	result.instructionIdx = -1;

	IRLabel *newLabel = BucketArrayAdd(&context->irLabels);
	*newLabel = result;
	return newLabel;
}

void PushIRScope(Context *context)
{
	IRScope newScope = {};
	DynamicArrayInit(&newScope.deferredStatements, 4);

	if (context->irStack.size)
	{
		IRScope *stackTop = &context->irStack[context->irStack.size - 1];
		newScope.stackCursor = stackTop->stackCursor;
	}

	*DynamicArrayAdd(&context->irStack) = newScope;
}

void PopIRScope(Context *context)
{
	ASSERT(context->irStack.size);
	--context->irStack.size;
}

IRProcedureScope *PushIRProcedure(Context *context, Procedure *procedure)
{
	IRProcedureScope procScope;
	procScope.procedure = procedure;
	procScope.irStackBase = context->irStack.size;
	procScope.currentRegisterId = 0;

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
	IRProcedureScope *currentProc = &context->irProcedureStack[context->irProcedureStack.size - 1];
	return BucketArrayAdd(&currentProc->procedure->instructions);
}

IRValue IRValueImmediate(s64 immediate, s64 typeTableIdx = TYPETABLEIDX_S64)
{
	IRValue result;
	result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
	result.immediate = immediate;
	result.typeTableIdx = typeTableIdx;
	result.dereference = false;
	return result;
}

IRValue IRValueRegister(s64 registerIdx, s64 typeTableIdx = TYPETABLEIDX_S64)
{
	IRValue result;
	result.valueType = IRVALUETYPE_REGISTER;
	result.registerIdx = registerIdx;
	result.typeTableIdx = typeTableIdx;
	result.dereference = false;
	return result;
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression);

IRValue IRDereferenceValue(Context *context, IRValue in)
{
	IRValue result = in;

	TypeInfo *pointerTypeInfo = &context->typeTable[in.typeTableIdx];
	ASSERT(pointerTypeInfo->typeCategory == TYPECATEGORY_POINTER);

	if (!result.dereference)
	{
		result.dereference = true;
		result.typeTableIdx = pointerTypeInfo->pointerInfo.pointedTypeTableIdx;
	}
	else
	{
		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.dst = IRValueRegister(NewVirtualRegister(context), in.typeTableIdx);
		inst.assignment.src = result;
		*AddInstruction(context) = inst;

		result = inst.assignment.dst;
		result.dereference = true;
		result.typeTableIdx = pointerTypeInfo->pointerInfo.pointedTypeTableIdx;
	}

	return result;
}

IRValue IRPointerToValue(Context *context, IRValue in)
{
	ASSERT(in.dereference);
	IRValue result = in;
	result.dereference = false;
	result.typeTableIdx = GetTypeInfoPointerOf(context, in.typeTableIdx);
	return result;
}

IRValue IRDoMemberAccess(Context *context, IRValue value, StructMember *structMember)
{
	if (value.dereference)
	{
		TypeInfo *structTypeInfo = &context->typeTable[value.typeTableIdx];
		if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
		{
			value = IRDereferenceValue(context, value);
			value.dereference = false;
			value.typeTableIdx = GetTypeInfoPointerOf(context, value.typeTableIdx);
		}
		else
			value = IRPointerToValue(context, value);
	}

	IRValue result;
	switch (value.valueType)
	{
	case IRVALUETYPE_STACK_OFFSET:
	{
		result = value;
		result.stackOffset -= structMember->offset;
	} break;
	case IRVALUETYPE_DATA_OFFSET:
	{
		result = value;
		result.dataOffset.offset += structMember->offset;
	} break;
	default:
	{
		s64 pointerTypeIdx = GetTypeInfoPointerOf(context, structMember->typeTableIdx);

		if (structMember->offset)
		{
			IRValue outValue = IRValueRegister(NewVirtualRegister(context), pointerTypeIdx);
			IRValue memberOffsetValue = IRValueImmediate(structMember->offset, TYPETABLEIDX_S64);

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ADD;
			inst.binaryOperation.left = value;
			inst.binaryOperation.right = memberOffsetValue;
			inst.binaryOperation.out = outValue;
			*AddInstruction(context) = inst;

			result = outValue;
		}
		else
		{
			result = value;
		}
	}
	}
	result.typeTableIdx = structMember->typeTableIdx;
	result.dereference = true;
	return result;
}

IRValue IRInstructionFromMultiply(Context *context, IRValue leftValue, IRValue rightValue)
{
	if (leftValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		if (leftValue.immediate == 1)
			return rightValue;
		else if (rightValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			leftValue.immediate *= rightValue.immediate;
			return leftValue;
		}
		else if (context->typeTable[rightValue.typeTableIdx].typeCategory == TYPECATEGORY_INTEGER &&
				IsPowerOf2(leftValue.immediate))
		{
			IRValue outValue = IRValueRegister(NewVirtualRegister(context), leftValue.typeTableIdx);

			unsigned long shiftAmount;
			_BitScanForward64(&shiftAmount, leftValue.immediate);

			ASSERT(1ll << shiftAmount == leftValue.immediate);

			IRInstruction inst;
			inst.type = IRINSTRUCTIONTYPE_SHIFT_LEFT;
			inst.binaryOperation.left = rightValue;
			inst.binaryOperation.right = IRValueImmediate(shiftAmount);
			inst.binaryOperation.out = outValue;
			*AddInstruction(context) = inst;

			return outValue;
		}
	}

	if (rightValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		if (rightValue.immediate == 1)
			return leftValue;
		if (context->typeTable[leftValue.typeTableIdx].typeCategory == TYPECATEGORY_INTEGER &&
			IsPowerOf2(rightValue.immediate))
		{
			IRValue outValue = IRValueRegister(NewVirtualRegister(context), rightValue.typeTableIdx);

			unsigned long shiftAmount;
			_BitScanForward64(&shiftAmount, rightValue.immediate);

			ASSERT(1ll << shiftAmount == rightValue.immediate);

			IRInstruction inst;
			inst.type = IRINSTRUCTIONTYPE_SHIFT_LEFT;
			inst.binaryOperation.left = leftValue;
			inst.binaryOperation.right = IRValueImmediate(shiftAmount);
			inst.binaryOperation.out = outValue;
			*AddInstruction(context) = inst;

			return outValue;
		}
	}

	IRValue outValue = IRValueRegister(NewVirtualRegister(context), leftValue.typeTableIdx);

	IRInstruction inst;
	inst.type = IRINSTRUCTIONTYPE_MULTIPLY;
	inst.binaryOperation.left = leftValue;
	inst.binaryOperation.right = rightValue;
	inst.binaryOperation.out = outValue;
	*AddInstruction(context) = inst;

	return outValue;
}

IRValue IRDoArrayAccess(Context *context, IRValue arrayValue, IRValue indexValue, s64 elementTypeIdx)
{
	s64 arrayTypeIdx = arrayValue.typeTableIdx;
	TypeInfo *arrayTypeInfo = &context->typeTable[arrayTypeIdx];
	if (arrayTypeInfo->typeCategory == TYPECATEGORY_POINTER)
	{
		arrayTypeIdx = arrayTypeInfo->pointerInfo.pointedTypeTableIdx;
		arrayTypeInfo = &context->typeTable[arrayTypeIdx];
	}

	s64 stringTableIdx = FindTypeInStackByName(context, {}, "String"_s);
	if (arrayTypeIdx == stringTableIdx || arrayTypeInfo->arrayInfo.count == 0)
	{
		// Access the 'data' pointer
		s64 arrayTypeTableIdx = FindTypeInStackByName(context, {}, "Array"_s);
		TypeInfo *arrayStructTypeInfo = &context->typeTable[arrayTypeTableIdx];
		StructMember *structMember = &arrayStructTypeInfo->structInfo.members[1];

		IRValue arrayMember = IRDoMemberAccess(context, arrayValue, structMember);

		arrayValue = arrayMember;
		arrayValue.dereference = true;
		arrayValue = IRDereferenceValue(context, arrayValue);
	}

	if (arrayValue.dereference)
	{
		if (arrayTypeInfo->typeCategory == TYPECATEGORY_POINTER)
		{
			arrayValue = IRDereferenceValue(context, arrayValue);
			arrayValue.dereference = false;
			arrayValue.typeTableIdx = GetTypeInfoPointerOf(context, arrayTypeIdx);
		}
		else
			arrayValue = IRPointerToValue(context, arrayValue);
	}

	s64 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementTypeIdx);

	s64 elementSize = context->typeTable[elementTypeIdx].size;

	IRValue offsetValue = IRInstructionFromMultiply(context, indexValue,
			IRValueImmediate(elementSize, TYPETABLEIDX_S64));

	IRValue outValue = IRValueRegister(NewVirtualRegister(context), pointerToElementTypeIdx);
	IRInstruction addOffsetInst = {};
	addOffsetInst.type = IRINSTRUCTIONTYPE_ADD;
	addOffsetInst.binaryOperation.left = arrayValue;
	addOffsetInst.binaryOperation.right = offsetValue;
	addOffsetInst.binaryOperation.out = outValue;
	*AddInstruction(context) = addOffsetInst;

	IRValue result = outValue;
	result.typeTableIdx = elementTypeIdx;
	result.dereference = true;
	return result;
}

void IRAddComment(Context *context, String comment)
{
	IRInstruction result;
	result.type = IRINSTRUCTIONTYPE_COMMENT;
	result.comment = comment;
	*AddInstruction(context) = result;
}

void IRAllocateVariableOnStack(Context *context, Variable *variable)
{
	IRScope *stackTop = &context->irStack[context->irStack.size - 1];
	s64 varSize = context->typeTable[variable->typeTableIdx].size;
	stackTop->stackCursor += varSize;
	variable->isRegister = false;
	variable->stackOffset = stackTop->stackCursor;

	IRAddComment(context, TPrintF("Declare variable \"%S\" on stack + 0x%llX", variable->name,
				variable->stackOffset));

	IRProcedureScope *currentProc = &context->irProcedureStack[context->irProcedureStack.size - 1];
	if (stackTop->stackCursor > currentProc->procedure->stackSize)
		currentProc->procedure->stackSize = stackTop->stackCursor;
}

Variable *IRAddTempVariable(Context *context, String name, s64 typeTableIdx)
{
	Variable *variable = NewVariable(context, name);
	variable->typeTableIdx = typeTableIdx;

	IRAllocateVariableOnStack(context, variable);

	return variable;
}

void IRInstructionFromAssignment(Context *context, IRValue leftValue, IRValue rightValue)
{
	s64 rightType = rightValue.typeTableIdx;
	ASSERT(rightType >= 0);

	// Cast to Any
	s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);
	s64 anyPointerTableIdx = GetTypeInfoPointerOf(context, anyTableIdx);
	if ((leftValue.typeTableIdx == anyTableIdx || leftValue.typeTableIdx == anyPointerTableIdx) &&
		(rightType != anyTableIdx && rightType != anyPointerTableIdx))
	{
		TypeInfo *anyTypeInfo = &context->typeTable[anyTableIdx];

		IRValue ptrToLeftValue = leftValue.dereference ? IRPointerToValue(context, leftValue) :
				leftValue;

		// Access typeInfo member
		IRValue typeInfoMember = IRDoMemberAccess(context, ptrToLeftValue,
				&anyTypeInfo->structInfo.members[0]);

		// Write pointer to typeInfo to it
		IRInstruction typeAssignInst = {};
		typeAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		typeAssignInst.assignment.src.valueType = IRVALUETYPE_TYPEOF;
		typeAssignInst.assignment.src.typeOfTypeTableIdx = rightValue.typeTableIdx;
		typeAssignInst.assignment.src.typeTableIdx = GetTypeInfoPointerOf(context,
				FindTypeInStackByName(context, {}, "TypeInfo"_s));
		typeAssignInst.assignment.dst = typeInfoMember;
		typeAssignInst.assignment.dst.dereference = true;
		*AddInstruction(context) = typeAssignInst;

		// Access data member
		IRValue dataMember = IRDoMemberAccess(context, ptrToLeftValue,
				&anyTypeInfo->structInfo.members[1]);

		IRValue data = rightValue;

		// If data isn't a pointer to something, copy to a variable
		if (!data.dereference)
		{
			static u64 tempVarForAnyUniqueID = 0;
			String tempVarName = TPrintF("_tempVarForAny%llu", tempVarForAnyUniqueID++);
			Variable *tempVar = IRAddTempVariable(context, tempVarName, data.typeTableIdx);

			IRValue tempVarIRValue = {};
			tempVarIRValue.valueType = IRVALUETYPE_STACK_OFFSET;
			tempVarIRValue.stackOffset = tempVar->stackOffset;
			tempVarIRValue.dereference = true;
			tempVarIRValue.typeTableIdx = data.typeTableIdx;

			IRInstruction dataCopyInst = {};
			dataCopyInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			dataCopyInst.assignment.src = data;
			dataCopyInst.assignment.dst = tempVarIRValue;
			*AddInstruction(context) = dataCopyInst;

			data = tempVarIRValue;
		}

		// Write pointer to data to it
		IRInstruction dataAssignInst = {};
		dataAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		dataAssignInst.assignment.src = IRPointerToValue(context, data);
		dataAssignInst.assignment.dst = dataMember;
		dataAssignInst.assignment.dst.dereference = true;
		*AddInstruction(context) = dataAssignInst;

		return;
	}

	// Cast static array to dynamic array
	TypeInfo *leftTypeInfo  = &context->typeTable[leftValue.typeTableIdx];
	TypeInfo *rightTypeInfo = &context->typeTable[rightValue.typeTableIdx];
	if (leftTypeInfo->typeCategory  == TYPECATEGORY_ARRAY &&
		rightTypeInfo->typeCategory == TYPECATEGORY_ARRAY &&
		leftTypeInfo->arrayInfo.count  == 0 &&
		rightTypeInfo->arrayInfo.count != 0)
	{
		s64 dynamicArrayTableIdx = FindTypeInStackByName(context, {}, "Array"_s);
		TypeInfo *dynamicArrayTypeInfo = &context->typeTable[dynamicArrayTableIdx];

		// Size
		StructMember *sizeStructMember = &dynamicArrayTypeInfo->structInfo.members[0];
		IRValue sizeMember = IRDoMemberAccess(context, leftValue, sizeStructMember);
		IRValue sizeValue = IRValueImmediate(rightTypeInfo->arrayInfo.count, TYPETABLEIDX_U64);
		IRInstructionFromAssignment(context, sizeMember, sizeValue);

		// Data
		StructMember *dataStructMember = &dynamicArrayTypeInfo->structInfo.members[1];
		IRValue dataMember = IRDoMemberAccess(context, leftValue, dataStructMember);
		IRValue dataValue = IRPointerToValue(context, rightValue);
		IRInstructionFromAssignment(context, dataMember, dataValue);

		return;
	}

	// Copy structs/arrays
	if (rightTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
		rightTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
	{
		IRValue sizeValue = {};
		sizeValue.valueType = IRVALUETYPE_SIZEOF;
		sizeValue.sizeOfTypeTableIdx = rightType;

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY;
		inst.memcpy.src = rightValue;
		inst.memcpy.dst = leftValue;
		inst.memcpy.size = sizeValue;

		if (inst.memcpy.src.dereference)
			inst.memcpy.src = IRPointerToValue(context, inst.memcpy.src);
		if (inst.memcpy.dst.dereference)
			inst.memcpy.dst = IRPointerToValue(context, inst.memcpy.dst);

		*AddInstruction(context) = inst;
	}
	else
	{
		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.src = rightValue;
		inst.assignment.dst = leftValue;

		*AddInstruction(context) = inst;
	}
}

IRValue IRInstructionFromBinaryOperation(Context *context, ASTExpression *expression)
{
	IRValue result = {};

	ASTExpression *rightHand = expression->binaryOperation.rightHand;
	ASTExpression *leftHand  = expression->binaryOperation.leftHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
	{
		IRValue value = IRGenFromExpression(context, leftHand);

		ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
		ASSERT(rightHand->identifier.type == NAMETYPE_STRUCT_MEMBER);
		StructMember *structMember = rightHand->identifier.structMemberInfo.offsets[0];

		result = IRDoMemberAccess(context, value, structMember);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
	{
		IRValue arrayValue = IRGenFromExpression(context, leftHand);
		IRValue indexValue = IRGenFromExpression(context, rightHand);
		result = IRDoArrayAccess(context, arrayValue, indexValue, expression->typeTableIdx);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_MULTIPLY)
	{
		IRValue leftValue  = IRGenFromExpression(context, leftHand);
		IRValue rightValue = IRGenFromExpression(context, rightHand);
		result = IRInstructionFromMultiply(context, leftValue, rightValue);
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

		IRValue outValue = IRValueRegister(NewVirtualRegister(context), leftHand->typeTableIdx);
		IRInstructionFromAssignment(context, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		IRInstruction *jumpToEndInst = AddInstruction(context);
		jumpToEndInst->type = IRINSTRUCTIONTYPE_JUMP;
		jumpToEndInst->jump.label = skipAssignZeroLabel;

		IRInstruction *assignZeroLabelInst = AddInstruction(context);
		assignZeroLabel->instructionIdx =
			BucketArrayCount(&assignZeroLabel->procedure->instructions) - 1;
		assignZeroLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		assignZeroLabelInst->label = assignZeroLabel;

		IRInstructionFromAssignment(context, outValue, IRValueImmediate(0));

		IRInstruction *endLabelInst = AddInstruction(context);
		skipAssignZeroLabel->instructionIdx =
			BucketArrayCount(&skipAssignZeroLabel->procedure->instructions) - 1;
		endLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		endLabelInst->label = skipAssignZeroLabel;
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

		IRInstruction *skipRightLabelInst = AddInstruction(context);
		skipRightLabel->instructionIdx =
			BucketArrayCount(&skipRightLabel->procedure->instructions) - 1;
		skipRightLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		skipRightLabelInst->label = skipRightLabel;

		IRValue outValue = IRValueRegister(NewVirtualRegister(context), leftHand->typeTableIdx);
		IRInstructionFromAssignment(context, outValue, IRValueImmediate(1));

		IRLabel *skipAssignZeroLabel = NewLabel(context, "skipAssignZero"_s);

		IRInstruction *jumpToEndInst = AddInstruction(context);
		jumpToEndInst->type = IRINSTRUCTIONTYPE_JUMP;
		jumpToEndInst->jump.label = skipAssignZeroLabel;

		IRInstruction *assignZeroLabelInst = AddInstruction(context);
		assignZeroLabel->instructionIdx =
			BucketArrayCount(&assignZeroLabel->procedure->instructions) - 1;
		assignZeroLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		assignZeroLabelInst->label = assignZeroLabel;

		IRInstructionFromAssignment(context, outValue, IRValueImmediate(0));

		IRInstruction *endLabelInst = AddInstruction(context);
		skipAssignZeroLabel->instructionIdx =
			BucketArrayCount(&skipAssignZeroLabel->procedure->instructions) - 1;
		endLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		endLabelInst->label = skipAssignZeroLabel;
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

		IRInstructionFromAssignment(context, leftValue, IRValueImmediate(0));

		IRInstruction *skipAssignZeroLabelInst = AddInstruction(context);
		skipAssignZeroLabel->instructionIdx =
			BucketArrayCount(&skipAssignZeroLabel->procedure->instructions) - 1;
		skipAssignZeroLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		skipAssignZeroLabelInst->label = skipAssignZeroLabel;
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

		IRInstructionFromAssignment(context, leftValue, IRValueImmediate(1));

		IRInstruction *skipAssignOneLabelInst = AddInstruction(context);
		skipAssignOneLabel->instructionIdx =
			BucketArrayCount(&skipAssignOneLabel->procedure->instructions) - 1;
		skipAssignOneLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		skipAssignOneLabelInst->label = skipAssignOneLabel;
	}
	else
	{
		IRInstruction inst = {};
		inst.binaryOperation.left  = IRGenFromExpression(context, leftHand);
		inst.binaryOperation.right = IRGenFromExpression(context, rightHand);

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

		IRValue outValue = IRValueRegister(NewVirtualRegister(context), leftHand->typeTableIdx);
		inst.binaryOperation.out = outValue;
		*AddInstruction(context) = inst;

		if (expression->binaryOperation.op >= TOKEN_OP_ASSIGNMENT_Begin &&
			expression->binaryOperation.op <= TOKEN_OP_ASSIGNMENT_End)
		{
			IRInstructionFromAssignment(context, inst.binaryOperation.left, outValue);
			result = inst.binaryOperation.left;
		}
		else
			result = outValue;
	}

	return result;
}

IRValue IRValueFromVariable(Context *context, Variable *variable)
{
	IRValue result = {};
	result.typeTableIdx = variable->typeTableIdx;

	if (variable->parameterIndex >= 0)
	{
		result.valueType = IRVALUETYPE_PARAMETER;
		result.parameterIdx = variable->parameterIndex;
		result.dereference = false;

		TypeInfo *typeInfo = &context->typeTable[variable->typeTableIdx];
		if (typeInfo->typeCategory == TYPECATEGORY_STRUCT ||
			typeInfo->typeCategory == TYPECATEGORY_ARRAY)
			result.typeTableIdx = GetTypeInfoPointerOf(context, variable->typeTableIdx);
	}
	else if (!variable->isStatic)
	{
		if (variable->isRegister)
			result = IRValueRegister(variable->registerIdx, variable->typeTableIdx);
		else
		{
			result.valueType = IRVALUETYPE_STACK_OFFSET;
			result.stackOffset = variable->stackOffset;
			result.dereference = true;
		}
	}
	else
	{
		result.valueType = IRVALUETYPE_DATA_OFFSET;
		result.dataOffset.variable = variable;
		result.dataOffset.offset = 0;
		result.dereference = true; // Dereference variables by default, since they are really pointers
	}
	return result;
}

bool IRShouldReturnByCopy(Context *context, s64 returnTypeTableIdx)
{
	TypeInfo *returnTypeInfo = &context->typeTable[returnTypeTableIdx];
	// @Speed
	return  returnTypeInfo->typeCategory == TYPECATEGORY_ARRAY ||
		   (returnTypeInfo->typeCategory == TYPECATEGORY_STRUCT &&
			returnTypeInfo->size != 1 &&
			returnTypeInfo->size != 2 &&
			returnTypeInfo->size != 4 &&
			returnTypeInfo->size != 8);
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		IRGenFromExpression(context, expression->staticDefinition.expression);
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		Procedure *procedure = expression->procedureDeclaration.procedure;
		BucketArrayInit(&procedure->instructions);

		IRProcedureScope *currentProc = PushIRProcedure(context, procedure);
		IRLabel *returnLabel = NewLabel(context, "return"_s);
		currentProc->returnLabel = returnLabel;

		if (procedure->astBody)
		{
			IRGenFromExpression(context, procedure->astBody);
			procedure->registerCount = currentProc->currentRegisterId;

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
	} break;
	case ASTNODETYPE_BLOCK:
	{
		PushIRScope(context);
		IRScope *currentScope = &context->irStack[context->irStack.size - 1];
		currentScope->closeLabel = NewLabel(context, "closeScope"_s);

		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			IRGenFromExpression(context, &expression->block.statements[i]);
		}

		IRProcedureScope *currentProc = &context->irProcedureStack[context->irProcedureStack.size - 1];
		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = context->irStack.size - 1;
				stackIdx >= currentProc->irStackBase; --stackIdx)
		{
			if (context->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			// Set should-return register to 0
			IRValue shouldReturnRegister = IRValueRegister(IRSPECIALREGISTER_SHOULD_RETURN,
					TYPETABLEIDX_U8);
			IRValue zero = IRValueImmediate(0, TYPETABLEIDX_U8);
			IRInstructionFromAssignment(context, shouldReturnRegister, zero);

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
			if ((s64)(context->irStack.size - 2) != currentProc->irStackBase)
			{
				IRLabel *skipLabel = NewLabel(context, "skipReturn"_s);

				IRInstruction jumpIfShouldntReturnInst;
				jumpIfShouldntReturnInst.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
				jumpIfShouldntReturnInst.conditionalJump.label = skipLabel;
				jumpIfShouldntReturnInst.conditionalJump.condition = shouldReturnRegister;
				*AddInstruction(context) = jumpIfShouldntReturnInst;

				// Jump to closing of next scope
				IRInstruction jumpInst;
				jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
				jumpInst.jump.label = context->irStack[context->irStack.size - 2].closeLabel;
				*AddInstruction(context) = jumpInst;

				IRInstruction *skipLabelInst = AddInstruction(context);
				skipLabel->instructionIdx = BucketArrayCount(&skipLabel->procedure->instructions) - 1;
				skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
				skipLabelInst->label = skipLabel;
			}
		}

		PopIRScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;
		Variable *variable = varDecl.variable;

		bool isGlobalScope = context->irProcedureStack.size == 0;
		if (isGlobalScope && !varDecl.variable->isStatic)
			LogError(context, expression->any.loc, "Global variables have to be static"_s);

		if (variable->isStatic)
		{
			IRStaticVariable newStaticVar = {};
			newStaticVar.variable = variable;
			newStaticVar.initialValue.valueType = IRVALUETYPE_INVALID;

			// Initial value
			if (varDecl.value)
			{
				if (varDecl.value->nodeType != ASTNODETYPE_LITERAL)
				{
					// @Todo: Somehow execute constant expressions and bake them?
					LogError(context, expression->any.loc, "Non literal initial values for global variables not yet supported"_s);
				}
				else if (varDecl.value->literal.type == LITERALTYPE_FLOATING)
				{
					newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
					newStaticVar.initialValue.immediateFloat = varDecl.value->literal.floating;
				}
				else if (varDecl.value->literal.type == LITERALTYPE_STRING)
				{
					newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRING;
					newStaticVar.initialValue.immediateString = varDecl.value->literal.string;
				}
				else
					newStaticVar.initialValue = IRGenFromExpression(context, varDecl.value);
			}

			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;
		}
		else
		{
#if 1
			TypeInfo *varTypeInfo = &context->typeTable[variable->typeTableIdx];
			if (variable->canBeRegister && varTypeInfo->size <= 8 && IsPowerOf2(varTypeInfo->size))
			{
				variable->isRegister = true;
				variable->registerIdx = NewVirtualRegister(context);
			}
			else
#endif
			{
				IRAllocateVariableOnStack(context, variable);
			}

			// Initial value
			if (varDecl.value)
			{
				IRValue leftValue = IRValueFromVariable(context, varDecl.variable);
				IRValue rightValue = IRGenFromExpression(context, varDecl.value);
				IRInstructionFromAssignment(context, leftValue, rightValue);
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
			case STATICDEFINITIONTYPE_CONSTANT_INTEGER:
			{
				result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
				result.immediate = expression->identifier.staticDefinition->constantInteger.value;
				result.typeTableIdx = TYPETABLEIDX_S64;
			} break;
			case STATICDEFINITIONTYPE_CONSTANT_FLOATING:
			{
				result.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
				result.immediateFloat = expression->identifier.staticDefinition->constantFloating.value;
				result.typeTableIdx = TYPETABLEIDX_F64;
			} break;
			}
		} break;
		case NAMETYPE_STRUCT_MEMBER:
		{
			IRValue left = IRValueFromVariable(context, expression->identifier.structMemberInfo.base);

			if (left.dereference)
			{
				TypeInfo *structTypeInfo = &context->typeTable[left.typeTableIdx];
				if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
				{
					s64 structTypeInfoIdx = structTypeInfo->pointerInfo.pointedTypeTableIdx;
					structTypeInfo = &context->typeTable[structTypeInfoIdx];

					// Only if it was pointer to a pointer, dereference
					if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
						left = IRDereferenceValue(context, left);
				}
				else
				{
					left = IRPointerToValue(context, left);
				}
			}

			for (int i = 0; i < expression->identifier.structMemberInfo.offsets.size; ++i)
			{
				StructMember *structMember = expression->identifier.structMemberInfo.offsets[i];

				IRValue memberValue = IRDoMemberAccess(context, left, structMember);

				// Set left for next one
				left = IRPointerToValue(context, memberValue);

				result = memberValue;
				result.typeTableIdx = structMember->typeTableIdx;
			}
			result.dereference = true;
		} break;
		case NAMETYPE_VARIABLE:
		{
			result = IRValueFromVariable(context, expression->identifier.variable);
		} break;
		default:
		{
			CRASH;
		}
		}
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall *astProcCall = &expression->procedureCall;
		Procedure *procedure = astProcCall->procedure;

		IRInstruction procCallInst = {};
		procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
		procCallInst.procedureCall.procedure = procedure;

		bool isReturnByCopy = IRShouldReturnByCopy(context, expression->typeTableIdx);

		// Support both varargs and default parameters here
		s32 totalParamCount = (s32)procedure->parameters.size;
		s32 callParamCount = (s32)astProcCall->arguments.size;
		s32 paramCount = Max(totalParamCount, callParamCount) + isReturnByCopy;
		ArrayInit(&procCallInst.procedureCall.parameters, paramCount, malloc);

		// Return value
		procCallInst.procedureCall.out.valueType = IRVALUETYPE_INVALID;
		if (expression->typeTableIdx != TYPETABLEIDX_VOID)
		{
			if (isReturnByCopy)
			{
				static u64 returnByCopyDeclarationUniqueID = 0;

				// Allocate stack for return value
				String tempVarName = TPrintF("_returnByCopy%llu", returnByCopyDeclarationUniqueID++);
				Variable *tempVar = IRAddTempVariable(context, tempVarName,
						expression->typeTableIdx);

				// Turn into a register
				IRValue reg = IRValueRegister(NewVirtualRegister(context),
						GetTypeInfoPointerOf(context, expression->typeTableIdx));

				IRValue tempVarIRValue = {};
				tempVarIRValue.valueType = IRVALUETYPE_STACK_OFFSET;
				tempVarIRValue.stackOffset = tempVar->stackOffset;

				IRInstructionFromAssignment(context, reg, tempVarIRValue);

				// Add register as parameter
				*ArrayAdd(&procCallInst.procedureCall.parameters) = reg;

				result = reg;
			}
			else
			{
				procCallInst.procedureCall.out = IRValueRegister(NewVirtualRegister(context),
						expression->typeTableIdx);
				result = procCallInst.procedureCall.out;
			}
		}

		// Set up parameters
		s64 normalArgumentsCount = Min(astProcCall->arguments.size,
				procedure->parameters.size - procedure->isVarargs);
		for (int argIdx = 0; argIdx < normalArgumentsCount; ++argIdx)
		{
			ASTExpression *arg = &astProcCall->arguments[argIdx];
			s64 argTypeTableIdx = arg->typeTableIdx;

			ProcedureParameter originalParam = procedure->parameters[argIdx];
			if (originalParam.variable->typeTableIdx != argTypeTableIdx)
			{
				argTypeTableIdx = originalParam.variable->typeTableIdx;
			}

			TypeInfo *argTypeInfo = &context->typeTable[argTypeTableIdx];
			if (argTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
					argTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
			{
				// Struct/array by copy:
				// Declare a variable in the stack, copy the struct/array to it, then pass the new
				// variable as parameter to the procedure.
				static u64 structByCopyDeclarationUniqueID = 0;

				IRAddComment(context, "Copy argument to the stack to pass as pointer"_s);
				// Allocate stack space for temp struct
				String tempVarName = TPrintF("_valueByCopy%llu", structByCopyDeclarationUniqueID++);
				Variable *tempVar = IRAddTempVariable(context, tempVarName,
						argTypeTableIdx);

				s64 pointerTypeIdx = GetTypeInfoPointerOf(context, argTypeTableIdx);

				IRValue tempVarIRValue = {};
				tempVarIRValue.valueType = IRVALUETYPE_STACK_OFFSET;
				tempVarIRValue.stackOffset = tempVar->stackOffset;
				tempVarIRValue.typeTableIdx = pointerTypeIdx;
				tempVarIRValue.dereference = false;

				// Turn into a register, mainly to pass it to the procedure
				IRValue paramReg = IRValueRegister(NewVirtualRegister(context), pointerTypeIdx);
				IRInstructionFromAssignment(context, paramReg, tempVarIRValue);

				paramReg.dereference = true;
				IRValue rightValue = IRGenFromExpression(context, arg);
				IRInstructionFromAssignment(context, paramReg, rightValue);

				// Add register as parameter
				*ArrayAdd(&procCallInst.procedureCall.parameters) = paramReg;
			}
			else
			{
				IRValue param = IRGenFromExpression(context, arg);

				// Make it a register if it's not already one
				if (param.valueType != IRVALUETYPE_REGISTER)
				{
					IRValue paramReg = IRValueRegister(NewVirtualRegister(context),
							argTypeTableIdx);
					IRInstructionFromAssignment(context, paramReg, param);
					param = paramReg;
				}

				*ArrayAdd(&procCallInst.procedureCall.parameters) = param;
			}
		}

		// Default parameters
		for (u64 argIdx = astProcCall->arguments.size; argIdx < astProcCall->procedure->parameters.size;
				++argIdx)
		{
			ASTExpression *arg = astProcCall->procedure->parameters[argIdx].defaultValue;
			IRValue param = IRGenFromExpression(context, arg);

			// Make it a register if it's not already one
			if (param.valueType != IRVALUETYPE_REGISTER)
			{
				IRValue paramReg = IRValueRegister(NewVirtualRegister(context), arg->typeTableIdx);
				IRInstructionFromAssignment(context, paramReg, param);
				param = paramReg;
			}

			*ArrayAdd(&procCallInst.procedureCall.parameters) = param;
		}

		// Varargs
		s64 varargsCount = astProcCall->arguments.size - (procedure->parameters.size -
				procedure->isVarargs);
		if (varargsCount > 0)
		{
			static u64 varargsUniqueID = 0;

			IRAddComment(context, "Build varargs array"_s);

			s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);

			// Allocate stack space for buffer
			String tempVarName = TPrintF("_varargsBuffer%llu", varargsUniqueID++);
			Variable *bufferVar = IRAddTempVariable(context, tempVarName,
					GetTypeInfoArrayOf(context, anyTableIdx, varargsCount));

			IRValue bufferIRValue = {};
			bufferIRValue.valueType = IRVALUETYPE_STACK_OFFSET;
			bufferIRValue.stackOffset = bufferVar->stackOffset;
			bufferIRValue.typeTableIdx = bufferVar->typeTableIdx;
			bufferIRValue.dereference = true;

			// Fill the buffer
			int nonVarargs = (int)procedure->parameters.size - 1;
			for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
			{
				ASTExpression *arg = &astProcCall->arguments[argIdx + nonVarargs];

				IRValue bufferIndexValue = {};
				bufferIndexValue.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
				bufferIndexValue.immediate = argIdx;

				IRValue bufferSlotValue = IRDoArrayAccess(context, bufferIRValue, bufferIndexValue,
						anyTableIdx);

				IRValue rightValue = IRGenFromExpression(context, arg);
				IRInstructionFromAssignment(context, bufferSlotValue, rightValue);
			}

			// By now we should have the buffer with all the varargs as Any structs.
			// Now we put it into a dynamic array struct.
			s64 dynamicArrayTableIdx = FindTypeInStackByName(context, {}, "Array"_s);

			// Allocate stack space for array
			tempVarName = TPrintF("_varargsArray%llu", varargsUniqueID++);
			Variable *arrayVar = IRAddTempVariable(context, tempVarName,
					dynamicArrayTableIdx);

			IRValue arrayIRValue = {};
			arrayIRValue.valueType = IRVALUETYPE_STACK_OFFSET;
			arrayIRValue.stackOffset = arrayVar->stackOffset;
			arrayIRValue.typeTableIdx = arrayVar->typeTableIdx;
			arrayIRValue.dereference = true;

			TypeInfo *dynamicArrayTypeInfo = &context->typeTable[dynamicArrayTableIdx];
			// Size
			{
				StructMember *sizeStructMember = &dynamicArrayTypeInfo->structInfo.members[0];
				IRValue sizeMember = IRDoMemberAccess(context, arrayIRValue, sizeStructMember);
				IRValue sizeValue = IRValueImmediate(varargsCount, TYPETABLEIDX_U64);
				IRInstructionFromAssignment(context, sizeMember, sizeValue);
			}

			// Data
			{
				StructMember *dataStructMember = &dynamicArrayTypeInfo->structInfo.members[1];
				IRValue dataMember = IRDoMemberAccess(context, arrayIRValue, dataStructMember);
				IRValue dataValue = IRPointerToValue(context, bufferIRValue);
				IRInstructionFromAssignment(context, dataMember, dataValue);
			}

			// Pass array as parameter!
			IRValue paramReg = {};
			paramReg.valueType = IRVALUETYPE_REGISTER;
			paramReg.registerIdx = NewVirtualRegister(context);
			paramReg.typeTableIdx = GetTypeInfoPointerOf(context, dynamicArrayTableIdx);
			IRInstructionFromAssignment(context, paramReg, IRPointerToValue(context, arrayIRValue));
			*ArrayAdd(&procCallInst.procedureCall.parameters) = paramReg;
		}

		*AddInstruction(context) = procCallInst;
	} break;
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

			inst.unaryOperation.out = IRValueRegister(NewVirtualRegister(context),
					expression->typeTableIdx);

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
			IRValue leftValue = IRGenFromExpression(context, leftHand);
			IRValue rightValue = IRGenFromExpression(context, rightHand);
			IRInstructionFromAssignment(context, leftValue, rightValue);
			result = leftValue;
		}
		else
		{
			result = IRInstructionFromBinaryOperation(context, expression);
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_INTEGER:
			result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
			result.immediate = expression->literal.integer;
			break;
		case LITERALTYPE_CHARACTER:
			result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
			result.immediate = expression->literal.character;
			break;
		case LITERALTYPE_FLOATING:
		{
			static u64 floatStaticVarUniqueID = 0;

			IRStaticVariable newStaticVar = {};
			newStaticVar.variable = NewVariable(context, TPrintF("staticFloat%d",
						floatStaticVarUniqueID++));
			newStaticVar.variable->typeTableIdx = TYPETABLEIDX_F64;
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
			newStaticVar.initialValue.immediateFloat = expression->literal.floating;
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;

			result.valueType = IRVALUETYPE_DATA_OFFSET;
			result.dataOffset.variable = newStaticVar.variable;
			result.dataOffset.offset = 0;
			result.dereference = true;
		} break;
		case LITERALTYPE_STRING:
		{
			static u64 stringStaticVarUniqueID = 0;

			IRStaticVariable newStaticVar = {};
			newStaticVar.variable = NewVariable(context, TPrintF("staticString%d",
						stringStaticVarUniqueID++));
			newStaticVar.variable->typeTableIdx = FindTypeInStackByName(context, {}, "String"_s);
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRING;
			newStaticVar.initialValue.immediateString = expression->literal.string;
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;

			result.valueType = IRVALUETYPE_DATA_OFFSET;
			result.dataOffset.variable = newStaticVar.variable;
			result.dataOffset.offset = 0;
		} break;
		}
		result.typeTableIdx = expression->typeTableIdx;
	} break;
	case ASTNODETYPE_IF:
	{
		IRValue conditionResult = IRGenFromExpression(context, expression->ifNode.condition);

		IRLabel *skipLabel = NewLabel(context, "skipIf"_s);

		IRInstruction *jump = AddInstruction(context);
		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = skipLabel;
		jump->conditionalJump.condition = conditionResult;

		// Body!
		IRGenFromExpression(context, expression->ifNode.body);

		IRInstruction *jumpAfterElse = nullptr;
		if (expression->ifNode.elseBody)
			// If we have an else, add a jump instruction here.
			jumpAfterElse = AddInstruction(context);

		IRInstruction *skipLabelInst = AddInstruction(context);
		skipLabel->instructionIdx = BucketArrayCount(&skipLabel->procedure->instructions) - 1;
		skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		skipLabelInst->label = skipLabel;

		IRLabel *afterElseLabel = NewLabel(context, "afterElse"_s);

		if (expression->ifNode.elseBody)
		{
			jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
			jumpAfterElse->jump.label = afterElseLabel;

			IRGenFromExpression(context, expression->ifNode.elseBody);

			IRInstruction *afterElseLabelInst = AddInstruction(context);
			afterElseLabel->instructionIdx =
				BucketArrayCount(&afterElseLabel->procedure->instructions) - 1;
			afterElseLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
			afterElseLabelInst->label = afterElseLabel;
		}

	} break;
	case ASTNODETYPE_WHILE:
	{
		IRInstruction *loopLabelInst = AddInstruction(context);
		IRLabel *loopLabel = NewLabel(context, "loop"_s);
		loopLabel->instructionIdx = BucketArrayCount(&loopLabel->procedure->instructions) - 1;
		loopLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		loopLabelInst->label = loopLabel;

		IRValue condition = IRGenFromExpression(context, expression->whileNode.condition);

		IRLabel *breakLabel = NewLabel(context, "break"_s);

		IRLabel *oldBreakLabel = context->currentBreakLabel;
		context->currentBreakLabel = breakLabel;

		IRInstruction *jump = AddInstruction(context);
		IRGenFromExpression(context, expression->whileNode.body);
		IRInstruction *loopJump = AddInstruction(context);

		IRInstruction *breakLabelInst = AddInstruction(context);
		breakLabel->instructionIdx = BucketArrayCount(&breakLabel->procedure->instructions) - 1;
		breakLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		breakLabelInst->label = breakLabel;

		context->currentBreakLabel = oldBreakLabel;

		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = breakLabel;
		jump->conditionalJump.condition = condition;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->jump.label = loopLabel;
	} break;
	case ASTNODETYPE_FOR:
	{
		PushIRScope(context);

		Variable *indexVar = expression->forNode.indexVariable;
		IRAllocateVariableOnStack(context, indexVar);
		IRValue indexValue = IRValueFromVariable(context, indexVar);

		IRInstruction *loopLabelInst = nullptr;

		TypeInfo *rangeTypeInfo = &context->typeTable[expression->forNode.range->typeTableIdx];

		IRValue from = {}, to = {}, arrayValue = {};
		if (expression->forNode.range->nodeType == ASTNODETYPE_BINARY_OPERATION)
		{
			ASTBinaryOperation binaryOp = expression->forNode.range->binaryOperation;
			ASSERT(binaryOp.op == TOKEN_OP_RANGE);

			from = IRGenFromExpression(context, binaryOp.leftHand);
			to =   IRGenFromExpression(context, binaryOp.rightHand);

			// Assign 'i'
			IRInstructionFromAssignment(context, indexValue, from);
		}
		else if (rangeTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
		{
			Variable *elementVar = expression->forNode.elementVariable;
			// Allocate 'it' variable
			IRAllocateVariableOnStack(context, elementVar);

			arrayValue = IRGenFromExpression(context, expression->forNode.range);

			from = IRValueImmediate(0, indexVar->typeTableIdx);
			if (rangeTypeInfo->arrayInfo.count == 0)
			{
				// Compare with size member
				s64 arrayTableIdx = FindTypeInStackByName(context, {}, "Array"_s);
				TypeInfo *dynamicArrayTypeInfo = &context->typeTable[arrayTableIdx];
				to = IRDoMemberAccess(context, arrayValue, &dynamicArrayTypeInfo->structInfo.members[0]);
			}
			else
				to = IRValueImmediate(rangeTypeInfo->arrayInfo.count, indexVar->typeTableIdx);

			// Assign 'i'
			IRInstructionFromAssignment(context, indexValue, from);

			// Assign 'it'
			IRValue elementVarValue = IRValueFromVariable(context, elementVar);
			IRValue elementValue = IRDoArrayAccess(context, arrayValue, indexValue,
					rangeTypeInfo->arrayInfo.elementTypeTableIdx);
			elementValue = IRPointerToValue(context, elementValue);
			IRInstructionFromAssignment(context, elementVarValue, elementValue);
		}
		else
			CRASH;

		loopLabelInst = AddInstruction(context);
		IRLabel *loopLabel = NewLabel(context, "loop"_s);
		loopLabel->instructionIdx = BucketArrayCount(&loopLabel->procedure->instructions) - 1;
		loopLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		loopLabelInst->label = loopLabel;

		IRValue condition = IRValueRegister(NewVirtualRegister(context), TYPETABLEIDX_BOOL);
		IRInstruction compareInst = {};
		compareInst.type = IRINSTRUCTIONTYPE_LESS_THAN;
		compareInst.binaryOperation.left = indexValue;
		compareInst.binaryOperation.right = to;
		compareInst.binaryOperation.out = condition;
		*AddInstruction(context) = compareInst;

		IRLabel *breakLabel = NewLabel(context, "break"_s);

		IRLabel *oldBreakLabel = context->currentBreakLabel;
		context->currentBreakLabel = breakLabel;

		IRInstruction *jump = AddInstruction(context);
		IRGenFromExpression(context, expression->whileNode.body);

		// Increment 'i'
		IRInstruction incrementInst = {};
		incrementInst.type = IRINSTRUCTIONTYPE_ADD;
		incrementInst.binaryOperation.left = indexValue;
		incrementInst.binaryOperation.right = IRValueImmediate(1, indexValue.typeTableIdx);
		incrementInst.binaryOperation.out = indexValue;
		*AddInstruction(context) = incrementInst;

		if (rangeTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
		{
			// Update 'it'
			Variable *elementVar = expression->forNode.elementVariable;
			IRValue elementVarValue = IRValueFromVariable(context, elementVar);
			IRValue elementValue = IRDoArrayAccess(context, arrayValue, indexValue,
					rangeTypeInfo->arrayInfo.elementTypeTableIdx);
			elementValue = IRPointerToValue(context, elementValue);
			IRInstructionFromAssignment(context, elementVarValue, elementValue);
		}

		IRInstruction *loopJump = AddInstruction(context);
		IRInstruction *breakLabelInst = AddInstruction(context);
		breakLabel->instructionIdx = BucketArrayCount(&breakLabel->procedure->instructions) - 1;
		breakLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		breakLabelInst->label = breakLabel;


		context->currentBreakLabel = oldBreakLabel;

		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = breakLabel;
		jump->conditionalJump.condition = condition;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->jump.label = loopLabel;

		PopIRScope(context);
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
		IRValue returnValue = IRGenFromExpression(context, expression->returnNode.expression);

		IRProcedureScope *currentProc = &context->irProcedureStack[context->irProcedureStack.size - 1];
		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = context->irStack.size - 1;
				stackIdx >= currentProc->irStackBase; --stackIdx)
		{
			if (context->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			// Set should return to one
			IRValue one = IRValueImmediate(1, TYPETABLEIDX_U8);
			IRValue shouldReturnRegister = IRValueRegister(IRSPECIALREGISTER_SHOULD_RETURN, TYPETABLEIDX_U8);
			IRInstructionFromAssignment(context, shouldReturnRegister, one);
		}
		else
		{
		}

		s64 returnTypeTableIdx = expression->returnNode.expression->typeTableIdx;
		if (IRShouldReturnByCopy(context, returnTypeTableIdx))
		{
			s64 pointerToType = GetTypeInfoPointerOf(context, returnTypeTableIdx);

			IRValue src = returnValue;
			src.typeTableIdx = pointerToType;
			src.dereference = false;

			IRValue dst;
			dst.valueType = IRVALUETYPE_REGISTER;
			dst.registerIdx = IRSPECIALREGISTER_RETURN;
			dst.typeTableIdx = pointerToType;
			dst.dereference = false;

			IRValue sizeValue = {};
			sizeValue.valueType = IRVALUETYPE_SIZEOF;
			sizeValue.sizeOfTypeTableIdx = returnTypeTableIdx;

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY;
			inst.memcpy.src = src;
			inst.memcpy.dst = dst;
			inst.memcpy.size = sizeValue;
			*AddInstruction(context) = inst;

			returnValue = dst;
		}
		else
		{
			IRInstruction returnValueSaveInst;
			returnValueSaveInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			returnValueSaveInst.assignment.src = returnValue;
			returnValueSaveInst.assignment.dst = IRValueRegister(IRSPECIALREGISTER_RETURN,
					returnValue.typeTableIdx);
			*AddInstruction(context) = returnValueSaveInst;
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
			jumpInst.jump.label = currentProc->returnLabel;
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
		result.valueType = IRVALUETYPE_TYPEOF;
		result.typeOfTypeTableIdx = expression->typeOfNode.expression->typeTableIdx;
		result.typeTableIdx = expression->typeTableIdx;
	} break;
	case ASTNODETYPE_CAST:
	{
		result = IRGenFromExpression(context, expression->castNode.expression);
		result.typeTableIdx = expression->typeTableIdx;
	} break;
	case ASTNODETYPE_ENUM_DECLARATION:
	{
	} break;
	}

	return result;
}

void PrintIRInstructionOperator(IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		Print("+");
		break;
	case IRINSTRUCTIONTYPE_SUBTRACT:
		Print("-");
		break;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		Print("*");
		break;
	case IRINSTRUCTIONTYPE_DIVIDE:
		Print("/");
		break;
	case IRINSTRUCTIONTYPE_MODULO:
		Print("%");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		Print("<<");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		Print(">>");
		break;
	case IRINSTRUCTIONTYPE_OR:
		Print("||");
		break;
	case IRINSTRUCTIONTYPE_AND:
		Print("&&");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
		Print("|");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
		Print("^");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_AND:
		Print("&");
		break;
	case IRINSTRUCTIONTYPE_EQUALS:
		Print("==");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		Print(">");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		Print(">=");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		Print("<");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		Print("<=");
		break;
	case IRINSTRUCTIONTYPE_NOT:
		Print("!");
		break;
	default:
		Print("<?>");
	}
}

void IRGenMain(Context *context)
{
	DynamicArrayInit(&context->irStaticVariables, 64);
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
