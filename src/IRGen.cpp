enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_VALUE,
	IRVALUETYPE_MEMORY,
	IRVALUETYPE_PROCEDURE,
	IRVALUETYPE_IMMEDIATE_INTEGER,
	IRVALUETYPE_IMMEDIATE_FLOAT,
	IRVALUETYPE_IMMEDIATE_STRING,
	IRVALUETYPE_IMMEDIATE_GROUP
};
struct IRValue
{
	IRValueType valueType;
	union
	{
		u32 valueIdx;
		s64 immediate;
		f64 immediateFloat;
		u32 immediateStringIdx;
		Array<IRValue, FrameAllocator> immediateStructMembers;
		s32 procedureIdx;
		struct
		{
			u32 baseValueIdx;
			s64 offset;
		} memory;
	};
	s64 typeTableIdx;
};
static_assert(offsetof(IRValue, valueIdx) == offsetof(IRValue, memory.baseValueIdx),
	"IRValue::valueIdx and IRValue::memory.baseValueIdx should have the same offset");

struct IRLabel
{
	String name;
	s32 procedureIdx;
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

struct IRConditionalJump2
{
	IRLabel *label;
	IRValue left;
	IRValue right;
};

struct IRProcedureCall
{
	union
	{
		s32 procedureIdx;
		IRValue procIRValue;
	};
	Array<IRValue, FrameAllocator> parameters;
	IRValue out;

	// Filled during register allocation
	u64 liveRegisters;
};

struct IRIntrinsic
{
	IntrinsicType type;
	Array<IRValue, FrameAllocator> parameters;
};

struct IRPushValue
{
	u32 valueIdx;
};

struct IRAssignment
{
	IRValue src;
	IRValue dst;
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

struct IRCopyMemory
{
	IRValue src;
	IRValue dst;
	IRValue size;
};

struct IRZeroMemory
{
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

	IRINSTRUCTIONTYPE_COMPARE_JUMP_BEGIN,
	IRINSTRUCTIONTYPE_JUMP_IF_EQUALS = IRINSTRUCTIONTYPE_COMPARE_JUMP_BEGIN,
	IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS,
	IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN,
	IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN,
	IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_COMPARE_JUMP_END,

	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT,
	IRINSTRUCTIONTYPE_INTRINSIC,
	IRINSTRUCTIONTYPE_PUSH_VALUE,
	IRINSTRUCTIONTYPE_PUSH_SCOPE,
	IRINSTRUCTIONTYPE_POP_SCOPE,

	IRINSTRUCTIONTYPE_ASSIGNMENT,

	IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_NOT = IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_BITWISE_NOT,
	IRINSTRUCTIONTYPE_SUBTRACT_UNARY,
	IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
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

	IRINSTRUCTIONTYPE_COMPARE_BEGIN,
	IRINSTRUCTIONTYPE_EQUALS = IRINSTRUCTIONTYPE_COMPARE_BEGIN,
	IRINSTRUCTIONTYPE_NOT_EQUALS,
	IRINSTRUCTIONTYPE_GREATER_THAN,
	IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_LESS_THAN,
	IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_COMPARE_END,
	IRINSTRUCTIONTYPE_BINARY_END,

	IRINSTRUCTIONTYPE_COPY_MEMORY,
	IRINSTRUCTIONTYPE_ZERO_MEMORY,
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
		IRConditionalJump2 conditionalJump2;
		IRProcedureCall procedureCall;
		IRIntrinsic intrinsic;
		IRPushValue pushValue;
		IRAssignment assignment;
		IRUnaryOperation unaryOperation;
		IRBinaryOperation binaryOperation;

		IRCopyMemory copyMemory;
		IRZeroMemory zeroMemory;
	};
};

struct IRScope
{
	IRLabel *closeLabel;
	DynamicArray<ASTExpression *, PhaseAllocator> deferredStatements;
};

struct IRProcedureScope
{
	s32 procedureIdx;
	s64 irStackBase;
	IRLabel *returnLabel;
	u32 shouldReturnValueIdx;

	SourceLocation definitionLoc;
};

struct IRStaticVariable
{
	u32 valueIdx;
	IRValue initialValue;
};

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

bool IRShouldPassByCopy(Context *context, s64 typeTableIdx)
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

IRValue IRValueValue(u32 valueIdx, s64 typeTableIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueValue(Context *context, u32 valueIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = context->values[valueIdx].typeTableIdx;
	return result;
}

IRValue IRValueTCValue(Context *context, TCValue tcValue)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	if (tcValue.type == TCVALUETYPE_VALUE)
	{
		result.valueIdx = tcValue.valueIdx;
		result.typeTableIdx = context->values[tcValue.valueIdx].typeTableIdx;
	}
	else
	{
		u32 procIdx = context->irProcedureStack[context->irProcedureStack.size - 1].procedureIdx;
		u32 paramValueIdx = GetProcedure(context, procIdx)->parameterValues[tcValue.parameterIdx];
		result.valueIdx = paramValueIdx;
		result.typeTableIdx = context->values[paramValueIdx].typeTableIdx;
	}
	return result;
}

IRValue IRValueMemory(u32 baseValueIdx, s64 offset, s64 typeTableIdx)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_MEMORY;
	result.memory.baseValueIdx = baseValueIdx;
	result.memory.offset = offset;
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueImmediate(s64 immediate, s64 typeTableIdx = TYPETABLEIDX_S64)
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
		s64 idx = BucketArrayCount(&context->stringLiterals);
		ASSERT(idx < U32_MAX);
		result.immediateStringIdx = (u32)idx;
		*BucketArrayAdd(&context->stringLiterals) = string;
	}
	return result;
}

IRValue IRValueImmediateFloat(Context *context, f64 f, s64 typeTableIdx = TYPETABLEIDX_F64)
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

IRValue IRValueNewValue(Context *context, s64 typeTableIdx, u32 flags, u32 immitateValueIdx = 0)
{
	u32 newValue = NewValue(context, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = newValue;
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueNewValue(Context *context, String name, s64 typeTableIdx, u32 flags,
		u32 immitateValueIdx = 0)
{
	u32 newValue = NewValue(context, name, typeTableIdx, flags, immitateValueIdx);

	IRValue result = {};
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = newValue;
	result.typeTableIdx = typeTableIdx;
	return result;
}

IRValue IRValueTypeOf(Context *context, s64 typeTableIdx)
{
	static s64 typeInfoPointerTypeIdx = GetTypeInfoPointerOf(context,
			TYPETABLEIDX_TYPE_INFO_STRUCT);
	u32 typeValueIdx = context->typeTable[typeTableIdx].valueIdx;
	return IRValueMemory(typeValueIdx, 0, typeInfoPointerTypeIdx);
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression);

IRValue IRDereferenceValue(Context *context, IRValue in)
{
	TypeInfo pointerTypeInfo = context->typeTable[in.typeTableIdx];
	ASSERT(pointerTypeInfo.typeCategory == TYPECATEGORY_POINTER);
	s64 pointedTypeIdx = pointerTypeInfo.pointerInfo.pointedTypeTableIdx;

	// This assert is cool and all, but would mean unnecesarily assigning types to things as we
	// generate IR.
	//ASSERT(pointedTypeIdx != TYPETABLEIDX_VOID);

	// We don't know which will be memory and which registers. Do the assignment for everybody.
	if (in.valueType == IRVALUETYPE_VALUE)
	{
		IRValue result = IRValueMemory(in.valueIdx, 0, pointedTypeIdx);
		return result;
	}
	else if (in.valueType == IRVALUETYPE_MEMORY)
	{
		String name = TPrintF("_deref_%S", context->values[in.valueIdx].name);
		u32 newValueIdx = NewValue(context, name, in.typeTableIdx, 0);
		IRValue value = IRValueValue(newValueIdx, in.typeTableIdx);

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.dst = value;
		inst.assignment.src = in;
		*AddInstruction(context) = inst;

		IRValue result = IRValueMemory(newValueIdx, 0, pointedTypeIdx);
		return result;
	}
	ASSERT(!"Dereferenced value must be either REGISTER or MEMORY");
	return {};
}

IRValue IRPointerToValue(Context *context, IRValue in)
{
	ASSERT(in.valueType == IRVALUETYPE_VALUE || in.valueType == IRVALUETYPE_MEMORY);
	s64 pointerTypeIdx = GetTypeInfoPointerOf(context, in.typeTableIdx);

	IRValue result = IRValueNewValue(context, "_pointerof"_s, pointerTypeIdx, 0);

	IRInstruction addressInst = {};
	addressInst.type = IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS;
	addressInst.unaryOperation.in = in;
	addressInst.unaryOperation.out = result;
	*AddInstruction(context) = addressInst;

	return result;
}

IRValue IRDoMemberAccess(Context *context, IRValue structValue, StructMember structMember)
{
	ASSERT(structValue.valueType == IRVALUETYPE_VALUE || structValue.valueType == IRVALUETYPE_MEMORY);

	s64 offset = structMember.offset;
	if (structValue.valueType == IRVALUETYPE_MEMORY)
		offset += structValue.memory.offset;
	IRValue result = IRValueMemory(structValue.valueIdx, offset, structMember.typeTableIdx);
	return result;
}

void IRDoAssignment(Context *context, IRValue dstValue, IRValue srcValue);
IRValue IRDoArrayAccess(Context *context, IRValue arrayValue, IRValue indexValue, s64 elementTypeIdx)
{
	TypeInfo arrayTypeInfo = context->typeTable[arrayValue.typeTableIdx];
	s64 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementTypeIdx);
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

	IRValue pointerToElementValue;
	if (indexValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && indexValue.immediate == 0)
		pointerToElementValue = IRPointerToValue(context, arrayValue);
	else
	{
		IRValue offsetValue;
		if (indexValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
			offsetValue = IRValueImmediate(indexValue.immediate * elementSize, TYPETABLEIDX_S64);
		else
		{
			offsetValue = IRValueNewValue(context, "_array_offset"_s, TYPETABLEIDX_S64, 0);
			if (elementSize == 1)
				IRDoAssignment(context, offsetValue, indexValue);
			else
			{
				offsetValue = IRValueNewValue(context, "_array_offset"_s, TYPETABLEIDX_S64, 0);
				IRInstruction multiplyInst = { IRINSTRUCTIONTYPE_MULTIPLY };
				multiplyInst.binaryOperation.left  = indexValue;
				multiplyInst.binaryOperation.right = IRValueImmediate(elementSize);
				multiplyInst.binaryOperation.out   = offsetValue;
				*AddInstruction(context) = multiplyInst;
			}
		}

		pointerToElementValue = IRValueNewValue(context, "_array_element"_s,
				pointerToElementTypeIdx, 0);
		IRInstruction addOffsetInst = {};
		addOffsetInst.type = IRINSTRUCTIONTYPE_ADD;
		addOffsetInst.binaryOperation.left = IRPointerToValue(context, arrayValue);
		addOffsetInst.binaryOperation.right = offsetValue;
		addOffsetInst.binaryOperation.out = pointerToElementValue;
		*AddInstruction(context) = addOffsetInst;
	}

	IRValue result = IRValueMemory(pointerToElementValue.valueIdx, 0, elementTypeIdx);
	return result;
}

inline void IRPushValueIntoStack(Context *context, u32 valueIdx)
{
	IRInstruction inst;
	inst.type = IRINSTRUCTIONTYPE_PUSH_VALUE;
	inst.pushValue.valueIdx = valueIdx;
	*AddInstruction(context) = inst;
}

u32 IRAddTempValue(Context *context, String name, s64 typeTableIdx, u8 flags)
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
		IRValue dataMember = IRDoMemberAccess(context, dstValue,
				anyTypeInfo.structInfo.members[1]);

		IRValue dataValue = srcValue;
		TypeInfo dataTypeInfo = context->typeTable[srcValue.typeTableIdx];

		// If data isn't in memory, copy to a variable
		if (dataValue.valueType != IRVALUETYPE_MEMORY &&
			dataTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
			dataTypeInfo.typeCategory != TYPECATEGORY_UNION &&
			dataTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
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
		}

		// Write pointer to data to it
		IRInstruction dataAssignInst = {};
		dataAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		dataAssignInst.assignment.src = IRPointerToValue(context, dataValue);
		dataAssignInst.assignment.dst = dataMember;
		*AddInstruction(context) = dataAssignInst;

		return;
	}

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

IRValue IRInstructionFromBinaryOperation(Context *context, ASTExpression *expression, IRValue outValue)
{
	IRValue result = {};

	ASTExpression *leftHand  = expression->binaryOperation.leftHand;
	ASTExpression *rightHand = expression->binaryOperation.rightHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
	{
		IRValue value = IRGenFromExpression(context, leftHand);

		TypeInfo structTypeInfo = context->typeTable[value.typeTableIdx];
		if (structTypeInfo.typeCategory == TYPECATEGORY_POINTER)
		{
			// Dereference the pointer to the struct
			String name = TPrintF("_deref_%S", context->values[value.valueIdx].name);
			u32 newValueIdx = NewValue(context, name, value.typeTableIdx, 0);
			IRValue newValue = IRValueValue(newValueIdx, value.typeTableIdx);

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			inst.assignment.dst = newValue;
			inst.assignment.src = value;
			*AddInstruction(context) = inst;

			s64 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
			value = IRValueMemory(newValueIdx, 0, pointedTypeIdx);
		}

		ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
		ASSERT(rightHand->identifier.type == NAMETYPE_STRUCT_MEMBER);
		StructMember structMember = *rightHand->identifier.structMemberInfo.structMember;

		result = IRDoMemberAccess(context, value, structMember);
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
			Value *v = &context->values[outValue.valueIdx];
			v->flags |= VALUEFLAGS_TRY_IMMITATE;
			v->tryImmitateValueIdx = inst.binaryOperation.left.valueIdx;
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

void IRConditionalJumpFromExpression(Context *context, ASTExpression *conditionExp, IRLabel *label)
{
	// The following tries to avoid saving condition to a bool, then comparing the bool with
	// 0 in the conditional jump.
	if (conditionExp->nodeType == ASTNODETYPE_BINARY_OPERATION)
	{
		IRInstruction jump = {};
		switch (conditionExp->binaryOperation.op)
		{
		case TOKEN_OP_EQUALS:
			jump.type = IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS;
			break;
		case TOKEN_OP_NOT_EQUALS:
			jump.type = IRINSTRUCTIONTYPE_JUMP_IF_EQUALS;
			break;
		case TOKEN_OP_GREATER_THAN:
			jump.type = IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS;
			break;
		case TOKEN_OP_LESS_THAN:
			jump.type = IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS;
			break;
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
			jump.type = IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN;
			break;
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
			jump.type = IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN;
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
				if (leftResult.immediate != rightResult.immediate)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_NOT_EQUALS:
				if (leftResult.immediate == rightResult.immediate)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_GREATER_THAN:
				if (leftResult.immediate <= rightResult.immediate)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_LESS_THAN:
				if (leftResult.immediate >= rightResult.immediate)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_GREATER_THAN_OR_EQUAL:
				if (leftResult.immediate < rightResult.immediate)
					goto insertSimpleJump;
				else return;
			case TOKEN_OP_LESS_THAN_OR_EQUAL:
				if (leftResult.immediate > rightResult.immediate)
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
		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
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
	IRPushValueIntoStack(context, returnValue.valueIdx);
	bool isVarargs = procTypeInfo.isVarargs;

	IRProcedureScope *stackTop = &context->irProcedureStack[context->irProcedureStack.size - 1];
	Procedure *callingProcedure = GetProcedure(context, stackTop->procedureIdx);

	s32 tempProcIdx = (s32)BucketArrayCount(&context->procedures);
	Procedure *tempProc = BucketArrayAdd(&context->procedures);
	{
		Procedure p = *procedure;
		p.instructions = callingProcedure->instructions;
		p.name = TPrintF("_%S_inline", procedure->name);
		p.returnValueIdx = returnValue.valueIdx;
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

		if (argValue.valueType == IRVALUETYPE_VALUE || argValue.valueType == IRVALUETYPE_MEMORY)
			*DynamicArrayAdd(&tempProc->parameterValues) = argValue.valueIdx;
		else
		{
			IRValue param = IRValueNewValue(context,
					procTypeInfo.parameters[argIdx].typeTableIdx, 0);
			IRDoAssignment(context, param, argValue);
			*DynamicArrayAdd(&tempProc->parameterValues) = param.valueIdx;
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
		*DynamicArrayAdd(&tempProc->parameterValues) = param.valueIdx;
	}

	// Varargs
	if (isVarargs)
	{
		static s64 anyPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_ANY_STRUCT);
		static s64 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);
		static s64 pointerToVarargsTypeIdx = GetTypeInfoPointerOf(context, arrayOfAnyTypeIdx);

		s64 varargsCount = astProcCall.arguments.size - procParamCount;
		IRValue varargsParam = IRValueNewValue(context, "_inline_varargsarr"_s, pointerToVarargsTypeIdx, 0);
		*DynamicArrayAdd(&tempProc->parameterValues) = varargsParam.valueIdx;

		if (varargsCount == 1)
		{
			ASTExpression *varargsArrayExp = &astProcCall.arguments[procParamCount];
			if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx)
			{
				IRValue varargsArray = IRGenFromExpression(context, varargsArrayExp);
				IRDoAssignment(context, varargsParam, varargsArray);
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
		IRDoAssignment(context, varargsParam, IRPointerToValue(context, arrayIRValue));
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
	s64 groupTypeIdx = value.typeTableIdx;
	ASSERT(groupTypeIdx > 0);
	TypeInfo groupTypeInfo = context->typeTable[groupTypeIdx];

	if (groupTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		groupTypeInfo.typeCategory == TYPECATEGORY_UNION)
	{
		struct StructStackFrame
		{
			IRValue irValue;
			s64 structTypeIdx;
			int idx;
		};
		DynamicArray<StructStackFrame, PhaseAllocator> structStack;
		DynamicArrayInit(&structStack, 8);
		*DynamicArrayAdd(&structStack) = { value, groupTypeIdx, 0 };

		for (int memberIdx = 0; structStack.size > 0; )
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
				structStack[structStack.size++] = { innerStructValue, currentMember.typeTableIdx, 0 };
				continue;
			}

			IRValue memberValue = IRDoMemberAccess(context, currentFrame.irValue, currentMember);
			IRValue src;
			if (memberIdx < astLiteral.members.size)
			{
				ASTExpression *literalMemberExp = astLiteral.members[memberIdx];
				src = IRGenFromExpression(context, literalMemberExp);
			}
			else
				src = IRValueImmediate(0, currentMember.typeTableIdx); // @Check: floats
			IRDoAssignment(context, memberValue, src);

			++structStack[structStack.size - 1].idx;
			++memberIdx;
		}
	}
	else if (groupTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
	{
		s64 elementTypeIdx = groupTypeInfo.arrayInfo.elementTypeTableIdx;
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
		Procedure *procedure = GetProcedure(context, procedureIdx);
		BucketArrayInit(&procedure->instructions);

		IRProcedureScope *currentProc = PushIRProcedure(context, expression->any.loc,
				procedureIdx);
		IRLabel *returnLabel = NewLabel(context, "return"_s);
		currentProc->returnLabel = returnLabel;

		for (int i = 0; i < procedure->parameterValues.size; ++i)
		{
			s32 paramValueIdx = procedure->parameterValues[i];

			Value *paramValue = &context->values[paramValueIdx];
			if (IRShouldPassByCopy(context, paramValue->typeTableIdx))
			{
				paramValue->flags |= VALUEFLAGS_PARAMETER_BY_COPY;
				paramValue->typeTableIdx = GetTypeInfoPointerOf(context, paramValue->typeTableIdx);
			}

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

		if (procedure->astProcedureDeclaration->astBody)
		{
			IRGenFromExpression(context, procedure->astProcedureDeclaration->astBody);

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
				TypeCategory typeCat = context->typeTable[expression->typeTableIdx].typeCategory;
				if (typeCat == TYPECATEGORY_FLOATING)
				{
					f64 f = expression->identifier.staticDefinition->constant.valueAsFloat;
					result = IRValueImmediateFloat(context, f);
				}
				else
					result = IRValueImmediate(expression->identifier.staticDefinition->constant.valueAsInt);
			} break;
			case STATICDEFINITIONTYPE_PROCEDURE:
			{
				result = IRValueProcedure(context, expression->identifier.staticDefinition->procedureIdx);
			} break;
			default:
				ASSERT(!"Invalid static definition type found while generating IR");
			}
		} break;
		case NAMETYPE_STRUCT_MEMBER:
		{
			IRValue left = IRValueTCValue(context, expression->identifier.structMemberInfo.tcValueBase);
			const StructMember *structMember = expression->identifier.structMemberInfo.structMember;
			result = IRDoMemberAccess(context, left, *structMember);
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
				context->values[result.valueIdx].flags & VALUEFLAGS_PARAMETER_BY_COPY)
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
		s64 procTypeIdx;
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
		Procedure *currentProc = GetProcedure(context, stackTop.procedureIdx);
		if (currentProc->allocatedParameterCount < paramCount)
			currentProc->allocatedParameterCount = paramCount;

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

				result = IRValueMemory(tempValueIdx, 0, expression->typeTableIdx);
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
			s64 argTypeTableIdx = procTypeInfo.parameters[argIdx].typeTableIdx;

			if (IRShouldPassByCopy(context, argTypeTableIdx))
			{
				// Struct/array by copy:
				// Declare a variable in the stack, copy the struct/array to it, then pass the new
				// variable as parameter to the procedure.
				static u64 structByCopyDeclarationUniqueID = 0;

				IRAddComment(context, "Copy argument to the stack to pass as pointer"_s);
				// Allocate stack space for temp struct
				String tempVarName = TPrintF("_valueByCopy%llu", structByCopyDeclarationUniqueID++);
				u32 tempValueIdx = IRAddTempValue(context, tempVarName, argTypeTableIdx,
						VALUEFLAGS_FORCE_MEMORY);
				IRValue tempVarIRValue = IRValueValue(context, tempValueIdx);

				IRValue argValue = IRGenFromExpression(context, arg);

				// Copy
				IRDoAssignment(context, tempVarIRValue, argValue);

				IRValue pointerToVarValue = IRPointerToValue(context, tempVarIRValue);

				// Add register as parameter
				*ArrayAdd(&procCallInst.procedureCall.parameters) = pointerToVarValue;
			}
			else
			{
				IRValue param = IRGenFromExpression(context, arg);
				*ArrayAdd(&procCallInst.procedureCall.parameters) = param;
			}
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

			static s64 anyPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_ANY_STRUCT);
			static s64 arrayOfAnyTypeIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

			if (varargsCount == 1)
			{
				ASTExpression *varargsArrayExp = &astProcCall->arguments[procParamCount];
				if (varargsArrayExp->typeTableIdx == arrayOfAnyTypeIdx)
				{
					IRValue varargsArray = IRGenFromExpression(context, varargsArrayExp);
					*ArrayAdd(&procCallInst.procedureCall.parameters) =
						IRPointerToValue(context, varargsArray);
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
			*ArrayAdd(&procCallInst.procedureCall.parameters) = IRPointerToValue(context, arrayIRValue);
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
			IRValue dstValue = IRGenFromExpression(context, leftHand);
			IRAssignmentFromExpression(context, dstValue, rightHand);
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
			TypeCategory typeCat = context->typeTable[expression->typeTableIdx].typeCategory;
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
			IRPushValueIntoStack(context, groupIRValue.valueIdx);
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
		IRConditionalJumpFromExpression(context, expression->ifNode.condition, skipLabel);

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

		IRConditionalJumpFromExpression(context, expression->whileNode.condition, breakLabel);

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

		s64 rangeTypeIdx = expression->forNode.range->typeTableIdx;
		TypeInfo rangeTypeInfo = context->typeTable[rangeTypeIdx];

		IRValue from = {}, to = {}, arrayValue = {};
		if (expression->forNode.range->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			expression->forNode.range->binaryOperation.op == TOKEN_OP_RANGE)
		{
			ASTBinaryOperation binaryOp = expression->forNode.range->binaryOperation;

			from = IRGenFromExpression(context, binaryOp.leftHand);
			to =   IRGenFromExpression(context, binaryOp.rightHand);

			// Assign 'i'
			IRDoAssignment(context, indexValue, from);
		}
		else if (rangeTypeIdx == TYPETABLEIDX_STRING_STRUCT || rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
		{
			u32 elementValueIdx = expression->forNode.elementValueIdx;
			// Allocate 'it' variable
			IRPushValueIntoStack(context, elementValueIdx);

			arrayValue = IRGenFromExpression(context, expression->forNode.range);

			s64 elementTypeIdx = TYPETABLEIDX_U8;
			if (rangeTypeIdx != TYPETABLEIDX_STRING_STRUCT)
				elementTypeIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			s64 pointerToElementTypeTableIdx = GetTypeInfoPointerOf(context, elementTypeIdx);

			from = IRValueImmediate(0);
			if (rangeTypeInfo.arrayInfo.count == 0 || rangeTypeIdx == TYPETABLEIDX_STRING_STRUCT)
			{
				// Compare with size member
				TypeInfo dynamicArrayTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];
				to = IRDoMemberAccess(context, arrayValue, dynamicArrayTypeInfo.structInfo.members[0]);
			}
			else
				to = IRValueImmediate(rangeTypeInfo.arrayInfo.count);

			// Assign 'i'
			IRDoAssignment(context, indexValue, from);

			// Assign 'it'
			IRValue elementVarValue = IRValueValue(elementValueIdx, pointerToElementTypeTableIdx);
			IRValue elementValue = IRDoArrayAccess(context, arrayValue, indexValue, elementTypeIdx);
			elementValue = IRPointerToValue(context, elementValue);
			IRDoAssignment(context, elementVarValue, elementValue);
		}
		else
			CRASH;

		IRLabel *loopLabel     = NewLabel(context, "loop"_s);
		IRLabel *breakLabel    = NewLabel(context, "break"_s);
		IRLabel *continueLabel = NewLabel(context, "continue"_s);

		IRInsertLabelInstruction(context, loopLabel);

		IRLabel *oldBreakLabel    = context->currentBreakLabel;
		IRLabel *oldContinueLabel = context->currentContinueLabel;
		context->currentBreakLabel    = breakLabel;
		context->currentContinueLabel = continueLabel;

		IRInstruction *breakJump = AddInstruction(context);
		breakJump->type = IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS;
		breakJump->conditionalJump2.label = breakLabel;
		breakJump->conditionalJump2.left  = indexValue;
		breakJump->conditionalJump2.right = to;

		IRGenFromExpression(context, expression->forNode.body);

		IRInsertLabelInstruction(context, continueLabel);

		// Increment 'i'
		IRInstruction incrementInst = {};
		incrementInst.type = IRINSTRUCTIONTYPE_ADD;
		incrementInst.binaryOperation.left = indexValue;
		incrementInst.binaryOperation.right = IRValueImmediate(1);
		incrementInst.binaryOperation.out = indexValue;
		*AddInstruction(context) = incrementInst;

		if (rangeTypeIdx == TYPETABLEIDX_STRING_STRUCT || rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
		{
			// Update 'it'
			s64 elementTypeIdx = TYPETABLEIDX_U8;
			if (rangeTypeIdx != TYPETABLEIDX_STRING_STRUCT)
				elementTypeIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;

			u32 elementValueIdx = expression->forNode.elementValueIdx;
			IRValue elementVarValue = IRValueValue(context, elementValueIdx);
			IRValue elementValue = IRDoArrayAccess(context, arrayValue, indexValue, elementTypeIdx);
			elementValue = IRPointerToValue(context, elementValue);
			IRDoAssignment(context, elementVarValue, elementValue);
		}

		IRInstruction *loopJump = AddInstruction(context);
		IRInsertLabelInstruction(context, breakLabel);

		context->currentBreakLabel    = oldBreakLabel;
		context->currentContinueLabel = oldContinueLabel;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->jump.label = loopLabel;

		PopIRScope(context);
	} break;
	case ASTNODETYPE_CONTINUE:
	{
		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.jump.label = context->currentContinueLabel;
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
			s64 returnTypeTableIdx = expression->returnNode.expression->typeTableIdx;
			ASSERT(returnTypeTableIdx > 0);

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
		s64 typeTableIdx = expression->typeOfNode.expression->typeTableIdx;
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
		s64 typeTableIdx = expression->sizeOfNode.expression->typeTableIdx;
		s64 size = context->typeTable[typeTableIdx].size;

		result = IRValueImmediate(size, TYPETABLEIDX_S64);
	} break;
	case ASTNODETYPE_CAST:
	{
		IRValue src = IRGenFromExpression(context, expression->castNode.expression);

		result = IRValueNewValue(context, "_cast"_s, expression->typeTableIdx, 0);
		IRDoAssignment(context, result, src);
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
