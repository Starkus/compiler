enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_REGISTER,
	IRVALUETYPE_VARIABLE,
	IRVALUETYPE_IMMEDIATE,
	IRVALUETYPE_IMMEDIATE_FLOAT,
	IRVALUETYPE_IMMEDIATE_STRING
};
enum IRType
{
	IRTYPE_INVALID = -1,
	IRTYPE_VOID,
	IRTYPE_PTR,
	IRTYPE_U8,
	IRTYPE_U16,
	IRTYPE_U32,
	IRTYPE_U64,
	IRTYPE_S8,
	IRTYPE_S16,
	IRTYPE_S32,
	IRTYPE_S64,
	IRTYPE_F32,
	IRTYPE_F64,
	IRTYPE_STRUCT,
	IRTYPE_STRING
};
struct IRTypeInfo
{
	IRType type;
	bool isPointer;
};
enum IRPointerType
{
	IRPOINTERTYPE_NONE,
	IRPOINTERTYPE_POINTERTO,
	IRPOINTERTYPE_DEREFERENCE,
};
struct IRValue
{
	IRValueType valueType;
	union
	{
		s64 registerIdx;
		String variable;
		s64 immediate;
		f64 immediateFloat;
		String immediateString;
	};
	IRTypeInfo typeInfo;
	IRPointerType pointerType;
};

struct IRJump
{
	String label;
};

struct IRConditionalJump
{
	String label;
	IRValue condition;
};

struct IRProcedureCall
{
	String label;
	Array<IRValue> parameters;
	IRValue out;
};

struct IRAssignment
{
	IRValue src;
	IRValue dst;
};

struct IRMemberAddress
{
	IRValue in;
	IRValue out;
	String structName;
	String memberName;
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
	String name;
	s64 size;
};

struct IRIntrinsicMemcpy
{
	IRValue src;
	IRValue dst;
	u64 size;
};

enum IRInstructionType
{
	IRINSTRUCTIONTYPE_INVALID = -1,

	IRINSTRUCTIONTYPE_LABEL,
	IRINSTRUCTIONTYPE_JUMP,
	IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL,

	IRINSTRUCTIONTYPE_VARIABLE_DECLARATION,
	IRINSTRUCTIONTYPE_ASSIGNMENT,
	IRINSTRUCTIONTYPE_MEMBER_ACCESS,

	IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_NOT = IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_UNARY_END,

	IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_ADD = IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_SUBTRACT,
	IRINSTRUCTIONTYPE_MULTIPLY,
	IRINSTRUCTIONTYPE_DIVIDE,
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
		String label;
		IRJump jump;
		IRConditionalJump conditionalJump;
		IRProcedureCall procedureCall;
		IRValue returnValue;
		IRGetParameter getParameter;
		IRSetParameter setParameter;
		IRVariableDeclaration variableDeclaration;
		IRAssignment assignment;
		IRMemberAddress memberAddress;
		IRUnaryOperation unaryOperation;
		IRBinaryOperation binaryOperation;

		IRIntrinsicMemcpy memcpy;
	};
};

struct IRVariable
{
	String name;
	Type type;
};

struct IRScope
{
	DynamicArray<IRVariable, malloc, realloc> variables;
};

struct IRProcedure
{
	String name;
	Array<IRVariable> parameters;
	BucketArray<IRInstruction, 256, malloc, realloc> instructions;
	IRTypeInfo returnTypeInfo;
	u64 registerCount;
};

struct IRStaticVariable
{
	String name;
	IRTypeInfo typeInfo;
	IRValue initialValue;
};

IRValue NewVirtualRegister(Context *context)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_REGISTER;
	result.registerIdx = context->currentRegisterId++;
	return result;
}

String NewLabel(Context *context, String prefix)
{
	return TPrintF("%.*s%d", prefix.size, prefix.data, context->currentLabelId++);
}

void PushIRScope(Context *context)
{
	IRScope *newScope = DynamicArrayAdd(&context->irStack);
	DynamicArrayInit(&newScope->variables, 64);
}

void PopIRScope(Context *context)
{
	--context->irStack.size;
}

s64 CalculateTypeSize(Context *context, Type type)
{
	ASSERT(type.typeTableIdx >= 0);

	if (type.pointerLevels != 0)
		return 8;

	// @Todo: arrays

	TypeInfo *typeInfo  = &context->typeTable[type.typeTableIdx];
	return typeInfo->size;
}

inline IRInstruction *AddInstruction(Context *context)
{
	return BucketArrayAdd(&context->irProcedures[context->currentProcedureIdx].instructions);
}

IRTypeInfo ASTTypeToIRTypeInfo(Context *context, Type astType)
{
	IRTypeInfo result;
	result.type = IRTYPE_INVALID;
	result.isPointer = astType.pointerLevels > 0;

	if (astType.pointerLevels > 1)
		result.type = IRTYPE_PTR;
	else if (astType.typeTableIdx >= TYPETABLEIDX_STRUCT_BEGIN)
		result.type = IRTYPE_STRUCT;
	else switch (astType.typeTableIdx)
	{
	case TYPETABLEIDX_U8:
	case TYPETABLEIDX_BOOL:
		result.type = IRTYPE_U8;
		break;
	case TYPETABLEIDX_U16:
		result.type = IRTYPE_U16;
		break;
	case TYPETABLEIDX_U32:
		result.type = IRTYPE_U32;
		break;
	case TYPETABLEIDX_U64:
		result.type = IRTYPE_U64;
		break;
	case TYPETABLEIDX_S8:
		result.type = IRTYPE_S8;
		break;
	case TYPETABLEIDX_S16:
		result.type = IRTYPE_S16;
		break;
	case TYPETABLEIDX_S32:
		result.type = IRTYPE_S32;
		break;
	case TYPETABLEIDX_S64:
	case TYPETABLEIDX_NUMBER:
		result.type = IRTYPE_S64;
		break;
	case TYPETABLEIDX_F32:
	case TYPETABLEIDX_FLOATING:
		result.type = IRTYPE_F32;
		break;
	case TYPETABLEIDX_F64:
		result.type = IRTYPE_F64;
		break;
	case TYPETABLEIDX_VOID:
		result.type = IRTYPE_VOID;
		break;
	}
	return result;
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression);

IRInstruction IRInstructionFromBinaryOperation(Context *context, ASTExpression *expression)
{
	IRInstruction inst;

	ASTExpression *rightHand = expression->binaryOperation.rightHand;
	ASTExpression *leftHand  = expression->binaryOperation.leftHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
	{
		IRValue left = IRGenFromExpression(context, leftHand);

		ASSERT(rightHand->nodeType == ASTNODETYPE_VARIABLE);
		String memberName = rightHand->variable.name;

		Type type = leftHand->type;
		ASSERT(type.typeTableIdx >= 0);
		TypeInfo *typeInfo  = &context->typeTable[type.typeTableIdx];
		ASSERT(typeInfo->typeCategory == TYPECATEGORY_STRUCT);

		inst.type = IRINSTRUCTIONTYPE_MEMBER_ACCESS;
		inst.memberAddress.in = left;
		inst.memberAddress.structName = typeInfo->structInfo.name;
		inst.memberAddress.memberName = memberName;
	}
	else
	{
		inst.binaryOperation.left  = IRGenFromExpression(context, leftHand);
		inst.binaryOperation.right = IRGenFromExpression(context, rightHand);

		switch (expression->binaryOperation.op)
		{
		case TOKEN_OP_PLUS:
		{
			inst.type = IRINSTRUCTIONTYPE_ADD;
		} break;
		case TOKEN_OP_MINUS:
		{
			inst.type = IRINSTRUCTIONTYPE_SUBTRACT;
		} break;
		case TOKEN_OP_MULTIPLY:
		{
			inst.type = IRINSTRUCTIONTYPE_MULTIPLY;
		} break;
		case TOKEN_OP_DIVIDE:
		{
			inst.type = IRINSTRUCTIONTYPE_DIVIDE;
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
		default:
		{
			inst.type = IRINSTRUCTIONTYPE_INVALID;
		} break;
		}
	}

	return inst;
}

IRInstruction IRInstructionFromAssignment(Context *context, IRValue *leftValue, ASTExpression *rightHand)
{
	if (rightHand->type.typeTableIdx >= TYPETABLEIDX_STRUCT_BEGIN &&
			rightHand->type.pointerLevels == 0 && rightHand->type.arrayCount == 0)
	{
		TypeInfo *structTypeInfo = &context->typeTable[rightHand->type.typeTableIdx];
		ASSERT(structTypeInfo);

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY;
		inst.memcpy.src = IRGenFromExpression(context, rightHand);
		inst.memcpy.dst = *leftValue;
		inst.memcpy.size = structTypeInfo->size;

		// @Cleanup: is this the best way?
		inst.memcpy.src.pointerType = IRPOINTERTYPE_POINTERTO;
		inst.memcpy.dst.pointerType = IRPOINTERTYPE_POINTERTO;

		return inst;
	}
	else if (rightHand->nodeType == ASTNODETYPE_BINARY_OPERATION &&
		rightHand->binaryOperation.op != TOKEN_OP_ASSIGNMENT)
	{
		// This is to save a useless intermediate value, we use the assignment of the
		// right-hand binary operation instead of making a new one.
		IRInstruction inst = IRInstructionFromBinaryOperation(context, rightHand);
		inst.binaryOperation.out = *leftValue;
		return inst;
	}
	else
	{
		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.src = IRGenFromExpression(context, rightHand);
		inst.assignment.dst = *leftValue;

		return inst;
	}
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		u64 prevProcedureIdx = context->currentProcedureIdx;

		context->currentProcedureIdx = context->irProcedures.size;
		IRProcedure *procedure = DynamicArrayAdd(&context->irProcedures);

		*procedure = {};
		procedure->name = expression->procedureDeclaration.name;

		procedure->returnTypeInfo = ASTTypeToIRTypeInfo(context, expression->type);

		u64 paramCount = expression->procedureDeclaration.parameters.size;
		ArrayInit(&procedure->parameters, paramCount, malloc);
		procedure->parameters.size = paramCount;

		BucketArrayInit(&procedure->instructions);

		PushIRScope(context);

		for (int paramIdx = 0; paramIdx < paramCount; ++paramIdx)
		{
			ASTVariableDeclaration param = expression->procedureDeclaration.parameters[paramIdx];

			IRScope *stackTop = &context->irStack[context->irStack.size - 1];
			IRVariable *newVar = DynamicArrayAdd(&stackTop->variables);
			*newVar = {};

			newVar->name = param.name;
			newVar->type = param.type;

			procedure->parameters[paramIdx] = { param.name, param.type };
		}

		context->currentRegisterId = 0;
		IRGenFromExpression(context, expression->procedureDeclaration.body);
		procedure->registerCount = context->currentRegisterId;

		PopIRScope(context);
		context->currentProcedureIdx = prevProcedureIdx;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			IRGenFromExpression(context, &expression->block.statements[i]);
		}
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		bool isGlobalScope = context->currentProcedureIdx == U64_MAX;
		if (isGlobalScope && !varDecl.isStatic)
			PrintError(context, expression->any.loc, "Global variables have to be constant"_s);

		if (varDecl.isStatic)
		{
			IRStaticVariable newStaticVar = {};
			newStaticVar.name = varDecl.name;
			newStaticVar.typeInfo = ASTTypeToIRTypeInfo(context, varDecl.type);
			newStaticVar.initialValue.valueType = IRVALUETYPE_INVALID;

			// Initial value
			if (varDecl.value)
			{
				if (varDecl.value->nodeType != ASTNODETYPE_LITERAL)
				{
					// @Todo: Somehow execute constant expressions and bake them?
					PrintError(context, expression->any.loc, "Non literal initial values for global variables not yet supported"_s);
				}
				newStaticVar.initialValue = IRGenFromExpression(context, varDecl.value);
			}

			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;
		}
		else
		{
			IRVariable newVar = {};
			newVar.name = varDecl.name;
			newVar.type = varDecl.type;

			IRScope *stackTop = &context->irStack[context->irStack.size - 1];
			*DynamicArrayAdd(&stackTop->variables) = newVar;

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
			inst.variableDeclaration.name = varDecl.name;
			inst.variableDeclaration.size = CalculateTypeSize(context, varDecl.type);
			*AddInstruction(context) = inst;

			// Initial value
			if (varDecl.value)
			{
				IRValue leftValue = {};
				leftValue.valueType = IRVALUETYPE_VARIABLE;
				leftValue.variable = newVar.name;
				leftValue.typeInfo = ASTTypeToIRTypeInfo(context, varDecl.type);
				leftValue.pointerType = IRPOINTERTYPE_NONE;

				*AddInstruction(context) = IRInstructionFromAssignment(context, &leftValue, varDecl.value);
			}
		}
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		result.valueType = IRVALUETYPE_VARIABLE;
		result.variable = expression->variable.name;
		result.typeInfo = ASTTypeToIRTypeInfo(context, expression->type);
		result.pointerType = IRPOINTERTYPE_NONE;
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String label = expression->procedureCall.name; // @Improve: oh my god...

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
		inst.procedureCall.label = label;
		ArrayInit(&inst.procedureCall.parameters, expression->procedureCall.arguments.size, malloc);

		// Set up parameters
		for (int argIdx = 0; argIdx < expression->procedureCall.arguments.size; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];

			IRValue param = IRGenFromExpression(context, arg);
			IRValue paramReg = NewVirtualRegister(context);
			if (param.pointerType == IRPOINTERTYPE_NONE)
				paramReg.typeInfo = param.typeInfo;
			else
				paramReg.typeInfo = { IRTYPE_PTR, false };
			paramReg.pointerType = IRPOINTERTYPE_NONE;

			IRInstruction paramIntermediateInst = {};
			paramIntermediateInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			paramIntermediateInst.assignment.src = param;
			paramIntermediateInst.assignment.dst = paramReg;
			*AddInstruction(context) = paramIntermediateInst;

			*ArrayAdd(&inst.procedureCall.parameters) = paramReg;
		}

		if (expression->type.typeTableIdx != TYPETABLEIDX_VOID)
		{
			inst.procedureCall.out = NewVirtualRegister(context);
			inst.procedureCall.out.typeInfo = ASTTypeToIRTypeInfo(context, expression->type);

			result = inst.procedureCall.out;
		}

		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		if (expression->unaryOperation.op == TOKEN_OP_POINTER_TO)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);
			ASSERT(result.valueType == IRVALUETYPE_VARIABLE);
			result.pointerType = IRPOINTERTYPE_POINTERTO;
		}
		else if (expression->unaryOperation.op == TOKEN_OP_DEREFERENCE)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);

			if (result.pointerType == IRPOINTERTYPE_DEREFERENCE)
			{
				IRInstruction inst = {};
				inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;

				// The resulting type
				IRTypeInfo typeInfo = ASTTypeToIRTypeInfo(context, expression->type);

				// Source side will be cast to a pointer of resulting type and then dereferenced
				inst.assignment.src = result;
				inst.assignment.src.typeInfo = typeInfo;
				inst.assignment.src.typeInfo.isPointer = true;
				inst.assignment.src.pointerType = IRPOINTERTYPE_DEREFERENCE;

				// Destination side will be of the dereferenced type
				inst.assignment.dst = NewVirtualRegister(context);
				inst.assignment.dst.typeInfo = typeInfo;
				inst.assignment.dst.pointerType = IRPOINTERTYPE_NONE;

				*AddInstruction(context) = inst;

				result = inst.assignment.dst;
			}
			else
			{
				result.pointerType = IRPOINTERTYPE_DEREFERENCE;
			}
		}
		else
		{
			IRInstruction inst = {};
			inst.unaryOperation.in  = IRGenFromExpression(context, expression->unaryOperation.expression);
			inst.unaryOperation.out = NewVirtualRegister(context);
			inst.unaryOperation.out.typeInfo = ASTTypeToIRTypeInfo(context, expression->type);

			switch (expression->unaryOperation.op)
			{
			case TOKEN_OP_NOT:
			{
				inst.type = IRINSTRUCTIONTYPE_NOT;
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
			*AddInstruction(context) = IRInstructionFromAssignment(context, &leftValue, rightHand);
		}
		else
		{
			IRInstruction inst = IRInstructionFromBinaryOperation(context, expression);

			if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
			{
				inst.memberAddress.out = NewVirtualRegister(context);
				inst.memberAddress.out.typeInfo = { IRTYPE_PTR, false };

				result = inst.memberAddress.out;
				result.typeInfo = ASTTypeToIRTypeInfo(context, expression->type);
				result.typeInfo.isPointer = true;
				result.pointerType = IRPOINTERTYPE_DEREFERENCE;
			}
			else
			{
				inst.binaryOperation.out = NewVirtualRegister(context);
				inst.binaryOperation.out.typeInfo = ASTTypeToIRTypeInfo(context, expression->type);

				result = inst.binaryOperation.out;
			}

			*AddInstruction(context) = inst;
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_FLOATING:
			result.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
			result.typeInfo = { IRTYPE_F64, false };
			result.immediateFloat = expression->literal.floating;
			break;
		case LITERALTYPE_INTEGER:
			result.valueType = IRVALUETYPE_IMMEDIATE;
			result.typeInfo = { IRTYPE_S64, false };
			result.immediate = expression->literal.integer;
			break;
		case LITERALTYPE_STRING:
		{
			static u64 stringStaticVarUniqueID = 0;

			IRStaticVariable newStaticVar = {};
			newStaticVar.name = TPrintF("staticString%d", stringStaticVarUniqueID++);
			newStaticVar.typeInfo.type = IRTYPE_STRING;
			newStaticVar.typeInfo.isPointer = false;
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRING;
			newStaticVar.initialValue.immediateString = expression->literal.string;
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;

			result.valueType = IRVALUETYPE_VARIABLE;
			result.variable = newStaticVar.name;
		} break;
		}
	} break;
	case ASTNODETYPE_IF:
	{
		IRInstruction *jump = AddInstruction(context);

		// Body!
		IRGenFromExpression(context, expression->ifNode.body);

		IRInstruction *jumpAfterElse = nullptr;
		if (expression->ifNode.elseNode)
			// If we have an else, add a jump instruction here.
			jumpAfterElse = AddInstruction(context);

		IRInstruction *skipLabelInst = AddInstruction(context);

		String skipLabel = NewLabel(context, "skipIf"_s);
		String afterElseLabel = NewLabel(context, "afterElse"_s);

		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = skipLabel;
		jump->conditionalJump.condition = IRGenFromExpression(context, expression->ifNode.condition);

		skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		skipLabelInst->label = skipLabel;

		if (expression->ifNode.elseNode)
		{
			jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
			jumpAfterElse->jump.label = afterElseLabel;

			IRGenFromExpression(context, expression->ifNode.elseNode);
		}
		IRInstruction *afterElseLabelInst = AddInstruction(context);
		afterElseLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		afterElseLabelInst->label = afterElseLabel;

	} break;
	case ASTNODETYPE_WHILE:
	{
		IRInstruction *loopLabelInst = AddInstruction(context);

		IRValue condition = IRGenFromExpression(context, expression->whileNode.condition);

		String breakLabel = NewLabel(context, "break"_s);

		String oldBreakLabel = context->currentBreakLabel;
		context->currentBreakLabel = breakLabel;

		IRInstruction *jump = AddInstruction(context);
		IRGenFromExpression(context, expression->whileNode.body);
		IRInstruction *loopJump = AddInstruction(context);
		IRInstruction *breakLabelInst = AddInstruction(context);

		context->currentBreakLabel = oldBreakLabel;

		String loopLabel = NewLabel(context, "loop"_s);
		loopLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		loopLabelInst->label = loopLabel;

		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = breakLabel;
		jump->conditionalJump.condition = condition;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->jump.label = loopLabel;

		breakLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		breakLabelInst->label = breakLabel;
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

		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_RETURN;
		inst.returnValue = returnValue;
		*AddInstruction(context) = inst;
	} break;
	}
	return result;
}

void PrintIRValue(IRValue value)
{
	if (value.pointerType == IRPOINTERTYPE_DEREFERENCE)
		Log("[");

	if (value.pointerType == IRPOINTERTYPE_POINTERTO)
		Log("address(");

	if (value.valueType == IRVALUETYPE_REGISTER)
		Log("$r%d", value.registerIdx);
	else if (value.valueType == IRVALUETYPE_VARIABLE)
		Log("%.*s", value.variable.size, value.variable.data);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE)
		Log("0x%x", value.immediate);
	else
		Log("???");

	if (value.pointerType == IRPOINTERTYPE_POINTERTO)
		Log(")");

	if (value.pointerType == IRPOINTERTYPE_DEREFERENCE)
		Log("]");
}

void PrintIRInstructionOperator(IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		Log("+");
		break;
	case IRINSTRUCTIONTYPE_SUBTRACT:
		Log("-");
		break;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		Log("*");
		break;
	case IRINSTRUCTIONTYPE_DIVIDE:
		Log("/");
		break;
	case IRINSTRUCTIONTYPE_EQUALS:
		Log("==");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		Log(">");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		Log(">=");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		Log("<");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		Log("<=");
		break;
	case IRINSTRUCTIONTYPE_NOT:
		Log("!");
		break;
	default:
		Log("<?>");
	}
}

String IRTypeToStr(IRType type)
{
	switch (type)
	{
	case IRTYPE_VOID:
		return "void"_s;
	case IRTYPE_PTR:
		return "ptr"_s;
	case IRTYPE_U8:
		return "u8"_s;
	case IRTYPE_U16:
		return "u16"_s;
	case IRTYPE_U32:
		return "u32"_s;
	case IRTYPE_U64:
		return "u64"_s;
	case IRTYPE_S8:
		return "s8"_s;
	case IRTYPE_S16:
		return "s16"_s;
	case IRTYPE_S32:
		return "s32"_s;
	case IRTYPE_S64:
		return "s64"_s;
	case IRTYPE_F32:
		return "f32"_s;
	case IRTYPE_F64:
		return "f64"_s;
	}
	return "u8"_s; // Default to u8 for things like structs
}

String IRTypeInfoToStr(IRTypeInfo typeInfo)
{
	String result = IRTypeToStr(typeInfo.type);
	if (typeInfo.isPointer)
		result = TPrintF("%.*s", result.size, result.data);
	return result;
}

void IRGenMain(Context *context)
{
	DynamicArrayInit(&context->irProcedures, 64);
	DynamicArrayInit(&context->irStaticVariables, 64);
	DynamicArrayInit(&context->irStack, 64);
	context->currentProcedureIdx = U64_MAX;
	context->currentRegisterId = 1;
	context->currentLabelId = 1;

	PushIRScope(context);

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		IRGenFromExpression(context, statement);
	}
}
