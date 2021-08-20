enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_REGISTER,
	IRVALUETYPE_VARIABLE,
	IRVALUETYPE_IMMEDIATE
};
enum IRType
{
	IRTYPE_INVALID = -1,
	IRTYPE_VOID,
	IRTYPE_U8,
	IRTYPE_U16,
	IRTYPE_U32,
	IRTYPE_U64,
	IRTYPE_S8,
	IRTYPE_S16,
	IRTYPE_S32,
	IRTYPE_S64,
	IRTYPE_F32,
	IRTYPE_F64
};
struct IRValue
{
	IRValueType valueType;
	union
	{
		s64 registerIdx;
		String variable;
		s64 immediate;
	};
	IRType type;
	bool pointerOf;
	bool asPointer;
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

struct IRCall
{
	String label;
	Array<IRValue> parameters;
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

enum IRInstructionType
{
	IRINSTRUCTIONTYPE_INVALID = -1,

	IRINSTRUCTIONTYPE_LABEL,
	IRINSTRUCTIONTYPE_JUMP,
	IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_CALL,

	IRINSTRUCTIONTYPE_VARIABLE_DECLARATION,
	IRINSTRUCTIONTYPE_ASSIGNMENT,
	IRINSTRUCTIONTYPE_MEMBER_ADDRESS,

	IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_NOT = IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_UNARY_END,

	IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_ADD = IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_SUBTRACT,
	IRINSTRUCTIONTYPE_MULTIPLY,
	IRINSTRUCTIONTYPE_DIVIDE,
	IRINSTRUCTIONTYPE_EQUALS,
	IRINSTRUCTIONTYPE_LESS_THAN,
	IRINSTRUCTIONTYPE_GREATER_THAN,
	IRINSTRUCTIONTYPE_BINARY_END
};
struct IRInstruction
{
	IRInstructionType type;
	union
	{
		String label;
		IRJump jump;
		IRConditionalJump conditionalJump;
		IRCall call;
		IRValue returnValue;
		IRGetParameter getParameter;
		IRSetParameter setParameter;
		IRVariableDeclaration variableDeclaration;
		IRAssignment assignment;
		IRMemberAddress memberAddress;
		IRUnaryOperation unaryOperation;
		IRBinaryOperation binaryOperation;
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
	IRType returnType;
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

IRType ASTTypeToIRType(Context *context, Type astType)
{
	//TypeInfo *typeInfo = &context->typeTable[astType.typeTableIdx];
	switch (astType.typeTableIdx)
	{
		case TYPETABLEIDX_U8:
		case TYPETABLEIDX_BOOL:
			return IRTYPE_U8;
		case TYPETABLEIDX_U16:
			return IRTYPE_U16;
		case TYPETABLEIDX_U32:
			return IRTYPE_U32;
		case TYPETABLEIDX_U64:
			return IRTYPE_U64;
		case TYPETABLEIDX_S8:
			return IRTYPE_S8;
		case TYPETABLEIDX_S16:
			return IRTYPE_S16;
		case TYPETABLEIDX_S32:
			return IRTYPE_S32;
		case TYPETABLEIDX_S64:
		case TYPETABLEIDX_NUMBER:
			return IRTYPE_S64;
		case TYPETABLEIDX_F32:
		case TYPETABLEIDX_FLOATING:
			return IRTYPE_F32;
		case TYPETABLEIDX_F64:
			return IRTYPE_F64;
		case TYPETABLEIDX_VOID:
			return IRTYPE_VOID;
	}
	return IRTYPE_INVALID;
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		IRProcedure procedure = {};
		procedure.name = expression->procedureDeclaration.name;

		procedure.returnType = ASTTypeToIRType(context, expression->type);

		u64 paramCount = expression->procedureDeclaration.parameters.size;
		ArrayInit(&procedure.parameters, paramCount, malloc);
		procedure.parameters.size = paramCount;

		BucketArrayInit(&procedure.instructions);

		PushIRScope(context);

		for (int paramIdx = 0; paramIdx < paramCount; ++paramIdx)
		{
			ASTVariableDeclaration param = expression->procedureDeclaration.parameters[paramIdx];

			IRScope *stackTop = &context->irStack[context->irStack.size - 1];
			IRVariable *newVar = DynamicArrayAdd(&stackTop->variables);
			*newVar = {};

			newVar->name = param.name;
			newVar->type = param.type;

			procedure.parameters[paramIdx] = { param.name, param.type };

#if 0
			IRInstruction getParamInst;
			getParamInst.type = IRINSTRUCTIONTYPE_GET_PARAMETER;
			getParamInst.getParameter.parameterIdx = paramIdx;
			getParamInst.getParameter.out.type = IRVALUETYPE_VARIABLE;
			getParamInst.getParameter.out.variable = newVar->name;
			*AddInstruction(context) = getParamInst;
#endif
		}

		context->currentProcedureIdx = context->irProcedures.size;
		*DynamicArrayAdd(&context->irProcedures) = procedure;

		IRGenFromExpression(context, expression->procedureDeclaration.body);

		PopIRScope(context);
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
		TypeInfo *typeInfo = &context->typeTable[varDecl.type.typeTableIdx];

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
		inst.variableDeclaration.name = varDecl.name;
		inst.variableDeclaration.size = typeInfo->size;
		*AddInstruction(context) = inst;

		IRScope *stackTop = &context->irStack[context->irStack.size - 1];
		IRVariable *newVar = DynamicArrayAdd(&stackTop->variables);
		*newVar = {};

		newVar->name = varDecl.name;
		newVar->type = varDecl.type;

		// Initial value
		if (expression->variableDeclaration.value)
		{
			IRInstruction initialValueInst = {};
			initialValueInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			initialValueInst.assignment.src = IRGenFromExpression(context, varDecl.value);
			initialValueInst.assignment.dst.valueType = IRVALUETYPE_VARIABLE;
			initialValueInst.assignment.dst.variable = newVar->name;
			initialValueInst.assignment.dst.type = ASTTypeToIRType(context, varDecl.type);

			*AddInstruction(context) = initialValueInst;
		}
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		result.valueType = IRVALUETYPE_VARIABLE;
		result.variable = expression->variable.name;
		result.type = ASTTypeToIRType(context, expression->type);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String label = expression->procedureCall.name; // @Improve: oh my god...

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_CALL;
		inst.call.label = label;
		ArrayInit(&inst.call.parameters, expression->procedureCall.arguments.size, malloc);

		// Set up parameters
		for (int argIdx = 0; argIdx < expression->procedureCall.arguments.size; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];

			IRValue param = IRGenFromExpression(context, arg);
			*ArrayAdd(&inst.call.parameters) = param;
		}
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		if (expression->unaryOperation.op == TOKEN_OP_POINTERTO)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);

			ASSERT(result.valueType == IRVALUETYPE_VARIABLE);

			result.pointerOf = true;
		}
		else if (expression->unaryOperation.op == TOKEN_OP_DEREFERENCE)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);

			if (result.asPointer)
			{
				// If already a pointer, add intermediate assignment
				IRInstruction inst = {};
				inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				inst.assignment.src = result;
				inst.assignment.dst = NewVirtualRegister(context);
				*AddInstruction(context) = inst;

				result = inst.assignment.dst;
			}
			result.asPointer = true;
		}
		else
		{
			IRInstruction inst = {};
			inst.unaryOperation.in  = IRGenFromExpression(context, expression->unaryOperation.expression);
			inst.unaryOperation.out = NewVirtualRegister(context);

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
		if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
		{
			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			inst.assignment.src = IRGenFromExpression(context, expression->binaryOperation.rightHand);
			inst.assignment.dst = IRGenFromExpression(context, expression->binaryOperation.leftHand);

			*AddInstruction(context) = inst;
		}
		else if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
		{
			ASTExpression *leftHand = expression->binaryOperation.leftHand;
			IRValue left = IRGenFromExpression(context, expression->binaryOperation.leftHand);

			ASTExpression *rightHand = expression->binaryOperation.rightHand;
			ASSERT(rightHand->nodeType == ASTNODETYPE_VARIABLE);
			String memberName = rightHand->variable.name;

			Type type = leftHand->type;
			ASSERT(type.typeTableIdx >= 0);
			TypeInfo *typeInfo  = &context->typeTable[type.typeTableIdx];
			ASSERT(typeInfo->typeCategory == TYPECATEGORY_STRUCT);

			IRInstruction inst;
			inst.type = IRINSTRUCTIONTYPE_MEMBER_ADDRESS;
			inst.memberAddress.in = left;
			inst.memberAddress.out = NewVirtualRegister(context);
			inst.memberAddress.structName = typeInfo->structInfo.name;
			inst.memberAddress.memberName = memberName;
			*AddInstruction(context) = inst;

			result = inst.memberAddress.out;
			result.asPointer = true;
		}
		else
		{
			IRInstruction inst;
			inst.binaryOperation.left  = IRGenFromExpression(context, expression->binaryOperation.leftHand);
			inst.binaryOperation.right = IRGenFromExpression(context, expression->binaryOperation.rightHand);
			inst.binaryOperation.out = NewVirtualRegister(context);
			inst.binaryOperation.out.type = ASTTypeToIRType(context, expression->type);

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
			case TOKEN_OP_LESSTHAN:
			{
				inst.type = IRINSTRUCTIONTYPE_LESS_THAN;
			} break;
			case TOKEN_OP_GREATERTHAN:
			{
				inst.type = IRINSTRUCTIONTYPE_GREATER_THAN;
			} break;
			default:
			{
				inst.type = IRINSTRUCTIONTYPE_INVALID;
			} break;
			}

			*AddInstruction(context) = inst;
			result = inst.binaryOperation.out;
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		result.valueType = IRVALUETYPE_IMMEDIATE;
		union
		{
			s64 asInt;
			f64 asFloat;
		};

		switch (expression->literal.type)
		{
		case LITERALTYPE_FLOATING:
			asFloat = expression->literal.floating;
		case LITERALTYPE_INTEGER:
			asInt = expression->literal.integer;
		}

		result.immediate = asInt;
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
	if (value.asPointer)
		Log("[");

	if (value.pointerOf)
		Log("address(");

	if (value.valueType == IRVALUETYPE_REGISTER)
		Log("$r%d", value.registerIdx);
	else if (value.valueType == IRVALUETYPE_VARIABLE)
		Log("%.*s", value.variable.size, value.variable.data);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE)
		Log("0x%x", value.immediate);
	else
		Log("???");

	if (value.pointerOf)
		Log(")");

	if (value.asPointer)
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
	case IRINSTRUCTIONTYPE_LESS_THAN:
		Log("<");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		Log(">");
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
	}
	return "???"_s;
}

void IRGenMain(Context *context)
{
	DynamicArrayInit(&context->irProcedures, 64);
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

	const int padding = 20;
	const u64 procedureCount = context->irProcedures.size;
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		IRProcedure proc = context->irProcedures[procedureIdx];
		String returnTypeStr = IRTypeToStr(proc.returnType);

		Log("proc %.*s(", proc.name.size, proc.name.data);
		Log(")");
		if (proc.returnType != IRTYPE_VOID)
			Log(" -> %.*s", returnTypeStr.size, returnTypeStr.data);
		Log("\n");

		const u64 instructionCount = BucketArrayCount(&proc.instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc.instructions[instructionIdx];
			if (inst.type == IRINSTRUCTIONTYPE_LABEL)
			{
				Log("%.*s: ", inst.label.size, inst.label.data);
				for (s64 i = inst.label.size + 2; i < padding; ++i)
					Log(" ");

				IRInstruction nextInst = proc.instructions[instructionIdx + 1];
				if (nextInst.type != IRINSTRUCTIONTYPE_LABEL)
				{
					++instructionIdx;
					if (instructionIdx >= instructionCount)
						break;
					inst = proc.instructions[instructionIdx];
				}
			}
			else
			{
				for (s64 i = 0; i < padding; ++i)
					Log(" ");
			}

			if (inst.type == IRINSTRUCTIONTYPE_JUMP)
			{
				Log("jump \"%.*s\"", inst.jump.label.size, inst.jump.label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO)
			{
				Log("if !");
				PrintIRValue(inst.conditionalJump.condition);
				Log(" jump %.*s", inst.conditionalJump.label.size, inst.conditionalJump.label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_CALL)
			{
				Log("call %.*s", inst.call.label.size, inst.call.label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
			{
				Log("return ");
				PrintIRValue(inst.returnValue);
			}
#if 0
			else if (inst.type == IRINSTRUCTIONTYPE_GET_PARAMETER)
			{
				PrintIRValue(inst.getParameter.out);
				Log(" := parameters[%d]", inst.getParameter.parameterIdx);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_SET_PARAMETER)
			{
				Log("parameters[%d] := ", inst.setParameter.parameterIdx);
				PrintIRValue(inst.setParameter.in);
			}
#endif
			else if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				Log("%.*s : %d bytes", inst.variableDeclaration.name.size, inst.variableDeclaration.name.data,
						inst.variableDeclaration.size);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			{
				PrintIRValue(inst.assignment.dst);
				Log(" = ");
				PrintIRValue(inst.assignment.src);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_MEMBER_ADDRESS)
			{
				PrintIRValue(inst.memberAddress.out);
				Log(" = ");
				PrintIRValue(inst.memberAddress.in);
				Log(" -> offset(%.*s::%.*s)",
						inst.memberAddress.structName.size, inst.memberAddress.structName.data,
						inst.memberAddress.memberName.size, inst.memberAddress.memberName.data);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
			{
				PrintIRValue(inst.unaryOperation.out);
				Log(" := ");
				PrintIRInstructionOperator(inst);
				PrintIRValue(inst.unaryOperation.in);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
			{
				PrintIRValue(inst.binaryOperation.out);
				Log(" := ");
				PrintIRValue(inst.binaryOperation.left);
				Log(" ");
				PrintIRInstructionOperator(inst);
				Log(" ");
				PrintIRValue(inst.binaryOperation.right);
			}
			Log("\n");
		}
	}
	Log("\n");
}
