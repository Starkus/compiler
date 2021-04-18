enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_REGISTER,
	IRVALUETYPE_VARIABLE,
	IRVALUETYPE_IMMEDIATE
};
struct IRValue
{
	IRValueType type;
	union
	{
		s64 registerIdx;
		String variable;
		s64 immediate;
	};
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
	IRINSTRUCTIONTYPE_SET_PARAMETER,
	IRINSTRUCTIONTYPE_GET_PARAMETER,

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
	DynamicArray<IRVariable> variables;
};

IRValue NewVirtualRegister(Context *context)
{
	IRValue result = {};
	result.type = IRVALUETYPE_REGISTER;
	result.registerIdx = context->currentRegisterId++;
	return result;
}

String NewLabel(Context *context, String prefix)
{
	return TPrintF("%.*s%d", prefix.size, prefix.data, context->currentLabelId++);
}

void PushIRScope(Context *context)
{
	IRScope *newScope = DynamicArrayAdd(&context->irStack, realloc);
	DynamicArrayInit(&newScope->variables, 64, malloc);
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

IRValue IRGenFromExpression(Context *context, ASTExpression *expression)
{
	IRValue result = {};
	result.type = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
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
		*DynamicArrayAdd(&context->instructions, realloc) = inst;

		IRScope *stackTop = &context->irStack[context->irStack.size - 1];
		IRVariable *newVar = DynamicArrayAdd(&stackTop->variables, realloc);
		*newVar = {};

		newVar->name = varDecl.name;
		newVar->type = varDecl.type;

		// Initial value
		if (expression->variableDeclaration.value)
		{
			IRInstruction initialValueInst = {};
			initialValueInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			initialValueInst.assignment.src = IRGenFromExpression(context, varDecl.value);
			initialValueInst.assignment.dst.type = IRVALUETYPE_VARIABLE;
			initialValueInst.assignment.dst.variable = newVar->name;

			*DynamicArrayAdd(&context->instructions, realloc) = initialValueInst;
		}
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_LABEL;
		inst.label = expression->variableDeclaration.name;
		*DynamicArrayAdd(&context->instructions, realloc) = inst;

		PushIRScope(context);

		for (int paramIdx = 0; paramIdx < expression->procedureDeclaration.parameters.size;
				++paramIdx)
		{
			ASTVariableDeclaration param = expression->procedureDeclaration.parameters[paramIdx];

			IRScope *stackTop = &context->irStack[context->irStack.size - 1];
			IRVariable *newVar = DynamicArrayAdd(&stackTop->variables, realloc);
			*newVar = {};

			newVar->name = param.name;
			newVar->type = param.type;

			IRInstruction getParamInst;
			getParamInst.type = IRINSTRUCTIONTYPE_GET_PARAMETER;
			getParamInst.getParameter.parameterIdx = paramIdx;
			getParamInst.getParameter.out.type = IRVALUETYPE_VARIABLE;
			getParamInst.getParameter.out.variable = newVar->name;
			*DynamicArrayAdd(&context->instructions, realloc) = getParamInst;
		}

		IRGenFromExpression(context, expression->procedureDeclaration.body);

		PopIRScope(context);
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		result.type = IRVALUETYPE_VARIABLE;
		result.variable = expression->variable.name;
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		// Set up parameters
		for (int argIdx = 0; argIdx < expression->procedureCall.arguments.size; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];

			IRInstruction setParamInst;
			setParamInst.type = IRINSTRUCTIONTYPE_SET_PARAMETER;
			setParamInst.setParameter.parameterIdx = argIdx;
			setParamInst.setParameter.in = IRGenFromExpression(context, arg);
			*DynamicArrayAdd(&context->instructions, realloc) = setParamInst;
		}

		String label = expression->procedureCall.name; // @Improve: oh my god...

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_CALL;
		inst.call.label = label;
		*DynamicArrayAdd(&context->instructions, realloc) = inst;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		if (expression->unaryOperation.op == TOKEN_OP_POINTERTO)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);

			ASSERT(result.type == IRVALUETYPE_VARIABLE);

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
				*DynamicArrayAdd(&context->instructions, realloc) = inst;

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

			*DynamicArrayAdd(&context->instructions, realloc) = inst;
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

			*DynamicArrayAdd(&context->instructions, realloc) = inst;
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
			*DynamicArrayAdd(&context->instructions, realloc) = inst;

			result = inst.memberAddress.out;
			result.asPointer = true;
		}
		else
		{
			IRInstruction inst;
			inst.binaryOperation.left  = IRGenFromExpression(context, expression->binaryOperation.leftHand);
			inst.binaryOperation.right = IRGenFromExpression(context, expression->binaryOperation.rightHand);
			inst.binaryOperation.out = NewVirtualRegister(context);

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

			*DynamicArrayAdd(&context->instructions, realloc) = inst;
			result = inst.binaryOperation.out;
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		result.type = IRVALUETYPE_IMMEDIATE;
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
		IRInstruction *jump = DynamicArrayAdd(&context->instructions, realloc);

		// Body!
		IRGenFromExpression(context, expression->ifNode.body);

		IRInstruction *jumpAfterElse = nullptr;
		if (expression->ifNode.elseNode)
			// If we have an else, add a jump instruction here.
			jumpAfterElse = DynamicArrayAdd(&context->instructions, realloc);

		IRInstruction *skipLabelInst = DynamicArrayAdd(&context->instructions, realloc);

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
		IRInstruction *afterElseLabelInst = DynamicArrayAdd(&context->instructions, realloc);
		afterElseLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		afterElseLabelInst->label = afterElseLabel;

	} break;
	case ASTNODETYPE_WHILE:
	{
		IRInstruction *loopLabelInst = DynamicArrayAdd(&context->instructions, realloc);

		IRValue condition = IRGenFromExpression(context, expression->whileNode.condition);

		String breakLabel = NewLabel(context, "break"_s);

		String oldBreakLabel = context->currentBreakLabel;
		context->currentBreakLabel = breakLabel;

		IRInstruction *jump = DynamicArrayAdd(&context->instructions, realloc);
		IRGenFromExpression(context, expression->whileNode.body);
		IRInstruction *loopJump = DynamicArrayAdd(&context->instructions, realloc);
		IRInstruction *breakLabelInst = DynamicArrayAdd(&context->instructions, realloc);

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
		*DynamicArrayAdd(&context->instructions, realloc) = inst;
	} break;
	case ASTNODETYPE_RETURN:
	{
		IRValue returnValue = IRGenFromExpression(context, expression->returnNode.expression);

		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_RETURN;
		inst.returnValue = returnValue;
		*DynamicArrayAdd(&context->instructions, realloc) = inst;
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

	if (value.type == IRVALUETYPE_REGISTER)
		Log("$r%d", value.registerIdx);
	else if (value.type == IRVALUETYPE_VARIABLE)
		Log("%.*s", value.variable.size, value.variable.data);
	else if (value.type == IRVALUETYPE_IMMEDIATE)
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

void IRGenMain(Context *context)
{
	DynamicArrayInit(&context->instructions, 4096, malloc);
	DynamicArrayInit(&context->irStack, 64, malloc);
	context->currentRegisterId = 1;
	context->currentLabelId = 1;

	PushIRScope(context);

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		IRGenFromExpression(context, statement);
	}

	const int padding = 20;
	for (int instructionIdx = 0; instructionIdx < context->instructions.size; ++instructionIdx)
	{
		IRInstruction inst = context->instructions[instructionIdx];
		if (inst.type == IRINSTRUCTIONTYPE_LABEL)
		{
			Log("%.*s: ", inst.label.size, inst.label.data);
			for (s64 i = inst.label.size + 2; i < padding; ++i)
				Log(" ");

			IRInstruction nextInst = context->instructions[instructionIdx + 1];
			if (nextInst.type != IRINSTRUCTIONTYPE_LABEL)
			{
				++instructionIdx;
				if (instructionIdx >= context->instructions.size)
					break;
				inst = context->instructions[instructionIdx];
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
	Log("\n");
}
