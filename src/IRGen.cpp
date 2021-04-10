enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_REGISTER,
	IRVALUETYPE_DATAOFFSET,
	IRVALUETYPE_STACKOFFSET,
	IRVALUETYPE_STACKBASE,
	IRVALUETYPE_DATABASE,
	IRVALUETYPE_IMMEDIATE
};
struct IRValue
{
	IRValueType type;
	union
	{
		s64 registerIdx;
		s64 offset;
		s64 immediate;
	};
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

enum IRInstructionType
{
	IRINSTRUCTIONTYPE_INVALID = -1,

	IRINSTRUCTIONTYPE_LABEL,
	IRINSTRUCTIONTYPE_JUMP,
	IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_CALL,
	IRINSTRUCTIONTYPE_SETPARAMETER,
	IRINSTRUCTIONTYPE_GETPARAMETER,

	IRINSTRUCTIONTYPE_ASSIGNMENT,

	IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_NOT = IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_UNARY_END,

	IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_ADD = IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_SUBTRACT,
	IRINSTRUCTIONTYPE_MULTIPLY,
	IRINSTRUCTIONTYPE_DIVIDE,
	IRINSTRUCTIONTYPE_EQUALS,
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
		IRAssignment assignment;
		IRUnaryOperation unaryOperation;
		IRBinaryOperation binaryOperation;
	};
};

struct IRVariable
{
	String name;
	Type type;
	IRValue value;
};

struct IRScope
{
	DynamicArray<IRVariable> variables;
	s64 stackSize;
};

struct IRContext
{
	ASTRoot *root;
	DynamicArray<IRInstruction> instructions;
	DynamicArray<IRScope> stack;
	u64 currentRegisterId;
	u64 currentLabelId;
};

IRValue NewVirtualRegister(IRContext *context)
{
	IRValue result = {};
	result.type = IRVALUETYPE_REGISTER;
	result.registerIdx = context->currentRegisterId++;
	return result;
}

String NewLabel(IRContext *context)
{
	return TPrintF("labelNo%d", context->currentLabelId++);
}

void PushScope(IRContext *context)
{
	IRScope *newScope = DynamicArrayAdd<IRScope>(&context->stack, realloc);
	DynamicArrayInit<IRVariable>(&newScope->variables, 64, malloc);
	newScope->stackSize = 0;
}

void PopScope(IRContext *context)
{
	--context->stack.size;
}

s64 CalculateTypeSize(IRContext *context, Type type)
{
	ASSERT(type.typeTableIdx >= 0);

	if (type.pointerLevels != 0)
		return 8;

	// @Todo: arrays

	TypeInfo *typeInfo  = &context->root->typeTable[type.typeTableIdx];
	return typeInfo->size;
}

IRValue FindVariable(IRContext *context, String name)
{
	IRValue result = { IRVALUETYPE_INVALID };
	s64 offset = 0;
	for (s64 stackIdx = context->stack.size - 1; stackIdx >= 0; --stackIdx)
	{
		IRScope *scope = &context->stack[stackIdx];
		for (int varIdx = 0; varIdx < scope->variables.size; ++varIdx)
		{
			if (StringEquals(name, scope->variables[varIdx].name))
			{
				result = scope->variables[varIdx].value;
				if (stackIdx == 0)
				{
					result.type = IRVALUETYPE_DATAOFFSET;
					result.offset = result.offset;
				}
				else
				{
					result.offset += offset;
				}
				return result;
			}
		}
		offset -= scope->stackSize;
	}
	return result;
}

IRValue IRGenFromExpression(IRContext *context, ASTExpression *expression)
{
	IRValue result = {};
	result.type = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_BLOCK:
	{
		//PushScope(context);
		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			IRGenFromExpression(context, &expression->block.statements[i]);
		}
		//PopScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		IRScope *stackTop = &context->stack[context->stack.size - 1];
		IRVariable *newVar = DynamicArrayAdd<IRVariable>(&stackTop->variables, realloc);
		*newVar = {};

		newVar->name = expression->variableDeclaration.name;
		newVar->type = expression->variableDeclaration.type->type;
		newVar->value.type = IRVALUETYPE_STACKOFFSET;
		newVar->value.offset = stackTop->stackSize;

		ASSERT(newVar->type.typeTableIdx >= 0);
		TypeInfo *typeInfo  = &context->root->typeTable[newVar->type.typeTableIdx];
		stackTop->stackSize += typeInfo->size;

		// Initial value
		if (expression->variableDeclaration.value)
		{
			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			inst.assignment.src = IRGenFromExpression(context, expression->variableDeclaration.value);
			inst.assignment.dst = newVar->value;

			*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;
		}
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_LABEL;
		inst.label = expression->variableDeclaration.name;
		*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;

		PushScope(context);

		for (int paramIdx = 0; paramIdx < expression->procedureDeclaration.parameters.size;
				++paramIdx)
		{
			ASTVariableDeclaration param = expression->procedureDeclaration.parameters[paramIdx];

			IRScope *stackTop = &context->stack[context->stack.size - 1];
			IRVariable *newVar = DynamicArrayAdd<IRVariable>(&stackTop->variables, realloc);
			*newVar = {};

			newVar->name = param.name;
			newVar->type = param.type->type;
			newVar->value.type = IRVALUETYPE_STACKOFFSET;
			newVar->value.offset = stackTop->stackSize;

			stackTop->stackSize += CalculateTypeSize(context, newVar->type);

			IRInstruction getParamInst;
			getParamInst.type = IRINSTRUCTIONTYPE_GETPARAMETER;
			getParamInst.getParameter.parameterIdx = paramIdx;
			getParamInst.getParameter.out = newVar->value;
			*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = getParamInst;
		}

		IRGenFromExpression(context, expression->procedureDeclaration.body);

		PopScope(context);
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		result = FindVariable(context, expression->variable.name);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		// Set up parameters
		for (int argIdx = 0; argIdx < expression->procedureCall.arguments.size; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];

			IRInstruction setParamInst;
			setParamInst.type = IRINSTRUCTIONTYPE_SETPARAMETER;
			setParamInst.setParameter.parameterIdx = argIdx;
			setParamInst.setParameter.in = IRGenFromExpression(context, arg);
			*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = setParamInst;
		}

		String label = expression->procedureCall.name; // @Improve: oh my god...

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_CALL;
		inst.call.label = label;
		*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		if (expression->unaryOperation.op == TOKEN_OP_POINTERTO)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);

			ASSERT(result.type == IRVALUETYPE_STACKOFFSET || result.type == IRVALUETYPE_DATAOFFSET);

			// Add intermediate instruction to compute address
			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_ADD;
			inst.binaryOperation.left = { IRVALUETYPE_STACKBASE };
			inst.binaryOperation.right = { IRVALUETYPE_IMMEDIATE, result.offset };
			inst.binaryOperation.out = NewVirtualRegister(context);
			*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;

			result = inst.binaryOperation.out;
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
				*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;

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

			*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;
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

			*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;
		}
		else if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
		{
			ASTExpression *leftHand = expression->binaryOperation.leftHand;
			result = IRGenFromExpression(context, expression->binaryOperation.leftHand);

			ASTExpression *rightHand = expression->binaryOperation.rightHand;
			ASSERT(rightHand->nodeType == ASTNODETYPE_VARIABLE);
			String memberName = rightHand->variable.name;

			Type type = leftHand->type;
			ASSERT(type.typeTableIdx >= 0);
			TypeInfo *typeInfo  = &context->root->typeTable[type.typeTableIdx];
			ASSERT(typeInfo->typeCategory == TYPECATEGORY_STRUCT);

			// @Improve: cache member somehow during type checking instead of looking up twice?
			for (int memberIdx = 0; memberIdx < typeInfo->structInfo.members.size; ++memberIdx)
			{
				StructMember member = typeInfo->structInfo.members[memberIdx];
				if (StringEquals(memberName, member.name))
				{
					result.offset += member.offset;
					break;
				}
			}
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
			default:
			{
				inst.type = IRINSTRUCTIONTYPE_INVALID;
			} break;
			}

			*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;
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
		IRValue condition = IRGenFromExpression(context, expression->ifNode.condition);

		IRInstruction *jump = DynamicArrayAdd<IRInstruction>(&context->instructions, realloc);
		IRGenFromExpression(context, expression->ifNode.body);
		IRInstruction *jumpAfterElse = DynamicArrayAdd<IRInstruction>(&context->instructions, realloc);
		IRInstruction *skipLabel = DynamicArrayAdd<IRInstruction>(&context->instructions, realloc);

		String label = NewLabel(context);
		String afterElse = NewLabel(context);

		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = label;
		jump->conditionalJump.condition = condition;

		jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
		jumpAfterElse->jump.label = afterElse;

		skipLabel->type = IRINSTRUCTIONTYPE_LABEL;
		skipLabel->label = label;

		if (expression->ifNode.elseNode)
		{
			IRGenFromExpression(context, expression->ifNode.elseNode);
		}
		IRInstruction *afterElseLabel = DynamicArrayAdd<IRInstruction>(&context->instructions, realloc);
		afterElseLabel->type = IRINSTRUCTIONTYPE_LABEL;
		afterElseLabel->label = afterElse;

	} break;
	case ASTNODETYPE_RETURN:
	{
		IRValue returnValue = IRGenFromExpression(context, expression->ifNode.condition);

		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_RETURN;
		inst.returnValue = returnValue;
		*DynamicArrayAdd<IRInstruction>(&context->instructions, realloc) = inst;
	} break;
	}
	return result;
}

void PrintIRValue(IRValue value)
{
	if (value.asPointer)
		Log("[");

	if (value.type == IRVALUETYPE_REGISTER)
		Log("r%d", value.registerIdx);
	else if (value.type == IRVALUETYPE_DATAOFFSET)
		Log("dataSection[0x%x]", value.offset);
	else if (value.type == IRVALUETYPE_STACKOFFSET)
		Log("stackBase[0x%x]", value.offset);
	else if (value.type == IRVALUETYPE_IMMEDIATE)
		Log("0x%x", value.immediate);
	else if (value.type == IRVALUETYPE_STACKBASE)
		Log("stackBase");
	else if (value.type == IRVALUETYPE_DATABASE)
		Log("dataBase");
	else
		Log("???");

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
	case IRINSTRUCTIONTYPE_NOT:
		Log("!");
		break;
	default:
		Log("<?>");
	}
}

void IRGenMain(ASTRoot *root)
{
	IRContext context;
	context.root = root;
	DynamicArrayInit<IRInstruction>(&context.instructions, 4096, malloc);
	DynamicArrayInit<IRScope>(&context.stack, 64, malloc);
	context.currentRegisterId = 1;
	context.currentLabelId = 1;

	PushScope(&context);

	for (int statementIdx = 0; statementIdx < root->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &root->block.statements[statementIdx];
		IRGenFromExpression(&context, statement);
	}

	const int padding = 20;
	for (int instructionIdx = 0; instructionIdx < context.instructions.size; ++instructionIdx)
	{
		IRInstruction inst = context.instructions[instructionIdx];
		if (inst.type == IRINSTRUCTIONTYPE_LABEL)
		{
			Log("%.*s: ", inst.label.size, inst.label.data);
			for (s64 i = inst.label.size + 2; i < padding; ++i)
				Log(" ");

			++instructionIdx;
			if (instructionIdx >= context.instructions.size)
				break;
			inst = context.instructions[instructionIdx];
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
			Log(" jump \"%.*s\"", inst.conditionalJump.label.size, inst.conditionalJump.label.data);
		}
		else if (inst.type == IRINSTRUCTIONTYPE_CALL)
		{
			Log("call \"%.*s\"", inst.call.label.size, inst.call.label.data);
		}
		else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
		{
			Log("return ");
			PrintIRValue(inst.returnValue);
		}
		else if (inst.type == IRINSTRUCTIONTYPE_GETPARAMETER)
		{
			PrintIRValue(inst.getParameter.out);
			Log(" := parameters[%d]", inst.getParameter.parameterIdx);
		}
		else if (inst.type == IRINSTRUCTIONTYPE_SETPARAMETER)
		{
			Log("parameters[%d] := ", inst.setParameter.parameterIdx);
			PrintIRValue(inst.setParameter.in);
		}
		else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
		{
			PrintIRValue(inst.assignment.dst);
			Log(" = ");
			PrintIRValue(inst.assignment.src);
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
