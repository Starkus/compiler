struct PrintContext
{
	int indentLevels;
};

String OperatorToString(s32 op)
{
	switch (op)
	{
		case TOKEN_OP_ASSIGNMENT:
			return "="_s;
		case TOKEN_OP_EQUALS:
			return "=="_s;
		case TOKEN_OP_PLUS:
			return "+"_s;
		case TOKEN_OP_MINUS:
			return "-"_s;
		case TOKEN_OP_MULTIPLY:
			return "*"_s;
		case TOKEN_OP_DIVIDE:
			return "/"_s;
		case TOKEN_OP_ARROW:
			return "->"_s;
		case TOKEN_OP_VARIABLE_DECLARATION:
			return ":"_s;
		case TOKEN_OP_STATIC_DEF:
			return "::"_s;
		case TOKEN_OP_AND:
			return "&&"_s;
		case TOKEN_OP_OR:
			return "||"_s;
		case TOKEN_OP_NOT:
			return "!"_s;
		case TOKEN_OP_BITWISE_AND:
			return "&"_s;
		case TOKEN_OP_BITWISE_OR:
			return "|"_s;
		case TOKEN_OP_BITWISE_NOT:
			return "~"_s;
		case TOKEN_OP_MEMBER_ACCESS:
			return "."_s;
	}
	return {};
}

void Indent(PrintContext *context)
{
	for (int i = 0; i < context->indentLevels - 1; ++i)
		Log("| ");
	if (context->indentLevels)
		Log("+-");
}

void PrintExpression(PrintContext *context, ASTExpression *e)
{
	Indent(context);
	switch (e->nodeType)
	{
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		Log("Variable declaration \"%.*s\" of type \"%.*s\"\n", e->variableDeclaration.name.size,
				e->variableDeclaration.name.data, e->variableDeclaration.type->name.size,
				e->variableDeclaration.type->name.data);
		if (e->variableDeclaration.value)
		{
			++context->indentLevels;
			PrintExpression(context, e->variableDeclaration.value);
			--context->indentLevels;
		}
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		Log("Procedure declaration \"%.*s\"\n", e->procedureDeclaration.name.size,
				e->procedureDeclaration.name.data);
		++context->indentLevels;

		Indent(context);
		Log("Parameters:\n");
		for (int i = 0; i < e->procedureDeclaration.parameters.size; ++i)
		{
			ASTExpression pexp = {};
			pexp.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			pexp.variableDeclaration = e->procedureDeclaration.parameters[i];
			PrintExpression(context, &pexp);
		}

		PrintExpression(context, e->procedureDeclaration.body);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		Log("Block\n");

		++context->indentLevels;
		for (int i = 0; i < e->block.statements.size; ++i)
		{
			PrintExpression(context, &e->block.statements[i]);
		}
		--context->indentLevels;
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		Log("Variable \"%.*s\"\n", e->variable.name.size, e->variable.name.data);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		Log("Procedure call \"%.*s\"\n", e->procedureCall.name.size, e->procedureCall.name.data);
		++context->indentLevels;
		for (int i = 0; i < e->procedureCall.arguments.size; ++i)
		{
			PrintExpression(context, &e->procedureCall.arguments[i]);
		}
		--context->indentLevels;
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (e->literal.type)
		{
		case LITERALTYPE_INTEGER:
		{
			Log("Literal %d\n", e->literal.integer);
		} break;
		case LITERALTYPE_STRING:
		{
			Log("Constant \"%.*s\"\n", e->literal.string.size, e->literal.string.data);
		} break;
		}
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->unaryOperation.op);
		Log("Unary operation (%.*s)\n", operatorStr.size, operatorStr.data);
		++context->indentLevels;
		PrintExpression(context, e->unaryOperation.expression);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->binaryOperation.op);
		Log("Binary operation (%.*s)\n", operatorStr.size, operatorStr.data);
		++context->indentLevels;
		PrintExpression(context, e->binaryOperation.leftHand);
		PrintExpression(context, e->binaryOperation.rightHand);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_IF:
	{
		Log("If\n");
		++context->indentLevels;
		PrintExpression(context, e->ifNode.condition);
		PrintExpression(context, e->ifNode.body);
		if (e->ifNode.elseNode)
		{
			Indent(context);
			Log("Else:\n");
			++context->indentLevels;
			PrintExpression(context, e->ifNode.elseNode);
			--context->indentLevels;
		}
		--context->indentLevels;
	} break;
	case ASTNODETYPE_WHILE:
	{
		Log("While\n");
		++context->indentLevels;
		PrintExpression(context, e->whileNode.condition);
		PrintExpression(context, e->whileNode.body);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_RETURN:
	{
		Log("Return\n");
		++context->indentLevels;
		PrintExpression(context, e->returnNode.expression);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_BREAK:
	{
		Log("Break\n");
	} break;
	case ASTNODETYPE_STRUCT_DECLARATION:
	{
		Log("Struct\n");
		++context->indentLevels;
		for (int i = 0; i < e->structNode.members.size; ++i)
		{
			ASTExpression pexp = {};
			pexp.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			pexp.variableDeclaration = e->structNode.members[i];
			PrintExpression(context, &pexp);
		}
		--context->indentLevels;
	} break;
	default:
	{
		Log("UNKNOWN!\n");
		CRASH;
	} break;
	}
}

void PrintAST(ASTRoot *root)
{
	PrintContext context = {};

	for (int i = 0; i < root->block.statements.size; ++i)
	{
		ASTExpression *statement = &root->block.statements[i];
		PrintExpression(&context, statement);
	}
}
