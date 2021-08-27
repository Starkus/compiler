#define PRINT_TOKEN_SOURCE_LOCATION 0

struct PrintContext
{
	int indentLevels;

	String filename;
	u8 *fileBuffer;
	u64 fileSize;
};

String OperatorToString(s32 op)
{
	switch (op)
	{
		case TOKEN_OP_ASSIGNMENT:
			return "="_s;
		case TOKEN_OP_EQUALS:
			return "=="_s;
		case TOKEN_OP_GREATER_THAN:
			return ">"_s;
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
			return ">="_s;
		case TOKEN_OP_LESS_THAN:
			return "<"_s;
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
			return "<="_s;
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
	return "???OP"_s;
}

void Indent(PrintContext *context)
{
	for (int i = 0; i < context->indentLevels - 1; ++i)
		Log("| ");
	if (context->indentLevels)
		Log("+-");
}

#if PRINT_TOKEN_SOURCE_LOCATION
void PrintSourceLocation(PrintContext *context, SourceLocation loc)
{
	const char *colorCode = "\u001b[36m";
	const char *clearCode = "\u001b[0m";

	const char *beginningOfLine = nullptr;
	int size = 0;
	int l = 1;
	for (const char *scan = (const char *)context->fileBuffer; *scan; ++scan)
	{
		if (l == loc.line)
		{
			beginningOfLine = scan;
			break;
		}
		if (*scan == '\n')
			++l;
	}
	s64 shiftedChar = loc.character;
	for (const char *scan = beginningOfLine; *scan; ++scan)
	{
		if (!IsWhitespace(*scan))
		{
			beginningOfLine = scan;
			break;
		}
		--shiftedChar;
	}
	if (beginningOfLine)
	{
		for (const char *scan = beginningOfLine; ; ++scan)
		{
			if (!*scan || *scan == '\n')
				break;
			++size;
		}

		Log("   -- src ---> ");

		Log("%.*s%s%.*s%s%.*s", shiftedChar, beginningOfLine, colorCode,
				loc.size, beginningOfLine + shiftedChar, clearCode,
				size - shiftedChar - loc.size, beginningOfLine + shiftedChar + loc.size);
	}
}
#else
#define PrintSourceLocation(...)
#endif

void PrintExpression(PrintContext *context, ASTExpression *e)
{
	Indent(context);
	switch (e->nodeType)
	{
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		if (e->variableDeclaration.isStatic)
			Log("Static variable declaration ");
		else
			Log("Variable declaration ");
		Log("\"%.*s\" of type \"%.*s\"", e->variableDeclaration.name.size,
				e->variableDeclaration.name.data, e->variableDeclaration.typeName.size,
				e->variableDeclaration.typeName.data);

		PrintSourceLocation(context, e->any.loc);
		Log("\n");

		if (e->variableDeclaration.value)
		{
			++context->indentLevels;
			PrintExpression(context, e->variableDeclaration.value);
			--context->indentLevels;
		}
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		Log("Procedure declaration \"%.*s\"", e->procedureDeclaration.name.size,
				e->procedureDeclaration.name.data);
		++context->indentLevels;

		PrintSourceLocation(context, e->any.loc);
		Log("\n");

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
		Log("Variable \"%.*s\"", e->variable.name.size, e->variable.name.data);

		PrintSourceLocation(context, e->any.loc);
		Log("\n");
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		Log("Procedure call \"%.*s\"", e->procedureCall.name.size, e->procedureCall.name.data);

		PrintSourceLocation(context, e->any.loc);
		Log("\n");

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
			Log("Literal %d", e->literal.integer);
		} break;
		case LITERALTYPE_FLOATING:
		{
			Log("Literal %f", e->literal.integer);
		} break;
		case LITERALTYPE_STRING:
		{
			Log("Constant \"%.*s\"", e->literal.string.size, e->literal.string.data);
		} break;
		}

		PrintSourceLocation(context, e->any.loc);
		Log("\n");
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->unaryOperation.op);
		Log("Unary operation (%.*s)", operatorStr.size, operatorStr.data);

		PrintSourceLocation(context, e->any.loc);
		Log("\n");

		++context->indentLevels;
		PrintExpression(context, e->unaryOperation.expression);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->binaryOperation.op);
		Log("Binary operation (%.*s)", operatorStr.size, operatorStr.data);

		PrintSourceLocation(context, e->any.loc);
		Log("\n");

		++context->indentLevels;
		PrintExpression(context, e->binaryOperation.leftHand);
		PrintExpression(context, e->binaryOperation.rightHand);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_IF:
	{
		Log("If");

		PrintSourceLocation(context, e->any.loc);
		Log("\n");

		++context->indentLevels;
		PrintExpression(context, e->ifNode.condition);
		PrintExpression(context, e->ifNode.body);
		if (e->ifNode.elseNode)
		{
			Indent(context);
			Log("Else:");

			PrintSourceLocation(context, e->ifNode.elseLoc);
			Log("\n");

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

void PrintAST(Context *context)
{
	PrintContext printContext = {};
	printContext.filename = context->filename;
	printContext.fileBuffer = context->fileBuffer;
	printContext.fileSize = context->fileSize;

	for (int i = 0; i < context->astRoot->block.statements.size; ++i)
	{
		ASTExpression *statement = &context->astRoot->block.statements[i];
		PrintExpression(&printContext, statement);
	}
}
