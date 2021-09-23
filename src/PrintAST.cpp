#if PRINT_AST_TREE
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
		case TOKEN_OP_ASSIGNMENT_PLUS:
			return "+="_s;
		case TOKEN_OP_ASSIGNMENT_MINUS:
			return "-="_s;
		case TOKEN_OP_ASSIGNMENT_MULTIPLY:
			return "*="_s;
		case TOKEN_OP_ASSIGNMENT_DIVIDE:
			return "/="_s;
		case TOKEN_OP_ASSIGNMENT_MODULO:
			return "%="_s;
		case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
			return "<<="_s;
		case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
			return ">>="_s;
		case TOKEN_OP_ASSIGNMENT_OR:
			return "||="_s;
		case TOKEN_OP_ASSIGNMENT_AND:
			return "&&="_s;
		case TOKEN_OP_ASSIGNMENT_BITWISE_OR:
			return "|="_s;
		case TOKEN_OP_ASSIGNMENT_BITWISE_XOR:
			return "^="_s;
		case TOKEN_OP_ASSIGNMENT_BITWISE_AND:
			return "&="_s;
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
		case TOKEN_OP_MODULO:
			return "%"_s;
		case TOKEN_OP_SHIFT_LEFT:
			return "<<"_s;
		case TOKEN_OP_SHIFT_RIGHT:
			return ">>"_s;
		case TOKEN_OP_ARROW:
			return "->"_s;
		case TOKEN_OP_VARIABLE_DECLARATION:
			return ":"_s;
		case TOKEN_OP_STATIC_DEF:
			return "::"_s;
		case TOKEN_OP_RANGE:
			return ".."_s;
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
		case TOKEN_OP_ARRAY_ACCESS:
			return "[]"_s;
		case TOKEN_OP_POINTER_TO:
			return "^"_s;
		case TOKEN_OP_DEREFERENCE:
			return "@"_s;
	}
	return "???OP"_s;
}

#if PRINT_TOKEN_SOURCE_LOCATION
void PrintSourceLocation(PrintContext *context, SourceLocation loc)
{
	const char *colorCode = "\u001b[36m";
	const char *clearCode = "\u001b[0m";

	const char *beginningOfLine = nullptr;
	int size = 0;
	int l = 1;
	for (const char *scan = (const char *)loc.fileBuffer; *scan; ++scan)
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

		Print("   -- src ---> ");

		Print("%.*s%s%.*s%s%.*s", shiftedChar, beginningOfLine, colorCode,
				loc.size, beginningOfLine + shiftedChar, clearCode,
				size - shiftedChar - loc.size, beginningOfLine + shiftedChar + loc.size);
	}
}
#else
#define PrintSourceLocation(...)
#endif

void Indent(PrintContext *context)
{
	for (int i = 0; i < context->indentLevels - 1; ++i)
		Print("| ");
	if (context->indentLevels)
		Print("+-");
}

void PrintExpression(PrintContext *context, ASTExpression *e);
void PrintASTType(PrintContext *context, ASTType *type)
{
	Indent(context);
	if (!type)
	{
		Print("<inferred>");
		return;
	}

	switch (type->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
	{
		Print("\"%S\"", type->name);

		PrintSourceLocation(context, type->loc);
		Print("\n");
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		Print("^");

		PrintSourceLocation(context, type->loc);
		Print("\n");

		++context->indentLevels;
		PrintASTType(context, type->pointedType);
		--context->indentLevels;
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		Print("[%d]", type->arrayCount);

		PrintSourceLocation(context, type->loc);
		Print("\n");

		++context->indentLevels;
		PrintASTType(context, type->arrayType);
		--context->indentLevels;
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	{
		Print("Struct");

		PrintSourceLocation(context, type->loc);
		Print("\n");

		++context->indentLevels;
		for (int i = 0; i < type->structDeclaration.members.size; ++i)
		{
			ASTStructMemberDeclaration *member = &type->structDeclaration.members[i];

			Indent(context);
			Print("Struct member \"%S\" of type:\n", member->name);

			++context->indentLevels;
			PrintASTType(context, member->astType);
			--context->indentLevels;

			if (member->value)
			{
				++context->indentLevels;
				PrintExpression(context, member->value);
				--context->indentLevels;
			}
		}
		--context->indentLevels;
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		Print("Enum");

		PrintSourceLocation(context, type->loc);
		Print("\n");

		if (type->enumDeclaration.astType)
		{
			Indent(context);
			Print("Of type:\n");
			++context->indentLevels;
			PrintASTType(context, type->enumDeclaration.astType);
			--context->indentLevels;
		}

		++context->indentLevels;
		for (int i = 0; i < type->enumDeclaration.members.size; ++i)
		{
			ASTEnumMember *member = &type->enumDeclaration.members[i];

			Indent(context);
			Print("Enum member \"%S\"\n", member->name);

			if (member->value)
			{
				++context->indentLevels;
				PrintExpression(context, member->value);
				--context->indentLevels;
			}
		}
		--context->indentLevels;
	} break;
	default:
		Print("???TYPE");

		PrintSourceLocation(context, type->loc);
		Print("\n");
	}
}

void PrintExpression(PrintContext *context, ASTExpression *e)
{
	Indent(context);
	switch (e->nodeType)
	{
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		Variable *var = e->variableDeclaration.variable;
		if (var->isStatic)
			Print("Static variable declaration ");
		else
			Print("Variable declaration ");
		String typeStr = ASTTypeToString(e->variableDeclaration.astType);
		Print("\"%S\" of type \"%S\"", var->name, typeStr);

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		if (e->variableDeclaration.value)
		{
			++context->indentLevels;
			PrintExpression(context, e->variableDeclaration.value);
			--context->indentLevels;
		}
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		Print("Procedure declaration");
		++context->indentLevels;

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		Indent(context);
		Print("Parameters:\n");
		for (int i = 0; i < e->procedureDeclaration.astParameters.size; ++i)
		{
			ASTExpression pexp = {};
			pexp.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			pexp.variableDeclaration = e->procedureDeclaration.astParameters[i];
			PrintExpression(context, &pexp);
		}

		if (e->procedureDeclaration.procedure->astBody)
			PrintExpression(context, e->procedureDeclaration.procedure->astBody);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		Print("Block\n");

		++context->indentLevels;
		for (int i = 0; i < e->block.statements.size; ++i)
		{
			PrintExpression(context, &e->block.statements[i]);
		}
		--context->indentLevels;
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		Print("Identifier \"%S\"", e->identifier.string);

		PrintSourceLocation(context, e->any.loc);
		Print("\n");
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		Print("Procedure call \"%S\"", e->procedureCall.name);

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

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
			Print("Literal %d", e->literal.integer);
		} break;
		case LITERALTYPE_FLOATING:
		{
			Print("Literal %f", e->literal.integer);
		} break;
		case LITERALTYPE_STRING:
		{
			Print("Constant \"%S\"", e->literal.string);
		} break;
		}

		PrintSourceLocation(context, e->any.loc);
		Print("\n");
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->unaryOperation.op);
		Print("Unary operation (%S)", operatorStr);

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++context->indentLevels;
		PrintExpression(context, e->unaryOperation.expression);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->binaryOperation.op);
		Print("Binary operation (%S)", operatorStr);

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++context->indentLevels;
		PrintExpression(context, e->binaryOperation.leftHand);
		PrintExpression(context, e->binaryOperation.rightHand);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_IF:
	{
		Print("If");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++context->indentLevels;
		PrintExpression(context, e->ifNode.condition);
		PrintExpression(context, e->ifNode.body);
		if (e->ifNode.elseBody)
		{
			Indent(context);
			Print("Else:");

			PrintSourceLocation(context, e->ifNode.elseLoc);
			Print("\n");

			++context->indentLevels;
			PrintExpression(context, e->ifNode.elseBody);
			--context->indentLevels;
		}
		--context->indentLevels;
	} break;
	case ASTNODETYPE_WHILE:
	{
		Print("While");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++context->indentLevels;
		PrintExpression(context, e->whileNode.condition);
		PrintExpression(context, e->whileNode.body);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_FOR:
	{
		Print("For");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++context->indentLevels;
		PrintExpression(context, e->forNode.range);
		PrintExpression(context, e->forNode.body);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_RETURN:
	{
		Print("Return");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++context->indentLevels;
		PrintExpression(context, e->returnNode.expression);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_DEFER:
	{
		Print("Defer");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++context->indentLevels;
		PrintExpression(context, e->deferNode.expression);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_BREAK:
	{
		Print("Break\n");
	} break;
	case ASTNODETYPE_STRUCT_DECLARATION:
	{
		Print("Struct\n");
		++context->indentLevels;
		for (int i = 0; i < e->structDeclaration.members.size; ++i)
		{
			ASTStructMemberDeclaration *member = &e->structDeclaration.members[i];

			Print("Struct member ");
			String typeStr = ASTTypeToString(member->astType);
			Print("\"%S\" of type \"%S\"", member->name, typeStr);

			PrintSourceLocation(context, e->any.loc);
			Print("\n");

			if (member->value)
			{
				++context->indentLevels;
				PrintExpression(context, member->value);
				--context->indentLevels;
			}
		}
		--context->indentLevels;
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		Print("Static definition \"%S\"\n", e->staticDefinition.name);

		++context->indentLevels;
		PrintExpression(context, e->staticDefinition.expression);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_TYPE:
	{
		Print("Type\n");
		++context->indentLevels;
		PrintASTType(context, &e->astType);
		--context->indentLevels;
	} break;
	case ASTNODETYPE_CAST:
	{
		Print("Cast\n");
		++context->indentLevels;
		PrintASTType(context, &e->castNode.astType);
		PrintExpression(context, e->castNode.expression);
		--context->indentLevels;
	} break;
	default:
	{
		Print("UNKNOWN!\n");
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
#endif
