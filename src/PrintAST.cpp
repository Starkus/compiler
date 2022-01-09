int indentLevels;

String ASTTypeToString(ASTType *type)
{
	if (!type)
		return "<inferred>"_s;

	switch (type->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
		return type->name;
	case ASTTYPENODETYPE_POINTER:
		return StringConcat("^"_s, ASTTypeToString(type->pointedType));
	case ASTTYPENODETYPE_ARRAY:
	{
		String typeStr = ASTTypeToString(type->arrayType);
		return TPrintF("[%d] %S", type->arrayCount, typeStr);
	}
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
		return "Struct"_s;
	}
	return "???TYPE"_s;
}

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
void PrintSourceLocation(Context *context, SourceLocation loc)
{
	const char *colorCode = "\u001b[36m";
	const char *clearCode = "\u001b[0m";

	SourceFile sourceFile = context->sourceFiles[loc.fileIdx];

	String beforeToken = {};
	String token = {};
	String afterToken = {};

	const char *beginningOfLine = nullptr;
	int size = 0;
	int l = 1;
	ASSERT(loc.line > 0);
	for (const char *scan = (const char *)sourceFile.buffer; *scan; ++scan)
	{
		if (l == loc.line)
		{
			beginningOfLine = scan;
			break;
		}
		if (*scan == '\n' || *scan == '\r')
			++l;
	}
	s64 relativeChar = loc.character;
	for (const char *scan = beginningOfLine; *scan; ++scan)
	{
		if (!IsWhitespace(*scan))
		{
			beginningOfLine = scan;
			break;
		}
		--relativeChar;
	}
	if (beginningOfLine)
	{
		for (const char *scan = beginningOfLine; ; ++scan)
		{
			if (!*scan || *scan == '\n' || *scan == '\r')
				break;
			++size;
		}

		Print("   -- src ---> ");

		beforeToken = { loc.character, beginningOfLine };
		token = { loc.size, beginningOfLine + loc.character };
		afterToken = { Max(0, size - loc.character - loc.size), token.data + loc.size };
		Print("%S%s%S%s%S", beforeToken, colorCode,
				token, clearCode,
				afterToken);
	}
}
#else
#define PrintSourceLocation(...)
#endif

void Indent()
{
	for (int i = 0; i < indentLevels - 1; ++i)
		Print("| ");
	if (indentLevels)
		Print("+-");
}

void PrintExpression(Context *context, ASTExpression *e);
void PrintASTType(Context *context, ASTType *type)
{
	Indent();
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

		++indentLevels;
		PrintASTType(context, type->pointedType);
		--indentLevels;
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		Print("[%d]", type->arrayCount);

		PrintSourceLocation(context, type->loc);
		Print("\n");

		++indentLevels;
		PrintASTType(context, type->arrayType);
		--indentLevels;
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	{
		Print("Struct");

		PrintSourceLocation(context, type->loc);
		Print("\n");

		++indentLevels;
		for (int i = 0; i < type->structDeclaration.members.size; ++i)
		{
			ASTStructMemberDeclaration *member = &type->structDeclaration.members[i];

			Indent();
			Print("Struct member \"%S\" of type:\n", member->name);

			++indentLevels;
			PrintASTType(context, member->astType);
			--indentLevels;

			if (member->value)
			{
				++indentLevels;
				PrintExpression(context, member->value);
				--indentLevels;
			}
		}
		--indentLevels;
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		Print("Enum");

		PrintSourceLocation(context, type->loc);
		Print("\n");

		if (type->enumDeclaration.astType)
		{
			Indent();
			Print("Of type:\n");
			++indentLevels;
			PrintASTType(context, type->enumDeclaration.astType);
			--indentLevels;
		}

		++indentLevels;
		for (int i = 0; i < type->enumDeclaration.members.size; ++i)
		{
			ASTEnumMember *member = &type->enumDeclaration.members[i];

			Indent();
			Print("Enum member \"%S\"\n", member->name);

			if (member->value)
			{
				++indentLevels;
				PrintExpression(context, member->value);
				--indentLevels;
			}
		}
		--indentLevels;
	} break;
	default:
		Print("???TYPE");

		PrintSourceLocation(context, type->loc);
		Print("\n");
	}
}

void PrintExpression(Context *context, ASTExpression *e)
{
	Indent();
	switch (e->nodeType)
	{
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		if (e->variableDeclaration.isStatic)
			Print("Static variable declaration ");
		else
			Print("Variable declaration ");
		String typeStr = ASTTypeToString(e->variableDeclaration.astType);
		Print("\"%S\" of type \"%S\"", e->variableDeclaration.name, typeStr);

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		if (e->variableDeclaration.astInitialValue)
		{
			++indentLevels;
			PrintExpression(context, e->variableDeclaration.astInitialValue);
			--indentLevels;
		}
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		Print("Procedure declaration");
		++indentLevels;

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		Indent();
		Print("Parameters:\n");
		for (int i = 0; i < e->procedureDeclaration.prototype.astParameters.size; ++i)
		{
			ASTProcedureParameter astParam = e->procedureDeclaration.prototype.astParameters[i];
			Print("Parameter #%d ", i);
			String typeStr = ASTTypeToString(astParam.astType);
			Print("\"%S\" of type \"%S\"", astParam.name, typeStr);

			PrintSourceLocation(context, astParam.loc);
			Print("\n");

			if (astParam.astInitialValue)
			{
				++indentLevels;
				PrintExpression(context, astParam.astInitialValue);
				--indentLevels;
			}
		}

		Procedure *procedure = GetProcedure(context, e->procedureDeclaration.procedureIdx);
		if (procedure->astBody)
			PrintExpression(context, procedure->astBody);
		--indentLevels;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		Print("Block\n");

		++indentLevels;
		for (int i = 0; i < e->block.statements.size; ++i)
		{
			PrintExpression(context, &e->block.statements[i]);
		}
		--indentLevels;
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

		++indentLevels;
		for (int i = 0; i < e->procedureCall.arguments.size; ++i)
		{
			PrintExpression(context, &e->procedureCall.arguments[i]);
		}
		--indentLevels;
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

		++indentLevels;
		PrintExpression(context, e->unaryOperation.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->binaryOperation.op);
		Print("Binary operation (%S)", operatorStr);

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++indentLevels;
		PrintExpression(context, e->binaryOperation.leftHand);
		PrintExpression(context, e->binaryOperation.rightHand);
		--indentLevels;
	} break;
	case ASTNODETYPE_IF:
	{
		Print("If");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++indentLevels;
		PrintExpression(context, e->ifNode.condition);
		PrintExpression(context, e->ifNode.body);
		if (e->ifNode.elseBody)
		{
			Indent();
			Print("Else:");

			PrintSourceLocation(context, e->ifNode.elseLoc);
			Print("\n");

			++indentLevels;
			PrintExpression(context, e->ifNode.elseBody);
			--indentLevels;
		}
		--indentLevels;
	} break;
	case ASTNODETYPE_WHILE:
	{
		Print("While");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++indentLevels;
		PrintExpression(context, e->whileNode.condition);
		PrintExpression(context, e->whileNode.body);
		--indentLevels;
	} break;
	case ASTNODETYPE_FOR:
	{
		Print("For");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++indentLevels;
		PrintExpression(context, e->forNode.range);
		PrintExpression(context, e->forNode.body);
		--indentLevels;
	} break;
	case ASTNODETYPE_RETURN:
	{
		Print("Return");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++indentLevels;
		PrintExpression(context, e->returnNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_DEFER:
	{
		Print("Defer");

		PrintSourceLocation(context, e->any.loc);
		Print("\n");

		++indentLevels;
		PrintExpression(context, e->deferNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_BREAK:
	{
		Print("Break\n");
	} break;
	case ASTNODETYPE_STRUCT_DECLARATION:
	{
		Print("Struct\n");
		++indentLevels;
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
				++indentLevels;
				PrintExpression(context, member->value);
				--indentLevels;
			}
		}
		--indentLevels;
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		Print("Static definition \"%S\"\n", e->staticDefinition.name);

		++indentLevels;
		PrintExpression(context, e->staticDefinition.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_TYPE:
	{
		Print("Type\n");
		++indentLevels;
		PrintASTType(context, &e->astType);
		--indentLevels;
	} break;
	case ASTNODETYPE_CAST:
	{
		Print("Cast\n");
		++indentLevels;
		PrintASTType(context, &e->castNode.astType);
		PrintExpression(context, e->castNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		Print("Type of\n");
		++indentLevels;
		PrintExpression(context, e->typeOfNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		Print("Size of\n");
		++indentLevels;
		PrintExpression(context, e->typeOfNode.expression);
		--indentLevels;
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
	indentLevels = 0;

	for (int i = 0; i < context->astRoot->block.statements.size; ++i)
	{
		ASTExpression *statement = &context->astRoot->block.statements[i];
		PrintExpression(context, statement);
	}
}
