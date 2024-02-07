int indentLevels;

s64 PASTPrintOut(const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);

	OutputBufferPut(size, buffer);

#if DEBUG_BUILD
	memset(t_threadMemPtr, 0x00, size + 1);
#endif

	va_end(args);
	return size;
}

s64 PASTPrintOutString(String string)
{
	OutputBufferPut(string.size, string.data);
	return string.size;
}

String ASTTypeToString(ASTType *type)
{
	if (!type)
		return "<inferred>"_s;

	switch (type->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
		return type->name;
	case ASTTYPENODETYPE_POINTER:
		return TStringConcat("^"_s, ASTTypeToString(type->pointedType));
	case ASTTYPENODETYPE_ARRAY:
	{
		String typeStr = ASTTypeToString(type->arrayType);
		ASTExpression *countExp = type->arrayCountExp;
		if (!countExp)
			return TPrintF("[] %S", typeStr);
		else {
			// @Improve
			return TPrintF("[exp] %S", typeStr);
		}
	}
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
		return "Struct"_s;
	case ASTTYPENODETYPE_UNION_DECLARATION:
		return "Union"_s;
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
		case TOKEN_OP_NOT_EQUALS:
			return "!="_s;
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
void PrintSourceLocation(SourceLocation loc)
{
	if (loc.fileIdx == 0)
		return;

	SourceFile sourceFile = g_context->sourceFiles[loc.fileIdx];

	FatSourceLocation fatLoc = ExpandSourceLocation(loc);

	String beforeToken = {};
	String token = {};
	String afterToken = {};

	int lineSize = 0;
	ASSERT(fatLoc.line > 0);
	if (fatLoc.beginingOfLine) {
		for (const char *scan = fatLoc.beginingOfLine; ; ++scan) {
			if (!*scan || *scan == '\n' || *scan == '\r')
				break;
			++lineSize;
		}

		PASTPrintOutString("   -- src ---> "_s);

		beforeToken = { fatLoc.column, fatLoc.beginingOfLine };
		token = { fatLoc.size, fatLoc.beginingOfLine + fatLoc.column };
		afterToken = { u64(Max(0, lineSize - int(fatLoc.column) - int(fatLoc.size))), token.data + fatLoc.size };

		ConsoleSetColor(CONSOLE_RESET_COLOR);
		PASTPrintOutString(beforeToken);

		ConsoleSetColor(CONSOLE_CYAN_TXT);
		PASTPrintOutString(token);

		ConsoleSetColor(CONSOLE_RESET_COLOR);
		PASTPrintOutString(afterToken);
	}
}
#else
#define PrintSourceLocation(...)
#endif

void Indent()
{
	for (int i = 0; i < indentLevels - 1; ++i)
		PASTPrintOutString("| "_s);
	if (indentLevels)
		PASTPrintOutString("+-"_s);
}

void PrintExpression(const ASTExpression *e);
void PrintASTType(const ASTType *type)
{
	Indent();
	if (!type)
	{
		PASTPrintOutString("<inferred>"_s);
		return;
	}

	switch (type->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
	{
		PASTPrintOut("\"%S\"", type->name);

		PrintSourceLocation(type->loc);
		PASTPrintOutString("\n"_s);
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		PASTPrintOutString("^"_s);

		PrintSourceLocation(type->loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintASTType(type->pointedType);
		--indentLevels;
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		PASTPrintOutString("[]"_s);

		PrintSourceLocation(type->loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		if (type->arrayCountExp) {
			Indent();
			PASTPrintOutString("Count:\n"_s);
			PrintExpression(type->arrayCountExp);
		}

		PrintASTType(type->arrayType);
		--indentLevels;
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		if (type->nodeType == ASTTYPENODETYPE_UNION_DECLARATION)
			PASTPrintOutString("Union"_s);
		else
			PASTPrintOutString("Struct"_s);

		PrintSourceLocation(type->loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		for (int i = 0; i < type->structDeclaration.members.size; ++i) {
			const ASTStructMemberDeclaration *member = &type->structDeclaration.members[i];

			Indent();
			PASTPrintOut("Member \"%S\" of type:\n", member->name);

			++indentLevels;
			PrintASTType(member->astType);
			--indentLevels;

			if (member->value) {
				++indentLevels;
				PrintExpression(member->value);
				--indentLevels;
			}
		}
		--indentLevels;
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		PASTPrintOutString("Enum"_s);

		PrintSourceLocation(type->loc);
		PASTPrintOutString("\n"_s);

		if (type->enumDeclaration.astType)
		{
			Indent();
			PASTPrintOutString("Of type:\n"_s);
			++indentLevels;
			PrintASTType(type->enumDeclaration.astType);
			--indentLevels;
		}

		++indentLevels;
		for (int i = 0; i < type->enumDeclaration.members.size; ++i)
		{
			const ASTEnumMember *member = &type->enumDeclaration.members[i];

			Indent();
			PASTPrintOut("Enum member \"%S\"\n", member->name);

			if (member->value)
			{
				++indentLevels;
				PrintExpression(member->value);
				--indentLevels;
			}
		}
		--indentLevels;
	} break;
	default:
		PASTPrintOutString("???TYPE"_s);

		PrintSourceLocation(type->loc);
		PASTPrintOutString("\n"_s);
	}
}

void PrintExpression(const ASTExpression *e)
{
	Indent();
	switch (e->nodeType)
	{
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		if (e->variableDeclaration.isStatic)
			PASTPrintOutString("Static variable declaration "_s);
		else
			PASTPrintOutString("Variable declaration "_s);
		String typeStr = ASTTypeToString(e->variableDeclaration.astType);
		PASTPrintOut("of type \"%S\"", typeStr);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		if (e->variableDeclaration.astInitialValue)
		{
			++indentLevels;
			PrintExpression(e->variableDeclaration.astInitialValue);
			--indentLevels;
		}
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		PASTPrintOutString("Procedure declaration"_s);
		++indentLevels;

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		Indent();
		PASTPrintOutString("Parameters:\n"_s);
		++indentLevels;
		const ASTProcedurePrototype *prototype = &e->procedureDeclaration.prototype;
		for (int i = 0; i < prototype->astParameters.size; ++i)
		{
			ASTProcedureParameter astParam = prototype->astParameters[i];
			Indent();
			PASTPrintOut("Parameter #%d ", i);
			String typeStr = ASTTypeToString(astParam.astType);
			PASTPrintOut("\"%S\" of type \"%S\"", astParam.name, typeStr);

			PrintSourceLocation(astParam.loc);
			PASTPrintOutString("\n"_s);

			if (astParam.astInitialValue)
			{
				++indentLevels;
				PrintExpression(astParam.astInitialValue);
				--indentLevels;
			}
		}
		--indentLevels;

		if (e->procedureDeclaration.astBody)
			PrintExpression(e->procedureDeclaration.astBody);

		--indentLevels;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		PASTPrintOutString("Block\n"_s);

		++indentLevels;
		for (int i = 0; i < e->block.statements.size; ++i)
		{
			PrintExpression(&e->block.statements[i]);
		}
		--indentLevels;
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		PASTPrintOut("Identifier \"%S\"", e->identifier.string);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		PASTPrintOutString("Procedure call"_s);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->procedureCall.procedureExpression);
		for (int i = 0; i < e->procedureCall.arguments.size; ++i)
		{
			PrintExpression(e->procedureCall.arguments[i]);
		}
		--indentLevels;
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (e->literal.type)
		{
		case LITERALTYPE_INTEGER:
		{
			PASTPrintOut("Literal %d", e->literal.integer);
		} break;
		case LITERALTYPE_FLOATING:
		{
			PASTPrintOut("Literal %f", e->literal.integer);
		} break;
		case LITERALTYPE_STRING:
		{
			PASTPrintOut("Constant \"%S\"", e->literal.string);
		} break;
		}

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->unaryOperation.op);
		PASTPrintOut("Unary operation (%S)", operatorStr);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->unaryOperation.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		String operatorStr = OperatorToString(e->binaryOperation.op);
		PASTPrintOut("Binary operation (%S)", operatorStr);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->binaryOperation.leftHand);
		PrintExpression(e->binaryOperation.rightHand);
		--indentLevels;
	} break;
	case ASTNODETYPE_IF:
	{
		PASTPrintOutString("If"_s);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->ifNode.condition);
		PrintExpression(e->ifNode.body);
		if (e->ifNode.elseBody)
		{
			Indent();
			PASTPrintOutString("Else:"_s);

			PrintSourceLocation(e->ifNode.elseLoc);
			PASTPrintOutString("\n"_s);

			++indentLevels;
			PrintExpression(e->ifNode.elseBody);
			--indentLevels;
		}
		--indentLevels;
	} break;
	case ASTNODETYPE_IF_STATIC:
	{
		PASTPrintOutString("If (static)"_s);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->ifStaticNode.condition);
		PrintExpression(e->ifStaticNode.body);
		if (e->ifStaticNode.elseBody)
		{
			Indent();
			PASTPrintOutString("Else:"_s);

			PrintSourceLocation(e->ifStaticNode.elseLoc);
			PASTPrintOutString("\n"_s);

			++indentLevels;
			PrintExpression(e->ifStaticNode.elseBody);
			--indentLevels;
		}
		--indentLevels;
	} break;
	case ASTNODETYPE_WHILE:
	{
		PASTPrintOutString("While"_s);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->whileNode.condition);
		PrintExpression(e->whileNode.body);
		--indentLevels;
	} break;
	case ASTNODETYPE_FOR:
	{
		PASTPrintOutString("For"_s);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->forNode.range);
		PrintExpression(e->forNode.body);
		--indentLevels;
	} break;
	case ASTNODETYPE_RETURN:
	{
		PASTPrintOutString("Return"_s);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		if (e->returnNode.expression) {
			++indentLevels;
			PrintExpression(e->returnNode.expression);
			--indentLevels;
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		PASTPrintOutString("Defer"_s);

		PrintSourceLocation(e->any.loc);
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->deferNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_BREAK:
	{
		PASTPrintOutString("Break\n"_s);
	} break;
	case ASTNODETYPE_CONTINUE:
	{
		PASTPrintOutString("Continue\n"_s);
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		PASTPrintOutString("Static definition "_s);
		if (e->staticDefinition.nameCount == 1)
			PASTPrintOut("\"%S\"", e->staticDefinition.name);
		else for (u32 i = 0; i < e->staticDefinition.nameCount == 1; ++i) {
			if (i) PASTPrintOutString(", "_s);
			PASTPrintOut("\"%S\"", e->staticDefinition.arrayOfNames[i]);
		}
		PASTPrintOutString("\n"_s);

		++indentLevels;
		PrintExpression(e->staticDefinition.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_TYPE:
	{
		PASTPrintOutString("Type\n"_s);
		++indentLevels;
		PrintASTType(&e->astType);
		--indentLevels;
	} break;
	case ASTNODETYPE_CAST:
	{
		PASTPrintOutString("Cast\n"_s);
		++indentLevels;
		PrintASTType(&e->castNode.astType);
		PrintExpression(e->castNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		PASTPrintOutString("Type of\n"_s);
		++indentLevels;
		PrintExpression(e->typeOfNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		PASTPrintOutString("Size of\n"_s);
		++indentLevels;
		PrintExpression(e->typeOfNode.expression);
		--indentLevels;
	} break;
	case ASTNODETYPE_GARBAGE:
	{
		PASTPrintOutString("Garbage\n"_s);
	} break;
	case ASTNODETYPE_INCLUDE:
	{
		PASTPrintOut("Include \"%S\"\n", e->include.filename);
	} break;
	case ASTNODETYPE_LINKLIB:
	{
		PASTPrintOut("Link library \"%S\"\n", e->linklib.filename);
	} break;
	case ASTNODETYPE_DEFINED:
	{
		PASTPrintOut("Defined? \"%S\"\n", e->definedNode.identifier);
	} break;
	case ASTNODETYPE_INTRINSIC:
	{
		PASTPrintOut("Intrinsic \"%S\"\n", e->intrinsic.name);

		++indentLevels;
		for (int i = 0; i < e->intrinsic.arguments.size; ++i)
		{
			PrintExpression(&e->intrinsic.arguments[i]);
		}
		--indentLevels;
	} break;
	default:
	{
		PASTPrintOutString("UNKNOWN!\n"_s);
		//CRASH;
	} break;
	}
}

void PrintAST(PContext *context, String title)
{
	static Mutex printASTMutex = SYSCreateMutex();

	indentLevels = 0;

	SYSMutexLock(printASTMutex);

	OutputBufferReset();

	ArrayView<ASTExpression> statements = context->astRoot.block.statements;
	for (int i = 0; i < statements.size; ++i) {
		const ASTExpression *statement = &statements[i];
		PrintExpression(statement);
	}

	OutputBufferWriteToFile(TPrintF("output/ast_%S.txt", title));

	SYSMutexUnlock(printASTMutex);
}
