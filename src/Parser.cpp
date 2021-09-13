ASTExpression ParseExpression(Context *context, s32 precedence);
ASTExpression ParseStatement(Context *context);
ASTVariableDeclaration ParseVariableDeclaration(Context *context);

struct Variable
{
	String name;
	s8 parameterIndex; // Negative if not a parameter
	bool isStatic;

	s64 typeTableIdx;

	// Back end
	u64 stackOffset;
};

struct ProcedureParameter
{
	Variable *variable;
	ASTExpression *defaultValue;
};
struct IRInstruction;
struct Procedure
{
	bool isVarargs;
	bool isExternal;
	DynamicArray<ProcedureParameter, malloc, realloc> parameters;
	ASTExpression *astBody;
	s32 requiredParameterCount;
	s64 returnTypeTableIdx;

	// IRGen
	BucketArray<IRInstruction, 256, malloc, realloc> instructions;
	u64 registerCount;
};

void Advance(Context *context)
{
	ASSERT(context->token == &context->tokens[context->currentTokenIdx]);

	++context->currentTokenIdx;
	ASSERT(context->currentTokenIdx < BucketArrayCount(&context->tokens));
	context->token = &context->tokens[context->currentTokenIdx];
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

ASTExpression *NewTreeNode(Context *context)
{
	return BucketArrayAdd(&context->treeNodes);
}

ASTType *NewASTType(Context *context)
{
	return BucketArrayAdd(&context->astTypeNodes);
}

ASTStructDeclaration ParseStructOrUnion(Context *context);
ASTEnumDeclaration ParseEnumDeclaration(Context *context);
ASTType ParseType(Context *context)
{
	ASTType astType;
	astType.loc = context->token->loc;

	if (context->token->type == TOKEN_OP_ARRAY_ACCESS)
	{
		Advance(context);
		astType.nodeType = ASTTYPENODETYPE_ARRAY;

		astType.arrayCount = 0;
		if (context->token->type == TOKEN_LITERAL_NUMBER)
		{
			astType.arrayCount = IntFromString(context->token->string);
			Advance(context);
		}
		AssertToken(context, context->token, ']');
		Advance(context);

		astType.arrayType = NewASTType(context);
		*astType.arrayType = ParseType(context);
	}
	else if (context->token->type == TOKEN_OP_POINTER_TO)
	{
		Advance(context);
		astType.nodeType = ASTTYPENODETYPE_POINTER;
		astType.pointedType = NewASTType(context);
		*astType.pointedType = ParseType(context);
	}
	else if (context->token->type == TOKEN_KEYWORD_STRUCT ||
			 context->token->type == TOKEN_KEYWORD_UNION)
	{
		astType.nodeType = ASTTYPENODETYPE_STRUCT_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(context);
	}
	else if (context->token->type == TOKEN_KEYWORD_ENUM)
	{
		astType.nodeType = ASTTYPENODETYPE_ENUM_DECLARATION;
		astType.enumDeclaration = ParseEnumDeclaration(context);
	}
	else if (context->token->type == TOKEN_IDENTIFIER)
	{
		astType.nodeType = ASTTYPENODETYPE_IDENTIFIER;
		astType.name = context->token->string;
		Advance(context);
	}
	else
	{
		astType.nodeType = ASTTYPENODETYPE_INVALID;
	}

	return astType;
}

bool TryParseUnaryOperation(Context *context, s32 prevPrecedence, ASTUnaryOperation *result)
{
	if (!IsTokenOperator(context->token))
		return false;

	Token *oldToken = context->token;
	s64 oldTokenIdx = context->currentTokenIdx;

	result->loc = context->token->loc;

	switch (context->token->type)
	{
	case TOKEN_OP_POINTER_TO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	case TOKEN_OP_MINUS:
	{
		enum TokenType op = context->token->type;
		result->op = op;
		Advance(context);

		s32 precedence = GetOperatorPrecedence(op);
		//if (precedence > prevPrecedence) // @Check: ??
		{
			result->expression = NewTreeNode(context);
			*result->expression = ParseExpression(context, precedence);

			return true;
		}
	} break;
	}

	context->token = oldToken;
	context->currentTokenIdx = oldTokenIdx;
	return false;
}

bool TryParseBinaryOperation(Context *context, ASTExpression leftHand, s32 prevPrecedence,
		ASTBinaryOperation *result)
{
	if (!IsTokenOperator(context->token))
		return false;

	Token *oldToken = context->token;
	s64 oldTokenIdx = context->currentTokenIdx;

	result->loc = context->token->loc;

	switch (context->token->type)
	{
	case TOKEN_OP_PLUS_EQUALS:
	case TOKEN_OP_MINUS_EQUALS:
	case TOKEN_OP_MULTIPLY_EQUALS:
	case TOKEN_OP_DIVIDE_EQUALS:
	{
		// Replace directly for x = x + y
		enum TokenType op;
		switch (context->token->type)
		{
		case TOKEN_OP_PLUS_EQUALS:
			op = TOKEN_OP_PLUS; break;
		case TOKEN_OP_MINUS_EQUALS:
			op = TOKEN_OP_MINUS; break;
		case TOKEN_OP_MULTIPLY_EQUALS:
			op = TOKEN_OP_MULTIPLY; break;
		case TOKEN_OP_DIVIDE_EQUALS:
			op = TOKEN_OP_DIVIDE; break;
		case TOKEN_OP_MODULO_EQUALS:
			op = TOKEN_OP_MODULO; break;
		default:
			op = TOKEN_END_OF_FILE;
			CRASH;
		}
		Advance(context);

		result->op = TOKEN_OP_ASSIGNMENT;
		s32 precedence = GetOperatorPrecedence(TOKEN_OP_ASSIGNMENT);
		if (precedence > prevPrecedence)
		{
			result->leftHand = NewTreeNode(context);
			*result->leftHand = leftHand;

			result->rightHand = NewTreeNode(context);
			result->rightHand->any.loc = result->loc;
			result->rightHand->nodeType = ASTNODETYPE_BINARY_OPERATION;

			// NOTE! Tree branch referenced twice here! The tree is now a graph D:
			// Consider copying if there's a problem with this.
			result->rightHand->binaryOperation.leftHand = NewTreeNode(context);
			*result->rightHand->binaryOperation.leftHand = leftHand;

			result->rightHand->binaryOperation.rightHand = NewTreeNode(context);
			*result->rightHand->binaryOperation.rightHand = ParseExpression(context, precedence);

			result->rightHand->binaryOperation.op = op;

			return true;
		}
	} break;
	case TOKEN_OP_ARRAY_ACCESS:
	{
		result->leftHand = NewTreeNode(context);
		*result->leftHand = leftHand;

		result->op = TOKEN_OP_ARRAY_ACCESS;
		Advance(context);

		s32 precedence = GetOperatorPrecedence(TOKEN_OP_ARRAY_ACCESS);
		if (precedence > (prevPrecedence & (~1)))
		{
			result->rightHand = NewTreeNode(context);
			*result->rightHand = ParseExpression(context, -1);

			AssertToken(context, context->token, ']');
			Advance(context);

			return true;
		}
	} break;
	case TOKEN_OP_ASSIGNMENT:
	case TOKEN_OP_EQUALS:
	case TOKEN_OP_GREATER_THAN:
	case TOKEN_OP_GREATER_THAN_OR_EQUAL:
	case TOKEN_OP_LESS_THAN:
	case TOKEN_OP_LESS_THAN_OR_EQUAL:
	case TOKEN_OP_PLUS:
	case TOKEN_OP_MINUS:
	case TOKEN_OP_MULTIPLY:
	case TOKEN_OP_DIVIDE:
	case TOKEN_OP_MODULO:
	case TOKEN_OP_MEMBER_ACCESS:
	{
		result->leftHand = NewTreeNode(context);
		*result->leftHand = leftHand;

		enum TokenType op = context->token->type;
		result->op = op;
		Advance(context);

		s32 precedence = GetOperatorPrecedence(op);
		if (precedence > (prevPrecedence & (~1)))
		{
			result->rightHand = NewTreeNode(context);
			*result->rightHand = ParseExpression(context, precedence);

			return true;
		}
	} break;
	default:
	{
		String opStr = TokenTypeToString(context->token->type);
		PrintError(context, context->token->loc, TPrintF("Unexpected operator %S", opStr));
	} break;
	}

	context->token = oldToken;
	context->currentTokenIdx = oldTokenIdx;
	return false;
}

ASTIf ParseIf(Context *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_IF);
	Advance(context);

	ASTIf ifNode = {};
	ifNode.loc = context->token->loc;

	ifNode.condition = NewTreeNode(context);
	*ifNode.condition = ParseExpression(context, -1);

	ifNode.body = NewTreeNode(context);
	*ifNode.body = ParseStatement(context);

	if (context->token->type == TOKEN_KEYWORD_ELSE)
	{
		ifNode.elseLoc = context->token->loc;
		Advance(context);
		ifNode.elseBody = NewTreeNode(context);
		*ifNode.elseBody = ParseStatement(context);
	}
	return ifNode;
}

ASTWhile ParseWhile(Context *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_WHILE);

	ASTWhile whileNode = {};
	whileNode.loc = context->token->loc;
	Advance(context);

	whileNode.condition = NewTreeNode(context);
	*whileNode.condition = ParseExpression(context, -1);
	whileNode.body = NewTreeNode(context);
	*whileNode.body = ParseStatement(context);

	return whileNode;
}

ASTStructMemberDeclaration ParseStructMemberDeclaration(Context *context)
{
	ASTStructMemberDeclaration structMem = {};
	structMem.loc = context->token->loc;

	if (context->token->type == TOKEN_KEYWORD_USING)
	{
		structMem.isUsing = true;
		Advance(context);
	}

	// Anonymous structs/unions
	if (context->token->type == TOKEN_KEYWORD_STRUCT || 
		context->token->type == TOKEN_KEYWORD_UNION)
	{
	}
	else
	{
		AssertToken(context, context->token, TOKEN_IDENTIFIER);
		structMem.name = context->token->string;
		Advance(context);

		AssertToken(context, context->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(context);
	}

	if (context->token->type != TOKEN_OP_ASSIGNMENT)
	{
		structMem.astType = NewASTType(context);
		*structMem.astType = ParseType(context);
		if (structMem.astType->nodeType == ASTTYPENODETYPE_INVALID)
			PrintError(context, context->token->loc, "Expected type"_s);
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT)
	{
		Advance(context);
		structMem.value = NewTreeNode(context);
		*structMem.value = ParseExpression(context, -1);
	}

	return structMem;
}

ASTEnumDeclaration ParseEnumDeclaration(Context *context)
{
	SourceLocation loc = context->token->loc;

	AssertToken(context, context->token, TOKEN_KEYWORD_ENUM);
	Advance(context);

	ASTEnumDeclaration enumNode = {};
	enumNode.loc = loc;
	DynamicArrayInit(&enumNode.members, 16);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION)
	{
		Advance(context);
		enumNode.astType = NewASTType(context);
		*enumNode.astType = ParseType(context);
	}

	AssertToken(context, context->token, '{');
	Advance(context);
	while (context->token->type != '}')
	{
		ASTEnumMember enumMember = {};

		AssertToken(context, context->token, TOKEN_IDENTIFIER);
		enumMember.name = context->token->string;
		Advance(context);

		if (context->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			enumMember.value = NewTreeNode(context);
			*enumMember.value = ParseExpression(context, -1);
		}

		if (context->token->type != '}')
		{
			AssertToken(context, context->token, ',');
			Advance(context);
		}

		*DynamicArrayAdd(&enumNode.members) = enumMember;
	}
	Advance(context);

	return enumNode;
}

ASTStructDeclaration ParseStructOrUnion(Context *context)
{
	SourceLocation loc = context->token->loc;

	ASSERT(context->token->type == TOKEN_KEYWORD_STRUCT ||
			context->token->type == TOKEN_KEYWORD_UNION);
	bool isUnion = context->token->type == TOKEN_KEYWORD_UNION;
	Advance(context);

	ASTStructDeclaration structDeclaration = {};
	structDeclaration.loc = loc;
	structDeclaration.isUnion = isUnion;
	DynamicArrayInit(&structDeclaration.members, 16);

	AssertToken(context, context->token, '{');
	Advance(context);
	while (context->token->type != '}')
	{
		ASTStructMemberDeclaration member = ParseStructMemberDeclaration(context);
		*DynamicArrayAdd(&structDeclaration.members) = member;
		AssertToken(context, context->token, ';');
		Advance(context);
	}
	Advance(context);

	return structDeclaration;
}

ASTVariableDeclaration ParseVariableDeclaration(Context *context)
{
	ASTVariableDeclaration varDecl = {};
	varDecl.loc = context->token->loc;

	if (context->token->type == TOKEN_KEYWORD_USING)
	{
		varDecl.isUsing = true;
		Advance(context);
	}

	varDecl.variable = BucketArrayAdd(&context->variables);
	*varDecl.variable = {};
	varDecl.variable->parameterIndex = -1;

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	varDecl.variable->name = context->token->string;
	Advance(context);

	if (context->token->type != TOKEN_OP_VARIABLE_DECLARATION && context->token->type !=
			TOKEN_OP_VARIABLE_DECLARATION_STATIC)
		UnexpectedTokenError(context, context->token);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
	{
		varDecl.variable->isStatic = true;
	}

	Advance(context);

	if (context->token->type != TOKEN_OP_ASSIGNMENT)
	{
		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context);
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT)
	{
		Advance(context);
		varDecl.value = NewTreeNode(context);
		*varDecl.value = ParseExpression(context, -1);
	}

	return varDecl;
}

ASTProcedureDeclaration ParseProcedureDeclaration(Context *context)
{
	ASTProcedureDeclaration procDecl = {};
	procDecl.loc = context->token->loc;

	Procedure *procedure = BucketArrayAdd(&context->procedures);
	*procedure = {};
	procDecl.procedure = procedure;

	if (context->token->type == TOKEN_KEYWORD_EXTERNAL)
	{
		procedure->isExternal = true;
		Advance(context);
	}

	DynamicArrayInit(&procDecl.astParameters, 4);

	AssertToken(context, context->token, '(');
	Advance(context);
	while (context->token->type != ')')
	{
		if (context->token->type == TOKEN_OP_VARARGS)
		{
			Advance(context);
			procedure->isVarargs = true;
			break;
		}

		// @Improve: separate node type for procedure parameter?
		ASTVariableDeclaration astVarDecl = ParseVariableDeclaration(context);
		ASSERT(procDecl.astParameters.size <= S8_MAX);
		astVarDecl.variable->parameterIndex = (s8)procDecl.astParameters.size;

		if (astVarDecl.variable->isStatic)
			PrintError(context, context->token->loc, "Procedure parameters can't be static"_s);

		*DynamicArrayAdd(&procDecl.astParameters) = astVarDecl;

		if (context->token->type != ')')
		{
			AssertToken(context, context->token, ',');
			Advance(context);
		}
	}
	Advance(context);

	if (context->token->type == TOKEN_OP_ARROW)
	{
		Advance(context);
		procDecl.astReturnType = NewASTType(context);
		*procDecl.astReturnType = ParseType(context);
	}
	else
	{
		procDecl.astReturnType = nullptr;
	}

	if (context->token->type == ';')
		Advance(context);
	else
	{
		procedure->astBody = NewTreeNode(context);
		*procedure->astBody = ParseStatement(context);
	}

	return procDecl;
}

ASTExpression ParseExpression(Context *context, s32 precedence)
{
	ASTExpression result = {};

	// Parenthesis
	if (context->token->type == '(')
	{
		Advance(context);

		result = ParseExpression(context, -1);

		AssertToken(context, context->token, ')');
		Advance(context);
	}
	else if (context->token->type == TOKEN_IDENTIFIER)
	{
		result.any.loc = context->token->loc;
		String identifier = context->token->string;
		Advance(context);

		if (context->token->type == '(')
		{
			// Procedure call
			result.nodeType = ASTNODETYPE_PROCEDURE_CALL;
			result.procedureCall.name = identifier;
			DynamicArrayInit(&result.procedureCall.arguments, 4);

			// Parse arguments
			Advance(context);
			while (context->token->type != ')')
			{
				ASTExpression arg = ParseExpression(context, -1);
				*DynamicArrayAdd(&result.procedureCall.arguments) = arg;

				if (context->token->type != ')')
				{
					AssertToken(context, context->token, ',');
					Advance(context);
				}
			}
			Advance(context);
		}
		else
		{
			result.nodeType = ASTNODETYPE_IDENTIFIER;
			result.identifier.string = identifier;
		}
	}
	else if (context->token->type == TOKEN_LITERAL_NUMBER)
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		bool isFloating = false;
		for (int i = 0; i < context->token->size; ++i)
		{
			if (context->token->begin[i] == '.')
			{
				isFloating = true;
				break;
			}
		}
		if (!isFloating)
		{
			result.literal.type = LITERALTYPE_INTEGER;
			result.literal.integer = IntFromString(context->token->string);
		}
		else
		{
			result.literal.type = LITERALTYPE_FLOATING;
			result.literal.floating = atof(context->token->string.data); // @Todo: replace atof
		}
		Advance(context);
	}
	else if (context->token->type == TOKEN_LITERAL_STRING)
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = context->token->string;
		Advance(context);
	}
	else if (context->token->type == TOKEN_KEYWORD_TYPEOF)
	{
		Advance(context);

		// Parenthesis should actually not be necessary here...
		//AssertToken(context, context->token, '(');
		//Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_TYPEOF;
		result.typeOfNode.expression = NewTreeNode(context);
		*result.typeOfNode.expression = ParseExpression(context, -1);

		//AssertToken(context, context->token, ')');
		//Advance(context);
	}
	else if (context->token->type == TOKEN_KEYWORD_CAST)
	{
		Advance(context);
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_CAST;

		AssertToken(context, context->token, '(');
		Advance(context);

		result.castNode.astType = ParseType(context);

		AssertToken(context, context->token, ')');
		Advance(context);

		result.castNode.expression = NewTreeNode(context);
		*result.castNode.expression = ParseExpression(context, -1);
	}
	else if (IsTokenOperator(context->token))
	{
		// Page intentionally left blank!
	}
	else
	{
		UnexpectedTokenError(context, context->token);
	}

	while (true)
	{
		if (result.nodeType == ASTNODETYPE_INVALID)
		{
			// If we have no left hand, try unary operation
			ASTUnaryOperation unaryOp = result.unaryOperation;
			bool success = TryParseUnaryOperation(context, precedence, &unaryOp);
			if (!success)
				PrintError(context, context->token->loc, "Invalid expression!"_s);
			result.nodeType = ASTNODETYPE_UNARY_OPERATION;
			result.unaryOperation = unaryOp;
		}
		else
		{
			ASTBinaryOperation binaryOp = result.binaryOperation;
			bool success = TryParseBinaryOperation(context, result, precedence, &binaryOp);
			if (!success)
				break;
			result.nodeType = ASTNODETYPE_BINARY_OPERATION;
			result.binaryOperation = binaryOp;
		}
	}

	return result;
}

ASTStaticDefinition ParseStaticDefinition(Context *context)
{
	ASTStaticDefinition result;

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	result.name = context->token->string;
	Advance(context);

	Advance(context);

	ASTExpression expression = {};
	expression.any.loc = context->token->loc;

	if (context->token->type == '(' || (context->token)->type == TOKEN_KEYWORD_EXTERNAL)
	{
		expression.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;
		expression.procedureDeclaration = ParseProcedureDeclaration(context);
	}
	else if (context->token->type == TOKEN_KEYWORD_STRUCT ||
			 context->token->type == TOKEN_KEYWORD_UNION ||
			 context->token->type == TOKEN_KEYWORD_ENUM)
	{
		expression.nodeType = ASTNODETYPE_TYPE;
		expression.astType = ParseType(context);

		AssertToken(context, context->token, ';');
		Advance(context);
	}
	else if (context->token->type == TOKEN_KEYWORD_TYPE)
	{
		Advance(context);
		expression.nodeType = ASTNODETYPE_TYPE;
		expression.astType = ParseType(context);

		AssertToken(context, context->token, ';');
		Advance(context);
	}
	else
	{
		expression = ParseExpression(context, -1);
		if (expression.nodeType != ASTNODETYPE_LITERAL)
			PrintError(context, context->token->loc, "Unsupported!"_s);

		AssertToken(context, context->token, ';');
		Advance(context);
	}

	result.expression = NewTreeNode(context);
	*result.expression = expression;

	return result;
}

ASTExpression ParseStatement(Context *context)
{
	ASTExpression result = {};

	switch (context->token->type)
	{
	case '{':
	{
		result.nodeType = ASTNODETYPE_BLOCK;

		Advance(context);
		DynamicArrayInit(&result.block.statements, 512);
		while (context->token->type != '}')
		{
			*DynamicArrayAdd(&result.block.statements) = ParseStatement(context);
		}
		Advance(context);
	} break;
	case TOKEN_KEYWORD_IF:
	{
		result.nodeType = ASTNODETYPE_IF;
		result.ifNode = ParseIf(context);
	} break;
	case TOKEN_KEYWORD_ELSE:
	{
		PrintError(context, context->token->loc, "Invalid 'else' without matching 'if'"_s);
	} break;
	case TOKEN_KEYWORD_WHILE:
	{
		result.nodeType = ASTNODETYPE_WHILE;
		result.whileNode = ParseWhile(context);
	} break;
	case TOKEN_KEYWORD_BREAK:
	{
		result.nodeType = ASTNODETYPE_BREAK;
		Advance(context);

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_RETURN;
		result.returnNode.expression = NewTreeNode(context);
		*result.returnNode.expression = ParseExpression(context, -1);

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_DEFER:
	{
		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_DEFER;
		result.deferNode.expression = NewTreeNode(context);
		*result.deferNode.expression = ParseExpression(context, -1);

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	case TOKEN_KEYWORD_UNION:
	{
		ASTVariableDeclaration varDecl = {};
		varDecl.loc = context->token->loc;

		varDecl.variable = BucketArrayAdd(&context->variables);
		*varDecl.variable = {};
		varDecl.variable->parameterIndex = -1;

		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context); // This will parse the struct/union declaration.

		if (context->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			varDecl.value = NewTreeNode(context);
			*varDecl.value = ParseExpression(context, -1);
		}

		result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
		result.variableDeclaration = varDecl;

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_ENUM:
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_ENUM_DECLARATION;
		result.enumDeclaration = ParseEnumDeclaration(context);
	} break;
	case TOKEN_KEYWORD_USING:
	{
		Advance(context);

		result = ParseStatement(context);
		switch (result.nodeType)
		{
		case ASTNODETYPE_VARIABLE_DECLARATION:
			result.variableDeclaration.isUsing = true;
			break;
		case ASTNODETYPE_IDENTIFIER:
			result.identifier.isUsing = true;
			break;
		default:
			PrintError(context, result.any.loc,
					"Expression after 'using' should be a variable or variable declaration"_s);
		}
	} break;
	default:
	{
		if ((context->token + 1)->type == TOKEN_OP_STATIC_DEF)
		{
			result.nodeType = ASTNODETYPE_STATIC_DEFINITION;
			result.staticDefinition = ParseStaticDefinition(context);
		}
		else if ((context->token + 1)->type == TOKEN_OP_VARIABLE_DECLARATION ||
				(context->token + 1)->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
		{
			result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			result.variableDeclaration = ParseVariableDeclaration(context);

			AssertToken(context, context->token, ';');
			Advance(context);
		}
		else
		{
			result = ParseExpression(context, -1);
			AssertToken(context, context->token, ';');
			Advance(context);
		}
	} break;
	}

	return result;
}

ASTExpression ParseStaticStatement(Context *context)
{
	ASTExpression result = {};

	switch (context->token->type)
	{
	case '{':
	{
		result.nodeType = ASTNODETYPE_BLOCK;

		Advance(context);
		DynamicArrayInit(&result.block.statements, 512);
		while (context->token->type != '}')
		{
			*DynamicArrayAdd(&result.block.statements) = ParseStaticStatement(context);
		}
		Advance(context);
	} break;
	default:
	{
		if ((context->token + 1)->type == TOKEN_OP_STATIC_DEF)
		{
			result.nodeType = ASTNODETYPE_STATIC_DEFINITION;
			result.staticDefinition = ParseStaticDefinition(context);
		}
		else if ((context->token + 1)->type == TOKEN_OP_VARIABLE_DECLARATION ||
				(context->token + 1)->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
		{
			result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			result.variableDeclaration = ParseVariableDeclaration(context);

			ASSERT(result.variableDeclaration.variable->isStatic);

			AssertToken(context, context->token, ';');
			Advance(context);
		}
		else
		{
			PrintError(context, context->token->loc, "Invalid expression in static context"_s);
			result = ParseExpression(context, -1);
			AssertToken(context, context->token, ';');
			Advance(context);
		}
	} break;
	}

	return result;
}

ASTRoot *GenerateSyntaxTree(Context *context)
{
	ASTRoot *root = ALLOC(FrameAlloc, ASTRoot);
	context->astRoot = root;
	DynamicArrayInit(&root->block.statements, 4096);
	BucketArrayInit(&context->treeNodes);
	BucketArrayInit(&context->astTypeNodes);
	BucketArrayInit(&context->variables);
	BucketArrayInit(&context->procedures);

	context->currentTokenIdx = 0;
	context->token = &context->tokens[0];
	while (context->token->type != TOKEN_END_OF_FILE)
	{
		*DynamicArrayAdd(&root->block.statements) = ParseStaticStatement(context);
	}

	return root;
}
