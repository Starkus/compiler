ASTExpression ParseExpression(Context *context, s32 precedence);
ASTExpression ParseStatement(Context *context);
ASTVariableDeclaration ParseVariableDeclaration(Context *context);

Type ParseType(Context *context, String *outTypeName)
{
	*outTypeName = {};
	Type result = {};

	while (context->token->type == TOKEN_OP_POINTERTO)
	{
		++result.pointerLevels;
		++context->token;
	}

	Token *nameToken = context->token;
	AssertToken(context, nameToken, TOKEN_IDENTIFIER);
	*outTypeName = nameToken->string;
	++context->token;

	return result;
}

bool TryParseUnaryOperation(Context *context, s32 prevPrecedence, ASTUnaryOperation *result)
{
	if (!IsTokenOperator(context->token))
		return false;

	Token *oldToken = context->token;

	switch (context->token->type)
	{
	case TOKEN_OP_POINTERTO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	{
		enum TokenType op = context->token->type;
		result->op = op;
		++context->token;

		s32 precedence = GetOperatorPrecedence(op);
		//if (precedence > prevPrecedence) // @Check: ??
		{
			result->expression = ALLOC(FrameAlloc, ASTExpression);
			*result->expression = ParseExpression(context, precedence);

			return true;
		}
	} break;
	}

	context->token = oldToken;
	return false;
}

bool TryParseBinaryOperation(Context *context, ASTExpression leftHand, s32 prevPrecedence,
		ASTBinaryOperation *result)
{
	if (!IsTokenOperator(context->token))
		return false;

	Token *oldToken = context->token;

	switch (context->token->type)
	{
	case TOKEN_OP_ASSIGNMENT:
	case TOKEN_OP_EQUALS:
	case TOKEN_OP_LESSTHAN:
	case TOKEN_OP_GREATERTHAN:
	case TOKEN_OP_PLUS:
	case TOKEN_OP_MINUS:
	case TOKEN_OP_MULTIPLY:
	case TOKEN_OP_DIVIDE:
	case TOKEN_OP_MEMBER_ACCESS:
	{
		result->leftHand = ALLOC(FrameAlloc, ASTExpression);
		*result->leftHand = leftHand;

		enum TokenType op = context->token->type;
		result->op = op;
		++context->token;

		s32 precedence = GetOperatorPrecedence(op);

		if (precedence > prevPrecedence)
		{
			result->rightHand = ALLOC(FrameAlloc, ASTExpression);
			*result->rightHand = ParseExpression(context, precedence);

			return true;
		}
	} break;
	}

	context->token = oldToken;
	return false;
}

ASTIf ParseIf(Context *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_IF);
	++context->token;

	ASTIf ifNode = {};

	ifNode.condition = ALLOC(FrameAlloc, ASTExpression);
	*ifNode.condition = ParseExpression(context, -1);

	ifNode.body = ALLOC(FrameAlloc, ASTExpression);
	*ifNode.body = ParseStatement(context);

	if (context->token->type == TOKEN_KEYWORD_ELSE)
	{
		++context->token;
		ifNode.elseNode = ALLOC(FrameAlloc, ASTExpression);
		*ifNode.elseNode = ParseStatement(context);
	}
	return ifNode;
}

ASTWhile ParseWhile(Context *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_WHILE);
	++context->token;

	ASTWhile whileNode = {};
	whileNode.condition = ALLOC(FrameAlloc, ASTExpression);
	*whileNode.condition = ParseExpression(context, -1);
	whileNode.body = ALLOC(FrameAlloc, ASTExpression);
	*whileNode.body = ParseStatement(context);

	return whileNode;
}

ASTStruct ParseStruct(Context *context)
{
	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	String name = context->token->string;
	++context->token;

	AssertToken(context, context->token, TOKEN_OP_STATIC_DEF);
	++context->token;

	AssertToken(context, context->token, TOKEN_KEYWORD_STRUCT);
	++context->token;

	ASTStruct structNode = {};
	structNode.name = name;
	DynamicArrayInit(&structNode.members, 16, FrameAlloc);

	AssertToken(context, context->token, '{');
	++context->token;
	while (context->token->type != '}')
	{
		ASTVariableDeclaration member = ParseVariableDeclaration(context);
		*DynamicArrayAdd(&structNode.members, FrameRealloc) = member;
		AssertToken(context, context->token, ';');
		++context->token;
	}
	++context->token;

	return structNode;
}

ASTVariableDeclaration ParseVariableDeclaration(Context *context)
{
	ASTVariableDeclaration varDecl = {};

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	varDecl.name = context->token->string;
	++context->token;

	AssertToken(context, context->token, TOKEN_OP_VARIABLE_DECLARATION);
	++context->token;

	if (context->token->type != TOKEN_OP_ASSIGNMENT)
	{
		Type type = ParseType(context, &varDecl.typeName);
		varDecl.type = type;
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT)
	{
		++context->token;
		varDecl.value = ALLOC(FrameAlloc, ASTExpression);
		*varDecl.value = ParseExpression(context, -1);
	}

	return varDecl;
}

ASTProcedureDeclaration ParseProcedureDeclaration(Context *context)
{
	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	String name = context->token->string;
	++context->token;

	AssertToken(context, context->token, TOKEN_OP_STATIC_DEF);
	++context->token;

	ASTProcedureDeclaration procDecl = {};
	procDecl.name = name;

	DynamicArrayInit(&procDecl.parameters, 4, FrameAlloc);

	AssertToken(context, context->token, '(');
	++context->token;
	while (context->token->type != ')')
	{
		// @Improve: separate node type for procedure parameter?
		ASTVariableDeclaration parameter = ParseVariableDeclaration(context);
		*DynamicArrayAdd(&procDecl.parameters, FrameRealloc) = parameter;

		if (context->token->type != ')')
		{
			AssertToken(context, context->token, ',');
			++context->token;
		}
	}
	++context->token;

	if (context->token->type == TOKEN_OP_ARROW)
	{
		++context->token;

		Type type = ParseType(context, &procDecl.returnTypeName);
		procDecl.returnType = type;
	}
	else
	{
		procDecl.returnTypeName = {};
	}

	procDecl.body = ALLOC(FrameAlloc, ASTExpression);
	*procDecl.body = ParseStatement(context);

	return procDecl;
}

ASTExpression ParseExpression(Context *context, s32 precedence)
{
	ASTExpression result = {};

	// Parenthesis
	if (context->token->type == '(')
	{
		++context->token;

		ASTExpression innerExp = ParseExpression(context, -1);

		AssertToken(context, context->token, ')');
		++context->token;

		return innerExp;
	}

	if (context->token->type == TOKEN_IDENTIFIER)
	{
		String identifier = context->token->string;
		++context->token;

		if (context->token->type == '(')
		{
			// Procedure call
			result.nodeType = ASTNODETYPE_PROCEDURE_CALL;
			result.procedureCall.name = identifier;
			DynamicArrayInit(&result.procedureCall.arguments, 4, FrameAlloc);

			// Parse arguments
			++context->token;
			while (context->token->type != ')')
			{
				ASTExpression arg = ParseExpression(context, -1);
				*DynamicArrayAdd(&result.procedureCall.arguments, FrameRealloc) = arg;

				if (context->token->type != ')')
				{
					AssertToken(context, context->token, ',');
					++context->token;
				}
			}
			++context->token;
		}
		else
		{
			// Variable
			result.nodeType = ASTNODETYPE_VARIABLE;
			result.variable.name = identifier;
		}
	}
	else if (context->token->type == TOKEN_LITERAL_NUMBER)
	{
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
			result.literal.integer = atoi(context->token->string.data);
		}
		else
		{
			result.literal.type = LITERALTYPE_FLOATING;
			result.literal.floating = atof(context->token->string.data);
		}
		++context->token;
	}
	else if (context->token->type == TOKEN_LITERAL_STRING)
	{
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = context->token->string;
		++context->token;
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

	result.any.loc = context->token->loc;

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

		++context->token;
		DynamicArrayInit(&result.block.statements, 512, FrameAlloc);
		while (context->token->type != '}')
		{
			ASTExpression statement = ParseStatement(context);
			*DynamicArrayAdd(&result.block.statements, FrameRealloc) = statement;
		}
		++context->token;
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
		++context->token;

		AssertToken(context, context->token, ';');
		++context->token;
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		++context->token;

		result.nodeType = ASTNODETYPE_RETURN;
		result.returnNode.expression = ALLOC(FrameAlloc, ASTExpression);
		*result.returnNode.expression = ParseExpression(context, -1);

		AssertToken(context, context->token, ';');
		++context->token;
	} break;
	default:
	{
		if ((context->token + 1)->type == TOKEN_OP_STATIC_DEF)
		{
			if ((context->token + 2)->type == '(')
			{
				result.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;
				result.procedureDeclaration = ParseProcedureDeclaration(context);
			}
			else if ((context->token + 2)->type == TOKEN_KEYWORD_STRUCT)
			{
				result.nodeType = ASTNODETYPE_STRUCT_DECLARATION;
				result.structNode = ParseStruct(context);
			}
			else
			{
				// @Todo: static constants
				PrintError(context, context->token->loc, "Unsupported!"_s);
			}
		}
		else if ((context->token + 1)->type == TOKEN_OP_VARIABLE_DECLARATION)
		{
			result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			result.variableDeclaration = ParseVariableDeclaration(context);

			AssertToken(context, context->token, ';');
			++context->token;
		}
		else
		{
			result = ParseExpression(context, -1);
			AssertToken(context, context->token, ';');
			++context->token;
		}
	} break;
	}

	result.any.loc = context->token->loc;

	return result;
}

ASTRoot *GenerateSyntaxTree(Context *context)
{
	ASTRoot *root = (ASTRoot *)FrameAlloc(sizeof(ASTRoot));
	context->astRoot = root;
	DynamicArrayInit(&root->block.statements, 4096, FrameAlloc);

	context->token = context->tokens.data;
	while (context->token->type != TOKEN_END_OF_FILE)
	{
		*DynamicArrayAdd(&root->block.statements, FrameRealloc) = ParseStatement(context);
	}
	return root;
}
