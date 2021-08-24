ASTExpression ParseExpression(Context *context, s32 precedence);
ASTExpression ParseStatement(Context *context);
ASTVariableDeclaration ParseVariableDeclaration(Context *context);

void Advance(Context *context)
{
	ASSERT(context->token == &context->tokens[context->currentTokenIdx]);

	++context->currentTokenIdx;
	ASSERT(context->currentTokenIdx < BucketArrayCount(&context->tokens));
	context->token = &context->tokens[context->currentTokenIdx];
}

ASTExpression *NewTreeNode(Context *context)
{
	return BucketArrayAdd(&context->treeNodes);
}

Type ParseType(Context *context, String *outTypeName)
{
	*outTypeName = {};
	Type result = {};

	while (context->token->type == TOKEN_OP_POINTER_TO)
	{
		++result.pointerLevels;
		Advance(context);
	}

	Token *nameToken = context->token;
	AssertToken(context, nameToken, TOKEN_IDENTIFIER);
	*outTypeName = nameToken->string;
	Advance(context);

	return result;
}

bool TryParseUnaryOperation(Context *context, s32 prevPrecedence, ASTUnaryOperation *result)
{
	if (!IsTokenOperator(context->token))
		return false;

	Token *oldToken = context->token;
	s64 oldTokenIdx = context->currentTokenIdx;

	switch (context->token->type)
	{
	case TOKEN_OP_POINTER_TO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
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
	case TOKEN_OP_MEMBER_ACCESS:
	{
		result->leftHand = NewTreeNode(context);
		*result->leftHand = leftHand;

		enum TokenType op = context->token->type;
		result->op = op;
		Advance(context);

		s32 precedence = GetOperatorPrecedence(op);

		if (precedence > prevPrecedence)
		{
			result->rightHand = NewTreeNode(context);
			*result->rightHand = ParseExpression(context, precedence);

			return true;
		}
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

	ifNode.condition = NewTreeNode(context);
	*ifNode.condition = ParseExpression(context, -1);

	ifNode.body = NewTreeNode(context);
	*ifNode.body = ParseStatement(context);

	if (context->token->type == TOKEN_KEYWORD_ELSE)
	{
		Advance(context);
		ifNode.elseNode = NewTreeNode(context);
		*ifNode.elseNode = ParseStatement(context);
	}
	return ifNode;
}

ASTWhile ParseWhile(Context *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_WHILE);
	Advance(context);

	ASTWhile whileNode = {};
	whileNode.condition = NewTreeNode(context);
	*whileNode.condition = ParseExpression(context, -1);
	whileNode.body = NewTreeNode(context);
	*whileNode.body = ParseStatement(context);

	return whileNode;
}

ASTStruct ParseStruct(Context *context)
{
	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	String name = context->token->string;
	Advance(context);

	AssertToken(context, context->token, TOKEN_OP_STATIC_DEF);
	Advance(context);

	AssertToken(context, context->token, TOKEN_KEYWORD_STRUCT);
	Advance(context);

	ASTStruct structNode = {};
	structNode.name = name;
	DynamicArrayInit(&structNode.members, 16);

	AssertToken(context, context->token, '{');
	Advance(context);
	while (context->token->type != '}')
	{
		ASTVariableDeclaration member = ParseVariableDeclaration(context);
		*DynamicArrayAdd(&structNode.members) = member;
		AssertToken(context, context->token, ';');
		Advance(context);
	}
	Advance(context);

	return structNode;
}

ASTVariableDeclaration ParseVariableDeclaration(Context *context)
{
	ASTVariableDeclaration varDecl = {};

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	varDecl.name = context->token->string;
	Advance(context);

	AssertToken(context, context->token, TOKEN_OP_VARIABLE_DECLARATION);
	Advance(context);

	if (context->token->type != TOKEN_OP_ASSIGNMENT)
	{
		Type type = ParseType(context, &varDecl.typeName);
		varDecl.type = type;
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
	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	String name = context->token->string;
	Advance(context);

	AssertToken(context, context->token, TOKEN_OP_STATIC_DEF);
	Advance(context);

	ASTProcedureDeclaration procDecl = {};
	procDecl.name = name;

	DynamicArrayInit(&procDecl.parameters, 4);

	AssertToken(context, context->token, '(');
	Advance(context);
	while (context->token->type != ')')
	{
		// @Improve: separate node type for procedure parameter?
		ASTVariableDeclaration parameter = ParseVariableDeclaration(context);
		*DynamicArrayAdd(&procDecl.parameters) = parameter;

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

		Type type = ParseType(context, &procDecl.returnTypeName);
		procDecl.returnType = type;
	}
	else
	{
		procDecl.returnTypeName = {};
	}

	procDecl.body = NewTreeNode(context);
	*procDecl.body = ParseStatement(context);

	return procDecl;
}

ASTExpression ParseExpression(Context *context, s32 precedence)
{
	ASTExpression result = {};

	// Parenthesis
	if (context->token->type == '(')
	{
		Advance(context);

		ASTExpression innerExp = ParseExpression(context, -1);

		AssertToken(context, context->token, ')');
		Advance(context);

		return innerExp;
	}

	if (context->token->type == TOKEN_IDENTIFIER)
	{
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
		Advance(context);
	}
	else if (context->token->type == TOKEN_LITERAL_STRING)
	{
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = context->token->string;
		Advance(context);
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

		Advance(context);
		DynamicArrayInit(&result.block.statements, 512);
		while (context->token->type != '}')
		{
			ASTExpression statement = ParseStatement(context);
			*DynamicArrayAdd(&result.block.statements) = statement;
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

		result.nodeType = ASTNODETYPE_RETURN;
		result.returnNode.expression = NewTreeNode(context);
		*result.returnNode.expression = ParseExpression(context, -1);

		AssertToken(context, context->token, ';');
		Advance(context);
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

	result.any.loc = context->token->loc;

	return result;
}

ASTRoot *GenerateSyntaxTree(Context *context)
{
	ASTRoot *root = ALLOC(FrameAlloc, ASTRoot);
	context->astRoot = root;
	DynamicArrayInit(&root->block.statements, 4096);
	BucketArrayInit(&context->treeNodes);

	context->currentTokenIdx = 0;
	context->token = &context->tokens[0];
	while (context->token->type != TOKEN_END_OF_FILE)
	{
		*DynamicArrayAdd(&root->block.statements) = ParseStatement(context);
	}
	return root;
}
