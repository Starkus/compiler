ASTExpression ParseExpression(Token **cursor, s32 precedence);
ASTExpression ParseStatement(Token **cursor);
ASTVariableDeclaration ParseVariableDeclaration(Token **cursor);

ASTType *ParseType(Token **cursor)
{
	Token *token = *cursor;

	ASTType *result = ALLOC(FrameAlloc, ASTType);
	result->name = {};
	result->file = token->file;
	result->line = token->line;
	result->character = token->character;
	result->type.pointerLevels = 0;

	while (token->type == TOKEN_OP_POINTERTO)
	{
		++result->type.pointerLevels;
		++token;
	}

	Token *nameToken = token;
	AssertToken(nameToken, TOKEN_IDENTIFIER);
	result->name = nameToken->string;
	++token;

	*cursor = token;
	return result;
}

bool TryParseUnaryOperation(Token **cursor, s32 prevPrecedence, ASTUnaryOperation *result)
{
	Token *token = *cursor;

	if (!IsTokenOperator(token))
	{
		return false;
	}

	switch (token->type)
	{
	case TOKEN_OP_MULTIPLY:
	{
		token->type = TOKEN_OP_DEREFERENCE;
	} break;
	}

	switch (token->type)
	{
	case TOKEN_OP_POINTERTO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	{
		enum TokenType op = token->type;
		result->op = op;
		++token;

		s32 precedence = GetOperatorPrecedence(op);
		if (precedence < prevPrecedence)
			return false;

		result->expression = ALLOC(FrameAlloc, ASTExpression);
		*result->expression = ParseExpression(&token, precedence);

		*cursor = token;
		return true;
	} break;
	}

	*cursor = token;
	return false;
}

bool TryParseBinaryOperation(Token **cursor, ASTExpression leftHand, s32 prevPrecedence,
		ASTBinaryOperation *result)
{
	Token *token = *cursor;

	if (!IsTokenOperator(token))
	{
		return false;
	}

	switch (token->type)
	{
	case TOKEN_OP_ASSIGNMENT:
	case TOKEN_OP_EQUALS:
	case TOKEN_OP_PLUS:
	case TOKEN_OP_MINUS:
	case TOKEN_OP_MULTIPLY:
	case TOKEN_OP_DIVIDE:
	case TOKEN_OP_MEMBER_ACCESS:
	{
		result->leftHand = ALLOC(FrameAlloc, ASTExpression);
		*result->leftHand = leftHand;

		enum TokenType op = token->type;
		result->op = op;
		++token;

		s32 precedence = GetOperatorPrecedence(op);

		if (precedence <= prevPrecedence)
			return false;

		result->rightHand = ALLOC(FrameAlloc, ASTExpression);
		*result->rightHand = ParseExpression(&token, precedence);

		*cursor = token;
		return true;
	} break;
	}

	*cursor = token;
	return false;
}

ASTIf ParseIf(Token **cursor)
{
	Token *token = *cursor;

	ASSERT(token->type == TOKEN_KEYWORD_IF);
	++token;

	ASTIf ifNode = {};

	ifNode.condition = ALLOC(FrameAlloc, ASTExpression);
	*ifNode.condition = ParseExpression(&token, -1);

	ifNode.body = ALLOC(FrameAlloc, ASTExpression);
	*ifNode.body = ParseStatement(&token);

	if (token->type == TOKEN_KEYWORD_ELSE)
	{
		++token;
		ifNode.elseNode = ALLOC(FrameAlloc, ASTExpression);
		*ifNode.elseNode = ParseStatement(&token);
	}

	*cursor = token;
	return ifNode;
}

ASTWhile ParseWhile(Token **cursor)
{
	Token *token = *cursor;

	ASSERT(token->type == TOKEN_KEYWORD_WHILE);
	++token;

	ASTWhile whileNode = {};
	whileNode.condition = ALLOC(FrameAlloc, ASTExpression);
	*whileNode.condition = ParseExpression(&token, -1);
	whileNode.body = ALLOC(FrameAlloc, ASTExpression);
	*whileNode.body = ParseStatement(&token);

	*cursor = token;
	return whileNode;
}

ASTStruct ParseStruct(Token **cursor)
{
	Token *token = *cursor;

	AssertToken(token, TOKEN_IDENTIFIER);
	String name = token->string;
	++token;

	AssertToken(token, TOKEN_OP_STATIC_DEF);
	++token;

	AssertToken(token, TOKEN_KEYWORD_STRUCT);
	++token;

	ASTStruct structNode = {};
	structNode.name = name;
	DynamicArrayInit<ASTVariableDeclaration>(&structNode.members, 16, FrameAlloc);

	AssertToken(token, '{');
	++token;
	while (token->type != '}')
	{
		ASTVariableDeclaration member = ParseVariableDeclaration(&token);
		*DynamicArrayAdd<ASTVariableDeclaration>(&structNode.members, FrameRealloc) = member;
		AssertToken(token, ';');
		++token;
	}
	++token;

	*cursor = token;
	return structNode;
}

ASTVariableDeclaration ParseVariableDeclaration(Token **cursor)
{
	Token *token = *cursor;

	ASTVariableDeclaration varDecl = {};

	AssertToken(token, TOKEN_IDENTIFIER);
	varDecl.name = token->string;
	++token;

	AssertToken(token, TOKEN_OP_VARIABLE_DECLARATION);
	++token;

	ASTType *type = ParseType(&token);
	varDecl.type = type;

	if (token->type == TOKEN_OP_ASSIGNMENT)
	{
		++token;
		varDecl.value = ALLOC(FrameAlloc, ASTExpression);
		*varDecl.value = ParseExpression(&token, -1);
	}

	*cursor = token;
	return varDecl;
}

ASTProcedureDeclaration ParseProcedureDeclaration(Token **cursor)
{
	Token *token = *cursor;

	AssertToken(token, TOKEN_IDENTIFIER);
	String name = token->string;
	++token;

	AssertToken(token, TOKEN_OP_STATIC_DEF);
	++token;

	ASTProcedureDeclaration procDecl = {};
	procDecl.name = name;

	DynamicArrayInit<ASTVariableDeclaration>(&procDecl.parameters, 4, FrameAlloc);

	AssertToken(token, '(');
	++token;
	while (token->type != ')')
	{
		// @Improve: separate node type for procedure parameter?
		ASTVariableDeclaration parameter = ParseVariableDeclaration(&token);
		*DynamicArrayAdd<ASTVariableDeclaration>(&procDecl.parameters, FrameRealloc) = parameter;

		if (token->type != ')')
		{
			AssertToken(token, ',');
			++token;
		}
	}
	++token;

	if (token->type == TOKEN_OP_ARROW)
	{
		++token;

		ASTType *type = ParseType(&token);
		procDecl.returnType = type;
	}
	else
	{
		procDecl.returnType = nullptr;
	}

	procDecl.body = ALLOC(FrameAlloc, ASTExpression);
	*procDecl.body = ParseStatement(&token);

	*cursor = token;
	return procDecl;
}

ASTExpression ParseExpression(Token **cursor, s32 precedence)
{
	Token *token = *cursor;

	ASTExpression result = {};

	// Parenthesis
	if (token->type == '(')
	{
		++token;

		ASTExpression innerExp = ParseExpression(&token, -1);

		AssertToken(token, ')');
		++token;

		*cursor = token;
		return innerExp;
	}

	if (token->type == TOKEN_IDENTIFIER)
	{
		String identifier = token->string;
		++token;

		if (token->type == '(')
		{
			// Procedure call
			result.nodeType = ASTNODETYPE_PROCEDURE_CALL;
			result.procedureCall.name = identifier;
			DynamicArrayInit<ASTExpression>(&result.procedureCall.arguments, 4, FrameAlloc);

			// Parse arguments
			++token;
			while (token->type != ')')
			{
				ASTExpression arg = ParseExpression(&token, -1);
				*DynamicArrayAdd<ASTExpression>(&result.procedureCall.arguments, FrameRealloc) = arg;

				if (token->type != ')')
				{
					AssertToken(token, ',');
					++token;
				}
			}
			++token;
		}
		else
		{
			// Variable
			result.nodeType = ASTNODETYPE_VARIABLE;
			result.variable.name = identifier;
		}
	}
	else if (token->type == TOKEN_LITERAL_NUMBER)
	{
		result.nodeType = ASTNODETYPE_LITERAL;

		bool isFloating = false;
		for (int i = 0; i < token->size; ++i)
		{
			if (token->begin[i] == '.')
			{
				isFloating = true;
				break;
			}
		}
		if (!isFloating)
		{
			result.literal.type = LITERALTYPE_INTEGER;
			result.literal.integer = atoi(token->string.data);
		}
		else
		{
			result.literal.type = LITERALTYPE_FLOATING;
			result.literal.floating = atof(token->string.data);
		}
		++token;
	}
	else if (token->type == TOKEN_LITERAL_STRING)
	{
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = token->string;
		++token;
	}
	else if (IsTokenOperator(token))
	{
		// Page intentionally left blank!
	}
	else
	{
		UnexpectedTokenError(token);
	}

	while (true)
	{
		if (result.nodeType == ASTNODETYPE_INVALID)
		{
			// If we have no left hand, try unary operation
			ASTUnaryOperation unaryOp = result.unaryOperation;
			bool success = TryParseUnaryOperation(&token, precedence, &unaryOp);
			if (!success)
				PrintError(token, "Invalid expression!"_s);
			result.nodeType = ASTNODETYPE_UNARY_OPERATION;
			result.unaryOperation = unaryOp;
		}
		else
		{
			ASTBinaryOperation binaryOp = result.binaryOperation;
			bool success = TryParseBinaryOperation(&token, result, precedence, &binaryOp);
			if (!success)
				break;
			result.nodeType = ASTNODETYPE_BINARY_OPERATION;
			result.binaryOperation = binaryOp;
		}

		// @Check: update precedence in every loop? Don't think so...
	}

	result.any.file = token->file;
	result.any.line = token->line;
	result.any.character = token->character;

	*cursor = token;
	return result;
}

ASTExpression ParseStatement(Token **cursor)
{
	Token *token = *cursor;

	ASTExpression result = {};

	switch (token->type)
	{
	case '{':
	{
		result.nodeType = ASTNODETYPE_BLOCK;

		++token;
		DynamicArrayInit<ASTExpression>(&result.block.statements, 512, FrameAlloc);
		while (token->type != '}')
		{
			ASTExpression statement = ParseStatement(&token);
			*DynamicArrayAdd<ASTExpression>(&result.block.statements, FrameRealloc) = statement;
		}
		++token;
	} break;
	case TOKEN_KEYWORD_IF:
	{
		result.nodeType = ASTNODETYPE_IF;
		result.ifNode = ParseIf(&token);
	} break;
	case TOKEN_KEYWORD_ELSE:
	{
		PrintError(token, "Invalid 'else' without matching 'if'"_s);
	} break;
	case TOKEN_KEYWORD_WHILE:
	{
		result.nodeType = ASTNODETYPE_WHILE;
		result.whileNode = ParseWhile(&token);
	} break;
	case TOKEN_KEYWORD_BREAK:
	{
		result.nodeType = ASTNODETYPE_BREAK;
		++token;

		AssertToken(token, ';');
		++token;
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		++token;

		result.nodeType = ASTNODETYPE_RETURN;
		result.returnNode.expression = ALLOC(FrameAlloc, ASTExpression);
		*result.returnNode.expression = ParseExpression(&token, -1);

		AssertToken(token, ';');
		++token;
	} break;
	default:
	{
		if ((token + 1)->type == TOKEN_OP_STATIC_DEF)
		{
			if ((token + 2)->type == '(')
			{
				result.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;
				result.procedureDeclaration = ParseProcedureDeclaration(&token);
			}
			else if ((token + 2)->type == TOKEN_KEYWORD_STRUCT)
			{
				result.nodeType = ASTNODETYPE_STRUCT_DECLARATION;
				result.structNode = ParseStruct(&token);
			}
			else
			{
				// @Todo: static constants
				PrintError(token, "Unsupported!"_s);
			}
		}
		else if ((token + 1)->type == TOKEN_OP_VARIABLE_DECLARATION)
		{
			result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			result.variableDeclaration = ParseVariableDeclaration(&token);

			AssertToken(token, ';');
			++token;
		}
		else
		{
			result = ParseExpression(&token, -1);
			AssertToken(token, ';');
			++token;
		}
	} break;
	}

	result.any.file = token->file;
	result.any.line = token->line;
	result.any.character = token->character;

	*cursor = token;
	return result;
}

ASTRoot *GenerateSyntaxTree(DynamicArray<Token> *tokens)
{
	ASTRoot *root = (ASTRoot *)FrameAlloc(sizeof(ASTRoot));
	DynamicArrayInit<ASTExpression>(&root->block.statements, 4096, FrameAlloc);

	Token *token = tokens->data;
	while (token->type != TOKEN_END_OF_FILE)
	{
		*DynamicArrayAdd<ASTExpression>(&root->block.statements, FrameRealloc) = ParseStatement(&token);
	}
	return root;
}
