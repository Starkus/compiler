ASTExpression ParseExpression(Context *context, s32 precedence);
ASTExpression ParseStatement(Context *context);
ASTExpression ParseStaticStatement(Context *context);
ASTVariableDeclaration ParseVariableDeclaration(Context *context);

void AssertToken(Context *context, Token *token, int type)
{
	if (token->type != type)
	{
		String tokenTypeGot = TokenToStringOrType(context, *token);
		String tokenTypeExp = TokenTypeToString(type);
		String errorStr = TPrintF("Expected token of type %S but got %S",
				tokenTypeExp, tokenTypeGot);
		LogError(context, token->loc, errorStr);
	}
}

#define UNEXPECTED_TOKEN_ERROR(CONTEXT, TOKEN) do { \
	String tokenString = TokenTypeToString(TOKEN->type); \
	LogError(CONTEXT, TOKEN->loc, TPrintF("Unexpected token of type %S", \
			tokenString)); \
	} while (0)

void Advance(Context *context)
{
	ASSERT(context->token == &context->tokens[context->currentTokenIdx]);

	++context->currentTokenIdx;
	u64 tokenCount = BucketArrayCount(&context->tokens);
	if (context->currentTokenIdx > tokenCount)
		LogError(context, context->token->loc, "Unexpected end of file"_s);
	context->token = &context->tokens[context->currentTokenIdx];
}

inline ASTExpression *NewTreeNode(Context *context)
{
	return BucketArrayAdd(&context->treeNodes);
}

inline ASTType *NewASTType(Context *context)
{
	return BucketArrayAdd(&context->astTypeNodes);
}

inline s64 ParseInt(Context *context, String str)
{
	bool isHex = false;
	if (str.data[0] == '0')
	{
		if (str.data[1] == 'x' || str.data[1] == 'X')
			isHex = true;
	}

	ParseNumberResult parseResult;
	if (isHex)
	{
		String numbersOnly = { str.size - 2, str.data + 2 };
		parseResult = IntFromStringHex(numbersOnly);
	}
	else
		parseResult = IntFromString(str);

	if (parseResult.error)
	{
		switch (parseResult.error)
		{
		case PARSENUMBERRROR_OVERFLOW:
			LogError(context, context->token->loc, "Integer literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context, context->token->loc, "Integer literal too negative!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context, context->token->loc,
					"Integer literal contains invalid characters"_s);
			break;
		}
	}
	return parseResult.number;
}

inline f64 ParseFloat(Context *context, String str)
{
	ParseFloatResult parseResult = F64FromString(str);
	if (parseResult.error)
	{
		switch (parseResult.error)
		{
		case PARSENUMBERRROR_OVERFLOW:
			LogError(context, context->token->loc, "Floating point literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context, context->token->loc, "Floating point literal too small!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context, context->token->loc,
					"Floating point literal contains invalid characters"_s);
		case PARSENUMBERRROR_INVALID_EXPONENT:
			LogError(context, context->token->loc,
					"Could not parse exponent in scientific notation"_s);
			break;
		}
	}
	return parseResult.number;
}

ASTStructDeclaration ParseStructOrUnion(Context *context);
ASTEnumDeclaration ParseEnumDeclaration(Context *context);
ASTProcedurePrototype ParseProcedurePrototype(Context *context);
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
			astType.arrayCount = ParseInt(context, TokenToString(context, *context->token));
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
	else if (context->token->type == TOKEN_KEYWORD_STRUCT)
	{
		astType.nodeType = ASTTYPENODETYPE_STRUCT_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(context);
	}
	else if (context->token->type == TOKEN_KEYWORD_UNION)
	{
		astType.nodeType = ASTTYPENODETYPE_UNION_DECLARATION;
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
		astType.name = TokenToString(context, *context->token);
		Advance(context);
	}
	else if (context->token->type == '(' ||
			 context->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		astType.nodeType = ASTTYPENODETYPE_PROCEDURE;
		astType.procedurePrototype = ParseProcedurePrototype(context);
	}
	else
	{
		astType.nodeType = ASTTYPENODETYPE_INVALID;
		LogError(context, context->token->loc, "Failed to parse type"_s);
	}

	return astType;
}

bool TryParseUnaryOperation(Context *context, s32 prevPrecedence, ASTUnaryOperation *result)
{
	if (!IsOperatorToken(context->token))
		return false;

	Token *oldToken = context->token;
	s64 oldTokenIdx = context->currentTokenIdx;

	result->loc = context->token->loc;

	switch (context->token->type)
	{
	case TOKEN_OP_POINTER_TO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	case TOKEN_OP_BITWISE_NOT:
	case TOKEN_OP_MINUS:
	{
		enum TokenType op = context->token->type;
		result->op = op;
		Advance(context);

		int precedenceOf = op == TOKEN_OP_MINUS ? PRECEDENCE_UNARY_SUBTRACT : op;
		s32 precedence = GetOperatorPrecedence(precedenceOf);
		result->expression = NewTreeNode(context);
		*result->expression = ParseExpression(context, precedence);

		return true;
	} break;
	}

	context->token = oldToken;
	context->currentTokenIdx = oldTokenIdx;
	return false;
}

bool TryParseBinaryOperation(Context *context, ASTExpression leftHand, s32 prevPrecedence,
		ASTBinaryOperation *result)
{
	if (!IsOperatorToken(context->token))
		return false;

	Token *oldToken = context->token;
	s64 oldTokenIdx = context->currentTokenIdx;

	result->loc = context->token->loc;

	switch (context->token->type)
	{
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
	case TOKEN_OP_ASSIGNMENT_PLUS:
	case TOKEN_OP_ASSIGNMENT_MINUS:
	case TOKEN_OP_ASSIGNMENT_MULTIPLY:
	case TOKEN_OP_ASSIGNMENT_DIVIDE:
	case TOKEN_OP_ASSIGNMENT_MODULO:
	case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
	case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
	case TOKEN_OP_ASSIGNMENT_OR:
	case TOKEN_OP_ASSIGNMENT_AND:
	case TOKEN_OP_ASSIGNMENT_BITWISE_OR:
	case TOKEN_OP_ASSIGNMENT_BITWISE_XOR:
	case TOKEN_OP_ASSIGNMENT_BITWISE_AND:
	case TOKEN_OP_EQUALS:
	case TOKEN_OP_NOT_EQUALS:
	case TOKEN_OP_GREATER_THAN:
	case TOKEN_OP_GREATER_THAN_OR_EQUAL:
	case TOKEN_OP_LESS_THAN:
	case TOKEN_OP_LESS_THAN_OR_EQUAL:
	case TOKEN_OP_PLUS:
	case TOKEN_OP_MINUS:
	case TOKEN_OP_MULTIPLY:
	case TOKEN_OP_DIVIDE:
	case TOKEN_OP_MODULO:
	case TOKEN_OP_SHIFT_LEFT:
	case TOKEN_OP_SHIFT_RIGHT:
	case TOKEN_OP_AND:
	case TOKEN_OP_OR:
	case TOKEN_OP_BITWISE_AND:
	case TOKEN_OP_BITWISE_OR:
	case TOKEN_OP_BITWISE_XOR:
	case TOKEN_OP_MEMBER_ACCESS:
	case TOKEN_OP_RANGE:
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
		LogError(context, context->token->loc, TPrintF("Unexpected operator %S", opStr));
	} break;
	}

	context->token = oldToken;
	context->currentTokenIdx = oldTokenIdx;
	return false;
}

ASTIf ParseIf(Context *context, bool onStaticContext)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_IF ||
		   context->token->type == TOKEN_DIRECTIVE_IF);
	Advance(context);

	ASTIf ifNode = {};
	ifNode.loc = context->token->loc;

	ifNode.condition = NewTreeNode(context);
	if (context->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*ifNode.condition = ParseExpression(context, -1);
		AssertToken(context, context->token, ')');
		Advance(context);
	}
	else
		*ifNode.condition = ParseExpression(context, -1);

	ifNode.body = NewTreeNode(context);
	if (onStaticContext)
		*ifNode.body = ParseStaticStatement(context);
	else
		*ifNode.body = ParseStatement(context);

	if (context->token->type == TOKEN_KEYWORD_ELSE)
	{
		ifNode.elseLoc = context->token->loc;
		Advance(context);
		ifNode.elseBody = NewTreeNode(context);
		if (onStaticContext)
			*ifNode.elseBody = ParseStaticStatement(context);
		else
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
	if (context->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*whileNode.condition = ParseExpression(context, -1);
		AssertToken(context, context->token, ')');
		Advance(context);
	}
	else
		*whileNode.condition = ParseExpression(context, -1);

	whileNode.body = NewTreeNode(context);
	*whileNode.body = ParseStatement(context);

	return whileNode;
}

ASTFor ParseFor(Context *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_FOR);

	ASTFor forNode = {};
	forNode.loc = context->token->loc;
	forNode.indexVariableName = "i"_s;
	forNode.itemVariableName = "it"_s;
	Advance(context);

	bool closeParenthesis = false;
	forNode.range = NewTreeNode(context);
	if (context->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		closeParenthesis = true;
	}

	Token *oldToken = context->token;
	u64 oldTokenIdx = context->currentTokenIdx;

	Token first = *context->token;
	Advance(context);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION)
	{
		Advance(context);
		if (first.type != TOKEN_IDENTIFIER)
			LogError(context, first.loc, "Expected name of index variable before ':' inside "
					"for loop range"_s);
		Advance(context);

		forNode.indexVariableName = TokenToString(context, first);
	}
	else if (context->token->type == ',')
	{
		Advance(context);
		Token second = *context->token;
		Advance(context);
		if (context->token->type == ',')
			LogError(context, context->token->loc, "Too many names in for loop condition, only up "
					"to 2 allowed (indexVar, itemVar : expr)"_s);
		AssertToken(context, context->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(context);

		if (first.type != TOKEN_IDENTIFIER)
			LogError(context, first.loc, "Expected name of index variable before ',' inside "
					"for loop range"_s);
		if (second.type != TOKEN_IDENTIFIER)
			LogError(context, first.loc, "Expected name of item variable before ':' inside "
					"for loop range"_s);

		forNode.indexVariableName = TokenToString(context, first);
		forNode.itemVariableName  = TokenToString(context, second);
	}
	else
	{
		context->token = oldToken;
		context->currentTokenIdx = oldTokenIdx;
	}

	*forNode.range = ParseExpression(context, -1);

	if (closeParenthesis)
	{
		AssertToken(context, context->token, ')');
		Advance(context);
	}

	forNode.body = NewTreeNode(context);
	*forNode.body = ParseStatement(context);

	return forNode;
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
		structMem.name = TokenToString(context, *context->token);
		Advance(context);

		AssertToken(context, context->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(context);
	}

	if (context->token->type != TOKEN_OP_ASSIGNMENT)
	{
		structMem.astType = NewASTType(context);
		*structMem.astType = ParseType(context);
		if (structMem.astType->nodeType == ASTTYPENODETYPE_INVALID)
			LogError(context, context->token->loc, "Expected type"_s);
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
		enumMember.name = TokenToString(context, *context->token);
		enumMember.loc = context->token->loc;
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
	Advance(context);

	ASTStructDeclaration structDeclaration = {};
	structDeclaration.loc = loc;
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

Array<ASTExpression *, FrameAllocator> ParseGroupLiteral(Context *context)
{
	DynamicArray<ASTExpression *, FrameAllocator> members;
	DynamicArrayInit(&members, 8);

	while (true)
	{
		ASTExpression *newTreeNode = NewTreeNode(context);
		*newTreeNode = ParseExpression(context, -1);
		*DynamicArrayAdd(&members) = newTreeNode;

		if (context->token->type == '}')
			break;
		if (context->token->type == ',')
		{
			Advance(context);
			continue;
		}

		String tokenStr = TokenTypeToString(context->token->type);
		LogError(context, context->token->loc, TPrintF("Parsing struct literal. Expected ',' or '}' but got %S", tokenStr));
	}

	Array<ASTExpression *, FrameAllocator> result;
	result.data = members.data;
	result.size = members.size;
#if DEBUG_BUILD
	result._capacity = members.capacity;
#endif
	return result;
}

ASTVariableDeclaration ParseVariableDeclaration(Context *context)
{
	ASTVariableDeclaration varDecl = {};
	varDecl.loc = context->token->loc;

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	varDecl.name = TokenToString(context, *context->token);
	Advance(context);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
	{
		varDecl.isStatic = true;
		Advance(context);
	}
	else
		UNEXPECTED_TOKEN_ERROR(context, context->token);

	if (context->token->type == TOKEN_DIRECTIVE_EXTERNAL)
	{
		varDecl.isExternal = true;
		Advance(context);
	}

	if (context->token->type != TOKEN_OP_ASSIGNMENT)
	{
		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context);
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT)
	{
		if (varDecl.isExternal)
			LogError(context, context->token->loc, "Can't assign value to external variable"_s);

		Advance(context);
		varDecl.astInitialValue = NewTreeNode(context);
		*varDecl.astInitialValue = ParseExpression(context, -1);
	}

	return varDecl;
}

ASTProcedureParameter ParseProcedureParameter(Context *context)
{
	ASTProcedureParameter astParameter = {};
	astParameter.loc = context->token->loc;

	if (context->token->type == TOKEN_KEYWORD_USING)
	{
		astParameter.isUsing = true;
		Advance(context);
	}

	u64 startTokenIdx = context->currentTokenIdx;

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	astParameter.name = TokenToString(context, *context->token);
	Advance(context);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else
	{
		// Nameless parameter, rewind
		context->currentTokenIdx = startTokenIdx;
		context->token = &context->tokens[startTokenIdx];
		astParameter.name = {};
	}

	if (context->token->type != TOKEN_OP_ASSIGNMENT)
	{
		astParameter.astType = NewASTType(context);
		*astParameter.astType = ParseType(context);
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT)
	{
		Advance(context);
		astParameter.astInitialValue = NewTreeNode(context);
		*astParameter.astInitialValue = ParseExpression(context, -1);
	}

	return astParameter;
}

ASTProcedurePrototype ParseProcedurePrototype(Context *context)
{
	ASTProcedurePrototype prototype = {};
	prototype.loc = context->token->loc;
	prototype.callingConvention = CC_DEFAULT;

	if (context->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		Advance(context);
		AssertToken(context, context->token, '(');
		Advance(context);

		AssertToken(context, context->token, TOKEN_IDENTIFIER);
		String tokenStr = TokenToString(context, *context->token);
		if (StringEquals(tokenStr, "win64"_s))
			prototype.callingConvention = CC_WIN64;
		else if (StringEquals(tokenStr, "linux64"_s))
			prototype.callingConvention = CC_LINUX64;
		else
			LogError(context, context->token->loc, "Invalid calling convention specified"_s);
		Advance(context);

		AssertToken(context, context->token, ')');
		Advance(context);
	}

	DynamicArrayInit(&prototype.astParameters, 4);

	AssertToken(context, context->token, '(');
	Advance(context);
	while (context->token->type != ')')
	{
		if (context->token->type == TOKEN_OP_RANGE)
		{
			Advance(context);
			prototype.isVarargs = true;
			prototype.varargsLoc = context->token->loc;

			if (context->token->type == TOKEN_IDENTIFIER)
			{
				prototype.varargsName = TokenToString(context, *context->token);
				Advance(context);
			}
			break;
		}

		ASTProcedureParameter astParam = ParseProcedureParameter(context);
		ASSERT(prototype.astParameters.size <= S8_MAX);

		*DynamicArrayAdd(&prototype.astParameters) = astParam;

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
		prototype.astReturnType = NewASTType(context);
		*prototype.astReturnType = ParseType(context);
	}
	else
	{
		prototype.astReturnType = nullptr;
	}

	return prototype;
}

ASTExpression ParseExpression(Context *context, s32 precedence)
{
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_UNSET;
	result.any.loc = context->token->loc;

	switch (context->token->type)
	{
	// Parenthesis
	case '(':
	{
		Advance(context);

		result = ParseExpression(context, -1);

		AssertToken(context, context->token, ')');
		Advance(context);
	} break;
	case '{':
	{
		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		result.literal.type = LITERALTYPE_GROUP;
		result.literal.members = ParseGroupLiteral(context);

		AssertToken(context, context->token, '}');
		Advance(context);
	} break;
	case '?':
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_GARBAGE;
		Advance(context);
	} break;
	case TOKEN_IDENTIFIER:
	{
		result.any.loc = context->token->loc;
		String identifier = TokenToString(context, *context->token);
		Advance(context);

		if (context->token->type == '(')
		{
			// Procedure call
			result.nodeType = ASTNODETYPE_PROCEDURE_CALL;
			result.procedureCall.name = identifier;
			result.procedureCall.procedureFound = false;
			DynamicArrayInit(&result.procedureCall.arguments, 4);

			// Parse arguments
			Advance(context);
			while (context->token->type != ')')
			{
				ASTExpression arg = ParseExpression(context, -1);
				*DynamicArrayAdd(&result.procedureCall.arguments) = arg;

				if (context->token->type != ')')
				{
					if (context->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(context, *context->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(context, context->token->loc, errorStr);
					}
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
	} break;
	case TOKEN_LITERAL_NUMBER:
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		String tokenStr = TokenToString(context, *context->token);

		bool isHex = false;
		bool isFloating = false;
		if (tokenStr.data[0] == '0')
		{
			if (tokenStr.data[1] == 'x' || tokenStr.data[1] == 'X')
				isHex = true;
		}

		for (u32 i = 0; i < context->token->size; ++i)
		{
			if (tokenStr.data[i] == '.')
			{
				isFloating = true;
				break;
			}
		}
		if (!isFloating)
		{
			result.literal.type = LITERALTYPE_INTEGER;
			result.literal.integer = ParseInt(context, tokenStr);
		}
		else
		{
			result.literal.type = LITERALTYPE_FLOATING;
			result.literal.floating = ParseFloat(context, tokenStr);
		}
		Advance(context);
	} break;
	case TOKEN_LITERAL_CHARACTER:
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_CHARACTER;
		result.literal.character = TokenToString(context, *context->token).data[0];
		Advance(context);
	} break;
	case TOKEN_LITERAL_STRING:
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = TokenToString(context, *context->token);
		Advance(context);
	} break;
	case TOKEN_KEYWORD_TYPEOF:
	{
		result.any.loc = context->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_TYPEOF;
		result.typeOfNode.expression = NewTreeNode(context);
		*result.typeOfNode.expression = ParseExpression(context, -1);
	} break;
	case TOKEN_KEYWORD_SIZEOF:
	{
		result.any.loc = context->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_SIZEOF;
		result.sizeOfNode.expression = NewTreeNode(context);
		*result.sizeOfNode.expression = ParseExpression(context, -1);
	} break;
	case TOKEN_DIRECTIVE_DEFINED:
	{
		result.any.loc = context->token->loc;
		Advance(context);

		AssertToken(context, context->token, '(');
		Advance(context);

		result.nodeType = ASTNODETYPE_DEFINED;
		AssertToken(context, context->token, TOKEN_IDENTIFIER);
		result.definedNode.identifier = TokenToString(context, *context->token);
		Advance(context);

		AssertToken(context, context->token, ')');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_CAST:
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
		int castPrecedence = GetOperatorPrecedence(TOKEN_KEYWORD_CAST);
		*result.castNode.expression = ParseExpression(context, castPrecedence);
	} break;
	case TOKEN_DIRECTIVE_INTRINSIC:
	{
		Advance(context);
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_INTRINSIC;

		AssertToken(context, context->token, '(');
		Advance(context);

		AssertToken(context, context->token, TOKEN_IDENTIFIER);
		result.intrinsic.name = TokenToString(context, *context->token);
		Advance(context);

		if (context->token->type == ',')
		{
			// Parse arguments
			Advance(context);
			DynamicArrayInit(&result.intrinsic.arguments, 4);
			while (context->token->type != ')')
			{
				ASTExpression arg = ParseExpression(context, -1);
				*DynamicArrayAdd(&result.intrinsic.arguments) = arg;

				if (context->token->type != ')')
				{
					if (context->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(context, *context->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(context, context->token->loc, errorStr);
					}
					Advance(context);
				}
			}
		}

		AssertToken(context, context->token, ')');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_IF:
	{
		LogError(context, context->token->loc, "'if' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_WHILE:
	{
		LogError(context, context->token->loc, "'while' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_FOR:
	{
		LogError(context, context->token->loc, "'for' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_DEFER:
	{
		LogError(context, context->token->loc, "'defer' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		LogError(context, context->token->loc, "'return' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	{
		LogError(context, context->token->loc, "'struct' not valid on this context!"_s);
	} break;
	case TOKEN_KEYWORD_UNION:
	{
		LogError(context, context->token->loc, "'union' not valid on this context!"_s);
	} break;
	case TOKEN_DIRECTIVE_TYPE:
	{
		LogError(context, context->token->loc, "Not a valid type context!"_s);
	} break;
	default:
	{
		if (!IsOperatorToken(context->token))
			UNEXPECTED_TOKEN_ERROR(context, context->token);
		// Operators are handled in the loop below.
	}
	}

	// Binary/unary operators loop
	while (true)
	{
		if (result.nodeType == ASTNODETYPE_INVALID)
		{
			// If we have no left hand, try unary operation
			ASTUnaryOperation unaryOp = result.unaryOperation;
			bool success = TryParseUnaryOperation(context, precedence, &unaryOp);
			if (!success)
				LogError(context, context->token->loc, "Invalid expression!"_s);
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
	ASTStaticDefinition result = {};

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	result.name = TokenToString(context, *context->token);
	result.loc = context->token->loc;
	Advance(context);

	Advance(context);

	ASTExpression expression = {};
	expression.typeTableIdx = TYPETABLEIDX_UNSET;
	expression.any.loc = context->token->loc;

	bool isInline = false;
	bool isExternal = false;
	bool isExported = false;
	while (true)
	{
		if (context->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			if (isInline) LogError(context, context->token->loc, "'inline' used twice"_s);
			isInline = true;
			Advance(context);
		}
		else if (context->token->type == TOKEN_DIRECTIVE_EXTERNAL)
		{
			if (isExternal) LogError(context, context->token->loc, "'external' used twice"_s);
			isExternal = true;
			Advance(context);
		}
		else if (context->token->type == TOKEN_DIRECTIVE_EXPORT)
		{
			if (isExported) LogError(context, context->token->loc, "'export' used twice"_s);
			isExported = true;
			Advance(context);
		}
		else
			break;
	}

	// Procedures!
	if (context->token->type == '(' ||
		context->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		expression.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;

		ASTProcedureDeclaration procDecl = {};
		procDecl.loc = context->token->loc;
		procDecl.name = result.name;
		procDecl.isInline = isInline;
		procDecl.isExternal = isExternal;
		procDecl.isExported = isExported;
		procDecl.prototype = ParseProcedurePrototype(context);

		if (context->token->type == ';')
			Advance(context);
		else
		{
			if (isExternal)
				LogError(context, context->token->loc, "External procedure declaration can't have a body"_s);
			procDecl.astBody = NewTreeNode(context);
			*procDecl.astBody = ParseStatement(context);
		}

		expression.procedureDeclaration = procDecl;
	}
	else
	{
		if (isInline)
			LogError(context, context->token->loc, "'inline' specified for a non-procedure!"_s);
		if (isExternal)
			LogError(context, context->token->loc, "'external' specified for a non-procedure!"_s);
		if (isExported)
			LogError(context, context->token->loc, "'external' specified for a non-procedure!"_s);

		switch (context->token->type)
		{
		case TOKEN_KEYWORD_STRUCT:
		case TOKEN_KEYWORD_UNION:
		case TOKEN_KEYWORD_ENUM:
		{
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);

			AssertToken(context, context->token, ';');
			Advance(context);
		} break;
		case TOKEN_DIRECTIVE_TYPE:
		{
			Advance(context);
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);

			AssertToken(context, context->token, ';');
			Advance(context);
		} break;
		case TOKEN_DIRECTIVE_ALIAS:
		{
			Advance(context);
			expression.nodeType = ASTNODETYPE_ALIAS;
			expression.astType = ParseType(context);

			AssertToken(context, context->token, ';');
			Advance(context);
		} break;
		default:
		{
			expression = ParseExpression(context, -1);

			AssertToken(context, context->token, ';');
			Advance(context);
		}
		}
	}

	result.expression = NewTreeNode(context);
	*result.expression = expression;

	return result;
}

ASTExpression ParseStatement(Context *context)
{
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_UNSET;
	result.any.loc = context->token->loc;

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
		result.ifNode = ParseIf(context, false);
	} break;
	case TOKEN_DIRECTIVE_IF:
	{
		result.nodeType = ASTNODETYPE_IF_STATIC;
		result.ifNode = ParseIf(context, false);
	} break;
	case TOKEN_KEYWORD_ELSE:
	{
		LogError(context, context->token->loc, "Invalid 'else' without matching 'if'"_s);
	} break;
	case TOKEN_KEYWORD_WHILE:
	{
		result.nodeType = ASTNODETYPE_WHILE;
		result.whileNode = ParseWhile(context);
	} break;
	case TOKEN_KEYWORD_FOR:
	{
		result.nodeType = ASTNODETYPE_FOR;
		result.forNode = ParseFor(context);
	} break;
	case TOKEN_KEYWORD_CONTINUE:
	{
		result.nodeType = ASTNODETYPE_CONTINUE;
		Advance(context);

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_REMOVE:
	{
		result.nodeType = ASTNODETYPE_REMOVE;
		Advance(context);

		AssertToken(context, context->token, ';');
		Advance(context);
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
		if (context->token->type == ';')
			result.returnNode.expression = nullptr;
		else
		{
			result.returnNode.expression = NewTreeNode(context);
			*result.returnNode.expression = ParseExpression(context, -1);
		}

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

		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context); // This will parse the struct/union declaration.

		if (context->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			varDecl.astInitialValue = NewTreeNode(context);
			*varDecl.astInitialValue = ParseExpression(context, -1);
		}

		result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
		result.variableDeclaration = varDecl;

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_USING:
	{
		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_USING;
		result.usingNode.expression = NewTreeNode(context);
		*result.usingNode.expression = ParseStatement(context);
	} break;
	default:
	{
		Token *next = &context->tokens[context->currentTokenIdx + 1];
		if (next->type == TOKEN_OP_STATIC_DEF)
		{
			result.nodeType = ASTNODETYPE_STATIC_DEFINITION;
			result.staticDefinition = ParseStaticDefinition(context);
		}
		else if (next->type == TOKEN_OP_VARIABLE_DECLARATION ||
				next->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
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
	result.any.loc = context->token->loc;
	result.typeTableIdx = TYPETABLEIDX_UNSET;

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
	case TOKEN_DIRECTIVE_IF:
	{
		result.nodeType = ASTNODETYPE_IF_STATIC;
		result.ifNode = ParseIf(context, true);
	} break;
	case TOKEN_DIRECTIVE_OPERATOR:
	{
		Advance(context);

		enum TokenType op = context->token->type;
		if (op < TOKEN_OP_Begin || op > TOKEN_OP_End)
			UNEXPECTED_TOKEN_ERROR(context, context->token);
		Advance(context);

		AssertToken(context, context->token, TOKEN_OP_STATIC_DEF);
		Advance(context);

		bool isInline = false;
		if (context->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			isInline = true;
			Advance(context);
		}

		ASTOperatorOverload overload = {};
		overload.loc = context->token->loc;
		overload.op = op;
		overload.isInline = isInline;
		overload.prototype = ParseProcedurePrototype(context);

		overload.astBody = NewTreeNode(context);
		*overload.astBody = ParseStatement(context);

		result.nodeType = ASTNODETYPE_OPERATOR_OVERLOAD;
		result.operatorOverload = overload;
	} break;
	case TOKEN_DIRECTIVE_INCLUDE:
	{
		result.nodeType = ASTNODETYPE_INCLUDE;
		Advance(context);

		AssertToken(context, context->token, TOKEN_LITERAL_STRING);
		result.include.filename = TokenToString(context, *context->token);
		Advance(context);

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	case TOKEN_DIRECTIVE_LINKLIB:
	{
		result.nodeType = ASTNODETYPE_LINKLIB;
		Advance(context);

		AssertToken(context, context->token, TOKEN_LITERAL_STRING);
		result.linklib.filename = TokenToString(context, *context->token);
		Advance(context);

		AssertToken(context, context->token, ';');
		Advance(context);
	} break;
	default:
	{
		Token *next = &context->tokens[context->currentTokenIdx + 1];
		if (next->type == TOKEN_OP_STATIC_DEF)
		{
			result.nodeType = ASTNODETYPE_STATIC_DEFINITION;
			result.staticDefinition = ParseStaticDefinition(context);
		}
		else if (next->type == TOKEN_OP_VARIABLE_DECLARATION ||
				next->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
		{
			result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			result.variableDeclaration = ParseVariableDeclaration(context);

			ASSERT(result.variableDeclaration.isStatic ||
				   result.variableDeclaration.isExternal);

			AssertToken(context, context->token, ';');
			Advance(context);
		}
		else
			LogError(context, context->token->loc, "Invalid expression in static context"_s);
	} break;
	}

	return result;
}

ASTRoot *GenerateSyntaxTree(Context *context)
{
	ASTRoot *root = ALLOC(FrameAllocator::Alloc, ASTRoot);
	context->astRoot = root;
	DynamicArrayInit(&root->block.statements, 4096);
	BucketArrayInit(&context->treeNodes);
	BucketArrayInit(&context->astTypeNodes);
	BucketArrayInit(&context->stringLiterals);

	// Empty string
	*BucketArrayAdd(&context->stringLiterals) = {};

	context->currentTokenIdx = 0;
	context->token = &context->tokens[0];
	while (context->token->type != TOKEN_END_OF_FILE)
	{
		*DynamicArrayAdd(&root->block.statements) = ParseStaticStatement(context);
	}

	return root;
}
