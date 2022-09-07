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
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	BucketArray<Token, HeapAllocator, 1024> &fileTokens = context->fileTokens[threadData->fileIdx];
	ASSERT(threadData->token == &fileTokens[threadData->currentTokenIdx]);

	++threadData->currentTokenIdx;
	if (threadData->currentTokenIdx > BucketArrayCount(&fileTokens))
		LogError(context, threadData->token->loc, "Unexpected end of file"_s);
	threadData->token = &fileTokens[threadData->currentTokenIdx];
}

inline ASTExpression *NewTreeNode(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	auto treeNodes = context->fileTreeNodes[threadData->fileIdx].GetForWrite();
	return BucketArrayAdd(&treeNodes);
}

inline ASTType *NewASTType(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	auto typeNodes = context->fileTypeNodes[threadData->fileIdx].GetForWrite();
	return BucketArrayAdd(&typeNodes);
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
		ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
		switch (parseResult.error)
		{
		case PARSENUMBERRROR_OVERFLOW:
			LogError(context, threadData->token->loc, "Integer literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context, threadData->token->loc, "Integer literal too negative!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context, threadData->token->loc,
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
		ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
		switch (parseResult.error)
		{
		case PARSENUMBERRROR_OVERFLOW:
			LogError(context, threadData->token->loc, "Floating point literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context, threadData->token->loc, "Floating point literal too small!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context, threadData->token->loc,
					"Floating point literal contains invalid characters"_s);
		case PARSENUMBERRROR_INVALID_EXPONENT:
			LogError(context, threadData->token->loc,
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
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);

	ASTType astType;
	astType.loc = threadData->token->loc;

	if (threadData->token->type == TOKEN_OP_ARRAY_ACCESS)
	{
		Advance(context);
		astType.nodeType = ASTTYPENODETYPE_ARRAY;

		astType.arrayCount = 0;
		if (threadData->token->type == TOKEN_LITERAL_NUMBER)
		{
			astType.arrayCount = ParseInt(context, TokenToString(context, *threadData->token));
			Advance(context);
		}
		AssertToken(context, threadData->token, ']');
		Advance(context);

		astType.arrayType = NewASTType(context);
		*astType.arrayType = ParseType(context);
	}
	else if (threadData->token->type == TOKEN_OP_POINTER_TO)
	{
		Advance(context);
		astType.nodeType = ASTTYPENODETYPE_POINTER;
		astType.pointedType = NewASTType(context);
		*astType.pointedType = ParseType(context);
	}
	else if (threadData->token->type == TOKEN_KEYWORD_STRUCT)
	{
		astType.nodeType = ASTTYPENODETYPE_STRUCT_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(context);
	}
	else if (threadData->token->type == TOKEN_KEYWORD_UNION)
	{
		astType.nodeType = ASTTYPENODETYPE_UNION_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(context);
	}
	else if (threadData->token->type == TOKEN_KEYWORD_ENUM)
	{
		astType.nodeType = ASTTYPENODETYPE_ENUM_DECLARATION;
		astType.enumDeclaration = ParseEnumDeclaration(context);
	}
	else if (threadData->token->type == TOKEN_IDENTIFIER)
	{
		astType.nodeType = ASTTYPENODETYPE_IDENTIFIER;
		astType.name = TokenToString(context, *threadData->token);
		Advance(context);
	}
	else if (threadData->token->type == '(' ||
			 threadData->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		astType.nodeType = ASTTYPENODETYPE_PROCEDURE;
		astType.procedurePrototype = ParseProcedurePrototype(context);
	}
	else
	{
		astType.nodeType = ASTTYPENODETYPE_INVALID;
		LogError(context, threadData->token->loc, "Failed to parse type"_s);
	}

	return astType;
}

bool TryParseUnaryOperation(Context *context, s32 prevPrecedence, ASTUnaryOperation *result)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);

	if (!IsOperatorToken(threadData->token))
		return false;

	Token *oldToken = threadData->token;
	s64 oldTokenIdx = threadData->currentTokenIdx;

	result->loc = threadData->token->loc;

	switch (threadData->token->type)
	{
	case TOKEN_OP_POINTER_TO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	case TOKEN_OP_BITWISE_NOT:
	case TOKEN_OP_MINUS:
	{
		enum TokenType op = threadData->token->type;
		result->op = op;
		Advance(context);

		int precedenceOf = op == TOKEN_OP_MINUS ? PRECEDENCE_UNARY_SUBTRACT : op;
		s32 precedence = GetOperatorPrecedence(precedenceOf);
		result->expression = NewTreeNode(context);
		*result->expression = ParseExpression(context, precedence);

		return true;
	} break;
	}

	threadData->token = oldToken;
	threadData->currentTokenIdx = oldTokenIdx;
	return false;
}

bool TryParseBinaryOperation(Context *context, ASTExpression leftHand, s32 prevPrecedence,
		ASTBinaryOperation *result)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);

	if (!IsOperatorToken(threadData->token))
		return false;

	Token *oldToken = threadData->token;
	s64 oldTokenIdx = threadData->currentTokenIdx;

	result->loc = threadData->token->loc;

	switch (threadData->token->type)
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

			AssertToken(context, threadData->token, ']');
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

		enum TokenType op = threadData->token->type;
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
		String opStr = TokenTypeToString(threadData->token->type);
		LogError(context, threadData->token->loc, TPrintF("Unexpected operator %S", opStr));
	} break;
	}

	threadData->token = oldToken;
	threadData->currentTokenIdx = oldTokenIdx;
	return false;
}

ASTIf ParseIf(Context *context, bool onStaticContext)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASSERT(threadData->token->type == TOKEN_KEYWORD_IF ||
		   threadData->token->type == TOKEN_DIRECTIVE_IF);
	Advance(context);

	ASTIf ifNode = {};
	ifNode.loc = threadData->token->loc;

	ifNode.condition = NewTreeNode(context);
	if (threadData->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*ifNode.condition = ParseExpression(context, -1);
		AssertToken(context, threadData->token, ')');
		Advance(context);
	}
	else
		*ifNode.condition = ParseExpression(context, -1);

	ifNode.body = NewTreeNode(context);
	if (onStaticContext)
		*ifNode.body = ParseStaticStatement(context);
	else
		*ifNode.body = ParseStatement(context);

	if (threadData->token->type == TOKEN_KEYWORD_ELSE)
	{
		ifNode.elseLoc = threadData->token->loc;
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
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASSERT(threadData->token->type == TOKEN_KEYWORD_WHILE);

	ASTWhile whileNode = {};
	whileNode.loc = threadData->token->loc;
	Advance(context);

	whileNode.condition = NewTreeNode(context);
	if (threadData->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*whileNode.condition = ParseExpression(context, -1);
		AssertToken(context, threadData->token, ')');
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
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASSERT(threadData->token->type == TOKEN_KEYWORD_FOR);

	ASTFor forNode = {};
	forNode.loc = threadData->token->loc;
	forNode.indexVariableName = "i"_s;
	forNode.itemVariableName = "it"_s;
	Advance(context);

	bool closeParenthesis = false;
	forNode.range = NewTreeNode(context);
	if (threadData->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		closeParenthesis = true;
	}

	Token *oldToken = threadData->token;
	u64 oldTokenIdx = threadData->currentTokenIdx;

	Token first = *threadData->token;
	Advance(context);

	if (threadData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
	{
		Advance(context);
		if (first.type != TOKEN_IDENTIFIER)
			LogError(context, first.loc, "Expected name of index variable before ':' inside "
					"for loop range"_s);
		Advance(context);

		forNode.indexVariableName = TokenToString(context, first);
	}
	else if (threadData->token->type == ',')
	{
		Advance(context);
		Token second = *threadData->token;
		Advance(context);
		if (threadData->token->type == ',')
			LogError(context, threadData->token->loc, "Too many names in for loop condition, only up "
					"to 2 allowed (indexVar, itemVar : expr)"_s);
		AssertToken(context, threadData->token, TOKEN_OP_VARIABLE_DECLARATION);
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
		threadData->token = oldToken;
		threadData->currentTokenIdx = oldTokenIdx;
	}

	*forNode.range = ParseExpression(context, -1);

	if (closeParenthesis)
	{
		AssertToken(context, threadData->token, ')');
		Advance(context);
	}

	forNode.body = NewTreeNode(context);
	*forNode.body = ParseStatement(context);

	return forNode;
}

ASTStructMemberDeclaration ParseStructMemberDeclaration(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTStructMemberDeclaration structMem = {};
	structMem.loc = threadData->token->loc;

	if (threadData->token->type == TOKEN_KEYWORD_USING)
	{
		structMem.isUsing = true;
		Advance(context);
	}

	// Anonymous structs/unions
	if (threadData->token->type == TOKEN_KEYWORD_STRUCT || 
		threadData->token->type == TOKEN_KEYWORD_UNION)
	{
	}
	else
	{
		AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
		structMem.name = TokenToString(context, *threadData->token);
		Advance(context);

		AssertToken(context, threadData->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(context);
	}

	if (threadData->token->type != TOKEN_OP_ASSIGNMENT)
	{
		structMem.astType = NewASTType(context);
		*structMem.astType = ParseType(context);
		if (structMem.astType->nodeType == ASTTYPENODETYPE_INVALID)
			LogError(context, threadData->token->loc, "Expected type"_s);
	}

	if (threadData->token->type == TOKEN_OP_ASSIGNMENT)
	{
		Advance(context);
		structMem.value = NewTreeNode(context);
		*structMem.value = ParseExpression(context, -1);
	}

	return structMem;
}

ASTEnumDeclaration ParseEnumDeclaration(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	SourceLocation loc = threadData->token->loc;

	AssertToken(context, threadData->token, TOKEN_KEYWORD_ENUM);
	Advance(context);

	ASTEnumDeclaration enumNode = {};
	enumNode.loc = loc;
	DynamicArrayInit(&enumNode.members, 16);

	if (threadData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
	{
		Advance(context);
		enumNode.astType = NewASTType(context);
		*enumNode.astType = ParseType(context);
	}

	AssertToken(context, threadData->token, '{');
	Advance(context);
	while (threadData->token->type != '}')
	{
		ASTEnumMember enumMember = {};

		AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
		enumMember.name = TokenToString(context, *threadData->token);
		enumMember.loc = threadData->token->loc;
		Advance(context);

		if (threadData->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			enumMember.value = NewTreeNode(context);
			*enumMember.value = ParseExpression(context, -1);
		}

		if (threadData->token->type != '}')
		{
			AssertToken(context, threadData->token, ',');
			Advance(context);
		}

		*DynamicArrayAdd(&enumNode.members) = enumMember;
	}
	Advance(context);

	return enumNode;
}

ASTStructDeclaration ParseStructOrUnion(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	SourceLocation loc = threadData->token->loc;

	ASSERT(threadData->token->type == TOKEN_KEYWORD_STRUCT ||
			threadData->token->type == TOKEN_KEYWORD_UNION);
	Advance(context);

	ASTStructDeclaration structDeclaration = {};
	structDeclaration.loc = loc;
	DynamicArrayInit(&structDeclaration.members, 16);

	AssertToken(context, threadData->token, '{');
	Advance(context);
	while (threadData->token->type != '}')
	{
		ASTStructMemberDeclaration member = ParseStructMemberDeclaration(context);
		*DynamicArrayAdd(&structDeclaration.members) = member;

		AssertToken(context, threadData->token, ';');
		Advance(context);
	}
	Advance(context);

	return structDeclaration;
}

Array<ASTExpression *, LinearAllocator> ParseGroupLiteral(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	DynamicArray<ASTExpression *, LinearAllocator> members;
	DynamicArrayInit(&members, 8);

	while (true)
	{
		ASTExpression *newTreeNode = NewTreeNode(context);
		*newTreeNode = ParseExpression(context, -1);
		*DynamicArrayAdd(&members) = newTreeNode;

		if (threadData->token->type == '}')
			break;
		if (threadData->token->type == ',')
		{
			Advance(context);
			continue;
		}

		String tokenStr = TokenTypeToString(threadData->token->type);
		LogError(context, threadData->token->loc, TPrintF("Parsing struct literal. Expected ',' or '}' but got %S", tokenStr));
	}

	Array<ASTExpression *, LinearAllocator> result;
	result.data = members.data;
	result.size = members.size;
#if DEBUG_BUILD
	result._capacity = members.capacity;
#endif
	return result;
}

ASTVariableDeclaration ParseVariableDeclaration(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTVariableDeclaration varDecl = {};
	varDecl.loc = threadData->token->loc;

	AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
	varDecl.name = TokenToString(context, *threadData->token);
	Advance(context);

	if (threadData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else if (threadData->token->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
	{
		varDecl.isStatic = true;
		Advance(context);
	}
	else
		UNEXPECTED_TOKEN_ERROR(context, threadData->token);

	if (threadData->token->type == TOKEN_DIRECTIVE_EXTERNAL)
	{
		varDecl.isExternal = true;
		Advance(context);
	}

	if (threadData->token->type != TOKEN_OP_ASSIGNMENT)
	{
		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context);
	}

	if (threadData->token->type == TOKEN_OP_ASSIGNMENT)
	{
		if (varDecl.isExternal)
			LogError(context, threadData->token->loc, "Can't assign value to external variable"_s);

		Advance(context);
		varDecl.astInitialValue = NewTreeNode(context);
		*varDecl.astInitialValue = ParseExpression(context, -1);
	}

	return varDecl;
}

ASTProcedureParameter ParseProcedureParameter(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTProcedureParameter astParameter = {};
	astParameter.loc = threadData->token->loc;

	if (threadData->token->type == TOKEN_KEYWORD_USING)
	{
		astParameter.isUsing = true;
		Advance(context);
	}

	u64 startTokenIdx = threadData->currentTokenIdx;

	AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
	astParameter.name = TokenToString(context, *threadData->token);
	Advance(context);

	if (threadData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else
	{
		// Nameless parameter, rewind
		threadData->currentTokenIdx = startTokenIdx;
		threadData->token = &context->fileTokens[threadData->fileIdx][startTokenIdx];
		astParameter.name = {};
	}

	if (threadData->token->type != TOKEN_OP_ASSIGNMENT)
	{
		astParameter.astType = NewASTType(context);
		*astParameter.astType = ParseType(context);
	}

	if (threadData->token->type == TOKEN_OP_ASSIGNMENT)
	{
		Advance(context);
		astParameter.astInitialValue = NewTreeNode(context);
		*astParameter.astInitialValue = ParseExpression(context, -1);
	}

	return astParameter;
}

ASTProcedurePrototype ParseProcedurePrototype(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTProcedurePrototype prototype = {};
	prototype.loc = threadData->token->loc;
	prototype.callingConvention = CC_DEFAULT;

	if (threadData->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		Advance(context);
		AssertToken(context, threadData->token, '(');
		Advance(context);

		AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
		String tokenStr = TokenToString(context, *threadData->token);
		if (StringEquals(tokenStr, "win64"_s))
			prototype.callingConvention = CC_WIN64;
		else if (StringEquals(tokenStr, "linux64"_s))
			prototype.callingConvention = CC_LINUX64;
		else
			LogError(context, threadData->token->loc, "Invalid calling convention specified"_s);
		Advance(context);

		AssertToken(context, threadData->token, ')');
		Advance(context);
	}

	DynamicArrayInit(&prototype.astParameters, 4);

	AssertToken(context, threadData->token, '(');
	Advance(context);
	while (threadData->token->type != ')')
	{
		if (threadData->token->type == TOKEN_OP_RANGE)
		{
			Advance(context);
			prototype.isVarargs = true;
			prototype.varargsLoc = threadData->token->loc;

			if (threadData->token->type == TOKEN_IDENTIFIER)
			{
				prototype.varargsName = TokenToString(context, *threadData->token);
				Advance(context);
			}
			break;
		}

		ASTProcedureParameter astParam = ParseProcedureParameter(context);
		ASSERT(prototype.astParameters.size <= S8_MAX);

		*DynamicArrayAdd(&prototype.astParameters) = astParam;

		if (threadData->token->type != ')')
		{
			AssertToken(context, threadData->token, ',');
			Advance(context);
		}
	}
	Advance(context);

	if (threadData->token->type == TOKEN_OP_ARROW)
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
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_Unset;
	result.any.loc = threadData->token->loc;

	switch (threadData->token->type)
	{
	// Parenthesis
	case '(':
	{
		Advance(context);

		result = ParseExpression(context, -1);

		AssertToken(context, threadData->token, ')');
		Advance(context);
	} break;
	case '{':
	{
		Advance(context);

		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		result.literal.type = LITERALTYPE_GROUP;
		result.literal.members = ParseGroupLiteral(context);

		AssertToken(context, threadData->token, '}');
		Advance(context);
	} break;
	case '?':
	{
		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_GARBAGE;
		Advance(context);
	} break;
	case TOKEN_IDENTIFIER:
	{
		result.any.loc = threadData->token->loc;
		String identifier = TokenToString(context, *threadData->token);
		Advance(context);

		if (threadData->token->type == '(')
		{
			// Procedure call
			result.nodeType = ASTNODETYPE_PROCEDURE_CALL;
			result.procedureCall.name = identifier;
			HybridArrayInit(&result.procedureCall.arguments);

			// Parse arguments
			Advance(context);
			while (threadData->token->type != ')')
			{
				ASTExpression *arg = NewTreeNode(context);
				*arg = ParseExpression(context, -1);
				*HybridArrayAdd(&result.procedureCall.arguments) = arg;

				if (threadData->token->type != ')')
				{
					if (threadData->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(context, *threadData->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(context, threadData->token->loc, errorStr);
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
		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		String tokenStr = TokenToString(context, *threadData->token);

		bool isHex = false;
		bool isFloating = false;
		if (tokenStr.data[0] == '0')
		{
			if (tokenStr.data[1] == 'x' || tokenStr.data[1] == 'X')
				isHex = true;
		}

		for (u32 i = 0; i < threadData->token->size; ++i)
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
		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_CHARACTER;
		result.literal.character = TokenToString(context, *threadData->token).data[0];
		Advance(context);
	} break;
	case TOKEN_LITERAL_STRING:
	{
		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = TokenToString(context, *threadData->token);
		Advance(context);
	} break;
	case TOKEN_KEYWORD_TYPEOF:
	{
		result.any.loc = threadData->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_TYPEOF;
		result.typeOfNode.expression = NewTreeNode(context);
		*result.typeOfNode.expression = ParseExpression(context, -1);
	} break;
	case TOKEN_KEYWORD_SIZEOF:
	{
		result.any.loc = threadData->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_SIZEOF;
		result.sizeOfNode.expression = NewTreeNode(context);
		*result.sizeOfNode.expression = ParseExpression(context, -1);
	} break;
	case TOKEN_DIRECTIVE_DEFINED:
	{
		result.any.loc = threadData->token->loc;
		Advance(context);

		AssertToken(context, threadData->token, '(');
		Advance(context);

		result.nodeType = ASTNODETYPE_DEFINED;
		AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
		result.definedNode.identifier = TokenToString(context, *threadData->token);
		Advance(context);

		AssertToken(context, threadData->token, ')');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_CAST:
	{
		Advance(context);
		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_CAST;

		AssertToken(context, threadData->token, '(');
		Advance(context);

		result.castNode.astType = ParseType(context);

		AssertToken(context, threadData->token, ')');
		Advance(context);

		result.castNode.expression = NewTreeNode(context);
		int castPrecedence = GetOperatorPrecedence(TOKEN_KEYWORD_CAST);
		*result.castNode.expression = ParseExpression(context, castPrecedence);
	} break;
	case TOKEN_DIRECTIVE_INTRINSIC:
	{
		Advance(context);
		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_INTRINSIC;

		AssertToken(context, threadData->token, '(');
		Advance(context);

		AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
		result.intrinsic.name = TokenToString(context, *threadData->token);
		Advance(context);

		if (threadData->token->type == ',')
		{
			// Parse arguments
			Advance(context);
			DynamicArrayInit(&result.intrinsic.arguments, 4);
			while (threadData->token->type != ')')
			{
				ASTExpression arg = ParseExpression(context, -1);
				*DynamicArrayAdd(&result.intrinsic.arguments) = arg;

				if (threadData->token->type != ')')
				{
					if (threadData->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(context, *threadData->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(context, threadData->token->loc, errorStr);
					}
					Advance(context);
				}
			}
		}

		AssertToken(context, threadData->token, ')');
		Advance(context);
	} break;
	case TOKEN_DIRECTIVE_TYPE:
	{
		Advance(context);
		result.nodeType = ASTNODETYPE_TYPE;
		result.astType = ParseType(context);
	} break;
	case TOKEN_KEYWORD_IF:
	{
		LogError(context, threadData->token->loc, "'if' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_WHILE:
	{
		LogError(context, threadData->token->loc, "'while' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_FOR:
	{
		LogError(context, threadData->token->loc, "'for' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_DEFER:
	{
		LogError(context, threadData->token->loc, "'defer' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		LogError(context, threadData->token->loc, "'return' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	{
		LogError(context, threadData->token->loc, "'struct' not valid on this context!"_s);
	} break;
	case TOKEN_KEYWORD_UNION:
	{
		LogError(context, threadData->token->loc, "'union' not valid on this context!"_s);
	} break;
	default:
	{
		if (!IsOperatorToken(threadData->token))
			UNEXPECTED_TOKEN_ERROR(context, threadData->token);
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
				LogError(context, threadData->token->loc, "Invalid expression!"_s);
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
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTStaticDefinition result = {};

	AssertToken(context, threadData->token, TOKEN_IDENTIFIER);
	result.name = TokenToString(context, *threadData->token);
	result.loc = threadData->token->loc;
	Advance(context);

	Advance(context);

	ASTExpression expression = {};
	expression.typeTableIdx = TYPETABLEIDX_Unset;
	expression.any.loc = threadData->token->loc;

	bool isInline = false;
	bool isExternal = false;
	bool isExported = false;
	SourceLocation isInlineLoc = {}, isExternalLoc = {}, isExportedLoc = {};
	while (true)
	{
		if (threadData->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			if (isInline) LogError(context, threadData->token->loc, "'inline' used twice"_s);
			isInline = true;
			isInlineLoc = threadData->token->loc;
			Advance(context);
		}
		else if (threadData->token->type == TOKEN_DIRECTIVE_EXTERNAL)
		{
			if (isExternal) LogError(context, threadData->token->loc, "'external' used twice"_s);
			isExternal = true;
			isExternalLoc = threadData->token->loc;
			Advance(context);
		}
		else if (threadData->token->type == TOKEN_DIRECTIVE_EXPORT)
		{
			if (isExported) LogError(context, threadData->token->loc, "'export' used twice"_s);
			isExported = true;
			isExportedLoc = threadData->token->loc;
			Advance(context);
		}
		else
			break;
	}

	// Procedures!
	if (threadData->token->type == '(' ||
		threadData->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		expression.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;

		ASTProcedureDeclaration procDecl = {};
		procDecl.loc = threadData->token->loc;
		procDecl.name = result.name;
		procDecl.isInline = isInline;
		procDecl.isExternal = isExternal;
		procDecl.isExported = isExported;
		procDecl.prototype = ParseProcedurePrototype(context);

		if (threadData->token->type == ';')
			Advance(context);
		else
		{
			if (isExternal)
				LogError(context, threadData->token->loc, "External procedure declaration can't have a body"_s);
			procDecl.astBody = NewTreeNode(context);
			*procDecl.astBody = ParseStatement(context);
		}

		expression.procedureDeclaration = procDecl;
	}
	else
	{
		if (isInline)
			LogError(context, isInlineLoc, "'inline' specified for a non-procedure!"_s);
		if (isExternal)
			LogError(context, isExternalLoc, "'external' specified for a non-procedure!"_s);
		if (isExported)
			LogError(context, isExportedLoc, "'external' specified for a non-procedure!"_s);

		switch (threadData->token->type)
		{
		case TOKEN_KEYWORD_STRUCT:
		case TOKEN_KEYWORD_UNION:
		case TOKEN_KEYWORD_ENUM:
		{
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);

			AssertToken(context, threadData->token, ';');
			Advance(context);
		} break;
		case TOKEN_DIRECTIVE_TYPE:
		{
			Advance(context);
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);

			AssertToken(context, threadData->token, ';');
			Advance(context);
		} break;
		case TOKEN_DIRECTIVE_ALIAS:
		{
			Advance(context);
			expression.nodeType = ASTNODETYPE_ALIAS;
			expression.astType = ParseType(context);

			AssertToken(context, threadData->token, ';');
			Advance(context);
		} break;
		default:
		{
			expression = ParseExpression(context, -1);

			AssertToken(context, threadData->token, ';');
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
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_Unset;
	result.any.loc = threadData->token->loc;

	switch (threadData->token->type)
	{
	case '{':
	{
		result.nodeType = ASTNODETYPE_BLOCK;

		Advance(context);
		DynamicArrayInit(&result.block.statements, 512);
		while (threadData->token->type != '}')
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
		LogError(context, threadData->token->loc, "Invalid 'else' without matching 'if'"_s);
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

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_REMOVE:
	{
		result.nodeType = ASTNODETYPE_REMOVE;
		Advance(context);

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_BREAK:
	{
		result.nodeType = ASTNODETYPE_BREAK;
		Advance(context);

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		Advance(context);

		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_RETURN;
		if (threadData->token->type == ';')
			result.returnNode.expression = nullptr;
		else
		{
			result.returnNode.expression = NewTreeNode(context);
			*result.returnNode.expression = ParseExpression(context, -1);
		}

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_DEFER:
	{
		Advance(context);

		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_DEFER;
		result.deferNode.expression = NewTreeNode(context);
		*result.deferNode.expression = ParseExpression(context, -1);

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	case TOKEN_KEYWORD_UNION:
	{
		ASTVariableDeclaration varDecl = {};
		varDecl.loc = threadData->token->loc;

		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context); // This will parse the struct/union declaration.

		if (threadData->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			varDecl.astInitialValue = NewTreeNode(context);
			*varDecl.astInitialValue = ParseExpression(context, -1);
		}

		result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
		result.variableDeclaration = varDecl;

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_USING:
	{
		Advance(context);

		result.any.loc = threadData->token->loc;
		result.nodeType = ASTNODETYPE_USING;
		result.usingNode.expression = NewTreeNode(context);
		*result.usingNode.expression = ParseStatement(context);
	} break;
	default:
	{
		Token *next = &context->fileTokens[threadData->fileIdx][threadData->currentTokenIdx + 1];
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

			AssertToken(context, threadData->token, ';');
			Advance(context);
		}
		else
		{
			result = ParseExpression(context, -1);
			AssertToken(context, threadData->token, ';');
			Advance(context);
		}
	} break;
	}

	return result;
}

ASTExpression ParseStaticStatement(Context *context)
{
	ParseThreadData *threadData = (ParseThreadData *)TlsGetValue(context->tlsIndex);
	ASTExpression result = {};
	result.any.loc = threadData->token->loc;
	result.typeTableIdx = TYPETABLEIDX_Unset;

	switch (threadData->token->type)
	{
	case '{':
	{
		result.nodeType = ASTNODETYPE_BLOCK;

		Advance(context);
		DynamicArrayInit(&result.block.statements, 512);
		while (threadData->token->type != '}')
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

		enum TokenType op = threadData->token->type;
		if (op < TOKEN_OP_Begin || op > TOKEN_OP_End)
			UNEXPECTED_TOKEN_ERROR(context, threadData->token);
		Advance(context);

		AssertToken(context, threadData->token, TOKEN_OP_STATIC_DEF);
		Advance(context);

		bool isInline = false;
		if (threadData->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			isInline = true;
			Advance(context);
		}

		ASTOperatorOverload overload = {};
		overload.loc = threadData->token->loc;
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

		AssertToken(context, threadData->token, TOKEN_LITERAL_STRING);
		result.include.filename = TokenToString(context, *threadData->token);
		Advance(context);

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	case TOKEN_DIRECTIVE_LINKLIB:
	{
		result.nodeType = ASTNODETYPE_LINKLIB;
		Advance(context);

		AssertToken(context, threadData->token, TOKEN_LITERAL_STRING);
		result.linklib.filename = TokenToString(context, *threadData->token);
		Advance(context);

		AssertToken(context, threadData->token, ';');
		Advance(context);
	} break;
	default:
	{
		Token *next = &context->fileTokens[threadData->fileIdx][threadData->currentTokenIdx + 1];
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

			AssertToken(context, threadData->token, ';');
			Advance(context);
		}
		else
			LogError(context, threadData->token->loc, "Invalid expression in static context"_s);
	} break;
	}

	return result;
}

DWORD ParseJobProc(void *args)
{
	ParseJobArgs *argsStruct = (ParseJobArgs *)args;
	Context *context = argsStruct->context;
	u32 fileIdx = argsStruct->fileIdx;
	u32 jobIdx = argsStruct->fileIdx;

	ParseThreadData threadData;
	threadData.fileIdx = fileIdx;
	threadData.currentTokenIdx = 0;
	threadData.token = &context->fileTokens[fileIdx][0];
	TlsSetValue(context->tlsIndex, &threadData);

	MemoryInitThread(1 * 1024 * 1024);

#if !FINAL_BUILD
	HANDLE thread = GetCurrentThread();
	SourceFile sourceFile;
	{
		ScopedLockSpin filesLock(&context->filesLock);
		sourceFile = context->sourceFiles[fileIdx];
	}
	String threadName = TPrintF("P:%S", sourceFile.name);

	char buffer[512];
	char *dst = buffer;
	const char *src = threadName.data;
	for (int i = 0; i < threadName.size; ++i)
	{
		*dst++ = *src++;
		*dst++ = 0;
	}
	*dst++ = 0;
	*dst++ = 0;
	SetThreadDescription(thread, (PCWSTR)buffer);
#endif

	TokenizeFile(context, fileIdx);

	DynamicArray<ASTExpression, LinearAllocator> *statements =
		&context->fileASTRoots[fileIdx].block.statements;
	while (threadData.token->type != TOKEN_END_OF_FILE)
	{
		ASTExpression *statement = DynamicArrayAdd(statements);
		*statement = ParseStaticStatement(context);
		GenerateTypeCheckJobs(context, statement);
	}

	(*context->parseJobStates.Get())[jobIdx] = JOBSTATE_DONE;
	SYSFree(threadData.threadMem);
	return 0;
}

void ParserMain(Context *context)
{
	{
		ScopedLockSpin filesLock(&context->filesLock);
		DynamicArrayInit(&context->sourceFiles, 64);
		DynamicArrayInit(&context->fileASTRoots, 64);
		DynamicArrayInit(&context->fileTokens, 64);
		DynamicArrayInit(&context->fileTreeNodes, 64);
		DynamicArrayInit(&context->fileTypeNodes, 64);
	}
	DynamicArrayInit(&context->parseThreads.Get(), 64);
	DynamicArrayInit(&context->parseJobStates.Get(), 64);
}
