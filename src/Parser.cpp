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
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	BucketArray<Token, HeapAllocator, 1024> &fileTokens = context->fileTokens[jobData->fileIdx];
	ASSERT(jobData->token == &fileTokens[jobData->currentTokenIdx]);

	++jobData->currentTokenIdx;
	if (jobData->currentTokenIdx > BucketArrayCount(&fileTokens))
		LogError(context, jobData->token->loc, "Unexpected end of file"_s);
	jobData->token = &fileTokens[jobData->currentTokenIdx];
}

inline ASTExpression *NewTreeNode(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	auto treeNodes = context->fileTreeNodes[jobData->fileIdx].GetForWrite();
	return BucketArrayAdd(&treeNodes);
}

inline ASTType *NewASTType(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	auto typeNodes = context->fileTypeNodes[jobData->fileIdx].GetForWrite();
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
		ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
		switch (parseResult.error)
		{
		case PARSENUMBERRROR_OVERFLOW:
			LogError(context, jobData->token->loc, "Integer literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context, jobData->token->loc, "Integer literal too negative!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context, jobData->token->loc,
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
		ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
		switch (parseResult.error)
		{
		case PARSENUMBERRROR_OVERFLOW:
			LogError(context, jobData->token->loc, "Floating point literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context, jobData->token->loc, "Floating point literal too small!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context, jobData->token->loc,
					"Floating point literal contains invalid characters"_s);
		case PARSENUMBERRROR_INVALID_EXPONENT:
			LogError(context, jobData->token->loc,
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
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);

	ASTType astType;
	astType.loc = jobData->token->loc;

	switch (jobData->token->type)
	{
	case TOKEN_OP_ARRAY_ACCESS:
	{
		Advance(context);
		astType.nodeType = ASTTYPENODETYPE_ARRAY;

		astType.arrayCount = 0;
		if (jobData->token->type == TOKEN_LITERAL_NUMBER)
		{
			astType.arrayCount = ParseInt(context, TokenToString(context, *jobData->token));
			Advance(context);
		}
		AssertToken(context, jobData->token, ']');
		Advance(context);

		astType.arrayType = NewASTType(context);
		*astType.arrayType = ParseType(context);
	} break;
	case TOKEN_OP_POINTER_TO:
	{
		Advance(context);
		astType.nodeType = ASTTYPENODETYPE_POINTER;
		astType.pointedType = NewASTType(context);
		*astType.pointedType = ParseType(context);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	{
		astType.nodeType = ASTTYPENODETYPE_STRUCT_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(context);
	} break;
	case TOKEN_KEYWORD_UNION:
	{
		astType.nodeType = ASTTYPENODETYPE_UNION_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(context);
	} break;
	case TOKEN_KEYWORD_ENUM:
	{
		astType.nodeType = ASTTYPENODETYPE_ENUM_DECLARATION;
		astType.enumDeclaration = ParseEnumDeclaration(context);
	} break;
	case TOKEN_IDENTIFIER:
	{
		astType.nodeType = ASTTYPENODETYPE_IDENTIFIER;
		astType.name = TokenToString(context, *jobData->token);
		Advance(context);
	} break;
	case '(':
	case TOKEN_DIRECTIVE_CALLING_CONVENTION:
	{
		astType.nodeType = ASTTYPENODETYPE_PROCEDURE;
		astType.procedurePrototype = ParseProcedurePrototype(context);
	} break;
	default:
	{
		astType.nodeType = ASTTYPENODETYPE_INVALID;
		LogError(context, jobData->token->loc, "Failed to parse type"_s);
	}
	}

	return astType;
}

bool TryParseUnaryOperation(Context *context, s32 prevPrecedence, ASTUnaryOperation *result)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);

	if (!IsOperatorToken(jobData->token))
		return false;

	Token *oldToken = jobData->token;
	s64 oldTokenIdx = jobData->currentTokenIdx;

	result->loc = jobData->token->loc;

	switch (jobData->token->type)
	{
	case TOKEN_OP_POINTER_TO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	case TOKEN_OP_BITWISE_NOT:
	case TOKEN_OP_MINUS:
	{
		enum TokenType op = jobData->token->type;
		result->op = op;
		Advance(context);

		int precedenceOf = op == TOKEN_OP_MINUS ? PRECEDENCE_UNARY_SUBTRACT : op;
		s32 precedence = GetOperatorPrecedence(precedenceOf);
		result->expression = NewTreeNode(context);
		*result->expression = ParseExpression(context, precedence);

		return true;
	} break;
	}

	jobData->token = oldToken;
	jobData->currentTokenIdx = oldTokenIdx;
	return false;
}

bool TryParseBinaryOperation(Context *context, ASTExpression leftHand, s32 prevPrecedence,
		ASTBinaryOperation *result)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);

	if (!IsOperatorToken(jobData->token))
		return false;

	Token *oldToken = jobData->token;
	s64 oldTokenIdx = jobData->currentTokenIdx;

	result->loc = jobData->token->loc;

	switch (jobData->token->type)
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

			AssertToken(context, jobData->token, ']');
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

		enum TokenType op = jobData->token->type;
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
		String opStr = TokenTypeToString(jobData->token->type);
		LogError(context, jobData->token->loc, TPrintF("Unexpected operator %S", opStr));
	} break;
	}

	jobData->token = oldToken;
	jobData->currentTokenIdx = oldTokenIdx;
	return false;
}

ASTIf ParseIf(Context *context, bool onStaticContext)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASSERT(jobData->token->type == TOKEN_KEYWORD_IF ||
		   jobData->token->type == TOKEN_DIRECTIVE_IF);
	Advance(context);

	ASTIf ifNode = {};
	ifNode.loc = jobData->token->loc;

	ifNode.condition = NewTreeNode(context);
	if (jobData->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*ifNode.condition = ParseExpression(context, -1);
		AssertToken(context, jobData->token, ')');
		Advance(context);
	}
	else
		*ifNode.condition = ParseExpression(context, -1);

	ifNode.body = NewTreeNode(context);
	if (onStaticContext)
		*ifNode.body = ParseStaticStatement(context);
	else
		*ifNode.body = ParseStatement(context);

	if (jobData->token->type == TOKEN_KEYWORD_ELSE)
	{
		ifNode.elseLoc = jobData->token->loc;
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
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASSERT(jobData->token->type == TOKEN_KEYWORD_WHILE);

	ASTWhile whileNode = {};
	whileNode.loc = jobData->token->loc;
	Advance(context);

	whileNode.condition = NewTreeNode(context);
	if (jobData->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*whileNode.condition = ParseExpression(context, -1);
		AssertToken(context, jobData->token, ')');
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
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASSERT(jobData->token->type == TOKEN_KEYWORD_FOR);

	ASTFor forNode = {};
	forNode.loc = jobData->token->loc;
	forNode.indexVariableName = "i"_s;
	forNode.itemVariableName = "it"_s;
	Advance(context);

	bool closeParenthesis = false;
	forNode.range = NewTreeNode(context);
	if (jobData->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		closeParenthesis = true;
	}

	Token *oldToken = jobData->token;
	u64 oldTokenIdx = jobData->currentTokenIdx;

	Token first = *jobData->token;
	Advance(context);

	if (jobData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
	{
		Advance(context);
		if (first.type != TOKEN_IDENTIFIER)
			LogError(context, first.loc, "Expected name of index variable before ':' inside "
					"for loop range"_s);
		Advance(context);

		forNode.indexVariableName = TokenToString(context, first);
	}
	else if (jobData->token->type == ',')
	{
		Advance(context);
		Token second = *jobData->token;
		Advance(context);
		if (jobData->token->type == ',')
			LogError(context, jobData->token->loc, "Too many names in for loop condition, only up "
					"to 2 allowed (indexVar, itemVar : expr)"_s);
		AssertToken(context, jobData->token, TOKEN_OP_VARIABLE_DECLARATION);
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
		jobData->token = oldToken;
		jobData->currentTokenIdx = oldTokenIdx;
	}

	*forNode.range = ParseExpression(context, -1);

	if (closeParenthesis)
	{
		AssertToken(context, jobData->token, ')');
		Advance(context);
	}

	forNode.body = NewTreeNode(context);
	*forNode.body = ParseStatement(context);

	return forNode;
}

ASTStructMemberDeclaration ParseStructMemberDeclaration(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTStructMemberDeclaration structMem = {};
	structMem.loc = jobData->token->loc;

	if (jobData->token->type == TOKEN_KEYWORD_USING)
	{
		structMem.isUsing = true;
		Advance(context);
	}

	// Anonymous structs/unions
	if (jobData->token->type == TOKEN_KEYWORD_STRUCT || 
		jobData->token->type == TOKEN_KEYWORD_UNION)
	{
	}
	else
	{
		AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
		structMem.name = TokenToString(context, *jobData->token);
		Advance(context);

		AssertToken(context, jobData->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(context);
	}

	if (jobData->token->type != TOKEN_OP_ASSIGNMENT)
	{
		structMem.astType = NewASTType(context);
		*structMem.astType = ParseType(context);
		if (structMem.astType->nodeType == ASTTYPENODETYPE_INVALID)
			LogError(context, jobData->token->loc, "Expected type"_s);
	}

	if (jobData->token->type == TOKEN_OP_ASSIGNMENT)
	{
		Advance(context);
		structMem.value = NewTreeNode(context);
		*structMem.value = ParseExpression(context, -1);
	}

	return structMem;
}

ASTEnumDeclaration ParseEnumDeclaration(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	SourceLocation loc = jobData->token->loc;

	AssertToken(context, jobData->token, TOKEN_KEYWORD_ENUM);
	Advance(context);

	ASTEnumDeclaration enumNode = {};
	enumNode.loc = loc;
	DynamicArrayInit(&enumNode.members, 16);

	if (jobData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
	{
		Advance(context);
		enumNode.astType = NewASTType(context);
		*enumNode.astType = ParseType(context);
	}

	AssertToken(context, jobData->token, '{');
	Advance(context);
	while (jobData->token->type != '}')
	{
		ASTEnumMember enumMember = {};

		AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
		enumMember.name = TokenToString(context, *jobData->token);
		enumMember.loc = jobData->token->loc;
		Advance(context);

		if (jobData->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			enumMember.value = NewTreeNode(context);
			*enumMember.value = ParseExpression(context, GetOperatorPrecedence(',') + 1);
		}

		if (jobData->token->type != '}')
		{
			AssertToken(context, jobData->token, ',');
			Advance(context);
		}

		*DynamicArrayAdd(&enumNode.members) = enumMember;
	}
	Advance(context);

	return enumNode;
}

ASTStructDeclaration ParseStructOrUnion(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	SourceLocation loc = jobData->token->loc;

	ASSERT(jobData->token->type == TOKEN_KEYWORD_STRUCT ||
			jobData->token->type == TOKEN_KEYWORD_UNION);
	Advance(context);

	ASTStructDeclaration structDeclaration = {};
	structDeclaration.loc = loc;
	DynamicArrayInit(&structDeclaration.members, 16);

	AssertToken(context, jobData->token, '{');
	Advance(context);
	while (jobData->token->type != '}')
	{
		ASTStructMemberDeclaration member = ParseStructMemberDeclaration(context);
		*DynamicArrayAdd(&structDeclaration.members) = member;

		AssertToken(context, jobData->token, ';');
		Advance(context);
	}
	Advance(context);

	return structDeclaration;
}

Array<ASTExpression *, LinearAllocator> ParseGroupLiteral(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	DynamicArray<ASTExpression *, LinearAllocator> members;
	DynamicArrayInit(&members, 8);

	while (true)
	{
		ASTExpression *newTreeNode = NewTreeNode(context);
		*newTreeNode = ParseExpression(context, GetOperatorPrecedence(',') + 1);
		*DynamicArrayAdd(&members) = newTreeNode;

		if (jobData->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			ASTExpression assignment = { ASTNODETYPE_BINARY_OPERATION };
			assignment.typeTableIdx = TYPETABLEIDX_Unset;
			assignment.binaryOperation.op = TOKEN_OP_ASSIGNMENT;
			assignment.binaryOperation.leftHand = NewTreeNode(context);
			*assignment.binaryOperation.leftHand = *newTreeNode;
			assignment.binaryOperation.rightHand = NewTreeNode(context);
			*assignment.binaryOperation.rightHand = ParseExpression(context,
					GetOperatorPrecedence(TOKEN_OP_ASSIGNMENT));
			*newTreeNode = assignment;
		}

		if (jobData->token->type == '}')
			break;
		if (jobData->token->type == ',')
		{
			Advance(context);
			continue;
		}

		String tokenStr = TokenTypeToString(jobData->token->type);
		LogError(context, jobData->token->loc, TPrintF("Parsing struct literal. Expected ',' or '}' but got %S", tokenStr));
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
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTVariableDeclaration varDecl = {};
	varDecl.loc = jobData->token->loc;

	AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
	varDecl.name = TokenToString(context, *jobData->token);
	Advance(context);

	if (jobData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else if (jobData->token->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
	{
		varDecl.isStatic = true;
		Advance(context);
	}
	else
		UNEXPECTED_TOKEN_ERROR(context, jobData->token);

	if (jobData->token->type == TOKEN_DIRECTIVE_EXTERNAL)
	{
		varDecl.isExternal = true;
		Advance(context);
	}

	if (jobData->token->type != TOKEN_OP_ASSIGNMENT)
	{
		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context);
	}

	if (jobData->token->type == TOKEN_OP_ASSIGNMENT)
	{
		if (varDecl.isExternal)
			LogError(context, jobData->token->loc, "Can't assign value to external variable"_s);

		Advance(context);
		varDecl.astInitialValue = NewTreeNode(context);
		*varDecl.astInitialValue = ParseExpression(context, -1);
	}

	return varDecl;
}

ASTProcedureParameter ParseProcedureParameter(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTProcedureParameter astParameter = {};
	astParameter.loc = jobData->token->loc;

	if (jobData->token->type == TOKEN_KEYWORD_USING)
	{
		astParameter.isUsing = true;
		Advance(context);
	}

	u64 startTokenIdx = jobData->currentTokenIdx;

	AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
	astParameter.name = TokenToString(context, *jobData->token);
	Advance(context);

	if (jobData->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else
	{
		// Nameless parameter, rewind
		jobData->currentTokenIdx = startTokenIdx;
		jobData->token = &context->fileTokens[jobData->fileIdx][startTokenIdx];
		astParameter.name = {};
	}

	if (jobData->token->type != TOKEN_OP_ASSIGNMENT)
	{
		astParameter.astType = NewASTType(context);
		*astParameter.astType = ParseType(context);
	}

	if (jobData->token->type == TOKEN_OP_ASSIGNMENT)
	{
		Advance(context);
		astParameter.astInitialValue = NewTreeNode(context);
		*astParameter.astInitialValue = ParseExpression(context, GetOperatorPrecedence(',') + 1);
	}

	return astParameter;
}

ASTProcedurePrototype ParseProcedurePrototype(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTProcedurePrototype astPrototype = {};
	astPrototype.loc = jobData->token->loc;
	astPrototype.callingConvention = CC_DEFAULT;

	if (jobData->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		Advance(context);
		AssertToken(context, jobData->token, '(');
		Advance(context);

		AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
		String tokenStr = TokenToString(context, *jobData->token);
		if (StringEquals(tokenStr, "win64"_s))
			astPrototype.callingConvention = CC_WIN64;
		else if (StringEquals(tokenStr, "linux64"_s))
			astPrototype.callingConvention = CC_LINUX64;
		else
			LogError(context, jobData->token->loc, "Invalid calling convention specified"_s);
		Advance(context);

		AssertToken(context, jobData->token, ')');
		Advance(context);
	}

	DynamicArrayInit(&astPrototype.astParameters, 4);

	AssertToken(context, jobData->token, '(');
	Advance(context);
	while (jobData->token->type != ')')
	{
		if (jobData->token->type == TOKEN_OP_RANGE)
		{
			Advance(context);
			astPrototype.isVarargs = true;
			astPrototype.varargsLoc = jobData->token->loc;

			if (jobData->token->type == TOKEN_IDENTIFIER)
			{
				astPrototype.varargsName = TokenToString(context, *jobData->token);
				Advance(context);
			}
			break;
		}

		ASTProcedureParameter astParam = ParseProcedureParameter(context);
		ASSERT(astPrototype.astParameters.size <= S8_MAX);

		*DynamicArrayAdd(&astPrototype.astParameters) = astParam;

		if (jobData->token->type != ')')
		{
			AssertToken(context, jobData->token, ',');
			Advance(context);
		}
	}
	Advance(context);

	if (jobData->token->type == TOKEN_OP_ARROW)
	{
		Advance(context);
		DynamicArrayInit(&astPrototype.astReturnTypes, 4);

loop:
		ASTType *newASTType = NewASTType(context);
		*newASTType = ParseType(context);
		*DynamicArrayAdd(&astPrototype.astReturnTypes) = newASTType;
		if (jobData->token->type == ',')
		{
			Advance(context);
			goto loop;
		}
	}

	return astPrototype;
}

ASTExpression ParseExpression(Context *context, s32 precedence)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_Unset;
	result.any.loc = jobData->token->loc;

	switch (jobData->token->type)
	{
	// Parenthesis
	case '(':
	{
		Advance(context);

		result = ParseExpression(context, -1);

		AssertToken(context, jobData->token, ')');
		Advance(context);
	} break;
	case '{':
	{
		Advance(context);

		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		result.literal.type = LITERALTYPE_GROUP;
		result.literal.members = ParseGroupLiteral(context);

		AssertToken(context, jobData->token, '}');
		Advance(context);
	} break;
	case '?':
	{
		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_GARBAGE;
		Advance(context);
	} break;
	case TOKEN_IDENTIFIER:
	{
		result.any.loc = jobData->token->loc;
		String identifier = TokenToString(context, *jobData->token);
		Advance(context);

		if (jobData->token->type == '(')
		{
			// Procedure call
			result.nodeType = ASTNODETYPE_PROCEDURE_CALL;
			result.procedureCall.name = identifier;
			HybridArrayInit(&result.procedureCall.arguments);

			// Parse arguments
			Advance(context);
			while (jobData->token->type != ')')
			{
				ASTExpression *arg = NewTreeNode(context);
				*arg = ParseExpression(context, GetOperatorPrecedence(',') + 1);
				*HybridArrayAdd(&result.procedureCall.arguments) = arg;

				if (jobData->token->type != ')')
				{
					if (jobData->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(context, *jobData->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(context, jobData->token->loc, errorStr);
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
		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		String tokenStr = TokenToString(context, *jobData->token);

		bool isHex = false;
		bool isFloating = false;
		if (tokenStr.data[0] == '0')
		{
			if (tokenStr.data[1] == 'x' || tokenStr.data[1] == 'X')
				isHex = true;
		}

		for (u32 i = 0; i < jobData->token->size; ++i)
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
		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_CHARACTER;
		result.literal.character = TokenToString(context, *jobData->token).data[0];
		Advance(context);
	} break;
	case TOKEN_LITERAL_STRING:
	{
		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = TokenToString(context, *jobData->token);
		Advance(context);
	} break;
	case TOKEN_KEYWORD_TYPEOF:
	{
		result.any.loc = jobData->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_TYPEOF;
		result.typeOfNode.expression = NewTreeNode(context);
		*result.typeOfNode.expression = ParseExpression(context, precedence);
	} break;
	case TOKEN_KEYWORD_SIZEOF:
	{
		result.any.loc = jobData->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_SIZEOF;
		result.sizeOfNode.expression = NewTreeNode(context);
		*result.sizeOfNode.expression = ParseExpression(context, precedence);
	} break;
	case TOKEN_DIRECTIVE_DEFINED:
	{
		result.any.loc = jobData->token->loc;
		Advance(context);

		AssertToken(context, jobData->token, '(');
		Advance(context);

		result.nodeType = ASTNODETYPE_DEFINED;
		AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
		result.definedNode.identifier = TokenToString(context, *jobData->token);
		Advance(context);

		AssertToken(context, jobData->token, ')');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_CAST:
	{
		Advance(context);
		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_CAST;

		AssertToken(context, jobData->token, '(');
		Advance(context);

		result.castNode.astType = ParseType(context);

		AssertToken(context, jobData->token, ')');
		Advance(context);

		result.castNode.expression = NewTreeNode(context);
		int castPrecedence = GetOperatorPrecedence(TOKEN_KEYWORD_CAST);
		*result.castNode.expression = ParseExpression(context, castPrecedence);
	} break;
	case TOKEN_DIRECTIVE_INTRINSIC:
	{
		Advance(context);
		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_INTRINSIC;

		AssertToken(context, jobData->token, '(');
		Advance(context);

		AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
		result.intrinsic.name = TokenToString(context, *jobData->token);
		Advance(context);

		if (jobData->token->type == ',')
		{
			// Parse arguments
			Advance(context);
			DynamicArrayInit(&result.intrinsic.arguments, 4);
			while (jobData->token->type != ')')
			{
				ASTExpression arg = ParseExpression(context, GetOperatorPrecedence(',') + 1);
				*DynamicArrayAdd(&result.intrinsic.arguments) = arg;

				if (jobData->token->type != ')')
				{
					if (jobData->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(context, *jobData->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(context, jobData->token->loc, errorStr);
					}
					Advance(context);
				}
			}
		}

		AssertToken(context, jobData->token, ')');
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
		LogError(context, jobData->token->loc, "'if' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_WHILE:
	{
		LogError(context, jobData->token->loc, "'while' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_FOR:
	{
		LogError(context, jobData->token->loc, "'for' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_DEFER:
	{
		LogError(context, jobData->token->loc, "'defer' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		LogError(context, jobData->token->loc, "'return' only valid at statement level!"_s);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	{
		LogError(context, jobData->token->loc, "'struct' not valid on this context!"_s);
	} break;
	case TOKEN_KEYWORD_UNION:
	{
		LogError(context, jobData->token->loc, "'union' not valid on this context!"_s);
	} break;
	default:
	{
		if (!IsOperatorToken(jobData->token))
			UNEXPECTED_TOKEN_ERROR(context, jobData->token);
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
				LogError(context, jobData->token->loc, "Invalid expression!"_s);
			result.nodeType = ASTNODETYPE_UNARY_OPERATION;
			result.unaryOperation = unaryOp;
			continue;
		}
		else
		{
			ASTBinaryOperation binaryOp = result.binaryOperation;
			bool success = TryParseBinaryOperation(context, result, precedence, &binaryOp);
			if (success)
			{
				result.nodeType = ASTNODETYPE_BINARY_OPERATION;
				result.binaryOperation = binaryOp;
				continue;
			}
		}

		if (jobData->token->type == ',' && GetOperatorPrecedence(',') >= precedence)
		{
			Advance(context);

			ASTExpression mux;
			mux.any.loc = result.any.loc;
			mux.typeTableIdx = TYPETABLEIDX_Unset;
			mux.nodeType = ASTNODETYPE_MULTIPLE_EXPRESSIONS;

			ASTExpression *first = NewTreeNode(context);
			*first = result;

			ASTExpression second = ParseExpression(context, GetOperatorPrecedence(','));
			if (second.nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS)
			{
				DynamicArrayInit(&mux.multipleExpressions.array, second.multipleExpressions.array.size + 1);
				*DynamicArrayAdd(&mux.multipleExpressions.array) = first;
				for (int i = 0; i < second.multipleExpressions.array.size; ++i)
					*DynamicArrayAdd(&mux.multipleExpressions.array) = second.multipleExpressions.array[i];
			}
			else
			{
				DynamicArrayInit(&mux.multipleExpressions.array, 2);
				ASTExpression *sec = NewTreeNode(context);
				*sec = second;
				*DynamicArrayAdd(&mux.multipleExpressions.array) = first;
				*DynamicArrayAdd(&mux.multipleExpressions.array) = sec;
			}

			result = mux;
		}
		else
			break;
	}

	return result;
}

ASTStaticDefinition ParseStaticDefinition(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTStaticDefinition result = {};

	AssertToken(context, jobData->token, TOKEN_IDENTIFIER);
	result.name = TokenToString(context, *jobData->token);
	result.loc = jobData->token->loc;
	Advance(context);

	Advance(context);

	ASTExpression expression = {};
	expression.typeTableIdx = TYPETABLEIDX_Unset;
	expression.any.loc = jobData->token->loc;

	bool isInline = false;
	bool isExternal = false;
	bool isExported = false;
	SourceLocation isInlineLoc = {}, isExternalLoc = {}, isExportedLoc = {};
	while (true)
	{
		if (jobData->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			if (isInline) LogError(context, jobData->token->loc, "'inline' used twice"_s);
			isInline = true;
			isInlineLoc = jobData->token->loc;
			Advance(context);
		}
		else if (jobData->token->type == TOKEN_DIRECTIVE_EXTERNAL)
		{
			if (isExternal) LogError(context, jobData->token->loc, "'external' used twice"_s);
			isExternal = true;
			isExternalLoc = jobData->token->loc;
			Advance(context);
		}
		else if (jobData->token->type == TOKEN_DIRECTIVE_EXPORT)
		{
			if (isExported) LogError(context, jobData->token->loc, "'export' used twice"_s);
			isExported = true;
			isExportedLoc = jobData->token->loc;
			Advance(context);
		}
		else
			break;
	}

	// Procedures!
	if (jobData->token->type == '(' ||
		jobData->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		expression.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;

		ASTProcedureDeclaration procDecl = {};
		procDecl.loc = jobData->token->loc;
		procDecl.name = result.name;
		procDecl.isInline = isInline;
		procDecl.isExternal = isExternal;
		procDecl.isExported = isExported;
		procDecl.prototype = ParseProcedurePrototype(context);

		if (jobData->token->type == ';')
			Advance(context);
		else
		{
			if (isExternal)
				LogError(context, jobData->token->loc, "External procedure declaration can't have a body"_s);
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

		switch (jobData->token->type)
		{
		case TOKEN_KEYWORD_STRUCT:
		case TOKEN_KEYWORD_UNION:
		case TOKEN_KEYWORD_ENUM:
		{
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);

			AssertToken(context, jobData->token, ';');
			Advance(context);
		} break;
		case TOKEN_DIRECTIVE_TYPE:
		{
			Advance(context);
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);

			AssertToken(context, jobData->token, ';');
			Advance(context);
		} break;
		case TOKEN_DIRECTIVE_ALIAS:
		{
			Advance(context);
			expression.nodeType = ASTNODETYPE_ALIAS;
			expression.astType = ParseType(context);

			AssertToken(context, jobData->token, ';');
			Advance(context);
		} break;
		default:
		{
			expression = ParseExpression(context, -1);

			AssertToken(context, jobData->token, ';');
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
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_Unset;
	result.any.loc = jobData->token->loc;

	switch (jobData->token->type)
	{
	case '{':
	{
		result.nodeType = ASTNODETYPE_BLOCK;

		Advance(context);
		DynamicArrayInit(&result.block.statements, 512);
		while (jobData->token->type != '}')
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
		LogError(context, jobData->token->loc, "Invalid 'else' without matching 'if'"_s);
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

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_REMOVE:
	{
		result.nodeType = ASTNODETYPE_REMOVE;
		Advance(context);

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_BREAK:
	{
		result.nodeType = ASTNODETYPE_BREAK;
		Advance(context);

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_RETURN:
	{
		Advance(context);

		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_RETURN;
		if (jobData->token->type == ';')
			result.returnNode.expression = nullptr;
		else
		{
			result.returnNode.expression = NewTreeNode(context);
			*result.returnNode.expression = ParseExpression(context, -1);
		}

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_DEFER:
	{
		Advance(context);

		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_DEFER;
		result.deferNode.expression = NewTreeNode(context);
		*result.deferNode.expression = ParseExpression(context, -1);

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	case TOKEN_KEYWORD_UNION:
	{
		ASTVariableDeclaration varDecl = {};
		varDecl.loc = jobData->token->loc;

		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context); // This will parse the struct/union declaration.

		if (jobData->token->type == TOKEN_OP_ASSIGNMENT)
		{
			Advance(context);
			varDecl.astInitialValue = NewTreeNode(context);
			*varDecl.astInitialValue = ParseExpression(context, -1);
		}

		result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
		result.variableDeclaration = varDecl;

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_USING:
	{
		Advance(context);

		result.any.loc = jobData->token->loc;
		result.nodeType = ASTNODETYPE_USING;
		result.usingNode.expression = NewTreeNode(context);
		*result.usingNode.expression = ParseStatement(context);
	} break;
	default:
	{
		Token *next = &context->fileTokens[jobData->fileIdx][jobData->currentTokenIdx + 1];
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

			AssertToken(context, jobData->token, ';');
			Advance(context);
		}
		else
		{
			result = ParseExpression(context, -1);
			AssertToken(context, jobData->token, ';');
			Advance(context);
		}
	} break;
	}

	return result;
}

ASTExpression ParseStaticStatement(Context *context)
{
	ParseJobData *jobData = (ParseJobData *)SYSGetFiberData(context->flsIndex);
	ASTExpression result = {};
	result.any.loc = jobData->token->loc;
	result.typeTableIdx = TYPETABLEIDX_Unset;

	switch (jobData->token->type)
	{
	case '{':
	{
		result.nodeType = ASTNODETYPE_BLOCK;

		Advance(context);
		DynamicArrayInit(&result.block.statements, 512);
		while (jobData->token->type != '}')
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

		enum TokenType op = jobData->token->type;
		if (op < TOKEN_OP_Begin || op > TOKEN_OP_End)
			UNEXPECTED_TOKEN_ERROR(context, jobData->token);
		Advance(context);

		AssertToken(context, jobData->token, TOKEN_OP_STATIC_DEF);
		Advance(context);

		bool isInline = false;
		if (jobData->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			isInline = true;
			Advance(context);
		}

		ASTOperatorOverload overload = {};
		overload.loc = result.any.loc;
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

		AssertToken(context, jobData->token, TOKEN_LITERAL_STRING);
		result.include.filename = TokenToString(context, *jobData->token);
		Advance(context);

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	case TOKEN_DIRECTIVE_LINKLIB:
	{
		result.nodeType = ASTNODETYPE_LINKLIB;
		Advance(context);

		AssertToken(context, jobData->token, TOKEN_LITERAL_STRING);
		result.linklib.filename = TokenToString(context, *jobData->token);
		Advance(context);

		AssertToken(context, jobData->token, ';');
		Advance(context);
	} break;
	default:
	{
		Token *next = &context->fileTokens[jobData->fileIdx][jobData->currentTokenIdx + 1];
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

			AssertToken(context, jobData->token, ';');
			Advance(context);
		}
		else
			LogError(context, jobData->token->loc, "Invalid expression in static context"_s);
	} break;
	}

	return result;
}

void ParseJobProc(void *args)
{
	ParseJobArgs *argsStruct = (ParseJobArgs *)args;
	Context *context = argsStruct->context;
	u32 fileIdx = argsStruct->fileIdx;
	u32 jobIdx = argsStruct->jobIdx;

	ThreadDataCommon *threadData = (ThreadDataCommon *)SYSGetThreadData(context->tlsIndex);
	if (threadData->lastJobIdx != U32_MAX)
	{
		auto jobs = context->jobs.Get();
		ASSERT((*jobs)[threadData->lastJobIdx].isRunning);
		(*jobs)[threadData->lastJobIdx].isRunning = 0;
		threadData->lastJobIdx = U32_MAX;
	}

	ParseJobData jobData;
	jobData.jobIdx = jobIdx;
	jobData.fileIdx = fileIdx;
	jobData.currentTokenIdx = 0;
	jobData.token = &context->fileTokens[fileIdx][0];
	SYSSetFiberData(context->flsIndex, &jobData);

	MemoryInitJob(1 * 1024 * 1024);

	{
		auto jobs = context->jobs.Get();
		ASSERT((*jobs)[jobIdx].isRunning);
		(*jobs)[jobIdx].state = JOBSTATE_RUNNING;

#if !FINAL_BUILD
		(*jobs)[jobIdx].title = SStringConcat("P:"_s, context->sourceFiles[fileIdx].name);
#endif
	}

	TokenizeFile(context, fileIdx);

	DynamicArray<ASTExpression, LinearAllocator> *statements =
		&context->fileASTRoots[fileIdx].block.statements;
	while (jobData.token->type != TOKEN_END_OF_FILE)
	{
		ASTExpression *statement = DynamicArrayAdd(statements);
		*statement = ParseStaticStatement(context);
		GenerateTypeCheckJobs(context, statement);
	}

	{
		auto jobs = context->jobs.Get();
		(*jobs)[jobIdx].state = JOBSTATE_DONE;
	}
	SYSFree(jobData.jobMem);
	SwitchJob(context);
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
	{
		auto jobs = context->jobs.Get();
		DynamicArrayInit(&jobs, 64);
	}
}
