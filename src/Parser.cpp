ASTExpression ParseExpression(PContext *pContext, s32 precedence, bool isStatement);

void AssertToken(Token *token, int type)
{
	if (token->type != type)
	{
		String tokenTypeGot = TokenToStringOrType(*token);
		String tokenTypeExp = TokenTypeToString(type);
		String errorStr = TPrintF("Expected token of type %S but got %S",
				tokenTypeExp, tokenTypeGot);
		LogError(token->loc, errorStr);
	}
}

#define UNEXPECTED_TOKEN_ERROR(TOKEN) do { \
	String tokenString = TokenTypeToString(TOKEN->type); \
	LogError(TOKEN->loc, TPrintF("Unexpected token of type %S", tokenString)); \
	} while (0)

void Advance(PContext *pContext)
{
	ASSERT(pContext->token == &pContext->tokens[pContext->currentTokenIdx]);

	++pContext->currentTokenIdx;
	if (pContext->currentTokenIdx > pContext->tokens.count)
		LogError(pContext->token->loc, "Unexpected end of file"_s);
	pContext->token = &pContext->tokens[pContext->currentTokenIdx];
}

inline ASTExpression *PNewTreeNode(PContext *pContext)
{
	return BucketArrayAdd(&pContext->astTreeNodes);
}

inline ASTType *PNewASTType(PContext *pContext)
{
	return BucketArrayAdd(&pContext->astTypes);
}

inline String *GetVariableName(ASTVariableDeclaration *astVarDecl, u32 idx)
{
	ASSERT(idx < astVarDecl->nameCount);
	if (astVarDecl->nameCount == 1)
		return &astVarDecl->name;
	else
		return &astVarDecl->arrayOfNames[idx];
}

inline u32 *GetVariableTypeIdx(ASTVariableDeclaration *astVarDecl, u32 idx)
{
	ASSERT(idx < astVarDecl->nameCount);
	if (astVarDecl->nameCount == 1)
		return &astVarDecl->typeIdx;
	else
		return &astVarDecl->arrayOfTypeIndices[idx];
}

inline u32 *GetVariableValueIdx(ASTVariableDeclaration *astVarDecl, u32 idx)
{
	ASSERT(idx < astVarDecl->nameCount);
	if (astVarDecl->nameCount == 1)
		return &astVarDecl->valueIdx;
	else
		return &astVarDecl->arrayOfValueIndices[idx];
}

inline String *GetVariableName(ASTStaticDefinition *astStaticDef, u32 idx)
{
	ASSERT(idx < astStaticDef->nameCount);
	if (astStaticDef->nameCount == 1)
		return &astStaticDef->name;
	else
		return &astStaticDef->arrayOfNames[idx];
}

inline s64 ParseInt(PContext *pContext, String str)
{
	bool isHex = false;
	if (str.data[0] == '0') {
		if (str.data[1] == 'x' || str.data[1] == 'X')
			isHex = true;
	}

	ParseNumberResult parseResult;
	if (isHex) {
		String numbersOnly = { str.size - 2, str.data + 2 };
		parseResult = IntFromStringHex(numbersOnly);
	}
	else
		parseResult = IntFromString(str);

	if (parseResult.error) {
		switch (parseResult.error) {
		case PARSENUMBERRROR_OVERFLOW:
			LogError(pContext->token->loc, "Integer literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(pContext->token->loc, "Integer literal too negative!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(pContext->token->loc,
					"Integer literal contains invalid characters"_s);
			break;
		}
	}
	return parseResult.number;
}

inline f64 ParseFloat(PContext *pContext, String str)
{
	ParseFloatResult parseResult = F64FromString(str);
	if (parseResult.error) {
		switch (parseResult.error) {
		case PARSENUMBERRROR_OVERFLOW:
			LogError(pContext->token->loc, "Floating point literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(pContext->token->loc, "Floating point literal too small!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(pContext->token->loc,
					"Floating point literal contains invalid characters"_s);
		case PARSENUMBERRROR_INVALID_EXPONENT:
			LogError(pContext->token->loc,
					"Could not parse exponent in scientific notation"_s);
			break;
		}
	}
	return parseResult.number;
}

ASTStructDeclaration ParseStructOrUnion(PContext *pContext);
ASTEnumDeclaration ParseEnumDeclaration(PContext *pContext);
ASTProcedurePrototype ParseProcedurePrototype(PContext *pContext);
ASTType ParseType(PContext *pContext)
{
	ASTType astType;
	astType.loc = pContext->token->loc;

	switch (pContext->token->type) {
	case TOKEN_OP_ARRAY_ACCESS:
	{
		Advance(pContext);
		astType.nodeType = ASTTYPENODETYPE_ARRAY;

		astType.arrayCountExp = nullptr;
		if (pContext->token->type != ']') {
			astType.arrayCountExp = PNewTreeNode(pContext);
			*astType.arrayCountExp = ParseExpression(pContext, -1, false);
		}
		AssertToken(pContext->token, ']');
		Advance(pContext);

		astType.arrayType = PNewASTType(pContext);
		*astType.arrayType = ParseType(pContext);
	} break;
	case TOKEN_OP_POINTER_TO:
	{
		Advance(pContext);
		astType.nodeType = ASTTYPENODETYPE_POINTER;
		astType.pointedType = PNewASTType(pContext);
		*astType.pointedType = ParseType(pContext);
	} break;
	case TOKEN_KEYWORD_STRUCT:
	{
		astType.nodeType = ASTTYPENODETYPE_STRUCT_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(pContext);
	} break;
	case TOKEN_KEYWORD_UNION:
	{
		astType.nodeType = ASTTYPENODETYPE_UNION_DECLARATION;
		astType.structDeclaration = ParseStructOrUnion(pContext);
	} break;
	case TOKEN_KEYWORD_ENUM:
	{
		astType.nodeType = ASTTYPENODETYPE_ENUM_DECLARATION;
		astType.enumDeclaration = ParseEnumDeclaration(pContext);
	} break;
	case TOKEN_IDENTIFIER:
	{
		astType.nodeType = ASTTYPENODETYPE_IDENTIFIER;
		astType.name = TokenToString(*pContext->token);
		Advance(pContext);
	} break;
	case '(':
	case TOKEN_DIRECTIVE_CALLING_CONVENTION:
	{
		astType.nodeType = ASTTYPENODETYPE_PROCEDURE;
		astType.procedurePrototype = ParseProcedurePrototype(pContext);
	} break;
	case '$':
	{
		Advance(pContext);
		astType.nodeType = ASTTYPENODETYPE_TYPE_ARGUMENT_DECLARATION;
		astType.name = TokenToString(*pContext->token);
		Advance(pContext);
	} break;
	default:
	{
		astType.nodeType = ASTTYPENODETYPE_INVALID;
		LogError(pContext->token->loc, "Failed to parse type"_s);
	}
	}

	return astType;
}

const int PRECEDENCE_UNARY_SUBTRACT = TOKEN_OP_End+1; // @Hack

constexpr int GetOperatorPrecedence(s32 op)
{
	// Even means evaluated left to right with things of same precedence.
	// Odd means evaluated right to left with things of same precedence.
	if (op >= TOKEN_OP_ASSIGNMENT_Begin && op <= TOKEN_OP_ASSIGNMENT_End)
		return 1;
	else switch (op)
	{
		case TOKEN_OP_STATIC_DEF:
		case TOKEN_OP_VARIABLE_DECLARATION:
		case TOKEN_OP_VARIABLE_DECLARATION_STATIC:
			return 1;
		case ',':
			return 2;
		case TOKEN_OP_AND:
		case TOKEN_OP_OR:
			return 4;
		case TOKEN_OP_EQUALS:
		case TOKEN_OP_NOT_EQUALS:
		case TOKEN_OP_GREATER_THAN:
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
		case TOKEN_OP_LESS_THAN:
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
		case TOKEN_OP_RANGE:
			return 6;
		case TOKEN_OP_PLUS:
		case TOKEN_OP_MINUS:
			return 8;
		case TOKEN_OP_MULTIPLY:
		case TOKEN_OP_DIVIDE:
		case TOKEN_OP_MODULO:
			return 10;
		case TOKEN_OP_SHIFT_LEFT:
		case TOKEN_OP_SHIFT_RIGHT:
			return 12;
		case TOKEN_OP_BITWISE_AND:
		case TOKEN_OP_BITWISE_OR:
			return 14;
		case TOKEN_OP_NOT:
		case TOKEN_OP_BITWISE_NOT:
		case PRECEDENCE_UNARY_SUBTRACT:
			return 16;
		case TOKEN_OP_POINTER_TO:
		case TOKEN_OP_DEREFERENCE:
			return 18;
		case TOKEN_KEYWORD_CAST:
			return 20;
		case '(':
			return 22;
		case TOKEN_OP_ARRAY_ACCESS:
		case TOKEN_OP_MEMBER_ACCESS:
			return 24;
		default:
			ASSERT(false);
	}
}

ASTStaticDefinition ParseStaticDefinition(PContext *pContext, ArrayView<String> names)
{
	ASTStaticDefinition astStaticDef = {};
	astStaticDef.loc = pContext->token->loc;

	astStaticDef.nameCount = (u32)names.size;
	if (names.size == 1)
		astStaticDef.name = names[0];
	else {
		ASSERT(names.size > 1);
		String *arrayOfNames = (String *)LinearAllocator::Alloc(sizeof(String) * names.size,
				alignof(String));
		// Copy
		for (int i = 0; i < names.size; ++i)
			arrayOfNames[i] = names[i];
		astStaticDef.arrayOfNames = arrayOfNames;
	}

	ASTExpression expression = {};
	expression.typeTableIdx = TYPETABLEIDX_Unset;
	expression.any.loc = pContext->token->loc;

	bool isInline = false;
	bool isExternal = false;
	bool isExported = false;
	SourceLocation isInlineLoc = {}, isExternalLoc = {}, isExportedLoc = {};
	while (true) {
		if (pContext->token->type == TOKEN_DIRECTIVE_INLINE) {
			if (isInline) LogError(pContext->token->loc, "'inline' used twice"_s);
			isInline = true;
			isInlineLoc = pContext->token->loc;
			Advance(pContext);
		}
		else if (pContext->token->type == TOKEN_DIRECTIVE_EXTERNAL) {
			if (isExternal) LogError(pContext->token->loc, "'external' used twice"_s);
			isExternal = true;
			isExternalLoc = pContext->token->loc;
			Advance(pContext);
		}
		else if (pContext->token->type == TOKEN_DIRECTIVE_EXPORT) {
			if (isExported) LogError(pContext->token->loc, "'export' used twice"_s);
			isExported = true;
			isExportedLoc = pContext->token->loc;
			Advance(pContext);
		}
		else
			break;
	}

	// Procedures!
	if (pContext->token->type == '(' ||
		pContext->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION)
	{
		expression.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;

		ASTProcedureDeclaration procDecl = {};
		procDecl.loc = pContext->token->loc;
		procDecl.name = *GetVariableName(&astStaticDef, 0);
		procDecl.isInline = isInline;
		procDecl.isExternal = isExternal;
		procDecl.isExported = isExported;
		procDecl.prototype = ParseProcedurePrototype(pContext);

		if (pContext->token->type == ';')
			Advance(pContext);
		else {
			if (isExternal)
				LogError(pContext->token->loc, "External procedure declaration can't have a body"_s);
			procDecl.astBody = PNewTreeNode(pContext);
			*procDecl.astBody = ParseExpression(pContext, -1, true);
		}

		expression.procedureDeclaration = procDecl;
	}
	else {
		if (isInline)
			LogError(isInlineLoc, "'inline' specified for a non-procedure!"_s);
		if (isExternal)
			LogError(isExternalLoc, "'external' specified for a non-procedure!"_s);
		if (isExported)
			LogError(isExportedLoc, "'external' specified for a non-procedure!"_s);

		switch (pContext->token->type) {
		case TOKEN_KEYWORD_STRUCT:
		case TOKEN_KEYWORD_UNION:
		case TOKEN_KEYWORD_ENUM:
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(pContext);
			break;
		case TOKEN_DIRECTIVE_TYPE:
			Advance(pContext);
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(pContext);

			AssertToken(pContext->token, ';');
			Advance(pContext);
			break;
		case TOKEN_DIRECTIVE_ALIAS:
			Advance(pContext);
			expression.nodeType = ASTNODETYPE_ALIAS;
			expression.astType = ParseType(pContext);

			AssertToken(pContext->token, ';');
			Advance(pContext);
			break;
		default:
			expression = ParseExpression(pContext, -1, false);

			AssertToken(pContext->token, ';');
			Advance(pContext);
		}
	}

	astStaticDef.expression = PNewTreeNode(pContext);
	*astStaticDef.expression = expression;

	return astStaticDef;
}

ASTVariableDeclaration ParseVariableDeclaration(PContext *pContext, ArrayView<String> names)
{
	ASTVariableDeclaration varDecl = {};
	varDecl.loc = pContext->token->loc;

	varDecl.nameCount = (u32)names.size;
	if (names.size == 1)
		varDecl.name = names[0];
	else {
		ASSERT(names.size > 1);
		String *arrayOfNames = (String *)LinearAllocator::Alloc(sizeof(String) * names.size,
				alignof(String));
		// Copy
		for (int i = 0; i < names.size; ++i)
			arrayOfNames[i] = names[i];
		varDecl.arrayOfNames = arrayOfNames;
	}

	if (pContext->token->type == TOKEN_DIRECTIVE_EXTERNAL) {
		varDecl.isExternal = true;
		Advance(pContext);
	}

	if (pContext->token->type != TOKEN_OP_ASSIGNMENT) {
		varDecl.astType = PNewASTType(pContext);
		*varDecl.astType = ParseType(pContext);
	}

	if (pContext->token->type == TOKEN_OP_ASSIGNMENT) {
		if (varDecl.isExternal)
			LogError(pContext->token->loc, "Can't assign value to external variable"_s);

		Advance(pContext);
		varDecl.astInitialValue = PNewTreeNode(pContext);
		*varDecl.astInitialValue = ParseExpression(pContext, -1, false);
	}

	AssertToken(pContext->token, ';');
	Advance(pContext);

	return varDecl;
}

bool TryParseUnaryOperation(PContext *pContext, ASTUnaryOperation *result)
{
	if (!IsOperatorToken(pContext->token->type))
		return false;

	Token *oldToken = pContext->token;
	s64 oldTokenIdx = pContext->currentTokenIdx;

	result->loc = pContext->token->loc;

	switch (pContext->token->type)
	{
	case TOKEN_OP_POINTER_TO:
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	case TOKEN_OP_BITWISE_NOT:
	case TOKEN_OP_MINUS:
	{
		enum TokenType op = pContext->token->type;
		result->op = op;
		Advance(pContext);

		int precedenceOf = op == TOKEN_OP_MINUS ? PRECEDENCE_UNARY_SUBTRACT : op;
		s32 precedence = GetOperatorPrecedence(precedenceOf);
		result->expression = PNewTreeNode(pContext);
		*result->expression = ParseExpression(pContext, precedence, false);

		return true;
	} break;
	}

	pContext->token = oldToken;
	pContext->currentTokenIdx = oldTokenIdx;
	return false;
}

bool TryParseBinaryOperation(PContext *pContext, ASTExpression leftHand, s32 prevPrecedence,
		ASTExpression *result)
{
	result->any.loc = pContext->token->loc;
	result->typeTableIdx = TYPETABLEIDX_Unset;

	Token *oldToken = pContext->token;
	s64 oldTokenIdx = pContext->currentTokenIdx;

	enum TokenType op = pContext->token->type;
	if (op == TOKEN_END_OF_FILE)
		LogError(pContext->tokens[oldTokenIdx - 1].loc, "Unexpected end of file"_s);
	Advance(pContext);

	if (op == '(') {
		// Procedure calls
		s32 precedence = GetOperatorPrecedence('(');
		if (precedence <= (prevPrecedence & (~1))) 
			goto abort;

		result->nodeType = ASTNODETYPE_PROCEDURE_CALL;
		result->any.loc = leftHand.any.loc;
		result->procedureCall.inlineType = CALLINLINETYPE_DONT_CARE;
		DynamicArrayInit(&result->procedureCall.arguments, 4);

		result->procedureCall.procedureExpression = PNewTreeNode(pContext);
		*result->procedureCall.procedureExpression = leftHand;

		// Parse arguments
		while (pContext->token->type != ')') {
			ASTExpression *arg = PNewTreeNode(pContext);
			*arg = ParseExpression(pContext, GetOperatorPrecedence(',') + 1, false);
			*DynamicArrayAdd(&result->procedureCall.arguments) = arg;

			if (pContext->token->type != ')') {
				if (pContext->token->type != ',') {
					String tokenTypeGot = TokenToStringOrType(*pContext->token);
					String errorStr = TPrintF("Expected ')' or ',' but got %S",
							tokenTypeGot);
					LogError(pContext->token->loc, errorStr);
				}
				Advance(pContext);
			}
		}
		Advance(pContext);

		if (pContext->token->type == TOKEN_DIRECTIVE_INLINE) {
			result->procedureCall.inlineType = CALLINLINETYPE_ALWAYS_INLINE;
			Advance(pContext);
		}
		else if (pContext->token->type == TOKEN_DIRECTIVE_NOINLINE) {
			result->procedureCall.inlineType = CALLINLINETYPE_NEVER_INLINE;
			Advance(pContext);
		}

		if (pContext->token->type == TOKEN_DIRECTIVE_INLINE ||
			pContext->token->type == TOKEN_DIRECTIVE_NOINLINE)
			LogError(pContext->token->loc, "Multiple inline directives"_s);

		return true;
	}

	// Other than exceptions above, if the token is not an operator, return early.
	if (!IsOperatorToken(op))
		goto abort;

	result->nodeType = ASTNODETYPE_BINARY_OPERATION;
	result->binaryOperation.op = op;
	result->binaryOperation.leftHand = PNewTreeNode(pContext);
	*result->binaryOperation.leftHand = leftHand;

	switch (op) {
	case TOKEN_OP_STATIC_DEF:
	{
		s32 precedence = GetOperatorPrecedence(op);
		if (precedence > (prevPrecedence & (~1))) {
			ArrayView<String> names;
			if (leftHand.nodeType == ASTNODETYPE_IDENTIFIER)
				names = { &leftHand.identifier.string, 1 };
			else if (leftHand.nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				int nameCount = (int)leftHand.multipleExpressions.array.size;
				Array<String, ThreadAllocator> namesArray;
				ArrayInit(&namesArray, nameCount);
				for (int i = 0; i < nameCount; ++i) {
					ASTExpression *nameExp = leftHand.multipleExpressions.array[i];
					if (nameExp->nodeType != ASTNODETYPE_IDENTIFIER)
						LogError(leftHand.any.loc, "Only identifiers expected "
								"before a static definition, separated by commas"_s);
					*ArrayAdd(&namesArray) = nameExp->identifier.string;
				}
				names = namesArray;
			}
			else
				LogError(leftHand.any.loc, "Only identifiers expected before a "
						"static definition, separated by commas"_s);

			result->nodeType = ASTNODETYPE_STATIC_DEFINITION;
			result->staticDefinition = ParseStaticDefinition(pContext, names);
			return true;
		}
	} break;
	case TOKEN_OP_VARIABLE_DECLARATION:
	case TOKEN_OP_VARIABLE_DECLARATION_STATIC:
	{
		s32 precedence = GetOperatorPrecedence(op);
		if (precedence > (prevPrecedence & (~1))) {
			ArrayView<String> names;
			if (leftHand.nodeType == ASTNODETYPE_IDENTIFIER)
				names = { &leftHand.identifier.string, 1 };
			else if (leftHand.nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				int nameCount = (int)leftHand.multipleExpressions.array.size;
				Array<String, ThreadAllocator> namesArray;
				ArrayInit(&namesArray, nameCount);
				for (int i = 0; i < nameCount; ++i) {
					ASTExpression *nameExp = leftHand.multipleExpressions.array[i];
					if (nameExp->nodeType != ASTNODETYPE_IDENTIFIER)
						LogError(leftHand.any.loc, "Only identifiers expected "
								"before a variable declaration, separated by commas"_s);
					*ArrayAdd(&namesArray) = nameExp->identifier.string;
				}
				names = namesArray;
			}
			else
				LogError(leftHand.any.loc, "Only identifiers expected before a "
						"variable declaration, separated by commas"_s);

			result->nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
			result->variableDeclaration = ParseVariableDeclaration(pContext, names);
			if (op == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
				result->variableDeclaration.isStatic = true;
			return true;
		}
	} break;
	case TOKEN_OP_ARRAY_ACCESS:
	{
		s32 precedence = GetOperatorPrecedence(TOKEN_OP_ARRAY_ACCESS);
		if (precedence > (prevPrecedence & (~1))) {
			result->binaryOperation.rightHand = PNewTreeNode(pContext);
			*result->binaryOperation.rightHand = ParseExpression(pContext, -1, false);

			AssertToken(pContext->token, ']');
			Advance(pContext);

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
		s32 precedence = GetOperatorPrecedence(op);
		if (precedence > (prevPrecedence & (~1))) {
			result->binaryOperation.rightHand = PNewTreeNode(pContext);
			*result->binaryOperation.rightHand = ParseExpression(pContext, precedence, false);

			if (op >= TOKEN_OP_Statement_Begin && op <= TOKEN_OP_Statement_End) {
				AssertToken(pContext->token, ';');
				Advance(pContext);
			}

			return true;
		}
	} break;
	// Unary ops
	//case TOKEN_OP_POINTER_TO: // this is actually binary XOR
	case TOKEN_OP_DEREFERENCE:
	case TOKEN_OP_NOT:
	case TOKEN_OP_BITWISE_NOT:
		goto abort;
	default:
	{
		String opStr = TokenTypeToString(op);
		LogError(result->any.loc, TPrintF("Unexpected operator %S", opStr));
	} break;
	}

abort:
	pContext->token = oldToken;
	pContext->currentTokenIdx = oldTokenIdx;
	return false;
}

ASTIf ParseIf(PContext *pContext)
{
	ASSERT(pContext->token->type == TOKEN_KEYWORD_IF ||
		   pContext->token->type == TOKEN_DIRECTIVE_IF);
	Advance(pContext);

	ASTIf ifNode = {};
	ifNode.loc = pContext->token->loc;

	ifNode.condition = PNewTreeNode(pContext);
	if (pContext->token->type == '(') {
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(pContext);
		*ifNode.condition = ParseExpression(pContext, -1, false);
		AssertToken(pContext->token, ')');
		Advance(pContext);
	}
	else
		*ifNode.condition = ParseExpression(pContext, -1, false);

	ifNode.body = PNewTreeNode(pContext);
	*ifNode.body = ParseExpression(pContext, -1, true);

	if (pContext->token->type == TOKEN_KEYWORD_ELSE) {
		ifNode.elseLoc = pContext->token->loc;
		Advance(pContext);
		ifNode.elseBody = PNewTreeNode(pContext);
		*ifNode.elseBody = ParseExpression(pContext, -1, true);
	}
	return ifNode;
}

ASTWhile ParseWhile(PContext *pContext)
{
	ASSERT(pContext->token->type == TOKEN_KEYWORD_WHILE);

	ASTWhile whileNode = {};
	whileNode.loc = pContext->token->loc;
	Advance(pContext);

	whileNode.condition = PNewTreeNode(pContext);
	if (pContext->token->type == '(') {
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(pContext);
		*whileNode.condition = ParseExpression(pContext, -1, false);
		AssertToken(pContext->token, ')');
		Advance(pContext);
	}
	else
		*whileNode.condition = ParseExpression(pContext, -1, false);

	whileNode.body = PNewTreeNode(pContext);
	*whileNode.body = ParseExpression(pContext, -1, true);

	return whileNode;
}

ASTFor ParseFor(PContext *pContext)
{
	ASSERT(pContext->token->type == TOKEN_KEYWORD_FOR);

	ASTFor forNode = {};
	forNode.loc = pContext->token->loc;
	forNode.indexVariableName = "i"_s;
	forNode.itemVariableName = "it"_s;
	Advance(pContext);

	bool closeParenthesis = false;
	forNode.range = PNewTreeNode(pContext);
	if (pContext->token->type == '(') {
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(pContext);
		closeParenthesis = true;
	}

	Token *oldToken = pContext->token;
	u64 oldTokenIdx = pContext->currentTokenIdx;

	Token first = *pContext->token;
	Advance(pContext);

	if (pContext->token->type == TOKEN_OP_VARIABLE_DECLARATION) {
		Advance(pContext);
		if (first.type != TOKEN_IDENTIFIER)
			LogError(first.loc, "Expected name of index variable before ':' "
					"inside for loop range"_s);
		Advance(pContext);

		forNode.indexVariableName = TokenToString(first);
	}
	else if (pContext->token->type == ',') {
		Advance(pContext);
		Token second = *pContext->token;
		Advance(pContext);
		if (pContext->token->type == ',')
			LogError(pContext->token->loc, "Too many names in for loop condition, "
					"only up to 2 allowed (indexVar, itemVar : expr)"_s);
		AssertToken(pContext->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(pContext);

		if (first.type != TOKEN_IDENTIFIER)
			LogError(first.loc, "Expected name of index variable before ',' "
					"inside for loop range"_s);
		if (second.type != TOKEN_IDENTIFIER)
			LogError(first.loc, "Expected name of item variable before ':' "
					"inside for loop range"_s);

		forNode.indexVariableName = TokenToString(first);
		forNode.itemVariableName  = TokenToString(second);
	}
	else {
		pContext->token = oldToken;
		pContext->currentTokenIdx = oldTokenIdx;
	}

	*forNode.range = ParseExpression(pContext, -1, false);

	if (closeParenthesis) {
		AssertToken(pContext->token, ')');
		Advance(pContext);
	}

	forNode.body = PNewTreeNode(pContext);
	*forNode.body = ParseExpression(pContext, -1, true);

	return forNode;
}

ASTStructMemberDeclaration ParseStructMemberDeclaration(PContext *pContext)
{
	ASTStructMemberDeclaration structMem = {};
	structMem.loc = pContext->token->loc;

	if (pContext->token->type == TOKEN_KEYWORD_USING)
	{
		structMem.isUsing = true;
		Advance(pContext);
	}

	// Anonymous structs/unions
	if (pContext->token->type != TOKEN_KEYWORD_STRUCT && 
		pContext->token->type != TOKEN_KEYWORD_UNION)
	{
		AssertToken(pContext->token, TOKEN_IDENTIFIER);
		structMem.name = TokenToString(*pContext->token);
		Advance(pContext);

		AssertToken(pContext->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(pContext);
	}

	if (pContext->token->type != TOKEN_OP_ASSIGNMENT) {
		structMem.astType = PNewASTType(pContext);
		*structMem.astType = ParseType(pContext);
		if (structMem.astType->nodeType == ASTTYPENODETYPE_INVALID)
			LogError(pContext->token->loc, "Expected type"_s);
	}

	if (pContext->token->type == TOKEN_OP_ASSIGNMENT) {
		Advance(pContext);
		structMem.value = PNewTreeNode(pContext);
		*structMem.value = ParseExpression(pContext, -1, false);
	}

	return structMem;
}

ASTEnumDeclaration ParseEnumDeclaration(PContext *pContext)
{
	SourceLocation loc = pContext->token->loc;

	AssertToken(pContext->token, TOKEN_KEYWORD_ENUM);
	Advance(pContext);

	ASTEnumDeclaration enumNode = {};
	enumNode.loc = loc;
	DynamicArrayInit(&enumNode.members, 16);

	if (pContext->token->type == TOKEN_OP_VARIABLE_DECLARATION) {
		Advance(pContext);
		enumNode.astType = PNewASTType(pContext);
		*enumNode.astType = ParseType(pContext);
	}

	AssertToken(pContext->token, '{');
	Advance(pContext);
	while (pContext->token->type != '}') {
		ASTEnumMember enumMember = {};

		AssertToken(pContext->token, TOKEN_IDENTIFIER);
		enumMember.name = TokenToString(*pContext->token);
		enumMember.loc = pContext->token->loc;
		Advance(pContext);

		if (pContext->token->type == TOKEN_OP_ASSIGNMENT) {
			Advance(pContext);
			enumMember.value = PNewTreeNode(pContext);
			*enumMember.value = ParseExpression(pContext, GetOperatorPrecedence(',') + 1, false);
		}

		if (pContext->token->type != '}') {
			AssertToken(pContext->token, ',');
			Advance(pContext);
		}

		*DynamicArrayAdd(&enumNode.members) = enumMember;
	}
	Advance(pContext);

	return enumNode;
}

ASTStructDeclaration ParseStructOrUnion(PContext *pContext)
{
	SourceLocation loc = pContext->token->loc;

	ASSERT(pContext->token->type == TOKEN_KEYWORD_STRUCT ||
			pContext->token->type == TOKEN_KEYWORD_UNION);
	Advance(pContext);

	ASTStructDeclaration structDeclaration = {};
	structDeclaration.loc = loc;
	DynamicArrayInit(&structDeclaration.members, 16);

	if (pContext->token->type == TOKEN_DIRECTIVE_ALIGN) {
		Advance(pContext);
		AssertToken(pContext->token, '(');
		Advance(pContext);

		structDeclaration.alignExp = PNewTreeNode(pContext);
		*structDeclaration.alignExp = ParseExpression(pContext, -1, false);

		AssertToken(pContext->token, ')');
		Advance(pContext);
	}

	AssertToken(pContext->token, '{');
	Advance(pContext);
	while (pContext->token->type != '}') {
		ASTStructMemberDeclaration member = ParseStructMemberDeclaration(pContext);
		*DynamicArrayAdd(&structDeclaration.members) = member;

		AssertToken(pContext->token, ';');
		Advance(pContext);
	}
	Advance(pContext);

	return structDeclaration;
}

Array<ASTExpression *, LinearAllocator> ParseGroupLiteral(PContext *pContext)
{
	DynamicArray<ASTExpression *, LinearAllocator> members;
	DynamicArrayInit(&members, 8);

	while (pContext->token->type != '}') {
		ASTExpression *newTreeNode = PNewTreeNode(pContext);
		*newTreeNode = ParseExpression(pContext, GetOperatorPrecedence(',') + 1, false);
		*DynamicArrayAdd(&members) = newTreeNode;

		if (pContext->token->type == TOKEN_OP_ASSIGNMENT) {
			Advance(pContext);
			ASTExpression assignment = { ASTNODETYPE_BINARY_OPERATION };
			assignment.typeTableIdx = TYPETABLEIDX_Unset;
			assignment.binaryOperation.op = TOKEN_OP_ASSIGNMENT;
			assignment.binaryOperation.leftHand = PNewTreeNode(pContext);
			*assignment.binaryOperation.leftHand = *newTreeNode;
			assignment.binaryOperation.rightHand = PNewTreeNode(pContext);
			*assignment.binaryOperation.rightHand = ParseExpression(pContext,
					GetOperatorPrecedence(TOKEN_OP_ASSIGNMENT), false);
			*newTreeNode = assignment;
		}

		if (pContext->token->type == ',') {
			Advance(pContext);
			continue;
		}
		else if (pContext->token->type != '}') {
			String tokenStr = TokenTypeToString(pContext->token->type);
			LogError(pContext->token->loc, TPrintF("Parsing struct literal. "
						"Expected ',' or '}' but got %S", tokenStr));
		}
	}

	Array<ASTExpression *, LinearAllocator> result;
	result.data = members.data;
	result.size = members.size;
#if DEBUG_BUILD
	result._capacity = members.capacity;
#endif
	return result;
}

ASTProcedureParameter ParseProcedureParameter(PContext *pContext)
{
	ASTProcedureParameter astParameter = {};
	astParameter.loc = pContext->token->loc;

	if (pContext->token->type == TOKEN_KEYWORD_USING) {
		astParameter.isUsing = true;
		Advance(pContext);
	}

	u64 startTokenIdx = pContext->currentTokenIdx;

	AssertToken(pContext->token, TOKEN_IDENTIFIER);
	astParameter.name = TokenToString(*pContext->token);
	Advance(pContext);

	if (pContext->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(pContext);
	else {
		// Nameless parameter, rewind
		pContext->currentTokenIdx = startTokenIdx;
		pContext->token = &pContext->tokens[startTokenIdx];
		astParameter.name = {};
	}

	if (pContext->token->type != TOKEN_OP_ASSIGNMENT) {
		astParameter.astType = PNewASTType(pContext);
		*astParameter.astType = ParseType(pContext);
	}

	if (pContext->token->type == TOKEN_OP_ASSIGNMENT) {
		Advance(pContext);
		astParameter.astInitialValue = PNewTreeNode(pContext);
		*astParameter.astInitialValue = ParseExpression(pContext, GetOperatorPrecedence(',') + 1,
				false);
	}

	return astParameter;
}

ASTProcedurePrototype ParseProcedurePrototype(PContext *pContext)
{
	ASTProcedurePrototype astPrototype = {};
	astPrototype.loc = pContext->token->loc;
	astPrototype.callingConvention = CC_DEFAULT;

	if (pContext->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION) {
		Advance(pContext);
		AssertToken(pContext->token, '(');
		Advance(pContext);

		AssertToken(pContext->token, TOKEN_IDENTIFIER);
		String tokenStr = TokenToString(*pContext->token);
		if (StringEquals(tokenStr, "win64"_s))
			astPrototype.callingConvention = CC_WIN64;
		else if (StringEquals(tokenStr, "linux64"_s))
			astPrototype.callingConvention = CC_LINUX64;
		else {
			LogErrorNoCrash(pContext->token->loc, TPrintF("Invalid calling "
						"convention specified (\"%S\")", tokenStr));
			LogNote({}, "Could be one of:\n"
					"    * win64\n"
					"    * linux64"_s);
		}
		Advance(pContext);

		AssertToken(pContext->token, ')');
		Advance(pContext);
	}

	DynamicArrayInit(&astPrototype.astParameters, 4);

	AssertToken(pContext->token, '(');
	Advance(pContext);
	while (pContext->token->type != ')') {
		if (pContext->token->type == TOKEN_OP_RANGE) {
			Advance(pContext);
			astPrototype.isVarargs = true;
			astPrototype.varargsLoc = pContext->token->loc;

			if (pContext->token->type == TOKEN_IDENTIFIER) {
				astPrototype.varargsName = TokenToString(*pContext->token);
				Advance(pContext);
			}
			break;
		}

		ASTProcedureParameter astParam = ParseProcedureParameter(pContext);
		ASSERT(astPrototype.astParameters.size <= S8_MAX);

		*DynamicArrayAdd(&astPrototype.astParameters) = astParam;

		if (pContext->token->type != ')') {
			AssertToken(pContext->token, ',');
			Advance(pContext);
		}
	}
	Advance(pContext);

	if (pContext->token->type == TOKEN_OP_ARROW) {
		Advance(pContext);
		DynamicArrayInit(&astPrototype.astReturnTypes, 4);

loop:
		ASTType *newASTType = PNewASTType(pContext);
		*newASTType = ParseType(pContext);
		*DynamicArrayAdd(&astPrototype.astReturnTypes) = newASTType;
		if (pContext->token->type == ',') {
			Advance(pContext);
			goto loop;
		}
	}

	return astPrototype;
}

String EscapeString(String string, SourceLocation loc)
{
	// Return same string if there's nothing to escape
	int i;
	for (i = 0; i < string.size; ++i) {
		char c = string.data[i];
		if (c == '\\')
			goto escape;
	}
	return string;

escape:
	u64 size = string.size;
	char *buffer = (char *)t_threadMemPtr;
	char *out = buffer;
	const u8 *in = (const u8 *)string.data;
	// Copy all that we've already scanned
	memcpy(out, in, i);
	out += i;
	in  += i;
	const void *end = string.data + string.size;

	while (in < end) {
		if (*in == '\\') {
			++in;
			switch (*in) {
			case 'n':
				*out++ = '\n';
				break;
			case '0':
				*out++ = 0;
				break;
			case '"':
				*out++ = '"';
				break;
			case '\'':
				*out++ = '\'';
				break;
			case '\\':
				*out++ = '\\';
				break;
			case 'x':
				if (in + 2 >= end)
					LogError(loc, "Missing characters for hexadecimal number following \\x"_s);
				ParseNumberResult result = IntFromStringHex({ 2, (const char *)in + 1 });
				if (result.error != PARSENUMBERRROR_OK)
					LogError(loc, "Could not parse hexadecimal number following \\x"_s);
				*out++ = (char)result.number;
				in += 2;
				size -= 2;
				break;
			}
			++in;
			--size; // Don't count backslash for string size.
		}
		else {
			*out++ = *in++;
		}
	}

	t_threadMemPtr = (u8 *)t_threadMemPtr + size;

	String result = { size, buffer };
	return result;
}

inline bool IsASTExpressionAStatement(ASTExpression *expression)
{
	// Is it a statement node type
	if (expression->nodeType >= ASTNODETYPE_Statement_Begin &&
		expression->nodeType <= ASTNODETYPE_Statement_End)
		return true;

	// Is it a statement binary op
	if (expression->nodeType == ASTNODETYPE_BINARY_OPERATION &&
		expression->binaryOperation.op >= TOKEN_OP_Statement_Begin &&
		expression->binaryOperation.op <= TOKEN_OP_Statement_End)
		return true;

	return false;
}

ASTExpression ParseExpression(PContext *pContext, s32 precedence, bool isStatement)
{
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_Unset;
	result.any.loc = pContext->token->loc;

	switch (pContext->token->type) {
	// Parenthesis
	case '(':
	{
		Advance(pContext);

		result = ParseExpression(pContext, -1, false);

		AssertToken(pContext->token, ')');
		Advance(pContext);
	} break;
	case '{':
	{
		if (isStatement) {
			// Statement block/scope
			Advance(pContext);

			result.nodeType = ASTNODETYPE_BLOCK;

			DynamicArrayInit(&result.block.statements, 512);
			while (pContext->token->type != '}')
				*DynamicArrayAdd(&result.block.statements) = ParseExpression(pContext, -1, true);

			Advance(pContext);
			// Return because we don't need a semicolon after a scope
			return result;
		}
		else {
			// Group literal
			Advance(pContext);

			result.nodeType = ASTNODETYPE_LITERAL;

			result.literal.type = LITERALTYPE_GROUP;
			result.literal.members = ParseGroupLiteral(pContext);

			AssertToken(pContext->token, '}');
			Advance(pContext);
		}
	} break;
	case '?':
	{
		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_GARBAGE;
		Advance(pContext);
	} break;
	case TOKEN_IDENTIFIER:
	{
		result.any.loc = pContext->token->loc;
		String identifier = TokenToString(*pContext->token);
		Advance(pContext);

		result.nodeType = ASTNODETYPE_IDENTIFIER;
		result.identifier.string = identifier;
	} break;
	case TOKEN_LITERAL_NUMBER:
	{
		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		String tokenStr = TokenToString(*pContext->token);

		bool isFloating = false;
		for (u32 i = 0; i < pContext->token->size; ++i) {
			if (tokenStr.data[i] == '.') {
				isFloating = true;
				break;
			}
		}
		if (!isFloating) {
			result.literal.type = LITERALTYPE_INTEGER;
			result.literal.integer = ParseInt(pContext, tokenStr);
		}
		else {
			result.literal.type = LITERALTYPE_FLOATING;
			result.literal.floating = ParseFloat(pContext, tokenStr);
		}
		Advance(pContext);
	} break;
	case TOKEN_LITERAL_CHARACTER:
	{
		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_CHARACTER;

		String str = TokenToString(*pContext->token);
		if (str.size == 1)
			result.literal.character = str.data[0];
		else {
			ASSERT(str.size == 2);
			ASSERT(str.data[0] == '\\');
			switch (str.data[1]) {
			case 'n':
				result.literal.character = '\n';
				break;
			case 'r':
				result.literal.character = '\r';
				break;
			case 't':
				result.literal.character = '\t';
				break;
			case '\\':
				result.literal.character = '\\';
				break;
			default:
				LogError(result.any.loc, "Invalid escape character"_s);
			}
		}
		Advance(pContext);
	} break;
	case TOKEN_LITERAL_STRING:
	{
		String str = TokenToString(*pContext->token);
		str = EscapeString(str, pContext->token->loc);
		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = str;
		Advance(pContext);
	} break;
	case TOKEN_DIRECTIVE_CSTR:
	{
		Advance(pContext);
		AssertToken(pContext->token, TOKEN_LITERAL_STRING);

		String str = TokenToString(*pContext->token);
		str = EscapeString(str, pContext->token->loc);

		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_CSTR;
		result.literal.string = str;
		Advance(pContext);
	} break;
	case TOKEN_KEYWORD_TYPEOF:
	{
		result.any.loc = pContext->token->loc;
		Advance(pContext);

		result.nodeType = ASTNODETYPE_TYPEOF;
		result.typeOfNode.expression = PNewTreeNode(pContext);
		*result.typeOfNode.expression = ParseExpression(pContext, precedence, false);
	} break;
	case TOKEN_KEYWORD_SIZEOF:
	{
		result.any.loc = pContext->token->loc;
		Advance(pContext);

		result.nodeType = ASTNODETYPE_SIZEOF;
		result.sizeOfNode.expression = PNewTreeNode(pContext);
		*result.sizeOfNode.expression = ParseExpression(pContext, precedence, false);
	} break;
	case TOKEN_DIRECTIVE_DEFINED:
	{
		result.any.loc = pContext->token->loc;
		Advance(pContext);

		AssertToken(pContext->token, '(');
		Advance(pContext);

		result.nodeType = ASTNODETYPE_DEFINED;
		AssertToken(pContext->token, TOKEN_IDENTIFIER);
		result.definedNode.identifier = TokenToString(*pContext->token);
		Advance(pContext);

		AssertToken(pContext->token, ')');
		Advance(pContext);
	} break;
	case TOKEN_KEYWORD_CAST:
	{
		Advance(pContext);
		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_CAST;

		AssertToken(pContext->token, '(');
		Advance(pContext);

		result.castNode.astType = ParseType(pContext);

		AssertToken(pContext->token, ')');
		Advance(pContext);

		result.castNode.expression = PNewTreeNode(pContext);
		int castPrecedence = GetOperatorPrecedence(TOKEN_KEYWORD_CAST);
		*result.castNode.expression = ParseExpression(pContext, castPrecedence, false);
	} break;
	case TOKEN_DIRECTIVE_INTRINSIC:
	{
		Advance(pContext);
		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_INTRINSIC;

		AssertToken(pContext->token, '(');
		Advance(pContext);

		AssertToken(pContext->token, TOKEN_IDENTIFIER);
		result.intrinsic.name = TokenToString(*pContext->token);
		Advance(pContext);

		if (pContext->token->type == ',')
		{
			// Parse arguments
			Advance(pContext);
			DynamicArrayInit(&result.intrinsic.arguments, 4);
			while (pContext->token->type != ')')
			{
				ASTExpression arg = ParseExpression(pContext, GetOperatorPrecedence(',') + 1, false);
				*DynamicArrayAdd(&result.intrinsic.arguments) = arg;

				if (pContext->token->type != ')')
				{
					if (pContext->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(*pContext->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(pContext->token->loc, errorStr);
					}
					Advance(pContext);
				}
			}
		}

		AssertToken(pContext->token, ')');
		Advance(pContext);
	} break;
	case TOKEN_DIRECTIVE_TYPE:
	{
		Advance(pContext);
		result.nodeType = ASTNODETYPE_TYPE;
		result.astType = ParseType(pContext);
	} break;
	case TOKEN_DIRECTIVE_RUN:
	{
		Advance(pContext);
		result.nodeType = ASTNODETYPE_RUN;
		result.runNode.expression = PNewTreeNode(pContext);
		if (isStatement) {
			*result.runNode.expression = ParseExpression(pContext, precedence, isStatement);
			return result;
		}
		else {
			if (pContext->token->type == '{')
				// If the run body is in braces, parse it as a statement. The result will be the
				// return value.
				*result.runNode.expression = ParseExpression(pContext, precedence, true);
			else
				// Else parse as statement, so we allow something like:
				//    foo := #run Bar() + 2;
				*result.runNode.expression = ParseExpression(pContext, precedence, false);
		}
	} break;
	case TOKEN_KEYWORD_UNION:
		if (!isStatement)
			LogError(pContext->token->loc, "'union' not valid on this pContext!"_s);
		// Fall through
	case TOKEN_KEYWORD_STRUCT:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'struct' not valid on this pContext!"_s);

		// Structs/unions out of the blue are considered anonymous and are treated as variable
		// declarations.

		ASTVariableDeclaration varDecl = {};
		varDecl.loc = pContext->token->loc;

		varDecl.astType = PNewASTType(pContext);
		*varDecl.astType = ParseType(pContext); // This will parse the struct/union declaration.

		if (pContext->token->type == TOKEN_OP_ASSIGNMENT) {
			Advance(pContext);
			varDecl.astInitialValue = PNewTreeNode(pContext);
			*varDecl.astInitialValue = ParseExpression(pContext, -1, false);
		}

		result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
		result.variableDeclaration = varDecl;

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} break;
	// Statements! They are in charge of parsing their own ';' at the end if necessary, and don't
	// fall down to the operator parsing, but return here instead.
	case TOKEN_KEYWORD_IF:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'if' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_IF;
		result.ifNode = ParseIf(pContext);
	} return result;
	case TOKEN_DIRECTIVE_IF:
	{
		result.nodeType = ASTNODETYPE_IF_STATIC;
		result.ifNode = ParseIf(pContext);
	} return result;
	case TOKEN_KEYWORD_ELSE:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'else' only valid at statement level!"_s);

		LogError(pContext->token->loc, "Invalid 'else' without matching 'if'"_s);
	}
	case TOKEN_KEYWORD_WHILE:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'while' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_WHILE;
		result.whileNode = ParseWhile(pContext);
	} return result;
	case TOKEN_KEYWORD_FOR:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'for' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_FOR;
		result.forNode = ParseFor(pContext);
	} return result;
	case TOKEN_KEYWORD_CONTINUE:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'continue' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_CONTINUE;
		Advance(pContext);

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} return result;
	case TOKEN_KEYWORD_REMOVE:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'remove' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_REMOVE;
		Advance(pContext);

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} return result;
	case TOKEN_KEYWORD_BREAK:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'break' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_BREAK;
		Advance(pContext);

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} return result;
	case TOKEN_KEYWORD_RETURN:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'return' only valid at statement level!"_s);

		Advance(pContext);

		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_RETURN;
		if (pContext->token->type == ';')
			result.returnNode.expression = nullptr;
		else {
			result.returnNode.expression = PNewTreeNode(pContext);
			*result.returnNode.expression = ParseExpression(pContext, -1, false);
		}

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} return result;
	case TOKEN_KEYWORD_DEFER:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'defer' only valid at statement level!"_s);

		Advance(pContext);

		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_DEFER;
		result.deferNode.expression = PNewTreeNode(pContext);
		*result.deferNode.expression = ParseExpression(pContext, -1, true);
	} return result;
	case TOKEN_KEYWORD_USING:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "'union' not valid on this pContext!"_s);

		Advance(pContext);

		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_USING;
		result.usingNode.expression = PNewTreeNode(pContext);
		*result.usingNode.expression = ParseExpression(pContext, -1, true);
	} return result;
	case TOKEN_DIRECTIVE_BREAK:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "Compiler breakpoint only valid at statement level"_s);

		Advance(pContext);
		result.any.loc = pContext->token->loc;
		result.nodeType = ASTNODETYPE_COMPILER_BREAKPOINT;

		AssertToken(pContext->token, '(');
		Advance(pContext);

		AssertToken(pContext->token, TOKEN_IDENTIFIER);
		String breakpointTypeStr = TokenToString(*pContext->token);
		SourceLocation stringLoc = pContext->token->loc;
		Advance(pContext);

		AssertToken(pContext->token, ')');
		Advance(pContext);

		result.compilerBreakpointType = COMPILERBREAKPOINT_Invalid;
		for (int i = 0; i < COMPILERBREAKPOINT_Count; ++i) {
			if (StringEquals(breakpointTypeStr, compilerBreakpointTypeStrings[i])) {
				result.compilerBreakpointType = (CompilerBreakpointType)i;
				break;
			}
		}

		if (result.compilerBreakpointType == COMPILERBREAKPOINT_Invalid) {
			LogErrorNoCrash(stringLoc, "Invalid type of compiler breakpoint specified"_s);
			String noteMsg = "Could be one of:"_s;
			for (int i = 0; i < COMPILERBREAKPOINT_Count; ++i)
				noteMsg = TPrintF("%S\n\t* \"%S\"", noteMsg, compilerBreakpointTypeStrings[i]);
			LogNote({}, noteMsg);
			PANIC;
		}

		if (result.compilerBreakpointType == COMPILERBREAKPOINT_PARSER)
			BREAK;

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} return result;
	case TOKEN_DIRECTIVE_OPERATOR:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "operator declaration is only valid at statement level"_s);

		Advance(pContext);

		enum TokenType op = pContext->token->type;
		if (op < TOKEN_OP_Begin || op > TOKEN_OP_End)
			UNEXPECTED_TOKEN_ERROR(pContext->token);
		Advance(pContext);

		AssertToken(pContext->token, TOKEN_OP_STATIC_DEF);
		Advance(pContext);

		bool isInline = false;
		if (pContext->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			isInline = true;
			Advance(pContext);
		}

		ASTOperatorOverload overload = {};
		overload.loc = result.any.loc;
		overload.op = op;
		overload.isInline = isInline;
		overload.prototype = ParseProcedurePrototype(pContext);

		overload.astBody = PNewTreeNode(pContext);
		*overload.astBody = ParseExpression(pContext, -1, true);

		result.nodeType = ASTNODETYPE_OPERATOR_OVERLOAD;
		result.operatorOverload = overload;
	} return result;
	case TOKEN_DIRECTIVE_INCLUDE:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "include directive is only valid at statement level"_s);

		result.nodeType = ASTNODETYPE_INCLUDE;
		Advance(pContext);

		AssertToken(pContext->token, TOKEN_LITERAL_STRING);
		result.include.filename = TokenToString(*pContext->token);
		Advance(pContext);

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} return result;
	case TOKEN_DIRECTIVE_LINKLIB:
	{
		if (!isStatement)
			LogError(pContext->token->loc, "linklib directive is only valid at statement level"_s);

		result.nodeType = ASTNODETYPE_LINKLIB;
		Advance(pContext);

		AssertToken(pContext->token, TOKEN_LITERAL_STRING);
		result.linklib.filename = TokenToString(*pContext->token);
		Advance(pContext);

		AssertToken(pContext->token, ';');
		Advance(pContext);
	} return result;
	default:
	{
		if (!IsOperatorToken(pContext->token->type))
			UNEXPECTED_TOKEN_ERROR(pContext->token);
		// Operators are handled in the loop below.
	}
	}

	// Binary/unary operators loop
	while (true) {
		if (result.nodeType == ASTNODETYPE_INVALID) {
			// If we have no left hand, try unary operation
			ASTUnaryOperation unaryOp = result.unaryOperation;
			bool success = TryParseUnaryOperation(pContext, &unaryOp);
			if (!success)
				LogError(pContext->token->loc, "Invalid expression!"_s);
			result.nodeType = ASTNODETYPE_UNARY_OPERATION;
			result.unaryOperation = unaryOp;

			if (unaryOp.op == TOKEN_OP_MINUS && unaryOp.expression->nodeType == ASTNODETYPE_LITERAL)
			{
				ASTLiteral *literal = &unaryOp.expression->literal;
				switch (literal->type) {
				case LITERALTYPE_INTEGER:
					literal->integer = -literal->integer;
					break;
				case LITERALTYPE_FLOATING:
					literal->floating = -literal->floating;
					break;
				case LITERALTYPE_CHARACTER:
					literal->character = -literal->character;
					break;
				default:
					continue;
				}
				literal->loc = result.any.loc;
				result.nodeType = ASTNODETYPE_LITERAL;
				result.literal = *literal;
			}

			continue;
		}
		else {
			ASTExpression exp;
			bool success = TryParseBinaryOperation(pContext, result, precedence, &exp);
			if (success) {
				result = exp;
				if (IsASTExpressionAStatement(&result))
					break;
				else
					continue;
			}
		}

		if (pContext->token->type == ',' && GetOperatorPrecedence(',') >= precedence) {
			Advance(pContext);

			ASTExpression mux;
			mux.any.loc = result.any.loc;
			mux.typeTableIdx = TYPETABLEIDX_Unset;
			mux.nodeType = ASTNODETYPE_MULTIPLE_EXPRESSIONS;

			ASTExpression *first = PNewTreeNode(pContext);
			*first = result;

			ASTExpression second = ParseExpression(pContext, GetOperatorPrecedence(','), false);
			if (second.nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				DynamicArrayInit(&mux.multipleExpressions.array, second.multipleExpressions.array.size + 1);
				*DynamicArrayAdd(&mux.multipleExpressions.array) = first;
				for (int i = 0; i < second.multipleExpressions.array.size; ++i)
					*DynamicArrayAdd(&mux.multipleExpressions.array) = second.multipleExpressions.array[i];
			}
			else {
				DynamicArrayInit(&mux.multipleExpressions.array, 2);
				ASTExpression *sec = PNewTreeNode(pContext);
				*sec = second;
				*DynamicArrayAdd(&mux.multipleExpressions.array) = first;
				*DynamicArrayAdd(&mux.multipleExpressions.array) = sec;
			}

			result = mux;
		}
		else
			break;
	}

	if (!isStatement && result.nodeType == ASTNODETYPE_BINARY_OPERATION &&
			result.binaryOperation.op >= TOKEN_OP_ASSIGNMENT_Begin &&
			result.binaryOperation.op <= TOKEN_OP_ASSIGNMENT_End)
		LogError(result.any.loc, "Assignment only valid at statement level"_s);

	if (isStatement && !IsASTExpressionAStatement(&result)) {
		// Look for semicolon only if we are parsing a statement, and the binary expression we
		// parsed was not an statement itself (e.g. a variable declaration).
		AssertToken(pContext->token, ';');
		Advance(pContext);
	}

	return result;
}

void ParseJobProc(void *args)
{
	ParseJobArgs *argsStruct = (ParseJobArgs *)args;
	u32 fileIdx = argsStruct->fileIdx;

	PContext *pContext = ALLOC(LinearAllocator, PContext);
	pContext->fileIdx = fileIdx;
	pContext->token = nullptr;
	pContext->currentTokenIdx = 0;
	BucketArrayInit(&pContext->tokens);
	DynamicArrayInit(&pContext->astRoot.block.statements, 4096);
	BucketArrayInit(&pContext->astTreeNodes);
	BucketArrayInit(&pContext->astTypes);

	Job *runningJob = GetCurrentJob();
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->description = SStringConcat("P:"_s, g_context->sourceFiles[fileIdx].name);
#endif

	TokenizeFile(pContext, fileIdx);

	pContext->token = &pContext->tokens[pContext->currentTokenIdx];

	DynamicArray<ASTExpression, LinearAllocator> *statements =
		&pContext->astRoot.block.statements;
	while (pContext->token->type != TOKEN_END_OF_FILE) {
		ASTExpression *statement = DynamicArrayAdd(statements);
		*statement = ParseExpression(pContext, -1, true);
		GenerateTypeCheckJobs(statement);
	}

	if (g_context->config.logAST)
		PrintAST(pContext);

	FinishCurrentJob();
}

void ParserMain()
{
	MTQueueInit<HeapAllocator>(&g_context->readyJobs, 2048);

	for (int i = 0; i < YIELDREASON_Count; ++i)
		DynamicArrayInit(&g_context->waitingJobsByReason[i].unsafe, 64);
}
