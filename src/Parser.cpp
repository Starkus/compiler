ASTExpression ParseExpression(PContext *context, s32 precedence, bool isStatement);
ASTExpression ParseStatement(PContext *context);

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

void Advance(PContext *context)
{
	ASSERT(context->token == &context->tokens[context->currentTokenIdx]);

	++context->currentTokenIdx;
	if (context->currentTokenIdx > context->tokens.count)
		LogError(context->token->loc, "Unexpected end of file"_s);
	context->token = &context->tokens[context->currentTokenIdx];
}

inline ASTExpression *PNewTreeNode(PContext *context)
{
	return BucketArrayAdd(&context->astTreeNodes);
}

inline ASTType *NewASTType(PContext *context)
{
	return BucketArrayAdd(&context->astTypes);
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

inline s64 ParseInt(PContext *context, String str)
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
			LogError(context->token->loc, "Integer literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context->token->loc, "Integer literal too negative!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context->token->loc,
					"Integer literal contains invalid characters"_s);
			break;
		}
	}
	return parseResult.number;
}

inline f64 ParseFloat(PContext *context, String str)
{
	ParseFloatResult parseResult = F64FromString(str);
	if (parseResult.error) {
		switch (parseResult.error) {
		case PARSENUMBERRROR_OVERFLOW:
			LogError(context->token->loc, "Floating point literal too big!"_s);
			break;
		case PARSENUMBERRROR_UNDERFLOW:
			LogError(context->token->loc, "Floating point literal too small!"_s);
			break;
		case PARSENUMBERRROR_INVALID_CHARACTER:
			LogError(context->token->loc,
					"Floating point literal contains invalid characters"_s);
		case PARSENUMBERRROR_INVALID_EXPONENT:
			LogError(context->token->loc,
					"Could not parse exponent in scientific notation"_s);
			break;
		}
	}
	return parseResult.number;
}

ASTStructDeclaration ParseStructOrUnion(PContext *context);
ASTEnumDeclaration ParseEnumDeclaration(PContext *context);
ASTProcedurePrototype ParseProcedurePrototype(PContext *context);
ASTType ParseType(PContext *context)
{
	ASTType astType;
	astType.loc = context->token->loc;

	switch (context->token->type) {
	case TOKEN_OP_ARRAY_ACCESS:
	{
		Advance(context);
		astType.nodeType = ASTTYPENODETYPE_ARRAY;

		astType.arrayCountExp = nullptr;
		if (context->token->type != ']') {
			astType.arrayCountExp = PNewTreeNode(context);
			*astType.arrayCountExp = ParseExpression(context, -1, false);
		}
		AssertToken(context->token, ']');
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
		astType.name = TokenToString(*context->token);
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
		LogError(context->token->loc, "Failed to parse type"_s);
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

ASTStaticDefinition ParseStaticDefinition(PContext *context, ArrayView<String> names)
{
	ASTStaticDefinition astStaticDef = {};
	astStaticDef.loc = context->token->loc;

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
	expression.any.loc = context->token->loc;

	bool isInline = false;
	bool isExternal = false;
	bool isExported = false;
	SourceLocation isInlineLoc = {}, isExternalLoc = {}, isExportedLoc = {};
	while (true) {
		if (context->token->type == TOKEN_DIRECTIVE_INLINE) {
			if (isInline) LogError(context->token->loc, "'inline' used twice"_s);
			isInline = true;
			isInlineLoc = context->token->loc;
			Advance(context);
		}
		else if (context->token->type == TOKEN_DIRECTIVE_EXTERNAL) {
			if (isExternal) LogError(context->token->loc, "'external' used twice"_s);
			isExternal = true;
			isExternalLoc = context->token->loc;
			Advance(context);
		}
		else if (context->token->type == TOKEN_DIRECTIVE_EXPORT) {
			if (isExported) LogError(context->token->loc, "'export' used twice"_s);
			isExported = true;
			isExportedLoc = context->token->loc;
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
		procDecl.name = *GetVariableName(&astStaticDef, 0);
		procDecl.isInline = isInline;
		procDecl.isExternal = isExternal;
		procDecl.isExported = isExported;
		procDecl.prototype = ParseProcedurePrototype(context);

		if (context->token->type == ';')
			Advance(context);
		else {
			if (isExternal)
				LogError(context->token->loc, "External procedure declaration can't have a body"_s);
			procDecl.astBody = PNewTreeNode(context);
			*procDecl.astBody = ParseExpression(context, -1, true);
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

		switch (context->token->type) {
		case TOKEN_KEYWORD_STRUCT:
		case TOKEN_KEYWORD_UNION:
		case TOKEN_KEYWORD_ENUM:
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);
			break;
		case TOKEN_DIRECTIVE_TYPE:
			Advance(context);
			expression.nodeType = ASTNODETYPE_TYPE;
			expression.astType = ParseType(context);
			break;
		case TOKEN_DIRECTIVE_ALIAS:
			Advance(context);
			expression.nodeType = ASTNODETYPE_ALIAS;
			expression.astType = ParseType(context);
			break;
		default:
			expression = ParseExpression(context, -1, false);
		}
		AssertToken(context->token, ';');
		Advance(context);
	}

	astStaticDef.expression = PNewTreeNode(context);
	*astStaticDef.expression = expression;

	return astStaticDef;
}

ASTVariableDeclaration ParseVariableDeclaration(PContext *context, ArrayView<String> names)
{
	ASTVariableDeclaration varDecl = {};
	varDecl.loc = context->token->loc;

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

	if (context->token->type == TOKEN_DIRECTIVE_EXTERNAL) {
		varDecl.isExternal = true;
		Advance(context);
	}

	if (context->token->type != TOKEN_OP_ASSIGNMENT) {
		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context);
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT) {
		if (varDecl.isExternal)
			LogError(context->token->loc, "Can't assign value to external variable"_s);

		Advance(context);
		varDecl.astInitialValue = PNewTreeNode(context);
		*varDecl.astInitialValue = ParseExpression(context, -1, false);
	}

	AssertToken(context->token, ';');
	Advance(context);

	return varDecl;
}

bool TryParseUnaryOperation(PContext *context, s32 prevPrecedence, ASTUnaryOperation *result)
{
	if (!IsOperatorToken(context->token->type))
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
		result->expression = PNewTreeNode(context);
		*result->expression = ParseExpression(context, precedence, false);

		return true;
	} break;
	}

	context->token = oldToken;
	context->currentTokenIdx = oldTokenIdx;
	return false;
}

bool TryParseBinaryOperation(PContext *context, ASTExpression leftHand, s32 prevPrecedence,
		ASTExpression *result)
{
	result->any.loc = context->token->loc;
	result->typeTableIdx = TYPETABLEIDX_Unset;

	Token *oldToken = context->token;
	s64 oldTokenIdx = context->currentTokenIdx;

	enum TokenType op = context->token->type;
	if (op == TOKEN_END_OF_FILE)
		LogError(context->tokens[oldTokenIdx - 1].loc, "Unexpected end of file"_s);
	Advance(context);

	if (op == '(') {
		// Procedure calls
		s32 precedence = GetOperatorPrecedence('(');
		if (precedence <= (prevPrecedence & (~1))) 
			goto abort;

		result->nodeType = ASTNODETYPE_PROCEDURE_CALL;
		result->any.loc = leftHand.any.loc;
		result->procedureCall.inlineType = CALLINLINETYPE_DONT_CARE;
		DynamicArrayInit(&result->procedureCall.arguments, 4);

		result->procedureCall.procedureExpression = PNewTreeNode(context);
		*result->procedureCall.procedureExpression = leftHand;

		// Parse arguments
		while (context->token->type != ')') {
			ASTExpression *arg = PNewTreeNode(context);
			*arg = ParseExpression(context, GetOperatorPrecedence(',') + 1, false);
			*DynamicArrayAdd(&result->procedureCall.arguments) = arg;

			if (context->token->type != ')') {
				if (context->token->type != ',') {
					String tokenTypeGot = TokenToStringOrType(*context->token);
					String errorStr = TPrintF("Expected ')' or ',' but got %S",
							tokenTypeGot);
					LogError(context->token->loc, errorStr);
				}
				Advance(context);
			}
		}
		Advance(context);

		if (context->token->type == TOKEN_DIRECTIVE_INLINE) {
			result->procedureCall.inlineType = CALLINLINETYPE_ALWAYS_INLINE;
			Advance(context);
		}
		else if (context->token->type == TOKEN_DIRECTIVE_NOINLINE) {
			result->procedureCall.inlineType = CALLINLINETYPE_NEVER_INLINE;
			Advance(context);
		}

		if (context->token->type == TOKEN_DIRECTIVE_INLINE ||
			context->token->type == TOKEN_DIRECTIVE_NOINLINE)
			LogError(context->token->loc, "Multiple inline directives"_s);

		return true;
	}

	// Other than exceptions above, if the token is not an operator, return early.
	if (!IsOperatorToken(op))
		goto abort;

	result->nodeType = ASTNODETYPE_BINARY_OPERATION;
	result->binaryOperation.op = op;
	result->binaryOperation.leftHand = PNewTreeNode(context);
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
			result->staticDefinition = ParseStaticDefinition(context, names);
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
			result->variableDeclaration = ParseVariableDeclaration(context, names);
			if (op == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
				result->variableDeclaration.isStatic = true;
			return true;
		}
	} break;
	case TOKEN_OP_ARRAY_ACCESS:
	{
		s32 precedence = GetOperatorPrecedence(TOKEN_OP_ARRAY_ACCESS);
		if (precedence > (prevPrecedence & (~1))) {
			result->binaryOperation.rightHand = PNewTreeNode(context);
			*result->binaryOperation.rightHand = ParseExpression(context, -1, false);

			AssertToken(context->token, ']');
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
		s32 precedence = GetOperatorPrecedence(op);
		if (precedence > (prevPrecedence & (~1))) {
			result->binaryOperation.rightHand = PNewTreeNode(context);
			*result->binaryOperation.rightHand = ParseExpression(context, precedence, false);

			if (op >= TOKEN_OP_Statement_Begin && op <= TOKEN_OP_Statement_End) {
				AssertToken(context->token, ';');
				Advance(context);
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
	context->token = oldToken;
	context->currentTokenIdx = oldTokenIdx;
	return false;
}

ASTIf ParseIf(PContext *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_IF ||
		   context->token->type == TOKEN_DIRECTIVE_IF);
	Advance(context);

	ASTIf ifNode = {};
	ifNode.loc = context->token->loc;

	ifNode.condition = PNewTreeNode(context);
	if (context->token->type == '(') {
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*ifNode.condition = ParseExpression(context, -1, false);
		AssertToken(context->token, ')');
		Advance(context);
	}
	else
		*ifNode.condition = ParseExpression(context, -1, false);

	ifNode.body = PNewTreeNode(context);
	*ifNode.body = ParseExpression(context, -1, true);

	if (context->token->type == TOKEN_KEYWORD_ELSE) {
		ifNode.elseLoc = context->token->loc;
		Advance(context);
		ifNode.elseBody = PNewTreeNode(context);
		*ifNode.elseBody = ParseExpression(context, -1, true);
	}
	return ifNode;
}

ASTWhile ParseWhile(PContext *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_WHILE);

	ASTWhile whileNode = {};
	whileNode.loc = context->token->loc;
	Advance(context);

	whileNode.condition = PNewTreeNode(context);
	if (context->token->type == '(') {
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*whileNode.condition = ParseExpression(context, -1, false);
		AssertToken(context->token, ')');
		Advance(context);
	}
	else
		*whileNode.condition = ParseExpression(context, -1, false);

	whileNode.body = PNewTreeNode(context);
	*whileNode.body = ParseExpression(context, -1, true);

	return whileNode;
}

ASTFor ParseFor(PContext *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_FOR);

	ASTFor forNode = {};
	forNode.loc = context->token->loc;
	forNode.indexVariableName = "i"_s;
	forNode.itemVariableName = "it"_s;
	Advance(context);

	bool closeParenthesis = false;
	forNode.range = PNewTreeNode(context);
	if (context->token->type == '(') {
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		closeParenthesis = true;
	}

	Token *oldToken = context->token;
	u64 oldTokenIdx = context->currentTokenIdx;

	Token first = *context->token;
	Advance(context);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION) {
		Advance(context);
		if (first.type != TOKEN_IDENTIFIER)
			LogError(first.loc, "Expected name of index variable before ':' "
					"inside for loop range"_s);
		Advance(context);

		forNode.indexVariableName = TokenToString(first);
	}
	else if (context->token->type == ',') {
		Advance(context);
		Token second = *context->token;
		Advance(context);
		if (context->token->type == ',')
			LogError(context->token->loc, "Too many names in for loop condition, "
					"only up to 2 allowed (indexVar, itemVar : expr)"_s);
		AssertToken(context->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(context);

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
		context->token = oldToken;
		context->currentTokenIdx = oldTokenIdx;
	}

	*forNode.range = ParseExpression(context, -1, false);

	if (closeParenthesis) {
		AssertToken(context->token, ')');
		Advance(context);
	}

	forNode.body = PNewTreeNode(context);
	*forNode.body = ParseExpression(context, -1, true);

	return forNode;
}

ASTStructMemberDeclaration ParseStructMemberDeclaration(PContext *context)
{
	ASTStructMemberDeclaration structMem = {};
	structMem.loc = context->token->loc;

	if (context->token->type == TOKEN_KEYWORD_USING)
	{
		structMem.isUsing = true;
		Advance(context);
	}

	// Anonymous structs/unions
	if (context->token->type != TOKEN_KEYWORD_STRUCT && 
		context->token->type != TOKEN_KEYWORD_UNION)
	{
		AssertToken(context->token, TOKEN_IDENTIFIER);
		structMem.name = TokenToString(*context->token);
		Advance(context);

		AssertToken(context->token, TOKEN_OP_VARIABLE_DECLARATION);
		Advance(context);
	}

	if (context->token->type != TOKEN_OP_ASSIGNMENT) {
		structMem.astType = NewASTType(context);
		*structMem.astType = ParseType(context);
		if (structMem.astType->nodeType == ASTTYPENODETYPE_INVALID)
			LogError(context->token->loc, "Expected type"_s);
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT) {
		Advance(context);
		structMem.value = PNewTreeNode(context);
		*structMem.value = ParseExpression(context, -1, false);
	}

	return structMem;
}

ASTEnumDeclaration ParseEnumDeclaration(PContext *context)
{
	SourceLocation loc = context->token->loc;

	AssertToken(context->token, TOKEN_KEYWORD_ENUM);
	Advance(context);

	ASTEnumDeclaration enumNode = {};
	enumNode.loc = loc;
	DynamicArrayInit(&enumNode.members, 16);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION) {
		Advance(context);
		enumNode.astType = NewASTType(context);
		*enumNode.astType = ParseType(context);
	}

	AssertToken(context->token, '{');
	Advance(context);
	while (context->token->type != '}') {
		ASTEnumMember enumMember = {};

		AssertToken(context->token, TOKEN_IDENTIFIER);
		enumMember.name = TokenToString(*context->token);
		enumMember.loc = context->token->loc;
		Advance(context);

		if (context->token->type == TOKEN_OP_ASSIGNMENT) {
			Advance(context);
			enumMember.value = PNewTreeNode(context);
			*enumMember.value = ParseExpression(context, GetOperatorPrecedence(',') + 1, false);
		}

		if (context->token->type != '}') {
			AssertToken(context->token, ',');
			Advance(context);
		}

		*DynamicArrayAdd(&enumNode.members) = enumMember;
	}
	Advance(context);

	return enumNode;
}

ASTStructDeclaration ParseStructOrUnion(PContext *context)
{
	SourceLocation loc = context->token->loc;

	ASSERT(context->token->type == TOKEN_KEYWORD_STRUCT ||
			context->token->type == TOKEN_KEYWORD_UNION);
	Advance(context);

	ASTStructDeclaration structDeclaration = {};
	structDeclaration.loc = loc;
	DynamicArrayInit(&structDeclaration.members, 16);

	if (context->token->type == TOKEN_DIRECTIVE_ALIGN) {
		Advance(context);
		AssertToken(context->token, '(');
		Advance(context);

		structDeclaration.alignExp = PNewTreeNode(context);
		*structDeclaration.alignExp = ParseExpression(context, -1, false);

		AssertToken(context->token, ')');
		Advance(context);
	}

	AssertToken(context->token, '{');
	Advance(context);
	while (context->token->type != '}') {
		ASTStructMemberDeclaration member = ParseStructMemberDeclaration(context);
		*DynamicArrayAdd(&structDeclaration.members) = member;

		AssertToken(context->token, ';');
		Advance(context);
	}
	Advance(context);

	return structDeclaration;
}

Array<ASTExpression *, LinearAllocator> ParseGroupLiteral(PContext *context)
{
	DynamicArray<ASTExpression *, LinearAllocator> members;
	DynamicArrayInit(&members, 8);

	while (context->token->type != '}') {
		ASTExpression *newTreeNode = PNewTreeNode(context);
		*newTreeNode = ParseExpression(context, GetOperatorPrecedence(',') + 1, false);
		*DynamicArrayAdd(&members) = newTreeNode;

		if (context->token->type == TOKEN_OP_ASSIGNMENT) {
			Advance(context);
			ASTExpression assignment = { ASTNODETYPE_BINARY_OPERATION };
			assignment.typeTableIdx = TYPETABLEIDX_Unset;
			assignment.binaryOperation.op = TOKEN_OP_ASSIGNMENT;
			assignment.binaryOperation.leftHand = PNewTreeNode(context);
			*assignment.binaryOperation.leftHand = *newTreeNode;
			assignment.binaryOperation.rightHand = PNewTreeNode(context);
			*assignment.binaryOperation.rightHand = ParseExpression(context,
					GetOperatorPrecedence(TOKEN_OP_ASSIGNMENT), false);
			*newTreeNode = assignment;
		}

		if (context->token->type == ',') {
			Advance(context);
			continue;
		}
		else if (context->token->type != '}') {
			String tokenStr = TokenTypeToString(context->token->type);
			LogError(context->token->loc, TPrintF("Parsing struct literal. "
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

ASTProcedureParameter ParseProcedureParameter(PContext *context)
{
	ASTProcedureParameter astParameter = {};
	astParameter.loc = context->token->loc;

	if (context->token->type == TOKEN_KEYWORD_USING) {
		astParameter.isUsing = true;
		Advance(context);
	}

	u64 startTokenIdx = context->currentTokenIdx;

	AssertToken(context->token, TOKEN_IDENTIFIER);
	astParameter.name = TokenToString(*context->token);
	Advance(context);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else {
		// Nameless parameter, rewind
		context->currentTokenIdx = startTokenIdx;
		context->token = &context->tokens[startTokenIdx];
		astParameter.name = {};
	}

	if (context->token->type != TOKEN_OP_ASSIGNMENT) {
		astParameter.astType = NewASTType(context);
		*astParameter.astType = ParseType(context);
	}

	if (context->token->type == TOKEN_OP_ASSIGNMENT) {
		Advance(context);
		astParameter.astInitialValue = PNewTreeNode(context);
		*astParameter.astInitialValue = ParseExpression(context, GetOperatorPrecedence(',') + 1,
				false);
	}

	return astParameter;
}

ASTProcedurePrototype ParseProcedurePrototype(PContext *context)
{
	ASTProcedurePrototype astPrototype = {};
	astPrototype.loc = context->token->loc;
	astPrototype.callingConvention = CC_DEFAULT;

	if (context->token->type == TOKEN_DIRECTIVE_CALLING_CONVENTION) {
		Advance(context);
		AssertToken(context->token, '(');
		Advance(context);

		AssertToken(context->token, TOKEN_IDENTIFIER);
		String tokenStr = TokenToString(*context->token);
		if (StringEquals(tokenStr, "win64"_s))
			astPrototype.callingConvention = CC_WIN64;
		else if (StringEquals(tokenStr, "linux64"_s))
			astPrototype.callingConvention = CC_LINUX64;
		else {
			LogErrorNoCrash(context->token->loc, TPrintF("Invalid calling "
						"convention specified (\"%S\")", tokenStr));
			LogNote({}, "Could be one of:\n"
					"    * win64\n"
					"    * linux64"_s);
		}
		Advance(context);

		AssertToken(context->token, ')');
		Advance(context);
	}

	DynamicArrayInit(&astPrototype.astParameters, 4);

	AssertToken(context->token, '(');
	Advance(context);
	while (context->token->type != ')') {
		if (context->token->type == TOKEN_OP_RANGE) {
			Advance(context);
			astPrototype.isVarargs = true;
			astPrototype.varargsLoc = context->token->loc;

			if (context->token->type == TOKEN_IDENTIFIER) {
				astPrototype.varargsName = TokenToString(*context->token);
				Advance(context);
			}
			break;
		}

		ASTProcedureParameter astParam = ParseProcedureParameter(context);
		ASSERT(astPrototype.astParameters.size <= S8_MAX);

		*DynamicArrayAdd(&astPrototype.astParameters) = astParam;

		if (context->token->type != ')') {
			AssertToken(context->token, ',');
			Advance(context);
		}
	}
	Advance(context);

	if (context->token->type == TOKEN_OP_ARROW) {
		Advance(context);
		DynamicArrayInit(&astPrototype.astReturnTypes, 4);

loop:
		ASTType *newASTType = NewASTType(context);
		*newASTType = ParseType(context);
		*DynamicArrayAdd(&astPrototype.astReturnTypes) = newASTType;
		if (context->token->type == ',') {
			Advance(context);
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

ASTExpression ParseExpression(PContext *context, s32 precedence, bool isStatement)
{
	ASTExpression result = {};
	result.typeTableIdx = TYPETABLEIDX_Unset;
	result.any.loc = context->token->loc;

	switch (context->token->type) {
	// Parenthesis
	case '(':
	{
		Advance(context);

		result = ParseExpression(context, -1, false);

		AssertToken(context->token, ')');
		Advance(context);
	} break;
	case '{':
	{
		if (isStatement) {
			// Statement block/scope
			Advance(context);

			result.nodeType = ASTNODETYPE_BLOCK;

			DynamicArrayInit(&result.block.statements, 512);
			while (context->token->type != '}')
				*DynamicArrayAdd(&result.block.statements) = ParseExpression(context, -1, true);

			Advance(context);
			// Return because we don't need a semicolon after a scope
			return result;
		}
		else {
			// Group literal
			Advance(context);

			result.nodeType = ASTNODETYPE_LITERAL;

			result.literal.type = LITERALTYPE_GROUP;
			result.literal.members = ParseGroupLiteral(context);

			AssertToken(context->token, '}');
			Advance(context);
		}
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
		String identifier = TokenToString(*context->token);
		Advance(context);

		result.nodeType = ASTNODETYPE_IDENTIFIER;
		result.identifier.string = identifier;
	} break;
	case TOKEN_LITERAL_NUMBER:
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		String tokenStr = TokenToString(*context->token);

		bool isHex = false;
		bool isFloating = false;
		if (tokenStr.data[0] == '0') {
			if (tokenStr.data[1] == 'x' || tokenStr.data[1] == 'X')
				isHex = true;
		}

		for (u32 i = 0; i < context->token->size; ++i) {
			if (tokenStr.data[i] == '.') {
				isFloating = true;
				break;
			}
		}
		if (!isFloating) {
			result.literal.type = LITERALTYPE_INTEGER;
			result.literal.integer = ParseInt(context, tokenStr);
		}
		else {
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

		String str = TokenToString(*context->token);
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
		Advance(context);
	} break;
	case TOKEN_LITERAL_STRING:
	{
		String str = TokenToString(*context->token);
		str = EscapeString(str, context->token->loc);
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_STRING;
		result.literal.string = str;
		Advance(context);
	} break;
	case TOKEN_DIRECTIVE_CSTR:
	{
		Advance(context);
		AssertToken(context->token, TOKEN_LITERAL_STRING);

		String str = TokenToString(*context->token);
		str = EscapeString(str, context->token->loc);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_CSTR;
		result.literal.string = str;
		Advance(context);
	} break;
	case TOKEN_KEYWORD_TYPEOF:
	{
		result.any.loc = context->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_TYPEOF;
		result.typeOfNode.expression = PNewTreeNode(context);
		*result.typeOfNode.expression = ParseExpression(context, precedence, false);
	} break;
	case TOKEN_KEYWORD_SIZEOF:
	{
		result.any.loc = context->token->loc;
		Advance(context);

		result.nodeType = ASTNODETYPE_SIZEOF;
		result.sizeOfNode.expression = PNewTreeNode(context);
		*result.sizeOfNode.expression = ParseExpression(context, precedence, false);
	} break;
	case TOKEN_DIRECTIVE_DEFINED:
	{
		result.any.loc = context->token->loc;
		Advance(context);

		AssertToken(context->token, '(');
		Advance(context);

		result.nodeType = ASTNODETYPE_DEFINED;
		AssertToken(context->token, TOKEN_IDENTIFIER);
		result.definedNode.identifier = TokenToString(*context->token);
		Advance(context);

		AssertToken(context->token, ')');
		Advance(context);
	} break;
	case TOKEN_KEYWORD_CAST:
	{
		Advance(context);
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_CAST;

		AssertToken(context->token, '(');
		Advance(context);

		result.castNode.astType = ParseType(context);

		AssertToken(context->token, ')');
		Advance(context);

		result.castNode.expression = PNewTreeNode(context);
		int castPrecedence = GetOperatorPrecedence(TOKEN_KEYWORD_CAST);
		*result.castNode.expression = ParseExpression(context, castPrecedence, false);
	} break;
	case TOKEN_DIRECTIVE_INTRINSIC:
	{
		Advance(context);
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_INTRINSIC;

		AssertToken(context->token, '(');
		Advance(context);

		AssertToken(context->token, TOKEN_IDENTIFIER);
		result.intrinsic.name = TokenToString(*context->token);
		Advance(context);

		if (context->token->type == ',')
		{
			// Parse arguments
			Advance(context);
			DynamicArrayInit(&result.intrinsic.arguments, 4);
			while (context->token->type != ')')
			{
				ASTExpression arg = ParseExpression(context, GetOperatorPrecedence(',') + 1, false);
				*DynamicArrayAdd(&result.intrinsic.arguments) = arg;

				if (context->token->type != ')')
				{
					if (context->token->type != ',')
					{
						String tokenTypeGot = TokenToStringOrType(*context->token);
						String errorStr = TPrintF("Expected ')' or ',' but got %S",
								tokenTypeGot);
						LogError(context->token->loc, errorStr);
					}
					Advance(context);
				}
			}
		}

		AssertToken(context->token, ')');
		Advance(context);
	} break;
	case TOKEN_DIRECTIVE_TYPE:
	{
		Advance(context);
		result.nodeType = ASTNODETYPE_TYPE;
		result.astType = ParseType(context);
	} break;
	case TOKEN_DIRECTIVE_RUN:
	{
		Advance(context);
		result.nodeType = ASTNODETYPE_RUN;
		result.runNode.expression = PNewTreeNode(context);
		*result.runNode.expression = ParseExpression(context, precedence, false);
	} break;
	case TOKEN_KEYWORD_UNION:
		if (!isStatement)
			LogError(context->token->loc, "'union' not valid on this context!"_s);
		// Fall through
	case TOKEN_KEYWORD_STRUCT:
	{
		if (!isStatement)
			LogError(context->token->loc, "'struct' not valid on this context!"_s);

		// Structs/unions out of the blue are considered anonymous and are treated as variable
		// declarations.

		ASTVariableDeclaration varDecl = {};
		varDecl.loc = context->token->loc;

		varDecl.astType = NewASTType(context);
		*varDecl.astType = ParseType(context); // This will parse the struct/union declaration.

		if (context->token->type == TOKEN_OP_ASSIGNMENT) {
			Advance(context);
			varDecl.astInitialValue = PNewTreeNode(context);
			*varDecl.astInitialValue = ParseExpression(context, -1, false);
		}

		result.nodeType = ASTNODETYPE_VARIABLE_DECLARATION;
		result.variableDeclaration = varDecl;

		AssertToken(context->token, ';');
		Advance(context);
	} break;
	// Statements! They are in charge of parsing their own ';' at the end if necessary, and don't
	// fall down to the operator parsing, but return here instead.
	case TOKEN_KEYWORD_IF:
	{
		if (!isStatement)
			LogError(context->token->loc, "'if' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_IF;
		result.ifNode = ParseIf(context);
	} return result;
	case TOKEN_DIRECTIVE_IF:
	{
		result.nodeType = ASTNODETYPE_IF_STATIC;
		result.ifNode = ParseIf(context);
	} return result;
	case TOKEN_KEYWORD_ELSE:
	{
		if (!isStatement)
			LogError(context->token->loc, "'else' only valid at statement level!"_s);

		LogError(context->token->loc, "Invalid 'else' without matching 'if'"_s);
	}
	case TOKEN_KEYWORD_WHILE:
	{
		if (!isStatement)
			LogError(context->token->loc, "'while' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_WHILE;
		result.whileNode = ParseWhile(context);
	} return result;
	case TOKEN_KEYWORD_FOR:
	{
		if (!isStatement)
			LogError(context->token->loc, "'for' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_FOR;
		result.forNode = ParseFor(context);
	} return result;
	case TOKEN_KEYWORD_CONTINUE:
	{
		if (!isStatement)
			LogError(context->token->loc, "'continue' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_CONTINUE;
		Advance(context);

		AssertToken(context->token, ';');
		Advance(context);
	} return result;
	case TOKEN_KEYWORD_REMOVE:
	{
		if (!isStatement)
			LogError(context->token->loc, "'remove' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_REMOVE;
		Advance(context);

		AssertToken(context->token, ';');
		Advance(context);
	} return result;
	case TOKEN_KEYWORD_BREAK:
	{
		if (!isStatement)
			LogError(context->token->loc, "'break' only valid at statement level!"_s);

		result.nodeType = ASTNODETYPE_BREAK;
		Advance(context);

		AssertToken(context->token, ';');
		Advance(context);
	} return result;
	case TOKEN_KEYWORD_RETURN:
	{
		if (!isStatement)
			LogError(context->token->loc, "'return' only valid at statement level!"_s);

		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_RETURN;
		if (context->token->type == ';')
			result.returnNode.expression = nullptr;
		else {
			result.returnNode.expression = PNewTreeNode(context);
			*result.returnNode.expression = ParseExpression(context, -1, false);
		}

		AssertToken(context->token, ';');
		Advance(context);
	} return result;
	case TOKEN_KEYWORD_DEFER:
	{
		if (!isStatement)
			LogError(context->token->loc, "'defer' only valid at statement level!"_s);

		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_DEFER;
		result.deferNode.expression = PNewTreeNode(context);
		*result.deferNode.expression = ParseExpression(context, -1, true);
	} return result;
	case TOKEN_KEYWORD_USING:
	{
		if (!isStatement)
			LogError(context->token->loc, "'union' not valid on this context!"_s);

		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_USING;
		result.usingNode.expression = PNewTreeNode(context);
		*result.usingNode.expression = ParseExpression(context, -1, true);
	} return result;
	case TOKEN_DIRECTIVE_BREAK:
	{
		if (!isStatement)
			LogError(context->token->loc, "Compiler breakpoint only valid at statement level"_s);

		Advance(context);
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_COMPILER_BREAKPOINT;

		AssertToken(context->token, '(');
		Advance(context);

		AssertToken(context->token, TOKEN_IDENTIFIER);
		String breakpointTypeStr = TokenToString(*context->token);
		SourceLocation stringLoc = context->token->loc;
		Advance(context);

		AssertToken(context->token, ')');
		Advance(context);

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

		AssertToken(context->token, ';');
		Advance(context);
	} return result;
	case TOKEN_DIRECTIVE_OPERATOR:
	{
		if (!isStatement)
			LogError(context->token->loc, "operator declaration is only valid at statement level"_s);

		Advance(context);

		enum TokenType op = context->token->type;
		if (op < TOKEN_OP_Begin || op > TOKEN_OP_End)
			UNEXPECTED_TOKEN_ERROR(context->token);
		Advance(context);

		AssertToken(context->token, TOKEN_OP_STATIC_DEF);
		Advance(context);

		bool isInline = false;
		if (context->token->type == TOKEN_DIRECTIVE_INLINE)
		{
			isInline = true;
			Advance(context);
		}

		ASTOperatorOverload overload = {};
		overload.loc = result.any.loc;
		overload.op = op;
		overload.isInline = isInline;
		overload.prototype = ParseProcedurePrototype(context);

		overload.astBody = PNewTreeNode(context);
		*overload.astBody = ParseExpression(context, -1, true);

		result.nodeType = ASTNODETYPE_OPERATOR_OVERLOAD;
		result.operatorOverload = overload;
	} return result;
	case TOKEN_DIRECTIVE_INCLUDE:
	{
		if (!isStatement)
			LogError(context->token->loc, "include directive is only valid at statement level"_s);

		result.nodeType = ASTNODETYPE_INCLUDE;
		Advance(context);

		AssertToken(context->token, TOKEN_LITERAL_STRING);
		result.include.filename = TokenToString(*context->token);
		Advance(context);

		AssertToken(context->token, ';');
		Advance(context);
	} return result;
	case TOKEN_DIRECTIVE_LINKLIB:
	{
		if (!isStatement)
			LogError(context->token->loc, "linklib directive is only valid at statement level"_s);

		result.nodeType = ASTNODETYPE_LINKLIB;
		Advance(context);

		AssertToken(context->token, TOKEN_LITERAL_STRING);
		result.linklib.filename = TokenToString(*context->token);
		Advance(context);

		AssertToken(context->token, ';');
		Advance(context);
	} return result;
	default:
	{
		if (!IsOperatorToken(context->token->type))
			UNEXPECTED_TOKEN_ERROR(context->token);
		// Operators are handled in the loop below.
	}
	}

	// Binary/unary operators loop
	while (true) {
		if (result.nodeType == ASTNODETYPE_INVALID) {
			// If we have no left hand, try unary operation
			ASTUnaryOperation unaryOp = result.unaryOperation;
			bool success = TryParseUnaryOperation(context, precedence, &unaryOp);
			if (!success)
				LogError(context->token->loc, "Invalid expression!"_s);
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
			bool success = TryParseBinaryOperation(context, result, precedence, &exp);
			if (success) {
				result = exp;
				if (IsASTExpressionAStatement(&result))
					break;
				else
					continue;
			}
		}

		if (context->token->type == ',' && GetOperatorPrecedence(',') >= precedence) {
			Advance(context);

			ASTExpression mux;
			mux.any.loc = result.any.loc;
			mux.typeTableIdx = TYPETABLEIDX_Unset;
			mux.nodeType = ASTNODETYPE_MULTIPLE_EXPRESSIONS;

			ASTExpression *first = PNewTreeNode(context);
			*first = result;

			ASTExpression second = ParseExpression(context, GetOperatorPrecedence(','), false);
			if (second.nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				DynamicArrayInit(&mux.multipleExpressions.array, second.multipleExpressions.array.size + 1);
				*DynamicArrayAdd(&mux.multipleExpressions.array) = first;
				for (int i = 0; i < second.multipleExpressions.array.size; ++i)
					*DynamicArrayAdd(&mux.multipleExpressions.array) = second.multipleExpressions.array[i];
			}
			else {
				DynamicArrayInit(&mux.multipleExpressions.array, 2);
				ASTExpression *sec = PNewTreeNode(context);
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
		AssertToken(context->token, ';');
		Advance(context);
	}

	return result;
}

void ParseJobProc(u32 jobIdx, void *args)
{
	ParseJobArgs *argsStruct = (ParseJobArgs *)args;
	u32 fileIdx = argsStruct->fileIdx;

	PContext *context = ALLOC(LinearAllocator, PContext);
	context->jobIdx = jobIdx;
	context->fileIdx = fileIdx;
	context->token = nullptr;
	context->currentTokenIdx = 0;
	BucketArrayInit(&context->tokens);
	DynamicArrayInit(&context->astRoot.block.statements, 4096);
	BucketArrayInit(&context->astTreeNodes);
	BucketArrayInit(&context->astTypes);

	Job *runningJob = GetCurrentJob(context);
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->description = SStringConcat("P:"_s, g_context->sourceFiles[fileIdx].name);
#endif

	TokenizeFile(context, fileIdx);

	context->token = &context->tokens[context->currentTokenIdx];

	DynamicArray<ASTExpression, LinearAllocator> *statements =
		&context->astRoot.block.statements;
	while (context->token->type != TOKEN_END_OF_FILE) {
		ASTExpression *statement = DynamicArrayAdd(statements);
		*statement = ParseExpression(context, -1, true);
		GenerateTypeCheckJobs(statement);
	}

	if (g_context->config.logAST)
		PrintAST(context);

	FinishCurrentJob(context);
}

void ParserMain()
{
	DynamicArrayInit(&g_context->sourceFiles, 64);

	MTQueueInit<HeapAllocator>(&g_context->readyJobs, 2048);

	for (int i = 0; i < YIELDREASON_Count; ++i)
		DynamicArrayInit(&g_context->waitingJobsByReason[i].unsafe, 64);
}
