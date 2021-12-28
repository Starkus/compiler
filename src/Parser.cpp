ASTExpression ParseExpression(Context *context, s32 precedence);
ASTExpression ParseStatement(Context *context);
ASTVariableDeclaration ParseVariableDeclaration(Context *context);

enum ValueFlags
{
	VALUEFLAGS_IS_USED              = 1,
	VALUEFLAGS_FORCE_REGISTER       = 2,
	VALUEFLAGS_FORCE_MEMORY         = 4,
	VALUEFLAGS_IS_MEMORY            = 8,
	VALUEFLAGS_IS_ALLOCATED         = 16,
	VALUEFLAGS_IS_EXTERNAL          = 32,
	VALUEFLAGS_ON_STATIC_STORAGE    = 64,
	VALUEFLAGS_BASE_RELATIVE        = 128,
	VALUEFLAGS_HAS_PUSH_INSTRUCTION = 256
};

struct Value
{
	String name;
	s64 typeTableIdx;
	u32 flags;

	// IRGen
	union
	{
		s32 allocatedRegister;
		s32 stackOffset;
	};
};

enum ConstantType
{
	CONSTANTTYPE_INVALID = 0,
	CONSTANTTYPE_INTEGER,
	CONSTANTTYPE_FLOATING
};
struct Constant
{
	ConstantType type;
	union
	{
		s64 valueAsInt;
		f64 valueAsFloat;
	};
};

struct IRInstruction;
struct Procedure
{
	String name;
	DynamicArray<u32, malloc, realloc> parameterValues;
	ASTExpression *astBody;
	u32 returnValueIdx;
	s64 typeTableIdx; // Type of the procedure

	// IRGen
	BucketArray<IRInstruction, 256, malloc, realloc> instructions;
	s64 allocatedParameterCount;
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

u32 NewValue(Context *context, s64 typeTableIdx, u32 flags)
{
	ASSERT(typeTableIdx != 0);
	u64 idx = BucketArrayCount(&context->values);
	Value *result = BucketArrayAdd(&context->values);
	result->name = {};
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 NewValue(Context *context, String name, s64 typeTableIdx, u32 flags)
{
	ASSERT(typeTableIdx != 0);
	u64 idx = BucketArrayCount(&context->values);
	Value *result = BucketArrayAdd(&context->values);
	result->name = name;
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
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
	else if (context->token->type == '(')
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
	case TOKEN_OP_BITWISE_NOT:
	case TOKEN_OP_MINUS:
	{
		enum TokenType op = context->token->type;
		result->op = op;
		Advance(context);

		s32 precedence = GetOperatorPrecedence(op);
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
	if (!IsTokenOperator(context->token))
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

inline Procedure *GetProcedure(Context *context, s32 procedureIdx)
{
	ASSERT(procedureIdx != 0);
	if (procedureIdx > 0)
		return &context->procedures[procedureIdx];
	else
		return &context->externalProcedures[-procedureIdx];
}

ASTIf ParseIf(Context *context)
{
	ASSERT(context->token->type == TOKEN_KEYWORD_IF);
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
	Advance(context);

	forNode.range = NewTreeNode(context);
	if (context->token->type == '(')
	{
		// If there are parenthesis, grab _only_ the expression inside.
		Advance(context);
		*forNode.range = ParseExpression(context, -1);
		AssertToken(context, context->token, ')');
		Advance(context);
	}
	else
		*forNode.range = ParseExpression(context, -1);

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

Array<ASTExpression *> ParseStructLiteral(Context *context)
{
	DynamicArray<ASTExpression *, FrameAlloc, FrameRealloc> members;
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

	Array<ASTExpression *> result;
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

	u64 startTokenIdx = context->currentTokenIdx;

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	varDecl.name = context->token->string;
	Advance(context);

	if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION)
		Advance(context);
	else if (context->token->type == TOKEN_OP_VARIABLE_DECLARATION_STATIC)
	{
		varDecl.isStatic = true;
		Advance(context);
	}
	else
	{
		//UnexpectedTokenError(context, context->token);
		// Rewind
		context->currentTokenIdx = startTokenIdx;
		context->token = &context->tokens[startTokenIdx];
		varDecl.name = {};
	}

	if (context->token->type == TOKEN_KEYWORD_EXTERNAL)
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

ASTProcedurePrototype ParseProcedurePrototype(Context *context)
{
	ASTProcedurePrototype prototype = {};
	prototype.loc = context->token->loc;

	DynamicArrayInit(&prototype.astParameters, 4);

	AssertToken(context, context->token, '(');
	Advance(context);
	while (context->token->type != ')')
	{
		if (context->token->type == TOKEN_OP_RANGE)
		{
			Advance(context);
			prototype.isVarargs = true;

			if (context->token->type == TOKEN_IDENTIFIER)
			{
				prototype.varargsName = context->token->string;
				Advance(context);
			}
			break;
		}

		bool isUsing = false;
		if (context->token->type == TOKEN_KEYWORD_USING)
		{
			isUsing = true;
			Advance(context);
		}
		// @Improve: separate node type for procedure parameter?
		ASTVariableDeclaration astVarDecl = ParseVariableDeclaration(context);
		astVarDecl.isUsing = isUsing;
		ASSERT(prototype.astParameters.size <= S8_MAX);

		if (astVarDecl.isStatic)
			LogError(context, context->token->loc, "Procedure parameters can't be static"_s);

		*DynamicArrayAdd(&prototype.astParameters) = astVarDecl;

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
	result.any.loc = context->token->loc;

	// Parenthesis
	if (context->token->type == '(')
	{
		Advance(context);

		result = ParseExpression(context, -1);

		AssertToken(context, context->token, ')');
		Advance(context);
	}
	else if (context->token->type == '{')
	{
		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		result.literal.type = LITERALTYPE_STRUCT;
		result.literal.members = ParseStructLiteral(context);

		AssertToken(context, context->token, '}');
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
					if (context->token->type != ',')
					{
						const String tokenTypeGot = TokenToString(context->token);
						const String errorStr = TPrintF("Expected ')' or ',' but got %S",
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
	}
	else if (context->token->type == TOKEN_LITERAL_NUMBER)
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;

		bool isHex = false;
		bool isFloating = false;
		if (context->token->begin[0] == '0')
		{
			if (context->token->begin[1] == 'x' || context->token->begin[1] == 'X')
				isHex = true;
		}

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
			if (isHex)
			{
				String numbersOnly = context->token->string;
				numbersOnly.size -= 2;
				numbersOnly.data += 2;
				// @Todo: error reporting
				result.literal.integer = IntFromStringHex(numbersOnly);
			}
			else
				// @Todo: error reporting
				result.literal.integer = IntFromString(context->token->string);
		}
		else
		{
			result.literal.type = LITERALTYPE_FLOATING;
			result.literal.floating = atof(context->token->string.data); // @Todo: replace atof
		}
		Advance(context);
	}
	else if (context->token->type == TOKEN_LITERAL_CHARACTER)
	{
		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_LITERAL;
		result.literal.type = LITERALTYPE_CHARACTER;
		result.literal.character = context->token->begin[1];
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
	else if (context->token->type == TOKEN_KEYWORD_SIZEOF)
	{
		Advance(context);

		result.any.loc = context->token->loc;
		result.nodeType = ASTNODETYPE_SIZEOF;
		result.sizeOfNode.expression = NewTreeNode(context);
		*result.sizeOfNode.expression = ParseExpression(context, -1);
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
	else if (context->token->type == TOKEN_KEYWORD_IF)
	{
		LogError(context, context->token->loc, "'if' only valid at statement level!"_s);
	}
	else if (context->token->type == TOKEN_KEYWORD_WHILE)
	{
		LogError(context, context->token->loc, "'while' only valid at statement level!"_s);
	}
	else if (context->token->type == TOKEN_KEYWORD_FOR)
	{
		LogError(context, context->token->loc, "'for' only valid at statement level!"_s);
	}
	else if (context->token->type == TOKEN_KEYWORD_DEFER)
	{
		LogError(context, context->token->loc, "'defer' only valid at statement level!"_s);
	}
	else if (context->token->type == TOKEN_KEYWORD_RETURN)
	{
		LogError(context, context->token->loc, "'return' only valid at statement level!"_s);
	}
	else if (IsTokenOperator(context->token))
	{
		// This is just to avoid falling into the else below. Operators will be handled by the
		// binary expression loop at the bottom.
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
	ASTStaticDefinition result;

	AssertToken(context, context->token, TOKEN_IDENTIFIER);
	result.name = context->token->string;
	Advance(context);

	Advance(context);

	ASTExpression expression = {};
	expression.any.loc = context->token->loc;

	if (context->token->type == '(')
	{
		expression.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;

		s32 procedureIdx = (s32)BucketArrayCount(&context->procedures);
		Procedure *procedure = BucketArrayAdd(&context->procedures);
		*procedure = {};
		procedure->returnValueIdx = U32_MAX;
		procedure->name = result.name;

		ASTProcedureDeclaration procDecl = {};
		procDecl.loc = context->token->loc;
		procDecl.procedureIdx = procedureIdx;
		procDecl.prototype = ParseProcedurePrototype(context);
		expression.procedureDeclaration = procDecl;

		if (context->token->type == ';')
			Advance(context);
		else
		{
			procedure->astBody = NewTreeNode(context);
			*procedure->astBody = ParseStatement(context);
		}
	}
	else if (context->token->type == TOKEN_KEYWORD_EXTERNAL)
	{
		Advance(context);

		expression.nodeType = ASTNODETYPE_PROCEDURE_DECLARATION;

		s32 procedureIdx = -(s32)BucketArrayCount(&context->externalProcedures);
		Procedure *procedure = BucketArrayAdd(&context->externalProcedures);
		*procedure = {};
		procedure->name = result.name;
		procedure->returnValueIdx = U32_MAX;

		ASTProcedureDeclaration procDecl = {};
		procDecl.loc = context->token->loc;
		procDecl.procedureIdx = procedureIdx;
		procDecl.prototype = ParseProcedurePrototype(context);
		expression.procedureDeclaration = procDecl;

		if (context->token->type != ';')
			LogError(context, context->token->loc, "External procedure declaration can't have a body"_s);

		Advance(context);
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
		result.ifNode = ParseIf(context);
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
			LogError(context, result.any.loc,
					"Expression after 'using' should be a variable or variable declaration"_s);
		}
	} break;
	default:
	{
		Token *next = &context->tokens[context->currentTokenIdx + 1];
		if (next->type == TOKEN_OP_STATIC_DEF)
		{
			result.nodeType = ASTNODETYPE_STATIC_DEFINITION;
			result.staticDefinition = ParseStaticDefinition(context);
			result.any.loc = result.staticDefinition.expression->any.loc;
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
		Token *next = &context->tokens[context->currentTokenIdx + 1];
		if (next->type == TOKEN_OP_STATIC_DEF)
		{
			result.nodeType = ASTNODETYPE_STATIC_DEFINITION;
			result.staticDefinition = ParseStaticDefinition(context);
			result.any.loc = result.staticDefinition.expression->any.loc;
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
		{
			LogError(context, context->token->loc, "Invalid expression in static context"_s);
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
	BucketArrayInit(&context->values);
	BucketArrayInit(&context->procedures);
	BucketArrayInit(&context->externalProcedures);
	BucketArrayInit(&context->stringLiterals);

	// Empty string
	*BucketArrayAdd(&context->stringLiterals) = {};

	// Procedure 0 is invalid
	*BucketArrayAdd(&context->procedures) = {};
	*BucketArrayAdd(&context->externalProcedures) = {};

	context->currentTokenIdx = 0;
	context->token = &context->tokens[0];
	while (context->token->type != TOKEN_END_OF_FILE)
	{
		*DynamicArrayAdd(&root->block.statements) = ParseStaticStatement(context);
	}

	return root;
}
