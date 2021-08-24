enum TypeTableIndexes
{
	TYPETABLEIDX_S8,
	TYPETABLEIDX_S16,
	TYPETABLEIDX_S32,
	TYPETABLEIDX_S64,
	TYPETABLEIDX_U8,
	TYPETABLEIDX_U16,
	TYPETABLEIDX_U32,
	TYPETABLEIDX_U64,
	TYPETABLEIDX_F32,
	TYPETABLEIDX_F64,
	TYPETABLEIDX_BOOL,
	TYPETABLEIDX_NUMBER,
	TYPETABLEIDX_FLOATING,
	TYPETABLEIDX_VOID,
	TYPETABLEIDX_COUNT,
};

enum TypeCategory
{
	TYPECATEGORY_INVALID,
	TYPECATEGORY_INTEGER,
	TYPECATEGORY_FLOATING,
	TYPECATEGORY_STRUCT
};

struct TypeInfo;

struct TypeInfoInteger
{
	s32 isSigned;
};

struct StructMember
{
	String name;
	Type type;
	u64 offset;
};
struct TypeInfoStruct
{
	String name;
	DynamicArray<StructMember, malloc, realloc> members;
};

struct TypeInfoPointer
{
	s64 pointedTypeTableIdx;
};

struct TypeInfo
{
	TypeCategory typeCategory;
	s64 size;
	union
	{
		TypeInfoInteger integerInfo;
		TypeInfoStruct structInfo;
		TypeInfoPointer pointerInfo;
	};
};

struct TCVariable
{
	String name;
	Type type;
};

struct TCProcedure
{
	String name;
	DynamicArray<TCVariable, malloc, realloc> parameters;
	Type returnType;
};

struct TCScope
{
	DynamicArray<TCVariable, malloc, realloc> variables;
	DynamicArray<TCProcedure, malloc, realloc> procedures;
	DynamicArray<s64, malloc, realloc> typeIndices;
};

void PushTCScope(Context *context)
{
	TCScope *newScope = DynamicArrayAdd(&context->tcStack);

	DynamicArrayInit(&newScope->variables, 128);
	DynamicArrayInit(&newScope->procedures, 128);
	DynamicArrayInit(&newScope->typeIndices, 128);
}

void PopTCScope(Context *context)
{
	--context->tcStack.size;
}

void FindTypeInTable(Context *context, SourceLocation loc, String typeName, Type *type)
{
	type->typeTableIdx = -1;

	DynamicArray<TypeInfo, malloc, realloc> &typeTable = context->typeTable;

	if (StringEquals(typeName, "s8"_s))
		type->typeTableIdx = TYPETABLEIDX_S8;
	else if (StringEquals(typeName, "s16"_s))
		type->typeTableIdx = TYPETABLEIDX_S16;
	else if (StringEquals(typeName, "s32"_s))
		type->typeTableIdx = TYPETABLEIDX_S32;
	else if (StringEquals(typeName, "s64"_s))
		type->typeTableIdx = TYPETABLEIDX_S64;
	else if (StringEquals(typeName, "u8"_s))
		type->typeTableIdx = TYPETABLEIDX_U8;
	else if (StringEquals(typeName, "u16"_s))
		type->typeTableIdx = TYPETABLEIDX_U16;
	else if (StringEquals(typeName, "u32"_s))
		type->typeTableIdx = TYPETABLEIDX_U32;
	else if (StringEquals(typeName, "u64"_s))
		type->typeTableIdx = TYPETABLEIDX_U64;
	else if (StringEquals(typeName, "f32"_s))
		type->typeTableIdx = TYPETABLEIDX_F32;
	else if (StringEquals(typeName, "f64"_s))
		type->typeTableIdx = TYPETABLEIDX_F64;
	else if (StringEquals(typeName, "bool"_s))
		type->typeTableIdx = TYPETABLEIDX_BOOL;
	else if (StringEquals(typeName, "void"_s))
		type->typeTableIdx = TYPETABLEIDX_VOID;
	else
	{
		for (int i = TYPETABLEIDX_COUNT; i < typeTable.size; ++i)
		{
			TypeInfo *t = &typeTable[i];
			if (t->typeCategory == TYPECATEGORY_STRUCT &&
					StringEquals(typeName, t->structInfo.name))
				type->typeTableIdx = i;
		}
	}

	if (type->typeTableIdx < 0)
		PrintError(context, loc, TPrintF("Type \"%.*s\" not found!", typeName.size, typeName.data));
}

bool CheckTypesMatch(Context *context, Type left, Type right)
{
	if (left.typeTableIdx < 0 || right.typeTableIdx < 0)
		return false;

	if (left.pointerLevels != right.pointerLevels)
		return false;

	if (left.arrayCount != right.arrayCount)
		return false;

	TypeInfo *leftTypeInfo  = &context->typeTable[left.typeTableIdx];
	TypeInfo *rightTypeInfo = &context->typeTable[right.typeTableIdx];

	if (leftTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
		rightTypeInfo->typeCategory == TYPECATEGORY_STRUCT)
	{
		return left.typeTableIdx == right.typeTableIdx;
	}
	else if (left.typeTableIdx == TYPETABLEIDX_BOOL)
	{
		if (right.typeTableIdx == TYPETABLEIDX_BOOL || right.typeTableIdx == TYPETABLEIDX_NUMBER ||
			right.typeTableIdx == TYPETABLEIDX_FLOATING)
			return true;
		if (rightTypeInfo->typeCategory == TYPECATEGORY_INTEGER ||
			rightTypeInfo->typeCategory == TYPECATEGORY_FLOATING)
			return true;
		return false;
	}
	else if (leftTypeInfo->typeCategory == TYPECATEGORY_INTEGER)
	{
		if (right.typeTableIdx == TYPETABLEIDX_NUMBER || right.typeTableIdx == TYPETABLEIDX_BOOL)
			return true;

		if (rightTypeInfo->typeCategory != TYPECATEGORY_INTEGER)
			return false;

		if (leftTypeInfo->integerInfo.isSigned != rightTypeInfo->integerInfo.isSigned)
		{
			Log("Signed-unsigned mismatch!\n");
			return false;
		}

		if (leftTypeInfo->size < rightTypeInfo->size)
		{
			Log("Trying to fit integer of size %d into integer of size %d!\n",
					rightTypeInfo->size, leftTypeInfo->size);
			return false;
		}

		return true;
	}
	else if (leftTypeInfo->typeCategory == TYPECATEGORY_FLOATING)
	{
		if (right.typeTableIdx == TYPETABLEIDX_FLOATING || right.typeTableIdx == TYPETABLEIDX_NUMBER)
			return true;

		if (rightTypeInfo->typeCategory != TYPECATEGORY_FLOATING ||
			rightTypeInfo->typeCategory != TYPECATEGORY_INTEGER)
			return false;

		return true;
	}
	else if (left.typeTableIdx == TYPETABLEIDX_NUMBER)
	{
		if (right.typeTableIdx == TYPETABLEIDX_NUMBER)
			return true;

		if (rightTypeInfo->typeCategory == TYPECATEGORY_INTEGER)
			return true;
		return false;
	}
	else if (left.typeTableIdx == TYPETABLEIDX_FLOATING)
	{
		if (right.typeTableIdx == TYPETABLEIDX_FLOATING || right.typeTableIdx == TYPETABLEIDX_NUMBER)
			return true;

		if (rightTypeInfo->typeCategory == TYPECATEGORY_FLOATING ||
			rightTypeInfo->typeCategory == TYPECATEGORY_INTEGER)
			return true;
		return false;
	}

	return false;
}

void TypeCheckType(Context *context, SourceLocation loc, String typeName, Type *type)
{
	FindTypeInTable(context, loc, typeName, type);

	// Check type is in the scope stack!
	// Search backwards so we find types higher in the stack first.
	for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
	{
		TCScope *currentScope = &context->tcStack[stackIdx];
		for (int i = 0; i < currentScope->typeIndices.size; ++i)
		{
			if (type->typeTableIdx == currentScope->typeIndices[i])
				return;
		}
	}

	// @Fix
	PrintError(context, loc, TPrintF("Type \"%.*s\" not in scope!", typeName.size, typeName.data));
}

bool IsTemporalValue(ASTExpression *expression)
{
	switch (expression->nodeType)
	{
		case ASTNODETYPE_VARIABLE:
			return false;
	}
	return true;
}

Type InferType(Type fromType)
{
	Type result = fromType;

	if (fromType.typeTableIdx == TYPETABLEIDX_NUMBER)
	{
		result.typeTableIdx = TYPETABLEIDX_S64;
	}
	else if (fromType.typeTableIdx == TYPETABLEIDX_FLOATING)
	{
		result.typeTableIdx = TYPETABLEIDX_F32;
	}

	return result;
}

void TypeCheckExpression(Context *context, ASTExpression *expression);

Type TypeCheckVariableDeclaration(Context *context, ASTVariableDeclaration *varDecl)
{
	String varName = varDecl->name;
	// Check if already exists
	TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
	for (s64 i = 0; i < (s64)stackTop->variables.size; ++i)
	{
		TCVariable currentVar = stackTop->variables[i];
		if (StringEquals(varName, currentVar.name))
		{
			PrintError(context, varDecl->loc, TPrintF("Duplicate variable \"%.*s\"", varName.size,
						varName.data));
		}
	}

	if (varDecl->typeName)
	{
		TypeCheckType(context, varDecl->loc, varDecl->typeName, &varDecl->type);
	}

	TCVariable variable;
	variable.name = varName;
	variable.type = varDecl->type;

	if (varDecl->value)
	{
		TypeCheckExpression(context, varDecl->value);
		Type valueType = varDecl->value->type;
		if (varDecl->typeName)
		{
			if (!CheckTypesMatch(context, variable.type, valueType))
			{
				PrintError(context, varDecl->loc, "Variable declaration type and initial type don't match"_s);
			}
		}
		else
		{
			variable.type = InferType(valueType);
			varDecl->type = variable.type;
		}
	}
	*DynamicArrayAdd(&stackTop->variables) = variable;

	return variable.type;
}

void TypeCheckExpression(Context *context, ASTExpression *expression)
{
	switch (expression->nodeType)
	{
	case ASTNODETYPE_BLOCK:
	{
		PushTCScope(context);

		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			TypeCheckExpression(context, &expression->block.statements[i]);
		}

		PopTCScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration *varDecl = &expression->variableDeclaration;
		Type variableType = TypeCheckVariableDeclaration(context, varDecl);
		expression->type = variableType;
	} break;
	case ASTNODETYPE_STRUCT_DECLARATION:
	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_STRUCT;
		t.structInfo.name = expression->structNode.name;
		t.size = 0;
		DynamicArrayInit(&t.structInfo.members, 16);

		for (int memberIdx = 0; memberIdx < expression->structNode.members.size; ++memberIdx)
		{
			ASTVariableDeclaration *astMember = &expression->structNode.members[memberIdx];

			StructMember *member = DynamicArrayAdd(&t.structInfo.members);
			member->name = astMember->name;
			TypeCheckType(context, astMember->loc, astMember->typeName, &astMember->type);
			member->type = astMember->type;
			// @Todo: alignment.
			member->offset = t.size;
			t.size += context->typeTable[member->type.typeTableIdx].size;
		}

		s64 typeTableIdx = context->typeTable.size;
		*DynamicArrayAdd(&context->typeTable) = t;

		*DynamicArrayAdd(&context->tcStack[context->tcStack.size - 1].typeIndices) = typeTableIdx;

		expression->type = {};
		expression->type.typeTableIdx = typeTableIdx;
		return;
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		ASTProcedureDeclaration *procDecl = &expression->procedureDeclaration;
		String procName = procDecl->name;
		// Check if already exists
		TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
		for (s64 i = 0; i < (s64)stackTop->procedures.size; ++i)
		{
			TCProcedure currentProc = stackTop->procedures[i];
			if (StringEquals(procName, currentProc.name))
			{
				PrintError(context, expression->any.loc, TPrintF("Duplicate procedure \"%.*s\"", procName.size,
							procName.data));
			}
		}

		TCProcedure procedure;
		procedure.name = procName;

		PushTCScope(context);

		// Parameters
		DynamicArrayInit(&procedure.parameters, 8);
		for (int i = 0; i < procDecl->parameters.size; ++i)
		{
			ASTVariableDeclaration *astParam = &procDecl->parameters[i];
			TypeCheckVariableDeclaration(context, astParam);

			TCVariable *tcParam = DynamicArrayAdd(&procedure.parameters);
			tcParam->name = astParam->name;
			tcParam->type = astParam->type;
		}

		Type returnType = {};
		returnType.typeTableIdx = TYPETABLEIDX_VOID;
		if (procDecl->returnTypeName)
		{
			TypeCheckType(context, expression->any.loc, procDecl->returnTypeName,
					&procDecl->returnType);
			returnType = procDecl->returnType;
		}

		Type oldReturnType = context->currentReturnType;
		context->currentReturnType = returnType;

		TypeCheckExpression(context, procDecl->body);

		context->currentReturnType = oldReturnType;
		PopTCScope(context);

		procedure.returnType = returnType;
		*DynamicArrayAdd(&stackTop->procedures) = procedure;

		expression->type = returnType;
		return;
	} break;
	case ASTNODETYPE_RETURN:
	{
		TypeCheckExpression(context, expression->returnNode.expression);
		Type type = expression->returnNode.expression->type;
		if (!CheckTypesMatch(context, type, context->currentReturnType))
			PrintError(context, expression->any.loc, "Incorrect return type"_s);
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		String varName = expression->variable.name;
		// Search backwards so we find variables higher in the stack first.
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->variables.size; ++i)
			{
				TCVariable currentVar = currentScope->variables[i];
				if (StringEquals(varName, currentVar.name))
				{
					expression->type = currentVar.type;
					return;
				}
			}
		}

		PrintError(context, expression->any.loc, TPrintF("Invalid variable \"%.*s\" referenced", varName.size,
					varName.data));
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String procName = expression->procedureCall.name;

		// Search backwards so we find procedures higher in the stack first.
		TCProcedure *procedure = nullptr;
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->procedures.size; ++i)
			{
				TCProcedure *currentProc = &currentScope->procedures[i];
				if (StringEquals(procName, currentProc->name))
				{
					procedure = currentProc;
				}
			}
		}

		if (!procedure)
			PrintError(context, expression->any.loc, TPrintF("Invalid procedure \"%.*s\" called", procName.size,
						procName.data));

		expression->type = procedure->returnType;

		// Type check arguments
		s64 neededArguments = procedure->parameters.size;
		s64 givenArguments  = expression->procedureCall.arguments.size;
		if (neededArguments != givenArguments)
			PrintError(context, expression->any.loc, TPrintF("Procedure \"%.*s\" has %d arguments but %d were given",
						procName.size, procName.data, neededArguments, givenArguments));

		for (int argIdx = 0; argIdx < neededArguments; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];
			TypeCheckExpression(context, arg);

			TCVariable param = procedure->parameters[argIdx];
			if (!CheckTypesMatch(context, arg->type, param.type))
				PrintError(context, arg->any.loc, TPrintF("When calling procedure \"%.*s\": type of parameter #%d didn't match",
							procName.size, procName.data, argIdx));
		}
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		TypeCheckExpression(context, expression->unaryOperation.expression);
		Type expressionType = expression->unaryOperation.expression->type;
		switch (expression->unaryOperation.op)
		{
		case TOKEN_OP_NOT:
			if (!CheckTypesMatch(context, { TYPETABLEIDX_BOOL }, expressionType))
				PrintError(context, expression->any.loc, "Expression can't be cast to boolean"_s);
			expression->type = { TYPETABLEIDX_BOOL };
			break;
		case TOKEN_OP_POINTER_TO:
			// Forbid pointer to temporal values
			if (IsTemporalValue(expression->unaryOperation.expression))
				PrintError(context, expression->any.loc, "Trying to get pointer to temporal value"_s);

			expression->type = expressionType;
			++expression->type.pointerLevels;
			break;
		case TOKEN_OP_DEREFERENCE:
			expression->type = expressionType;
			if (expression->type.pointerLevels < 1)
				PrintError(context, expression->any.loc, "Trying to dereference a non pointer"_s);
			--expression->type.pointerLevels;
			break;
		default:
			expression->type = expressionType;
		};
		return;
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
		{
			TypeCheckExpression(context, expression->binaryOperation.leftHand);
			s64 leftHandTypeIdx = expression->binaryOperation.leftHand->type.typeTableIdx;

			ASSERT(expression->binaryOperation.rightHand->nodeType == ASTNODETYPE_VARIABLE);

			TypeInfo *structTypeInfo = &context->typeTable[leftHandTypeIdx];
			if (structTypeInfo->typeCategory != TYPECATEGORY_STRUCT)
			{
				PrintError(context, expression->any.loc, "Left of '.' has to be a struct"_s);
			}

			String memberName = expression->binaryOperation.rightHand->variable.name;
			for (int i = 0; i < structTypeInfo->structInfo.members.size; ++i)
			{
				if (StringEquals(memberName, structTypeInfo->structInfo.members[i].name))
				{
					expression->type = structTypeInfo->structInfo.members[i].type;
					return;
				}
			}
			PrintError(context, expression->any.loc, TPrintF("\"%.*s\" is not a member of \"%.*s\"",
						memberName.size, memberName.data, structTypeInfo->structInfo.name.size,
						structTypeInfo->structInfo.name.data));
		}
		else
		{
			TypeCheckExpression(context, expression->binaryOperation.leftHand);
			TypeCheckExpression(context, expression->binaryOperation.rightHand);
			Type leftSideType  = expression->binaryOperation.leftHand->type;
			Type rightSideType = expression->binaryOperation.rightHand->type;

			if (!CheckTypesMatch(context, leftSideType, rightSideType))
				PrintError(context, expression->any.loc, "Type mismatch!"_s);

			switch (expression->binaryOperation.op)
			{
			case TOKEN_OP_AND:
			case TOKEN_OP_OR:
				expression->type = { TYPETABLEIDX_BOOL };
			default:
				expression->type = leftSideType;
			};
			return;
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_INTEGER:
			expression->type = { TYPETABLEIDX_NUMBER };
			break;
		case LITERALTYPE_FLOATING:
			expression->type = { TYPETABLEIDX_FLOATING };
			break;
		}
		return;
	} break;
	case ASTNODETYPE_IF:
	{
		TypeCheckExpression(context, expression->ifNode.condition);
		Type conditionType = expression->ifNode.condition->type;
		if (!CheckTypesMatch(context, { TYPETABLEIDX_BOOL }, conditionType))
			PrintError(context, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->ifNode.body);
	} break;
	case ASTNODETYPE_WHILE:
	{
		TypeCheckExpression(context, expression->whileNode.condition);
		Type conditionType = expression->whileNode.condition->type;
		if (!CheckTypesMatch(context, { TYPETABLEIDX_BOOL }, conditionType))
			PrintError(context, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->ifNode.body);
	} break;
	case ASTNODETYPE_BREAK:
	{
	} break;
	default:
	{
		Log("COMPILER ERROR! Unknown expression type on type checking\n");
		CRASH;
	} break;
	}
}

void TypeCheckMain(Context *context)
{
	context->currentReturnType = {};
	context->currentReturnType.typeTableIdx = -1;

	DynamicArrayInit(&context->tcStack, 128);

	PushTCScope(context);

	DynamicArrayInit(&context->typeTable, 2048);
	DynamicArrayAddMany(&context->typeTable, TYPETABLEIDX_COUNT);
	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_INTEGER;
		t.integerInfo.isSigned = true;

		t.size = 1;
		context->typeTable[TYPETABLEIDX_S8]  = t;
		t.size = 2;
		context->typeTable[TYPETABLEIDX_S16] = t;
		t.size = 4;
		context->typeTable[TYPETABLEIDX_S32] = t;
		t.size = 8;
		context->typeTable[TYPETABLEIDX_S64] = t;

		t.integerInfo.isSigned = false;

		t.size = 1;
		context->typeTable[TYPETABLEIDX_U8]  = t;
		context->typeTable[TYPETABLEIDX_BOOL]  = t;
		t.size = 2;
		context->typeTable[TYPETABLEIDX_U16] = t;
		t.size = 4;
		context->typeTable[TYPETABLEIDX_U32] = t;
		t.size = 8;
		context->typeTable[TYPETABLEIDX_U64] = t;

		t.typeCategory = TYPECATEGORY_FLOATING;
		t.size = 4;
		context->typeTable[TYPETABLEIDX_F32] = t;
		t.size = 8;
		context->typeTable[TYPETABLEIDX_F64] = t;

		t.typeCategory = TYPECATEGORY_INVALID;
		t.size = 0;
		context->typeTable[TYPETABLEIDX_VOID] = t;
	}
	for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
		*DynamicArrayAdd(&context->tcStack[0].typeIndices) = i;

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		TypeCheckExpression(context, statement);
	}
}
