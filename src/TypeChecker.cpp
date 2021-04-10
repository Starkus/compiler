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
	TYPETABLEIDX_COUNT,
};

enum TypeCategory
{
	TYPECATEGORY_INTEGER,
	TYPECATEGORY_FLOATING,
	TYPECATEGORY_STRUCT,
	TYPECATEGORY_POINTER
};

struct TypeInfo;

struct TypeInfoInteger
{
	s32 isSigned;
};

struct StructMember
{
	String name;
	s64 typeTableIdx;
	u64 offset;
};
struct TypeInfoStruct
{
	String name;
	DynamicArray<StructMember> members;
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
	s64 typeTableIdx;
};

struct TCProcedure
{
	String name;
	DynamicArray<TCVariable> parameters;
	s64 returnTypeTableIdx;
};

struct TCScope
{
	DynamicArray<TCVariable> variables;
	DynamicArray<TCProcedure> procedures;
	DynamicArray<s64> typeIndices;
};

struct TCContext
{
	ASTRoot *root;

	DynamicArray<TCScope> stack;

	s64 currentReturnTypeIdx;
};

void PushScope(TCContext *context)
{
	TCScope *newScope = DynamicArrayAdd<TCScope>(&context->stack, realloc);

	DynamicArrayInit<TCVariable>(&newScope->variables, 128, malloc);
	DynamicArrayInit<TCProcedure>(&newScope->procedures, 128, malloc);
	DynamicArrayInit<s64>(&newScope->typeIndices, 128, malloc);
}

void PopScope(TCContext *context)
{
	--context->stack.size;
}

s64 FindTypeInTable(TCContext *context, ASTType *astType)
{
	DynamicArray<TypeInfo> &typeTable = context->root->typeTable;

	if (StringEquals(astType->name, "s8"_s))
		return TYPETABLEIDX_S8;
	else if (StringEquals(astType->name, "s16"_s))
		return TYPETABLEIDX_S16;
	else if (StringEquals(astType->name, "s32"_s))
		return TYPETABLEIDX_S32;
	else if (StringEquals(astType->name, "s64"_s))
		return TYPETABLEIDX_S64;
	else if (StringEquals(astType->name, "u8"_s))
		return TYPETABLEIDX_U8;
	else if (StringEquals(astType->name, "u16"_s))
		return TYPETABLEIDX_U16;
	else if (StringEquals(astType->name, "u32"_s))
		return TYPETABLEIDX_U32;
	else if (StringEquals(astType->name, "u64"_s))
		return TYPETABLEIDX_U64;
	else if (StringEquals(astType->name, "f32"_s))
		return TYPETABLEIDX_F32;
	else if (StringEquals(astType->name, "f64"_s))
		return TYPETABLEIDX_F64;
	else if (StringEquals(astType->name, "bool"_s))
		return TYPETABLEIDX_BOOL;
	else
	{
		for (int i = TYPETABLEIDX_COUNT; i < typeTable.size; ++i)
		{
			TypeInfo *t = &typeTable[i];
			if (t->typeCategory == TYPECATEGORY_STRUCT &&
					StringEquals(astType->name, t->structInfo.name))
				return i;
		}
	}

	PrintError(astType, TPrintF("Type \"%.*s\" not found!", astType->name.size, astType->name.data));

	return -1;
}

bool CheckTypesMatch(TCContext *context, s64 left, s64 right)
{
	if (left < 0 || right < 0)
		return false;

	TypeInfo *leftTypeInfo  = &context->root->typeTable[left];
	TypeInfo *rightTypeInfo = &context->root->typeTable[right];

	if (leftTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
		rightTypeInfo->typeCategory == TYPECATEGORY_STRUCT)
	{
		return left == right;
	}
	else if (leftTypeInfo->typeCategory == TYPECATEGORY_POINTER)
	{
		if (rightTypeInfo->typeCategory != TYPECATEGORY_POINTER)
			return false;

		return leftTypeInfo->pointerInfo.pointedTypeTableIdx ==
			  rightTypeInfo->pointerInfo.pointedTypeTableIdx;
	}
	else if (leftTypeInfo->typeCategory == TYPECATEGORY_INTEGER)
	{
		if (right == TYPETABLEIDX_NUMBER || right == TYPETABLEIDX_BOOL)
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
		if (right == TYPETABLEIDX_FLOATING || right == TYPETABLEIDX_NUMBER)
			return true;

		if (rightTypeInfo->typeCategory != TYPECATEGORY_FLOATING ||
			rightTypeInfo->typeCategory != TYPECATEGORY_INTEGER)
			return false;

		return true;
	}
	else if (left == TYPETABLEIDX_NUMBER)
	{
		if (right == TYPETABLEIDX_NUMBER)
			return true;

		if (rightTypeInfo->typeCategory == TYPECATEGORY_INTEGER)
			return true;
		return false;
	}
	else if (left == TYPETABLEIDX_FLOATING)
	{
		if (right == TYPETABLEIDX_FLOATING || right == TYPETABLEIDX_NUMBER)
			return true;

		if (rightTypeInfo->typeCategory == TYPECATEGORY_FLOATING ||
			rightTypeInfo->typeCategory == TYPECATEGORY_INTEGER)
			return true;
		return false;
	}
	else if (left == TYPETABLEIDX_BOOL)
	{
		if (right == TYPETABLEIDX_BOOL || right == TYPETABLEIDX_NUMBER)
			return true;
		if (rightTypeInfo->typeCategory == TYPECATEGORY_INTEGER)
			return true;
		return false;
	}

	return false;
}

void TypeCheckType(TCContext *context, ASTType *astType)
{
	astType->typeTableIdx = FindTypeInTable(context, astType);

	// Search backwards so we find types higher in the stack first.
	for (s64 stackIdx = context->stack.size - 1; stackIdx >= 0; --stackIdx)
	{
		TCScope *currentScope = &context->stack[stackIdx];
		for (int i = 0; i < currentScope->typeIndices.size; ++i)
		{
			if (astType->typeTableIdx == currentScope->typeIndices[i])
				return;
		}
	}

	PrintError(astType, TPrintF("Type \"%.*s\" not found!", astType->name.size, astType->name.data));
}

void TypeCheckExpression(TCContext *context, ASTExpression *expression)
{
	switch (expression->type)
	{
	case ASTNODETYPE_BLOCK:
	{
		PushScope(context);

		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			TypeCheckExpression(context, &expression->block.statements[i]);
		}

		PopScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		String varName = expression->variableDeclaration.name;
		// Check if already exists
		TCScope *stackTop = &context->stack[context->stack.size - 1];
		for (s64 i = 0; i < (s64)stackTop->variables.size; ++i)
		{
			TCVariable currentVar = stackTop->variables[i];
			if (StringEquals(varName, currentVar.name))
			{
				PrintError(&expression->any, TPrintF("Duplicate variable \"%.*s\"", varName.size,
							varName.data));
			}
		}

		TypeCheckType(context, expression->variableDeclaration.type);

		TCVariable variable;
		variable.name = varName;
		variable.typeTableIdx = expression->variableDeclaration.type->typeTableIdx;
		*DynamicArrayAdd<TCVariable>(&stackTop->variables, realloc) = variable;

		if (expression->variableDeclaration.value)
		{
			TypeCheckExpression(context, expression->variableDeclaration.value);
			s64 valueTypeIdx = expression->variableDeclaration.value->typeTableIdx;
			if (!CheckTypesMatch(context, variable.typeTableIdx, valueTypeIdx))
			{
				PrintError(&expression->any, "Variable declaration type and initial type don't match"_s);
			}
		}

		expression->typeTableIdx = variable.typeTableIdx;
		return;
	} break;
	case ASTNODETYPE_STRUCT_DECLARATION:
	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_STRUCT;
		t.structInfo.name = expression->structNode.name;
		t.size = 0;
		DynamicArrayInit<StructMember>(&t.structInfo.members, 16, malloc);

		for (int memberIdx = 0; memberIdx < expression->structNode.members.size; ++memberIdx)
		{
			StructMember *member = DynamicArrayAdd<StructMember>(&t.structInfo.members, realloc);
			member->name = expression->structNode.members[memberIdx].name;
			TypeCheckType(context, expression->structNode.members[memberIdx].type);
			member->typeTableIdx = expression->structNode.members[memberIdx].type->typeTableIdx;
			// @Todo: alignment.
			member->offset = t.size;
			t.size += context->root->typeTable[member->typeTableIdx].size;
		}

		s64 typeTableIdx = context->root->typeTable.size;
		*DynamicArrayAdd<TypeInfo>(&context->root->typeTable, realloc) = t;

		*DynamicArrayAdd<s64>(&context->stack[context->stack.size - 1].typeIndices, realloc) = typeTableIdx;

		expression->typeTableIdx = typeTableIdx;
		return;
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		String procName = expression->variableDeclaration.name;
		// Check if already exists
		TCScope *stackTop = &context->stack[context->stack.size - 1];
		for (s64 i = 0; i < (s64)stackTop->procedures.size; ++i)
		{
			TCProcedure currentProc = stackTop->procedures[i];
			if (StringEquals(procName, currentProc.name))
			{
				PrintError(&expression->any, TPrintF("Duplicate procedure \"%.*s\"", procName.size,
							procName.data));
			}
		}

		TCProcedure procedure;
		procedure.name = procName;

		PushScope(context);

		// Parameters
		DynamicArrayInit(&procedure.parameters, 8, malloc);
		for (int i = 0; i < expression->procedureDeclaration.parameters.size; ++i)
		{
			ASTVariableDeclaration *parameter = &expression->procedureDeclaration.parameters[i];
			ASTExpression ex = {};
			ex.type = ASTNODETYPE_VARIABLE_DECLARATION;
			ex.variableDeclaration = *parameter;
			TypeCheckExpression(context, &ex);

			TCVariable *param = DynamicArrayAdd(&procedure.parameters, realloc);
			param->name = parameter->name;
			param->typeTableIdx = parameter->type->typeTableIdx;
		}

		s64 returnTypeIdx = -1;
		if (expression->procedureDeclaration.returnType)
		{
			TypeCheckType(context, expression->procedureDeclaration.returnType);
			returnTypeIdx = expression->procedureDeclaration.returnType->typeTableIdx;
		}

		s64 oldReturnTypeIdx = context->currentReturnTypeIdx;
		context->currentReturnTypeIdx = returnTypeIdx;

		TypeCheckExpression(context, expression->procedureDeclaration.body);

		context->currentReturnTypeIdx = oldReturnTypeIdx;
		PopScope(context);

		procedure.returnTypeTableIdx = returnTypeIdx;
		*DynamicArrayAdd<TCProcedure>(&stackTop->procedures, realloc) = procedure;

		expression->typeTableIdx = returnTypeIdx;
		return;
	} break;
	case ASTNODETYPE_RETURN:
	{
		TypeCheckExpression(context, expression->returnNode.expression);
		s64 type = expression->returnNode.expression->typeTableIdx;
		if (!CheckTypesMatch(context, type, context->currentReturnTypeIdx))
			PrintError(&expression->any, "Incorrect return type"_s);
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		String varName = expression->variable.name;
		// Search backwards so we find variables higher in the stack first.
		for (s64 stackIdx = context->stack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->stack[stackIdx];
			for (int i = 0; i < currentScope->variables.size; ++i)
			{
				TCVariable currentVar = currentScope->variables[i];
				if (StringEquals(varName, currentVar.name))
				{
					expression->typeTableIdx = currentVar.typeTableIdx;
					return;
				}
			}
		}

		PrintError(&expression->any, TPrintF("Invalid variable \"%.*s\" referenced", varName.size,
					varName.data));
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String procName = expression->procedureCall.name;

		// Search backwards so we find procedures higher in the stack first.
		TCProcedure *procedure = nullptr;
		for (s64 stackIdx = context->stack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->stack[stackIdx];
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
			PrintError(&expression->any, TPrintF("Invalid procedure \"%.*s\" called", procName.size,
						procName.data));

		expression->typeTableIdx = procedure->returnTypeTableIdx;

		// Type check arguments
		s64 neededArguments = procedure->parameters.size;
		s64 givenArguments  = expression->procedureCall.arguments.size;
		if (neededArguments != givenArguments)
			PrintError(&expression->any, TPrintF("Procedure \"%.*s\" has %d arguments but %d were given",
						procName.size, procName.data, neededArguments, givenArguments));

		for (int argIdx = 0; argIdx < neededArguments; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];
			TypeCheckExpression(context, arg);

			TCVariable param = procedure->parameters[argIdx];
			if (!CheckTypesMatch(context, arg->typeTableIdx, param.typeTableIdx))
				PrintError(&arg->any, TPrintF("When calling procedure \"%.*s\": type of parameter #%d didn't match",
							procName.size, procName.data, argIdx));
		}
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		TypeCheckExpression(context, expression->unaryOperation.expression);
		s64 expressionTypeIdx = expression->unaryOperation.expression->typeTableIdx;
		switch (expression->unaryOperation.op)
		{
		case TOKEN_OP_NOT:
			if (!CheckTypesMatch(context, TYPETABLEIDX_BOOL, expressionTypeIdx))
				PrintError(&expression->any, "Expression can't be cast to boolean"_s);
			expression->typeTableIdx = TYPETABLEIDX_BOOL;
		default:
			expression->typeTableIdx = expressionTypeIdx;
		};
		return;
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
		{
			TypeCheckExpression(context, expression->binaryOperation.leftHand);
			s64 leftHandTypeIdx = expression->binaryOperation.leftHand->typeTableIdx;

			ASSERT(expression->binaryOperation.rightHand->type == ASTNODETYPE_VARIABLE);

			TypeInfo *structTypeInfo = &context->root->typeTable[leftHandTypeIdx];
			if (structTypeInfo->typeCategory != TYPECATEGORY_STRUCT)
			{
				PrintError(&expression->any, "Left of '.' has to be a struct"_s);
			}

			String memberName = expression->binaryOperation.rightHand->variable.name;
			for (int i = 0; i < structTypeInfo->structInfo.members.size; ++i)
			{
				if (StringEquals(memberName, structTypeInfo->structInfo.members[i].name))
				{
					expression->typeTableIdx = structTypeInfo->structInfo.members[i].typeTableIdx;
					return;
				}
			}
			PrintError(&expression->any, TPrintF("\"%.*s\" is not a member of \"%.*s\"",
						memberName.size, memberName.data, structTypeInfo->structInfo.name.size,
						structTypeInfo->structInfo.name.data));
		}
		else
		{
			TypeCheckExpression(context, expression->binaryOperation.leftHand);
			TypeCheckExpression(context, expression->binaryOperation.rightHand);
			s64 leftSideTypeIdx  = expression->binaryOperation.leftHand->typeTableIdx;
			s64 rightSideTypeIdx = expression->binaryOperation.rightHand->typeTableIdx;

			if (!CheckTypesMatch(context, leftSideTypeIdx, rightSideTypeIdx))
				PrintError(&expression->any, "Type mismatch!"_s);

			switch (expression->binaryOperation.op)
			{
			case TOKEN_OP_AND:
			case TOKEN_OP_OR:
				expression->typeTableIdx = TYPETABLEIDX_BOOL;
			default:
				expression->typeTableIdx = leftSideTypeIdx;
			};
			return;
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_INTEGER:
			expression->typeTableIdx = TYPETABLEIDX_NUMBER;
			break;
		case LITERALTYPE_FLOATING:
			expression->typeTableIdx = TYPETABLEIDX_FLOATING;
			break;
		}
		return;
	} break;
	case ASTNODETYPE_IF:
	{
		TypeCheckExpression(context, expression->ifNode.condition);
		s64 conditionTypeIdx = expression->ifNode.condition->typeTableIdx;
		if (!CheckTypesMatch(context, TYPETABLEIDX_BOOL, conditionTypeIdx))
			PrintError(&expression->any, "If condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->ifNode.body);
	} break;
	case ASTNODETYPE_WHILE:
	{
		TypeCheckExpression(context, expression->whileNode.condition);
		s64 conditionTypeIdx = expression->whileNode.condition->typeTableIdx;
		if (!CheckTypesMatch(context, TYPETABLEIDX_BOOL, conditionTypeIdx))
			PrintError(&expression->any, "If condition doesn't evaluate to a boolean"_s);

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

void TypeCheckMain(ASTRoot *root)
{
	TCContext context;
	context.root = root;
	context.currentReturnTypeIdx = -1;

	DynamicArrayInit<TCScope>(&context.stack, 128, malloc);

	PushScope(&context);

	DynamicArrayInit<TypeInfo>(&root->typeTable, 2048, malloc);
	DynamicArrayAddMany<TypeInfo>(&root->typeTable, TYPETABLEIDX_COUNT, realloc);
	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_INTEGER;
		t.integerInfo.isSigned = false;

		t.size = 1;
		root->typeTable[TYPETABLEIDX_S8]  = t;
		t.size = 2;
		root->typeTable[TYPETABLEIDX_S16] = t;
		t.size = 4;
		root->typeTable[TYPETABLEIDX_S32] = t;
		t.size = 8;
		root->typeTable[TYPETABLEIDX_S64] = t;

		t.integerInfo.isSigned = true;

		t.size = 1;
		root->typeTable[TYPETABLEIDX_U8]  = t;
		t.size = 2;
		root->typeTable[TYPETABLEIDX_U16] = t;
		t.size = 4;
		root->typeTable[TYPETABLEIDX_U32] = t;
		t.size = 8;
		root->typeTable[TYPETABLEIDX_U64] = t;

		t.typeCategory = TYPECATEGORY_FLOATING;
		t.size = 4;
		root->typeTable[TYPETABLEIDX_F32] = t;
		t.size = 8;
		root->typeTable[TYPETABLEIDX_F64] = t;
	}
	for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
		*DynamicArrayAdd<s64>(&context.stack[0].typeIndices, realloc) = i;

	for (int statementIdx = 0; statementIdx < root->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &root->block.statements[statementIdx];
		TypeCheckExpression(&context, statement);
	}
}
