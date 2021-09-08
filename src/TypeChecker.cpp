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
	TYPETABLEIDX_INTEGER,
	TYPETABLEIDX_FLOATING,
	TYPETABLEIDX_VOID,

	TYPETABLEIDX_STRUCT_STRING,

	TYPETABLEIDX_COUNT,
};

enum TypeCategory
{
	TYPECATEGORY_INVALID,
	TYPECATEGORY_INTEGER,
	TYPECATEGORY_FLOATING,
	TYPECATEGORY_STRUCT,
	TYPECATEGORY_POINTER,
	TYPECATEGORY_ARRAY,
};

struct TypeInfo;

struct TypeInfoInteger
{
	s64 size;
	s32 isSigned;
};

struct TypeInfoFloating
{
	s64 size;
};

struct StructMember
{
	String name;
	s64 typeTableIdx;

	// Filled by back-end
	u64 offset;
};
struct TypeInfoStruct
{
	String name;
	DynamicArray<StructMember, malloc, realloc> members;

	// Filled by back-end
	s64 size;
};

struct TypeInfoPointer
{
	s64 pointedTypeTableIdx;
};

struct TypeInfoArray
{
	s64 elementTypeTableIdx;
	u64 count;
};

struct TypeInfo
{
	TypeCategory typeCategory;
	union
	{
		TypeInfoInteger integerInfo;
		TypeInfoFloating floatingInfo;
		TypeInfoStruct structInfo;
		TypeInfoPointer pointerInfo;
		TypeInfoArray arrayInfo;
	};
};

struct TCScope
{
	DynamicArray<Variable *, malloc, realloc> variables;
	DynamicArray<Procedure *, malloc, realloc> procedures;
	DynamicArray<s64, malloc, realloc> typeIndices;
};

String TypeInfoToString(Context *context, s64 typeTableIdx)
{
	TypeInfo *typeInfo = &context->typeTable[typeTableIdx];
	switch (typeInfo->typeCategory)
	{
	case TYPECATEGORY_STRUCT:
		return typeInfo->structInfo.name;
	case TYPECATEGORY_POINTER:
		return StringConcat("^"_s, TypeInfoToString(context, typeInfo->pointerInfo.pointedTypeTableIdx));
	case TYPECATEGORY_ARRAY:
	{
		String typeStr = TypeInfoToString(context, typeInfo->arrayInfo.elementTypeTableIdx);
		return TPrintF("[%d] %S", typeInfo->arrayInfo.count, typeStr);
	}
	case TYPECATEGORY_INTEGER:
	{
		if (typeInfo->integerInfo.isSigned) switch (typeInfo->integerInfo.size)
		{
			case 1: return "s8"_s;
			case 2: return "s16"_s;
			case 4: return "s32"_s;
			case 8: return "s64"_s;
		}
		else switch (typeInfo->integerInfo.size)
		{
			case 1: return "u8"_s;
			case 2: return "u16"_s;
			case 4: return "u32"_s;
			case 8: return "u64"_s;
		}
	} break;
	case TYPECATEGORY_FLOATING:
	{
		switch (typeInfo->floatingInfo.size)
		{
			case 4: return "f32"_s;
			case 8: return "f64"_s;
		}
	} break;
	}
	return "???TYPE"_s;
}

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

s64 FindLeafTypeInTable(Context *context, SourceLocation loc, ASTType *astType)
{
	s64 typeTableIdx = -1;

	if (StringEquals(astType->name, "s8"_s))
		typeTableIdx = TYPETABLEIDX_S8;
	else if (StringEquals(astType->name, "s16"_s))
		typeTableIdx = TYPETABLEIDX_S16;
	else if (StringEquals(astType->name, "s32"_s))
		typeTableIdx = TYPETABLEIDX_S32;
	else if (StringEquals(astType->name, "s64"_s))
		typeTableIdx = TYPETABLEIDX_S64;
	else if (StringEquals(astType->name, "u8"_s))
		typeTableIdx = TYPETABLEIDX_U8;
	else if (StringEquals(astType->name, "u16"_s))
		typeTableIdx = TYPETABLEIDX_U16;
	else if (StringEquals(astType->name, "u32"_s))
		typeTableIdx = TYPETABLEIDX_U32;
	else if (StringEquals(astType->name, "u64"_s))
		typeTableIdx = TYPETABLEIDX_U64;
	else if (StringEquals(astType->name, "f32"_s))
		typeTableIdx = TYPETABLEIDX_F32;
	else if (StringEquals(astType->name, "f64"_s))
		typeTableIdx = TYPETABLEIDX_F64;
	else if (StringEquals(astType->name, "bool"_s))
		typeTableIdx = TYPETABLEIDX_BOOL;
	else if (StringEquals(astType->name, "void"_s))
		typeTableIdx = TYPETABLEIDX_VOID;
	else
	{
		u64 tableSize = BucketArrayCount(&context->typeTable);
		for (int i = 0; i < tableSize; ++i)
		{
			TypeInfo *t = &context->typeTable[i];
			if (t->typeCategory == TYPECATEGORY_STRUCT &&
					StringEquals(astType->name, t->structInfo.name))
				typeTableIdx = i;
		}
	}

	if (typeTableIdx < 0)
		PrintError(context, loc, TPrintF("Type \"%S\" not found!", astType->name));

	return typeTableIdx;
}

bool CheckTypesMatch(Context *context, s64 leftTableIdx, s64 rightTableIdx)
{
	if (leftTableIdx == rightTableIdx)
		return true;

	TypeInfo *left  = &context->typeTable[leftTableIdx];
	TypeInfo *right = &context->typeTable[rightTableIdx];

	if (leftTableIdx == TYPETABLEIDX_BOOL)
	{
		return right->typeCategory == TYPECATEGORY_INTEGER ||
			right->typeCategory == TYPECATEGORY_FLOATING;
	}

	if (rightTableIdx == TYPETABLEIDX_INTEGER)
	{
		return left->typeCategory == TYPECATEGORY_INTEGER ||
			left->typeCategory == TYPECATEGORY_FLOATING;
	}
	else if (rightTableIdx == TYPETABLEIDX_FLOATING)
	{
		return left->typeCategory == TYPECATEGORY_FLOATING;
	}

	if (left->typeCategory != right->typeCategory)
	{
		// Allow int->float and float->int
		if ((left->typeCategory == TYPECATEGORY_INTEGER ||
			left->typeCategory == TYPECATEGORY_FLOATING) &&
			right->typeCategory == TYPECATEGORY_INTEGER ||
			right->typeCategory == TYPECATEGORY_FLOATING)
			return true;

		return false;
	}

	switch (left->typeCategory)
	{
	case TYPECATEGORY_POINTER:
	{
		// Cast any pointer to void pointer
		if (left->pointerInfo.pointedTypeTableIdx == TYPETABLEIDX_VOID)
			return true;

		return left->pointerInfo.pointedTypeTableIdx == right->pointerInfo.pointedTypeTableIdx;
	} break;
	case TYPECATEGORY_ARRAY:
	{
		return left->arrayInfo.count == right->arrayInfo.count;
	} break;
	case TYPECATEGORY_STRUCT:
	{
		return false;
	} break;
	case TYPECATEGORY_INTEGER:
	{
		if (left->integerInfo.isSigned != right->integerInfo.isSigned)
		{
			Log("Signed-unsigned mismatch!\n");
			return false;
		}
		if (left->integerInfo.size < right->integerInfo.size)
		{
			Log("Trying to fit integer of size %d into integer of size %d!\n",
					right->integerInfo.size, left->integerInfo.size);
			return false;
		}
		return true;
	} break;
	case TYPECATEGORY_FLOATING:
	{
		return true;
	} break;
	}

	return false;
}

bool AreTypeInfosEqual(TypeInfo *a, TypeInfo *b)
{
	if (a->typeCategory != b->typeCategory)
		return false;

	switch (a->typeCategory)
	{
	case TYPECATEGORY_INTEGER:
		return a->integerInfo.size == b->integerInfo.size &&
			a->integerInfo.isSigned == b->integerInfo.isSigned;
	case TYPECATEGORY_FLOATING:
		return true;
	case TYPECATEGORY_STRUCT:
		if (!StringEquals(a->structInfo.name, b->structInfo.name))
			return false;
		if (a->structInfo.members.size != b->structInfo.members.size)
			return false;
		for (int i = 0; i < a->structInfo.members.size; ++i)
			if (a->structInfo.members[i].typeTableIdx != b->structInfo.members[i].typeTableIdx)
				return false;
		return true;
	case TYPECATEGORY_POINTER:
		return a->pointerInfo.pointedTypeTableIdx == b->pointerInfo.pointedTypeTableIdx;
	case TYPECATEGORY_ARRAY:
		return a->arrayInfo.elementTypeTableIdx == b->arrayInfo.elementTypeTableIdx &&
			a->arrayInfo.count == b->arrayInfo.count;
	default:
		CRASH;
	}
	return false;
}

s64 FindOrAddTypeTableIdx(Context *context, TypeInfo typeInfo)
{
	u64 tableSize = BucketArrayCount(&context->typeTable);
	for (int i = 0; i < tableSize; ++i)
	{
		TypeInfo *t = &context->typeTable[i];
		if (AreTypeInfosEqual(&typeInfo, t))
			return i;
	}

	s64 typeTableIdx = tableSize;
	*BucketArrayAdd(&context->typeTable) = typeInfo;
	return typeTableIdx;
}

// Util TypeInfo procedures
s64 GetTypeInfoPointerOf(Context *context, s64 inType)
{
	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_POINTER;
	resultTypeInfo.pointerInfo.pointedTypeTableIdx = inType;
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

s64 TypeCheckType(Context *context, SourceLocation loc, ASTType *astType)
{
	switch (astType->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
	{
		s64 typeTableIdx = FindLeafTypeInTable(context, loc, astType);

		// Check type is in the scope stack!
		// Search backwards so we find types higher in the stack first.
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->typeIndices.size; ++i)
			{
				if (typeTableIdx == currentScope->typeIndices[i])
					return typeTableIdx;
			}
		}

		PrintError(context, loc, TPrintF("Type \"%S\" not in scope!", astType->name));
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		TypeInfo tempTypeInfo;
		tempTypeInfo.typeCategory = TYPECATEGORY_ARRAY;
		tempTypeInfo.arrayInfo.count = astType->arrayCount;
		tempTypeInfo.arrayInfo.elementTypeTableIdx = TypeCheckType(context, loc, astType->arrayType);

		s64 typeTableIdx = FindOrAddTypeTableIdx(context, tempTypeInfo);
		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		TypeInfo tempTypeInfo;
		tempTypeInfo.typeCategory = TYPECATEGORY_POINTER;
		tempTypeInfo.arrayInfo.elementTypeTableIdx = TypeCheckType(context, loc, astType->pointedType);

		s64 typeTableIdx = FindOrAddTypeTableIdx(context, tempTypeInfo);
		return typeTableIdx;
	} break;
	default:
		CRASH;
	}

	return -1;
}

bool IsTemporalValue(ASTExpression *expression)
{
	switch (expression->nodeType)
	{
		case ASTNODETYPE_VARIABLE:
			return false;
		case ASTNODETYPE_BINARY_OPERATION:
			if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
				return false;
			if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
				return false;
			break;
	}
	return true;
}

u64 InferType(u64 fromType)
{
	if (fromType == TYPETABLEIDX_INTEGER)
	{
		return TYPETABLEIDX_S64;
	}
	else if (fromType == TYPETABLEIDX_FLOATING)
	{
		return TYPETABLEIDX_F32;
	}

	return fromType;
}

void TypeCheckExpression(Context *context, ASTExpression *expression);

void TypeCheckVariableDeclaration(Context *context, ASTVariableDeclaration *varDecl)
{
	String varName = varDecl->variable->name;
	s64 *varType = &varDecl->variable->typeTableIdx;
	// Check if already exists
	TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
	for (s64 i = 0; i < (s64)stackTop->variables.size; ++i)
	{
		Variable *currentVar = stackTop->variables[i];
		if (StringEquals(varName, currentVar->name))
		{
			PrintError(context, varDecl->loc, TPrintF("Duplicate variable \"%S\"", varName));
		}
	}

	if (varDecl->astType)
	{
		*varType = TypeCheckType(context, varDecl->loc, varDecl->astType);

		if (*varType == TYPETABLEIDX_VOID)
			PrintError(context, varDecl->loc, "Variable can't be of type void!"_s);
	}

	if (varDecl->value)
	{
		TypeCheckExpression(context, varDecl->value);
		u64 valueType = varDecl->value->typeTableIdx;

		if (varDecl->astType)
		{
			if (!CheckTypesMatch(context, *varType, valueType))
			{
				String varTypeStr = TypeInfoToString(context, *varType);
				String valueTypeStr = TypeInfoToString(context, valueType);
				PrintError(context, varDecl->loc, TPrintF(
							"Variable declaration type and initial type don't match (variable "
							"is %S and initial value is %S", varTypeStr, valueTypeStr));
			}
		}
		else
		{
			if (valueType == TYPETABLEIDX_VOID)
				PrintError(context, varDecl->loc, "Variable can't be of type void!"_s);

			*varType = InferType(valueType);
		}
	}
	*DynamicArrayAdd(&stackTop->variables) = varDecl->variable;
}

enum ReturnCheckResult
{
	RETURNCHECKRESULT_NEVER,
	RETURNCHECKRESULT_SOMETIMES,
	RETURNCHECKRESULT_ALWAYS
};
ReturnCheckResult CheckIfReturnsValue(Context *context, ASTExpression *expression)
{
	switch (expression->nodeType)
	{
	case ASTNODETYPE_RETURN:
	{
		return RETURNCHECKRESULT_ALWAYS;
	}
	case ASTNODETYPE_BLOCK:
	{
		ReturnCheckResult result = RETURNCHECKRESULT_NEVER;
		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			ReturnCheckResult statementResult = CheckIfReturnsValue(context, &expression->block.statements[i]);
			if (statementResult > result)
			{
				result = statementResult;
			}
		}
		return result;
	}
	case ASTNODETYPE_IF:
	{
		ReturnCheckResult ifStatement = CheckIfReturnsValue(context, expression->ifNode.body);
		ReturnCheckResult elseStatement = RETURNCHECKRESULT_NEVER;
		if (expression->ifNode.elseBody)
			elseStatement = CheckIfReturnsValue(context, expression->ifNode.elseBody);

		if (ifStatement == RETURNCHECKRESULT_ALWAYS && elseStatement == RETURNCHECKRESULT_ALWAYS)
			return RETURNCHECKRESULT_ALWAYS;

		if (ifStatement != RETURNCHECKRESULT_NEVER || elseStatement != RETURNCHECKRESULT_NEVER)
			return RETURNCHECKRESULT_SOMETIMES;
	}
	case ASTNODETYPE_WHILE:
	{
		return CheckIfReturnsValue(context, expression->whileNode.body);
	}
	// @Todo: Add For loops here once they exist!
	}
	return RETURNCHECKRESULT_NEVER;
}

void TypeCheckExpression(Context *context, ASTExpression *expression)
{
	switch (expression->nodeType)
	{
	case ASTNODETYPE_BLOCK:
	{
		PushTCScope(context);

		for (int i = 0; i < expression->block.statements.size; ++i)
			TypeCheckExpression(context, &expression->block.statements[i]);

		PopTCScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration *varDecl = &expression->variableDeclaration;
		TypeCheckVariableDeclaration(context, varDecl);
		expression->typeTableIdx = varDecl->variable->typeTableIdx;
	} break;
	case ASTNODETYPE_STRUCT_DECLARATION:
	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_STRUCT;
		t.structInfo.name = expression->structNode.name;
		DynamicArrayInit(&t.structInfo.members, 16);

		for (int memberIdx = 0; memberIdx < expression->structNode.members.size; ++memberIdx)
		{
			ASTStructMemberDeclaration *astMember = &expression->structNode.members[memberIdx];

			StructMember *member = DynamicArrayAdd(&t.structInfo.members);
			member->name = astMember->name;
			member->typeTableIdx = TypeCheckType(context, astMember->loc, astMember->astType);
		}

		s64 typeTableIdx = BucketArrayCount(&context->typeTable);
		*BucketArrayAdd(&context->typeTable) = t;

		*DynamicArrayAdd(&context->tcStack[context->tcStack.size - 1].typeIndices) = typeTableIdx;
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		ASTProcedureDeclaration *procDecl = &expression->procedureDeclaration;
		Procedure *procedure = procDecl->procedure;
		// Check if already exists
		TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
		for (s64 i = 0; i < (s64)stackTop->procedures.size; ++i)
		{
			Procedure *currentProc = stackTop->procedures[i];
			if (StringEquals(procedure->name, currentProc->name))
			{
				PrintError(context, expression->any.loc, TPrintF("Duplicate procedure \"%S\"",
							procedure->name));
			}
		}

		procedure->requiredParameterCount = 0;

		PushTCScope(context);

		// Parameters
		bool beginOptionalParameters = false;
		DynamicArrayInit(&procedure->parameters, 8);
		for (int i = 0; i < procDecl->astParameters.size; ++i)
		{
			ASTVariableDeclaration *astParam = &procDecl->astParameters[i];
			TypeCheckVariableDeclaration(context, astParam);
			ProcedureParameter *procParam = DynamicArrayAdd(&procedure->parameters);
			procParam->variable = astParam->variable;
			procParam->defaultValue = astParam->value;

			if (!astParam->value)
			{
				if (beginOptionalParameters)
					PrintError(context, astParam->loc, "Non-optional parameter after optional parameter found!"_s);

				++procedure->requiredParameterCount;
			}
			else
			{
				beginOptionalParameters = true;
			}
		}

		s64 returnType = TYPETABLEIDX_VOID;
		if (procDecl->astReturnType)
		{
			returnType = TypeCheckType(context, expression->any.loc, procDecl->astReturnType);
		}
		procedure->returnTypeTableIdx = returnType;

		u64 oldReturnType = context->tcCurrentReturnType;
		context->tcCurrentReturnType = returnType;

		if (procedure->astBody)
			TypeCheckExpression(context, procedure->astBody);

		context->tcCurrentReturnType = oldReturnType;
		PopTCScope(context);

		procedure->returnTypeTableIdx = returnType;
		*DynamicArrayAdd(&stackTop->procedures) = procedure;

		//expression->typeTableIdx = returnType;

		// Check all paths return
		if (procedure->astBody && returnType != TYPETABLEIDX_VOID)
		{
			ReturnCheckResult result = CheckIfReturnsValue(context, procedure->astBody);
			if (result == RETURNCHECKRESULT_SOMETIMES)
				PrintError(context, expression->any.loc, "Procedure doesn't always return a value"_s);
			else if (result == RETURNCHECKRESULT_NEVER)
				PrintError(context, expression->any.loc, "Procedure has to return a value"_s);
		}
	} break;
	case ASTNODETYPE_RETURN:
	{
		TypeCheckExpression(context, expression->returnNode.expression);
		u64 typeTableIdx = expression->returnNode.expression->typeTableIdx;
		if (!CheckTypesMatch(context, context->tcCurrentReturnType, typeTableIdx))
			PrintError(context, expression->any.loc, "Incorrect return type"_s);
	} break;
	case ASTNODETYPE_DEFER:
	{
		TypeCheckExpression(context, expression->deferNode.expression);
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
				Variable *currentVar = currentScope->variables[i];
				if (StringEquals(varName, currentVar->name))
				{
					expression->variable.variable = currentVar;
					expression->typeTableIdx = currentVar->typeTableIdx;
					return;
				}
			}
		}

		PrintError(context, expression->any.loc, TPrintF("Invalid variable \"%S\" referenced", varName));
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String procName = expression->procedureCall.name;

		// Search backwards so we find procedures higher in the stack first.
		Procedure *procedure = nullptr;
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->procedures.size; ++i)
			{
				Procedure *currentProc = currentScope->procedures[i];
				if (StringEquals(procName, currentProc->name))
				{
					procedure = currentProc;
					break;
				}
			}
		}

		if (!procedure)
			PrintError(context, expression->any.loc, TPrintF("Invalid procedure \"%S\" called", procName));

		expression->procedureCall.procedure = procedure;
		expression->typeTableIdx = procedure->returnTypeTableIdx;

		// Type check arguments
		s64 requiredArguments = procedure->requiredParameterCount;
		s64 totalArguments = procedure->parameters.size;
		s64 givenArguments  = expression->procedureCall.arguments.size;
		if (procedure->isVarargs)
		{
			if (requiredArguments > givenArguments)
				PrintError(context, expression->any.loc,
						TPrintF("Procedure \"%S\" needs at least %d arguments but only %d were given",
							procName, requiredArguments, givenArguments));
		}
		else
		{
			if (requiredArguments > givenArguments)
				PrintError(context, expression->any.loc,
						TPrintF("Procedure \"%S\" needs at least %d arguments but only %d were given",
						procName, requiredArguments, givenArguments));

			if (givenArguments > totalArguments)
				PrintError(context, expression->any.loc,
						TPrintF("Procedure \"%S\" needs %d arguments but %d were given",
						procName, totalArguments, givenArguments));
		}

		for (int argIdx = 0; argIdx < givenArguments; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];
			TypeCheckExpression(context, arg);

			Variable *param = procedure->parameters[argIdx].variable;
			if (!CheckTypesMatch(context, param->typeTableIdx, arg->typeTableIdx))
				PrintError(context, arg->any.loc, TPrintF("When calling procedure \"%S\": type of parameter #%d didn't match",
							procName, argIdx));
		}
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		TypeCheckExpression(context, expression->unaryOperation.expression);
		u64 expressionType = expression->unaryOperation.expression->typeTableIdx;
		switch (expression->unaryOperation.op)
		{
		case TOKEN_OP_NOT:
		{
			if (!CheckTypesMatch(context, TYPETABLEIDX_BOOL, expressionType))
				PrintError(context, expression->any.loc, "Expression can't be cast to boolean"_s);
			expression->typeTableIdx = TYPETABLEIDX_BOOL;
		} break;
		case TOKEN_OP_POINTER_TO:
		{
			// Forbid pointer to temporal values
			if (IsTemporalValue(expression->unaryOperation.expression))
				PrintError(context, expression->any.loc, "Trying to get pointer to temporal value"_s);

			TypeInfo newTypeInfo;
			newTypeInfo.typeCategory = TYPECATEGORY_POINTER;
			newTypeInfo.pointerInfo.pointedTypeTableIdx = expressionType;
			expression->typeTableIdx = FindOrAddTypeTableIdx(context, newTypeInfo);
		} break;
		case TOKEN_OP_DEREFERENCE:
		{
			TypeInfo *expressionTypeInfo = &context->typeTable[expressionType];
			if (expressionTypeInfo->typeCategory != TYPECATEGORY_POINTER)
				PrintError(context, expression->any.loc, "Trying to dereference a non pointer"_s);
			expression->typeTableIdx = expressionTypeInfo->pointerInfo.pointedTypeTableIdx;
		} break;
		default:
		{
			expression->typeTableIdx = expressionType;
		}
		};
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;
		ASTExpression *rightHand = expression->binaryOperation.rightHand;

		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
		{
			TypeCheckExpression(context, leftHand);
			s64 leftHandTypeIdx = leftHand->typeTableIdx;

			ASSERT(rightHand->nodeType == ASTNODETYPE_VARIABLE);
			// Augment right hand to STRUCT_MEMBER node
			rightHand->nodeType = ASTNODETYPE_STRUCT_MEMBER;

			TypeInfo *structTypeInfo = &context->typeTable[leftHandTypeIdx];
			if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
			{
				s64 pointedTypeIdx = structTypeInfo->pointerInfo.pointedTypeTableIdx;
				structTypeInfo = &context->typeTable[pointedTypeIdx];
			}

			if (structTypeInfo->typeCategory != TYPECATEGORY_STRUCT)
			{
				PrintError(context, expression->any.loc, "Left of '.' has to be a struct"_s);
			}

			String memberName = rightHand->structMember.name;
			for (int i = 0; i < structTypeInfo->structInfo.members.size; ++i)
			{
				if (StringEquals(memberName, structTypeInfo->structInfo.members[i].name))
				{
					StructMember *structMember = &structTypeInfo->structInfo.members[i];
					rightHand->structMember.structMember = structMember;
					expression->typeTableIdx = structMember->typeTableIdx;
					return;
				}
			}
			PrintError(context, expression->any.loc, TPrintF("\"%S\" is not a member of \"%S\"",
						memberName, structTypeInfo->structInfo.name));
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
		{
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);

			s64 arrayType = leftHand->typeTableIdx;
			TypeInfo *arrayTypeInfo = &context->typeTable[arrayType];
			if (arrayTypeInfo->typeCategory == TYPECATEGORY_POINTER)
			{
				s64 pointedTypeIdx = arrayTypeInfo->pointerInfo.pointedTypeTableIdx;
				arrayTypeInfo = &context->typeTable[pointedTypeIdx];
			}

			if (arrayTypeInfo->typeCategory != TYPECATEGORY_ARRAY)
				PrintError(context, leftHand->any.loc,
						"Expression does not evaluate to an array"_s);
			expression->typeTableIdx = arrayTypeInfo->arrayInfo.elementTypeTableIdx;
		}
		else
		{
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);
			s64 leftSideType  = leftHand->typeTableIdx;
			s64 rightSideType = rightHand->typeTableIdx;

			if (!CheckTypesMatch(context, leftSideType, rightSideType))
				PrintError(context, expression->any.loc, "Type mismatch!"_s);

			switch (expression->binaryOperation.op)
			{
			case TOKEN_OP_AND:
			case TOKEN_OP_OR:
			case TOKEN_OP_EQUALS:
			case TOKEN_OP_GREATER_THAN:
			case TOKEN_OP_GREATER_THAN_OR_EQUAL:
			case TOKEN_OP_LESS_THAN:
			case TOKEN_OP_LESS_THAN_OR_EQUAL:
				expression->typeTableIdx = TYPETABLEIDX_BOOL;
			default:
				expression->typeTableIdx = leftSideType;
			};
			return;
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_INTEGER:
			expression->typeTableIdx = TYPETABLEIDX_INTEGER;
			break;
		case LITERALTYPE_FLOATING:
			expression->typeTableIdx = TYPETABLEIDX_FLOATING;
			break;
		case LITERALTYPE_STRING:
			expression->typeTableIdx = TYPETABLEIDX_STRUCT_STRING;
			break;
		default:
			CRASH;
		}
	} break;
	case ASTNODETYPE_IF:
	{
		TypeCheckExpression(context, expression->ifNode.condition);
		s64 conditionType = expression->ifNode.condition->typeTableIdx;
		if (!CheckTypesMatch(context, TYPETABLEIDX_BOOL, conditionType))
			PrintError(context, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->ifNode.body);

		if (expression->ifNode.elseBody)
			TypeCheckExpression(context, expression->ifNode.elseBody);
	} break;
	case ASTNODETYPE_WHILE:
	{
		TypeCheckExpression(context, expression->whileNode.condition);
		s64 conditionType = expression->whileNode.condition->typeTableIdx;
		if (!CheckTypesMatch(context, TYPETABLEIDX_BOOL, conditionType))
			PrintError(context, expression->any.loc, "While condition doesn't evaluate to a boolean"_s);

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
	context->tcCurrentReturnType = -1;

	DynamicArrayInit(&context->tcStack, 128);

	PushTCScope(context);

	BucketArrayInit(&context->typeTable);
	for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
		BucketArrayAdd(&context->typeTable);

	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_INTEGER;
		t.integerInfo.isSigned = true;

		t.integerInfo.size = 1;
		context->typeTable[TYPETABLEIDX_S8]  = t;
		t.integerInfo.size = 2;
		context->typeTable[TYPETABLEIDX_S16] = t;
		t.integerInfo.size = 4;
		context->typeTable[TYPETABLEIDX_S32] = t;
		t.integerInfo.size = 8;
		context->typeTable[TYPETABLEIDX_S64] = t;
		context->typeTable[TYPETABLEIDX_INTEGER] = t;

		t.integerInfo.isSigned = false;

		t.integerInfo.size = 1;
		context->typeTable[TYPETABLEIDX_U8]  = t;
		context->typeTable[TYPETABLEIDX_BOOL]  = t;
		t.integerInfo.size = 2;
		context->typeTable[TYPETABLEIDX_U16] = t;
		t.integerInfo.size = 4;
		context->typeTable[TYPETABLEIDX_U32] = t;
		t.integerInfo.size = 8;
		context->typeTable[TYPETABLEIDX_U64] = t;

		t.typeCategory = TYPECATEGORY_FLOATING;
		t.floatingInfo.size = 4;
		context->typeTable[TYPETABLEIDX_F32] = t;
		t.floatingInfo.size = 8;
		context->typeTable[TYPETABLEIDX_F64] = t;
		context->typeTable[TYPETABLEIDX_FLOATING] = t;

		t = {};
		t.typeCategory = TYPECATEGORY_INVALID;
		context->typeTable[TYPETABLEIDX_VOID] = t;

		// Built-in structs
		// String
		{
			t.typeCategory = TYPECATEGORY_STRUCT;
			t.structInfo.name = "String"_s;
			DynamicArrayInit(&t.structInfo.members, 2);

			StructMember *sizeMember = DynamicArrayAdd(&t.structInfo.members);
			sizeMember->name = "size"_s;
			sizeMember->typeTableIdx = TYPETABLEIDX_U64;

			StructMember *dataMember = DynamicArrayAdd(&t.structInfo.members);
			TypeInfo pointerToU8TypeInfo;
			pointerToU8TypeInfo.typeCategory = TYPECATEGORY_POINTER;
			pointerToU8TypeInfo.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_U8;

			dataMember->name = "data"_s;
			dataMember->typeTableIdx = FindOrAddTypeTableIdx(context, pointerToU8TypeInfo);

			context->typeTable[TYPETABLEIDX_STRUCT_STRING] = t;
		}
	}
	for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
		*DynamicArrayAdd(&context->tcStack[0].typeIndices) = i;

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		TypeCheckExpression(context, statement);
	}
}
