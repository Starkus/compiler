enum TypeTableIndices
{
	TYPETABLEIDX_PRIMITIVE_BEGIN,
	TYPETABLEIDX_S8 = TYPETABLEIDX_PRIMITIVE_BEGIN,
	TYPETABLEIDX_S16,
	TYPETABLEIDX_S32,
	TYPETABLEIDX_S64,
	TYPETABLEIDX_U8,
	TYPETABLEIDX_U16,
	TYPETABLEIDX_U32,
	TYPETABLEIDX_U64,
	TYPETABLEIDX_F32,
	TYPETABLEIDX_F64,
	TYPETABLEIDX_PRIMITIVE_END = TYPETABLEIDX_F64,

	TYPETABLEIDX_BOOL,
	TYPETABLEIDX_INTEGER,
	TYPETABLEIDX_FLOATING,
	TYPETABLEIDX_VOID,

	TYPETABLEIDX_COUNT
};

enum TypeCategory
{
	TYPECATEGORY_INVALID,
	TYPECATEGORY_INTEGER,
	TYPECATEGORY_FLOATING,
	TYPECATEGORY_STRUCT,
	TYPECATEGORY_ENUM,
	TYPECATEGORY_POINTER,
	TYPECATEGORY_ARRAY,
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
	bool isUsing;
	u64 offset;
};
struct TypeInfoStruct
{
	bool isUnion;
	DynamicArray<StructMember, malloc, realloc> members;
};

struct TypeInfoEnum
{
	s64 typeTableIdx;
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
	s64 size;
	union
	{
		TypeInfoInteger integerInfo;
		TypeInfoStruct structInfo;
		TypeInfoEnum enumInfo;
		TypeInfoPointer pointerInfo;
		TypeInfoArray arrayInfo;
	};
};

enum StaticDefinitionType
{
	STATICDEFINITIONTYPE_TYPE,
	STATICDEFINITIONTYPE_PROCEDURE,
	STATICDEFINITIONTYPE_CONSTANT_INTEGER,
	STATICDEFINITIONTYPE_CONSTANT_FLOATING
};
struct StaticDefinition
{
	String name;
	StaticDefinitionType definitionType;
	union
	{
		struct
		{
			s64 typeTableIdx;
		} type;
		struct
		{
			Procedure *procedure;
		} procedure;
		struct
		{
			s64 value;
			s64 typeTableIdx;
		} constantInteger;
		struct
		{
			f64 value;
			s64 typeTableIdx;
		} constantFloating;
	};
};

struct TCScopeName
{
	NameType type;
	String name;
	union
	{
		Variable *variable;
		struct
		{
			Variable *base;
			Array<StructMember *> offsets;
		} structMemberInfo;
		StaticDefinition *staticDefinition;
	};
};
struct TCScope
{
	DynamicArray<TCScopeName, malloc, realloc> names;
	DynamicArray<s64, malloc, realloc> typeIndices;
};

s64 GetStaticDefinitionTypeTableIdx(StaticDefinition def)
{
	switch (def.definitionType)
	{
	case STATICDEFINITIONTYPE_TYPE:
		return def.type.typeTableIdx;
	case STATICDEFINITIONTYPE_CONSTANT_INTEGER:
		return def.constantInteger.typeTableIdx;
	case STATICDEFINITIONTYPE_CONSTANT_FLOATING:
		return def.constantFloating.typeTableIdx;
	}
	return -1;
}

StaticDefinition *FindStaticDefinitionByName(Context *context, String name)
{
	u64 count = BucketArrayCount(&context->staticDefinitions);
	for (u64 i = 0; i < count; ++i)
	{
		StaticDefinition *currentDef = &context->staticDefinitions[i];
		if (StringEquals(name, currentDef->name))
			return currentDef;
	}
	return nullptr;
}

StaticDefinition *FindStaticDefinitionByTypeTableIdx(Context *context, s64 typeTableIdx)
{
	u64 count = BucketArrayCount(&context->staticDefinitions);
	for (u64 i = 0; i < count; ++i)
	{
		StaticDefinition *currentDef = &context->staticDefinitions[i];
		if (currentDef->definitionType == STATICDEFINITIONTYPE_TYPE &&
				currentDef->type.typeTableIdx == typeTableIdx)
			return currentDef;
	}
	return nullptr;
}

StaticDefinition *FindStaticDefinitionByProcedure(Context *context, Procedure *procedure)
{
	u64 count = BucketArrayCount(&context->staticDefinitions);
	for (u64 i = 0; i < count; ++i)
	{
		StaticDefinition *currentDef = &context->staticDefinitions[i];
		if (currentDef->definitionType == STATICDEFINITIONTYPE_PROCEDURE &&
				currentDef->procedure.procedure == procedure)
			return currentDef;
	}
	return nullptr;
}

String TypeInfoToString(Context *context, s64 typeTableIdx)
{
	if (typeTableIdx == TYPETABLEIDX_VOID)
		return "void"_s;

	TypeInfo *typeInfo = &context->typeTable[typeTableIdx];
	switch (typeInfo->typeCategory)
	{
	case TYPECATEGORY_STRUCT:
	{
		StaticDefinition *staticDef = FindStaticDefinitionByTypeTableIdx(context, typeTableIdx);
		if (staticDef != nullptr)
			return staticDef->name;
		return "<struct>"_s;
	}
	case TYPECATEGORY_ENUM:
	{
		StaticDefinition *staticDef = FindStaticDefinitionByTypeTableIdx(context, typeTableIdx);
		if (staticDef != nullptr)
			return staticDef->name;
		return "<enum>"_s;
	}
	case TYPECATEGORY_POINTER:
		return StringConcat("^"_s, TypeInfoToString(context, typeInfo->pointerInfo.pointedTypeTableIdx));
	case TYPECATEGORY_ARRAY:
	{
		String typeStr = TypeInfoToString(context, typeInfo->arrayInfo.elementTypeTableIdx);
		return TPrintF("[%d] %S", typeInfo->arrayInfo.count, typeStr);
	}
	case TYPECATEGORY_INTEGER:
	{
		if (typeInfo->integerInfo.isSigned) switch (typeInfo->size)
		{
			case 1: return "s8"_s;
			case 2: return "s16"_s;
			case 4: return "s32"_s;
			case 8: return "s64"_s;
		}
		else switch (typeInfo->size)
		{
			case 1: return "u8"_s;
			case 2: return "u16"_s;
			case 4: return "u32"_s;
			case 8: return "u64"_s;
		}
	} break;
	case TYPECATEGORY_FLOATING:
	{
		switch (typeInfo->size)
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

	DynamicArrayInit(&newScope->names, 64);
	DynamicArrayInit(&newScope->typeIndices, 64);
}

void PopTCScope(Context *context)
{
	--context->tcStack.size;
}

s64 FindTypeInStackByName(Context *context, SourceLocation loc, String name)
{
	s64 typeTableIdx = -1;

	if (StringEquals(name, "s8"_s))
		typeTableIdx = TYPETABLEIDX_S8;
	else if (StringEquals(name, "s16"_s))
		typeTableIdx = TYPETABLEIDX_S16;
	else if (StringEquals(name, "s32"_s))
		typeTableIdx = TYPETABLEIDX_S32;
	else if (StringEquals(name, "s64"_s))
		typeTableIdx = TYPETABLEIDX_S64;
	else if (StringEquals(name, "u8"_s))
		typeTableIdx = TYPETABLEIDX_U8;
	else if (StringEquals(name, "u16"_s))
		typeTableIdx = TYPETABLEIDX_U16;
	else if (StringEquals(name, "u32"_s))
		typeTableIdx = TYPETABLEIDX_U32;
	else if (StringEquals(name, "u64"_s))
		typeTableIdx = TYPETABLEIDX_U64;
	else if (StringEquals(name, "f32"_s))
		typeTableIdx = TYPETABLEIDX_F32;
	else if (StringEquals(name, "f64"_s))
		typeTableIdx = TYPETABLEIDX_F64;
	else if (StringEquals(name, "bool"_s))
		typeTableIdx = TYPETABLEIDX_BOOL;
	else if (StringEquals(name, "void"_s))
		typeTableIdx = TYPETABLEIDX_VOID;
	else
	{
		StaticDefinition *staticDef = nullptr;
		// Search backwards so we find types higher in the stack first.
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->names.size; ++i)
			{
				TCScopeName currentName = currentScope->names[i];
				if (StringEquals(name, currentName.name))
				{
					if (currentName.type != NAMETYPE_STATIC_DEFINITION)
						PrintError(context, loc, TPrintF("\"%S\" is not a type!",
									name));

					staticDef = currentName.staticDefinition;
					break;
				}
			}
		}
		if (staticDef == nullptr)
			PrintError(context, loc, TPrintF("Type \"%S\" not in scope!", name));
		else if (staticDef->definitionType != STATICDEFINITIONTYPE_TYPE)
			PrintError(context, loc, TPrintF("\"%S\" is not a type!", name));
		typeTableIdx = staticDef->type.typeTableIdx;
	}

	if (typeTableIdx < 0)
		PrintError(context, loc, TPrintF("Type \"%S\" not in scope!", name));

	return typeTableIdx;
}

bool CheckTypesMatch(Context *context, s64 leftTableIdx, s64 rightTableIdx)
{
	if (leftTableIdx == rightTableIdx)
		return true;

	// Allow anything to cast to Any
	s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);
	if (leftTableIdx == anyTableIdx || rightTableIdx == anyTableIdx)
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

		if ((left->typeCategory == TYPECATEGORY_POINTER &&
			right->typeCategory == TYPECATEGORY_INTEGER) ||
			(right->typeCategory == TYPECATEGORY_POINTER &&
			left->typeCategory == TYPECATEGORY_INTEGER))
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
		if (left->size < right->size)
		{
			Log("Trying to fit integer of size %d into integer of size %d!\n",
					right->size, left->size);
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

bool CheckTypesMatchAndSpecialize(Context *context, s64 *leftTableIdx, s64 *rightTableIdx)
{
	if (CheckTypesMatch(context, *leftTableIdx, *rightTableIdx))
		return true;

	TypeInfo *left  = &context->typeTable[*leftTableIdx];
	TypeInfo *right = &context->typeTable[*rightTableIdx];

	if (*leftTableIdx == TYPETABLEIDX_INTEGER && (right->typeCategory == TYPECATEGORY_INTEGER ||
				right->typeCategory == TYPECATEGORY_POINTER))
	{
		*rightTableIdx = *leftTableIdx;
		return true;
	}
	if (*rightTableIdx == TYPETABLEIDX_INTEGER && (left->typeCategory == TYPECATEGORY_INTEGER ||
				left->typeCategory == TYPECATEGORY_POINTER))
	{
		*leftTableIdx = *rightTableIdx;
		return true;
	}

	if (*leftTableIdx == TYPETABLEIDX_FLOATING && right->typeCategory == TYPECATEGORY_FLOATING)
	{
		*rightTableIdx = *leftTableIdx;
		return true;
	}
	if (*rightTableIdx == TYPETABLEIDX_FLOATING && left->typeCategory == TYPECATEGORY_FLOATING)
	{
		*leftTableIdx = *rightTableIdx;
		return true;
	}

	return false;
}

bool AreTypeInfosEqual(TypeInfo *a, TypeInfo *b)
{
	if (a->typeCategory != b->typeCategory)
		return false;

	if (a->size != b->size)
		return false;

	switch (a->typeCategory)
	{
	case TYPECATEGORY_INTEGER:
		return a->integerInfo.isSigned == b->integerInfo.isSigned;
	case TYPECATEGORY_FLOATING:
		return true;
	case TYPECATEGORY_STRUCT:
		if (a->structInfo.isUnion != b->structInfo.isUnion)
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
	resultTypeInfo.size = g_pointerSize;
	resultTypeInfo.pointerInfo.pointedTypeTableIdx = inType;
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

s64 GetTypeInfoArrayOf(Context *context, s64 inType, s64 count)
{
	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_ARRAY;
	resultTypeInfo.arrayInfo.elementTypeTableIdx = inType;
	resultTypeInfo.arrayInfo.count = count;
	if (count == 0)
		resultTypeInfo.size = 8 + g_pointerSize;
	else
	{
		TypeInfo *elementTypeInfo = &context->typeTable[inType];
		resultTypeInfo.size = elementTypeInfo->size * count;
	}
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

s64 TypeCheckType(Context *context, SourceLocation loc, ASTType *astType);

s64 TypeCheckStructDeclaration(Context *context, ASTStructDeclaration astStructDecl)
{
	TypeInfo t = {};
	t.typeCategory = TYPECATEGORY_STRUCT;
	t.structInfo.isUnion = astStructDecl.isUnion;
	DynamicArrayInit(&t.structInfo.members, 16);

	for (int memberIdx = 0; memberIdx < astStructDecl.members.size; ++memberIdx)
	{
		ASTStructMemberDeclaration astMember = astStructDecl.members[memberIdx];

		StructMember *member = DynamicArrayAdd(&t.structInfo.members);
		member->name = astMember.name;
		member->isUsing = astMember.isUsing;
		member->typeTableIdx = TypeCheckType(context, astMember.loc, astMember.astType);
		member->offset = t.size; // @Todo: aligning

		s64 memberSize = context->typeTable[member->typeTableIdx].size;
		t.size += memberSize; // @Todo: aligning
	}

	s64 typeTableIdx = BucketArrayCount(&context->typeTable);
	*BucketArrayAdd(&context->typeTable) = t;

	*DynamicArrayAdd(&context->tcStack[context->tcStack.size - 1].typeIndices) = typeTableIdx;
	return typeTableIdx;
}

s64 TypeCheckType(Context *context, SourceLocation loc, ASTType *astType)
{
	switch (astType->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
	{
		s64 typeTableIdx = FindTypeInStackByName(context, astType->loc, astType->name);
		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		s64 elementTypeIdx = TypeCheckType(context, loc, astType->arrayType);
		s64 typeTableIdx = GetTypeInfoArrayOf(context, elementTypeIdx, astType->arrayCount);
		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		s64 pointedTypeIdx = TypeCheckType(context, loc, astType->pointedType);
		s64 typeTableIdx = GetTypeInfoPointerOf(context, pointedTypeIdx);
		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	{
		return TypeCheckStructDeclaration(context, astType->structDeclaration);
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_ENUM;
		t.enumInfo.typeTableIdx = TYPETABLEIDX_S64;

		if (astType->enumDeclaration.astType)
		{
			SourceLocation astTypeLoc = astType->enumDeclaration.astType->loc;
			t.enumInfo.typeTableIdx = TypeCheckType(context, astTypeLoc, astType->enumDeclaration.astType);

			if (t.enumInfo.typeTableIdx < TYPETABLEIDX_PRIMITIVE_BEGIN ||
				t.enumInfo.typeTableIdx > TYPETABLEIDX_PRIMITIVE_END)
				PrintError(context, astTypeLoc, "Only primitive types are allowed as enum field types"_s);
		}

		t.size = context->typeTable[t.enumInfo.typeTableIdx].size;

		s64 typeTableIdx = BucketArrayCount(&context->typeTable);
		*BucketArrayAdd(&context->typeTable) = t;
		*DynamicArrayAdd(&context->tcStack[context->tcStack.size - 1].typeIndices) = typeTableIdx;

		s64 currentValue = 0;
		for (int memberIdx = 0; memberIdx < astType->enumDeclaration.members.size; ++memberIdx)
		{
			ASTEnumMember astMember = astType->enumDeclaration.members[memberIdx];

			StaticDefinition staticDefinition = {};
			staticDefinition.name = astMember.name;
			staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT_INTEGER;

			if (astMember.value)
			{
				if (astMember.value->nodeType != ASTNODETYPE_LITERAL)
				{
					// @Todo: Somehow execute constant expressions and bake them?
					PrintError(context, astType->loc, "Non literal initial values for enum values not yet supported"_s);
				}
				currentValue = astMember.value->literal.integer;
			}
			staticDefinition.constantInteger.value = currentValue++;
			staticDefinition.constantInteger.typeTableIdx = typeTableIdx;

			StaticDefinition *newStaticDef = BucketArrayAdd(&context->staticDefinitions);
			*newStaticDef = staticDefinition;

			TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
			TCScopeName newName;
			newName.type = NAMETYPE_STATIC_DEFINITION;
			newName.name = astMember.name;
			newName.staticDefinition = newStaticDef;
			*DynamicArrayAdd(&stackTop->names) = newName;
		}

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
		case ASTNODETYPE_IDENTIFIER:
			return expression->identifier.type == NAMETYPE_STATIC_DEFINITION;
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

void AddStructMembersToScope(Context *context, SourceLocation loc, Variable *base, s64 typeTableIdx,
		DynamicArray<StructMember *, malloc, realloc> *offsetStack)
{
	TypeInfo *typeInfo = &context->typeTable[typeTableIdx];
	ASSERT(typeInfo->typeCategory == TYPECATEGORY_STRUCT);

	TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
	for (int memberIdx = 0; memberIdx < typeInfo->structInfo.members.size; ++memberIdx)
	{
		StructMember *member = &typeInfo->structInfo.members[memberIdx];
		*DynamicArrayAdd(offsetStack) = member;

		if (member->name.size && !member->isUsing)
		{
			// Check if name already exists
			for (s64 i = 0; i < (s64)stackTop->names.size; ++i)
			{
				TCScopeName currentName = stackTop->names[i];
				if (StringEquals(member->name, currentName.name))
				{
					String fullMemberName = base->name;
					for (int j = 0; j < offsetStack->size; ++j)
						fullMemberName = TPrintF("%S.%S", fullMemberName, (*offsetStack)[j]->name);

					PrintError(context, loc, TPrintF("Failed to pull name \"%S\" into scope because "
								"name \"%S\" is already used", fullMemberName, member->name));
				}
			}

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_STRUCT_MEMBER;
			newScopeName.name = member->name;
			newScopeName.structMemberInfo.base = base;

			ArrayInit(&newScopeName.structMemberInfo.offsets, offsetStack->size, malloc);
			for (int i = 0; i < offsetStack->size; ++i)
				*ArrayAdd(&newScopeName.structMemberInfo.offsets) = (*offsetStack)[i];

			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}
		else
		{
			AddStructMembersToScope(context, loc, base, member->typeTableIdx, offsetStack);
		}
		--offsetStack->size;
	}
}

void TypeCheckExpression(Context *context, ASTExpression *expression);

void TypeCheckVariableDeclaration(Context *context, ASTVariableDeclaration varDecl)
{
	String varName = varDecl.variable->name;
	s64 *varType = &varDecl.variable->typeTableIdx;

	TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
	if (varName.size)
	{
		// Check if name already exists
		for (s64 i = 0; i < (s64)stackTop->names.size; ++i)
		{
			TCScopeName currentName = stackTop->names[i];
			if (StringEquals(varName, currentName.name))
			{
				PrintError(context, varDecl.loc, TPrintF("Duplicate name \"%S\" in scope", varName));
			}
		}
	}

	if (varDecl.astType)
	{
		*varType = TypeCheckType(context, varDecl.loc, varDecl.astType);

		if (*varType == TYPETABLEIDX_VOID)
			PrintError(context, varDecl.loc, "Variable can't be of type void!"_s);
	}

	if (varDecl.value)
	{
		TypeCheckExpression(context, varDecl.value);
		u64 valueType = varDecl.value->typeTableIdx;

		if (varDecl.astType)
		{
			if (!CheckTypesMatch(context, *varType, valueType))
			{
				String varTypeStr = TypeInfoToString(context, *varType);
				String valueTypeStr = TypeInfoToString(context, valueType);
				PrintError(context, varDecl.loc, TPrintF(
							"Variable declaration type and initial type don't match (variable "
							"is %S and initial value is %S", varTypeStr, valueTypeStr));
			}
		}
		else
		{
			if (valueType == TYPETABLEIDX_VOID)
				PrintError(context, varDecl.loc, "Variable can't be of type void!"_s);

			*varType = InferType(valueType);
		}
	}

	if (varName.size)
	{
		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = varDecl.variable->name;
		newScopeName.variable = varDecl.variable;
		*DynamicArrayAdd(&stackTop->names) = newScopeName;
	}
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

StructMember *FindStructMemberByName(Context *context, TypeInfo *structTypeInfo, String name)
{
	for (int i = 0; i < structTypeInfo->structInfo.members.size; ++i)
	{
		StructMember *currentMember = &structTypeInfo->structInfo.members[i];
		if (StringEquals(name, currentMember->name))
			return currentMember;
		if (currentMember->isUsing || currentMember->name.size == 0)
		{
			// Anonymous structs/unions and using
			TypeInfo *memberTypeInfo = &context->typeTable[currentMember->typeTableIdx];
			ASSERT(memberTypeInfo->typeCategory == TYPECATEGORY_STRUCT);
			StructMember *found = FindStructMemberByName(context, memberTypeInfo, name);
			if (found)
				return found;
		}
	}
	return nullptr;
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
		ASTVariableDeclaration varDecl = expression->variableDeclaration;
		TypeCheckVariableDeclaration(context, varDecl);
		expression->typeTableIdx = varDecl.variable->typeTableIdx;

		if (varDecl.isUsing || varDecl.variable->name.size == 0)
		{
			TypeInfo *typeInfo = &context->typeTable[expression->typeTableIdx];
			if (typeInfo->typeCategory != TYPECATEGORY_STRUCT)
				PrintError(context, expression->any.loc, "Using keyword only accepts structs!"_s);

			DynamicArray<StructMember *, malloc, realloc> offsetStack;
			DynamicArrayInit(&offsetStack, 8);
			AddStructMembersToScope(context, varDecl.loc, varDecl.variable,
					varDecl.variable->typeTableIdx,
					&offsetStack);
		}
		else if (varDecl.variable->name.size == 0)
		{
			// Anonymous struct!
			TypeInfo *typeInfo = &context->typeTable[expression->typeTableIdx];
			if (typeInfo->typeCategory != TYPECATEGORY_STRUCT)
				PrintError(context, expression->any.loc, "Anonymous variable has to be a struct!"_s);

			DynamicArray<StructMember *, malloc, realloc> offsetStack;
			DynamicArrayInit(&offsetStack, 8);
			AddStructMembersToScope(context, varDecl.loc, varDecl.variable,
					varDecl.variable->typeTableIdx, &offsetStack);
		}
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition astStaticDef = expression->staticDefinition;

		StaticDefinition staticDefinition = {};
		staticDefinition.name = astStaticDef.name;

		// Check if already exists
		TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
		for (s64 i = 0; i < (s64)stackTop->names.size; ++i)
		{
			TCScopeName currentName = stackTop->names[i];
			if (StringEquals(staticDefinition.name, currentName.name))
			{
				PrintError(context, expression->any.loc,
						TPrintF("Duplicate static definition \"%S\"", staticDefinition.name));
			}
		}

		TypeCheckExpression(context, astStaticDef.expression);
		if (astStaticDef.expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
		{
			staticDefinition.definitionType = STATICDEFINITIONTYPE_PROCEDURE;
			staticDefinition.procedure.procedure =
				astStaticDef.expression->procedureDeclaration.procedure;
		}
		else if (astStaticDef.expression->nodeType == ASTNODETYPE_TYPE)
		{
			staticDefinition.definitionType = STATICDEFINITIONTYPE_TYPE;
			staticDefinition.type.typeTableIdx = astStaticDef.expression->typeTableIdx;
		}
		else if (astStaticDef.expression->nodeType == ASTNODETYPE_LITERAL)
		{
			ASTLiteral astLiteral = astStaticDef.expression->literal;
			switch (astLiteral.type)
			{
			case LITERALTYPE_INTEGER:
			{
				staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT_INTEGER;
				staticDefinition.constantInteger.value = astLiteral.integer;
				staticDefinition.constantInteger.typeTableIdx = astStaticDef.expression->typeTableIdx;
			} break;
			case LITERALTYPE_FLOATING:
			{
				staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT_FLOATING;
				staticDefinition.constantFloating.value = astLiteral.floating;
				staticDefinition.constantFloating.typeTableIdx = astStaticDef.expression->typeTableIdx;
			} break;
			case LITERALTYPE_CHARACTER:
			{
				staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT_INTEGER;
				staticDefinition.constantFloating.value = astLiteral.character;
				staticDefinition.constantFloating.typeTableIdx = astStaticDef.expression->typeTableIdx;
			} break;
			}
		}
		else
			CRASH;

		StaticDefinition *newStaticDef = BucketArrayAdd(&context->staticDefinitions);
		*newStaticDef = staticDefinition;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_STATIC_DEFINITION;
		newScopeName.name = astStaticDef.name;
		newScopeName.staticDefinition = newStaticDef;
		*DynamicArrayAdd(&stackTop->names) = newScopeName;
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		ASTProcedureDeclaration *procDecl = &expression->procedureDeclaration;
		Procedure *procedure = procDecl->procedure;

		procedure->requiredParameterCount = 0;

		PushTCScope(context);

		// Parameters
		bool beginOptionalParameters = false;
		DynamicArrayInit(&procedure->parameters, 8);
		for (int i = 0; i < procDecl->astParameters.size; ++i)
		{
			ASTVariableDeclaration astVarDecl = procDecl->astParameters[i];

			TypeCheckVariableDeclaration(context, astVarDecl);
			ProcedureParameter *procParam = DynamicArrayAdd(&procedure->parameters);
			procParam->variable = astVarDecl.variable;
			procParam->defaultValue = astVarDecl.value;

			if (astVarDecl.isUsing)
			{
				DynamicArray<StructMember *, malloc, realloc> offsetStack;
				DynamicArrayInit(&offsetStack, 8);
				AddStructMembersToScope(context, astVarDecl.loc, astVarDecl.variable,
						astVarDecl.variable->typeTableIdx, &offsetStack);
			}

			if (!astVarDecl.value)
			{
				if (beginOptionalParameters)
					PrintError(context, astVarDecl.loc, "Non-optional parameter after optional parameter found!"_s);

				++procedure->requiredParameterCount;
			}
			else
			{
				beginOptionalParameters = true;
			}
		}

		// Varargs array
		if (procedure->isVarargs)
		{
			s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);
			Variable *var = BucketArrayAdd(&context->variables);
			*var = {};
			var->name = procedure->varargsName;
			var->parameterIndex = (s8)procedure->parameters.size;
			var->typeTableIdx = GetTypeInfoArrayOf(context, anyTableIdx, 0);

			ProcedureParameter *procParam = DynamicArrayAdd(&procedure->parameters);
			procParam->variable = var;
			procParam->defaultValue = nullptr;

			TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = var->name;
			newScopeName.variable = var;
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
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
	case ASTNODETYPE_IDENTIFIER:
	{
		String string = expression->identifier.string;

		// Search backwards so we find variables higher in the stack first.
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->names.size; ++i)
			{
				TCScopeName currentName = currentScope->names[i];
				if (StringEquals(string, currentName.name))
				{
					expression->identifier.type = currentName.type;
					switch (currentName.type)
					{
					case NAMETYPE_VARIABLE:
					{
						expression->identifier.variable = currentName.variable;
						expression->typeTableIdx = currentName.variable->typeTableIdx;
					} break;
					case NAMETYPE_STRUCT_MEMBER:
					{
						expression->identifier.structMemberInfo.base =
							currentName.structMemberInfo.base;
						expression->identifier.structMemberInfo.offsets =
							currentName.structMemberInfo.offsets;
						int lastIdx = (int)currentName.structMemberInfo.offsets.size - 1;
						expression->typeTableIdx = currentName.structMemberInfo.offsets[lastIdx]->typeTableIdx;
					} break;
					case NAMETYPE_STATIC_DEFINITION:
					{
						expression->identifier.staticDefinition = currentName.staticDefinition;
						expression->typeTableIdx =
							GetStaticDefinitionTypeTableIdx(*currentName.staticDefinition);
					} break;
					}
					goto skipInvalidIdentifierError;
				}
			}
		}
		PrintError(context, expression->any.loc, TPrintF("Invalid variable \"%S\" referenced", string));

skipInvalidIdentifierError:
		if (expression->identifier.isUsing)
		{
			if (expression->identifier.type != NAMETYPE_VARIABLE)
				PrintError(context, expression->any.loc,
						"Expression after 'using' does not evaluate to a variable"_s);

			TypeInfo *typeInfo = &context->typeTable[expression->typeTableIdx];
			if (typeInfo->typeCategory != TYPECATEGORY_STRUCT)
				PrintError(context, expression->any.loc, "Using keyword only accepts structs!"_s);

			Variable *base = expression->identifier.variable;

			DynamicArray<StructMember *, malloc, realloc> offsetStack;
			DynamicArrayInit(&offsetStack, 8);
			AddStructMembersToScope(context, expression->any.loc, base, base->typeTableIdx, &offsetStack);
		}
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String procName = expression->procedureCall.name;

		// Search backwards so we find procedures higher in the stack first.
		Procedure *procedure = nullptr;
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->names.size; ++i)
			{
				TCScopeName currentName = currentScope->names[i];
				if (StringEquals(procName, currentName.name))
				{
					if (currentName.type != NAMETYPE_STATIC_DEFINITION ||
						currentName.staticDefinition->definitionType != STATICDEFINITIONTYPE_PROCEDURE)
						PrintError(context, expression->any.loc, "Calling a non-procedure"_s);

					procedure = currentName.staticDefinition->procedure.procedure;
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
		s64 totalArguments = procedure->parameters.size - procedure->isVarargs;
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
		}

		s64 argsToCheck = Min(givenArguments, totalArguments);
		for (int argIdx = 0; argIdx < argsToCheck; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];
			Variable *param = procedure->parameters[argIdx].variable;
			if (!CheckTypesMatchAndSpecialize(context, &param->typeTableIdx, &arg->typeTableIdx))
			{
				String paramStr =  TypeInfoToString(context, param->typeTableIdx);
				String givenStr = TypeInfoToString(context, arg->typeTableIdx);
				PrintError(context, arg->any.loc, TPrintF("When calling procedure \"%S\": type of "
							"parameter #%d didn't match (parameter is %S but %S was given)",
							procName, argIdx, paramStr, givenStr));
			}
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

			expression->typeTableIdx = GetTypeInfoPointerOf(context, expressionType);
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

			ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
			rightHand->identifier.type = NAMETYPE_STRUCT_MEMBER;

			TypeInfo *structTypeInfo = &context->typeTable[leftHandTypeIdx];
			if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
			{
				s64 pointedTypeIdx = structTypeInfo->pointerInfo.pointedTypeTableIdx;
				structTypeInfo = &context->typeTable[pointedTypeIdx];
			}

			if (structTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
			{
				// This is only for dynamic size arrays!
				if (structTypeInfo->arrayInfo.count != 0)
					PrintError(context, expression->any.loc, "Array left of '.' has to be of dynamic size! ([])"_s);

				s64 arrayTypeTableIdx = FindTypeInStackByName(context, {}, "Array"_s);
				structTypeInfo = &context->typeTable[arrayTypeTableIdx];
			}
			else if (structTypeInfo->typeCategory != TYPECATEGORY_STRUCT)
			{
				PrintError(context, expression->any.loc, "Left of '.' has to be a struct"_s);
			}

			String memberName = rightHand->identifier.string;
			StructMember *foundMember = FindStructMemberByName(context, structTypeInfo, memberName);
			if (foundMember)
			{
				// We don't need the base in this case, just the top most offset.
				// @Cleanup
				rightHand->identifier.structMemberInfo.base = nullptr;
				ArrayInit(&rightHand->identifier.structMemberInfo.offsets, 1, malloc);
				*ArrayAdd(&rightHand->identifier.structMemberInfo.offsets) = foundMember;
				expression->typeTableIdx = foundMember->typeTableIdx;
				return;
			}

			PrintError(context, expression->any.loc, TPrintF("\"%S\" is not a member of \"%S\"",
						memberName, TypeInfoToString(context, leftHandTypeIdx)));
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

			s64 stringTypeIdx = FindTypeInStackByName(context, {}, "String"_s);
			if (arrayType == stringTypeIdx)
			{
				expression->typeTableIdx = TYPETABLEIDX_U8;
			}
			else
			{
				if (arrayTypeInfo->typeCategory != TYPECATEGORY_ARRAY)
					PrintError(context, leftHand->any.loc,
							"Expression does not evaluate to an array"_s);
				expression->typeTableIdx = arrayTypeInfo->arrayInfo.elementTypeTableIdx;
			}
		}
		else
		{
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);

			if (!CheckTypesMatchAndSpecialize(context, &leftHand->typeTableIdx,
						&rightHand->typeTableIdx))
			{
				String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
				String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
				PrintError(context, expression->any.loc, TPrintF("Type mismatch! (%S and %S)",
							leftStr, rightStr));
			}

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
				break;
			default:
				expression->typeTableIdx = leftHand->typeTableIdx;
				break;
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
		case LITERALTYPE_CHARACTER:
			expression->typeTableIdx = TYPETABLEIDX_INTEGER;
			break;
		case LITERALTYPE_STRING:
			expression->typeTableIdx = FindTypeInStackByName(context, {}, "String"_s);
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
	case ASTNODETYPE_TYPE:
	{
		expression->typeTableIdx = TypeCheckType(context, expression->any.loc, &expression->astType);
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		TypeCheckExpression(context, expression->typeOfNode.expression);

		s64 programTypeInfoTableIdx = FindTypeInStackByName(context, expression->any.loc, "TypeInfo"_s);
		expression->typeTableIdx = GetTypeInfoPointerOf(context, programTypeInfoTableIdx);
	} break;
	case ASTNODETYPE_CAST:
	{
		TypeCheckExpression(context, expression->castNode.expression);
		expression->typeTableIdx = TypeCheckType(context, expression->any.loc, &expression->castNode.astType);
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

	BucketArrayInit(&context->staticDefinitions);
	DynamicArrayInit(&context->tcStack, 128);

	PushTCScope(context);

	BucketArrayInit(&context->typeTable);
	for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
		BucketArrayAdd(&context->typeTable);

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
		context->typeTable[TYPETABLEIDX_INTEGER] = t;

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
		context->typeTable[TYPETABLEIDX_FLOATING] = t;

		t = {};
		t.typeCategory = TYPECATEGORY_INVALID;
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
