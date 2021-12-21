enum TypeTableIndices
{
	TYPETABLEIDX_STRUCT_LITERAL = -5,

	TYPETABLEIDX_PRIMITIVE_BEGIN = 1,
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
	TYPECATEGORY_UNION,
	TYPECATEGORY_ENUM,
	TYPECATEGORY_POINTER,
	TYPECATEGORY_ARRAY,
	TYPECATEGORY_PROCEDURE,
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
	DynamicArray<StructMember, malloc, realloc> members;
};

struct TypeInfoEnum
{
	s64 typeTableIdx;
	DynamicArray<String, malloc, realloc> names;
	DynamicArray<s64, malloc, realloc> values;
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

struct ProcedureParameter
{
	s64 typeTableIdx;
	Constant defaultValue;
};
struct TypeInfoProcedure
{
	s64 returnTypeTableIdx;
	DynamicArray<ProcedureParameter, malloc, realloc> parameters;
	bool isVarargs;
};

struct TypeInfo
{
	TypeCategory typeCategory;
	u32 valueIdx; // Value with runtime type information.
	s64 size;
	union
	{
		TypeInfoInteger integerInfo;
		TypeInfoStruct structInfo;
		TypeInfoEnum enumInfo;
		TypeInfoPointer pointerInfo;
		TypeInfoArray arrayInfo;
		TypeInfoProcedure procedureInfo;
	};
};

enum StaticDefinitionType
{
	STATICDEFINITIONTYPE_TYPE,
	STATICDEFINITIONTYPE_PROCEDURE,
	STATICDEFINITIONTYPE_CONSTANT
};
struct StaticDefinition
{
	String name;
	StaticDefinitionType definitionType;
	s64 typeTableIdx;
	union
	{
		s32 procedureIdx;
		Constant constant;
	};
};

struct TCScopeName
{
	NameType type;
	String name;
	SourceLocation loc;
	union
	{
		struct
		{
			u32 valueIdx;
			s64 typeTableIdx;
		} variableInfo;
		struct
		{
			u32 baseValueIdx;
			const StructMember *structMember;
		} structMemberInfo;
		struct
		{
			u32 baseValueIdx;
			Array<const StructMember *> offsets;
		} structMemberChain;
		StaticDefinition *staticDefinition;
	};
};
struct TCScope
{
	DynamicArray<TCScopeName, malloc, realloc> names;
	DynamicArray<s64, malloc, realloc> typeIndices;
};

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
				currentDef->typeTableIdx == typeTableIdx)
			return currentDef;
	}
	return nullptr;
}

StaticDefinition *FindStaticDefinitionByProcedure(Context *context, s32 procedureIdx)
{
	u64 count = BucketArrayCount(&context->staticDefinitions);
	for (u64 i = 0; i < count; ++i)
	{
		StaticDefinition *currentDef = &context->staticDefinitions[i];
		if (currentDef->definitionType == STATICDEFINITIONTYPE_PROCEDURE &&
				currentDef->procedureIdx == procedureIdx)
			return currentDef;
	}
	return nullptr;
}

String TypeInfoToString(Context *context, s64 typeTableIdx)
{
	if (typeTableIdx == TYPETABLEIDX_VOID)
		return "void"_s;
	if (typeTableIdx == TYPETABLEIDX_INTEGER)
		return "<number>"_s;
	if (typeTableIdx == TYPETABLEIDX_FLOATING)
		return "<floating>"_s;

	TypeInfo typeInfo = context->typeTable[typeTableIdx];
	switch (typeInfo.typeCategory)
	{
	case TYPECATEGORY_STRUCT:
	{
		StaticDefinition *staticDef = FindStaticDefinitionByTypeTableIdx(context, typeTableIdx);
		if (staticDef != nullptr)
			return staticDef->name;
		return "<struct>"_s;
	}
	case TYPECATEGORY_UNION:
	{
		StaticDefinition *staticDef = FindStaticDefinitionByTypeTableIdx(context, typeTableIdx);
		if (staticDef != nullptr)
			return staticDef->name;
		return "<union>"_s;
	}
	case TYPECATEGORY_ENUM:
	{
		StaticDefinition *staticDef = FindStaticDefinitionByTypeTableIdx(context, typeTableIdx);
		if (staticDef != nullptr)
			return staticDef->name;
		return "<enum>"_s;
	}
	case TYPECATEGORY_POINTER:
		return StringConcat("^"_s, TypeInfoToString(context, typeInfo.pointerInfo.pointedTypeTableIdx));
	case TYPECATEGORY_ARRAY:
	{
		String typeStr = TypeInfoToString(context, typeInfo.arrayInfo.elementTypeTableIdx);
		return TPrintF("[%d] %S", typeInfo.arrayInfo.count, typeStr);
	}
	case TYPECATEGORY_INTEGER:
	{
		if (typeInfo.integerInfo.isSigned) switch (typeInfo.size)
		{
			case 1: return "s8"_s;
			case 2: return "s16"_s;
			case 4: return "s32"_s;
			case 8: return "s64"_s;
		}
		else switch (typeInfo.size)
		{
			case 1: return "u8"_s;
			case 2: return "u16"_s;
			case 4: return "u32"_s;
			case 8: return "u64"_s;
		}
	} break;
	case TYPECATEGORY_FLOATING:
	{
		switch (typeInfo.size)
		{
			case 4: return "f32"_s;
			case 8: return "f64"_s;
		}
	} break;
	case TYPECATEGORY_PROCEDURE:
	{
		String result = "("_s;
		for (int i = 0; i < typeInfo.procedureInfo.parameters.size; ++i)
		{
			if (i) result = StringConcat(result, ", "_s);
			String paramStr = TypeInfoToString(context, typeInfo.procedureInfo.parameters[i].typeTableIdx);
			result = StringConcat(result, paramStr);
			Constant defaultValue = typeInfo.procedureInfo.parameters[i].defaultValue;
			if (defaultValue.type == CONSTANTTYPE_INTEGER)
				result = TPrintF("%S = %lld", result, defaultValue.valueAsInt);
			else if (defaultValue.type == CONSTANTTYPE_FLOATING)
				result = TPrintF("%S = %f", result, defaultValue.valueAsFloat);
		}
		String returnStr = TypeInfoToString(context, typeInfo.procedureInfo.returnTypeTableIdx);
		result = TPrintF("%S) -> %S", result, returnStr);
		return result;
	}
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
						LogError(context, loc, TPrintF("\"%S\" is not a type!",
									name));

					staticDef = currentName.staticDefinition;
					break;
				}
			}
		}
		if (staticDef == nullptr)
			LogError(context, loc, TPrintF("Type \"%S\" not in scope!", name));
		else if (staticDef->definitionType != STATICDEFINITIONTYPE_TYPE)
			LogError(context, loc, TPrintF("\"%S\" is not a type!", name));
		typeTableIdx = staticDef->typeTableIdx;
	}

	if (typeTableIdx < 0)
		LogError(context, loc, TPrintF("Type \"%S\" not in scope!", name));

	return typeTableIdx;
}

enum TypeCheckErrorCode
{
	TYPECHECK_COOL,
	TYPECHECK_MISC_ERROR,
	TYPECHECK_SIGN_MISMATCH,
	TYPECHECK_SIZE_MISMATCH,
	TYPECHECK_TYPE_CATEGORY_MISMATCH,
	TYPECHECK_POINTED_TYPE_MISMATCH,
	TYPECHECK_ARRAY_SIZE_MISMATCH,
	TYPECHECK_STRUCT_MISMATCH,
};

void ReportTypeCheckError(Context *context, TypeCheckErrorCode errorCode, SourceLocation sourceLoc,
		s64 leftTableIdx, s64 rightTableIdx)
{
	String leftStr  = TypeInfoToString(context, leftTableIdx);
	String rightStr = TypeInfoToString(context, rightTableIdx);
	switch (errorCode)
	{
	case TYPECHECK_SIGN_MISMATCH:
		LogError(context, sourceLoc, TPrintF(
			"Integer sign mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_SIZE_MISMATCH:
		LogError(context, sourceLoc, TPrintF(
			"Integer size mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_TYPE_CATEGORY_MISMATCH:
		LogError(context, sourceLoc, TPrintF(
			"Expression type mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_POINTED_TYPE_MISMATCH:
		LogError(context, sourceLoc, TPrintF(
			"Unrelated pointed types! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_ARRAY_SIZE_MISMATCH:
		LogError(context, sourceLoc, TPrintF(
			"Size of arrays are different! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_STRUCT_MISMATCH:
		LogError(context, sourceLoc, TPrintF(
			"Expressions evaluate to different structs! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_MISC_ERROR:
		LogError(context, sourceLoc, TPrintF(
			"Expression type mismatch! (left is %S and right is %S)", leftStr, rightStr));
	}
}

TypeCheckErrorCode CheckTypesMatch(Context *context, s64 leftTableIdx, s64 rightTableIdx)
{
	if (leftTableIdx == rightTableIdx)
		return TYPECHECK_COOL;

	// Allow anything to cast to Any
	s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);
	if (leftTableIdx == anyTableIdx || rightTableIdx == anyTableIdx)
		return TYPECHECK_COOL;

	TypeInfo left  = context->typeTable[leftTableIdx];
	TypeInfo right = context->typeTable[rightTableIdx];

	if (leftTableIdx == TYPETABLEIDX_BOOL)
	{
		if (right.typeCategory == TYPECATEGORY_INTEGER ||
			right.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}

	if (rightTableIdx == TYPETABLEIDX_INTEGER)
	{
		if (left.typeCategory == TYPECATEGORY_INTEGER ||
			left.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}
	else if (rightTableIdx == TYPETABLEIDX_FLOATING)
	{
		if (left.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}

	if (left.typeCategory != right.typeCategory)
	{
#if 1
		// Allow int->float and float->int
		if ((left.typeCategory == TYPECATEGORY_INTEGER ||
			left.typeCategory == TYPECATEGORY_FLOATING) &&
			right.typeCategory == TYPECATEGORY_INTEGER ||
			right.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;
#endif

		// Allow ptr = int? Confusing.
#if 1
		if ((left.typeCategory == TYPECATEGORY_POINTER &&
			right.typeCategory == TYPECATEGORY_INTEGER) ||
			(right.typeCategory == TYPECATEGORY_POINTER &&
			left.typeCategory == TYPECATEGORY_INTEGER))
			return TYPECHECK_COOL;
#endif

		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}

	switch (left.typeCategory)
	{
	case TYPECATEGORY_POINTER:
	{
		// Cast any pointer to void pointer
		if (left.pointerInfo.pointedTypeTableIdx == TYPETABLEIDX_VOID)
			return TYPECHECK_COOL;

		if (left.pointerInfo.pointedTypeTableIdx == right.pointerInfo.pointedTypeTableIdx)
			return TYPECHECK_COOL;

		return TYPECHECK_POINTED_TYPE_MISMATCH;
	} break;
	case TYPECATEGORY_ARRAY:
	{
		if (left.arrayInfo.count != right.arrayInfo.count &&
			left.arrayInfo.count != 0)
			return TYPECHECK_ARRAY_SIZE_MISMATCH;
		return TYPECHECK_COOL;
	} break;
	case TYPECATEGORY_STRUCT:
	case TYPECATEGORY_UNION:
	{
		return TYPECHECK_STRUCT_MISMATCH;
	} break;
	case TYPECATEGORY_INTEGER:
	{
		if (left.integerInfo.isSigned != right.integerInfo.isSigned)
			return TYPECHECK_SIGN_MISMATCH;
		if (left.size < right.size)
			return TYPECHECK_SIZE_MISMATCH;
		return TYPECHECK_COOL;
	} break;
	case TYPECATEGORY_FLOATING:
	{
		return TYPECHECK_COOL;
	} break;
	}

	return TYPECHECK_MISC_ERROR;
}

TypeCheckErrorCode CheckTypesMatchAndSpecialize(Context *context, s64 *leftTableIdx, ASTExpression *rightHand)
{
	if (rightHand->typeTableIdx == TYPETABLEIDX_STRUCT_LITERAL)
	{
		s64 structTypeIdx = *leftTableIdx;
		TypeInfo structTypeInfo = context->typeTable[structTypeIdx];
		ASSERT(structTypeInfo.typeCategory == TYPECATEGORY_STRUCT);

		ASSERT(rightHand->nodeType == ASTNODETYPE_LITERAL);
		ASSERT(rightHand->literal.type == LITERALTYPE_STRUCT);
		if (structTypeInfo.structInfo.members.size < rightHand->literal.members.size)
			LogError(context, rightHand->any.loc, "Too many values in struct literal"_s);
		for (int memberIdx = 0; memberIdx < rightHand->literal.members.size; ++memberIdx)
		{
			TypeCheckErrorCode errorCode = CheckTypesMatchAndSpecialize(context,
					&structTypeInfo.structInfo.members[memberIdx].typeTableIdx,
					rightHand->literal.members[memberIdx]);
			if (errorCode != TYPECHECK_COOL)
				Print("Type of struct literal value in position %d and "
						"type of struct member number %d don't match\n", memberIdx, memberIdx);
				ReportTypeCheckError(context, errorCode, rightHand->any.loc,
						structTypeInfo.structInfo.members[memberIdx].typeTableIdx,
						rightHand->literal.members[memberIdx]->typeTableIdx);
		}

		rightHand->typeTableIdx = structTypeIdx;
		return TYPECHECK_COOL;
	}

	TypeCategory leftTypeCat  = context->typeTable[*leftTableIdx].typeCategory;
	TypeCategory rightTypeCat = context->typeTable[rightHand->typeTableIdx].typeCategory;

	if (*leftTableIdx == TYPETABLEIDX_INTEGER && (rightTypeCat == TYPECATEGORY_INTEGER ||
				rightTypeCat == TYPECATEGORY_POINTER || rightTypeCat == TYPECATEGORY_FLOATING))
	{
		*leftTableIdx = rightHand->typeTableIdx;
		return TYPECHECK_COOL;
	}
	if (rightHand->typeTableIdx == TYPETABLEIDX_INTEGER && (leftTypeCat == TYPECATEGORY_INTEGER ||
				leftTypeCat == TYPECATEGORY_POINTER || leftTypeCat == TYPECATEGORY_FLOATING))
	{
		rightHand->typeTableIdx = *leftTableIdx;
		return TYPECHECK_COOL;
	}

	if (*leftTableIdx == TYPETABLEIDX_FLOATING && rightTypeCat == TYPECATEGORY_FLOATING)
	{
		*leftTableIdx = rightHand->typeTableIdx;
		return TYPECHECK_COOL;
	}
	if (rightHand->typeTableIdx == TYPETABLEIDX_FLOATING && leftTypeCat == TYPECATEGORY_FLOATING)
	{
		rightHand->typeTableIdx = *leftTableIdx;
		return TYPECHECK_COOL;
	}

	TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, *leftTableIdx, rightHand->typeTableIdx);
	if (typeCheckResult == TYPECHECK_COOL)
		return TYPECHECK_COOL;

	return TYPECHECK_MISC_ERROR;
}

// @Improve
TypeCheckErrorCode CheckTypesMatchAndSpecializeRight(Context *context, s64 leftTableIdx, ASTExpression *rightHand)
{
	return CheckTypesMatchAndSpecialize(context, &leftTableIdx, rightHand);
}

bool AreTypeInfosEqual(Context *context, TypeInfo a, TypeInfo b)
{
	if (a.typeCategory != b.typeCategory)
		return false;

	if (a.size != b.size)
		return false;

	switch (a.typeCategory)
	{
	case TYPECATEGORY_INTEGER:
		return a.integerInfo.isSigned == b.integerInfo.isSigned;
	case TYPECATEGORY_FLOATING:
		return true;
	case TYPECATEGORY_STRUCT:
		if (a.structInfo.members.size != b.structInfo.members.size)
			return false;
		for (int i = 0; i < a.structInfo.members.size; ++i)
			if (a.structInfo.members[i].typeTableIdx != b.structInfo.members[i].typeTableIdx)
				return false;
		return true;
	case TYPECATEGORY_UNION:
		if (a.structInfo.members.size != b.structInfo.members.size)
			return false;
		for (int i = 0; i < a.structInfo.members.size; ++i)
			if (a.structInfo.members[i].typeTableIdx != b.structInfo.members[i].typeTableIdx)
				return false;
		return true;
	case TYPECATEGORY_POINTER:
		return a.pointerInfo.pointedTypeTableIdx == b.pointerInfo.pointedTypeTableIdx;
	case TYPECATEGORY_ARRAY:
		return a.arrayInfo.elementTypeTableIdx == b.arrayInfo.elementTypeTableIdx &&
			a.arrayInfo.count == b.arrayInfo.count;
	case TYPECATEGORY_PROCEDURE:
		if (a.procedureInfo.parameters.size != b.procedureInfo.parameters.size)
			return false;
		if (a.procedureInfo.isVarargs != b.procedureInfo.isVarargs)
			return false;
		TypeInfo aReturnTypeInfo = context->typeTable[a.procedureInfo.returnTypeTableIdx];
		TypeInfo bReturnTypeInfo = context->typeTable[b.procedureInfo.returnTypeTableIdx];
		if (!AreTypeInfosEqual(context, aReturnTypeInfo, bReturnTypeInfo))
			return false;
		for (int i = 0; i < a.procedureInfo.parameters.size; ++i)
		{
			ProcedureParameter aParam = a.procedureInfo.parameters[i];
			ProcedureParameter bParam = b.procedureInfo.parameters[i];
			if (aParam.defaultValue.type != bParam.defaultValue.type)
				return false;
			if (aParam.defaultValue.type != CONSTANTTYPE_INVALID &&
					aParam.defaultValue.valueAsInt != bParam.defaultValue.valueAsInt)
				return false;
			TypeInfo aParamTypeInfo = context->typeTable[aParam.typeTableIdx];
			TypeInfo bParamTypeInfo = context->typeTable[bParam.typeTableIdx];
			if (!AreTypeInfosEqual(context, aParamTypeInfo, bParamTypeInfo))
				return false;
		}
		return true;
	case TYPECATEGORY_INVALID:
		return b.typeCategory == TYPECATEGORY_INVALID;
	default:
		CRASH;
	}
	return false;
}

inline s64 AddType(Context *context, TypeInfo typeInfo)
{
	s64 typeTableIdx = BucketArrayCount(&context->typeTable);
	typeInfo.valueIdx = NewValue(context, TPrintF("_typeInfo%lld", typeTableIdx), -1,
			VALUEFLAGS_ON_STATIC_STORAGE);
	*(TypeInfo *)BucketArrayAdd(&context->typeTable) = typeInfo;
	return typeTableIdx;
}

s64 FindOrAddTypeTableIdx(Context *context, TypeInfo typeInfo)
{
	u64 tableSize = BucketArrayCount(&context->typeTable);
	for (int i = 0; i < tableSize; ++i)
	{
		TypeInfo t = context->typeTable[i];
		if (AreTypeInfosEqual(context, typeInfo, t))
			return i;
	}
	return AddType(context, typeInfo);
}

// Util TypeInfo procedures
s64 GetTypeInfoPointerOf(Context *context, s64 inType)
{
	ASSERT(inType < (s64)BucketArrayCount(&context->typeTable));
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
		s64 elementSize = context->typeTable[inType].size;
		resultTypeInfo.size = elementSize * count;
	}
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

s64 TypeCheckType(Context *context, SourceLocation loc, ASTType *astType);

s64 TypeCheckStructDeclaration(Context *context, ASTStructDeclaration astStructDecl)
{
	TypeInfo t = {};
	t.typeCategory = astStructDecl.isUnion ? TYPECATEGORY_UNION : TYPECATEGORY_STRUCT;
	DynamicArrayInit(&t.structInfo.members, 16);

	int largestAlignment = 0;
	for (int memberIdx = 0; memberIdx < astStructDecl.members.size; ++memberIdx)
	{
		ASTStructMemberDeclaration astMember = astStructDecl.members[memberIdx];

		StructMember *member = DynamicArrayAdd(&t.structInfo.members);
		member->name = astMember.name;
		member->isUsing = astMember.isUsing;
		member->typeTableIdx = TypeCheckType(context, astMember.loc, astMember.astType);

		s64 memberSize = context->typeTable[member->typeTableIdx].size;
		int alignment = 8;
		if (memberSize < 8)
			alignment = NextPowerOf2((int)memberSize);

		if (alignment > largestAlignment)
			largestAlignment = alignment;

		if (!astStructDecl.isUnion)
		{
			if (t.size & (alignment - 1))
				t.size = (t.size & ~(alignment - 1)) + alignment;
			member->offset = t.size;
			t.size += memberSize;
		}
		else
		{
			member->offset = 0;
			if (t.size < memberSize)
				t.size = memberSize;
		}
	}
	if (t.size & (largestAlignment - 1))
		t.size = (t.size & ~(largestAlignment - 1)) + largestAlignment;

	s64 typeTableIdx = BucketArrayCount(&context->typeTable);
	AddType(context, t);

	*DynamicArrayAdd(&context->tcStack[context->tcStack.size - 1].typeIndices) = typeTableIdx;
	return typeTableIdx;
}

Constant EvaluateConstant(Context *context, ASTExpression *expression)
{
	Constant result;
	result.type = CONSTANTTYPE_INTEGER;
	result.valueAsInt = 0xFA11FA11FA11FA11;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_INTEGER:
		case LITERALTYPE_CHARACTER:
			result.valueAsInt = expression->literal.integer;
			break;
		case LITERALTYPE_FLOATING:
			result.type = CONSTANTTYPE_FLOATING;
			result.valueAsFloat = expression->literal.floating;
			break;
		default:
			goto error;
		}
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		if (expression->identifier.type == NAMETYPE_STATIC_DEFINITION)
			result = expression->identifier.staticDefinition->constant;
		else
			goto error;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTExpression *in = expression->unaryOperation.expression;
		Constant inValue  = EvaluateConstant(context, in);

		bool isFloat = inValue.type == CONSTANTTYPE_FLOATING;
		result.type = inValue.type;

		switch (expression->unaryOperation.op)
		{
		case TOKEN_OP_MINUS:
		{
			if (isFloat)
			{
				result.type = CONSTANTTYPE_FLOATING;
				result.valueAsFloat = -inValue.valueAsFloat;
			}
			else
				result.valueAsInt = -inValue.valueAsInt;
		} break;
		case TOKEN_OP_NOT:
		{
			if (isFloat)
			{
				result.type = CONSTANTTYPE_FLOATING;
				result.valueAsFloat = !inValue.valueAsFloat;
			}
			else
				result.valueAsInt = !inValue.valueAsInt;
		} break;
		case TOKEN_OP_BITWISE_NOT:
		{
			ASSERT(!isFloat);
			result.valueAsInt = ~inValue.valueAsInt;
		} break;
		default:
			goto error;
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *rightHand = expression->binaryOperation.rightHand;
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;
		Constant leftValue  = EvaluateConstant(context, leftHand);
		Constant rightValue = EvaluateConstant(context, rightHand);

		bool isFloat = false;
		if (leftValue.type == CONSTANTTYPE_INTEGER)
		{
			if (rightValue.type == CONSTANTTYPE_FLOATING)
			{
				rightValue.type = CONSTANTTYPE_INTEGER;
				rightValue.valueAsInt = (s64)rightValue.valueAsFloat;
			}
			result.type = CONSTANTTYPE_INTEGER;
		}
		if (leftValue.type == CONSTANTTYPE_FLOATING)
		{
			isFloat = true;
			if (rightValue.type == CONSTANTTYPE_INTEGER)
			{
				rightValue.type = CONSTANTTYPE_FLOATING;
				rightValue.valueAsFloat = (f64)rightValue.valueAsInt;
			}
			result.type = CONSTANTTYPE_FLOATING;
		}

		switch (expression->binaryOperation.op)
		{
		case TOKEN_OP_PLUS:
		{
			if (isFloat)
				result.valueAsFloat = leftValue.valueAsFloat + rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt + rightValue.valueAsInt;
		} break;
		case TOKEN_OP_MINUS:
		{
			if (isFloat)
				result.valueAsFloat = leftValue.valueAsFloat - rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt - rightValue.valueAsInt;
		} break;
		case TOKEN_OP_MULTIPLY:
		{
			if (isFloat)
				result.valueAsFloat = leftValue.valueAsFloat * rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt * rightValue.valueAsInt;
		} break;
		case TOKEN_OP_DIVIDE:
		{
			if (isFloat)
				result.valueAsFloat = leftValue.valueAsFloat / rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt / rightValue.valueAsInt;
		} break;
		case TOKEN_OP_MODULO:
		{
			if (isFloat)
				result.valueAsFloat = fmod(leftValue.valueAsFloat, rightValue.valueAsFloat);
			else
				result.valueAsInt = leftValue.valueAsInt % rightValue.valueAsInt;
		} break;
		case TOKEN_OP_SHIFT_LEFT:
		{
			ASSERT(!isFloat);
			result.valueAsInt = leftValue.valueAsInt << rightValue.valueAsInt;
		} break;
		case TOKEN_OP_SHIFT_RIGHT:
		{
			ASSERT(!isFloat);
			result.valueAsInt = leftValue.valueAsInt >> rightValue.valueAsInt;
		} break;
		case TOKEN_OP_AND:
		{
			if (isFloat)
				result.valueAsFloat = leftValue.valueAsFloat && rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt && rightValue.valueAsInt;
		} break;
		case TOKEN_OP_OR:
		{
			if (isFloat)
				result.valueAsFloat = leftValue.valueAsFloat || rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt || rightValue.valueAsInt;
		} break;
		case TOKEN_OP_BITWISE_AND:
		{
			ASSERT(!isFloat);
			result.valueAsInt = leftValue.valueAsInt & rightValue.valueAsInt;
		} break;
		case TOKEN_OP_BITWISE_OR:
		{
			ASSERT(!isFloat);
			result.valueAsInt = leftValue.valueAsInt | rightValue.valueAsInt;
		} break;
		case TOKEN_OP_BITWISE_XOR:
		{
			ASSERT(!isFloat);
			result.valueAsInt = leftValue.valueAsInt ^ rightValue.valueAsInt;
		} break;
		default:
			goto error;
		}
	} break;
	default:
		goto error;
	}
	return result;
error:
	LogError(context, expression->any.loc, "Failed to evaluate constant"_s);
	return {};
}

ASTProcedurePrototype TypeCheckProcedurePrototype(Context *context, ASTProcedurePrototype prototype);
TypeInfo TypeInfoFromASTProcedurePrototype(Context *context, ASTProcedurePrototype prototype);
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
		DynamicArrayInit(&t.enumInfo.names, 16);
		DynamicArrayInit(&t.enumInfo.values, 16);

		if (astType->enumDeclaration.astType)
		{
			SourceLocation astTypeLoc = astType->enumDeclaration.astType->loc;
			t.enumInfo.typeTableIdx = TypeCheckType(context, astTypeLoc, astType->enumDeclaration.astType);

			if (t.enumInfo.typeTableIdx < TYPETABLEIDX_PRIMITIVE_BEGIN ||
				t.enumInfo.typeTableIdx > TYPETABLEIDX_PRIMITIVE_END)
				LogError(context, astTypeLoc, "Only primitive types are allowed as enum field types"_s);
		}

		t.size = context->typeTable[t.enumInfo.typeTableIdx].size;

		s64 typeTableIdx = BucketArrayCount(&context->typeTable);

		s64 currentValue = 0;
		for (int memberIdx = 0; memberIdx < astType->enumDeclaration.members.size; ++memberIdx)
		{
			ASTEnumMember astMember = astType->enumDeclaration.members[memberIdx];

			StaticDefinition staticDefinition = {};
			staticDefinition.name = astMember.name;
			staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT;
			staticDefinition.typeTableIdx = typeTableIdx;

			if (astMember.value)
			{
				Constant constant = EvaluateConstant(context, astMember.value);
				if (constant.type == CONSTANTTYPE_FLOATING)
					currentValue = (s64)constant.valueAsFloat;
				else
					currentValue = constant.valueAsInt;
			}
			staticDefinition.constant.type = CONSTANTTYPE_INTEGER;
			staticDefinition.constant.valueAsInt = currentValue;

			StaticDefinition *newStaticDef = BucketArrayAdd(&context->staticDefinitions);
			*newStaticDef = staticDefinition;

			TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
			TCScopeName newName;
			newName.type = NAMETYPE_STATIC_DEFINITION;
			newName.name = astMember.name;
			newName.staticDefinition = newStaticDef;
			*DynamicArrayAdd(&stackTop->names) = newName;

			*DynamicArrayAdd(&t.enumInfo.names) = astMember.name;
			*DynamicArrayAdd(&t.enumInfo.values) = currentValue;
			++currentValue;
		}

		AddType(context, t);
		*DynamicArrayAdd(&context->tcStack[context->tcStack.size - 1].typeIndices) = typeTableIdx;

		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		astType->procedurePrototype = TypeCheckProcedurePrototype(context, astType->procedurePrototype);
		TypeInfo t = TypeInfoFromASTProcedurePrototype(context, astType->procedurePrototype);

		s64 typeTableIdx = FindOrAddTypeTableIdx(context, t);
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
				return IsTemporalValue(expression->binaryOperation.leftHand);
			if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
				return IsTemporalValue(expression->binaryOperation.leftHand);
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

void AddStructMembersToScope(Context *context, SourceLocation loc, u32 baseValueIdx,
		s64 typeTableIdx, DynamicArray<const StructMember *, malloc, realloc> *offsetStack)
{
	TypeInfo typeInfo = context->typeTable[typeTableIdx];
	ASSERT(typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		   typeInfo.typeCategory == TYPECATEGORY_UNION);

	Value *baseValue = &context->values[baseValueIdx];

	TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
	for (int memberIdx = 0; memberIdx < typeInfo.structInfo.members.size; ++memberIdx)
	{
		const StructMember *member = &typeInfo.structInfo.members[memberIdx];
		*DynamicArrayAdd(offsetStack) = member;

		if (member->name.size && !member->isUsing)
		{
			// Check if name already exists
			for (s64 i = 0; i < (s64)stackTop->names.size; ++i)
			{
				TCScopeName currentName = stackTop->names[i];
				if (StringEquals(member->name, currentName.name))
				{
					String fullMemberName = baseValue->name;
					for (int j = 0; j < offsetStack->size; ++j)
						fullMemberName = TPrintF("%S.%S", fullMemberName, (*offsetStack)[j]->name);

					LogError(context, loc, TPrintF("Failed to pull name \"%S\" into scope because "
								"name \"%S\" is already used", fullMemberName, member->name));
				}
			}

			// This variable can't be a register
			baseValue->flags |= VALUEFLAGS_FORCE_MEMORY;

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_STRUCT_MEMBER_CHAIN;
			newScopeName.name = member->name;
			newScopeName.structMemberChain.baseValueIdx = baseValueIdx;
			newScopeName.loc = loc;

			ArrayInit(&newScopeName.structMemberChain.offsets, offsetStack->size, malloc);
			for (int i = 0; i < offsetStack->size; ++i)
				*ArrayAdd(&newScopeName.structMemberChain.offsets) = (*offsetStack)[i];

			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}
		else
		{
			AddStructMembersToScope(context, loc, baseValueIdx, member->typeTableIdx, offsetStack);
		}
		--offsetStack->size;
	}
}

void TypeCheckExpression(Context *context, ASTExpression *expression);

ASTVariableDeclaration TypeCheckVariableDeclaration(Context *context, ASTVariableDeclaration varDecl)
{
	TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
	if (varDecl.name.size)
	{
		// Check if name already exists
		for (s64 i = 0; i < (s64)stackTop->names.size; ++i)
		{
			TCScopeName currentName = stackTop->names[i];
			if (StringEquals(varDecl.name, currentName.name))
			{
				LogErrorNoCrash(context, varDecl.loc, TPrintF("Duplicate name \"%S\" in scope", varDecl.name));
				LogNote(context, currentName.loc, "First defined here"_s);
				CRASH;
			}
		}
	}

	if (varDecl.astType)
	{
		varDecl.typeTableIdx = TypeCheckType(context, varDecl.loc, varDecl.astType);

		if (varDecl.typeTableIdx == TYPETABLEIDX_VOID)
			LogError(context, varDecl.loc, "Variable can't be of type void!"_s);
	}

	if (varDecl.astInitialValue)
	{
		TypeCheckExpression(context, varDecl.astInitialValue);

		if (varDecl.astType)
		{
			TypeCheckErrorCode typeCheckResult = CheckTypesMatchAndSpecialize(context,
					&varDecl.typeTableIdx, varDecl.astInitialValue);
			if (typeCheckResult != TYPECHECK_COOL)
			{
				Print("Variable declaration type and initial type don't match\n");
				ReportTypeCheckError(context, typeCheckResult, varDecl.loc, varDecl.typeTableIdx,
						varDecl.astInitialValue->typeTableIdx);
			}
		}
		else
		{
			if (varDecl.astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(context, varDecl.loc, "Variable can't be of type void!"_s);

			varDecl.typeTableIdx = InferType(varDecl.astInitialValue->typeTableIdx);
		}
	}

	return varDecl;
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
	case ASTNODETYPE_FOR:
	{
		return CheckIfReturnsValue(context, expression->forNode.body);
	}
	}
	return RETURNCHECKRESULT_NEVER;
}

const StructMember *FindStructMemberByName(Context *context, TypeInfo structTypeInfo, String name)
{
	for (int i = 0; i < structTypeInfo.structInfo.members.size; ++i)
	{
		const StructMember *currentMember = &structTypeInfo.structInfo.members[i];
		if (StringEquals(name, currentMember->name))
			return currentMember;
		if (currentMember->isUsing || currentMember->name.size == 0)
		{
			// Anonymous structs/unions and using
			TypeInfo memberTypeInfo = context->typeTable[currentMember->typeTableIdx];
			ASSERT(memberTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
				   memberTypeInfo.typeCategory == TYPECATEGORY_UNION);
			const StructMember *found = FindStructMemberByName(context, memberTypeInfo, name);
			if (found)
				return found;
		}
	}
	return nullptr;
}

ASTProcedurePrototype TypeCheckProcedurePrototype(Context *context, ASTProcedurePrototype prototype)
{
	// Parameters
	bool beginOptionalParameters = false;
	for (int i = 0; i < prototype.astParameters.size; ++i)
	{
		prototype.astParameters[i] = TypeCheckVariableDeclaration(context, prototype.astParameters[i]);

		ASTVariableDeclaration astVarDecl = prototype.astParameters[i];
		if (!astVarDecl.astInitialValue)
		{
			if (beginOptionalParameters)
				LogError(context, astVarDecl.loc, "Non-optional parameter after optional parameter found!"_s);
		}
		else
			beginOptionalParameters = true;
	}

	return prototype;
}

TypeInfo TypeInfoFromASTProcedurePrototype(Context *context, ASTProcedurePrototype prototype)
{
	TypeInfo t = {};
	t.size = g_pointerSize;
	t.typeCategory = TYPECATEGORY_PROCEDURE;
	DynamicArrayInit(&t.procedureInfo.parameters, 8);
	t.procedureInfo.isVarargs = prototype.isVarargs;

	// Parameters
	for (int i = 0; i < prototype.astParameters.size; ++i)
	{
		ASTVariableDeclaration astVarDecl = prototype.astParameters[i];

		ProcedureParameter *procParam = DynamicArrayAdd(&t.procedureInfo.parameters);
		procParam->typeTableIdx = astVarDecl.typeTableIdx;

		if (!astVarDecl.astInitialValue)
			procParam->defaultValue = {};
		else
			procParam->defaultValue = EvaluateConstant(context, astVarDecl.astInitialValue);
	}

	s64 returnType = TYPETABLEIDX_VOID;
	if (prototype.astReturnType)
		returnType = TypeCheckType(context, prototype.loc, prototype.astReturnType);
	t.procedureInfo.returnTypeTableIdx = returnType;

	return t;
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
		varDecl = TypeCheckVariableDeclaration(context, varDecl);
		expression->typeTableIdx = varDecl.typeTableIdx;

		u8 staticFlag = varDecl.isStatic ? VALUEFLAGS_ON_STATIC_STORAGE : 0;
		varDecl.valueIdx = NewValue(context, varDecl.name, varDecl.typeTableIdx, staticFlag);

		expression->variableDeclaration = varDecl;

		if (varDecl.name.size)
		{
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = varDecl.name;
			newScopeName.variableInfo.valueIdx = varDecl.valueIdx;
			newScopeName.variableInfo.typeTableIdx = varDecl.typeTableIdx;
			newScopeName.loc = varDecl.loc;

			TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}

		if (varDecl.isUsing || varDecl.name.size == 0)
		{
			TypeCategory typeCat = context->typeTable[expression->typeTableIdx].typeCategory;
			if (typeCat != TYPECATEGORY_STRUCT && typeCat != TYPECATEGORY_UNION)
				LogError(context, expression->any.loc, "Using keyword only accepts structs/unions!"_s);

			DynamicArray<const StructMember *, malloc, realloc> offsetStack;
			DynamicArrayInit(&offsetStack, 8);
			AddStructMembersToScope(context, varDecl.loc, varDecl.valueIdx, varDecl.typeTableIdx,
					&offsetStack);
		}
		else if (varDecl.name.size == 0)
		{
			// Anonymous struct!
			TypeCategory typeCat = context->typeTable[expression->typeTableIdx].typeCategory;
			if (typeCat != TYPECATEGORY_STRUCT && typeCat != TYPECATEGORY_UNION)
				LogError(context, expression->any.loc, "Anonymous variable has to be a struct/union!"_s);

			DynamicArray<const StructMember *, malloc, realloc> offsetStack;
			DynamicArrayInit(&offsetStack, 8);
			AddStructMembersToScope(context, varDecl.loc, varDecl.valueIdx, varDecl.typeTableIdx,
					&offsetStack);
		}
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition astStaticDef = expression->staticDefinition;

		// Check if already exists
		TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
		for (s64 i = 0; i < (s64)stackTop->names.size; ++i)
		{
			TCScopeName currentName = stackTop->names[i];
			if (StringEquals(astStaticDef.name, currentName.name))
			{
				LogErrorNoCrash(context, expression->any.loc,
						TPrintF("Duplicate static definition \"%S\"", astStaticDef.name));
				LogNote(context, currentName.loc, "First defined here"_s);
				CRASH;
			}
		}

		StaticDefinition *newStaticDef = BucketArrayAdd(&context->staticDefinitions);
		newStaticDef->name = astStaticDef.name;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_STATIC_DEFINITION;
		newScopeName.name = astStaticDef.name;
		newScopeName.staticDefinition = newStaticDef;
		newScopeName.loc = astStaticDef.loc;
		*DynamicArrayAdd(&stackTop->names) = newScopeName;

		if (astStaticDef.expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
		{
			newStaticDef->definitionType = STATICDEFINITIONTYPE_PROCEDURE;
			newStaticDef->procedureIdx = astStaticDef.expression->procedureDeclaration.procedureIdx;
		}
		else if (astStaticDef.expression->nodeType == ASTNODETYPE_TYPE)
		{
			newStaticDef->definitionType = STATICDEFINITIONTYPE_TYPE;
		}
		else
		{
			newStaticDef->definitionType = STATICDEFINITIONTYPE_CONSTANT;
		}
		TypeCheckExpression(context, astStaticDef.expression);
		newStaticDef->typeTableIdx = astStaticDef.expression->typeTableIdx;

		// Need to evaluate constant AFTER type checking expression.
		if (newStaticDef->definitionType == STATICDEFINITIONTYPE_CONSTANT)
			newStaticDef->constant = EvaluateConstant(context, astStaticDef.expression);
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		ASTProcedureDeclaration *procDecl = &expression->procedureDeclaration;
		Procedure *procedure = GetProcedure(context, procDecl->procedureIdx);

		PushTCScope(context);

		procDecl->prototype = TypeCheckProcedurePrototype(context, procDecl->prototype);
		TypeInfo t = TypeInfoFromASTProcedurePrototype(context, procDecl->prototype);

		TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];

		// Parameters
		DynamicArrayInit(&procedure->parameterValues, 8);
		for (int i = 0; i < procDecl->prototype.astParameters.size; ++i)
		{
			ASTVariableDeclaration astVarDecl = procDecl->prototype.astParameters[i];
			u32 paramValueIdx = NewValue(context, astVarDecl.name, astVarDecl.typeTableIdx, 0);
			*DynamicArrayAdd(&procedure->parameterValues) = paramValueIdx;

			if (astVarDecl.isUsing)
			{
				DynamicArray<const StructMember *, malloc, realloc> offsetStack;
				DynamicArrayInit(&offsetStack, 8);
				AddStructMembersToScope(context, astVarDecl.loc, paramValueIdx,
						astVarDecl.typeTableIdx, &offsetStack);
			}

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = astVarDecl.name;
			newScopeName.variableInfo.valueIdx = paramValueIdx;
			newScopeName.variableInfo.typeTableIdx = astVarDecl.typeTableIdx;
			newScopeName.loc = astVarDecl.loc;
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}

		// Varargs array
		if (procDecl->prototype.isVarargs)
		{
			s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);
			s64 arrayTableIdx = GetTypeInfoArrayOf(context, anyTableIdx, 0);
			u32 valueIdx = NewValue(context, procDecl->prototype.varargsName, arrayTableIdx, 0);

			*DynamicArrayAdd(&procedure->parameterValues) = valueIdx;

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = procDecl->prototype.varargsName;
			newScopeName.variableInfo.valueIdx = valueIdx;
			newScopeName.variableInfo.typeTableIdx = arrayTableIdx;
			newScopeName.loc = procDecl->loc;
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}

		s64 returnType = TYPETABLEIDX_VOID;
		if (procDecl->prototype.astReturnType)
			returnType = TypeCheckType(context, expression->any.loc, procDecl->prototype.astReturnType);
		t.procedureInfo.returnTypeTableIdx = returnType;

		s64 typeTableIdx = FindOrAddTypeTableIdx(context, t);
		procedure->typeTableIdx = typeTableIdx;
		expression->typeTableIdx = typeTableIdx;

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
				LogError(context, expression->any.loc, "Procedure doesn't always return a value"_s);
			else if (result == RETURNCHECKRESULT_NEVER)
				LogError(context, expression->any.loc, "Procedure has to return a value"_s);
		}
	} break;
	case ASTNODETYPE_RETURN:
	{
		TypeCheckErrorCode typeCheckResult;
		if (expression->returnNode.expression != nullptr)
		{
			TypeCheckExpression(context, expression->returnNode.expression);
			typeCheckResult = CheckTypesMatchAndSpecializeRight(context,
					context->tcCurrentReturnType, expression->returnNode.expression);
		}
		else
			typeCheckResult = CheckTypesMatch(context, context->tcCurrentReturnType, TYPETABLEIDX_VOID);

		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "Incorrect return type"_s);
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
						expression->identifier.valueIdx = currentName.variableInfo.valueIdx;
						expression->typeTableIdx = currentName.variableInfo.typeTableIdx;
					} break;
					case NAMETYPE_STRUCT_MEMBER:
					{
						expression->identifier.structMemberInfo.baseValueIdx =
							currentName.structMemberInfo.baseValueIdx;
						expression->identifier.structMemberInfo.structMember =
							currentName.structMemberInfo.structMember;
						expression->typeTableIdx = currentName.structMemberInfo.structMember->typeTableIdx;
					} break;
					case NAMETYPE_STRUCT_MEMBER_CHAIN:
					{
						expression->identifier.structMemberChain.baseValueIdx =
							currentName.structMemberChain.baseValueIdx;
						expression->identifier.structMemberChain.offsets =
							currentName.structMemberChain.offsets;
						int lastIdx = (int)currentName.structMemberChain.offsets.size - 1;
						expression->typeTableIdx = currentName.structMemberChain.offsets[lastIdx]->typeTableIdx;
					} break;
					case NAMETYPE_STATIC_DEFINITION:
					{
						expression->identifier.staticDefinition = currentName.staticDefinition;
						expression->typeTableIdx = currentName.staticDefinition->typeTableIdx;
					} break;
					}
					goto skipInvalidIdentifierError;
				}
			}
		}
		LogError(context, expression->any.loc, TPrintF("Invalid variable \"%S\" referenced", string));

skipInvalidIdentifierError:
		if (expression->identifier.isUsing)
		{
			if (expression->identifier.type != NAMETYPE_VARIABLE)
				LogError(context, expression->any.loc,
						"Expression after 'using' does not evaluate to a variable"_s);

			TypeCategory typeCat = context->typeTable[expression->typeTableIdx].typeCategory;
			if (typeCat != TYPECATEGORY_STRUCT)
				LogError(context, expression->any.loc, "Using keyword only accepts structs!"_s);

			u32 baseValueIdx = expression->identifier.valueIdx;

			DynamicArray<const StructMember *, malloc, realloc> offsetStack;
			DynamicArrayInit(&offsetStack, 8);
			AddStructMembersToScope(context, expression->any.loc, baseValueIdx,
					expression->typeTableIdx, &offsetStack);
		}
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String procName = expression->procedureCall.name;

		// Search backwards so we find procedures higher in the stack first.
		bool isIndirect = false;
		s32 procedureIdx = S32_MIN;
		s32 procedurePointerValueIdx = U32_MAX;
		s64 procedureTypeIdx = -1;
		for (s64 stackIdx = context->tcStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			TCScope *currentScope = &context->tcStack[stackIdx];
			for (int i = 0; i < currentScope->names.size; ++i)
			{
				TCScopeName currentName = currentScope->names[i];
				if (StringEquals(procName, currentName.name))
				{
					if (currentName.type == NAMETYPE_VARIABLE)
					{
						isIndirect = true;
						procedurePointerValueIdx = currentName.variableInfo.valueIdx;
						procedureTypeIdx = currentName.variableInfo.typeTableIdx;
						goto skipNotFoundError;
					}
					if (currentName.type == NAMETYPE_STATIC_DEFINITION &&
						currentName.staticDefinition->definitionType == STATICDEFINITIONTYPE_PROCEDURE)
					{
						procedureIdx = currentName.staticDefinition->procedureIdx;
						procedureTypeIdx = GetProcedure(context, procedureIdx)->typeTableIdx;
						goto skipNotFoundError;
					}

					LogError(context, expression->any.loc, "Calling a non-procedure"_s);
				}
			}
		}

		LogError(context, expression->any.loc, TPrintF("Invalid procedure \"%S\" called", procName));

skipNotFoundError:
		{
			ASSERT(context->typeTable[procedureTypeIdx].typeCategory == TYPECATEGORY_PROCEDURE);
			TypeInfoProcedure procTypeInfo = context->typeTable[procedureTypeIdx].procedureInfo;

			expression->typeTableIdx = procTypeInfo.returnTypeTableIdx;
			expression->procedureCall.isIndirect = isIndirect;
			if (isIndirect)
				expression->procedureCall.valueIdx = procedurePointerValueIdx;
			else
				expression->procedureCall.procedureIdx = procedureIdx;

			// Type check arguments
			s64 requiredArguments = 0;
			for (int i = 0; i < procTypeInfo.parameters.size; ++i)
			{
				if (procTypeInfo.parameters[i].defaultValue.type == CONSTANTTYPE_INVALID)
					++requiredArguments;
			}

			s64 totalArguments = procTypeInfo.parameters.size;
			s64 givenArguments  = expression->procedureCall.arguments.size;
			if (procTypeInfo.isVarargs)
			{
				if (requiredArguments > givenArguments)
					LogError(context, expression->any.loc,
							TPrintF("Procedure \"%S\" needs at least %d arguments but only %d were given",
								procName, requiredArguments, givenArguments));
			}
			else
			{
				if (requiredArguments > givenArguments)
					LogError(context, expression->any.loc,
							TPrintF("Procedure \"%S\" needs at least %d arguments but only %d were given",
							procName, requiredArguments, givenArguments));

				if (givenArguments > totalArguments)
					LogError(context, expression->any.loc,
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
				s64 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
				TypeCheckErrorCode typeCheckResult = CheckTypesMatchAndSpecializeRight(context,
						paramTypeIdx, arg);

				if (typeCheckResult != TYPECHECK_COOL)
				{
					String paramStr = TypeInfoToString(context, paramTypeIdx);
					String givenStr = TypeInfoToString(context, arg->typeTableIdx);
					LogError(context, arg->any.loc, TPrintF("When calling procedure \"%S\": type of "
								"parameter #%d didn't match (parameter is %S but %S was given)",
								procName, argIdx, paramStr, givenStr));
				}
			}
			break;
		}
	}
	case ASTNODETYPE_UNARY_OPERATION:
	{
		TypeCheckExpression(context, expression->unaryOperation.expression);
		u64 expressionType = expression->unaryOperation.expression->typeTableIdx;
		switch (expression->unaryOperation.op)
		{
		case TOKEN_OP_NOT:
		{
			TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
					expressionType);
			if (typeCheckResult != TYPECHECK_COOL)
				LogError(context, expression->any.loc, "Expression can't be cast to boolean"_s);
			expression->typeTableIdx = TYPETABLEIDX_BOOL;
		} break;
		case TOKEN_OP_POINTER_TO:
		{
			// Forbid pointer to temporal values
			if (IsTemporalValue(expression->unaryOperation.expression))
				LogError(context, expression->any.loc, "Trying to get pointer to temporal value"_s);

			ASTExpression *e = expression->unaryOperation.expression;
			switch (e->nodeType)
			{
			case ASTNODETYPE_IDENTIFIER:
			{
				if (e->identifier.type == NAMETYPE_VARIABLE)
					context->values[e->identifier.valueIdx].flags |= VALUEFLAGS_FORCE_MEMORY;
			} break;
			}

			expression->typeTableIdx = GetTypeInfoPointerOf(context, expressionType);
		} break;
		case TOKEN_OP_DEREFERENCE:
		{
			TypeInfo expressionTypeInfo = context->typeTable[expressionType];
			if (expressionTypeInfo.typeCategory != TYPECATEGORY_POINTER)
				LogError(context, expression->any.loc, "Trying to dereference a non pointer"_s);
			expression->typeTableIdx = expressionTypeInfo.pointerInfo.pointedTypeTableIdx;
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

			if (rightHand->nodeType != ASTNODETYPE_IDENTIFIER)
			{
				LogError(context, rightHand->any.loc, "Expected identifier after member access operator"_s);
			}

			rightHand->identifier.type = NAMETYPE_STRUCT_MEMBER;

			TypeInfo structTypeInfo = context->typeTable[leftHandTypeIdx];
			if (structTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			{
				s64 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
				structTypeInfo = context->typeTable[pointedTypeIdx];
			}

			if (structTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
			{
				// This is only for dynamic size arrays!
				if (structTypeInfo.arrayInfo.count != 0)
					LogError(context, expression->any.loc, "Array left of '.' has to be of dynamic size! ([])"_s);

				s64 arrayTypeTableIdx = FindTypeInStackByName(context, {}, "Array"_s);
				structTypeInfo = context->typeTable[arrayTypeTableIdx];
			}
			else if (structTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
					 structTypeInfo.typeCategory != TYPECATEGORY_UNION)
			{
				LogError(context, expression->any.loc, "Left of '.' has to be a struct/union"_s);
			}

			String memberName = rightHand->identifier.string;
			const StructMember *foundMember = FindStructMemberByName(context, structTypeInfo, memberName);
			if (foundMember)
			{
				// We don't need the base in this case, just the top most offset.
				rightHand->identifier.structMemberInfo.baseValueIdx = U32_MAX;
				rightHand->identifier.structMemberInfo.structMember = foundMember;
				expression->typeTableIdx = foundMember->typeTableIdx;
				return;
			}

			LogError(context, expression->any.loc, TPrintF("\"%S\" is not a member of \"%S\"",
						memberName, TypeInfoToString(context, leftHandTypeIdx)));
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
		{
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);

			s64 arrayType = leftHand->typeTableIdx;
			TypeInfo arrayTypeInfo = context->typeTable[arrayType];
			if (arrayTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			{
				s64 pointedTypeIdx = arrayTypeInfo.pointerInfo.pointedTypeTableIdx;
				arrayType = pointedTypeIdx;
				arrayTypeInfo = context->typeTable[pointedTypeIdx];
			}

			s64 stringTypeIdx = FindTypeInStackByName(context, {}, "String"_s);
			if (arrayType == stringTypeIdx)
			{
				expression->typeTableIdx = TYPETABLEIDX_U8;
			}
			else
			{
				if (arrayTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
					LogError(context, leftHand->any.loc,
							"Expression does not evaluate to an array"_s);
				expression->typeTableIdx = arrayTypeInfo.arrayInfo.elementTypeTableIdx;
			}
		}
		else
		{
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);

			TypeCheckErrorCode typeCheckResult = CheckTypesMatchAndSpecialize(context,
					&leftHand->typeTableIdx, rightHand);

			if (typeCheckResult != TYPECHECK_COOL)
			{
				String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
				String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
				LogError(context, expression->any.loc, TPrintF("Type mismatch! (%S and %S)",
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
		case LITERALTYPE_STRUCT:
			expression->typeTableIdx = TYPETABLEIDX_STRUCT_LITERAL;
			for (int memberIdx = 0; memberIdx < expression->literal.members.size; ++memberIdx)
				TypeCheckExpression(context, expression->literal.members[memberIdx]);
			break;
		default:
			ASSERT(!"Unexpected literal type");
		}
	} break;
	case ASTNODETYPE_IF:
	{
		TypeCheckExpression(context, expression->ifNode.condition);
		s64 conditionType = expression->ifNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->ifNode.body);

		if (expression->ifNode.elseBody)
			TypeCheckExpression(context, expression->ifNode.elseBody);
	} break;
	case ASTNODETYPE_WHILE:
	{
		TypeCheckExpression(context, expression->whileNode.condition);
		s64 conditionType = expression->whileNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "While condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->whileNode.body);
	} break;
	case ASTNODETYPE_FOR:
	{
		TypeCheckExpression(context, expression->forNode.range);

		PushTCScope(context);

		String indexValueName = "i"_s;
		u32 indexValueIdx = NewValue(context, indexValueName, TYPETABLEIDX_S64, 0);
		expression->forNode.indexValueIdx = indexValueIdx;

		TCScope *stackTop = &context->tcStack[context->tcStack.size - 1];
		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = indexValueName;
		newScopeName.variableInfo.valueIdx = indexValueIdx;
		newScopeName.variableInfo.typeTableIdx = TYPETABLEIDX_S64;
		newScopeName.loc = expression->any.loc;
		*DynamicArrayAdd(&stackTop->names) = newScopeName;

		ASTExpression *rangeExp = expression->forNode.range;
		bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			rangeExp->binaryOperation.op == TOKEN_OP_RANGE;

		if (!isExplicitRange)
		{
			s64 elementTypeTableIdx = TYPETABLEIDX_U8;
			s64 stringTypeIdx = FindTypeInStackByName(context, {}, "String"_s);
			if (rangeExp->typeTableIdx != stringTypeIdx)
			{
				TypeInfo rangeTypeInfo = context->typeTable[rangeExp->typeTableIdx];
				if (rangeTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
					LogError(context, expression->forNode.range->any.loc, "'for' range expression "
							"does not evaluate to an array nor is it a number range (..)"_s);
				elementTypeTableIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			}

			s64 pointerToElementTypeTableIdx = GetTypeInfoPointerOf(context, elementTypeTableIdx);
			String elementValueName = "it"_s;
			u32 elementValueIdx = NewValue(context, elementValueName, pointerToElementTypeTableIdx, 0);
			expression->forNode.elementValueIdx = elementValueIdx;

			newScopeName.name = elementValueName;
			newScopeName.variableInfo.valueIdx = elementValueIdx;
			newScopeName.variableInfo.typeTableIdx = pointerToElementTypeTableIdx;
			newScopeName.loc = expression->any.loc;
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}

		TypeCheckExpression(context, expression->forNode.body);

		PopTCScope(context);
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
	case ASTNODETYPE_SIZEOF:
	{
		TypeCheckExpression(context, expression->sizeOfNode.expression);
		expression->typeTableIdx = TYPETABLEIDX_S64;
	} break;
	case ASTNODETYPE_CAST:
	{
		TypeCheckExpression(context, expression->castNode.expression);
		expression->typeTableIdx = TypeCheckType(context, expression->any.loc, &expression->castNode.astType);
	} break;
	default:
	{
		LogError(context, expression->any.loc, "COMPILER ERROR! Unknown expression type on type checking"_s);
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

	TypeInfo *typeTable = (TypeInfo *)context->typeTable.buckets[0].data;

	{
		TypeInfo t;
		t.typeCategory = TYPECATEGORY_INTEGER;
		t.integerInfo.isSigned = true;

		t.size = 1;
		t.valueIdx = NewValue(context, "_typeInfo_s8"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_S8]  = t;
		t.size = 2;
		t.valueIdx = NewValue(context, "_typeInfo_s16"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_S16] = t;
		t.size = 4;
		t.valueIdx = NewValue(context, "_typeInfo_s32"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_S32] = t;
		t.size = 8;
		t.valueIdx = NewValue(context, "_typeInfo_s64"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_S64] = t;
		t.valueIdx = NewValue(context, "_typeInfo_integer"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_INTEGER] = t;

		t.integerInfo.isSigned = false;

		t.size = 1;
		t.valueIdx = NewValue(context, "_typeInfo_u8"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_U8]  = t;
		t.valueIdx = NewValue(context, "_typeInfo_bool"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_BOOL]  = t;
		t.size = 2;
		t.valueIdx = NewValue(context, "_typeInfo_u16"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_U16] = t;
		t.size = 4;
		t.valueIdx = NewValue(context, "_typeInfo_u32"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_U32] = t;
		t.size = 8;
		t.valueIdx = NewValue(context, "_typeInfo_u64"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_U64] = t;

		t.typeCategory = TYPECATEGORY_FLOATING;
		t.size = 4;
		t.valueIdx = NewValue(context, "_typeInfo_f32"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_F32] = t;
		t.size = 8;
		t.valueIdx = NewValue(context, "_typeInfo_f64"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_F64] = t;
		t.valueIdx = NewValue(context, "_typeInfo_floating"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_FLOATING] = t;

		t = {};
		t.typeCategory = TYPECATEGORY_INVALID;
		t.valueIdx = NewValue(context, "_typeInfo_void"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_VOID] = t;
	}
	for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
		*DynamicArrayAdd(&context->tcStack[0].typeIndices) = i;

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		TypeCheckExpression(context, statement);
	}
}
