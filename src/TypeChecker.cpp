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
	VALUEFLAGS_HAS_PUSH_INSTRUCTION = 256,
	VALUEFLAGS_PARAMETER_BY_COPY    = 512, // These values are pointers behind the scenes
	VALUEFLAGS_TRY_IMMITATE         = 1024
};

struct Value
{
	String name;
	s64 typeTableIdx;
	u32 flags;

	// Back end
	union
	{
		s32 allocatedRegister;
		s32 stackOffset;
	};
	u32 tryImmitateValueIdx;
};

enum ConstantType
{
	CONSTANTTYPE_INVALID = 0,
	CONSTANTTYPE_INTEGER,
	CONSTANTTYPE_FLOATING,
	CONSTANTTYPE_GROUP,
};
struct Constant
{
	ConstantType type;
	union
	{
		s64 valueAsInt;
		f64 valueAsFloat;
		Array<Constant, FrameAllocator> valueAsGroup;
	};
	s64 typeTableIdx;
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
struct OperatorOverload
{
	enum TokenType op;
	s32 procedureIdx;
};
struct TypeInfoStruct
{
	String name;
	Array<StructMember, FrameAllocator> members;
	Array<OperatorOverload, FrameAllocator> overloads;
};

struct TypeInfoEnum
{
	String name;
	s64 typeTableIdx;
	Array<String, FrameAllocator> names;
	Array<s64, FrameAllocator> values;
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
	Array<ProcedureParameter, FrameAllocator> parameters;
	bool isVarargs;
};

struct TypeInfo
{
	TypeCategory typeCategory;
	u32 valueIdx; // Value with runtime type information.
	u64 size;
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
			TCValue tcValue;
			s64 typeTableIdx;
		} variableInfo;
		const StructMember *structMember;
		ASTExpression *expression;
		StaticDefinition *staticDefinition;
	};
};
struct TCScope
{
	DynamicArray<TCScopeName, PhaseAllocator> names;
	DynamicArray<s64, PhaseAllocator> typeIndices;
};

struct IRInstruction;
struct Procedure
{
	String name;
	DynamicArray<u32, FrameAllocator> parameterValues;
	ASTProcedureDeclaration *astProcedureDeclaration;
	bool isInline;
	bool isBodyTypeChecked;
	u32 returnValueIdx;
	s64 typeTableIdx; // Type of the procedure

	// IRGen
	BucketArray<IRInstruction, FrameAllocator, 256> instructions;
	s64 allocatedParameterCount;
};

enum TCYieldCause
{
	TCYIELDCAUSE_NONE,
	TCYIELDCAUSE_DONE,
	TCYIELDCAUSE_MISSING_NAME,
	TCYIELDCAUSE_NEED_TYPE_CHECKING
};
struct TCYieldInfo
{
	TCYieldCause cause;
	String name;
	SourceLocation loc;
};

struct TCJob
{
	ASTExpression *expression;
	TCYieldInfo yieldInfo;
	DynamicArray<TCScope, PhaseAllocator> scopeStack;
};

struct TypeCheckTypeResult
{
	bool success;
	TCYieldInfo yieldInfo;
	s64 typeTableIdx;
};

struct FindTypeResult
{
	bool success;
	TCYieldInfo yieldInfo;
	s64 typeTableIdx;
};

struct TypeCheckStructResult
{
	bool success;
	TCYieldInfo yieldInfo;
	s64 typeTableIdx;
};

struct TypeCheckVariableResult
{
	bool success;
	TCYieldInfo yieldInfo;
};

struct TypeCheckProcedurePrototypeResult
{
	bool success;
	TCYieldInfo yieldInfo;
};

struct TypeCheckExpressionResult
{
	bool success;
	TCYieldInfo yieldInfo;
	s64 typeTableIdx;
};

u32 NewValue(Context *context, s64 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);
	u64 idx = BucketArrayCount(&context->values);
	Value *result = BucketArrayAdd(&context->values);
	result->name = {};
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 NewValue(Context *context, String name, s64 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);
	u64 idx = BucketArrayCount(&context->values);
	Value *result = BucketArrayAdd(&context->values);
	result->name = name;
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

inline Procedure *GetProcedure(Context *context, s32 procedureIdx)
{
	ASSERT(procedureIdx != 0);
	if (procedureIdx > 0)
		return &context->procedures[procedureIdx];
	else
		return &context->externalProcedures[-procedureIdx];
}

TCScope *GetTopMostScope(Context *context)
{
	TCJob currentJob = context->tcJobs[context->currentTCJob];
	if (currentJob.scopeStack.size > 0)
		return DynamicArrayBack(&currentJob.scopeStack);
	else
		return context->tcGlobalScope;
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
	TCScope *newScope =
		DynamicArrayAdd(&context->tcJobs[context->currentTCJob].scopeStack);

	DynamicArrayInit(&newScope->names, 64);
	DynamicArrayInit(&newScope->typeIndices, 64);
}

void PopTCScope(Context *context)
{
	--context->tcJobs[context->currentTCJob].scopeStack.size;
}

TCScopeName *FindScopeName(Context *context, String name)
{
	// Current stack
	DynamicArray<TCScope, PhaseAllocator> scopeStack =
		context->tcJobs[context->currentTCJob].scopeStack;
	for (s64 stackIdx = scopeStack.size - 1; stackIdx >= 0; --stackIdx)
	{
		TCScope *currentScope = &scopeStack[stackIdx];
		for (int i = 0; i < currentScope->names.size; ++i)
		{
			TCScopeName *currentName = &currentScope->names[i];
			if (StringEquals(name, currentName->name))
				return currentName;
		}
	}
	// Global scope
	for (int i = 0; i < context->tcGlobalScope->names.size; ++i)
	{
		TCScopeName *currentName = &context->tcGlobalScope->names[i];
		if (StringEquals(name, currentName->name))
			return currentName;
	}
	return nullptr;
}

FindTypeResult FindTypeInStackByName(Context *context, SourceLocation loc, String name)
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
		TCScopeName *scopeName = FindScopeName(context, name);

		if (scopeName == nullptr)
		{
#if PRINT_DEBUG_NOTES_UPON_YIELDING
			LogNote(context, loc, TPrintF("Type \"%S\" not in scope!", name));
#endif
			TCYieldInfo yieldInfo = {};
			yieldInfo.cause = TCYIELDCAUSE_MISSING_NAME;
			yieldInfo.name = name;
			yieldInfo.loc = loc;
			return { false, yieldInfo };
		}
		else if (scopeName->type != NAMETYPE_STATIC_DEFINITION)
			LogError(context, loc, TPrintF("\"%S\" is not a type!",
						name));
		else if (scopeName->staticDefinition->definitionType != STATICDEFINITIONTYPE_TYPE)
			LogError(context, loc, TPrintF("\"%S\" is not a type!", name));

		if (scopeName->staticDefinition->typeTableIdx == TYPETABLEIDX_UNSET)
		{
#if PRINT_DEBUG_NOTES_UPON_YIELDING
			LogNote(context, loc, TPrintF("Type \"%S\" not yet type checked!", name));
#endif
			TCYieldInfo yieldInfo = {};
			yieldInfo.cause = TCYIELDCAUSE_NEED_TYPE_CHECKING;
			yieldInfo.name = name;
			yieldInfo.loc = loc;
			return { false, yieldInfo };
		}

		typeTableIdx = scopeName->staticDefinition->typeTableIdx;
	}

	return { true, {}, typeTableIdx };
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
	TYPECHECK_CANT_DEDUCE_TYPE
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
	default:
		ASSERT(errorCode == TYPECHECK_COOL);
	}
}

s64 GetTypeInfoPointerOf(Context *context, s64 inType);
TypeCheckErrorCode CheckTypesMatch(Context *context, s64 leftTableIdx, s64 rightTableIdx)
{
	static s64 voidPointerIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

	if ((leftTableIdx == TYPETABLEIDX_VOID) != (rightTableIdx == TYPETABLEIDX_VOID))
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;

	if (leftTableIdx == rightTableIdx)
		return TYPECHECK_COOL;

	// Allow anything to cast to Any
	if (leftTableIdx == TYPETABLEIDX_ANY_STRUCT || rightTableIdx == TYPETABLEIDX_ANY_STRUCT)
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
		// Allow int->float and float->int
#if 1
		if ((left.typeCategory == TYPECATEGORY_INTEGER ||
			left.typeCategory == TYPECATEGORY_FLOATING) &&
			right.typeCategory == TYPECATEGORY_INTEGER ||
			right.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;
#endif

		// @Check: Allow ptr = int? Confusing.
#if 1
		if ((left.typeCategory == TYPECATEGORY_POINTER &&
			right.typeCategory == TYPECATEGORY_INTEGER) ||
			(right.typeCategory == TYPECATEGORY_POINTER &&
			left.typeCategory == TYPECATEGORY_INTEGER))
			return TYPECHECK_COOL;
#endif

		// Allow proc = ^void
#if 1
		if ((leftTableIdx == voidPointerIdx &&
			right.typeCategory == TYPECATEGORY_PROCEDURE) ||
			(rightTableIdx == voidPointerIdx &&
			left.typeCategory == TYPECATEGORY_PROCEDURE))
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

		// Allow implicit ^[T] -> ^T
		TypeInfo pointedLeft  = context->typeTable[left .pointerInfo.pointedTypeTableIdx];
		TypeInfo pointedRight = context->typeTable[right.pointerInfo.pointedTypeTableIdx];
		if (pointedLeft.typeCategory == TYPECATEGORY_ARRAY &&
			pointedRight.typeCategory != TYPECATEGORY_ARRAY)
		{
			return CheckTypesMatch(context, pointedLeft.arrayInfo.elementTypeTableIdx,
				right.pointerInfo.pointedTypeTableIdx);
		}
		if (pointedRight.typeCategory == TYPECATEGORY_ARRAY &&
			pointedLeft.typeCategory != TYPECATEGORY_ARRAY)
		{
			return CheckTypesMatch(context, pointedRight.arrayInfo.elementTypeTableIdx,
			left.pointerInfo.pointedTypeTableIdx);
		}

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

struct TypeCheckResult
{
	TypeCheckErrorCode errorCode;
	s64 leftTableIdx;
	s64 rightTableIdx;
};
TypeCheckResult CheckTypesMatchAndSpecialize(Context *context, s64 leftTableIdx, const ASTExpression *rightHand)
{
	s64 rightTableIdx = rightHand->typeTableIdx;

	ASSERT(leftTableIdx  != TYPETABLEIDX_UNSET);
	ASSERT(rightTableIdx != TYPETABLEIDX_UNSET);

	TypeCheckResult result = { TYPECHECK_COOL, leftTableIdx, rightTableIdx };

	if (rightTableIdx == TYPETABLEIDX_STRUCT_LITERAL)
	{
		ASSERT(rightHand->nodeType == ASTNODETYPE_LITERAL);
		ASSERT(rightHand->literal.type == LITERALTYPE_GROUP);

		s64 structTypeIdx = leftTableIdx;
		TypeInfo structTypeInfo = context->typeTable[structTypeIdx];
		if (structTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			structTypeInfo.typeCategory == TYPECATEGORY_UNION)
		{
			struct StructStackFrame
			{
				s64 structTypeIdx;
				int idx;
			};
			DynamicArray<StructStackFrame, PhaseAllocator> structStack;
			DynamicArrayInit(&structStack, 8);
			*DynamicArrayAdd(&structStack) = { structTypeIdx, 0 };

			int memberIdx = 0;
			while (memberIdx < rightHand->literal.members.size)
			{
				StructStackFrame currentFrame = structStack[structStack.size - 1];
				TypeInfo currentStructTypeInfo = context->typeTable[currentFrame.structTypeIdx];

				if (currentFrame.idx >= currentStructTypeInfo.structInfo.members.size)
				{
					// Pop struct frame
					--structStack.size;
					if (structStack.size == 0)
						LogError(context, rightHand->any.loc, "Too many values in struct literal"_s);
					continue;
				}

				s64 currentMemberTypeIdx =
					currentStructTypeInfo.structInfo.members[currentFrame.idx].typeTableIdx;
				TypeInfo currentMemberTypeInfo = context->typeTable[currentMemberTypeIdx];

				if (currentMemberTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
					currentMemberTypeInfo.typeCategory == TYPECATEGORY_UNION)
				{
					// Push struct frame
					structStack[structStack.size++] = { currentMemberTypeIdx, 0 };
					continue;
				}

				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];
				if (literalMemberExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
				{
					// Named member assignments handled in next loop
					break;
				}
				else
				{
					TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
							currentMemberTypeIdx, literalMemberExp);
					literalMemberExp->typeTableIdx = typeCheckResult.rightTableIdx;
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						Print("Type of struct literal value in position %d and "
								"type of struct member number %d don't match\n", memberIdx, memberIdx);
						ReportTypeCheckError(context, typeCheckResult.errorCode, rightHand->any.loc,
								currentMemberTypeIdx, literalMemberExp->typeTableIdx);
					}
					++structStack[structStack.size - 1].idx;
				}
				++memberIdx;
			}

			for (; memberIdx < rightHand->literal.members.size; ++memberIdx)
			{
				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];
				if (literalMemberExp->nodeType != ASTNODETYPE_BINARY_OPERATION ||
					literalMemberExp->binaryOperation.op != TOKEN_OP_ASSIGNMENT)
				{
					LogError(context, literalMemberExp->any.loc, "Non-named member found after "
							"named members in group literal"_s);
				}

				ASTExpression *leftExp  = literalMemberExp->binaryOperation.leftHand;
				ASTExpression *rightExp = literalMemberExp->binaryOperation.rightHand;

				ASSERT(leftExp->nodeType == ASTNODETYPE_IDENTIFIER); // We check this earlier.
				String memberName = leftExp->identifier.string;

				const StructMember *member = FindStructMemberByName(context, structTypeInfo, memberName);
				leftExp->identifier.structMember = member;

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
						member->typeTableIdx, rightExp);
				rightExp->typeTableIdx = typeCheckResult.rightTableIdx;
				if (typeCheckResult.errorCode != TYPECHECK_COOL)
				{
					Print("Type of struct literal value \"%S\" and "
							"type of struct member don't match\n", memberName);
					ReportTypeCheckError(context, typeCheckResult.errorCode, rightHand->any.loc,
							member->typeTableIdx, rightExp->typeTableIdx);
				}
			}

			result.rightTableIdx = structTypeIdx;
		}
		else if (structTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
		{
			if (structTypeInfo.arrayInfo.count < rightHand->literal.members.size)
				LogError(context, rightHand->any.loc, "Too many values in array literal"_s);

			for (int memberIdx = 0; memberIdx < rightHand->literal.members.size; ++memberIdx)
			{
				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];

				if (literalMemberExp->nodeType == ASTNODETYPE_IDENTIFIER &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
					LogError(context, literalMemberExp->any.loc, "Named members not allowed in array literals"_s);

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
						structTypeInfo.arrayInfo.elementTypeTableIdx,
						literalMemberExp);
				literalMemberExp->typeTableIdx = typeCheckResult.rightTableIdx;
				if (typeCheckResult.errorCode != TYPECHECK_COOL)
				{
					Print("Type of element %d in array literal doesn't match with type of array", memberIdx);
					ReportTypeCheckError(context, typeCheckResult.errorCode, rightHand->any.loc,
							structTypeInfo.arrayInfo.elementTypeTableIdx,
							rightHand->literal.members[memberIdx]->typeTableIdx);
				}
			}

			result.rightTableIdx = structTypeIdx;
		}
		else
			ASSERT(false);

		return result;
	}
	if (rightTableIdx == TYPETABLEIDX_ANYTHING)
	{
		if (leftTableIdx == TYPETABLEIDX_ANYTHING)
			result.errorCode = TYPECHECK_CANT_DEDUCE_TYPE;
		result.rightTableIdx = leftTableIdx;
		return result;
	}
	if (leftTableIdx == TYPETABLEIDX_ANYTHING)
	{
		result.leftTableIdx = rightTableIdx;
		return result;
	}

	TypeCategory leftTypeCat  = context->typeTable[leftTableIdx].typeCategory;
	TypeCategory rightTypeCat = context->typeTable[rightTableIdx].typeCategory;

	if (leftTableIdx == TYPETABLEIDX_INTEGER && (rightTypeCat == TYPECATEGORY_INTEGER ||
		rightTypeCat == TYPECATEGORY_POINTER || rightTypeCat == TYPECATEGORY_FLOATING))
	{
		result.leftTableIdx = rightTableIdx;
		return result;
	}
	if (rightTableIdx == TYPETABLEIDX_INTEGER && (leftTypeCat == TYPECATEGORY_INTEGER ||
				leftTypeCat == TYPECATEGORY_POINTER || leftTypeCat == TYPECATEGORY_FLOATING))
	{
		result.rightTableIdx = leftTableIdx;
		return result;
	}

	if (leftTableIdx == TYPETABLEIDX_FLOATING && rightTypeCat == TYPECATEGORY_FLOATING)
	{
		result.leftTableIdx = rightTableIdx;
		return result;
	}
	if (rightTableIdx == TYPETABLEIDX_FLOATING && leftTypeCat == TYPECATEGORY_FLOATING)
	{
		result.rightTableIdx = leftTableIdx;
		return result;
	}

	result.errorCode = CheckTypesMatch(context, leftTableIdx, rightTableIdx);
	return result;
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

TypeCheckTypeResult TypeCheckType(Context *context, String name, SourceLocation loc, ASTType *astType);

TypeCheckStructResult TypeCheckStructDeclaration(Context *context, String name, ASTStructDeclaration astStructDecl)
{
	for (int memberIdx = 0; memberIdx < astStructDecl.members.size; ++memberIdx)
	{
		ASTStructMemberDeclaration *astMember = &astStructDecl.members[memberIdx];

		if (astMember->astType == nullptr)
			LogError(context, astMember->loc, TPrintF("Type missing in declaration of struct "
					"member \"%S\"", astMember->name));

		if (astMember->value != nullptr)
			LogWarning(context, astMember->value->any.loc, TPrintF("Default value found on member "
						"\"%S\". This is not yet supported", astMember->name));

		TypeCheckTypeResult checkResult = TypeCheckType(context, {}, astMember->loc, astMember->astType);
		if (!checkResult.success)
			return { false, checkResult.yieldInfo };
		else
			astMember->typeTableIdx = checkResult.typeTableIdx;
	}

	for (int overloadIdx = 0; overloadIdx < astStructDecl.overloads.size; ++overloadIdx)
	{
		ASTOperatorOverload *overload = &astStructDecl.overloads[overloadIdx];

		TCScopeName *scopeName = FindScopeName(context, overload->name);
		if (scopeName == nullptr)
		{
			TCYieldInfo yieldInfo = {};
			yieldInfo.cause = TCYIELDCAUSE_MISSING_NAME;
			yieldInfo.name = overload->name;
			yieldInfo.loc = overload->loc;
			return { false, yieldInfo };
		}

		if (scopeName->type == NAMETYPE_STATIC_DEFINITION &&
			scopeName->staticDefinition->definitionType == STATICDEFINITIONTYPE_PROCEDURE)
			overload->procedureIdx = scopeName->staticDefinition->procedureIdx;
		else
			LogError(context, overload->loc, "Identifier in operator overload is not a procedure"_s);
	}

	TypeInfo t = {};
	t.typeCategory = astStructDecl.isUnion ? TYPECATEGORY_UNION : TYPECATEGORY_STRUCT;
	t.structInfo.name = name;

	DynamicArray<StructMember, FrameAllocator> structMembers;
	DynamicArrayInit(&structMembers, 16);

	int largestAlignment = 0;
	for (int memberIdx = 0; memberIdx < astStructDecl.members.size; ++memberIdx)
	{
		ASTStructMemberDeclaration astMember = astStructDecl.members[memberIdx];

		StructMember member = {};
		member.name = astMember.name;
		member.isUsing = astMember.isUsing;
		member.typeTableIdx = astMember.typeTableIdx;

		u64 memberSize = context->typeTable[member.typeTableIdx].size;
		int alignment = 8;
		if (memberSize < 8)
			alignment = NextPowerOf2((int)memberSize);

		if (alignment > largestAlignment)
			largestAlignment = alignment;

		if (!astStructDecl.isUnion)
		{
			if (t.size & (alignment - 1))
				t.size = (t.size & ~(alignment - 1)) + alignment;
			member.offset = t.size;
			t.size += memberSize;
		}
		else
		{
			member.offset = 0;
			if (t.size < memberSize)
				t.size = memberSize;
		}
		*DynamicArrayAdd(&structMembers) = member;
	}
	if (t.size & (largestAlignment - 1))
		t.size = (t.size & ~(largestAlignment - 1)) + largestAlignment;

	t.structInfo.members.data = structMembers.data;
	t.structInfo.members.size = structMembers.size;
#if DEBUG_BUILD
	t.structInfo.members._capacity = structMembers.capacity;
#endif

	if (astStructDecl.overloads.size > 0)
	{
		ArrayInit(&t.structInfo.overloads, astStructDecl.overloads.size);
		for (int overloadIdx = 0; overloadIdx < astStructDecl.overloads.size; ++overloadIdx)
		{
			ASTOperatorOverload astOverload = astStructDecl.overloads[overloadIdx];
			OperatorOverload overload;
			overload.op = astOverload.op;
			overload.procedureIdx = astOverload.procedureIdx;
			*ArrayAdd(&t.structInfo.overloads) = overload;
		}
	}

	s64 typeTableIdx;
	if (StringEquals(name, "String"_s))
	{
		typeTableIdx = TYPETABLEIDX_STRING_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_STRING_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_STRING_STRUCT] = t;
	}
	else if (StringEquals(name, "Array"_s))
	{
		typeTableIdx = TYPETABLEIDX_ARRAY_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_ARRAY_STRUCT] = t;
	}
	else if (StringEquals(name, "Any"_s))
	{
		typeTableIdx = TYPETABLEIDX_ANY_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_ANY_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_ANY_STRUCT] = t;
	}
	else if (StringEquals(name, "TypeInfo"_s))
	{
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT] = t;
	}
	else if (StringEquals(name, "TypeInfoInteger"_s))
	{
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT] = t;
	}
	else if (StringEquals(name, "TypeInfoStructMember"_s))
	{
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT] = t;
	}
	else if (StringEquals(name, "TypeInfoStruct"_s))
	{
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT] = t;
	}
	else if (StringEquals(name, "TypeInfoEnum"_s))
	{
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT] = t;
	}
	else if (StringEquals(name, "TypeInfoPointer"_s))
	{
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT] = t;
	}
	else if (StringEquals(name, "TypeInfoArray"_s))
	{
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT;
		t.valueIdx = context->typeTable[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT].valueIdx;
		(TypeInfo&)context->typeTable[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT] = t;
	}
	else
	{
		typeTableIdx = BucketArrayCount(&context->typeTable);
		AddType(context, t);
	}

	TCScope *stackTop = GetTopMostScope(context);
	*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;
	return { true, {}, typeTableIdx };
}

Constant TryEvaluateConstant(Context *context, ASTExpression *expression)
{
	Constant result;
	result.type = CONSTANTTYPE_INVALID;
	result.valueAsInt = 0xFA11FA11FA11FA11;
	result.typeTableIdx = expression->typeTableIdx;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_INTEGER:
		case LITERALTYPE_CHARACTER:
			result.type = CONSTANTTYPE_INTEGER;
			result.valueAsInt = expression->literal.integer;
			break;
		case LITERALTYPE_FLOATING:
			result.type = CONSTANTTYPE_FLOATING;
			result.valueAsFloat = expression->literal.floating;
			break;
		case LITERALTYPE_GROUP:
		{
			Array<Constant, FrameAllocator> constants;
			u64 membersCount = expression->literal.members.size;
			ArrayInit(&constants, membersCount);
			constants.size = membersCount;
			for (int i = 0; i < membersCount; ++i)
			{
				constants[i] = TryEvaluateConstant(context, expression->literal.members[i]);
				if (constants[i].type == CONSTANTTYPE_INVALID)
					goto error;
			}
			result.type = CONSTANTTYPE_GROUP;
			result.valueAsGroup = constants;
		} break;
		default:
			goto error;
		}
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		if (expression->identifier.type == NAMETYPE_STATIC_DEFINITION)
		{
			result.type = CONSTANTTYPE_INTEGER;
			result = expression->identifier.staticDefinition->constant;
		}
		else
			goto error;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTExpression *in = expression->unaryOperation.expression;
		Constant inValue  = TryEvaluateConstant(context, in);
		if (inValue.type == CONSTANTTYPE_INVALID)
			goto error;

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
		Constant leftValue  = TryEvaluateConstant(context, leftHand);
		Constant rightValue = TryEvaluateConstant(context, rightHand);
		if (leftValue.type == CONSTANTTYPE_INVALID || rightValue.type == CONSTANTTYPE_INVALID)
			goto error;

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
error:
	return result;
}

TypeCheckExpressionResult TryTypeCheckExpression(Context *context, ASTExpression *expression);
TypeCheckProcedurePrototypeResult TypeCheckProcedurePrototype(Context *context, ASTProcedurePrototype *prototype);
TypeInfo TypeInfoFromASTProcedurePrototype(Context *context, ASTProcedurePrototype prototype);
TypeCheckTypeResult TypeCheckType(Context *context, String name, SourceLocation loc, ASTType *astType)
{
	switch (astType->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
	{
		FindTypeResult result = FindTypeInStackByName(context, astType->loc, astType->name);
		if (!result.success)
			return { false, result.yieldInfo };

		return { true, {}, result.typeTableIdx };
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		TypeCheckTypeResult result = TypeCheckType(context, {}, loc, astType->arrayType);
		if (!result.success)
			return { false, result.yieldInfo };

		s64 elementTypeIdx = result.typeTableIdx;
		return { true, {}, GetTypeInfoArrayOf(context, elementTypeIdx, astType->arrayCount) };
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		TypeCheckTypeResult result = TypeCheckType(context, {}, loc, astType->pointedType);
		if (!result.success)
			return { false, result.yieldInfo };

		s64 pointedTypeIdx = result.typeTableIdx;
		return { true, {}, GetTypeInfoPointerOf(context, pointedTypeIdx) };
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	{
		TypeCheckStructResult result =
			TypeCheckStructDeclaration(context, name, astType->structDeclaration);
		if (!result.success)
			return { false, result.yieldInfo };

		return { true, {}, result.typeTableIdx };
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		// Yieldy things first
		s64 innerTypeIdx = TYPETABLEIDX_S64;
		if (astType->enumDeclaration.astType)
		{
			SourceLocation astTypeLoc = astType->enumDeclaration.astType->loc;
			TypeCheckTypeResult result = TypeCheckType(context, {}, astTypeLoc, astType->enumDeclaration.astType);
			if (!result.success)
				return { false, result.yieldInfo };

			if (innerTypeIdx < TYPETABLEIDX_PRIMITIVE_BEGIN ||
				innerTypeIdx > TYPETABLEIDX_PRIMITIVE_END)
				LogError(context, astTypeLoc, "Only primitive types are allowed as enum field types"_s);

			innerTypeIdx = result.typeTableIdx;
		}

		for (int memberIdx = 0; memberIdx < astType->enumDeclaration.members.size; ++memberIdx)
		{
			ASTExpression *memberValue = astType->enumDeclaration.members[memberIdx].value;
			if (memberValue)
			{
				TypeCheckExpressionResult result = TryTypeCheckExpression(context, memberValue);
				if (!result.success)
					return { false, result.yieldInfo };
			}
		}
		// End yieldy things

		TypeInfo t;
		t.typeCategory = TYPECATEGORY_ENUM;
		t.enumInfo.name = name;
		t.enumInfo.typeTableIdx = innerTypeIdx;
		t.size = context->typeTable[innerTypeIdx].size;

		DynamicArray<String, FrameAllocator> enumNames;
		DynamicArray<s64, FrameAllocator> enumValues;
		DynamicArrayInit(&enumNames, 16);
		DynamicArrayInit(&enumValues, 16);

		s64 typeTableIdx = BucketArrayCount(&context->typeTable);

		TCScope *stackTop = GetTopMostScope(context);

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
				Constant constant = TryEvaluateConstant(context, astMember.value);
				if (constant.type == CONSTANTTYPE_INVALID)
					LogError(context, astMember.value->any.loc,
							"Failed to evaluate constant in static definition"_s);
				else if (constant.type == CONSTANTTYPE_FLOATING)
					currentValue = (s64)constant.valueAsFloat;
				else
					currentValue = constant.valueAsInt;
			}
			staticDefinition.constant.type = CONSTANTTYPE_INTEGER;
			staticDefinition.constant.valueAsInt = currentValue;

			StaticDefinition *newStaticDef = BucketArrayAdd(&context->staticDefinitions);
			*newStaticDef = staticDefinition;

			TCScopeName newName;
			newName.type = NAMETYPE_STATIC_DEFINITION;
			newName.name = astMember.name;
			newName.staticDefinition = newStaticDef;
			*DynamicArrayAdd(&stackTop->names) = newName;

			*DynamicArrayAdd(&enumNames) = astMember.name;
			*DynamicArrayAdd(&enumValues) = currentValue;
			++currentValue;
		}

		t.enumInfo.names.data = enumNames.data;
		t.enumInfo.names.size = enumNames.size;
		t.enumInfo.values.data = enumValues.data;
		t.enumInfo.values.size = enumValues.size;
#if DEBUG_BUILD
		t.enumInfo.names._capacity  = enumNames.capacity;
		t.enumInfo.values._capacity = enumValues.capacity;
#endif

		AddType(context, t);
		*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;

		return { true, {}, typeTableIdx };
	} break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		TypeCheckProcedurePrototypeResult result = TypeCheckProcedurePrototype(context,
				&astType->procedurePrototype);
		if (!result.success)
			return { false, result.yieldInfo };

		TypeInfo t = TypeInfoFromASTProcedurePrototype(context, astType->procedurePrototype);
		s64 typeTableIdx = FindOrAddTypeTableIdx(context, t);

		return { true, {}, typeTableIdx };
	} break;
	default:
		ASSERT(false);
	}
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

void AddStructMembersToScope(Context *context, SourceLocation loc, ASTExpression *valueExpression)
{
	// This procedure spills all members of a struct expression into the current stack.
	// The way we do this is, we add a TCScopeName which has a member access AST node, left hand of
	// which is an arbitrary AST tree branch that evaluates to a struct; and of which right hand is
	// a simple struct member node.
	TypeInfo typeInfo = context->typeTable[valueExpression->typeTableIdx];
	ASSERT(typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		   typeInfo.typeCategory == TYPECATEGORY_UNION);

	TCScope *stackTop = GetTopMostScope(context);
	for (int memberIdx = 0; memberIdx < typeInfo.structInfo.members.size; ++memberIdx)
	{
		const StructMember *member = &typeInfo.structInfo.members[memberIdx];

		ASTExpression *memberAccessExp = NewTreeNode(context);
		{
			ASTExpression *rightHand = NewTreeNode(context);
			ASTExpression r = {};
			r.nodeType = ASTNODETYPE_IDENTIFIER;
			r.identifier.type = NAMETYPE_STRUCT_MEMBER;
			r.identifier.structMember = member;
			*rightHand = r;

			ASTExpression e = {};
			e.typeTableIdx = member->typeTableIdx;
			e.nodeType = ASTNODETYPE_BINARY_OPERATION;
			e.binaryOperation.op = TOKEN_OP_MEMBER_ACCESS;
			e.binaryOperation.leftHand = valueExpression;
			e.binaryOperation.rightHand = rightHand;
			*memberAccessExp = e;
		}

		if (member->name.size && !member->isUsing)
		{
			// Check if name already exists
			for (s64 i = 0; i < (s64)stackTop->names.size; ++i)
			{
				TCScopeName currentName = stackTop->names[i];
				if (StringEquals(member->name, currentName.name))
					LogError(context, loc, TPrintF("Failed to pull \"%S\" into scope because "
								"the name is already used", member->name));
			}

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_ASTEXPRESSION;
			newScopeName.name = member->name;
			newScopeName.expression = memberAccessExp;
			newScopeName.loc = loc;
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}
		else
		{
			// For using/anonymous members we recurse, spilling it's members too.
			AddStructMembersToScope(context, loc, memberAccessExp);
		}
	}
}

TypeCheckVariableResult TypeCheckVariableDeclaration(Context *context, ASTVariableDeclaration *varDecl)
{
	if (varDecl->name.size)
	{
		// Check if name already exists
		TCScope *stackTop = GetTopMostScope(context);
		for (int i = 0; i < stackTop->names.size; ++i)
		{
			TCScopeName *currentName = &stackTop->names[i];
			if (StringEquals(varDecl->name, currentName->name))
			{
				LogErrorNoCrash(context, varDecl->loc, TPrintF("Duplicate name \"%S\" in scope", varDecl->name));
				LogNote(context, currentName->loc, "First defined here"_s);
				CRASH;
			}
		}
	}

	if (varDecl->astType)
	{
		TypeCheckTypeResult result = TypeCheckType(context, {}, varDecl->loc, varDecl->astType);
		if (!result.success)
			return { false, result.yieldInfo };

		varDecl->typeTableIdx = result.typeTableIdx;

		if (varDecl->typeTableIdx == TYPETABLEIDX_VOID)
			LogError(context, varDecl->loc, "Variable can't be of type void!"_s);
	}

	if (varDecl->astInitialValue)
	{
		TypeCheckExpressionResult result = TryTypeCheckExpression(context, varDecl->astInitialValue);
		if (!result.success)
			return { false, result.yieldInfo };

		if (varDecl->astType)
		{
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					varDecl->typeTableIdx, varDecl->astInitialValue);
			if (typeCheckResult.errorCode != TYPECHECK_COOL)
			{
				Print("Variable declaration type and initial type don't match\n");
				ReportTypeCheckError(context, typeCheckResult.errorCode, varDecl->loc,
						varDecl->typeTableIdx, varDecl->astInitialValue->typeTableIdx);
			}
			varDecl->typeTableIdx = typeCheckResult.leftTableIdx;
			varDecl->astInitialValue->typeTableIdx = typeCheckResult.rightTableIdx;
		}
		else
		{
			if (varDecl->astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(context, varDecl->loc, "Variable can't be of type void!"_s);

			varDecl->typeTableIdx = InferType(varDecl->astInitialValue->typeTableIdx);
		}
	}

	return { true, {} };
}

TypeCheckVariableResult TypeCheckProcedureParameter(Context *context, ASTProcedureParameter *astParam)
{
	if (astParam->name.size)
	{
		// Check if name already exists
		TCScope *stackTop = GetTopMostScope(context);
		for (int i = 0; i < stackTop->names.size; ++i)
		{
			TCScopeName *currentName = &stackTop->names[i];
			if (StringEquals(astParam->name, currentName->name))
			{
				LogErrorNoCrash(context, astParam->loc, TPrintF("Duplicate name \"%S\" in scope", astParam->name));
				LogNote(context, currentName->loc, "First defined here"_s);
				CRASH;
			}
		}
	}

	if (astParam->astType)
	{
		TypeCheckTypeResult result = TypeCheckType(context, {}, astParam->loc, astParam->astType);
		if (!result.success)
			return { false, result.yieldInfo };
		else
		{
			astParam->typeTableIdx = result.typeTableIdx;

			if (astParam->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(context, astParam->loc, "Variable can't be of type void!"_s);
		}
	}

	if (astParam->astInitialValue)
	{
		TypeCheckExpressionResult result = TryTypeCheckExpression(context, astParam->astInitialValue);
		if (!result.success)
			return { false, result.yieldInfo };

		if (astParam->astType)
		{
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					astParam->typeTableIdx, astParam->astInitialValue);
			if (typeCheckResult.errorCode != TYPECHECK_COOL)
			{
				Print("Variable declaration type and initial type don't match\n");
				ReportTypeCheckError(context, typeCheckResult.errorCode, astParam->loc,
						astParam->typeTableIdx, astParam->astInitialValue->typeTableIdx);
			}
			astParam->typeTableIdx = typeCheckResult.leftTableIdx;
			astParam->astInitialValue->typeTableIdx = typeCheckResult.rightTableIdx;
		}
		else
		{
			if (astParam->astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(context, astParam->loc, "Variable can't be of type void!"_s);

			astParam->typeTableIdx = InferType(astParam->astInitialValue->typeTableIdx);
		}
	}

	return { true, {} };
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

TypeCheckProcedurePrototypeResult TypeCheckProcedurePrototype(Context *context, ASTProcedurePrototype *prototype)
{
	// Parameters
	bool beginOptionalParameters = false;
	for (int i = 0; i < prototype->astParameters.size; ++i)
	{
		TypeCheckVariableResult result = TypeCheckProcedureParameter(context, &prototype->astParameters[i]);
		if (!result.success)
			return { false, result.yieldInfo };

		ASTProcedureParameter astParameter = prototype->astParameters[i];
		if (!astParameter.astInitialValue)
		{
			if (beginOptionalParameters)
				LogError(context, astParameter.loc, "Non-optional parameter after optional parameter found!"_s);
		}
		else
			beginOptionalParameters = true;
	}

	prototype->returnTypeIdx = TYPETABLEIDX_VOID;
	if (prototype->astReturnType)
	{
		TypeCheckTypeResult result = TypeCheckType(context, {}, prototype->loc, prototype->astReturnType);
		if (!result.success)
			return { false, result.yieldInfo };
		prototype->returnTypeIdx = result.typeTableIdx;
	}

	return { true };
}

TypeInfo TypeInfoFromASTProcedurePrototype(Context *context, ASTProcedurePrototype prototype)
{
	TypeInfo t = {};
	t.size = g_pointerSize;
	t.typeCategory = TYPECATEGORY_PROCEDURE;
	t.procedureInfo.isVarargs = prototype.isVarargs;
	t.procedureInfo.returnTypeTableIdx = prototype.returnTypeIdx;

	int astParametersCount = (int)prototype.astParameters.size;
	if (astParametersCount)
	{
		DynamicArray<ProcedureParameter, FrameAllocator> parameters;
		DynamicArrayInit(&parameters, astParametersCount);

		// Parameters
		for (int i = 0; i < astParametersCount; ++i)
		{
			ASTProcedureParameter astParameter = prototype.astParameters[i];

			ProcedureParameter procParam = {};
			procParam.typeTableIdx = astParameter.typeTableIdx;
			if (astParameter.astInitialValue)
			{
				procParam.defaultValue = TryEvaluateConstant(context, astParameter.astInitialValue);
				if (procParam.defaultValue.type == CONSTANTTYPE_INVALID)
					LogError(context, astParameter.astInitialValue->any.loc,
							"Failed to evaluate constant in default parameter"_s);
			}
			*DynamicArrayAdd(&parameters) = procParam;
		}

		t.procedureInfo.parameters.data = parameters.data;
		t.procedureInfo.parameters.size = parameters.size;
#if DEBUG_BUILD
		t.procedureInfo.parameters._capacity = parameters.capacity;
#endif
	}

	return t;
}

ASTExpression InlineProcedureCopyTreeBranch(Context *context, const ASTExpression *expression)
{
	ASTExpression result;
	result.nodeType = expression->nodeType;
	result.any.loc = expression->any.loc;
	result.typeTableIdx = expression->typeTableIdx;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_BLOCK:
	{
		ASTBlock astBlock = {};
		DynamicArrayInit(&astBlock.statements, expression->block.statements.size);

		PushTCScope(context);

		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			ASTExpression statement = InlineProcedureCopyTreeBranch(context,
					&expression->block.statements[i]);
			if (statement.nodeType != ASTNODETYPE_INVALID)
				*DynamicArrayAdd(&astBlock.statements) = statement;
		}

		PopTCScope(context);

		result.block = astBlock;
		return result;
	}
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		TCValue tcValue = { TCVALUETYPE_VALUE };
		tcValue.valueIdx = NewValue(context, varDecl.name, varDecl.typeTableIdx,
				context->values[varDecl.valueIdx].flags);

		varDecl.valueIdx = tcValue.valueIdx;

		if (varDecl.name.size)
		{
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = varDecl.name;
			newScopeName.variableInfo.tcValue = tcValue;
			newScopeName.variableInfo.typeTableIdx = varDecl.typeTableIdx;
			newScopeName.loc = varDecl.loc;

			TCScope *stackTop = GetTopMostScope(context);
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}

		if (varDecl.name.size == 0)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = varDecl.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.string;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.tcValue = tcValue;
				*varExp = e;
			}
			AddStructMembersToScope(context, varDecl.loc, varExp);
		}

		if (varDecl.astInitialValue)
		{
			ASTExpression *astInitialValue = NewTreeNode(context);
			*astInitialValue = InlineProcedureCopyTreeBranch(context, varDecl.astInitialValue);
			varDecl.astInitialValue = astInitialValue;
		}

		result.variableDeclaration = varDecl;
		return result;
	}
	case ASTNODETYPE_IDENTIFIER:
	{
		ASTIdentifier astIdentifier = expression->identifier;

		TCScopeName *scopeName = FindScopeName(context, astIdentifier.string);
		ASSERT(scopeName);

		astIdentifier.type = scopeName->type;
		switch (scopeName->type)
		{
		case NAMETYPE_VARIABLE:
		{
			TCValue tcValue = scopeName->variableInfo.tcValue;
			astIdentifier.tcValue = scopeName->variableInfo.tcValue;
		} break;
		case NAMETYPE_STRUCT_MEMBER:
		{
			astIdentifier.structMember = scopeName->structMember;
		} break;
		case NAMETYPE_ASTEXPRESSION:
		{
			astIdentifier.expression = scopeName->expression;
		} break;
		case NAMETYPE_STATIC_DEFINITION:
		{
			astIdentifier.staticDefinition = scopeName->staticDefinition;
		} break;
		default:
			ASSERT(false);
		}

		result.identifier = astIdentifier;
		return result;
	}
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition astStaticDef = expression->staticDefinition;

		// Add scope name
		TCScope *stackTop = GetTopMostScope(context);
		TCScopeName staticDefScopeName;
		staticDefScopeName.type = NAMETYPE_STATIC_DEFINITION;
		staticDefScopeName.name = astStaticDef.name;
		staticDefScopeName.staticDefinition = expression->staticDefinition.staticDef;
		staticDefScopeName.loc = astStaticDef.loc;
		*DynamicArrayAdd(&stackTop->names) = staticDefScopeName;

		return {};
	}
	case ASTNODETYPE_RETURN:
	{
		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context,
				expression->returnNode.expression);
		result.returnNode.expression = e;
		return result;
	}
	case ASTNODETYPE_DEFER:
	{
		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context,
				expression->deferNode.expression);
		result.deferNode.expression = e;
		return result;
	}
	case ASTNODETYPE_USING:
	{
		ASTExpression *usingExp = NewTreeNode(context);
		*usingExp = InlineProcedureCopyTreeBranch(context,
				expression->usingNode.expression);

		if (usingExp->nodeType == ASTNODETYPE_VARIABLE_DECLARATION)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = usingExp->variableDeclaration.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.string;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.tcValue = { TCVALUETYPE_VALUE, usingExp->variableDeclaration.valueIdx };
				*varExp = e;
			}
			AddStructMembersToScope(context, usingExp->any.loc, varExp);
		}
		else
			AddStructMembersToScope(context, usingExp->any.loc, usingExp);

		result.usingNode.expression = usingExp;
		return result;
	}
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall original = expression->procedureCall;

		ASTProcedureCall astProcCall = original;
		DynamicArrayInit(&astProcCall.arguments, original.arguments.size);
		for (int argIdx = 0; argIdx < original.arguments.size; ++argIdx)
		{
			*DynamicArrayAdd(&astProcCall.arguments) =
				InlineProcedureCopyTreeBranch(context, &original.arguments[argIdx]);
		}

		if (astProcCall.callType == CALLTYPE_VALUE)
		{
			TCScopeName *scopeName = FindScopeName(context, astProcCall.name);
			ASSERT(scopeName);
			ASSERT(scopeName->type == NAMETYPE_VARIABLE);
			astProcCall.tcValue = scopeName->variableInfo.tcValue;
		}
		else if (astProcCall.callType == CALLTYPE_ASTEXPRESSION)
		{
			TCScopeName *scopeName = FindScopeName(context, astProcCall.name);
			ASSERT(scopeName);
			ASSERT(scopeName->type == NAMETYPE_ASTEXPRESSION);
			astProcCall.expression = scopeName->expression;
		}

		result.procedureCall = astProcCall;
		return result;
	}
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTUnaryOperation astUnary = expression->unaryOperation;
		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, expression->unaryOperation.expression);
		astUnary.expression = e;
		result.unaryOperation = astUnary;
		return result;
	}
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTBinaryOperation astBinary = expression->binaryOperation;
		ASTExpression *l = NewTreeNode(context);
		ASTExpression *r = NewTreeNode(context);
		*l = InlineProcedureCopyTreeBranch(context, expression->binaryOperation.leftHand);

		// For member access we can just copy
		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
			*r = *expression->binaryOperation.rightHand;
		else
			*r = InlineProcedureCopyTreeBranch(context, expression->binaryOperation.rightHand);

		astBinary.leftHand = l;
		astBinary.rightHand = r;
		result.binaryOperation = astBinary;
		return result;
	}
	case ASTNODETYPE_LITERAL:
	{
		ASTLiteral astLiteral = expression->literal;

		if (expression->literal.type == LITERALTYPE_GROUP)
		{
			ArrayInit(&astLiteral.members, expression->literal.members.size);
			for (int memberIdx = 0; memberIdx < expression->literal.members.size; ++memberIdx)
			{
				ASTExpression *e = NewTreeNode(context);
				*e = InlineProcedureCopyTreeBranch(context, expression->literal.members[memberIdx]);
				*ArrayAdd(&astLiteral.members) = e;
			}
		}

		result.literal = astLiteral;
		return result;
	}
	case ASTNODETYPE_IF:
	{
		ASTIf astIf = expression->ifNode;

		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, expression->ifNode.condition);
		astIf.condition = e;

		e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, expression->ifNode.body);
		astIf.body = e;

		if (expression->ifNode.elseBody)
		{
			e = NewTreeNode(context);
			*e = InlineProcedureCopyTreeBranch(context, expression->ifNode.elseBody);
			astIf.elseBody = e;
		}

		result.ifNode = astIf;
		return result;
	}
	case ASTNODETYPE_WHILE:
	{
		ASTWhile astWhile = expression->whileNode;

		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, expression->whileNode.condition);
		astWhile.condition = e;

		e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, expression->whileNode.body);
		astWhile.body = e;

		result.whileNode = astWhile;
		return result;
	}
	case ASTNODETYPE_FOR:
	{
		ASTFor astFor = expression->forNode;

		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, expression->forNode.range);
		astFor.range = e;

		PushTCScope(context);

		String indexValueName = "i"_s;
		u32 indexValueIdx = NewValue(context, indexValueName, TYPETABLEIDX_S64, 0);
		astFor.indexValueIdx = indexValueIdx;

		TCScope *stackTop = GetTopMostScope(context);
		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = indexValueName;
		newScopeName.variableInfo.tcValue = { TCVALUETYPE_VALUE, indexValueIdx };
		newScopeName.variableInfo.typeTableIdx = TYPETABLEIDX_S64;
		newScopeName.loc = expression->any.loc;
		*DynamicArrayAdd(&stackTop->names) = newScopeName;

		ASTExpression *rangeExp = expression->forNode.range;
		bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			rangeExp->binaryOperation.op == TOKEN_OP_RANGE;
		if (!isExplicitRange)
		{
			s64 origValueTypeIdx = context->values[expression->forNode.elementValueIdx].typeTableIdx;
			String elementValueName = "it"_s;
			u32 elementValueIdx = NewValue(context, elementValueName, origValueTypeIdx, 0);
			astFor.elementValueIdx = elementValueIdx;

			newScopeName.name = elementValueName;
			newScopeName.variableInfo.tcValue = { TCVALUETYPE_VALUE, elementValueIdx };
			newScopeName.variableInfo.typeTableIdx = origValueTypeIdx;
			newScopeName.loc = expression->any.loc;
			*DynamicArrayAdd(&stackTop->names) = newScopeName;
		}

		e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, expression->forNode.body);
		astFor.body = e;

		PopTCScope(context);

		result.forNode = astFor;
		return result;
	}
	case ASTNODETYPE_BREAK:
	case ASTNODETYPE_CONTINUE:
	case ASTNODETYPE_TYPE:
	case ASTNODETYPE_TYPEOF:
	case ASTNODETYPE_SIZEOF:
	{
		return *expression;
	}
	case ASTNODETYPE_CAST:
	{
		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context,
				expression->castNode.expression);
		result.castNode.expression = e;
		return result;
	}
	case ASTNODETYPE_INTRINSIC:
	{
		ASTIntrinsic astIntrinsic = expression->intrinsic;
		DynamicArrayInit(&astIntrinsic.arguments, expression->intrinsic.arguments.size);
		for (int argIdx = 0; argIdx < expression->intrinsic.arguments.size; ++argIdx)
		{
			*DynamicArrayAdd(&astIntrinsic.arguments) = InlineProcedureCopyTreeBranch(context,
					&expression->intrinsic.arguments[argIdx]);
		}
		result.intrinsic = astIntrinsic;
		return result;
	}
	default:
		ASSERT(false);
	}
}

void TCAddParametersToScope(Context *context, ASTProcedurePrototype prototype)
{
	TCScope *stackTop = GetTopMostScope(context);

	for (int i = 0; i < prototype.astParameters.size; ++i)
	{
		ASTProcedureParameter astParameter = prototype.astParameters[i];
		TCValue tcParamValue = { TCVALUETYPE_PARAMETER, (u32)i };

		if (astParameter.isUsing)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = astParameter.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.tcValue = tcParamValue;
				*varExp = e;
			}
			AddStructMembersToScope(context, astParameter.loc, varExp);
		}

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = astParameter.name;
		newScopeName.variableInfo.tcValue = tcParamValue;
		newScopeName.variableInfo.typeTableIdx = astParameter.typeTableIdx;
		newScopeName.loc = astParameter.loc;
		*DynamicArrayAdd(&stackTop->names) = newScopeName;
	}

	// Varargs array
	if (prototype.isVarargs)
	{
		static s64 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

		TCValue tcParamValue;
		tcParamValue.type = TCVALUETYPE_PARAMETER;
		tcParamValue.parameterIdx = (u32)prototype.astParameters.size;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = prototype.varargsName;
		newScopeName.variableInfo.tcValue = tcParamValue;
		newScopeName.variableInfo.typeTableIdx = arrayTableIdx;
		newScopeName.loc = prototype.varargsLoc;
		*DynamicArrayAdd(&stackTop->names) = newScopeName;
	}
}

String OperatorToString(s32 op);
TypeCheckExpressionResult TryTypeCheckExpression(Context *context, ASTExpression *expression)
{
	ASSERT(expression->typeTableIdx == TYPETABLEIDX_UNSET);

	switch (expression->nodeType)
	{
	case ASTNODETYPE_GARBAGE:
	{
		expression->typeTableIdx = TYPETABLEIDX_ANYTHING;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		ASTBlock *astBlock = &expression->block;
		if (!astBlock->scopePushed)
		{
			PushTCScope(context);
			astBlock->scopePushed = true;
		}

		while (astBlock->typeCheckingIdx < astBlock->statements.size)
		{
			TypeCheckExpressionResult result = TryTypeCheckExpression(context,
					&astBlock->statements[astBlock->typeCheckingIdx]);
			if (!result.success)
				return { false, result.yieldInfo };
			else
			{
				++astBlock->typeCheckingIdx;
				if (astBlock->typeCheckingIdx >= astBlock->statements.size)
					break;
			}
		}

		PopTCScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration *varDecl = &expression->variableDeclaration;
		TypeCheckVariableResult result = TypeCheckVariableDeclaration(context, varDecl);
		if (!result.success)
			return { false, result.yieldInfo };
		expression->typeTableIdx = varDecl->typeTableIdx;

		u32 staticFlag   = varDecl->isStatic   ? VALUEFLAGS_ON_STATIC_STORAGE : 0;
		u32 externalFlag = varDecl->isExternal ? VALUEFLAGS_IS_EXTERNAL       : 0;
		TCValue tcValue = { TCVALUETYPE_VALUE };
		tcValue.valueIdx = NewValue(context, varDecl->name, varDecl->typeTableIdx,
				staticFlag | externalFlag);

		varDecl->valueIdx = tcValue.valueIdx;

		if (!varDecl->addedScopeName)
		{
			if (varDecl->name.size)
			{
				TCScopeName newScopeName;
				newScopeName.type = NAMETYPE_VARIABLE;
				newScopeName.name = varDecl->name;
				newScopeName.variableInfo.tcValue = tcValue;
				newScopeName.variableInfo.typeTableIdx = varDecl->typeTableIdx;
				newScopeName.loc = varDecl->loc;

				TCScope *stackTop = GetTopMostScope(context);
				*DynamicArrayAdd(&stackTop->names) = newScopeName;
			}
			varDecl->addedScopeName = true;
		}

		if (varDecl->name.size == 0)
		{
			TypeCategory typeCat = context->typeTable[expression->typeTableIdx].typeCategory;
			if (typeCat != TYPECATEGORY_STRUCT && typeCat != TYPECATEGORY_UNION)
				LogError(context, expression->any.loc, "Anonymous variable has to be a struct/union!"_s);

			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = varDecl->typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.string;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.tcValue = tcValue;
				*varExp = e;
			}
			AddStructMembersToScope(context, varDecl->loc, varExp);
		}
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition *astStaticDef = &expression->staticDefinition;

		if (astStaticDef->staticDef == nullptr)
		{
			StaticDefinition *newStaticDef;

			// Check if already exists
			TCScopeName *scopeName = FindScopeName(context, astStaticDef->name);
			if (scopeName != nullptr)
			{
				LogErrorNoCrash(context, expression->any.loc,
						TPrintF("Duplicate static definition \"%S\"", astStaticDef->name));
				LogNote(context, scopeName->loc, "First defined here"_s);
				CRASH;
			}

			newStaticDef = BucketArrayAdd(&context->staticDefinitions);
			*newStaticDef = {};
			newStaticDef->typeTableIdx = TYPETABLEIDX_UNSET;
			newStaticDef->name = astStaticDef->name;

			// Add scope name
			TCScope *stackTop = GetTopMostScope(context);
			TCScopeName staticDefScopeName;
			staticDefScopeName.type = NAMETYPE_STATIC_DEFINITION;
			staticDefScopeName.name = astStaticDef->name;
			staticDefScopeName.staticDefinition = newStaticDef;
			staticDefScopeName.loc = astStaticDef->loc;
			*DynamicArrayAdd(&stackTop->names) = staticDefScopeName;

			// Add all information we can, that doesn't depend on others
			if (astStaticDef->expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
			{
				newStaticDef->definitionType = STATICDEFINITIONTYPE_PROCEDURE;
				ASTProcedureDeclaration *procDecl = &astStaticDef->expression->procedureDeclaration;
				Procedure *procedure;
				if (procDecl->isExternal)
				{
					newStaticDef->procedureIdx = -(s32)BucketArrayCount(&context->externalProcedures);
					procedure = BucketArrayAdd(&context->externalProcedures);
				}
				else
				{
					newStaticDef->procedureIdx = (s32)BucketArrayCount(&context->procedures);
					procedure = BucketArrayAdd(&context->procedures);
				}
				procDecl->procedureIdx = newStaticDef->procedureIdx;
				Procedure p = {};
				p.typeTableIdx = TYPETABLEIDX_UNSET;
				p.returnValueIdx = U32_MAX;
				p.name = procDecl->name;
				p.isInline = procDecl->isInline;
				p.astProcedureDeclaration = procDecl;
				DynamicArrayInit(&p.parameterValues, 8);
				*procedure = p;

				PushTCScope(context);
			}
			else if (astStaticDef->expression->nodeType == ASTNODETYPE_TYPE)
				newStaticDef->definitionType = STATICDEFINITIONTYPE_TYPE;
			else
				newStaticDef->definitionType = STATICDEFINITIONTYPE_CONSTANT;

			astStaticDef->staticDef = newStaticDef;
		}

		StaticDefinition *staticDef = astStaticDef->staticDef;
		ASSERT(staticDef != nullptr);

		// This can yield
		if (astStaticDef->expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
		{
			ASTProcedureDeclaration *procDecl = &astStaticDef->expression->procedureDeclaration;
			procDecl->procedureIdx = staticDef->procedureIdx;
			Procedure *procedure = GetProcedure(context, staticDef->procedureIdx);

			if (!procDecl->checkedPrototype)
			{
				TypeCheckProcedurePrototypeResult result = TypeCheckProcedurePrototype(context,
						&procDecl->prototype);
				if (!result.success)
					return { false, result.yieldInfo };
				TypeInfo t = TypeInfoFromASTProcedurePrototype(context, procDecl->prototype);

				// Parameters
				for (int i = 0; i < procDecl->prototype.astParameters.size; ++i)
				{
					ASTProcedureParameter astParameter = procDecl->prototype.astParameters[i];
					TCValue tcParamValue = { TCVALUETYPE_PARAMETER, (u32)i };
					u32 paramValueIdx = NewValue(context, astParameter.name, astParameter.typeTableIdx, 0);
					*DynamicArrayAdd(&procedure->parameterValues) = paramValueIdx;
				}
				// Varargs array
				if (procDecl->prototype.isVarargs)
				{
					static s64 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

					u32 valueIdx = NewValue(context, procDecl->prototype.varargsName, arrayTableIdx, 0);
					*DynamicArrayAdd(&procedure->parameterValues) = valueIdx;
				}
				TCAddParametersToScope(context, procDecl->prototype);

				s64 returnType = procDecl->prototype.returnTypeIdx;
				t.procedureInfo.returnTypeTableIdx = returnType;
				procedure->returnValueIdx = NewValue(context, "_returnValue"_s, returnType, 0);

				s64 typeTableIdx = FindOrAddTypeTableIdx(context, t);
				// @Check: Why so many here D:
				procedure->typeTableIdx = typeTableIdx;
				staticDef->typeTableIdx = typeTableIdx;
				astStaticDef->expression->typeTableIdx = typeTableIdx;

				procDecl->checkedPrototype = true;
			}

			TypeInfo t = context->typeTable[procedure->typeTableIdx];

			ASTProcedureDeclaration *astProcDecl = procedure->astProcedureDeclaration;
			if (astProcDecl && astProcDecl->astBody)
			{
				u64 previousReturnType = context->tcCurrentReturnType;
				context->tcCurrentReturnType = t.procedureInfo.returnTypeTableIdx;

				TypeCheckExpressionResult result =
					TryTypeCheckExpression(context, astProcDecl->astBody);

				// Important to restore return type whether we yield or not
				context->tcCurrentReturnType = previousReturnType;

				if (!result.success)
					return { false, result.yieldInfo };
			}
			procedure->isBodyTypeChecked = true;

			expression->typeTableIdx = procedure->typeTableIdx;
			PopTCScope(context);

			// Check all paths return
			if (astProcDecl->astBody && t.procedureInfo.returnTypeTableIdx != TYPETABLEIDX_VOID)
			{
				ReturnCheckResult result = CheckIfReturnsValue(context, astProcDecl->astBody);
				if (result == RETURNCHECKRESULT_SOMETIMES)
					LogError(context, expression->any.loc, "Procedure doesn't always return a value"_s);
				else if (result == RETURNCHECKRESULT_NEVER)
					LogError(context, expression->any.loc, "Procedure has to return a value"_s);
			}
		}
		else if (astStaticDef->expression->nodeType == ASTNODETYPE_TYPE)
		{
			TypeCheckTypeResult result = TypeCheckType(context, astStaticDef->name, expression->any.loc,
					&astStaticDef->expression->astType);
			if (!result.success)
				return { false, result.yieldInfo };
			astStaticDef->expression->typeTableIdx = result.typeTableIdx;

			staticDef->definitionType = STATICDEFINITIONTYPE_TYPE;
			staticDef->typeTableIdx = astStaticDef->expression->typeTableIdx;
			expression->typeTableIdx = astStaticDef->expression->typeTableIdx;
		}
		else
		{
			TypeCheckExpressionResult result = TryTypeCheckExpression(context, astStaticDef->expression);
			if (!result.success)
				return { false, result.yieldInfo };

			if (astStaticDef->expression->nodeType == ASTNODETYPE_IDENTIFIER &&
				astStaticDef->expression->identifier.type == NAMETYPE_STATIC_DEFINITION)
			{
				staticDef->definitionType = astStaticDef->expression->identifier.staticDefinition->definitionType;
				ASSERT(astStaticDef->expression->typeTableIdx != TYPETABLEIDX_UNSET);
				*staticDef = *astStaticDef->expression->identifier.staticDefinition;
				expression->typeTableIdx = astStaticDef->expression->typeTableIdx;
			}
			else
			{
				staticDef->definitionType = STATICDEFINITIONTYPE_CONSTANT;
				s64 constantTypeIdx = astStaticDef->expression->typeTableIdx;
				ASSERT(constantTypeIdx != TYPETABLEIDX_UNSET);
				expression->typeTableIdx = constantTypeIdx;
				staticDef->typeTableIdx = constantTypeIdx;

				staticDef->constant = TryEvaluateConstant(context, astStaticDef->expression);
				if (staticDef->constant.type == CONSTANTTYPE_INVALID)
					LogError(context, astStaticDef->expression->any.loc,
							"Failed to evaluate constant in default parameter"_s);
			}
		}
	} break;
	case ASTNODETYPE_RETURN:
	{
		ASTExpression *returnExp = expression->returnNode.expression;
		TypeCheckErrorCode errorCode;
		if (returnExp != nullptr)
		{
			TypeCheckExpressionResult result = TryTypeCheckExpression(context, returnExp);
			if (!result.success)
				return { false, result.yieldInfo };
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					context->tcCurrentReturnType, returnExp);
			returnExp->typeTableIdx = typeCheckResult.rightTableIdx;
			errorCode = typeCheckResult.errorCode;
		}
		else
			errorCode = CheckTypesMatch(context, context->tcCurrentReturnType, TYPETABLEIDX_VOID);

		if (errorCode != TYPECHECK_COOL)
		{
			Print("Incorrect return type");
			ReportTypeCheckError(context, errorCode, expression->any.loc,
					returnExp ? returnExp->typeTableIdx : TYPETABLEIDX_VOID,
					context->tcCurrentReturnType);
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		return TryTypeCheckExpression(context, expression->deferNode.expression);
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		String string = expression->identifier.string;

		TCScopeName *scopeName = FindScopeName(context, string);
		if (scopeName == nullptr)
		{
#if PRINT_DEBUG_NOTES_UPON_YIELDING
			LogNote(context, expression->any.loc, TPrintF("Invalid variable \"%S\" referenced",
					string));
#endif
			TCYieldInfo yieldInfo = {};
			yieldInfo.cause = TCYIELDCAUSE_MISSING_NAME;
			yieldInfo.name = string;
			yieldInfo.loc = expression->any.loc;
			return { false, yieldInfo };
		}

		expression->identifier.type = scopeName->type;
		switch (scopeName->type)
		{
		case NAMETYPE_VARIABLE:
		{
			expression->identifier.tcValue = scopeName->variableInfo.tcValue;
			s64 variableTypeIdx = scopeName->variableInfo.typeTableIdx;
			if (variableTypeIdx == TYPETABLEIDX_UNSET)
			{
#if PRINT_DEBUG_NOTES_UPON_YIELDING
				LogNote(context, expression->any.loc, TPrintF("Variable \"%S\" not type checked",
						string));
#endif
				TCYieldInfo yieldInfo = {};
				yieldInfo.cause = TCYIELDCAUSE_NEED_TYPE_CHECKING;
				yieldInfo.name = string;
				yieldInfo.loc = expression->any.loc;
				return { false, yieldInfo };
			}
			expression->typeTableIdx = variableTypeIdx;
		} break;
		case NAMETYPE_STRUCT_MEMBER:
		{
			expression->identifier.structMember = scopeName->structMember;
			expression->typeTableIdx = scopeName->structMember->typeTableIdx;
		} break;
		case NAMETYPE_ASTEXPRESSION:
		{
			expression->identifier.expression = scopeName->expression;
			expression->typeTableIdx = scopeName->expression->typeTableIdx;
		} break;
		case NAMETYPE_STATIC_DEFINITION:
		{
			expression->identifier.staticDefinition = scopeName->staticDefinition;
			s64 staticDefTypeIdx = scopeName->staticDefinition->typeTableIdx;
			if (staticDefTypeIdx == TYPETABLEIDX_UNSET)
			{
#if PRINT_DEBUG_NOTES_UPON_YIELDING
				LogNote(context, expression->any.loc, TPrintF("Static definition \"%S\" not type "
						"checked", string));
#endif
				TCYieldInfo yieldInfo = {};
				yieldInfo.cause = TCYIELDCAUSE_NEED_TYPE_CHECKING;
				yieldInfo.name = string;
				yieldInfo.loc = expression->any.loc;
				return { false, yieldInfo };
			}
			expression->typeTableIdx = staticDefTypeIdx;
		} break;
		default:
			ASSERT(false);
		}
	} break;
	case ASTNODETYPE_USING:
	{
		ASTExpression *usingExp = expression->usingNode.expression;
		TypeCheckExpressionResult result = TryTypeCheckExpression(context, usingExp);
		if (!result.success)
			return { false, result.yieldInfo };

		if (usingExp->nodeType == ASTNODETYPE_VARIABLE_DECLARATION)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = usingExp->variableDeclaration.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.string;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.tcValue = { TCVALUETYPE_VALUE, usingExp->variableDeclaration.valueIdx };
				*varExp = e;
			}
			AddStructMembersToScope(context, usingExp->any.loc, varExp);
		}
		else
			AddStructMembersToScope(context, usingExp->any.loc, usingExp);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String procName = expression->procedureCall.name;

		// Yieldy things here
		if (!expression->procedureCall.procedureFound)
		{
			ProcedureCallType callType = CALLTYPE_STATIC;
			s32 procedureIdx = S32_MIN;
			TCValue tcValue = {};
			ASTExpression *astExpression = nullptr;
			s64 procedureTypeIdx = -1;

			TCScopeName *scopeName = FindScopeName(context, procName);
			if (scopeName == nullptr)
			{
#if PRINT_DEBUG_NOTES_UPON_YIELDING
				LogNote(context, expression->any.loc, TPrintF("Invalid procedure \"%S\" called",
						procName));
#endif
				TCYieldInfo yieldInfo = {};
				yieldInfo.cause = TCYIELDCAUSE_MISSING_NAME;
				yieldInfo.name = procName;
				yieldInfo.loc = expression->any.loc;
				return { false, yieldInfo };
			}

			if (scopeName->type == NAMETYPE_VARIABLE)
			{
				callType = CALLTYPE_VALUE;
				tcValue = scopeName->variableInfo.tcValue;
				procedureTypeIdx = scopeName->variableInfo.typeTableIdx;
			}
			else if (scopeName->type == NAMETYPE_ASTEXPRESSION)
			{
				callType = CALLTYPE_ASTEXPRESSION;
				astExpression = scopeName->expression;
				procedureTypeIdx = scopeName->expression->typeTableIdx;
			}
			else if (scopeName->type == NAMETYPE_STATIC_DEFINITION &&
				scopeName->staticDefinition->definitionType == STATICDEFINITIONTYPE_PROCEDURE)
			{
				procedureIdx = scopeName->staticDefinition->procedureIdx;
				Procedure *proc = GetProcedure(context, procedureIdx);
				if (proc->isInline && !proc->isBodyTypeChecked)
				{
					// We need the whole body type checked
					TCYieldInfo yieldInfo = {};
					yieldInfo.cause = TCYIELDCAUSE_NEED_TYPE_CHECKING;
					yieldInfo.name = procName;
					yieldInfo.loc = expression->any.loc;
					return { false, yieldInfo };
				}
				procedureTypeIdx = proc->typeTableIdx;
			}
			else
				LogError(context, expression->any.loc, "Calling a non-procedure"_s);

			// @Todo: don't look up procedure again after this yields
			if (procedureTypeIdx == TYPETABLEIDX_UNSET)
			{
				TCYieldInfo yieldInfo = {};
				yieldInfo.cause = TCYIELDCAUSE_NEED_TYPE_CHECKING;
				yieldInfo.name = procName;
				yieldInfo.loc = expression->any.loc;
				return { false, yieldInfo };
			}

			s64 givenArguments = expression->procedureCall.arguments.size;
			u32 *argIdx = &expression->procedureCall.parameterTypeCheckingIdx;
			for (; *argIdx < givenArguments; ++*argIdx)
			{
				ASTExpression *arg = &expression->procedureCall.arguments[*argIdx];
				TypeCheckExpressionResult result = TryTypeCheckExpression(context, arg);
				if (!result.success)
					return { false, result.yieldInfo };
			}

			expression->procedureCall.callType = callType;
			if (callType == CALLTYPE_VALUE)
				expression->procedureCall.tcValue = tcValue;
			else if (callType == CALLTYPE_ASTEXPRESSION)
				expression->procedureCall.expression = astExpression;
			else
				expression->procedureCall.procedureIdx = procedureIdx;
			expression->procedureCall.procedureTypeIdx = procedureTypeIdx;

			expression->procedureCall.procedureFound = true;
		}

		// No yield
		s64 procedureTypeIdx = expression->procedureCall.procedureTypeIdx;
		ASSERT(context->typeTable[procedureTypeIdx].typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = context->typeTable[procedureTypeIdx].procedureInfo;

		expression->typeTableIdx = procTypeInfo.returnTypeTableIdx;

		// Type check arguments
		s64 requiredArguments = 0;
		for (int i = 0; i < procTypeInfo.parameters.size; ++i)
		{
			if (procTypeInfo.parameters[i].defaultValue.type == CONSTANTTYPE_INVALID)
				++requiredArguments;
		}

		s64 totalArguments = procTypeInfo.parameters.size;
		s64 givenArguments = expression->procedureCall.arguments.size;
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

		s64 argsToCheck = Min(givenArguments, totalArguments);
		for (int argIdx = 0; argIdx < argsToCheck; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];
			s64 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					paramTypeIdx, arg);
			arg->typeTableIdx = typeCheckResult.rightTableIdx;

			if (typeCheckResult.errorCode != TYPECHECK_COOL)
			{
				String paramStr = TypeInfoToString(context, paramTypeIdx);
				String givenStr = TypeInfoToString(context, arg->typeTableIdx);
				LogError(context, arg->any.loc, TPrintF("When calling procedure \"%S\": type of "
							"parameter #%d didn't match (parameter is %S but %S was given)",
							procName, argIdx, paramStr, givenStr));
			}
		}

		if (expression->procedureCall.callType == CALLTYPE_STATIC)
		{
			Procedure *proc = GetProcedure(context, expression->procedureCall.procedureIdx);
			if (proc->isInline)
			{
				PushTCScope(context);

				TCAddParametersToScope(context, proc->astProcedureDeclaration->prototype);

				ASTExpression *e = NewTreeNode(context);
				*e = InlineProcedureCopyTreeBranch(context, proc->astProcedureDeclaration->astBody);
				expression->procedureCall.astBodyInlineCopy = e;

				PopTCScope(context);
			}
		}
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTExpression *input = expression->unaryOperation.expression;
		if (input->typeTableIdx == TYPETABLEIDX_UNSET)
		{
			TypeCheckExpressionResult result = TryTypeCheckExpression(context, input);
			if (!result.success)
				return { false, result.yieldInfo };
		}

		u64 expressionType = input->typeTableIdx;
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
			if (IsTemporalValue(input))
				LogError(context, expression->any.loc, "Trying to get pointer to temporal value"_s);

			ASTExpression *e = input;
			switch (e->nodeType)
			{
			case ASTNODETYPE_IDENTIFIER:
			{
				if (e->identifier.type == NAMETYPE_VARIABLE &&
					e->identifier.tcValue.type == TCVALUETYPE_VALUE)
					context->values[e->identifier.tcValue.valueIdx].flags |= VALUEFLAGS_FORCE_MEMORY;
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
			if (leftHand->typeTableIdx == TYPETABLEIDX_UNSET)
			{
				TypeCheckExpressionResult result = TryTypeCheckExpression(context, leftHand);
				if (!result.success)
					return { false, result.yieldInfo };
			}
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

				structTypeInfo = context->typeTable[TYPETABLEIDX_ARRAY_STRUCT];
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
				rightHand->identifier.structMember = foundMember;
				expression->typeTableIdx = foundMember->typeTableIdx;
			}
			else
				LogError(context, expression->any.loc, TPrintF("\"%S\" is not a member of \"%S\"",
						memberName, TypeInfoToString(context, leftHandTypeIdx)));
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
		{
			TypeCheckExpressionResult result;
			if (leftHand->typeTableIdx == TYPETABLEIDX_UNSET)
			{
				result = TryTypeCheckExpression(context, leftHand);
				if (!result.success)
					return { false, result.yieldInfo };
			}
			if (rightHand->typeTableIdx == TYPETABLEIDX_UNSET)
			{
				result = TryTypeCheckExpression(context, rightHand);
				if (!result.success)
					return { false, result.yieldInfo };
			}

			s64 arrayType = leftHand->typeTableIdx;
			TypeInfo arrayTypeInfo = context->typeTable[arrayType];
			if (arrayTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			{
				s64 pointedTypeIdx = arrayTypeInfo.pointerInfo.pointedTypeTableIdx;
				arrayType = pointedTypeIdx;
				arrayTypeInfo = context->typeTable[pointedTypeIdx];
			}

			if (arrayType == TYPETABLEIDX_STRING_STRUCT)
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
			TypeCheckExpressionResult result;
			if (leftHand->typeTableIdx == TYPETABLEIDX_UNSET)
			{
				result = TryTypeCheckExpression(context, leftHand);
				if (!result.success)
					return { false, result.yieldInfo };
			}
			if (rightHand->typeTableIdx == TYPETABLEIDX_UNSET)
			{
				result = TryTypeCheckExpression(context, rightHand);
				if (!result.success)
					return { false, result.yieldInfo };
			}

			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					leftHand->typeTableIdx, rightHand);
			if (typeCheckResult.errorCode != TYPECHECK_COOL)
			{
				String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
				String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
				LogError(context, expression->any.loc, TPrintF("Type mismatch! (%S and %S)",
							leftStr, rightStr));
			}
			leftHand->typeTableIdx  = typeCheckResult.leftTableIdx;
			rightHand->typeTableIdx = typeCheckResult.rightTableIdx;

			OperatorOverload overload = {};
			TypeInfo leftTypeInfo = context->typeTable[leftHand->typeTableIdx];
			if (leftTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
				leftTypeInfo.typeCategory == TYPECATEGORY_UNION)
			{
				for (int i = 0; i < leftTypeInfo.structInfo.overloads.size; ++i)
				{
					overload = leftTypeInfo.structInfo.overloads[i];
					if (overload.op == expression->binaryOperation.op)
						goto skipNoOverloadError;
				}
				if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
					// This allows default 'overload'
					goto skipOverload;
				// Everyone else reports error
				LogError(context, expression->any.loc, TPrintF("No overload for operator %S found in struct \"%S\"",
						OperatorToString(expression->binaryOperation.op), leftTypeInfo.structInfo.name));
	skipNoOverloadError:
				// No type checking againts procedure prototype. We trust here that the overloads
				// got type checked properly in the struct declaration.
				Procedure *proc = GetProcedure(context, overload.procedureIdx);
				if (proc->isInline && !proc->isBodyTypeChecked)
				{
					TCYieldInfo yieldInfo = {};
					yieldInfo.cause = TCYIELDCAUSE_NEED_TYPE_CHECKING;
					yieldInfo.name = proc->name;
					yieldInfo.loc = expression->any.loc;
					return { false, yieldInfo };
				}
			}
skipOverload:

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

			// Overloads!
			if (overload.op != TOKEN_INVALID)
			{
				expression->nodeType = ASTNODETYPE_PROCEDURE_CALL;
				expression->procedureCall = {};
				expression->procedureCall.callType = CALLTYPE_STATIC;
				expression->procedureCall.procedureIdx = overload.procedureIdx;
				DynamicArrayInit(&expression->procedureCall.arguments, 2);
				*DynamicArrayAdd(&expression->procedureCall.arguments) = *leftHand;
				*DynamicArrayAdd(&expression->procedureCall.arguments) = *rightHand;
				// Inline?
				Procedure *proc = GetProcedure(context, overload.procedureIdx);
				if (proc->isInline)
				{
					PushTCScope(context);

					// Parameters
					TCAddParametersToScope(context, proc->astProcedureDeclaration->prototype);

					ASTExpression *e = NewTreeNode(context);
					*e = InlineProcedureCopyTreeBranch(context, proc->astProcedureDeclaration->astBody);
					expression->procedureCall.astBodyInlineCopy = e;

					PopTCScope(context);
				}
			}
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
			expression->typeTableIdx = TYPETABLEIDX_STRING_STRUCT;
			ASSERT(expression->typeTableIdx > 0);
			break;
		case LITERALTYPE_GROUP:
			for (int memberIdx = 0; memberIdx < expression->literal.members.size; ++memberIdx)
			{
				ASTExpression *memberExp = expression->literal.members[memberIdx];
				if (memberExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
					memberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
				{
					ASTExpression *leftExp  = memberExp->binaryOperation.leftHand;
					ASTExpression *rightExp = memberExp->binaryOperation.rightHand;

					if (leftExp->nodeType != ASTNODETYPE_IDENTIFIER)
						LogError(context, leftExp->any.loc, "Expected identifier before '='"_s);

					TypeCheckExpressionResult result = TryTypeCheckExpression(context, rightExp);
					if (!result.success)
						return { false, result.yieldInfo };
				}
				else
				{
					TypeCheckExpressionResult result = TryTypeCheckExpression(context, memberExp);
					if (!result.success)
						return { false, result.yieldInfo };
				}
			}
			expression->typeTableIdx = TYPETABLEIDX_STRUCT_LITERAL;
			break;
		default:
			ASSERT(!"Unexpected literal type");
		}
	} break;
	case ASTNODETYPE_IF:
	{
		if (expression->ifNode.condition->typeTableIdx == TYPETABLEIDX_UNSET)
		{
			TypeCheckExpressionResult result =
				TryTypeCheckExpression(context, expression->ifNode.condition);
			if (!result.success)
				return { false, result.yieldInfo };
		}

		s64 conditionType = expression->ifNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		if (expression->ifNode.body->typeTableIdx == TYPETABLEIDX_UNSET)
		{
			TypeCheckExpressionResult result =
				TryTypeCheckExpression(context, expression->ifNode.body);
			if (!result.success)
				return { false, result.yieldInfo };
		}

		if (expression->ifNode.elseBody &&
			expression->ifNode.elseBody->typeTableIdx == TYPETABLEIDX_UNSET)
		{
			TypeCheckExpressionResult result =
				TryTypeCheckExpression(context, expression->ifNode.elseBody);
			if (!result.success)
				return { false, result.yieldInfo };
		}
	} break;
	case ASTNODETYPE_WHILE:
	{
		if (expression->whileNode.condition->typeTableIdx == TYPETABLEIDX_UNSET)
		{
			TypeCheckExpressionResult result = TryTypeCheckExpression(context, expression->whileNode.condition);
			if (!result.success)
				return { false, result.yieldInfo };
		}

		s64 conditionType = expression->whileNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "While condition doesn't evaluate to a boolean"_s);

		TypeCheckExpressionResult result = TryTypeCheckExpression(context, expression->whileNode.body);
		if (!result.success)
			return { false, result.yieldInfo };
	} break;
	case ASTNODETYPE_FOR:
	{
		if (expression->forNode.range->typeTableIdx == TYPETABLEIDX_UNSET)
		{
			TypeCheckExpressionResult result = TryTypeCheckExpression(context, expression->forNode.range);
			if (!result.success)
				return { false, result.yieldInfo };
		}

		if (!expression->forNode.scopePushed)
		{
			PushTCScope(context);

			String indexValueName = "i"_s;
			u32 indexValueIdx = NewValue(context, indexValueName, TYPETABLEIDX_S64, 0);
			expression->forNode.indexValueIdx = indexValueIdx;

			TCScope *stackTop = GetTopMostScope(context);
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = indexValueName;
			newScopeName.variableInfo.tcValue = { TCVALUETYPE_VALUE, indexValueIdx };
			newScopeName.variableInfo.typeTableIdx = TYPETABLEIDX_S64;
			newScopeName.loc = expression->any.loc;
			*DynamicArrayAdd(&stackTop->names) = newScopeName;

			ASTExpression *rangeExp = expression->forNode.range;
			bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
				rangeExp->binaryOperation.op == TOKEN_OP_RANGE;

			if (!isExplicitRange)
			{
				s64 elementTypeTableIdx = TYPETABLEIDX_U8;
				if (rangeExp->typeTableIdx != TYPETABLEIDX_STRING_STRUCT)
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
				newScopeName.variableInfo.tcValue = { TCVALUETYPE_VALUE, elementValueIdx };
				newScopeName.variableInfo.typeTableIdx = pointerToElementTypeTableIdx;
				newScopeName.loc = expression->any.loc;
				*DynamicArrayAdd(&stackTop->names) = newScopeName;
			}

			expression->forNode.scopePushed = true;
		}

		TypeCheckExpressionResult result = TryTypeCheckExpression(context, expression->forNode.body);
		if (!result.success)
			return { false, result.yieldInfo };

		PopTCScope(context);
	} break;
	case ASTNODETYPE_BREAK:
	{
	} break;
	case ASTNODETYPE_CONTINUE:
	{
	} break;
	case ASTNODETYPE_TYPE:
	{
		TypeCheckTypeResult result = TypeCheckType(context, {}, expression->any.loc, &expression->astType);
		if (!result.success)
			return { false, result.yieldInfo };
		expression->typeTableIdx = result.typeTableIdx;
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		TypeCheckExpressionResult result = TryTypeCheckExpression(context, expression->typeOfNode.expression);
		if (!result.success)
			return { false, result.yieldInfo };

		static s64 typeInfoPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT);
		expression->typeTableIdx = typeInfoPointerTypeIdx;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		TypeCheckExpressionResult result = TryTypeCheckExpression(context, expression->sizeOfNode.expression);
		if (!result.success)
			return { false, result.yieldInfo };
		expression->typeTableIdx = TYPETABLEIDX_S64;
	} break;
	case ASTNODETYPE_CAST:
	{
		TypeCheckExpressionResult result = TryTypeCheckExpression(context,
				expression->castNode.expression);
		if (!result.success)
			return { false, result.yieldInfo };

		TypeCheckTypeResult typeCheckResult = TypeCheckType(context, {}, expression->any.loc,
				&expression->castNode.astType);
		if (!typeCheckResult.success)
			return { false, typeCheckResult.yieldInfo };

		expression->typeTableIdx = typeCheckResult.typeTableIdx;
	} break;
	case ASTNODETYPE_INTRINSIC:
	{
		if (expression->intrinsic.type == INTRINSIC_UNSET)
		{
			if (StringEquals(expression->intrinsic.name, "sqrt32"_s))
				expression->intrinsic.type = INTRINSIC_SQRT32;
			else if (StringEquals(expression->intrinsic.name, "sqrt64"_s))
				expression->intrinsic.type = INTRINSIC_SQRT64;
			else
				LogError(context, expression->any.loc, "Invalid compiler intrinsic"_s);
		}

		FixedArray<s64, 4> argTypes;
		switch (expression->intrinsic.type)
		{
		case INTRINSIC_SQRT32:
		{
			argTypes[0] = TYPETABLEIDX_F32;
			argTypes[1] = TYPETABLEIDX_F32;
			argTypes.size = 2;
		} break;
		case INTRINSIC_SQRT64:
		{
			argTypes[0] = TYPETABLEIDX_F64;
			argTypes[1] = TYPETABLEIDX_F64;
			argTypes.size = 2;
		} break;
		default:
			ASSERT(false);
		}

		if (expression->intrinsic.arguments.size > ArrayCount(argTypes))
			LogError(context, expression->any.loc, "Too many arguments for intrinsic"_s);
		for (int argIdx = 0; argIdx < expression->intrinsic.arguments.size; ++argIdx)
		{
			ASTExpression *arg = &expression->intrinsic.arguments[argIdx];
			TypeCheckExpressionResult result = TryTypeCheckExpression(context, arg);
			if (!result.success)
				return { false, result.yieldInfo };
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					argTypes[argIdx], arg);
			arg->typeTableIdx = typeCheckResult.rightTableIdx;

			if (typeCheckResult.errorCode != TYPECHECK_COOL)
			{
				String paramStr = TypeInfoToString(context, argTypes[argIdx]);
				String givenStr = TypeInfoToString(context, arg->typeTableIdx);
				LogError(context, arg->any.loc, TPrintF("When calling procedure \"%S\": type of "
							"parameter #%d didn't match (parameter is %S but %S was given)",
							expression->intrinsic.name, argIdx, paramStr, givenStr));
			}
		}
		expression->typeTableIdx = TYPETABLEIDX_VOID;
	} break;
	default:
	{
		LogError(context, expression->any.loc, "COMPILER ERROR! Unknown expression type on type checking"_s);
	} break;
	}

	TCYieldInfo yieldInfo = {};
	yieldInfo.cause = TCYIELDCAUSE_DONE;
	return { true, yieldInfo, expression->typeTableIdx };
}

void GenerateTypeCheckJobs(Context *context, ASTExpression *expression)
{
	switch (expression->nodeType)
	{
	case ASTNODETYPE_BLOCK:
	{
		for (int i = 0; i < expression->block.statements.size; ++i)
			GenerateTypeCheckJobs(context, &expression->block.statements[i]);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		TCJob job = { expression };
		*DynamicArrayAdd(&context->tcJobs) = job;
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition astStaticDef = expression->staticDefinition;

		TCJob job = { expression };
		if (astStaticDef.expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
			DynamicArrayInit(&job.scopeStack, 16);
		*DynamicArrayAdd(&context->tcJobs) = job;
	} break;
	case ASTNODETYPE_GARBAGE:
	case ASTNODETYPE_RETURN:
	case ASTNODETYPE_DEFER:
	case ASTNODETYPE_IDENTIFIER:
	case ASTNODETYPE_PROCEDURE_CALL:
	case ASTNODETYPE_UNARY_OPERATION:
	case ASTNODETYPE_BINARY_OPERATION:
	case ASTNODETYPE_LITERAL:
	case ASTNODETYPE_IF:
	case ASTNODETYPE_WHILE:
	case ASTNODETYPE_FOR:
	case ASTNODETYPE_BREAK:
	case ASTNODETYPE_CONTINUE:
	case ASTNODETYPE_TYPE:
	case ASTNODETYPE_TYPEOF:
	case ASTNODETYPE_SIZEOF:
	case ASTNODETYPE_CAST:
	case ASTNODETYPE_INTRINSIC:
	{
		LogError(context, expression->any.loc, "COMPILER ERROR! Invalid expression type found"
				"while generating type checking jobs"_s);
	} break;
	default:
	{
		LogError(context, expression->any.loc, "COMPILER ERROR! Unknown expression type found"
				"while generating type checking jobs"_s);
	} break;
	}
}

void TypeCheckMain(Context *context)
{
	context->tcCurrentReturnType = -1;

	DynamicArrayInit(&context->tcJobs, 128);

	BucketArrayInit(&context->staticDefinitions);

	BucketArrayInit(&context->procedures);
	// Procedure 0 is invalid
	*BucketArrayAdd(&context->procedures) = {};
	*BucketArrayAdd(&context->externalProcedures) = {};

	context->tcGlobalScope = ALLOC(malloc, TCScope);
	DynamicArrayInit(&context->tcGlobalScope->names, 64);
	DynamicArrayInit(&context->tcGlobalScope->typeIndices, 64);

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

		t.size = 16;
		t.valueIdx = NewValue(context, "_typeInfo_128"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_128] = t;

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

		t.valueIdx = NewValue(context, "_typeInfo_string_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_STRING_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_array_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_ARRAY_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_any_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_ANY_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_type_info_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_type_info_integer_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_type_info_struct_member_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_type_info_struct_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_type_info_enum_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_type_info_pointer_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT] = t;
		t.valueIdx = NewValue(context, "_typeInfo_type_info_array_struct"_s, -1, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTable[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT] = t;
	}
	for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
		*DynamicArrayAdd(&context->tcGlobalScope->typeIndices) = i;

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		GenerateTypeCheckJobs(context, statement);
	}

	while (true)
	{
		bool anyJobMadeAdvancements = false;
		for (int jobIdx = 0; jobIdx < context->tcJobs.size; ++jobIdx)
		{
			TCJob *job = &context->tcJobs[jobIdx];
			TCYieldInfo newYieldInfo;
			switch (job->yieldInfo.cause)
			{
			// @Todo: TCYIELDCAUSE_NEED_TYPE_CHECKING
			case TCYIELDCAUSE_DONE:
				continue;
			case TCYIELDCAUSE_MISSING_NAME:
			{
				context->currentTCJob = jobIdx;
				// Check if name now exists
				TCScopeName *scopeName = FindScopeName(context, job->yieldInfo.name);
				if (scopeName == nullptr)
					continue;
				else
				{
					TypeCheckExpressionResult result =
						TryTypeCheckExpression(context, job->expression);
					newYieldInfo = result.yieldInfo;
				}
			} break;
			case TCYIELDCAUSE_NONE:
			default:
			{
				context->currentTCJob = jobIdx;
				TypeCheckExpressionResult result = TryTypeCheckExpression(context, job->expression);
				newYieldInfo = result.yieldInfo;
			}
			}
			// !!! Update if TCYieldInfo is changed!
			if (job->yieldInfo.cause != newYieldInfo.cause &&
				!StringEquals(job->yieldInfo.name, newYieldInfo.name))
				anyJobMadeAdvancements = true;

			job->yieldInfo = newYieldInfo;
		}

		for (int jobIdx = 0; jobIdx < context->tcJobs.size;)
		{
			TCJob *job = &context->tcJobs[jobIdx];
			if (job->yieldInfo.cause == TCYIELDCAUSE_DONE)
				*job = context->tcJobs[--context->tcJobs.size];
			else
				++jobIdx;
		}

		if (context->tcJobs.size == 0) break;

		if (!anyJobMadeAdvancements)
		{
			for (int jobIdx = 0; jobIdx < context->tcJobs.size; ++jobIdx)
			{
				TCJob *job = &context->tcJobs[jobIdx];
				switch (job->yieldInfo.cause)
				{
				case TCYIELDCAUSE_DONE:
					continue;
				case TCYIELDCAUSE_MISSING_NAME:
					LogErrorNoCrash(context, job->yieldInfo.loc, TPrintF("Identifier \"%S\" not found!",
								job->yieldInfo.name));
					break;
				case TCYIELDCAUSE_NEED_TYPE_CHECKING:
					LogErrorNoCrash(context, job->yieldInfo.loc, TPrintF("COMPILER: Name \"%S\" never type checked!",
								job->yieldInfo.name));
					break;
				case TCYIELDCAUSE_NONE:
				default:
					LogErrorNoCrash(context, job->yieldInfo.loc, "COMPILER: Unspecified type checking error"_s);
				}
			}

			Print("Compiler isn't making advancements. Stopping.\n");
			CRASH;
		}
	}
}
