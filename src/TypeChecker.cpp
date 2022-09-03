u32 TCNewValue(Context *context, TCJob *job, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	u64 idx = BucketArrayCount(&job->localValues);
	Value *result = BucketArrayAdd(&job->localValues);
	result->name = {};
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 TCNewValue(Context *context, TCJob *job, String name, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	u64 idx = BucketArrayCount(&job->localValues);
	Value *result = BucketArrayAdd(&job->localValues);
	result->name = name;
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 TCNewValue(Context *context, TCJob *job, Value value)
{
	ASSERT(value.typeTableIdx != 0);
	ASSERT(!(value.flags & VALUEFLAGS_TRY_IMMITATE) || value.tryImmitateValueIdx != U32_MAX);

	u64 idx = BucketArrayCount(&job->localValues);
	Value *result = BucketArrayAdd(&job->localValues);
	*result = value;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

inline Value *TCGetValue(Context *context, TCJob *job, u32 valueIdx)
{
	ASSERT(!(valueIdx & 0x80000000));
	return &job->localValues[valueIdx];
}

u32 NewGlobalValue(Context *context, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	auto values = context->values.GetForWrite();

	u64 idx = BucketArrayCount(&values);
	Value *result = BucketArrayAdd(&values);
	result->name = {};
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	idx |= 0x80000000;
	return (u32)idx;
}

u32 NewGlobalValue(Context *context, String name, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	auto values = context->values.GetForWrite();

	u64 idx = BucketArrayCount(&values);
	Value *result = BucketArrayAdd(&values);
	result->name = name;
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	idx |= 0x80000000;
	return (u32)idx;
}

u32 NewGlobalValue(Context *context, Value value)
{
	ASSERT(value.typeTableIdx != 0);
	ASSERT(!(value.flags & VALUEFLAGS_TRY_IMMITATE) || value.tryImmitateValueIdx != U32_MAX);

	auto values = context->values.GetForWrite();

	u64 idx = BucketArrayCount(&values);
	Value *result = BucketArrayAdd(&values);
	*result = value;

	ASSERT(idx < U32_MAX);
	idx |= 0x80000000;
	return (u32)idx;
}

inline Value GetGlobalValue(Context *context, u32 valueIdx)
{
	ASSERT(valueIdx & 0x80000000);
	auto values = context->values.GetForRead();
	Value result = (*values)[valueIdx & 0x7FFFFFFF];
	return result;
}

inline void UpdateGlobalValue(Context *context, u32 valueIdx, Value *value)
{
	ASSERT(valueIdx & 0x80000000);
	auto values = context->values.GetForWrite();
	(*values)[valueIdx & 0x7FFFFFFF] = *value;
}

inline void TCSetValueFlags(Context *context, TCJob *job, u32 valueIdx, u32 flags)
{
	if (valueIdx & 0x80000000)
	{
		auto values = context->values.GetForWrite();
		(*values)[valueIdx & 0x7FFFFFFF].flags |= flags;
	}
	else
		job->localValues[valueIdx].flags |= flags;
}

inline Value TCGetValueRead(Context *context, TCJob *job, u32 valueIdx)
{
	if (valueIdx & 0x80000000)
		return GetGlobalValue(context, valueIdx);
	else
		return job->localValues[valueIdx];
}

inline Procedure GetProcedureRead(Context *context, s32 procedureIdx)
{
	Procedure result;
	ASSERT(procedureIdx != 0);
	if (procedureIdx > 0)
	{
		auto procedures = context->procedures.GetForRead();
		result = (*procedures)[procedureIdx];
	}
	else
	{
		auto externalProcedures = context->externalProcedures.GetForRead();
		result = (*externalProcedures)[-procedureIdx];
	}

	return result;
}

inline void UpdateProcedure(Context *context, s32 procedureIdx, Procedure *value)
{
	ASSERT(procedureIdx != 0);
	if (procedureIdx > 0)
	{
		auto procedures = context->procedures.GetForWrite();
		(*procedures)[procedureIdx] = *value;
	}
	else
	{
		auto externalProcedures = context->externalProcedures.GetForWrite();
		(*externalProcedures)[-procedureIdx] = *value;
	}
}

TCScope *GetTopMostScope(Context *context, TCJob *job)
{
	if (job->scopeStack.size > 0)
		return DynamicArrayBack(&job->scopeStack);
	else
		return nullptr;
}

inline TypeInfo GetTypeInfo(Context *context, u32 typeTableIdx)
{
	auto typeTable = context->typeTable.GetForRead();
	TypeInfo result = (*typeTable)[typeTableIdx];
	return result;
}

String TypeInfoToString(Context *context, u32 typeTableIdx)
{
	if (typeTableIdx == TYPETABLEIDX_VOID)
		return "void"_s;
	if (typeTableIdx == TYPETABLEIDX_INTEGER)
		return "<number>"_s;
	if (typeTableIdx == TYPETABLEIDX_FLOATING)
		return "<floating>"_s;

	TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
	switch (typeInfo.typeCategory)
	{
	case TYPECATEGORY_STRUCT:
	case TYPECATEGORY_UNION:
	{
		String name = typeInfo.structInfo.name;
		if (name.size)
			return name;
		return typeInfo.typeCategory == TYPECATEGORY_UNION ? "<union>"_s : "<struct>"_s;
	}
	case TYPECATEGORY_ENUM:
	{
		String name = typeInfo.enumInfo.name;
		if (name.size)
			return name;
		return "<enum>"_s;
	}
	case TYPECATEGORY_ALIAS:
	{
		String name = typeInfo.aliasInfo.name;
		String nameOfAliased = TypeInfoToString(context, typeInfo.aliasInfo.aliasedTypeIdx);
		if (name.size)
			return TPrintF("%S (alias of %S)", name, nameOfAliased);
		return TPrintF("<alias of %S>", nameOfAliased);
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
		if (typeInfo.procedureInfo.callingConvention == CC_WIN64)
			result = "cc:win64 ("_s;
		else if (typeInfo.procedureInfo.callingConvention == CC_LINUX64)
			result = "cc:linux64 ("_s;
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

inline void PushTCScope(Context *context, TCJob *job)
{
	TCScope *newScope = DynamicArrayAdd(&job->scopeStack);

	DynamicArrayInit(&newScope->names, 64);
	DynamicArrayInit(&newScope->typeIndices, 64);
}

inline void PopTCScope(Context *context, TCJob *job)
{
	--job->scopeStack.size;
}

inline bool TCIsAnyJobRunning(Context *context)
{
	{
		auto jobStates = context->tcJobStates.GetForRead();
		for (int i = 0; i < (*jobStates).size; ++i)
			if ((*jobStates)[i] == TCJOBSTATE_RUNNING ||
				(*jobStates)[i] == TCJOBSTATE_WAITING_FOR_STOP)
				return true;
	}
	{
		auto jobStates = context->parseJobStates.GetForRead();
		for (int i = 0; i < (*jobStates).size; ++i)
			if ((*jobStates)[i] == TCJOBSTATE_RUNNING)
				return true;
	}
	return false;
}

// Try to sleep and yield execution to other jobs.
// Returns false all other jobs are also sleeping.
// Caller must call TCResume after finding what it's looking for, to set the job state back to
// RUNNING.
bool TCYield(Context *context, TCJob *job)
{
#if USE_PROFILER_API
	performanceAPI.BeginEvent("TC Yield", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xE0, 0x10, 0x10));
#endif

	{
		auto jobStates = context->tcJobStates.GetForWrite();
		(*jobStates)[job->jobIdx] = TCJOBSTATE_SLEEPING;
	}
	if (!TCIsAnyJobRunning(context))
	{
#if USE_PROFILER_API
		performanceAPI.EndEvent();
#endif
		return false;
	}
	Sleep(0);
#if USE_PROFILER_API
	performanceAPI.EndEvent();
#endif
	return true;
}

bool TCYield(Context *context, TCJob *job, String name, SRWLOCK *lock)
{
#if USE_PROFILER_API
	performanceAPI.BeginEvent("TC Yield", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xE0, 0x10, 0x10));
#endif

	{
		auto jobStates = context->tcJobStates.GetForWrite();
		(*jobStates)[job->jobIdx] = TCJOBSTATE_SLEEPING;
	}
	if (!TCIsAnyJobRunning(context))
	{
#if USE_PROFILER_API
		performanceAPI.EndEvent();
#endif
		return false;
	}
	CONDITION_VARIABLE *conditionVar;
	{
		auto conditionVariables = context->tcConditionVariables.GetForRead();
		conditionVar = HashMapGet(*conditionVariables, name);
	}
	if (!conditionVar)
	{
		auto conditionVariables = context->tcConditionVariables.GetForWrite();
		conditionVar = HashMapGet(*conditionVariables, name);
		if (!conditionVar)
		{
			conditionVar = HashMapGetOrAdd(&conditionVariables, name);
			InitializeConditionVariable(conditionVar);
		}
	}
	SleepConditionVariableSRW(conditionVar, lock, INFINITE,
			CONDITION_VARIABLE_LOCKMODE_SHARED);
#if USE_PROFILER_API
	performanceAPI.EndEvent();
#endif
	return true;
}

inline void TCResume(Context *context, TCJob *job)
{
	auto jobStates = context->tcJobStates.GetForWrite();
	(*jobStates)[job->jobIdx] = TCJOBSTATE_RUNNING;
}

TCScopeName TCFindScopeName(Context *context, TCJob *job, String name)
{
	// Current stack
	ArrayView<TCScope> scopeStack = job->scopeStack;
	for (s64 stackIdx = scopeStack.size - 1; stackIdx >= 0; --stackIdx)
	{
		const TCScope *currentScope = &scopeStack[stackIdx];
		for (int i = 0; i < currentScope->names.size; ++i)
		{
			const TCScopeName *currentName = &currentScope->names[i];
			if (StringEquals(name, currentName->name))
				return *currentName;
		}
	}
	// Global scope
	while (true)
	{
		auto globalScope = context->tcGlobalScope.GetForRead();
		{
			for (int i = 0; i < globalScope->names.size; ++i)
			{
				const TCScopeName *currentName = &globalScope->names[i];
				if (StringEquals(name, currentName->name))
				{
					TCResume(context, job);
					return *currentName;
				}
			}
		}
		{
			auto jobStates = context->tcJobStates.GetForWrite();
			(*jobStates)[job->jobIdx] = TCJOBSTATE_SLEEPING;
		}
		if (!TCIsAnyJobRunning(context))
		{
			TCResume(context, job);
			return { NAMETYPE_INVALID };
		}
		CONDITION_VARIABLE *conditionVar;
		{
			auto conditionVariables = context->tcConditionVariables.GetForRead();
			conditionVar = HashMapGet(*conditionVariables, name);
		}
		if (!conditionVar)
		{
			auto conditionVariables = context->tcConditionVariables.GetForWrite();
			conditionVar = HashMapGet(*conditionVariables, name);
			if (!conditionVar)
			{
				conditionVar = HashMapGetOrAdd(&conditionVariables, name);
				InitializeConditionVariable(conditionVar);
			}
		}
		SleepConditionVariableSRW(conditionVar, &context->tcGlobalScope.rwLock, INFINITE,
				CONDITION_VARIABLE_LOCKMODE_SHARED);

	}
}

inline u32 TCNewStaticDefinition(Context *context, StaticDefinition *value)
{
	auto staticDefinitions = context->staticDefinitions.GetForWrite();
	u64 result = BucketArrayCount(&staticDefinitions);
	*BucketArrayAdd(&staticDefinitions) = *value;
	ASSERT(result < U32_MAX);

	CONDITION_VARIABLE *conditionVar;
	{
		auto conditionVariables = context->tcConditionVariables.GetForWrite();
		conditionVar = HashMapGet(*conditionVariables, value->name);
		if (conditionVar)
			HashMapRemove(&conditionVariables, value->name);
	}
	if (conditionVar)
		WakeAllConditionVariable(conditionVar);

	return (u32)result;
}

inline StaticDefinition GetStaticDefinition(Context *context, u32 staticDefinitionIdx)
{
	auto staticDefinitions = context->staticDefinitions.GetForRead();
	return (*staticDefinitions)[staticDefinitionIdx];
}

inline StaticDefinition TCGetStaticDefinition(Context *context, TCJob *job, u32 staticDefinitionIdx,
		bool ensureTypeChecked)
{
	auto staticDefinitions = context->staticDefinitions.GetForRead();
	StaticDefinition staticDefinition;
	while (true)
	{
		staticDefinition = (*staticDefinitions)[staticDefinitionIdx];
		if (staticDefinition.definitionType != STATICDEFINITIONTYPE_NOT_READY &&
			(!ensureTypeChecked || staticDefinition.typeTableIdx != TYPETABLEIDX_UNSET))
			break;

		if (!TCYield(context, job, staticDefinition.name, &context->staticDefinitions.rwLock))
			LogError(context, {},
					TPrintF("COMPILER ERROR! Static definition \"%S\" never processed",
					staticDefinition.name));
	}
	return staticDefinition;
}

inline void TCUpdateStaticDefinition(Context *context, TCJob *job, u32 staticDefinitionIdx,
		StaticDefinition *value)
{
	{
		auto staticDefinitions = context->staticDefinitions.GetForWrite();
		(*staticDefinitions)[staticDefinitionIdx] = *value;
	}

	CONDITION_VARIABLE *conditionVar;
	{
		auto conditionVariables = context->tcConditionVariables.GetForWrite();
		conditionVar = HashMapGet(*conditionVariables, value->name);
		if (conditionVar)
			HashMapRemove(&conditionVariables, value->name);
	}
	if (conditionVar)
		WakeAllConditionVariable(conditionVar);
}

u32 FindTypeInStackByName(Context *context, TCJob *job, SourceLocation loc, String name)
{
	u32 typeTableIdx = TYPETABLEIDX_UNSET;

	TCScopeName scopeName = TCFindScopeName(context, job, name);

	if (scopeName.type == NAMETYPE_INVALID)
		LogError(context, loc, TPrintF("Identifier \"%S\" not found!", name));

	if (scopeName.type == NAMETYPE_PRIMITIVE)
		return scopeName.primitiveTypeTableIdx;
	else if (scopeName.type != NAMETYPE_STATIC_DEFINITION)
		LogError(context, loc, TPrintF("\"%S\" is not a type!", name));

	StaticDefinition staticDefinition =
		TCGetStaticDefinition(context, job, scopeName.staticDefinitionIdx, true);

	if (staticDefinition.definitionType != STATICDEFINITIONTYPE_TYPE)
		LogError(context, loc, TPrintF("\"%S\" is not a type!", name));

	typeTableIdx = staticDefinition.typeTableIdx;
	return typeTableIdx;
}

inline void TCCheckIfNameAlreadyExists(Context *context, TCJob *job, String name, SourceLocation loc)
{
	TCScope *stackTop = GetTopMostScope(context, job);
	if (stackTop)
	{
		for (int i = 0; i < stackTop->names.size; ++i)
		{
			TCScopeName *currentName = &stackTop->names[i];
			if (!StringEquals(name, currentName->name))
				continue;

			if (currentName->type == NAMETYPE_PRIMITIVE)
				LogError(context, loc, TPrintF("Can not use name \"%S\", it is a language primitive",
						name));
			else
			{
				LogErrorNoCrash(context, loc, TPrintF("Duplicate static definition \"%S\"", name));
				LogNote(context, currentName->loc, "First defined here"_s);
				PANIC;
			}
			break;
		}
	}
	else
	{
		auto globalScope = context->tcGlobalScope.GetForRead();
		for (int i = 0; i < globalScope->names.size; ++i)
		{
			const TCScopeName *currentName = &globalScope->names[i];
			if (!StringEquals(name, currentName->name))
				continue;

			if (currentName->type == NAMETYPE_PRIMITIVE)
				LogError(context, loc, TPrintF("Can not use name \"%S\", it is a language primitive",
						name));
			else
			{
				LogErrorNoCrash(context, loc, TPrintF("Duplicate static definition \"%S\"", name));
				LogNote(context, currentName->loc, "First defined here"_s);
				PANIC;
			}
			break;
		}
	}

	// Primitives
	// @Speed: maybe keep these in a separate array?
	{
		auto globalScope = context->tcGlobalScope.GetForRead();
		for (int i = 0; i < globalScope->names.size; ++i)
		{
			const TCScopeName *currentName = &globalScope->names[i];
			if (currentName->type == NAMETYPE_PRIMITIVE && StringEquals(name, currentName->name))
				LogError(context, loc, TPrintF("Can not use name \"%S\", it is a language primitive",
						name));
		}
	}
}

enum TypeCheckErrorCode
{
	TYPECHECK_COOL,
	TYPECHECK_MISC_PANIC,
	TYPECHECK_SIGN_MISMATCH,
	TYPECHECK_SIZE_MISMATCH,
	TYPECHECK_TYPE_CATEGORY_MISMATCH,
	TYPECHECK_POINTED_TYPE_MISMATCH,
	TYPECHECK_ARRAY_SIZE_MISMATCH,
	TYPECHECK_STRUCT_MISMATCH,
	TYPECHECK_CANT_DEDUCE_TYPE
};

void ReportTypeCheckError(Context *context, TypeCheckErrorCode errorCode, SourceLocation sourceLoc,
		u32 leftTableIdx, u32 rightTableIdx)
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
	case TYPECHECK_MISC_PANIC:
		LogError(context, sourceLoc, TPrintF(
			"Expression type mismatch! (left is %S and right is %S)", leftStr, rightStr));
	default:
		ASSERT(errorCode == TYPECHECK_COOL);
	}
}

inline u32 StripImplicitlyCastAliases(Context *context, u32 typeTableIdx)
{
	auto typeTable = context->typeTable.GetForRead();
	TypeInfo typeInfo = (*typeTable)[typeTableIdx];
	while (typeInfo.typeCategory == TYPECATEGORY_ALIAS &&
		   typeInfo.aliasInfo.doesImplicitlyCast)
	{
		typeTableIdx = typeInfo.aliasInfo.aliasedTypeIdx;
		typeInfo = (*typeTable)[typeTableIdx];
	}
	return typeTableIdx;
}

inline u32 StripAllAliases(Context *context, u32 typeTableIdx)
{
	auto typeTable = context->typeTable.GetForRead();
	TypeInfo typeInfo = (*typeTable)[typeTableIdx];
	while (typeInfo.typeCategory == TYPECATEGORY_ALIAS)
	{
		typeTableIdx = typeInfo.aliasInfo.aliasedTypeIdx;
		typeInfo = (*typeTable)[typeTableIdx];
	}
	return typeTableIdx;
}

u32 GetTypeInfoPointerOf(Context *context, u32 inType);
TypeCheckErrorCode CheckTypesMatch(Context *context, u32 leftTableIdx, u32 rightTableIdx)
{
	static u32 voidPointerIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

	// Get rid of implicitly cast aliases
	leftTableIdx  = StripImplicitlyCastAliases(context, leftTableIdx);
	rightTableIdx = StripImplicitlyCastAliases(context, rightTableIdx);

	if ((leftTableIdx == TYPETABLEIDX_VOID) != (rightTableIdx == TYPETABLEIDX_VOID))
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;

	if (leftTableIdx == rightTableIdx)
		return TYPECHECK_COOL;

	// Allow anything to cast to Any
	if (leftTableIdx == TYPETABLEIDX_ANY_STRUCT || rightTableIdx == TYPETABLEIDX_ANY_STRUCT)
		return TYPECHECK_COOL;

	TypeInfo left  = GetTypeInfo(context, leftTableIdx);
	TypeInfo right = GetTypeInfo(context, rightTableIdx);

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
			left.typeCategory == TYPECATEGORY_POINTER || // @Check: Allow ptr = int? Confusing.
			left.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;

		// Allow implicit cast of <number> to <alias of number type>
		if (left.typeCategory == TYPECATEGORY_ALIAS)
		{
			u32 leftTableIdxStripped = StripAllAliases(context, leftTableIdx);
			TypeCategory strippedTypeCategory =
				GetTypeInfo(context, leftTableIdxStripped).typeCategory;
			if (strippedTypeCategory == TYPECATEGORY_INTEGER ||
				strippedTypeCategory == TYPECATEGORY_FLOATING)
				return TYPECHECK_COOL;
		}

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
		u32 pointedTypeIdxLeft  = StripImplicitlyCastAliases(context,
				left.pointerInfo.pointedTypeTableIdx);
		u32 pointedTypeIdxRight = StripImplicitlyCastAliases(context,
				right.pointerInfo.pointedTypeTableIdx);

		// Cast any pointer to void pointer
		if (pointedTypeIdxLeft == TYPETABLEIDX_VOID)
			return TYPECHECK_COOL;

		if (pointedTypeIdxLeft == pointedTypeIdxRight)
			return TYPECHECK_COOL;

		// Allow implicit ^[T] -> ^T
		TypeInfo pointedLeft  = GetTypeInfo(context, pointedTypeIdxLeft);
		TypeInfo pointedRight = GetTypeInfo(context, pointedTypeIdxRight);
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

		// Check pointed types
		TypeCheckErrorCode pointedTypesError = CheckTypesMatch(context,
				left.pointerInfo.pointedTypeTableIdx, right.pointerInfo.pointedTypeTableIdx);
		return pointedTypesError == TYPECHECK_COOL ? TYPECHECK_COOL : pointedTypesError;
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

	return TYPECHECK_MISC_PANIC;
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
			TypeInfo memberTypeInfo = GetTypeInfo(context, currentMember->typeTableIdx);
			ASSERT(memberTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
				   memberTypeInfo.typeCategory == TYPECATEGORY_UNION);
			const StructMember *found = FindStructMemberByName(context, memberTypeInfo, name);
			if (found)
			{
				if (currentMember->offset)
				{
					// Copy struct member and add the parent member's offset.
					StructMember *shiftedMember = (StructMember *)FrameAllocator::Alloc(sizeof(StructMember));
					memcpy(shiftedMember, found, sizeof(StructMember));
					shiftedMember->offset += currentMember->offset;
					return shiftedMember;
				}
				return found;
			}
		}
	}
	return nullptr;
}

struct TypeCheckResult
{
	TypeCheckErrorCode errorCode;
	u32 leftTableIdx;
	u32 rightTableIdx;
};
TypeCheckResult CheckTypesMatchAndSpecialize(Context *context, u32 leftTableIdx, const ASTExpression *rightHand)
{
	u32 rightTableIdx = rightHand->typeTableIdx;

	ASSERT(leftTableIdx  != TYPETABLEIDX_UNSET);
	ASSERT(rightTableIdx != TYPETABLEIDX_UNSET);

	// Get rid of aliases
	if (leftTableIdx >= TYPETABLEIDX_Begin)
		leftTableIdx  = StripImplicitlyCastAliases(context, leftTableIdx);
	if (rightTableIdx >= TYPETABLEIDX_Begin)
		rightTableIdx = StripImplicitlyCastAliases(context, rightTableIdx);

	TypeCheckResult result = { TYPECHECK_COOL, leftTableIdx, rightTableIdx };

	if (rightTableIdx == TYPETABLEIDX_STRUCT_LITERAL)
	{
		ASSERT(rightHand->nodeType == ASTNODETYPE_LITERAL);
		ASSERT(rightHand->literal.type == LITERALTYPE_GROUP);

		u32 structTypeIdx = leftTableIdx;
		TypeInfo structTypeInfo = GetTypeInfo(context, structTypeIdx);
		if (structTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			structTypeInfo.typeCategory == TYPECATEGORY_UNION)
		{
			struct StructStackFrame
			{
				u32 structTypeIdx;
				int idx;
			};
			DynamicArray<StructStackFrame, PhaseAllocator> structStack;
			DynamicArrayInit(&structStack, 8);
			*DynamicArrayAdd(&structStack) = { structTypeIdx, 0 };

			int memberIdx = 0;
			while (memberIdx < rightHand->literal.members.size)
			{
				StructStackFrame currentFrame = structStack[structStack.size - 1];
				TypeInfo currentStructTypeInfo = GetTypeInfo(context, currentFrame.structTypeIdx);

				if (currentFrame.idx >= currentStructTypeInfo.structInfo.members.size)
				{
					// Pop struct frame
					--structStack.size;
					if (structStack.size == 0)
						LogError(context, rightHand->any.loc, "Too many values in struct literal"_s);
					continue;
				}

				u32 currentMemberTypeIdx =
					currentStructTypeInfo.structInfo.members[currentFrame.idx].typeTableIdx;
				TypeInfo currentMemberTypeInfo = GetTypeInfo(context, currentMemberTypeIdx);

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

	u32 strippedLeftTypeIdx  = StripAllAliases(context, leftTableIdx);
	u32 strippedRightTypeIdx = StripAllAliases(context, rightTableIdx);
	TypeCategory strippedLeftTypeCat  = GetTypeInfo(context, strippedLeftTypeIdx).typeCategory;
	TypeCategory strippedRightTypeCat = GetTypeInfo(context, strippedRightTypeIdx).typeCategory;

	if (leftTableIdx == TYPETABLEIDX_INTEGER && (strippedRightTypeCat == TYPECATEGORY_INTEGER ||
		strippedRightTypeCat == TYPECATEGORY_POINTER || strippedRightTypeCat == TYPECATEGORY_FLOATING ||
		strippedRightTypeCat == TYPECATEGORY_ALIAS))
	{
		result.leftTableIdx = rightTableIdx;
		return result;
	}
	if (rightTableIdx == TYPETABLEIDX_INTEGER && (strippedLeftTypeCat == TYPECATEGORY_INTEGER ||
		strippedLeftTypeCat == TYPECATEGORY_POINTER || strippedLeftTypeCat == TYPECATEGORY_FLOATING ||
		strippedLeftTypeCat == TYPECATEGORY_ALIAS))
	{
		result.rightTableIdx = leftTableIdx;
		return result;
	}

	if (leftTableIdx == TYPETABLEIDX_FLOATING && strippedRightTypeCat == TYPECATEGORY_FLOATING)
	{
		result.leftTableIdx = rightTableIdx;
		return result;
	}
	if (rightTableIdx == TYPETABLEIDX_FLOATING && strippedLeftTypeCat == TYPECATEGORY_FLOATING)
	{
		result.rightTableIdx = leftTableIdx;
		return result;
	}

	result.errorCode = CheckTypesMatch(context, leftTableIdx, rightTableIdx);
	return result;
}

// NOTE!! This procedure assumes typeTable is locked!
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
	{
		if (a.procedureInfo.callingConvention != b.procedureInfo.callingConvention)
			return false;
		if (a.procedureInfo.parameters.size != b.procedureInfo.parameters.size)
			return false;
		if (a.procedureInfo.isVarargs != b.procedureInfo.isVarargs)
			return false;
		TypeInfo aReturnTypeInfo = context->typeTable.content[a.procedureInfo.returnTypeTableIdx];
		TypeInfo bReturnTypeInfo = context->typeTable.content[b.procedureInfo.returnTypeTableIdx];
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
			TypeInfo aParamTypeInfo = context->typeTable.content[aParam.typeTableIdx];
			TypeInfo bParamTypeInfo = context->typeTable.content[bParam.typeTableIdx];
			if (!AreTypeInfosEqual(context, aParamTypeInfo, bParamTypeInfo))
				return false;
		}
		return true;
	}
	case TYPECATEGORY_ALIAS:
	{
		if (!a.aliasInfo.doesImplicitlyCast || !b.aliasInfo.doesImplicitlyCast)
			return false;
		return a.aliasInfo.aliasedTypeIdx == b.aliasInfo.aliasedTypeIdx;
	}
	case TYPECATEGORY_INVALID:
		return b.typeCategory == TYPECATEGORY_INVALID;
	default:
		CRASH;
	}
}

inline u32 AddType(Context *context, TypeInfo typeInfo)
{
	typeInfo.valueIdx = NewGlobalValue(context, String{}, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);

	u32 typeTableIdx;
	{
		auto *typeTable = &context->typeTable.content;

		s64 typeCount = BucketArrayCount(typeTable);
		ASSERT(typeCount < U32_MAX); // Out of type memory
		typeTableIdx = (u32)typeCount;

		*(TypeInfo *)BucketArrayAdd(typeTable) = typeInfo;
	}
	{
		Value value = GetGlobalValue(context, typeInfo.valueIdx);
		value.name = TPrintF("_typeInfo%lld", typeTableIdx);
		UpdateGlobalValue(context, typeInfo.valueIdx, &value);
	}

	return typeTableIdx;
}

u32 FindOrAddTypeTableIdx(Context *context, TypeInfo typeInfo)
{
	{
		auto typeTable = context->typeTable.GetForRead();

		u32 tableSize = (u32)BucketArrayCount(&typeTable);
		for (u32 i = 0; i < tableSize; ++i)
		{
			TypeInfo t = (*typeTable)[i];
			if (AreTypeInfosEqual(context, typeInfo, t))
				return i;
		}
	}
	{
		// Check it didn't get added when we released the lock
		// @Speed: ugh...
		auto typeTable = context->typeTable.GetForWrite();

		u32 tableSize = (u32)BucketArrayCount(&typeTable);
		for (u32 i = 0; i < tableSize; ++i)
		{
			TypeInfo t = (*typeTable)[i];
			if (AreTypeInfosEqual(context, typeInfo, t))
				return i;
		}
		return AddType(context, typeInfo);
	}
}

// Util TypeInfo procedures
u32 GetTypeInfoPointerOf(Context *context, u32 inType)
{
#if DEBUG_BUILD
	{
		auto typeTable = context->typeTable.GetForRead();
		ASSERT(inType < (u32)BucketArrayCount(&typeTable));
	}
#endif

	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_POINTER;
	resultTypeInfo.size = g_pointerSize;
	resultTypeInfo.pointerInfo.pointedTypeTableIdx = inType;
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

u32 GetTypeInfoArrayOf(Context *context, u32 inType, s64 count)
{
	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_ARRAY;
	resultTypeInfo.arrayInfo.elementTypeTableIdx = inType;
	resultTypeInfo.arrayInfo.count = count;
	if (count == 0)
		resultTypeInfo.size = 8 + g_pointerSize;
	else
	{
		s64 elementSize = GetTypeInfo(context, inType).size;
		resultTypeInfo.size = elementSize * count;
	}
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

u32 TypeCheckType(Context *context, TCJob *job, String name, SourceLocation loc,
		ASTType *astType);

u32 TypeCheckStructDeclaration(Context *context, TCJob *job, String name, bool isUnion,
		ASTStructDeclaration astStructDecl)
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

		astMember->typeTableIdx = TypeCheckType(context, job, {}, astMember->loc,
				astMember->astType);
	}

	TypeInfo t = {};
	t.typeCategory = isUnion ? TYPECATEGORY_UNION : TYPECATEGORY_STRUCT;
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

		u64 memberSize = GetTypeInfo(context, member.typeTableIdx).size;
		int alignment = 8;
		if (memberSize < 8)
			alignment = NextPowerOf2((int)memberSize);

		if (alignment > largestAlignment)
			largestAlignment = alignment;

		if (!isUnion)
		{
			// Struct
			if (t.size & (alignment - 1))
				t.size = (t.size & ~(alignment - 1)) + alignment;
			member.offset = t.size;
			t.size += memberSize;
		}
		else
		{
			// Union
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

	u32 typeTableIdx;
	{
		auto typeTable = context->typeTable.GetForWrite();
		if (StringEquals(name, "String"_s))
		{
			typeTableIdx = TYPETABLEIDX_STRING_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_STRING_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_STRING_STRUCT] = t;
		}
		else if (StringEquals(name, "Array"_s))
		{
			typeTableIdx = TYPETABLEIDX_ARRAY_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_ARRAY_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_ARRAY_STRUCT] = t;
		}
		else if (StringEquals(name, "Any"_s))
		{
			typeTableIdx = TYPETABLEIDX_ANY_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_ANY_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_ANY_STRUCT] = t;
		}
		else if (StringEquals(name, "TypeInfo"_s))
		{
			typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_TYPE_INFO_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_TYPE_INFO_STRUCT] = t;
		}
		else if (StringEquals(name, "TypeInfoInteger"_s))
		{
			typeTableIdx = TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT] = t;
		}
		else if (StringEquals(name, "TypeInfoStructMember"_s))
		{
			typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT] = t;
		}
		else if (StringEquals(name, "TypeInfoStruct"_s))
		{
			typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT] = t;
		}
		else if (StringEquals(name, "TypeInfoEnum"_s))
		{
			typeTableIdx = TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT] = t;
		}
		else if (StringEquals(name, "TypeInfoPointer"_s))
		{
			typeTableIdx = TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT] = t;
		}
		else if (StringEquals(name, "TypeInfoArray"_s))
		{
			typeTableIdx = TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT;
			t.valueIdx = (*typeTable)[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT].valueIdx;
			(TypeInfo&)(*typeTable)[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT] = t;
		}
		else
		{
			ASSERT(BucketArrayCount(&context->typeTable.content) < U32_MAX);
			typeTableIdx = AddType(context, t);
		}
	}

	TCScope *stackTop = GetTopMostScope(context, job);
	if (stackTop)
		*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;
	else
	{
		auto globalScope = context->tcGlobalScope.GetForWrite();
		*DynamicArrayAdd(&globalScope->typeIndices) = typeTableIdx;
	}
	return typeTableIdx;
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
			result = GetStaticDefinition(context,
					expression->identifier.staticDefinitionIdx).constant;
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
		case TOKEN_OP_EQUALS:
		{
			result.type = CONSTANTTYPE_INTEGER;
			if (isFloat)
				result.valueAsInt = leftValue.valueAsFloat == rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt == rightValue.valueAsInt;
		} break;
		case TOKEN_OP_LESS_THAN:
		{
			result.type = CONSTANTTYPE_INTEGER;
			if (isFloat)
				result.valueAsInt = leftValue.valueAsFloat < rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt < rightValue.valueAsInt;
		} break;
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
		{
			result.type = CONSTANTTYPE_INTEGER;
			if (isFloat)
				result.valueAsInt = leftValue.valueAsFloat <= rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt <= rightValue.valueAsInt;
		} break;
		case TOKEN_OP_GREATER_THAN:
		{
			result.type = CONSTANTTYPE_INTEGER;
			if (isFloat)
				result.valueAsInt = leftValue.valueAsFloat > rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt > rightValue.valueAsInt;
		} break;
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
		{
			result.type = CONSTANTTYPE_INTEGER;
			if (isFloat)
				result.valueAsInt = leftValue.valueAsFloat >= rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt >= rightValue.valueAsInt;
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
	case ASTNODETYPE_CAST:
	{
		Constant constant = TryEvaluateConstant(context, expression->castNode.expression);
		bool isFloat = constant.type == CONSTANTTYPE_FLOATING;
		bool castToFloat = GetTypeInfo(context, expression->typeTableIdx).typeCategory ==
			TYPECATEGORY_FLOATING;
		if (!isFloat && castToFloat)
			result.valueAsFloat = (f64)constant.valueAsInt;
		else if (isFloat && !castToFloat)
			result.valueAsInt = (s64)constant.valueAsFloat;
		else
			result = constant;
		result.type = castToFloat ? CONSTANTTYPE_FLOATING : CONSTANTTYPE_INTEGER;
	} break;
	case ASTNODETYPE_DEFINED:
	{
		result.type = CONSTANTTYPE_INTEGER;
		result.valueAsInt = expression->definedNode.isDefined;
	} break;
	default:
		goto error;
	}
	return result;
error:
	return { CONSTANTTYPE_INVALID };
}

inline void TCAddScopeName(Context *context, TCJob *job, TCScopeName scopeName)
{
	TCScope *stackTop = GetTopMostScope(context, job);
	if (stackTop)
		*DynamicArrayAdd(&stackTop->names) = scopeName;
	else
	{
		{
			auto globalScope = context->tcGlobalScope.GetForWrite();
			*DynamicArrayAdd(&globalScope->names) = scopeName;
		}

		CONDITION_VARIABLE *conditionVar;
		{
			auto conditionVariables = context->tcConditionVariables.GetForWrite();
			conditionVar = HashMapGet(*conditionVariables, scopeName.name);
			if (conditionVar)
				HashMapRemove(&conditionVariables, scopeName.name);
		}
		if (conditionVar)
			WakeAllConditionVariable(conditionVar);
	}
}

void TryTypeCheckExpression(Context *context, TCJob *job, ASTExpression *expression);
bool TypeCheckProcedurePrototype(Context *context, TCJob *job, ASTProcedurePrototype *prototype);
TypeInfo TypeInfoFromASTProcedurePrototype(Context *context, ASTProcedurePrototype *prototype);
u32 TypeCheckType(Context *context, TCJob *job, String name, SourceLocation loc,
		ASTType *astType)
{
	switch (astType->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
	{
		return FindTypeInStackByName(context, job, astType->loc, astType->name);
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		u32 elementTypeIdx = TypeCheckType(context, job, {}, loc, astType->arrayType);
		return GetTypeInfoArrayOf(context, elementTypeIdx, astType->arrayCount);
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		u32 pointedTypeIdx = TypeCheckType(context, job, {}, loc, astType->pointedType);
		return GetTypeInfoPointerOf(context, pointedTypeIdx);
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	{
		return TypeCheckStructDeclaration(context, job, name, false, astType->structDeclaration);
	} break;
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		return TypeCheckStructDeclaration(context, job, name, true, astType->structDeclaration);
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		u32 innerTypeIdx = TYPETABLEIDX_S64;
		if (astType->enumDeclaration.astType)
		{
			SourceLocation astTypeLoc = astType->enumDeclaration.astType->loc;
			innerTypeIdx = TypeCheckType(context, job, {}, astTypeLoc,
					astType->enumDeclaration.astType);

			if (innerTypeIdx < TYPETABLEIDX_PRIMITIVE_BEGIN ||
				innerTypeIdx > TYPETABLEIDX_PRIMITIVE_END)
				LogError(context, astTypeLoc, "Only primitive types are allowed as enum field types"_s);
		}

		for (int memberIdx = 0; memberIdx < astType->enumDeclaration.members.size; ++memberIdx)
		{
			ASTExpression *memberValue = astType->enumDeclaration.members[memberIdx].value;
			if (memberValue)
				TryTypeCheckExpression(context, job, memberValue);
		}

		TypeInfo t;
		t.typeCategory = TYPECATEGORY_ENUM;
		t.enumInfo.name = name;
		t.enumInfo.typeTableIdx = innerTypeIdx;
		t.size = GetTypeInfo(context, innerTypeIdx).size;

		DynamicArray<String, FrameAllocator> enumNames;
		DynamicArray<s64, FrameAllocator> enumValues;
		DynamicArrayInit(&enumNames, 16);
		DynamicArrayInit(&enumValues, 16);

		Array<u32, FrameAllocator> valueStaticDefs;
		ArrayInit(&valueStaticDefs, astType->enumDeclaration.members.size);

		s64 currentValue = 0;
		for (int memberIdx = 0; memberIdx < astType->enumDeclaration.members.size; ++memberIdx)
		{
			ASTEnumMember astMember = astType->enumDeclaration.members[memberIdx];

			StaticDefinition staticDefinition = {};
			staticDefinition.name = astMember.name;
			staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT;
			staticDefinition.typeTableIdx = TYPETABLEIDX_UNSET;

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
			staticDefinition.constant.typeTableIdx = TYPETABLEIDX_UNSET;

			u32 newStaticDefIdx = TCNewStaticDefinition(context, &staticDefinition);
			*ArrayAdd(&valueStaticDefs) = newStaticDefIdx;

			TCScopeName newName;
			newName.type = NAMETYPE_STATIC_DEFINITION;
			newName.name = astMember.name;
			newName.staticDefinitionIdx = newStaticDefIdx;
			newName.loc = astMember.loc;
			TCAddScopeName(context, job, newName);

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

		u32 typeTableIdx;
		{
			auto typeTableLock = context->typeTable.GetForWrite();
			typeTableIdx = AddType(context, t);
		}

		// Now that the type exists, assign it to the static definitions of the enum values.
		for (int staticDefIdx = 0; staticDefIdx < valueStaticDefs.size; ++staticDefIdx)
		{
			u32 staticDef = valueStaticDefs[staticDefIdx];
			StaticDefinition staticDefinition = GetStaticDefinition(context, staticDef);
			staticDefinition.typeTableIdx = typeTableIdx;
			staticDefinition.constant.typeTableIdx = typeTableIdx;
			TCUpdateStaticDefinition(context, job, staticDef, &staticDefinition);
		}

		TCScope *stackTop = GetTopMostScope(context, job);
		if (stackTop)
			*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;
		else
		{
			auto globalScope = context->tcGlobalScope.GetForWrite();
			*DynamicArrayAdd(&globalScope->typeIndices) = typeTableIdx;
		}

		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		TypeCheckProcedurePrototype(context, job, &astType->procedurePrototype);

		TypeInfo t = TypeInfoFromASTProcedurePrototype(context, &astType->procedurePrototype);
		u32 typeTableIdx = FindOrAddTypeTableIdx(context, t);

		return typeTableIdx;
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

u32 InferType(u32 fromType)
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

void AddStructMembersToScope(Context *context, TCJob *job, SourceLocation loc, ASTExpression *valueExpression)
{
	// This procedure spills all members of a struct expression into the current stack.
	// The way we do this is, we add a TCScopeName which has a member access AST node, left hand of
	// which is an arbitrary AST tree branch that evaluates to a struct; and of which right hand is
	// a simple struct member node.
	TypeInfo typeInfo = GetTypeInfo(context, valueExpression->typeTableIdx);
	ASSERT(typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		   typeInfo.typeCategory == TYPECATEGORY_UNION);

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
			TCCheckIfNameAlreadyExists(context, job, member->name, loc);

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_ASTEXPRESSION;
			newScopeName.name = member->name;
			newScopeName.expression = memberAccessExp;
			newScopeName.loc = loc;
			TCAddScopeName(context, job, newScopeName);
		}
		else
		{
			// For using/anonymous members we recurse, spilling it's members too.
			AddStructMembersToScope(context, job, loc, memberAccessExp);
		}
	}
}

void TypeCheckVariableDeclaration(Context *context, TCJob *job, ASTVariableDeclaration *varDecl)
{
	if (varDecl->name.size)
		TCCheckIfNameAlreadyExists(context, job, varDecl->name, varDecl->loc);

	if (varDecl->astType)
	{
		varDecl->typeTableIdx = TypeCheckType(context, job, {}, varDecl->loc, varDecl->astType);

		if (varDecl->typeTableIdx == TYPETABLEIDX_VOID)
			LogError(context, varDecl->loc, "Variable can't be of type void!"_s);
	}

	if (varDecl->astInitialValue)
	{
		TryTypeCheckExpression(context, job, varDecl->astInitialValue);

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
}

void TypeCheckProcedureParameter(Context *context, TCJob *job, ASTProcedureParameter *astParam)
{
	if (astParam->name.size)
		TCCheckIfNameAlreadyExists(context, job, astParam->name, astParam->loc);

	if (astParam->astType)
	{
		astParam->typeTableIdx = TypeCheckType(context, job, {}, astParam->loc, astParam->astType);

		if (astParam->typeTableIdx == TYPETABLEIDX_VOID)
			LogError(context, astParam->loc, "Variable can't be of type void!"_s);
	}

	if (astParam->astInitialValue)
	{
		TryTypeCheckExpression(context, job, astParam->astInitialValue);

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

bool TypeCheckProcedurePrototype(Context *context, TCJob *job, ASTProcedurePrototype *prototype)
{
	// Parameters
	bool beginOptionalParameters = false;
	for (int i = 0; i < prototype->astParameters.size; ++i)
	{
		TypeCheckProcedureParameter(context, job, &prototype->astParameters[i]);

		ASTProcedureParameter astParameter = prototype->astParameters[i];
		if (!astParameter.astInitialValue)
		{
			if (beginOptionalParameters)
				LogError(context, astParameter.loc,
						"Non-optional parameter after optional parameter found!"_s);
		}
		else
			beginOptionalParameters = true;
	}

	prototype->returnTypeIdx = TYPETABLEIDX_VOID;
	if (prototype->astReturnType)
	{
		prototype->returnTypeIdx = TypeCheckType(context, job, {}, prototype->loc,
				prototype->astReturnType);
	}

	return { true };
}

TypeInfo TypeInfoFromASTProcedurePrototype(Context *context, ASTProcedurePrototype *prototype)
{
	TypeInfo t = {};
	t.size = g_pointerSize;
	t.typeCategory = TYPECATEGORY_PROCEDURE;
	t.procedureInfo.isVarargs = prototype->isVarargs;
	t.procedureInfo.callingConvention = prototype->callingConvention;
	t.procedureInfo.returnTypeTableIdx = prototype->returnTypeIdx;

	u64 astParametersCount = prototype->astParameters.size;
	if (astParametersCount)
	{
		DynamicArray<ProcedureParameter, FrameAllocator> parameters;
		DynamicArrayInit(&parameters, astParametersCount);

		// Parameters
		for (int i = 0; i < astParametersCount; ++i)
		{
			ASTProcedureParameter astParameter = prototype->astParameters[i];

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

ASTExpression InlineProcedureCopyTreeBranch(Context *context, TCJob *job,
		const ASTExpression *expression)
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

		PushTCScope(context, job);

		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			ASTExpression statement = InlineProcedureCopyTreeBranch(context, job,
					&expression->block.statements[i]);
			if (statement.nodeType != ASTNODETYPE_INVALID)
				*DynamicArrayAdd(&astBlock.statements) = statement;
		}

		PopTCScope(context, job);

		result.block = astBlock;
		return result;
	}
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		if (!varDecl.isStatic && !varDecl.isExternal)
			varDecl.valueIdx = TCNewValue(context, job, varDecl.name, varDecl.typeTableIdx,
					TCGetValue(context, job, varDecl.valueIdx)->flags);
		else
			varDecl.valueIdx = NewGlobalValue(context, varDecl.name, varDecl.typeTableIdx,
					TCGetValue(context, job, varDecl.valueIdx)->flags);

		if (varDecl.name.size)
		{
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = varDecl.name;
			newScopeName.variableInfo.valueIdx = varDecl.valueIdx;
			newScopeName.variableInfo.typeTableIdx = varDecl.typeTableIdx;
			newScopeName.loc = varDecl.loc;
			TCAddScopeName(context, job, newScopeName);
		}

		if (varDecl.name.size == 0)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = varDecl.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.valueIdx = varDecl.valueIdx;
				*varExp = e;
			}
			AddStructMembersToScope(context, job, varDecl.loc, varExp);
		}

		if (varDecl.astInitialValue)
		{
			ASTExpression *astInitialValue = NewTreeNode(context);
			*astInitialValue = InlineProcedureCopyTreeBranch(context, job, varDecl.astInitialValue);
			varDecl.astInitialValue = astInitialValue;
		}

		result.variableDeclaration = varDecl;
		return result;
	}
	case ASTNODETYPE_IDENTIFIER:
	{
		ASTIdentifier astIdentifier = expression->identifier;

		TCScopeName scopeName = TCFindScopeName(context, job, astIdentifier.string);

		astIdentifier.type = scopeName.type;
		switch (scopeName.type)
		{
		case NAMETYPE_VARIABLE:
		{
			astIdentifier.valueIdx = scopeName.variableInfo.valueIdx;
		} break;
		case NAMETYPE_STRUCT_MEMBER:
		{
			astIdentifier.structMember = scopeName.structMember;
		} break;
		case NAMETYPE_ASTEXPRESSION:
		{
			astIdentifier.expression = scopeName.expression;
		} break;
		case NAMETYPE_STATIC_DEFINITION:
		{
			astIdentifier.staticDefinitionIdx = scopeName.staticDefinitionIdx;
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
		TCScopeName staticDefScopeName;
		staticDefScopeName.type = NAMETYPE_STATIC_DEFINITION;
		staticDefScopeName.name = astStaticDef.name;
		staticDefScopeName.staticDefinitionIdx = expression->staticDefinition.staticDefinitionIdx;
		staticDefScopeName.loc = astStaticDef.loc;
		TCAddScopeName(context, job, staticDefScopeName);

		return {};
	}
	case ASTNODETYPE_RETURN:
	{
		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, expression->returnNode.expression);
		result.returnNode.expression = e;
		return result;
	}
	case ASTNODETYPE_DEFER:
	{
		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, expression->deferNode.expression);
		result.deferNode.expression = e;
		return result;
	}
	case ASTNODETYPE_USING:
	{
		ASTExpression *usingExp = NewTreeNode(context);
		*usingExp = InlineProcedureCopyTreeBranch(context, job, expression->usingNode.expression);

		if (usingExp->nodeType == ASTNODETYPE_VARIABLE_DECLARATION)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = usingExp->variableDeclaration.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.valueIdx = usingExp->variableDeclaration.valueIdx;
				*varExp = e;
			}
			AddStructMembersToScope(context, job, usingExp->any.loc, varExp);
		}
		else
			AddStructMembersToScope(context, job, usingExp->any.loc, usingExp);

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
				InlineProcedureCopyTreeBranch(context, job, &original.arguments[argIdx]);
		}

		if (astProcCall.callType == CALLTYPE_VALUE)
		{
			TCScopeName scopeName = TCFindScopeName(context, job, astProcCall.name);
			ASSERT(scopeName.type == NAMETYPE_VARIABLE);
			astProcCall.valueIdx = scopeName.variableInfo.valueIdx;
		}
		else if (astProcCall.callType == CALLTYPE_ASTEXPRESSION)
		{
			TCScopeName scopeName = TCFindScopeName(context, job, astProcCall.name);
			ASSERT(scopeName.type == NAMETYPE_ASTEXPRESSION);
			astProcCall.expression = scopeName.expression;
		}

		result.procedureCall = astProcCall;
		return result;
	}
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTUnaryOperation astUnary = expression->unaryOperation;
		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, expression->unaryOperation.expression);
		astUnary.expression = e;
		result.unaryOperation = astUnary;
		return result;
	}
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTBinaryOperation astBinary = expression->binaryOperation;
		ASTExpression *l = NewTreeNode(context);
		ASTExpression *r = NewTreeNode(context);
		*l = InlineProcedureCopyTreeBranch(context, job, expression->binaryOperation.leftHand);

		// For member access we can just copy
		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
			*r = *expression->binaryOperation.rightHand;
		else
			*r = InlineProcedureCopyTreeBranch(context, job, expression->binaryOperation.rightHand);

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
				*e = InlineProcedureCopyTreeBranch(context, job,
						expression->literal.members[memberIdx]);
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
		*e = InlineProcedureCopyTreeBranch(context, job, expression->ifNode.condition);
		astIf.condition = e;

		e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, expression->ifNode.body);
		astIf.body = e;

		if (expression->ifNode.elseBody)
		{
			e = NewTreeNode(context);
			*e = InlineProcedureCopyTreeBranch(context, job, expression->ifNode.elseBody);
			astIf.elseBody = e;
		}

		result.ifNode = astIf;
		return result;
	}
	case ASTNODETYPE_WHILE:
	{
		ASTWhile astWhile = expression->whileNode;

		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, expression->whileNode.condition);
		astWhile.condition = e;

		e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, expression->whileNode.body);
		astWhile.body = e;

		result.whileNode = astWhile;
		return result;
	}
	case ASTNODETYPE_FOR:
	{
		ASTFor astFor = expression->forNode;

		ASTExpression *e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, astFor.range);
		astFor.range = e;

		u32 oldForArray = job->currentForLoopArrayType;
		job->currentForLoopArrayType = TYPETABLEIDX_UNSET;

		PushTCScope(context, job);

		String indexValueName = "i"_s;
		u32 indexValueIdx = TCNewValue(context, job, indexValueName, TYPETABLEIDX_S64, 0);
		astFor.indexValueIdx = indexValueIdx;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = indexValueName;
		newScopeName.variableInfo.valueIdx = indexValueIdx;
		newScopeName.variableInfo.typeTableIdx = TYPETABLEIDX_S64;
		newScopeName.loc = expression->any.loc;
		TCAddScopeName(context, job, newScopeName);

		ASTExpression *rangeExp = astFor.range;
		bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			rangeExp->binaryOperation.op == TOKEN_OP_RANGE;
		if (!isExplicitRange)
		{
			job->currentForLoopArrayType = rangeExp->typeTableIdx;

			u32 origValueTypeIdx = TCGetValue(context, job, astFor.elementValueIdx)->typeTableIdx;
			String elementValueName = "it"_s;
			u32 elementValueIdx = TCNewValue(context, job, elementValueName, origValueTypeIdx, 0);
			astFor.elementValueIdx = elementValueIdx;

			newScopeName.name = elementValueName;
			newScopeName.variableInfo.valueIdx = elementValueIdx;
			newScopeName.variableInfo.typeTableIdx = origValueTypeIdx;
			newScopeName.loc = expression->any.loc;
			TCAddScopeName(context, job, newScopeName);
		}

		e = NewTreeNode(context);
		*e = InlineProcedureCopyTreeBranch(context, job, expression->forNode.body);
		astFor.body = e;

		PopTCScope(context, job);

		job->currentForLoopArrayType = oldForArray;

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
		*e = InlineProcedureCopyTreeBranch(context, job, expression->castNode.expression);
		result.castNode.expression = e;
		return result;
	}
	case ASTNODETYPE_INTRINSIC:
	{
		ASTIntrinsic astIntrinsic = expression->intrinsic;
		u64 argCount = expression->intrinsic.arguments.size;
		if (argCount)
		{
			DynamicArrayInit(&astIntrinsic.arguments, argCount);
			for (int argIdx = 0; argIdx < argCount; ++argIdx)
			{
				*DynamicArrayAdd(&astIntrinsic.arguments) = InlineProcedureCopyTreeBranch(context,
						job, &expression->intrinsic.arguments[argIdx]);
			}
		}
		result.intrinsic = astIntrinsic;
		return result;
	}
	default:
		ASSERT(false);
	}
}

void TCAddParametersToScope(Context *context, TCJob *job, ArrayView<u32> parameterValues,
		const ASTProcedurePrototype *prototype)
{
	for (int i = 0; i < prototype->astParameters.size; ++i)
	{
		ASTProcedureParameter astParameter = prototype->astParameters[i];
		u32 paramValueIdx = parameterValues[i];

		if (astParameter.isUsing)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = astParameter.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.valueIdx = paramValueIdx;
				*varExp = e;
			}
			AddStructMembersToScope(context, job, astParameter.loc, varExp);
		}

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = astParameter.name;
		newScopeName.variableInfo.valueIdx = paramValueIdx;
		newScopeName.variableInfo.typeTableIdx = astParameter.typeTableIdx;
		newScopeName.loc = astParameter.loc;
		TCAddScopeName(context, job, newScopeName);
	}

	// Varargs array
	if (prototype->isVarargs)
	{
		static u32 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

		u32 paramValueIdx = { parameterValues[prototype->astParameters.size] };

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = prototype->varargsName;
		newScopeName.variableInfo.valueIdx = paramValueIdx;
		newScopeName.variableInfo.typeTableIdx = arrayTableIdx;
		newScopeName.loc = prototype->varargsLoc;
		TCAddScopeName(context, job, newScopeName);
	}
}

s32 NewProcedure(Context *context, Procedure p, bool isExternal)
{
	s32 procedureIdx;
	if (!isExternal)
	{
		auto procedures = context->procedures.GetForWrite();
		procedureIdx = (s32)BucketArrayCount(&procedures);
		*BucketArrayAdd(&procedures) = p;
	}
	else
	{
		auto externalProcedures = context->externalProcedures.GetForWrite();
		procedureIdx = -(s32)BucketArrayCount(&externalProcedures);
		*BucketArrayAdd(&externalProcedures) = p;
	}
	return procedureIdx;
}

bool IsExpressionAType(Context *context, TCJob *job, ASTExpression *expression)
{
	switch (expression->nodeType)
	{
	case ASTNODETYPE_TYPE:
		return true;
	case ASTNODETYPE_IDENTIFIER:
		switch (expression->identifier.type)
		{
		case NAMETYPE_STATIC_DEFINITION:
		{
			u32 defIdx = expression->identifier.staticDefinitionIdx;
			StaticDefinition staticDef = TCGetStaticDefinition(context, job, defIdx, false);
			return staticDef.definitionType == STATICDEFINITIONTYPE_TYPE;
		}
		case NAMETYPE_PRIMITIVE:
			return true;
		default:
			return false;
		}
	default:
		return false;
	}
}

bool TCPushParametersAndInlineProcedureCall(Context *context, TCJob *job,
		ASTProcedureCall *astProcCall)
{
	if (astProcCall->callType != CALLTYPE_STATIC)
		return false;

	Procedure proc = GetProcedureRead(context, astProcCall->procedureIdx);

	if (!proc.isInline)
		return false;

	ASSERT(GetTypeInfo(context, proc.typeTableIdx).typeCategory == TYPECATEGORY_PROCEDURE);
	TypeInfoProcedure procTypeInfo = GetTypeInfo(context, proc.typeTableIdx).procedureInfo;

	PushTCScope(context, job);

	s64 totalArguments = procTypeInfo.parameters.size;
	ArrayInit(&astProcCall->inlineParameterValues, totalArguments + 1);

	for (int argIdx = 0; argIdx < totalArguments; ++argIdx)
	{
		u32 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
		u32 newValueIdx = TCNewValue(context, job, TPrintF("_inlinearg%d", argIdx),
				paramTypeIdx, 0);
		*ArrayAdd(&astProcCall->inlineParameterValues) = newValueIdx;
	}
	if (procTypeInfo.isVarargs)
	{
		static u32 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);
		u32 newValueIdx = TCNewValue(context, job, "_inlinevarargs"_s, arrayTableIdx, 0);
		*ArrayAdd(&astProcCall->inlineParameterValues) = newValueIdx;
	}

	TCAddParametersToScope(context, job, astProcCall->inlineParameterValues, &proc.astPrototype);

	ASTExpression *e = NewTreeNode(context);
	*e = InlineProcedureCopyTreeBranch(context, job, proc.astBody);
	astProcCall->astBodyInlineCopy = e;

	PopTCScope(context, job);

	return true;
}

String OperatorToString(s32 op);
bool LookForOperatorOverload(Context *context, TCJob *job, ASTExpression *expression)
{
	OperatorOverload overload = {};
	bool foundOverload = false;

	enum TokenType op;
	int paramCount;
	ASTExpression *leftHand = nullptr;
	ASTExpression *rightHand = nullptr;
	if (expression->nodeType == ASTNODETYPE_UNARY_OPERATION)
	{
		op = expression->unaryOperation.op;
		paramCount = 1;
		leftHand = expression->unaryOperation.expression;
	}
	else
	{
		op = expression->binaryOperation.op;
		paramCount = 2;
		leftHand = expression->binaryOperation.leftHand;
		rightHand = expression->binaryOperation.rightHand;
	}

	{
		auto operatorOverloads = context->operatorOverloads.GetForRead();
		for (int overloadIdx = 0; overloadIdx < operatorOverloads->size; ++overloadIdx)
		{
			OperatorOverload currentOverload = (*operatorOverloads)[overloadIdx];

			if (op != currentOverload.op)
				continue;

			Procedure procedure = GetProcedureRead(context, currentOverload.procedureIdx);
			TypeInfo procType = GetTypeInfo(context, procedure.typeTableIdx);
			ASSERT(procType.typeCategory == TYPECATEGORY_PROCEDURE);

			if (paramCount == 1)
			{
				if (procType.procedureInfo.parameters.size != 1)
					continue;

				u32 leftHandTypeIdx  = procType.procedureInfo.parameters[0].typeTableIdx;

				if (CheckTypesMatch(context, leftHand->typeTableIdx, leftHandTypeIdx) == TYPECHECK_COOL)
				{
					if (foundOverload)
						LogError(context, expression->any.loc,
								TPrintF("Multiple overloads found for operator %S with operand of "
									"type %S",
									OperatorToString(op),
									TypeInfoToString(context, leftHand->typeTableIdx)));
					overload = currentOverload;
					foundOverload = true;
				}
			}
			else
			{
				if (procType.procedureInfo.parameters.size != 2)
					continue;

				u32 leftHandTypeIdx  = procType.procedureInfo.parameters[0].typeTableIdx;
				u32 rightHandTypeIdx = procType.procedureInfo.parameters[1].typeTableIdx;

				if (CheckTypesMatch(context, leftHand->typeTableIdx, leftHandTypeIdx) ==
						TYPECHECK_COOL &&
					CheckTypesMatch(context, rightHand->typeTableIdx, rightHandTypeIdx) ==
						TYPECHECK_COOL)
				{
					if (foundOverload)
						LogError(context, expression->any.loc,
								TPrintF("Multiple overloads found for operator %S with left hand "
									"of type %S and right hand of type %S",
									OperatorToString(op),
									TypeInfoToString(context, leftHand->typeTableIdx),
									TypeInfoToString(context, rightHand->typeTableIdx)));
					overload = currentOverload;
					foundOverload = true;
				}
			}
		}
	}

	TypeInfo leftTypeInfo = GetTypeInfo(context, leftHand->typeTableIdx);

	if (foundOverload)
	{
		Procedure proc = GetProcedureRead(context, overload.procedureIdx);

		TypeInfo procTypeInfo = GetTypeInfo(context, proc.typeTableIdx);
		ASSERT(procTypeInfo.typeCategory == TYPECATEGORY_PROCEDURE);

		if (proc.isInline) while (true)
		{
			if (proc.isBodyTypeChecked)
				break;
			if (!TCYield(context, job))
				LogError(context, expression->any.loc, TPrintF("COMPILER ERROR! Body of inline "
						"procedure \"%S\" for operator overload never type checked", proc.name));
			proc = GetProcedureRead(context, overload.procedureIdx);
		}

		ASTProcedureCall astProcCall = {};
		astProcCall.callType = CALLTYPE_STATIC;
		astProcCall.procedureIdx = overload.procedureIdx;
		DynamicArrayInit(&astProcCall.arguments, paramCount);
		*DynamicArrayAdd(&astProcCall.arguments) = *leftHand;
		if (paramCount > 1)
			*DynamicArrayAdd(&astProcCall.arguments) = *rightHand;

		expression->typeTableIdx = procTypeInfo.procedureInfo.returnTypeTableIdx;
		expression->nodeType = ASTNODETYPE_PROCEDURE_CALL;
		expression->procedureCall = astProcCall;

		TCPushParametersAndInlineProcedureCall(context, job, &expression->procedureCall);

		return true;
	}
	return false;
}

void GenerateTypeCheckJobs(Context *context, ASTExpression *expression);
void TryTypeCheckExpression(Context *context, TCJob *job, ASTExpression *expression)
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
			PushTCScope(context, job);
			astBlock->scopePushed = true;
		}

		while (astBlock->typeCheckingIdx < astBlock->statements.size)
		{
			TryTypeCheckExpression(context, job, &astBlock->statements[astBlock->typeCheckingIdx]);
			++astBlock->typeCheckingIdx;
			if (astBlock->typeCheckingIdx >= astBlock->statements.size)
				break;
		}

		PopTCScope(context, job);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration *varDecl = &expression->variableDeclaration;
		TypeCheckVariableDeclaration(context, job, varDecl);
		expression->typeTableIdx = varDecl->typeTableIdx;

		bool isGlobal = varDecl->isStatic || varDecl->isExternal;

		if (job->onStaticContext && !isGlobal)
			LogError(context, expression->any.loc, "Variable on static scope has to be declared "
					"either static or external"_s);

		u32 forceMemoryFlag = context->config.dontPromoteMemoryToRegisters ? VALUEFLAGS_FORCE_MEMORY : 0;
		u32 staticFlag      = varDecl->isStatic   ? VALUEFLAGS_ON_STATIC_STORAGE : 0;
		u32 externalFlag    = varDecl->isExternal ? VALUEFLAGS_IS_EXTERNAL       : 0;
		u32 valueIdx;
		if (!isGlobal)
			valueIdx = TCNewValue(context, job, varDecl->name, varDecl->typeTableIdx,
					forceMemoryFlag | staticFlag | externalFlag);
		else
			valueIdx = NewGlobalValue(context, varDecl->name, varDecl->typeTableIdx,
					forceMemoryFlag | staticFlag | externalFlag);

		varDecl->valueIdx = valueIdx;

		if (!varDecl->addedScopeName)
		{
			if (varDecl->name.size)
			{
				TCScopeName newScopeName;
				newScopeName.type = NAMETYPE_VARIABLE;
				newScopeName.name = varDecl->name;
				newScopeName.variableInfo.valueIdx = valueIdx;
				newScopeName.variableInfo.typeTableIdx = varDecl->typeTableIdx;
				newScopeName.loc = varDecl->loc;
				TCAddScopeName(context, job, newScopeName);
			}
			varDecl->addedScopeName = true;
		}

		if (varDecl->name.size == 0)
		{
			TypeCategory typeCat = GetTypeInfo(context, expression->typeTableIdx).typeCategory;
			if (typeCat != TYPECATEGORY_STRUCT && typeCat != TYPECATEGORY_UNION)
				LogError(context, expression->any.loc, "Anonymous variable has to be a struct/union!"_s);

			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = varDecl->typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.valueIdx = valueIdx;
				*varExp = e;
			}
			AddStructMembersToScope(context, job, varDecl->loc, varExp);
		}

		if (job->onStaticContext)
		{
			IRJobArgs *args = ALLOC(PhaseAllocator::Alloc, IRJobArgs);
			*args = { context, 0, {}, expression };
			{
				auto irThreads = context->irThreads.GetForWrite();
				*DynamicArrayAdd(&irThreads) = (HANDLE)_beginthread(IRJobExpression, 0, (void *)args);
			}
		}
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition *astStaticDef = &expression->staticDefinition;

		TCCheckIfNameAlreadyExists(context, job, astStaticDef->name, astStaticDef->loc);

		StaticDefinition newStaticDef = {};
		newStaticDef.typeTableIdx = TYPETABLEIDX_UNSET;
		newStaticDef.name = astStaticDef->name;

		u32 newStaticDefIdx = TCNewStaticDefinition(context, &newStaticDef);

		// Add scope name
		TCScopeName staticDefScopeName;
		staticDefScopeName.type = NAMETYPE_STATIC_DEFINITION;
		staticDefScopeName.name = astStaticDef->name;
		staticDefScopeName.staticDefinitionIdx = newStaticDefIdx;
		staticDefScopeName.loc = astStaticDef->loc;
		TCAddScopeName(context, job, staticDefScopeName);

		astStaticDef->staticDefinitionIdx = newStaticDefIdx;

		if (astStaticDef->expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
		{
			BucketArray<Value, HeapAllocator, 1024> oldLocalValues = job->localValues;
			BucketArrayInit(&job->localValues);
			*BucketArrayAdd(&job->localValues) = {}; // No value number 0?

			bool oldOnStaticContext = job->onStaticContext;
			job->onStaticContext = false;

			// Add all information we can, that doesn't depend on others
			newStaticDef.definitionType = STATICDEFINITIONTYPE_PROCEDURE;
			ASTProcedureDeclaration *procDecl = &astStaticDef->expression->procedureDeclaration;

			Procedure procedure = {};
			procedure.typeTableIdx = TYPETABLEIDX_UNSET;
			procedure.name = procDecl->name;
			procedure.returnValueIdx = U32_MAX;
			procedure.astBody = procDecl->astBody;
			procedure.isInline = procDecl->isInline;
			procedure.isExported = procDecl->isExported;
			procedure.astPrototype = procDecl->prototype;
			DynamicArrayInit(&procedure.parameterValues, 8);
			s32 procedureIdx = NewProcedure(context, procedure, procDecl->isExternal);

			PushTCScope(context, job);

			newStaticDef.procedureIdx = procedureIdx;
			procDecl->procedureIdx = procedureIdx;
			ASTProcedurePrototype *astPrototype = &procDecl->prototype;

			TypeCheckProcedurePrototype(context, job, astPrototype);
			TypeInfo t = TypeInfoFromASTProcedurePrototype(context, astPrototype);
			u32 returnType = astPrototype->returnTypeIdx;
			t.procedureInfo.returnTypeTableIdx = returnType;

			u32 typeTableIdx = FindOrAddTypeTableIdx(context, t);
			procedure.typeTableIdx = typeTableIdx;
			astStaticDef->expression->typeTableIdx = typeTableIdx;
			newStaticDef.typeTableIdx = typeTableIdx;

			TCUpdateStaticDefinition(context, job, newStaticDefIdx, &newStaticDef);
			UpdateProcedure(context, procedureIdx, &procedure);

#if DEBUG_BUILD
			{
				TypeInfo storedTypeInfo = GetTypeInfo(context, typeTableIdx);
				ASSERT(AreTypeInfosEqual(context, t, storedTypeInfo));
			}
#endif

			// @Todo: don't add values for parameters/return if there's no body?
			// Parameters
			ArrayView<ASTProcedureParameter> astParameters = astPrototype->astParameters;
			for (int i = 0; i < astParameters.size; ++i)
			{
				const ASTProcedureParameter *astParameter = &astParameters[i];
				u32 paramValueIdx = TCNewValue(context, job, astParameter->name, astParameter->typeTableIdx, 0);
				*DynamicArrayAdd(&procedure.parameterValues) = paramValueIdx;
			}
			// Varargs array
			if (astPrototype->isVarargs)
			{
				static u32 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

				u32 valueIdx = TCNewValue(context, job, astPrototype->varargsName, arrayTableIdx, 0);
				*DynamicArrayAdd(&procedure.parameterValues) = valueIdx;
			}

			TCAddParametersToScope(context, job, procedure.parameterValues, &procDecl->prototype);

			procedure.returnValueIdx = TCNewValue(context, job, "_returnValue"_s, returnType, 0);

			if (procedure.astBody)
			{
				u32 previousReturnType = job->currentReturnType;
				job->currentReturnType = t.procedureInfo.returnTypeTableIdx;

				TryTypeCheckExpression(context, job, procedure.astBody);

				job->currentReturnType = previousReturnType;
			}
			procedure.isBodyTypeChecked = true;

			UpdateProcedure(context, procedureIdx, &procedure);

			expression->typeTableIdx = procedure.typeTableIdx;
			PopTCScope(context, job);

			// Check all paths return
			if (procedure.astBody && t.procedureInfo.returnTypeTableIdx != TYPETABLEIDX_VOID)
			{
				ReturnCheckResult result = CheckIfReturnsValue(context, procedure.astBody);
				if (result == RETURNCHECKRESULT_SOMETIMES)
					LogError(context, expression->any.loc, "Procedure doesn't always return a value"_s);
				else if (result == RETURNCHECKRESULT_NEVER)
					LogError(context, expression->any.loc, "Procedure has to return a value"_s);
			}

			IRJobArgs *args = ALLOC(PhaseAllocator::Alloc, IRJobArgs);
			*args = { context, procedureIdx, job->localValues, nullptr };
			job->localValues = {}; // Safety clear
			{
				auto irThreads = context->irThreads.GetForWrite();
				*DynamicArrayAdd(&irThreads) = (HANDLE)_beginthread(IRJobProcedure, 0, (void *)args);
			}

			job->localValues = oldLocalValues;
			job->onStaticContext = oldOnStaticContext;
		}
		else if (astStaticDef->expression->nodeType == ASTNODETYPE_TYPE ||
				 astStaticDef->expression->nodeType == ASTNODETYPE_ALIAS)
		{
			newStaticDef.definitionType = STATICDEFINITIONTYPE_TYPE;

			u32 result = TypeCheckType(context, job, astStaticDef->name,
					expression->any.loc, &astStaticDef->expression->astType);
			astStaticDef->expression->typeTableIdx = result;

			u32 newTypeIdx;
			if (astStaticDef->expression->astType.nodeType == ASTTYPENODETYPE_STRUCT_DECLARATION ||
				astStaticDef->expression->astType.nodeType == ASTTYPENODETYPE_UNION_DECLARATION ||
				astStaticDef->expression->astType.nodeType == ASTTYPENODETYPE_ENUM_DECLARATION)
			{
				newTypeIdx = result;
			}
			else
			{
				TypeInfo t;
				t.typeCategory = TYPECATEGORY_ALIAS;
				t.size = GetTypeInfo(context, result).size;
				t.aliasInfo.name = astStaticDef->name;
				t.aliasInfo.aliasedTypeIdx = result;
				t.aliasInfo.doesImplicitlyCast = astStaticDef->expression->nodeType ==
					ASTNODETYPE_ALIAS;
				newTypeIdx = FindOrAddTypeTableIdx(context, t);
			}

			newStaticDef.typeTableIdx = newTypeIdx;
			expression->typeTableIdx = newTypeIdx;
			TCUpdateStaticDefinition(context, job, newStaticDefIdx, &newStaticDef);
		}
		else
		{
			TryTypeCheckExpression(context, job, astStaticDef->expression);

			if (astStaticDef->expression->nodeType == ASTNODETYPE_IDENTIFIER &&
				astStaticDef->expression->identifier.type == NAMETYPE_STATIC_DEFINITION)
			{
				ASSERT(astStaticDef->expression->typeTableIdx != TYPETABLEIDX_UNSET);
				u32 identifierStaticDefIdx = astStaticDef->expression->identifier.staticDefinitionIdx;
				newStaticDef = TCGetStaticDefinition(context, job, identifierStaticDefIdx, true);
				newStaticDef.name = astStaticDef->name;
				newStaticDef.typeTableIdx = astStaticDef->expression->typeTableIdx;
				expression->typeTableIdx = astStaticDef->expression->typeTableIdx;
			}
			else
			{
				Constant constant = TryEvaluateConstant(context, astStaticDef->expression);

				newStaticDef.constant = constant;
				newStaticDef.definitionType = STATICDEFINITIONTYPE_CONSTANT;
				u32 constantTypeIdx = astStaticDef->expression->typeTableIdx;
				ASSERT(constantTypeIdx != TYPETABLEIDX_UNSET);
				expression->typeTableIdx = constantTypeIdx;
				newStaticDef.typeTableIdx = constantTypeIdx;

				if (newStaticDef.constant.type == CONSTANTTYPE_INVALID)
					LogError(context, astStaticDef->expression->any.loc,
							"Failed to evaluate constant"_s);
			}
			TCUpdateStaticDefinition(context, job, newStaticDefIdx, &newStaticDef);
		}
	} break;
	case ASTNODETYPE_RETURN:
	{
		ASTExpression *returnExp = expression->returnNode.expression;
		TypeCheckErrorCode errorCode;
		if (returnExp != nullptr)
		{
			TryTypeCheckExpression(context, job, returnExp);

			if (IsExpressionAType(context, job, returnExp))
				LogError(context, returnExp->any.loc, "Trying to return a type"_s);

			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					job->currentReturnType, returnExp);
			returnExp->typeTableIdx = typeCheckResult.rightTableIdx;
			errorCode = typeCheckResult.errorCode;
		}
		else
			errorCode = CheckTypesMatch(context, job->currentReturnType, TYPETABLEIDX_VOID);

		if (errorCode != TYPECHECK_COOL)
		{
			Print("Incorrect return type");
			ReportTypeCheckError(context, errorCode, expression->any.loc,
					returnExp ? returnExp->typeTableIdx : TYPETABLEIDX_VOID,
					job->currentReturnType);
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		TryTypeCheckExpression(context, job, expression->deferNode.expression);
		return;
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		String string = expression->identifier.string;

		TCScopeName scopeName = TCFindScopeName(context, job, string);

		if (scopeName.type == NAMETYPE_INVALID)
			LogError(context, expression->any.loc, TPrintF("Identifier \"%S\" not found!",
					string));

		expression->identifier.type = scopeName.type;
		switch (scopeName.type)
		{
		case NAMETYPE_VARIABLE:
		{
			expression->identifier.valueIdx = scopeName.variableInfo.valueIdx;

			u32 variableTypeIdx = scopeName.variableInfo.typeTableIdx;
			while (true)
			{
				if (variableTypeIdx != TYPETABLEIDX_UNSET)
					break;

				if (!TCYield(context, job))
					LogError(context, expression->any.loc, TPrintF("COMPILER ERROR! Variable "
								"\"%S\" never type checked", string));
				// @Speed: don't look this up again, just re-read
				scopeName = TCFindScopeName(context, job, string);
			}
			TCResume(context, job);

			expression->typeTableIdx = variableTypeIdx;
		} break;
		case NAMETYPE_STRUCT_MEMBER:
		{
			expression->identifier.structMember = scopeName.structMember;
			expression->typeTableIdx = scopeName.structMember->typeTableIdx;
		} break;
		case NAMETYPE_ASTEXPRESSION:
		{
			expression->identifier.expression = scopeName.expression;
			expression->typeTableIdx = scopeName.expression->typeTableIdx;
		} break;
		case NAMETYPE_STATIC_DEFINITION:
		{
			expression->identifier.staticDefinitionIdx = scopeName.staticDefinitionIdx;
			StaticDefinition staticDefinition = TCGetStaticDefinition(context, job,
					scopeName.staticDefinitionIdx, true);
			expression->typeTableIdx = staticDefinition.typeTableIdx;
		} break;
		case NAMETYPE_PRIMITIVE:
		{
			LogError(context, expression->any.loc, TPrintF("Primitive type \"%S\" used as a "
						"variable", string));
		} break;
		default:
			ASSERT(false);
		}
	} break;
	case ASTNODETYPE_USING:
	{
		ASTExpression *usingExp = expression->usingNode.expression;
		TryTypeCheckExpression(context, job, usingExp);

		if (usingExp->nodeType == ASTNODETYPE_VARIABLE_DECLARATION)
		{
			ASTExpression *varExp = NewTreeNode(context);
			{
				ASTExpression e = {};
				e.typeTableIdx = usingExp->variableDeclaration.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.valueIdx = usingExp->variableDeclaration.valueIdx;
				*varExp = e;
			}
			AddStructMembersToScope(context, job, usingExp->any.loc, varExp);
		}
		else
			AddStructMembersToScope(context, job, usingExp->any.loc, usingExp);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall *astProcCall = &expression->procedureCall;
		String procName = astProcCall->name;

		ProcedureCallType callType = CALLTYPE_STATIC;
		s32 procedureIdx = S32_MIN;
		u32 valueIdx = U32_MAX;
		ASTExpression *astExpression = nullptr;
		u32 procedureTypeIdx = TYPETABLEIDX_UNSET;

		TCScopeName scopeName = TCFindScopeName(context, job, procName);

		if (scopeName.type == NAMETYPE_VARIABLE)
		{
			callType = CALLTYPE_VALUE;
			valueIdx = scopeName.variableInfo.valueIdx;
			procedureTypeIdx = scopeName.variableInfo.typeTableIdx;
		}
		else if (scopeName.type == NAMETYPE_ASTEXPRESSION)
		{
			callType = CALLTYPE_ASTEXPRESSION;
			astExpression = scopeName.expression;
			procedureTypeIdx = scopeName.expression->typeTableIdx;
		}
		else if (scopeName.type == NAMETYPE_STATIC_DEFINITION)
		{
			StaticDefinition staticDefinition = TCGetStaticDefinition(context, job,
					scopeName.staticDefinitionIdx, false);

			if (staticDefinition.definitionType != STATICDEFINITIONTYPE_PROCEDURE)
				LogError(context, expression->any.loc, "Calling a non-procedure"_s);

			procedureIdx = staticDefinition.procedureIdx;
			Procedure proc = GetProcedureRead(context, procedureIdx);
			if (proc.isInline)
			{
				// We need the whole body type checked
				while (true)
				{
					if (proc.isBodyTypeChecked)
						break;
					if (!TCYield(context, job))
						LogError(context, expression->any.loc, TPrintF("COMPILER ERROR! Body of "
									"inline procedure \"%S\" never type checked", proc.name));
					proc = GetProcedureRead(context, procedureIdx);
				}
				TCResume(context, job);
			}
			procedureTypeIdx = proc.typeTableIdx;
		}
		else
			LogError(context, expression->any.loc, "Calling a non-procedure"_s);

		// @Todo: don't look up procedure again after this yields
		while (true)
		{
			if (procedureTypeIdx != TYPETABLEIDX_UNSET)
				break;
			if (!TCYield(context, job))
				LogError(context, expression->any.loc, TPrintF("COMPILER ERROR! Procedure "
							"\"%S\" never type checked",
							GetProcedureRead(context, procedureIdx).name));
			procedureTypeIdx = GetProcedureRead(context, procedureIdx).typeTableIdx;
		}
		TCResume(context, job);
		ASSERT(GetTypeInfo(context, procedureTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);

		s64 givenArguments = astProcCall->arguments.size;
		for (int argIdx = 0; argIdx < givenArguments; ++argIdx)
		{
			ASTExpression *arg = &astProcCall->arguments[argIdx];
			TryTypeCheckExpression(context, job, arg);
		}

		astProcCall->callType = callType;
		if (callType == CALLTYPE_VALUE)
			astProcCall->valueIdx = valueIdx;
		else if (callType == CALLTYPE_ASTEXPRESSION)
			astProcCall->expression = astExpression;
		else
			astProcCall->procedureIdx = procedureIdx;
		astProcCall->procedureTypeIdx = procedureTypeIdx;

		astProcCall->procedureFound = true;

		ASSERT(GetTypeInfo(context, procedureTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = GetTypeInfo(context, procedureTypeIdx).procedureInfo;

		expression->typeTableIdx = procTypeInfo.returnTypeTableIdx;

		// Type check arguments
		s64 requiredArguments = 0;
		for (int i = 0; i < procTypeInfo.parameters.size; ++i)
		{
			if (procTypeInfo.parameters[i].defaultValue.type == CONSTANTTYPE_INVALID)
				++requiredArguments;
		}

		s64 totalArguments = procTypeInfo.parameters.size;
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
			ASTExpression *arg = &astProcCall->arguments[argIdx];
			u32 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
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

		TCPushParametersAndInlineProcedureCall(context, job, astProcCall);
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTExpression *input = expression->unaryOperation.expression;
		TryTypeCheckExpression(context, job, input);

		bool foundOverload = LookForOperatorOverload(context, job, expression);

		if (!foundOverload)
		{
			u32 expressionType = input->typeTableIdx;
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
					if (e->identifier.type == NAMETYPE_VARIABLE)
						TCSetValueFlags(context, job, e->identifier.valueIdx, VALUEFLAGS_FORCE_MEMORY);
				} break;
				}

				expression->typeTableIdx = GetTypeInfoPointerOf(context, expressionType);
			} break;
			case TOKEN_OP_DEREFERENCE:
			{
				TypeInfo expressionTypeInfo = GetTypeInfo(context, expressionType);
				if (expressionTypeInfo.typeCategory != TYPECATEGORY_POINTER)
					LogError(context, expression->any.loc, "Trying to dereference a non pointer"_s);
				expression->typeTableIdx = expressionTypeInfo.pointerInfo.pointedTypeTableIdx;
			} break;
			default:
			{
				expression->typeTableIdx = expressionType;
			}
			};
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;
		ASTExpression *rightHand = expression->binaryOperation.rightHand;

		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
		{
			TryTypeCheckExpression(context, job, leftHand);
			u32 leftHandTypeIdx = leftHand->typeTableIdx;

			if (rightHand->nodeType != ASTNODETYPE_IDENTIFIER)
			{
				LogError(context, rightHand->any.loc, "Expected identifier after member access operator"_s);
			}

			rightHand->identifier.type = NAMETYPE_STRUCT_MEMBER;

			// Get rid of aliases
			u32 structTypeIdx = StripImplicitlyCastAliases(context, leftHandTypeIdx);

			TypeInfo structTypeInfo = GetTypeInfo(context, structTypeIdx);

			if (structTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			{
				u32 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
				structTypeInfo = GetTypeInfo(context, pointedTypeIdx);
			}

			if (structTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
			{
				// This is only for dynamic size arrays!
				if (structTypeInfo.arrayInfo.count != 0)
					LogError(context, expression->any.loc, "Array left of '.' has to be of dynamic size! ([])"_s);

				structTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);
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
						memberName, TypeInfoToString(context, structTypeIdx)));
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
		{
			TryTypeCheckExpression(context, job, leftHand);
			TryTypeCheckExpression(context, job, rightHand);

			u32 arrayType = leftHand->typeTableIdx;
			TypeInfo arrayTypeInfo = GetTypeInfo(context, arrayType);
			if (arrayTypeInfo.typeCategory == TYPECATEGORY_POINTER)
			{
				u32 pointedTypeIdx = arrayTypeInfo.pointerInfo.pointedTypeTableIdx;
				arrayType = pointedTypeIdx;
				arrayTypeInfo = GetTypeInfo(context, pointedTypeIdx);
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
			TryTypeCheckExpression(context, job, leftHand);
			TryTypeCheckExpression(context, job, rightHand);

			bool foundOverload = LookForOperatorOverload(context, job, expression);

			if (!foundOverload)
			{
				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
						leftHand->typeTableIdx, rightHand);
				leftHand->typeTableIdx  = typeCheckResult.leftTableIdx;
				rightHand->typeTableIdx = typeCheckResult.rightTableIdx;

				switch (expression->binaryOperation.op)
				{
				case TOKEN_OP_SHIFT_LEFT:
				case TOKEN_OP_SHIFT_RIGHT:
				case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
				case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL &&
						typeCheckResult.errorCode != TYPECHECK_SIZE_MISMATCH &&
						typeCheckResult.errorCode != TYPECHECK_SIGN_MISMATCH)
					{
						String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
						LogError(context, expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}
				} break;
				case TOKEN_OP_RANGE:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
						LogError(context, expression->any.loc, TPrintF("Invalid types on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					// Both operands have to be integers
					TypeCategory leftCat = GetTypeInfo(context, StripAllAliases(context,
							leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER)
						LogError(context, leftHand->any.loc, TPrintF("Left hand of .. operator "
									"does not evaluate to an integer (%S)",
									TypeInfoToString(context, leftHand->typeTableIdx)));

					TypeCategory rightCat = GetTypeInfo(context, StripAllAliases(context,
							rightHand->typeTableIdx)).typeCategory;
					if (rightCat != TYPECATEGORY_INTEGER)
						LogError(context, rightHand->any.loc, TPrintF("Right hand of .. operator "
									"does not evaluate to an integer (%S)",
									TypeInfoToString(context, typeCheckResult.rightTableIdx)));
				} break;
				case TOKEN_OP_ASSIGNMENT:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
						LogError(context, expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}
				} break;
				case TOKEN_OP_PLUS:
				case TOKEN_OP_MINUS:
				case TOKEN_OP_BITWISE_AND:
				case TOKEN_OP_BITWISE_NOT:
				case TOKEN_OP_BITWISE_OR:
				case TOKEN_OP_BITWISE_XOR:
				case TOKEN_OP_EQUALS:
				case TOKEN_OP_LESS_THAN:
				case TOKEN_OP_LESS_THAN_OR_EQUAL:
				case TOKEN_OP_GREATER_THAN_OR_EQUAL:
				case TOKEN_OP_GREATER_THAN:
				case TOKEN_OP_ASSIGNMENT_PLUS:
				case TOKEN_OP_ASSIGNMENT_MINUS:
				case TOKEN_OP_ASSIGNMENT_BITWISE_AND:
				case TOKEN_OP_ASSIGNMENT_BITWISE_OR:
				case TOKEN_OP_ASSIGNMENT_BITWISE_XOR:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
						LogError(context, expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					TypeCategory leftCat  = GetTypeInfo(context, StripAllAliases(context,
							leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER &&
						leftCat != TYPECATEGORY_FLOATING &&
						leftCat != TYPECATEGORY_ENUM &&
						leftCat != TYPECATEGORY_POINTER)
						LogError(context, expression->any.loc, TPrintF("Invalid types on inputs of "
									"operator %S (left hand is \"%S\"",
									OperatorToString(expression->binaryOperation.op),
									TypeInfoToString(context, leftHand->typeTableIdx)));
				} break;
				default:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
						LogError(context, expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					TypeCategory leftCat  = GetTypeInfo(context, StripAllAliases(context,
							leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER &&
						leftCat != TYPECATEGORY_FLOATING)
						LogError(context, expression->any.loc, TPrintF("Invalid types on inputs of "
									"operator %S (left hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									TypeInfoToString(context, leftHand->typeTableIdx)));
				}
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
			ASSERT(expression->typeTableIdx >= TYPETABLEIDX_Begin);
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

					TryTypeCheckExpression(context, job, rightExp);
				}
				else
					TryTypeCheckExpression(context, job, memberExp);
			}
			expression->typeTableIdx = TYPETABLEIDX_STRUCT_LITERAL;
			break;
		default:
			ASSERT(!"Unexpected literal type");
		}
	} break;
	case ASTNODETYPE_IF:
	{
		TryTypeCheckExpression(context, job, expression->ifNode.condition);

		u32 conditionType = expression->ifNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		TryTypeCheckExpression(context, job, expression->ifNode.body);

		if (expression->ifNode.elseBody)
			TryTypeCheckExpression(context, job, expression->ifNode.elseBody);
	} break;
	case ASTNODETYPE_IF_STATIC:
	{
		TryTypeCheckExpression(context, job, expression->ifStaticNode.condition);

		u32 conditionType = expression->ifStaticNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		Constant conditionResult = TryEvaluateConstant(context, expression->ifStaticNode.condition);
		if (conditionResult.type == CONSTANTTYPE_INVALID)
			LogError(context, expression->ifStaticNode.condition->any.loc,
					"Failed to evaluate static if condition"_s);

		bool evaluatesToTrue = conditionResult.valueAsInt != 0;
		expression->ifStaticNode.evaluatesToTrue = evaluatesToTrue;

		if (evaluatesToTrue)
			TryTypeCheckExpression(context, job, expression->ifStaticNode.body);

		else if (expression->ifStaticNode.elseBody)
			TryTypeCheckExpression(context, job, expression->ifStaticNode.elseBody);
	} break;
	case ASTNODETYPE_WHILE:
	{
		TryTypeCheckExpression(context, job, expression->whileNode.condition);

		u32 conditionType = expression->whileNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context, expression->any.loc, "While condition doesn't evaluate to a boolean"_s);

		TryTypeCheckExpression(context, job, expression->whileNode.body);
	} break;
	case ASTNODETYPE_FOR:
	{
		ASTFor *astFor = &expression->forNode;

		TryTypeCheckExpression(context, job, astFor->range);

		if (!astFor->scopePushed)
		{
			PushTCScope(context, job);

			u32 indexValueIdx = TCNewValue(context, job, astFor->indexVariableName,
					TYPETABLEIDX_S64, 0);
			astFor->indexValueIdx = indexValueIdx;

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = astFor->indexVariableName;
			newScopeName.variableInfo.valueIdx = indexValueIdx;
			newScopeName.variableInfo.typeTableIdx = TYPETABLEIDX_S64;
			newScopeName.loc = expression->any.loc;
			TCAddScopeName(context, job, newScopeName);

			ASTExpression *rangeExp = astFor->range;
			bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
				rangeExp->binaryOperation.op == TOKEN_OP_RANGE;

			if (!isExplicitRange)
			{
				u32 elementTypeTableIdx = TYPETABLEIDX_U8;
				if (rangeExp->typeTableIdx != TYPETABLEIDX_STRING_STRUCT)
				{
					TypeInfo rangeTypeInfo = GetTypeInfo(context, rangeExp->typeTableIdx);
					if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER)
						rangeTypeInfo = GetTypeInfo(context, rangeTypeInfo.pointerInfo.pointedTypeTableIdx);

					if (rangeTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
						LogError(context, astFor->range->any.loc, "'for' range "
								"expression does not evaluate to an array nor is it a number range "
								"(..)"_s);
					elementTypeTableIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
				}

				u32 pointerToElementTypeTableIdx = GetTypeInfoPointerOf(context, elementTypeTableIdx);
				u32 elementValueIdx = TCNewValue(context, job, astFor->itemVariableName,
						pointerToElementTypeTableIdx, 0);
				astFor->elementValueIdx = elementValueIdx;

				newScopeName.name = astFor->itemVariableName;
				newScopeName.variableInfo.valueIdx = elementValueIdx;
				newScopeName.variableInfo.typeTableIdx = pointerToElementTypeTableIdx;
				newScopeName.loc = expression->any.loc;
				TCAddScopeName(context, job, newScopeName);
			}

			astFor->scopePushed = true;
		}

		u32 oldForArray = job->currentForLoopArrayType;
		job->currentForLoopArrayType = astFor->range->typeTableIdx;

		TryTypeCheckExpression(context, job, astFor->body);

		// Important to restore whether we yield or not!
		job->currentForLoopArrayType = oldForArray;

		PopTCScope(context, job);
	} break;
	case ASTNODETYPE_BREAK:
	case ASTNODETYPE_CONTINUE:
	{
	} break;
	case ASTNODETYPE_REMOVE:
	{
		TypeInfo forArrayType = GetTypeInfo(context, job->currentForLoopArrayType);
		if (forArrayType.typeCategory != TYPECATEGORY_ARRAY || forArrayType.arrayInfo.count != 0)
			LogError(context, expression->any.loc, "'remove' found but there wasn't a for loop "
					"with a dynamic sized array as range"_s);
	} break;
	case ASTNODETYPE_TYPE:
	case ASTNODETYPE_ALIAS:
	{
		expression->typeTableIdx = TypeCheckType(context, job, {}, expression->any.loc,
				&expression->astType);
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		TryTypeCheckExpression(context, job, expression->typeOfNode.expression);

		static u32 typeInfoPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT);
		expression->typeTableIdx = typeInfoPointerTypeIdx;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		TryTypeCheckExpression(context, job, expression->sizeOfNode.expression);
		expression->typeTableIdx = TYPETABLEIDX_S64;
	} break;
	case ASTNODETYPE_CAST:
	{
		TryTypeCheckExpression(context, job, expression->castNode.expression);

		u32 typeCheckResult = TypeCheckType(context, job, {}, expression->any.loc,
				&expression->castNode.astType);

		TypeCheckResult typeSpecializeResult = CheckTypesMatchAndSpecialize(context, typeCheckResult,
				expression->castNode.expression);
		expression->castNode.expression->typeTableIdx = typeSpecializeResult.rightTableIdx;

		expression->typeTableIdx = typeCheckResult;
	} break;
	case ASTNODETYPE_INTRINSIC:
	{
		if (expression->intrinsic.type == INTRINSIC_UNSET)
		{
			if (StringEquals(expression->intrinsic.name, "breakpoint"_s))
				expression->intrinsic.type = INTRINSIC_BREAKPOINT;
			else if (StringEquals(expression->intrinsic.name, "sqrt32"_s))
				expression->intrinsic.type = INTRINSIC_SQRT32;
			else if (StringEquals(expression->intrinsic.name, "sqrt64"_s))
				expression->intrinsic.type = INTRINSIC_SQRT64;
			else
				LogError(context, expression->any.loc, "Invalid compiler intrinsic"_s);
		}

		FixedArray<u32, 4> argTypes;
		switch (expression->intrinsic.type)
		{
		case INTRINSIC_BREAKPOINT:
		{
			argTypes.size = 0;
		} break;
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
			TryTypeCheckExpression(context, job, arg);
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					argTypes[argIdx], arg);
			arg->typeTableIdx = typeCheckResult.rightTableIdx;

			if (typeCheckResult.errorCode != TYPECHECK_COOL)
			{
				String paramStr = TypeInfoToString(context, argTypes[argIdx]);
				String givenStr = TypeInfoToString(context, arg->typeTableIdx);
				LogError(context, arg->any.loc, TPrintF("When calling intrinsic \"%S\": type of "
							"parameter #%d didn't match (parameter is %S but %S was given)",
							expression->intrinsic.name, argIdx, paramStr, givenStr));
			}
		}
		expression->typeTableIdx = TYPETABLEIDX_VOID;
	} break;
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		ASTOperatorOverload *astOverload = &expression->operatorOverload;

		static u64 overloadUniqueId = 0;

		OperatorOverload overload = {};
		overload.op = astOverload->op;

		Procedure p = {};
		p.typeTableIdx = TYPETABLEIDX_UNSET;
		p.name = TPrintF("__overload%d_%d", overload.op, overloadUniqueId++);
		p.returnValueIdx = U32_MAX;
		p.astBody = astOverload->astBody;
		p.isInline = astOverload->isInline;
		p.astPrototype = astOverload->prototype;
		DynamicArrayInit(&p.parameterValues, 8);
		overload.procedureIdx = NewProcedure(context, p, false);

		{
			auto operatorOverloads = context->operatorOverloads.GetForWrite();
			*DynamicArrayAdd(&operatorOverloads) = overload;
		}

		astOverload->procedureIdx = overload.procedureIdx;

		PushTCScope(context, job);

		Procedure procedure = GetProcedureRead(context, astOverload->procedureIdx);

		TypeCheckProcedurePrototype(context, job,
				&astOverload->prototype);
		TypeInfo t = TypeInfoFromASTProcedurePrototype(context, &astOverload->prototype);

		u32 returnType = astOverload->prototype.returnTypeIdx;
		t.procedureInfo.returnTypeTableIdx = returnType;
		procedure.returnValueIdx = TCNewValue(context, job, "_returnValue"_s, returnType, 0);

		u32 typeTableIdx = FindOrAddTypeTableIdx(context, t);
		procedure.typeTableIdx = typeTableIdx;

		UpdateProcedure(context, astOverload->procedureIdx, &procedure);

		// Parameters
		u64 paramCount = astOverload->prototype.astParameters.size;
		if (paramCount == 0)
		{
			LogError(context, expression->any.loc, TPrintF(
					"No parameters provided on overload for operator %S.",
					OperatorToString(astOverload->op)));
		}
		else if (paramCount == 1)
		{
			if (astOverload->op != TOKEN_OP_NOT &&
				astOverload->op != TOKEN_OP_BITWISE_NOT &&
				astOverload->op != TOKEN_OP_MINUS)
				LogError(context, expression->any.loc, TPrintF(
							"Only 1 parameter is present on overload for operator %S. "
							"Expected 2.", OperatorToString(astOverload->op)));
		}
		else if (paramCount == 2)
		{
			if (astOverload->op == TOKEN_OP_NOT ||
				astOverload->op == TOKEN_OP_BITWISE_NOT)
				LogError(context, expression->any.loc, TPrintF(
							"2 parameters found on overload for operator %S. "
							"Expected 1.", OperatorToString(astOverload->op)));
		}
		else
		{
			LogError(context, expression->any.loc, TPrintF(
					"Too many parameters provided on overload for operator %S.",
					OperatorToString(astOverload->op)));
		}

		for (int i = 0; i < paramCount; ++i)
		{
			ASTProcedureParameter astParameter = astOverload->prototype.astParameters[i];
			u32 paramValueIdx = TCNewValue(context, job, astParameter.name, astParameter.typeTableIdx, 0);
			*DynamicArrayAdd(&procedure.parameterValues) = paramValueIdx;
		}
		// Varargs array
		if (astOverload->prototype.isVarargs)
		{
			static u32 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

			u32 valueIdx = TCNewValue(context, job, astOverload->prototype.varargsName, arrayTableIdx, 0);
			*DynamicArrayAdd(&procedure.parameterValues) = valueIdx;
		}
		TCAddParametersToScope(context, job, procedure.parameterValues, &astOverload->prototype);

		if (astOverload->astBody)
		{
			u32 previousReturnType = job->currentReturnType;
			job->currentReturnType = t.procedureInfo.returnTypeTableIdx;

			TryTypeCheckExpression(context, job, astOverload->astBody);

			job->currentReturnType = previousReturnType;
		}
		procedure.isBodyTypeChecked = true;

		expression->typeTableIdx = procedure.typeTableIdx;

		UpdateProcedure(context, astOverload->procedureIdx, &procedure);

		PopTCScope(context, job);

		// Check all paths return
		if (astOverload->astBody && t.procedureInfo.returnTypeTableIdx != TYPETABLEIDX_VOID)
		{
			ReturnCheckResult result = CheckIfReturnsValue(context, astOverload->astBody);
			if (result == RETURNCHECKRESULT_SOMETIMES)
				LogError(context, expression->any.loc, "Procedure doesn't always return a value"_s);
			else if (result == RETURNCHECKRESULT_NEVER)
				LogError(context, expression->any.loc, "Procedure has to return a value"_s);
		}
	} break;
	case ASTNODETYPE_INCLUDE:
	{
		String filename = expression->include.filename;

#if 1
		CompilerAddSourceFile(context, filename, expression->any.loc);
#else
		// Lock so we don't generate multiple jobs from the same tree branch.
		SYSMutexLock(context->tcMutex);

		if (CompilerAddSourceFile(context, filename, expression->any.loc))
		{
			TokenizeFile(context, (int)context->sourceFiles.GetForRead()->size - 1);

			while (context->token->type != TOKEN_END_OF_FILE)
			{
				ASTExpression *statement = DynamicArrayAdd(&context->astRoot->block.statements);
				*statement = ParseStaticStatement(context);
				GenerateTypeCheckJobs(context, statement);
			}
		}

		SYSMutexUnlock(context->tcMutex);
#endif
	} break;
	case ASTNODETYPE_LINKLIB:
	{
		String filename = expression->linklib.filename;
		*DynamicArrayAdd(&context->libsToLink) = filename;
	} break;
	case ASTNODETYPE_DEFINED:
	{
		expression->typeTableIdx = TYPETABLEIDX_BOOL;

		String identifier = expression->definedNode.identifier;
		bool isDefined;

		// Current stack
		ArrayView<TCScope> scopeStack = job->scopeStack;
		for (s64 stackIdx = scopeStack.size - 1; stackIdx >= 0; --stackIdx)
		{
			const TCScope *currentScope = &scopeStack[stackIdx];
			for (int i = 0; i < currentScope->names.size; ++i)
			{
				const TCScopeName *currentName = &currentScope->names[i];
				if (StringEquals(identifier, currentName->name))
				{
					isDefined = true;
					goto done;
				}
			}
		}
		// Global scope
		while (true)
		{
			{
				auto globalScope = context->tcGlobalScope.GetForRead();
				for (int i = 0; i < globalScope->names.size; ++i)
				{
					const TCScopeName *currentName = &globalScope->names[i];
					if (StringEquals(identifier, currentName->name))
					{
						TCResume(context, job);
						isDefined = true;
						goto done;
					}
				}
				{
					auto jobStates = context->tcJobStates.GetForWrite();
					(*jobStates)[job->jobIdx] = TCJOBSTATE_WAITING_FOR_STOP;
				}
				{
					auto jobStates = context->tcJobStates.GetForRead();
					for (int i = 0; i < (*jobStates).size; ++i)
						if ((*jobStates)[i] == TCJOBSTATE_RUNNING)
							goto sleep;
				}
				{
					auto jobStates = context->parseJobStates.GetForRead();
					for (int i = 0; i < (*jobStates).size; ++i)
						if ((*jobStates)[i] == TCJOBSTATE_RUNNING)
							goto sleep;
				}
				isDefined = false;
				Print("Decided that %S is not defined\n", identifier);
				goto done;
			}
sleep:
			Sleep(0);
		}

done:
		expression->definedNode.isDefined = isDefined;
	} break;
	default:
	{
		LogError(context, expression->any.loc, "COMPILER PANIC! Unknown expression type on type checking"_s);
	} break;
	}
}

void TCJobProc(void *args)
{
	TCJobArgs *argsStruct = (TCJobArgs *)args;
	Context *context = argsStruct->context;
	ASTExpression *expression = argsStruct->expression;
	TCJob job = { argsStruct->jobIdx, expression };
	job.onStaticContext = true;
	job.currentReturnType = TYPETABLEIDX_UNSET;

	ThreadData threadData = {};
	threadData.fileIdx = expression->any.loc.fileIdx;
	TlsSetValue(context->tlsIndex, &threadData);

#if DEBUG_BUILD
	String threadName = "???"_s;
	HANDLE thread = GetCurrentThread();
	switch (expression->nodeType)
	{
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		switch (expression->staticDefinition.expression->nodeType)
		{
		case ASTNODETYPE_PROCEDURE_DECLARATION:
		{
			threadName = TPrintF("TC:%S - Procedure declaration", expression->staticDefinition.name);
		} break;
		case ASTNODETYPE_TYPE:
		case ASTNODETYPE_ALIAS:
		{
			threadName = TPrintF("TC:%S - Type declaration", expression->staticDefinition.name);
		} break;
		case ASTNODETYPE_IDENTIFIER:
		{
			threadName = TPrintF("TC:%S - Constant declaration", expression->staticDefinition.name);
		} break;
		}
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		threadName = TPrintF("TC:%S - Variable declaration", expression->staticDefinition.name);
	} break;
	case ASTNODETYPE_IF_STATIC:
	{
		threadName = "TC:Static if"_s;
	} break;
	}

	char buffer[512];
	char *dst = buffer;
	const char *src = threadName.data;
	for (int i = 0; i < threadName.size; ++i)
	{
		*dst++ = *src++;
		*dst++ = 0;
	}
	*dst++ = 0;
	*dst++ = 0;
	SetThreadDescription(thread, (PCWSTR)buffer);
#endif

	switch (expression->nodeType)
	{
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition astStaticDef = expression->staticDefinition;

		if (astStaticDef.expression->nodeType == ASTNODETYPE_PROCEDURE_DECLARATION)
			DynamicArrayInit(&job.scopeStack, 16);
	} break;
	case ASTNODETYPE_INCLUDE:
	case ASTNODETYPE_LINKLIB:
	case ASTNODETYPE_IF_STATIC:
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		DynamicArrayInit(&job.scopeStack, 16);
	} break;
	default:
		CRASH;
	}

	TryTypeCheckExpression(context, &job, expression);

	{
		auto jobStates = context->tcJobStates.GetForWrite();
		(*jobStates)[job.jobIdx] = TCJOBSTATE_DONE;
	}
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
	case ASTNODETYPE_STATIC_DEFINITION:
	case ASTNODETYPE_INCLUDE:
	case ASTNODETYPE_LINKLIB:
	case ASTNODETYPE_IF_STATIC:
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		auto tcThreads = context->tcThreads.GetForWrite();
		u32 jobIdx = (u32)tcThreads->size;

		TCJobArgs *args = ALLOC(PhaseAllocator::Alloc, TCJobArgs);
		*args = { context, jobIdx, expression };
		{
			auto jobStates = context->tcJobStates.GetForWrite();
			ASSERT((*jobStates).size == tcThreads->size);
			*DynamicArrayAdd(&jobStates) = TCJOBSTATE_RUNNING;
		}
		*DynamicArrayAdd(&tcThreads) = (HANDLE)_beginthread(TCJobProc, 0, (void *)args);
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
		LogError(context, expression->any.loc, "COMPILER PANIC! Invalid expression type found "
				"while generating type checking jobs"_s);
	} break;
	default:
	{
		LogError(context, expression->any.loc, "COMPILER PANIC! Unknown expression type found "
				"while generating type checking jobs"_s);
	}
	}
}

void TypeCheckMain(Context *context)
{
	{
		auto staticDefinitions = context->staticDefinitions.GetForWrite();
		BucketArrayInit(&staticDefinitions);
	}

	{
		auto values = context->values.GetForWrite();
		BucketArrayInit(&values);
	}

	{
		auto procedures = context->procedures.GetForWrite();
		BucketArrayInit(&procedures);
		// Procedure 0 is invalid
		*BucketArrayAdd(&procedures) = {};
	}

	{
		auto externalProcedures = context->externalProcedures.GetForWrite();
		BucketArrayInit(&externalProcedures);
		*BucketArrayAdd(&externalProcedures) = {};
	}

	{
		auto operatorOverloads = context->operatorOverloads.GetForWrite();
		DynamicArrayInit(&operatorOverloads, 32);
	}

	{
		auto typeTable = context->typeTable.GetForWrite();
		BucketArrayInit(&typeTable);
		for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
			BucketArrayAdd(&typeTable);

		TypeInfo *typeTableFast = (TypeInfo *)(*typeTable).buckets[0].data;

		TypeInfo t;
		t.typeCategory = TYPECATEGORY_INTEGER;
		t.integerInfo.isSigned = true;

		t.size = 1;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s8"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S8]  = t;
		t.size = 2;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s16"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S16] = t;
		t.size = 4;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s32"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S32] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s64"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S64] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_integer"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_INTEGER] = t;

		t.integerInfo.isSigned = false;

		t.size = 1;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u8"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U8]  = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_bool"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_BOOL]  = t;
		t.size = 2;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u16"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U16] = t;
		t.size = 4;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u32"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U32] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u64"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U64] = t;

		t.size = 16;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_128"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_128] = t;

		t.typeCategory = TYPECATEGORY_FLOATING;
		t.size = 4;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_f32"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F32] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_f64"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F64] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_floating"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_FLOATING] = t;

		t = {};
		t.typeCategory = TYPECATEGORY_INVALID;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_void"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_VOID] = t;

		t.valueIdx = NewGlobalValue(context, "_typeInfo_string_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_STRING_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_array_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ARRAY_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_any_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ANY_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_integer_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_struct_member_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_struct_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_enum_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_pointer_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_array_struct"_s, TYPETABLEIDX_UNSET, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT] = t;
	}

	{
		auto conditionVariables = context->tcConditionVariables.GetForWrite();
		HashMapInit(&conditionVariables, 256);
	}

	{
		auto globalScope = &context->tcGlobalScope.GetForWrite();

		DynamicArrayInit(&globalScope->names, 64);
		DynamicArrayInit(&globalScope->typeIndices, 64);

		for (int i = 0; i < TYPETABLEIDX_COUNT; ++i)
			*DynamicArrayAdd(&globalScope->typeIndices) = i;

		TCScopeName scopeNamePrimitive;
		scopeNamePrimitive.type = NAMETYPE_PRIMITIVE;
		scopeNamePrimitive.loc = {};

		scopeNamePrimitive.name = "s8"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S8;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s16"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S16;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S32;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S64;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u8"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U8;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u16"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U16;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U32;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U64;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "f32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_F32;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "f64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_F64;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "bool"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_BOOL;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;

		scopeNamePrimitive.name = "void"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_VOID;
		*DynamicArrayAdd(&globalScope->names) = scopeNamePrimitive;
	}

	{
		auto tcThreads = context->tcThreads.GetForWrite();
		DynamicArrayInit(&tcThreads, 128);
	}
	{
		auto jobStates = context->tcJobStates.GetForWrite();
		DynamicArrayInit(&jobStates, 128);
	}

	{
		auto irThreads = context->irThreads.GetForWrite();
		DynamicArrayInit(&irThreads, 128);
	}
}
