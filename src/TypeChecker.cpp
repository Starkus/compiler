inline String GetValueName(Value value)
{
#if DEBUG_BUILD
	return value.name;
#else
	if (value.flags & VALUEFLAGS_IS_EXTERNAL)
		return StringExpand(value.externalSymbolName);
	else
		return "???"_s;
#endif
}

u32 TCNewValue(TCContext *context, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	u64 idx = context->localValues.count;
	Value *result = BucketArrayAdd(&context->localValues);
#if DEBUG_BUILD
	result->name = {};
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 TCNewValue(TCContext *context, String name, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	u64 idx = context->localValues.count;
	Value *result = BucketArrayAdd(&context->localValues);
#if DEBUG_BUILD
	result->name = name;
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 TCNewValue(TCContext *context, Value value)
{
	ASSERT(value.typeTableIdx != 0);
	ASSERT(!(value.flags & VALUEFLAGS_TRY_IMMITATE) || value.tryImmitateValueIdx != U32_MAX);

	u64 idx = context->localValues.count;
	Value *result = BucketArrayAdd(&context->localValues);
	*result = value;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

inline Value *TCGetValue(TCContext *context, u32 valueIdx)
{
	ASSERT(valueIdx > 0);
	ASSERT(!(valueIdx & VALUE_GLOBAL_BIT));
	return &context->localValues[valueIdx];
}

u32 NewGlobalValue(Context *context, u32 typeTableIdx, u32 flags, u32 immitateValueIdx = U32_MAX) {
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	auto globalValues = context->globalValues.GetForWrite();

	u64 idx = globalValues->count;
	Value *result = BucketArrayAdd(&globalValues);
#if DEBUG_BUILD
	result->name = {};
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;
	result->externalSymbolName = {};

	ASSERT(idx < U32_MAX);
	idx |= VALUE_GLOBAL_BIT;
	return (u32)idx;
}

u32 NewGlobalValue(Context *context, String name, u32 typeTableIdx, u32 flags,
		u32 immitateValueIdx = U32_MAX) {
	ASSERT(typeTableIdx != 0);
	ASSERT(!(flags & VALUEFLAGS_TRY_IMMITATE) || immitateValueIdx != U32_MAX);

	auto globalValues = context->globalValues.GetForWrite();

	u64 idx = globalValues->count;
	Value *result = BucketArrayAdd(&globalValues);
#if DEBUG_BUILD
	result->name = name;
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->tryImmitateValueIdx = immitateValueIdx;
	result->externalSymbolName = StringMinify(name);

	ASSERT(idx < U32_MAX);
	idx |= VALUE_GLOBAL_BIT;
	return (u32)idx;
}

u32 NewGlobalValue(Context *context, Value value) {
	ASSERT(value.typeTableIdx != 0);
	ASSERT(!(value.flags & VALUEFLAGS_TRY_IMMITATE) || value.tryImmitateValueIdx != U32_MAX);

	auto globalValues = context->globalValues.GetForWrite();

	u64 idx = globalValues->count;
	Value *result = BucketArrayAdd(&globalValues);
	*result = value;

	ASSERT(idx < U32_MAX);
	idx |= VALUE_GLOBAL_BIT;
	return (u32)idx;
}

inline Value GetGlobalValue(Context *context, u32 valueIdx) {
	ASSERT(valueIdx & VALUE_GLOBAL_BIT);
	auto globalValues = context->globalValues.GetForRead();
	Value result = globalValues[valueIdx & VALUE_GLOBAL_MASK];
	return result;
}

inline void UpdateGlobalValue(Context *context, u32 valueIdx, Value *value) {
	ASSERT(valueIdx & VALUE_GLOBAL_BIT);
	auto globalValues = context->globalValues.GetForWrite();
	globalValues[valueIdx & VALUE_GLOBAL_MASK] = *value;
}

inline void TCSetValueFlags(TCContext *context, u32 valueIdx, u32 flags) {
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = context->global->globalValues.GetForWrite();
		globalValues[valueIdx & VALUE_GLOBAL_MASK].flags |= flags;
	}
	else
		context->localValues[valueIdx].flags |= flags;
}

inline Value TCGetValueRead(TCContext *context, u32 valueIdx) {
	ASSERT(valueIdx > 0);
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(context->global, valueIdx);
	else {
		return context->localValues[valueIdx];
	}
}

inline Procedure GetProcedureRead(Context *context, u32 procedureIdx) {
	Procedure result;
	ASSERT(procedureIdx != 0);
	if (procedureIdx & PROCEDURE_EXTERNAL_BIT) {
		auto externalProcedures = context->externalProcedures.GetForRead();
		result = externalProcedures[procedureIdx & PROCEDURE_EXTERNAL_MASK];
	}
	else {
		auto procedures = context->procedures.GetForRead();
		result = procedures[procedureIdx];
	}

	return result;
}

inline void UpdateProcedure(Context *context, u32 procedureIdx, Procedure *value) {
	ASSERT(procedureIdx != 0);
	if (procedureIdx & PROCEDURE_EXTERNAL_BIT) {
		auto externalProcedures = context->externalProcedures.GetForWrite();
		externalProcedures[procedureIdx & PROCEDURE_EXTERNAL_MASK] = *value;
	}
	else {
		auto procedures = context->procedures.GetForWrite();
		procedures[procedureIdx] = *value;
	}
}

inline ASTExpression *TCNewTreeNode() {
	return ALLOC(ThreadAllocator, ASTExpression);
}

TCScope *TCGetTopMostScope(TCContext *context) {
	if (context->scopeStack.size > 0)
		return DynamicArrayBack(&context->scopeStack);
	else
		return nullptr;
}

inline TypeInfo GetTypeInfo(JobContext *context, u32 typeTableIdx)
{
	ASSERT(typeTableIdx > TYPETABLEIDX_Unset);

	// Optimize common path: type IS ready! don't lock anything, just read.
	auto &typeTable = context->global->typeTable.unsafe;
	TypeInfo result = typeTable[typeTableIdx];
	if (result.typeCategory != TYPECATEGORY_NOT_READY)
		return result;
	// Otherwise, lock and read again

	// Lock this, if we need to switch fibers, we don't unlock until we added this fiber to a
	// waiting list.
	// We lock the list of jobs here instead of the type table because no one locks the type table
	// to read it.
	SYSMutexLock(context->global->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
	result = typeTable[typeTableIdx];

	if (result.typeCategory == TYPECATEGORY_NOT_READY) {
		// IMPORTANT! The scheduler will unlock jobsWaitingForType once it adds this job to the
		// waiting list, so we don't miss waking it up when updating the type.
		SwitchJob(context, YIELDREASON_TYPE_NOT_READY, { .index = typeTableIdx });
		// Lock again!
		SYSMutexLock(context->global->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);

		result = typeTable[typeTableIdx];
		if (result.typeCategory == TYPECATEGORY_NOT_READY)
			LogCompilerError(context->global, {}, "Bad job resume"_s);
	}

	SYSMutexUnlock(context->global->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
	return result;
}

String TypeInfoToString(JobContext *context, u32 typeTableIdx)
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
		return TStringConcat("^"_s, TypeInfoToString(context, typeInfo.pointerInfo.pointedTypeTableIdx));
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
		for (int i = 0; i < typeInfo.procedureInfo.parameters.size; ++i) {
			if (i) result = TStringConcat(result, ", "_s);
			String paramStr = TypeInfoToString(context, typeInfo.procedureInfo.parameters[i].typeTableIdx);
			result = TStringConcat(result, paramStr);
			Constant defaultValue = typeInfo.procedureInfo.parameters[i].defaultValue;
			if (defaultValue.type == CONSTANTTYPE_INTEGER)
				result = TPrintF("%S = %lld", result, defaultValue.valueAsInt);
			else if (defaultValue.type == CONSTANTTYPE_FLOATING)
				result = TPrintF("%S = %f", result, defaultValue.valueAsFloat);
		}
		result = TStringConcat(result, ") -> "_s);
		if (!typeInfo.procedureInfo.returnTypeIndices.size)
			result = TStringConcat(result, "void"_s);
		else for (int i = 0; i < typeInfo.procedureInfo.returnTypeIndices.size; ++i) {
			if (i) result = TStringConcat(result, ", "_s);
			String paramStr = TypeInfoToString(context, typeInfo.procedureInfo.returnTypeIndices[i]);
			result = TStringConcat(result, paramStr);
		}
		return result;
	}
	}
	return "???TYPE"_s;
}

inline String TypeCategoryToString(TypeCategory typeCategory)
{
	switch (typeCategory) {
		case TYPECATEGORY_INTEGER:		return "integer"_s;
		case TYPECATEGORY_FLOATING:		return "floating"_s;
		case TYPECATEGORY_STRUCT:		return "struct"_s;
		case TYPECATEGORY_UNION:		return "union"_s;
		case TYPECATEGORY_ENUM:			return "enum"_s;
		case TYPECATEGORY_POINTER:		return "pointer"_s;
		case TYPECATEGORY_ARRAY:		return "array"_s;
		case TYPECATEGORY_PROCEDURE:	return "procedure"_s;
		case TYPECATEGORY_ALIAS:		return "alias"_s;
		case TYPECATEGORY_NOT_READY:	return "NOT READY"_s;
		case TYPECATEGORY_INVALID:
		default:						return "INVALID"_s;
	}
}

inline void TCPushScope(TCContext *context)
{
	TCScope *newScope = DynamicArrayAdd(&context->scopeStack);

	DynamicArrayInit(&newScope->names, 64);
	DynamicArrayInit(&newScope->typeIndices, 64);
}

inline void TCPopScope(TCContext *context)
{
	--context->scopeStack.size;
}

TCScopeName FindGlobalName(JobContext *context, SourceLocation loc, String name)
{
	// Lock this, if we need to switch fibers, we don't unlock until we added this fiber to a
	// waiting list.
	RWSpinlockLockForRead(&context->global->tcGlobalNames.rwLock);
	{
		auto &globalNames = context->global->tcGlobalNames.unsafe;
		for (int i = 0; i < globalNames.size; ++i) {
			const TCScopeName *currentName = &globalNames[i];
			if (StringEquals(name, currentName->name)) {
				RWSpinlockUnlockForRead(&context->global->tcGlobalNames.rwLock);
				return *currentName;
			}
		}
	}

	// IMPORTANT! The scheduler will unlock tcGlobalNames once it adds this job to
	// the waiting list, so we don't miss waking it up when adding the identifier.
	SwitchJob(context, YIELDREASON_UNKNOWN_IDENTIFIER, { .loc = loc, .identifier = name });

	// Lock again
	RWSpinlockLockForRead(&context->global->tcGlobalNames.rwLock);
	{
		auto &globalNames = context->global->tcGlobalNames.unsafe;
		for (int i = 0; i < globalNames.size; ++i) {
			const TCScopeName *currentName = &globalNames[i];
			if (StringEquals(name, currentName->name)) {
				RWSpinlockUnlockForRead(&context->global->tcGlobalNames.rwLock);
				return *currentName;
			}
		}
	}
	// Shouldn't have resumed this job if the identifier is still missing.
	LogCompilerError(context->global, loc, "Bad job resume"_s);
}

TCScopeName TCFindScopeName(TCContext *context, SourceLocation loc, String name)
{
	// Current stack
	ArrayView<TCScope> scopeStack = context->scopeStack;
	for (s64 stackIdx = scopeStack.size - 1; stackIdx >= 0; --stackIdx) {
		const TCScope *currentScope = &scopeStack[stackIdx];
		for (int i = 0; i < currentScope->names.size; ++i) {
			const TCScopeName *currentName = &currentScope->names[i];
			if (StringEquals(name, currentName->name))
				return *currentName;
		}
	}
	// Global scope
	return FindGlobalName(context, loc, name);
}

inline u32 NewStaticDefinition(Context *context, StaticDefinition *value)
{
	auto staticDefinitions = context->staticDefinitions.GetForWrite();
	u64 result = staticDefinitions->count;
	*BucketArrayAdd(&staticDefinitions) = *value;
	ASSERT(result < U32_MAX);

	return (u32)result;
}

inline StaticDefinition GetStaticDefinition(JobContext *context, u32 staticDefinitionIdx,
		bool ensureTypeChecked = false)
{
	StaticDefinition staticDefinition;

	// Optimize most frequent path: def IS ready! no locking.
	auto &staticDefinitions = context->global->staticDefinitions.unsafe;
	staticDefinition = staticDefinitions[staticDefinitionIdx];
	if (staticDefinition.definitionType != STATICDEFINITIONTYPE_NOT_READY &&
			(!ensureTypeChecked || staticDefinition.typeTableIdx != TYPETABLEIDX_Unset))
		return staticDefinition;

	ProfilerBegin("Waiting on static definition");

	// Lock this, if we need to switch fibers, we don't unlock until we added this fiber to a
	// waiting list.
	SYSLockForRead(&context->global->staticDefinitions.rwLock);

	staticDefinition = staticDefinitions[staticDefinitionIdx];
	if (staticDefinition.definitionType == STATICDEFINITIONTYPE_NOT_READY ||
			(ensureTypeChecked && staticDefinition.typeTableIdx == TYPETABLEIDX_Unset)) {
		// IMPORTANT! The scheduler will unlock staticDefinitions once it adds this job to the
		// waiting list, so we don't miss waking it up when adding the identifier.
		SwitchJob(context, YIELDREASON_STATIC_DEF_NOT_READY, { .index = staticDefinitionIdx });

		// Need to lock these again!
		SYSLockForRead(&context->global->staticDefinitions.rwLock);

		staticDefinition = staticDefinitions[staticDefinitionIdx];

		if (staticDefinition.definitionType == STATICDEFINITIONTYPE_NOT_READY ||
				(ensureTypeChecked && staticDefinition.typeTableIdx == TYPETABLEIDX_Unset))
			LogCompilerError(context->global, {}, "Bad job resume"_s);
	}
	SYSUnlockForRead(&context->global->staticDefinitions.rwLock);
	ProfilerEnd();
	return staticDefinition;
}

inline void UpdateStaticDefinition(Context *context, u32 staticDefinitionIdx,
		StaticDefinition *value)
{
	{
		auto staticDefinitions = context->staticDefinitions.GetForWrite();
		staticDefinitions[staticDefinitionIdx] = *value;
	}

	if (value->definitionType != STATICDEFINITIONTYPE_NOT_READY &&
		value->typeTableIdx != TYPETABLEIDX_Unset)
		// Wake up any job waiting for this static def to be ready.
		WakeUpAllByIndex(context, YIELDREASON_STATIC_DEF_NOT_READY, staticDefinitionIdx);
}

u32 FindTypeInStackByName(TCContext *context, SourceLocation loc, String name)
{
	u32 typeTableIdx = TYPETABLEIDX_Unset;

	TCScopeName scopeName = TCFindScopeName(context, loc, name);
	ASSERT(scopeName.type != NAMETYPE_INVALID);

	if (scopeName.type == NAMETYPE_PRIMITIVE)
		return scopeName.primitiveTypeTableIdx;
	else if (scopeName.type != NAMETYPE_STATIC_DEFINITION)
		LogError(context->global, loc, TPrintF("\"%S\" is not a type!", name));

	StaticDefinition staticDefinition =
		GetStaticDefinition(context, scopeName.staticDefinitionIdx, true);

	if (staticDefinition.definitionType != STATICDEFINITIONTYPE_TYPE)
		LogError(context->global, loc, TPrintF("\"%S\" is not a type!", name));

	typeTableIdx = staticDefinition.typeTableIdx;
	return typeTableIdx;
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
	TYPECHECK_PARAMETER_DEFAULT_VALUE_MISMATCH,
	TYPECHECK_CALLLING_CONVENTION_MISMATCH,
	TYPECHECK_PARAMETER_COUNT_MISMATCH,
	TYPECHECK_VARARGS_MISMATCH,
	TYPECHECK_RETURN_COUNT_MISMATCH,
	TYPECHECK_CANT_DEDUCE_TYPE,
};

void ReportTypeCheckError(JobContext *context, TypeCheckErrorCode errorCode, SourceLocation sourceLoc,
		u32 leftTypeIdx, u32 rightTypeIdx)
{
	String leftStr  = TypeInfoToString(context, leftTypeIdx);
	String rightStr = TypeInfoToString(context, rightTypeIdx);
	switch (errorCode)
	{
	case TYPECHECK_SIGN_MISMATCH:
		LogError(context->global, sourceLoc, TPrintF(
			"Integer sign mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_SIZE_MISMATCH:
		LogError(context->global, sourceLoc, TPrintF(
			"Integer size mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_TYPE_CATEGORY_MISMATCH:
		LogError(context->global, sourceLoc, TPrintF(
			"Expression type mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_POINTED_TYPE_MISMATCH:
		LogError(context->global, sourceLoc, TPrintF(
			"Unrelated pointed types! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_ARRAY_SIZE_MISMATCH:
		LogError(context->global, sourceLoc, TPrintF(
			"Size of arrays are different! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_STRUCT_MISMATCH:
		LogError(context->global, sourceLoc, TPrintF(
			"Expressions evaluate to different structs! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_MISC_PANIC:
		LogError(context->global, sourceLoc, TPrintF(
			"Expression type mismatch! (left is %S and right is %S)", leftStr, rightStr));
	default:
		ASSERT(errorCode == TYPECHECK_COOL);
	}
}

inline u32 StripImplicitlyCastAliases(Context *context, u32 typeTableIdx) {
	auto &typeTable = context->typeTable.unsafe;
	TypeInfo typeInfo = typeTable[typeTableIdx];
	while (typeInfo.typeCategory == TYPECATEGORY_ALIAS &&
		   typeInfo.aliasInfo.doesImplicitlyCast) {
		typeTableIdx = typeInfo.aliasInfo.aliasedTypeIdx;
		typeInfo = typeTable[typeTableIdx];
	}
	return typeTableIdx;
}

inline u32 StripAllAliases(Context *context, u32 typeTableIdx) {
	auto &typeTable = context->typeTable.unsafe;
	TypeInfo typeInfo = typeTable[typeTableIdx];
	while (typeInfo.typeCategory == TYPECATEGORY_ALIAS) {
		typeTableIdx = typeInfo.aliasInfo.aliasedTypeIdx;
		typeInfo = typeTable[typeTableIdx];
	}
	return typeTableIdx;
}

u32 GetTypeInfoPointerOf(JobContext *context, u32 inType);
TypeCheckErrorCode CheckTypesMatch(JobContext *context, u32 leftTypeIdx, u32 rightTypeIdx)
{
	// Get rid of implicitly cast aliases
	leftTypeIdx  = StripImplicitlyCastAliases(context->global, leftTypeIdx);
	rightTypeIdx = StripImplicitlyCastAliases(context->global, rightTypeIdx);

	if ((leftTypeIdx == TYPETABLEIDX_VOID) != (rightTypeIdx == TYPETABLEIDX_VOID))
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;

	if (leftTypeIdx == rightTypeIdx)
		return TYPECHECK_COOL;

	// Allow anything to cast to Any
	if (leftTypeIdx == TYPETABLEIDX_ANY_STRUCT || rightTypeIdx == TYPETABLEIDX_ANY_STRUCT)
		return TYPECHECK_COOL;

	TypeInfo left  = GetTypeInfo(context, leftTypeIdx);
	TypeInfo right = GetTypeInfo(context, rightTypeIdx);

	if (leftTypeIdx == TYPETABLEIDX_BOOL) {
		if (right.typeCategory == TYPECATEGORY_INTEGER ||
			right.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}

	if (rightTypeIdx == TYPETABLEIDX_INTEGER) {
		if (left.typeCategory == TYPECATEGORY_INTEGER ||
			left.typeCategory == TYPECATEGORY_POINTER || // @Check: Allow ptr = int? Confusing.
			left.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;

		// Allow implicit cast of <number> to <alias of number type>
		if (left.typeCategory == TYPECATEGORY_ALIAS) {
			u32 leftTypeIdxStripped = StripAllAliases(context->global, leftTypeIdx);
			TypeCategory strippedTypeCategory =
				GetTypeInfo(context, leftTypeIdxStripped).typeCategory;
			if (strippedTypeCategory == TYPECATEGORY_INTEGER ||
				strippedTypeCategory == TYPECATEGORY_FLOATING)
				return TYPECHECK_COOL;
		}

		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}
	else if (leftTypeIdx == TYPETABLEIDX_INTEGER) {
		if (right.typeCategory == TYPECATEGORY_INTEGER ||
			right.typeCategory == TYPECATEGORY_POINTER || // @Check: Allow ptr = int? Confusing.
			right.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;

		// Allow implicit cast of <number> to <alias of number type>
		if (right.typeCategory == TYPECATEGORY_ALIAS) {
			u32 rightTypeIdxStripped = StripAllAliases(context->global, rightTypeIdx);
			TypeCategory strippedTypeCategory =
				GetTypeInfo(context, rightTypeIdxStripped).typeCategory;
			if (strippedTypeCategory == TYPECATEGORY_INTEGER ||
				strippedTypeCategory == TYPECATEGORY_FLOATING)
				return TYPECHECK_COOL;
		}

		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}
	else if (rightTypeIdx == TYPETABLEIDX_FLOATING) {
		if (left.typeCategory == TYPECATEGORY_FLOATING)
			return TYPECHECK_COOL;
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}

	if (left.typeCategory != right.typeCategory) {
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
		if ((left.typeCategory == TYPECATEGORY_POINTER &&
			left.pointerInfo.pointedTypeTableIdx == TYPETABLEIDX_VOID &&
			right.typeCategory == TYPECATEGORY_PROCEDURE) ||
			(right.typeCategory == TYPECATEGORY_POINTER &&
			right.pointerInfo.pointedTypeTableIdx == TYPETABLEIDX_VOID &&
			left.typeCategory == TYPECATEGORY_PROCEDURE))
			return TYPECHECK_COOL;
#endif

		return TYPECHECK_TYPE_CATEGORY_MISMATCH;
	}

	switch (left.typeCategory) {
	case TYPECATEGORY_POINTER:
	{
		u32 pointedTypeIdxLeft  = StripImplicitlyCastAliases(context->global,
				left.pointerInfo.pointedTypeTableIdx);
		u32 pointedTypeIdxRight = StripImplicitlyCastAliases(context->global,
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
		if (left.size < right.size)
			return TYPECHECK_SIZE_MISMATCH;
		if (left.size == right.size && left.integerInfo.isSigned != right.integerInfo.isSigned)
			// Only when converting to same size (or less, but that goes to the error above).
			return TYPECHECK_SIGN_MISMATCH;
		return TYPECHECK_COOL;
	} break;
	case TYPECATEGORY_FLOATING:
	{
		return TYPECHECK_COOL;
	} break;
	case TYPECATEGORY_PROCEDURE:
	{
		if (left.procedureInfo.callingConvention != right.procedureInfo.callingConvention)
			return TYPECHECK_CALLLING_CONVENTION_MISMATCH;
		if (left.procedureInfo.parameters.size != right.procedureInfo.parameters.size)
			return TYPECHECK_PARAMETER_COUNT_MISMATCH;
		if (left.procedureInfo.isVarargs != right.procedureInfo.isVarargs)
			return TYPECHECK_VARARGS_MISMATCH;
		if (left.procedureInfo.returnTypeIndices.size != right.procedureInfo.returnTypeIndices.size)
			return TYPECHECK_RETURN_COUNT_MISMATCH;
		for (int i = 0; i < left.procedureInfo.returnTypeIndices.size; ++i) {
			u32 leftReturnTypeIdx  =  left.procedureInfo.returnTypeIndices[i];
			u32 rightReturnTypeIdx = right.procedureInfo.returnTypeIndices[i];
			TypeCheckErrorCode errorCode = CheckTypesMatch(context, leftReturnTypeIdx,
					rightReturnTypeIdx);
			if (errorCode != TYPECHECK_COOL)
				return errorCode;
		}
		for (int i = 0; i < left.procedureInfo.parameters.size; ++i) {
			ProcedureParameter leftParam = left.procedureInfo.parameters[i];
			ProcedureParameter rightParam = right.procedureInfo.parameters[i];
			if (leftParam.defaultValue.type != rightParam.defaultValue.type)
				return TYPECHECK_PARAMETER_DEFAULT_VALUE_MISMATCH;
			if (leftParam.defaultValue.type != CONSTANTTYPE_INVALID &&
					leftParam.defaultValue.valueAsInt != rightParam.defaultValue.valueAsInt)
				return TYPECHECK_PARAMETER_DEFAULT_VALUE_MISMATCH;
			u32 leftParamTypeIdx  = leftParam.typeTableIdx;
			u32 rightParamTypeIdx = rightParam.typeTableIdx;
			TypeCheckErrorCode errorCode = CheckTypesMatch(context, leftParamTypeIdx,
					rightParamTypeIdx);
			if (errorCode != TYPECHECK_COOL)
				return errorCode;
		}
		return TYPECHECK_COOL;
	} break;
	}

	return TYPECHECK_MISC_PANIC;
}

const StructMember *FindStructMemberByName(JobContext *context, TypeInfo structTypeInfo, String name)
{
	for (int i = 0; i < structTypeInfo.structInfo.members.size; ++i) {
		const StructMember *currentMember = &structTypeInfo.structInfo.members[i];
		if (StringEquals(name, currentMember->name))
			return currentMember;
		if (currentMember->isUsing || currentMember->name.size == 0) {
			// Anonymous structs/unions and using
			TypeInfo memberTypeInfo = GetTypeInfo(context, currentMember->typeTableIdx);
			ASSERT(memberTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
				   memberTypeInfo.typeCategory == TYPECATEGORY_UNION);
			const StructMember *found = FindStructMemberByName(context, memberTypeInfo, name);
			if (found) {
				if (currentMember->offset) {
					// Copy struct member and add the parent member's offset.
					StructMember *shiftedMember = ALLOC(LinearAllocator, StructMember);
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

void InferTypesInExpression(JobContext *context, ASTExpression *expression, u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
	if (expression->typeTableIdx == TYPETABLEIDX_INTEGER &&
			(typeInfo.typeCategory == TYPECATEGORY_INTEGER ||
			 typeInfo.typeCategory == TYPECATEGORY_FLOATING))
		expression->typeTableIdx = typeTableIdx;
	else if (expression->typeTableIdx == TYPETABLEIDX_FLOATING &&
			typeInfo.typeCategory == TYPECATEGORY_FLOATING)
		expression->typeTableIdx = typeTableIdx;

	// Recurse down
	switch (expression->nodeType) {
	case ASTNODETYPE_UNARY_OPERATION:
		InferTypesInExpression(context, expression->unaryOperation.expression, typeTableIdx);
		break;
	case ASTNODETYPE_BINARY_OPERATION:
		InferTypesInExpression(context, expression->binaryOperation.leftHand, typeTableIdx);
		InferTypesInExpression(context, expression->binaryOperation.rightHand, typeTableIdx);
		break;
	}
}

struct TypeCheckResult
{
	TypeCheckErrorCode errorCode;
	u32 leftTypeIdx;
	u32 rightTypeIdx;
};
TypeCheckResult CheckTypesMatchAndSpecialize(JobContext *context, u32 leftTypeIdx, ASTExpression *rightHand)
{
	u32 rightTypeIdx = rightHand->typeTableIdx;

	ASSERT(leftTypeIdx  != TYPETABLEIDX_Unset);
	ASSERT(rightTypeIdx != TYPETABLEIDX_Unset);

	// Get rid of aliases
	if (leftTypeIdx >= TYPETABLEIDX_Begin)
		leftTypeIdx  = StripImplicitlyCastAliases(context->global, leftTypeIdx);
	if (rightTypeIdx >= TYPETABLEIDX_Begin)
		rightTypeIdx = StripImplicitlyCastAliases(context->global, rightTypeIdx);

	TypeCheckResult result = { TYPECHECK_COOL, leftTypeIdx, rightTypeIdx };

	if (rightTypeIdx == TYPETABLEIDX_StructLiteral) {
		ASSERT(rightHand->nodeType == ASTNODETYPE_LITERAL);
		ASSERT(rightHand->literal.type == LITERALTYPE_GROUP);

		u32 structTypeIdx = leftTypeIdx;
		TypeInfo structTypeInfo = GetTypeInfo(context, structTypeIdx);
		if (structTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			structTypeInfo.typeCategory == TYPECATEGORY_UNION)
		{
			struct StructStackFrame
			{
				u32 structTypeIdx;
				int idx;
			};
			DynamicArray<StructStackFrame, ThreadAllocator> structStack;
			DynamicArrayInit(&structStack, 8);
			*DynamicArrayAdd(&structStack) = { structTypeIdx, 0 };

			int memberIdx = 0;
			while (memberIdx < rightHand->literal.members.size) {
				StructStackFrame currentFrame = structStack[structStack.size - 1];
				TypeInfo currentStructTypeInfo = GetTypeInfo(context, currentFrame.structTypeIdx);

				if (currentFrame.idx >= currentStructTypeInfo.structInfo.members.size) {
					// Pop struct frame
					--structStack.size;
					if (structStack.size == 0)
						LogError(context->global, rightHand->any.loc, "Too many values in struct literal"_s);
					continue;
				}

				u32 currentMemberTypeIdx =
					currentStructTypeInfo.structInfo.members[currentFrame.idx].typeTableIdx;
				TypeInfo currentMemberTypeInfo = GetTypeInfo(context, currentMemberTypeIdx);

				if (currentMemberTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
					currentMemberTypeInfo.typeCategory == TYPECATEGORY_UNION) {
					// Push struct frame
					structStack[structStack.size++] = { currentMemberTypeIdx, 0 };
					continue;
				}

				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];
				if (literalMemberExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
					// Named member assignments handled in next loop
					break;
				else {
					TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
							currentMemberTypeIdx, literalMemberExp);
					literalMemberExp->typeTableIdx = typeCheckResult.rightTypeIdx;
					if (typeCheckResult.errorCode != TYPECHECK_COOL) {
						Print("Type of struct literal value in position %d and "
								"type of struct member number %d don't match\n", memberIdx, memberIdx);
						ReportTypeCheckError(context, typeCheckResult.errorCode, rightHand->any.loc,
								currentMemberTypeIdx, literalMemberExp->typeTableIdx);
					}
					++structStack[structStack.size - 1].idx;
				}
				++memberIdx;
			}

			for (; memberIdx < rightHand->literal.members.size; ++memberIdx) {
				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];
				if (literalMemberExp->nodeType != ASTNODETYPE_BINARY_OPERATION ||
					literalMemberExp->binaryOperation.op != TOKEN_OP_ASSIGNMENT)
					LogError(context->global, literalMemberExp->any.loc, "Non-named member found after "
							"named members in group literal"_s);

				ASTExpression *leftExp  = literalMemberExp->binaryOperation.leftHand;
				ASTExpression *rightExp = literalMemberExp->binaryOperation.rightHand;

				ASSERT(leftExp->nodeType == ASTNODETYPE_IDENTIFIER); // We check this earlier.
				String memberName = leftExp->identifier.string;

				const StructMember *member = FindStructMemberByName(context, structTypeInfo, memberName);
				leftExp->identifier.structMember = member;

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
						member->typeTableIdx, rightExp);
				rightExp->typeTableIdx = typeCheckResult.rightTypeIdx;
				if (typeCheckResult.errorCode != TYPECHECK_COOL) {
					Print("Type of struct literal value \"%S\" and "
							"type of struct member don't match\n", memberName);
					ReportTypeCheckError(context, typeCheckResult.errorCode, rightHand->any.loc,
							member->typeTableIdx, rightExp->typeTableIdx);
				}
			}

			result.rightTypeIdx = structTypeIdx;
		}
		else if (structTypeInfo.typeCategory == TYPECATEGORY_ARRAY) {
			if (structTypeInfo.arrayInfo.count < rightHand->literal.members.size)
				LogError(context->global, rightHand->any.loc, "Too many values in array literal"_s);

			for (int memberIdx = 0; memberIdx < rightHand->literal.members.size; ++memberIdx) {
				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];

				if (literalMemberExp->nodeType == ASTNODETYPE_IDENTIFIER &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
					LogError(context->global, literalMemberExp->any.loc, "Named members not allowed in array literals"_s);

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
						structTypeInfo.arrayInfo.elementTypeTableIdx,
						literalMemberExp);
				literalMemberExp->typeTableIdx = typeCheckResult.rightTypeIdx;
				if (typeCheckResult.errorCode != TYPECHECK_COOL) {
					Print("Type of element %d in array literal doesn't match with type of array", memberIdx);
					ReportTypeCheckError(context, typeCheckResult.errorCode, rightHand->any.loc,
							structTypeInfo.arrayInfo.elementTypeTableIdx,
							rightHand->literal.members[memberIdx]->typeTableIdx);
				}
			}

			result.rightTypeIdx = structTypeIdx;
		}
		else
			ASSERT(false);
	}
	else if (rightTypeIdx == TYPETABLEIDX_Anything) {
		if (leftTypeIdx == TYPETABLEIDX_Anything)
			result.errorCode = TYPECHECK_CANT_DEDUCE_TYPE;
		result.rightTypeIdx = leftTypeIdx;
	}
	else if (leftTypeIdx == TYPETABLEIDX_Anything) {
		result.leftTypeIdx = rightTypeIdx;
		return result;
	}
	else {
		u32 strippedLeftTypeIdx  = StripAllAliases(context->global, leftTypeIdx);
		u32 strippedRightTypeIdx = StripAllAliases(context->global, rightTypeIdx);
		TypeCategory strippedLeftTypeCat  = GetTypeInfo(context, strippedLeftTypeIdx).typeCategory;
		TypeCategory strippedRightTypeCat = GetTypeInfo(context, strippedRightTypeIdx).typeCategory;

		if (leftTypeIdx == TYPETABLEIDX_INTEGER && (strippedRightTypeCat == TYPECATEGORY_INTEGER ||
			strippedRightTypeCat == TYPECATEGORY_POINTER || strippedRightTypeCat == TYPECATEGORY_FLOATING ||
			strippedRightTypeCat == TYPECATEGORY_ALIAS))
			// INTEGER converts to whatever is on the right as long as it makes sense
			result.leftTypeIdx = rightTypeIdx;
		else if (rightTypeIdx == TYPETABLEIDX_INTEGER && (strippedLeftTypeCat == TYPECATEGORY_INTEGER ||
			strippedLeftTypeCat == TYPECATEGORY_POINTER || strippedLeftTypeCat == TYPECATEGORY_FLOATING ||
			strippedLeftTypeCat == TYPECATEGORY_ALIAS)) {
			// INTEGER converts to whatever is on the left as long as it makes sense
			result.rightTypeIdx = leftTypeIdx;
			InferTypesInExpression(context, rightHand, leftTypeIdx);
		}

		else if (leftTypeIdx == TYPETABLEIDX_FLOATING && strippedRightTypeCat == TYPECATEGORY_FLOATING)
			// FLOATING converts to whatever is on the right as long as it's a floating type
			result.leftTypeIdx = rightTypeIdx;
		else if (rightTypeIdx == TYPETABLEIDX_FLOATING && strippedLeftTypeCat == TYPECATEGORY_FLOATING) {
			// FLOATING converts to whatever is on the left as long as it's a floating type
			result.rightTypeIdx = leftTypeIdx;
			InferTypesInExpression(context, rightHand, leftTypeIdx);
		}

		else
			// Normal type checking
			result.errorCode = CheckTypesMatch(context, leftTypeIdx, rightTypeIdx);
	}

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
	case TYPECATEGORY_NOT_READY:
		return false;
	case TYPECATEGORY_INTEGER:
		return a.integerInfo.isSigned == b.integerInfo.isSigned;
	case TYPECATEGORY_FLOATING:
		return true;
	case TYPECATEGORY_STRUCT:
	case TYPECATEGORY_UNION:
		// Even if we declare exactly the same struct twice, they are still different types!
		return false;
	case TYPECATEGORY_ENUM:
		return false;
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
		if (a.procedureInfo.returnTypeIndices.size != b.procedureInfo.returnTypeIndices.size)
			return false;
		for (int i = 0; i < a.procedureInfo.returnTypeIndices.size; ++i) {
			TypeInfo aReturnTypeInfo =
				context->typeTable.unsafe[a.procedureInfo.returnTypeIndices[i]];
			TypeInfo bReturnTypeInfo =
				context->typeTable.unsafe[b.procedureInfo.returnTypeIndices[i]];
			if (!AreTypeInfosEqual(context, aReturnTypeInfo, bReturnTypeInfo))
				return false;
		}
		for (int i = 0; i < a.procedureInfo.parameters.size; ++i) {
			ProcedureParameter aParam = a.procedureInfo.parameters[i];
			ProcedureParameter bParam = b.procedureInfo.parameters[i];
			if (aParam.defaultValue.type != bParam.defaultValue.type)
				return false;
			if (aParam.defaultValue.type != CONSTANTTYPE_INVALID &&
					aParam.defaultValue.valueAsInt != bParam.defaultValue.valueAsInt)
				return false;
			if (aParam.typeTableIdx != bParam.typeTableIdx) {
				TypeInfo aParamTypeInfo = context->typeTable.unsafe[aParam.typeTableIdx];
				TypeInfo bParamTypeInfo = context->typeTable.unsafe[bParam.typeTableIdx];
				if (!AreTypeInfosEqual(context, aParamTypeInfo, bParamTypeInfo))
					return false;
			}
		}
		return true;
	}
	case TYPECATEGORY_ALIAS:
	{
		if (!a.aliasInfo.doesImplicitlyCast || !b.aliasInfo.doesImplicitlyCast)
			return false;
		if (!StringEquals(a.aliasInfo.name, b.aliasInfo.name))
			return false;
		return a.aliasInfo.aliasedTypeIdx == b.aliasInfo.aliasedTypeIdx;
	}
	case TYPECATEGORY_INVALID:
		return b.typeCategory == TYPECATEGORY_INVALID;
	default:
		CRASH;
	}
}

void *AllocateStaticData(Context *context, u32 valueIdx, u64 size, int alignment)
{
	ASSERT(valueIdx & VALUE_GLOBAL_BIT);

	u8 *result;
	{
		ScopedLockSpin lock(&context->staticDataLock);

		result = STATIC_DATA_VIRTUAL_ADDRESS + context->staticDataSize;
		// Align
		ASSERT(IsPowerOf2(alignment));
		int alignmentMask = alignment - 1;
		if ((u64)result & alignmentMask) {
			result = (u8 *)(((u64)result + alignment) & (~alignmentMask));
		}

		u8 *end = result + size;
		while (end > STATIC_DATA_VIRTUAL_ADDRESS + context->staticDataAllocatedSpace) {
			// Allocate more memory
			void *newMem = VirtualAlloc(STATIC_DATA_VIRTUAL_ADDRESS + context->staticDataAllocatedSpace,
					0x100000, MEM_COMMIT, PAGE_READWRITE);
			memset(newMem, 0xCC, 0x100000);
			context->staticDataAllocatedSpace += 0x100000;
		}
		context->staticDataSize = result + size - STATIC_DATA_VIRTUAL_ADDRESS;
	}

	if (valueIdx != U32_MAX) {
		{
			// @Improve: same lock for both of these?
			ScopedLockSpin lock(&context->globalValuesLock);
			*HashMapGetOrAdd(&context->globalValueContents, valueIdx & VALUE_GLOBAL_MASK) = result;
		}

		// Wake up any jobs that were waiting for this global value to be allocated
		WakeUpAllByIndex(context, YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED, valueIdx);
	}

	return result;
}

inline void AddStaticDataPointerToRelocate(Context *context, void *ptr)
{
	ASSERT(ptr >= STATIC_DATA_VIRTUAL_ADDRESS &&
		   ptr <  STATIC_DATA_VIRTUAL_ADDRESS_END);

	ASSERT(*(void **)ptr == nullptr || (
		   *(void **)ptr >= STATIC_DATA_VIRTUAL_ADDRESS &&
		   *(void **)ptr <  STATIC_DATA_VIRTUAL_ADDRESS_END));

	ScopedLockSpin lock(&context->staticDataLock);
	*DynamicArrayAdd(&context->staticDataPointersToRelocate) = ptr;
}

bool DoesTypeContainPointers(JobContext *context, u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
	if (typeInfo.typeCategory == TYPECATEGORY_POINTER)
		return true;
	else if (typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			 typeInfo.typeCategory == TYPECATEGORY_UNION) {
		u64 memberCount = typeInfo.structInfo.members.size;
		for (int i = 0; i < memberCount; ++i) {
			// Recurse on each member
			StructMember *member = &typeInfo.structInfo.members[i];
			if (DoesTypeContainPointers(context, member->typeTableIdx))
				return true;
		}
	}
	return false;
}

void AddStaticDataPointersToRelocateInType(JobContext *context, void *ptr, u32 typeTableIdx)
{
	typeTableIdx = StripAllAliases(context->global, typeTableIdx);
	TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
	ASSERT(typeInfo.typeCategory != TYPECATEGORY_NOT_READY);

	// We don't allow unions that can have pointers inside since we wouldn't be certain what parts
	// are pointers most of the time.
	// @Todo: actual error message
	if (typeInfo.typeCategory == TYPECATEGORY_UNION) {
		u64 memberCount = typeInfo.structInfo.members.size;
		for (int i = 0; i < memberCount; ++i) {
			StructMember *member = &typeInfo.structInfo.members[i];
			if (DoesTypeContainPointers(context, member->typeTableIdx))
				ASSERT(!"Unions that have pointers somewhere inside are not allowed");
		}
	}

	if (typeInfo.typeCategory == TYPECATEGORY_POINTER)
		AddStaticDataPointerToRelocate(context->global, ptr);
	else if (typeInfo.typeCategory == TYPECATEGORY_STRUCT) {
		u64 memberCount = typeInfo.structInfo.members.size;
		for (int i = 0; i < memberCount; ++i) {
			// Recurse on each member
			StructMember *member = &typeInfo.structInfo.members[i];
			AddStaticDataPointersToRelocateInType(context, (u8 *)ptr + member->offset,
					member->typeTableIdx);
		}
	}
}

inline String CopyStringToStaticData(Context *context, String string, bool nullTerminate = false)
{
	char *nameCopy = (char *)AllocateStaticData(context, U32_MAX, string.size + nullTerminate, 1);
	memcpy(nameCopy, string.data, string.size);
	if (nullTerminate)
		nameCopy[string.size] = 0;
	return { string.size, nameCopy };
}

UserFacingTypeInfo *GetUserFacingTypeInfoPointer(JobContext *context, u32 typeTableIdx)
{
	UserFacingTypeInfo *result;

	// @Unsafe
	auto &typeTable = context->global->typeTable.unsafe;
	u32 typeInfoValueIdx = typeTable[typeTableIdx].valueIdx;

	SpinlockLock(&context->global->globalValuesLock);
	void **mapValue = HashMapGet(context->global->globalValueContents, typeInfoValueIdx & VALUE_GLOBAL_MASK);
	if (!mapValue) {
		SwitchJob(context, YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED, { .index = typeInfoValueIdx });
		// Lock again!
		SpinlockLock(&context->global->globalValuesLock);
		mapValue = HashMapGet(context->global->globalValueContents, typeInfoValueIdx & VALUE_GLOBAL_MASK);
		if (!mapValue)
			LogCompilerError(context->global, {}, "Bad job resume"_s);
	}
	SpinlockUnlock(&context->global->globalValuesLock);

	result = (UserFacingTypeInfo *)*mapValue;
	return result;
}

void WriteUserFacingTypeInfoToStaticData(JobContext *context, TypeInfo typeInfo)
{
	u64 size;
	switch (typeInfo.typeCategory) {
		case TYPECATEGORY_NOT_READY:
			// We'll hopefully write this once type is ready
			return;
		case TYPECATEGORY_INVALID:
			size = sizeof(UserFacingTypeInfo); break;
		case TYPECATEGORY_INTEGER:
			size = sizeof(UserFacingTypeInfoInteger); break;
		case TYPECATEGORY_FLOATING:
			size = sizeof(UserFacingTypeInfo); break;
		case TYPECATEGORY_STRUCT:
		case TYPECATEGORY_UNION:
			size = sizeof(UserFacingTypeInfoStruct); break;
		case TYPECATEGORY_ENUM:
			size = sizeof(UserFacingTypeInfoEnum); break;
		case TYPECATEGORY_POINTER:
			size = sizeof(UserFacingTypeInfoPointer); break;
		case TYPECATEGORY_ARRAY:
			size = sizeof(UserFacingTypeInfoArray); break;
		case TYPECATEGORY_PROCEDURE:
			size = sizeof(UserFacingTypeInfoProcedure); break;
		case TYPECATEGORY_ALIAS:
			size = sizeof(UserFacingTypeInfoAlias); break;
		default:
			ASSERT(false);
	}

	UserFacingTypeInfo *out;

	SpinlockLock(&context->global->globalValuesLock);
	void **existing = HashMapGet(context->global->globalValueContents, typeInfo.valueIdx & VALUE_GLOBAL_MASK);
	SpinlockUnlock(&context->global->globalValuesLock);
	if (existing)
		out = (UserFacingTypeInfo *)*existing;
	else
		out = (UserFacingTypeInfo *)AllocateStaticData(context->global, typeInfo.valueIdx, size, 8);

	out->size = typeInfo.size;
	switch (typeInfo.typeCategory) {
	case TYPECATEGORY_INTEGER:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_INTEGER;
		UserFacingTypeInfoInteger *outInteger = (UserFacingTypeInfoInteger *)out;
		outInteger->isSigned = typeInfo.integerInfo.isSigned;
	} break;
	case TYPECATEGORY_FLOATING:
		out->typeCategory = USERFACINGTYPECATEGORY_FLOATING;
		break;
	case TYPECATEGORY_STRUCT:
	case TYPECATEGORY_UNION:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_STRUCT;
		UserFacingTypeInfoStruct *outStruct = (UserFacingTypeInfoStruct *)out;
		ArrayView<UserFacingStructMember> members;
		u64 memberCount = typeInfo.structInfo.members.size;
		members.size = memberCount;
		members.data = (UserFacingStructMember *)AllocateStaticData(context->global, U32_MAX, memberCount *
				sizeof(UserFacingStructMember), 8);
		for (int i = 0; i < memberCount; ++i) {
			StructMember *origMember = &typeInfo.structInfo.members[i];
			UserFacingTypeInfo *memberUfti = GetUserFacingTypeInfoPointer(context,
					origMember->typeTableIdx);
			UserFacingStructMember member = {
				.name = CopyStringToStaticData(context->global, origMember->name),
				.typeInfo = memberUfti,
				.offset = origMember->offset
			};
			members[i] = member;

			AddStaticDataPointerToRelocate(context->global, &members[i].name.data);
			AddStaticDataPointerToRelocate(context->global, &members[i].typeInfo);
		}

		outStruct->name = CopyStringToStaticData(context->global, typeInfo.structInfo.name);
		outStruct->isUnion = typeInfo.typeCategory == TYPECATEGORY_UNION;
		outStruct->memberCount = members.size;
		outStruct->members = members.data;

		AddStaticDataPointerToRelocate(context->global, &outStruct->name.data);
		AddStaticDataPointerToRelocate(context->global, &outStruct->members);
	} break;
	case TYPECATEGORY_ENUM:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_ENUM;
		UserFacingTypeInfoEnum *outEnum = (UserFacingTypeInfoEnum *)out;
		outEnum->name = typeInfo.enumInfo.name;
		UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(context,
				typeInfo.enumInfo.typeTableIdx);
		outEnum->typeInfo = ufti;

		u64 memberCount = typeInfo.enumInfo.names.size;
		outEnum->nameCount = memberCount;
		outEnum->valueCount = memberCount;

		if (memberCount) {
			outEnum->names = (String *)AllocateStaticData(context->global, U32_MAX, memberCount * sizeof(String), 8);
			outEnum->values = (s64 *)AllocateStaticData(context->global, U32_MAX, memberCount * sizeof(s64), 8);
		}
		else {
			outEnum->names = nullptr;
			outEnum->values = nullptr;
		}

		for (int i = 0; i < memberCount; ++i) {
			outEnum->names[i]  = CopyStringToStaticData(context->global, typeInfo.enumInfo.names[i]);
			outEnum->values[i] = typeInfo.enumInfo.values[i];

			AddStaticDataPointerToRelocate(context->global, &outEnum->names[i].data);
		}

		AddStaticDataPointerToRelocate(context->global, &outEnum->typeInfo);
		AddStaticDataPointerToRelocate(context->global, &outEnum->names);
		AddStaticDataPointerToRelocate(context->global, &outEnum->values);
	} break;
	case TYPECATEGORY_POINTER:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_POINTER;
		UserFacingTypeInfoPointer *outPointer = (UserFacingTypeInfoPointer *)out;
		u32 pointedTypeIdx = typeInfo.pointerInfo.pointedTypeTableIdx;
		if (pointedTypeIdx == TYPETABLEIDX_VOID)
			outPointer->typeInfo = nullptr;
		else {
			UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(context, pointedTypeIdx);
			outPointer->typeInfo = ufti;
		}

		AddStaticDataPointerToRelocate(context->global, &outPointer->typeInfo);
	} break;
	case TYPECATEGORY_ARRAY:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_ARRAY;
		UserFacingTypeInfoArray *outArray = (UserFacingTypeInfoArray *)out;
		outArray->count = typeInfo.arrayInfo.count;
		UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(context,
				typeInfo.arrayInfo.elementTypeTableIdx);
		outArray->elementTypeInfo = ufti;

		AddStaticDataPointerToRelocate(context->global, &outArray->elementTypeInfo);
	} break;
	case TYPECATEGORY_PROCEDURE:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_PROCEDURE;
		UserFacingTypeInfoProcedure *outProcedure = (UserFacingTypeInfoProcedure *)out;
		u64 paramCount = typeInfo.procedureInfo.parameters.size;
		ArrayView<UserFacingTypeInfo *> parameters;
		parameters.size = paramCount;
		parameters.data = (UserFacingTypeInfo **)AllocateStaticData(context->global, U32_MAX, paramCount *
				sizeof(UserFacingTypeInfo *), 8);
		for (int i = 0; i < paramCount; ++i) {
			ProcedureParameter *origParam = &typeInfo.procedureInfo.parameters[i];
			UserFacingTypeInfo *paramUfti = GetUserFacingTypeInfoPointer(context,
					origParam->typeTableIdx);
			parameters[i] = paramUfti;

			AddStaticDataPointerToRelocate(context->global, &parameters[i]);
		}
		outProcedure->parameterCount = parameters.size;
		outProcedure->parameters = parameters.data;
		outProcedure->isVarargs = typeInfo.procedureInfo.isVarargs;

		AddStaticDataPointerToRelocate(context->global, &outProcedure->parameters);
	} break;
	case TYPECATEGORY_ALIAS:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_ALIAS;
		UserFacingTypeInfoAlias *outAlias = (UserFacingTypeInfoAlias *)out;
		UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(context,
				typeInfo.aliasInfo.aliasedTypeIdx);
		outAlias->typeInfo = ufti;

		AddStaticDataPointerToRelocate(context->global, &outAlias->typeInfo);
	} break;
	}
}

inline u32 AddType(JobContext *context, TypeInfo typeInfo)
{
	// Should lock type table before calling
	ASSERT(context->global->typeTable.lock != 0);
	typeInfo.valueIdx = NewGlobalValue(context->global, String{}, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);

	u32 typeTableIdx;
	{
		auto *typeTable = &context->global->typeTable.unsafe;

		s64 typeCount = typeTable->count;
		ASSERT(typeCount < U32_MAX); // Out of type memory
		typeTableIdx = (u32)typeCount;

		*(TypeInfo *)BucketArrayAdd(typeTable) = typeInfo;
	}
	SpinlockUnlock(&context->global->typeTable.lock);

	{
		Value value = GetGlobalValue(context->global, typeInfo.valueIdx);
#if DEBUG_BUILD
		value.name = SNPrintF(16, "_typeInfo%lld", typeTableIdx);
#endif
		UpdateGlobalValue(context->global, typeInfo.valueIdx, &value);
	}

	// Set up static data for user-facing type info
	WriteUserFacingTypeInfoToStaticData(context, typeInfo);

	return typeTableIdx;
}

u32 FindOrAddTypeTableIdx(JobContext *context, TypeInfo typeInfo)
{
	{
		// Optimize most frequent path. Read without locking.
		auto &typeTable = context->global->typeTable.unsafe;

		u32 tableSize = (u32)typeTable.count;
		for (u32 i = 0; i < tableSize; ++i)
		{
			TypeInfo t = typeTable[i];
			if (AreTypeInfosEqual(context->global, typeInfo, t))
				return i;
		}
	}
	{
		// Check it didn't get added when we released the lock
		// @Speed: ugh...
		SpinlockLock(&context->global->typeTable.lock);
		auto &typeTable = context->global->typeTable.unsafe;

		u32 tableSize = (u32)typeTable.count;
		for (u32 i = 0; i < tableSize; ++i)
		{
			TypeInfo t = typeTable[i];
			if (AreTypeInfosEqual(context->global, typeInfo, t)) {
				SpinlockUnlock(&context->global->typeTable.lock);
				return i;
			}
		}
		return AddType(context, typeInfo);
	}
}

// Util TypeInfo procedures
u32 GetTypeInfoPointerOf(JobContext *context, u32 inType)
{
	ASSERT(inType < (u32)context->global->typeTable.unsafe.count);

	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_POINTER;
	resultTypeInfo.size = g_pointerSize;
	resultTypeInfo.pointerInfo.pointedTypeTableIdx = inType;
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

u32 GetTypeInfoArrayOf(JobContext *context, u32 inType, s64 count)
{
	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_ARRAY;
	resultTypeInfo.arrayInfo.elementTypeTableIdx = inType;
	resultTypeInfo.arrayInfo.count = count;
	if (count == 0)
		resultTypeInfo.size = 8 + g_pointerSize;
	else {
		s64 elementSize = GetTypeInfo(context, inType).size;
		resultTypeInfo.size = elementSize * count;
	}
	return FindOrAddTypeTableIdx(context, resultTypeInfo);
}

u32 TypeCheckType(TCContext *context, String name, SourceLocation loc, ASTType *astType);

void TCAddScopeNames(TCContext *context, ArrayView<TCScopeName> scopeNames)
{
	// Primitives
	for (int i = 0; i < context->global->tcPrimitiveTypes.size; ++i) {
		const TCScopeName *currentName = &context->global->tcPrimitiveTypes[i];
		ASSERT(currentName->type == NAMETYPE_PRIMITIVE);
		for (int j = 0; j < scopeNames.size; ++j) {
			if (StringEquals(scopeNames[j].name, currentName->name))
				LogError(context->global, scopeNames[j].loc, TPrintF("Can not use name \"%S\", it is a "
						"language primitive", scopeNames[j].name));
		}
	}

	TCScope *stackTop = TCGetTopMostScope(context);
	if (stackTop) {
		// Check if already exists
		for (int i = 0; i < stackTop->names.size; ++i) {
			TCScopeName *currentName = &stackTop->names[i];
			for (int j = 0; j < scopeNames.size; ++j) {
				if (StringEquals(scopeNames[j].name, currentName->name)) {
					LogErrorNoCrash(context->global, scopeNames[j].loc, TPrintF("Name \"%S\" already assigned",
								scopeNames[j].name));
					LogNote(context->global, currentName->loc, "First defined here"_s);
					PANIC;
				}
			}
		}

		TCScopeName *newNames = DynamicArrayAddMany(&stackTop->names, scopeNames.size);
		for (int i = 0; i < scopeNames.size; ++i)
			newNames[i] = scopeNames[i];
	}
	else {
		{
			auto globalNames = context->global->tcGlobalNames.GetForWrite();

			// Check if already exists
			for (int i = 0; i < globalNames->size; ++i) {
				const TCScopeName *currentName = &globalNames[i];
				for (int j = 0; j < scopeNames.size; ++j) {
					if (StringEquals(scopeNames[j].name, currentName->name)) {
						LogErrorNoCrash(context->global, scopeNames[j].loc, TPrintF("Name \"%S\" already "
									"assigned", scopeNames[j].name));
						LogNote(context->global, currentName->loc, "First defined here"_s);
						PANIC;
					}
				}
			}

			TCScopeName *newNames = DynamicArrayAddMany(&globalNames, scopeNames.size);
			for (int i = 0; i < scopeNames.size; ++i)
				newNames[i] = scopeNames[i];
		}

		// Wake up any jobs that were waiting for these names
		auto jobsWaiting = context->global->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
		for (int i = 0; i < jobsWaiting->size; ) {
			u32 jobIdx = jobsWaiting[i];
			const Job *job = &context->global->jobs.unsafe[jobIdx];
			for (int nameIdx = 0; nameIdx < scopeNames.size; ++nameIdx) {
				if (StringEquals(job->context.identifier, scopeNames[nameIdx].name)) {
					EnqueueReadyJob(context->global, jobIdx);
					// Remove waiting job
					DynamicArraySwapRemove(&jobsWaiting, i);
					goto outerContinue;
				}
			}
			++i; // Didn't remove waiting job
outerContinue:
			continue;
		}
	}
}

void TypeCheckExpression(TCContext *context, ASTExpression *expression);
Constant TryEvaluateConstant(JobContext *context, ASTExpression *expression);
u32 TypeCheckStructDeclaration(TCContext *context, String name, bool isUnion,
		ASTStructDeclaration astStructDecl)
{
	u8 alignment = 0;
	if (astStructDecl.alignExp) {
		TypeCheckExpression(context, astStructDecl.alignExp);
		Constant constant = TryEvaluateConstant(context, astStructDecl.alignExp);
		if (constant.type == CONSTANTTYPE_INVALID)
			LogError(context->global, astStructDecl.alignExp->any.loc, "Could not evaluate alignment"_s);
		if (constant.type != CONSTANTTYPE_INTEGER)
			LogError(context->global, astStructDecl.alignExp->any.loc, "Alignment expression did not "
					"evaluate to an integer"_s);
		if (constant.valueAsInt > U8_MAX)
			LogError(context->global, astStructDecl.alignExp->any.loc, "Alignment specified is too large"_s);
		alignment = (u8)constant.valueAsInt;
	}

	u32 typeTableIdx;
	if (StringEquals(name, "String"_s))
		typeTableIdx = TYPETABLEIDX_STRING_STRUCT;
	else if (StringEquals(name, "Array"_s))
		typeTableIdx = TYPETABLEIDX_ARRAY_STRUCT;
	else if (StringEquals(name, "Any"_s))
		typeTableIdx = TYPETABLEIDX_ANY_STRUCT;
	else if (StringEquals(name, "TypeInfo"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;
	else if (StringEquals(name, "TypeInfoInteger"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT;
	else if (StringEquals(name, "TypeInfoStructMember"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT;
	else if (StringEquals(name, "TypeInfoStruct"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT;
	else if (StringEquals(name, "TypeInfoEnum"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT;
	else if (StringEquals(name, "TypeInfoPointer"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT;
	else if (StringEquals(name, "TypeInfoArray"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT;
	else if (StringEquals(name, "TypeInfoProcedurePointer"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_PROCEDURE_STRUCT;
	else if (StringEquals(name, "TypeInfoAlias"_s))
		typeTableIdx = TYPETABLEIDX_TYPE_INFO_ALIAS_STRUCT;
	else {
		SpinlockLock(&context->global->typeTable.lock);
		ASSERT(context->global->typeTable.unsafe.count < U32_MAX);
		TypeInfo t = {
			.typeCategory = TYPECATEGORY_NOT_READY,
			.alignment = alignment,
			.structInfo = {
				.name = name
			}
		};
		typeTableIdx = AddType(context, t);

		// Allocate static data but don't fill it
		AllocateStaticData(context->global, context->global->typeTable.unsafe[typeTableIdx].valueIdx,
				sizeof(UserFacingTypeInfoStruct), 8);
	}

	TCScope *stackTop = TCGetTopMostScope(context);
	if (stackTop)
		*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;
	else {
		auto globalTypes = context->global->tcGlobalTypeIndices.GetForWrite();
		*DynamicArrayAdd(&globalTypes) = typeTableIdx;
	}

	// Finish type checking struct members on another fiber. This takes care of circular
	// dependencies where two structs each have a member whose type is of the other struct.
	TCStructJobArgs *args = ALLOC(LinearAllocator, TCStructJobArgs);
	*args = {
		.context = context->global,
		.typeTableIdx = typeTableIdx,
		.astStructDecl = astStructDecl,
		.name = name,
		.isUnion = isUnion
	};
	RequestNewJob(context->global, JOBTYPE_TYPE_CHECK, TCStructJobProc, (void *)args);

	return typeTableIdx;
}

Constant ConstantFromCTRegister(CTRegister ctRegister, u32 typeTableIdx)
{
	Constant result;
	switch (typeTableIdx) {
	case TYPETABLEIDX_S8:
	case TYPETABLEIDX_S16:
	case TYPETABLEIDX_S32:
	case TYPETABLEIDX_S64:
	case TYPETABLEIDX_U8:
	case TYPETABLEIDX_U16:
	case TYPETABLEIDX_U32:
	case TYPETABLEIDX_U64:
		result.type = CONSTANTTYPE_INTEGER;
		result.valueAsInt = ctRegister.asS64;
		break;
	case TYPETABLEIDX_F32:
		result.type = CONSTANTTYPE_FLOATING;
		result.valueAsFloat = (f64)ctRegister.asF32;
		break;
	case TYPETABLEIDX_F64:
		result.type = CONSTANTTYPE_FLOATING;
		result.valueAsFloat = ctRegister.asF64;
		break;
	default:
		ASSERT(false);
	}
	return result;
}

Constant TryEvaluateConstant(JobContext *context, ASTExpression *expression)
{
	Constant result;
	result.type = CONSTANTTYPE_INVALID;
	result.valueAsInt = 0xFA11FA11FA11FA11;
	result.typeTableIdx = expression->typeTableIdx;

	switch (expression->nodeType) {
	case ASTNODETYPE_RUN:
	{
		result = expression->runNode.result;
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type) {
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
			Array<Constant, LinearAllocator> constants;
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
		case LITERALTYPE_STRING:
		{
			result.type = CONSTANTTYPE_STRING;
			result.valueAsString = expression->literal.string;
		} break;
		default:
			goto error;
		}
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		if (expression->identifier.type == NAMETYPE_STATIC_DEFINITION) {
			result.type = CONSTANTTYPE_INTEGER;
			result = GetStaticDefinition(context,
					expression->identifier.staticDefinitionIdx, false).constant;
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
		case TOKEN_OP_NOT_EQUALS:
		{
			result.type = CONSTANTTYPE_INTEGER;
			if (isFloat)
				result.valueAsInt = leftValue.valueAsFloat != rightValue.valueAsFloat;
			else
				result.valueAsInt = leftValue.valueAsInt != rightValue.valueAsInt;
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

bool TypeCheckProcedurePrototype(TCContext *context, ASTProcedurePrototype *prototype);
TypeInfo TypeInfoFromASTProcedurePrototype(JobContext *context, ASTProcedurePrototype *prototype);
u32 TypeCheckType(TCContext *context, String name, SourceLocation loc, ASTType *astType)
{
	switch (astType->nodeType)
	{
	case ASTTYPENODETYPE_IDENTIFIER:
	{
		return FindTypeInStackByName(context, astType->loc, astType->name);
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		u32 elementTypeIdx = TypeCheckType(context, {}, loc, astType->arrayType);
		s64 arrayCount = 0;
		if (astType->arrayCountExp) {
			TypeCheckExpression(context, astType->arrayCountExp);
			Constant constant = TryEvaluateConstant(context, astType->arrayCountExp);
			if (constant.type == CONSTANTTYPE_INVALID)
				LogError(context->global, loc, "Could not evaluate array count"_s);
			if (constant.type != CONSTANTTYPE_INTEGER)
				LogError(context->global, loc, "Array count did not evaluate to an integer"_s);
			arrayCount = constant.valueAsInt;
		}
		return GetTypeInfoArrayOf(context, elementTypeIdx, arrayCount);
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		u32 pointedTypeIdx = TypeCheckType(context, {}, loc, astType->pointedType);
		return GetTypeInfoPointerOf(context, pointedTypeIdx);
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	{
		return TypeCheckStructDeclaration(context, name, false, astType->structDeclaration);
	} break;
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		return TypeCheckStructDeclaration(context, name, true, astType->structDeclaration);
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		u32 innerTypeIdx = TYPETABLEIDX_S64;
		if (astType->enumDeclaration.astType)
		{
			SourceLocation astTypeLoc = astType->enumDeclaration.astType->loc;
			innerTypeIdx = TypeCheckType(context, {}, astTypeLoc,
					astType->enumDeclaration.astType);

			u32 strippedInnerTypeIdx = StripImplicitlyCastAliases(context->global, innerTypeIdx);
			if (strippedInnerTypeIdx < TYPETABLEIDX_PrimitiveBegin ||
				strippedInnerTypeIdx > TYPETABLEIDX_PrimitiveEnd)
				LogError(context->global, astTypeLoc, "Only primitive types are allowed as enum field types"_s);
		}

		TypeInfo t = {};
		t.typeCategory = TYPECATEGORY_ENUM;
		t.enumInfo.name = name;
		t.enumInfo.typeTableIdx = innerTypeIdx;
		t.size = GetTypeInfo(context, innerTypeIdx).size;
		u32 typeTableIdx;
		{
			SpinlockLock(&context->global->typeTable.lock);
			typeTableIdx = AddType(context, t);
		}

		u64 memberCount = astType->enumDeclaration.members.size;

		Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
		ArrayInit(&scopeNamesToAdd, memberCount);

		ArrayInit(&t.enumInfo.names,  memberCount);
		ArrayInit(&t.enumInfo.values, memberCount);

		Array<u32, LinearAllocator> valueStaticDefs;
		ArrayInit(&valueStaticDefs, memberCount);

		TCPushScope(context);

		s64 currentValue = 0;
		for (int memberIdx = 0; memberIdx < memberCount; ++memberIdx) {
			ASTEnumMember astMember = astType->enumDeclaration.members[memberIdx];

			ASTExpression *memberValue = astMember.value;
			if (memberValue)
				TypeCheckExpression(context, memberValue);

			StaticDefinition staticDefinition = {};
			staticDefinition.name = astMember.name;
			staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT;
			staticDefinition.typeTableIdx = typeTableIdx;

			if (astMember.value) {
				Constant constant = TryEvaluateConstant(context, astMember.value);
				if (constant.type == CONSTANTTYPE_INVALID)
					LogError(context->global, astMember.value->any.loc,
							"Failed to evaluate constant in static definition"_s);
				else if (constant.type == CONSTANTTYPE_FLOATING)
					currentValue = (s64)constant.valueAsFloat;
				else
					currentValue = constant.valueAsInt;
			}
			staticDefinition.constant.type = CONSTANTTYPE_INTEGER;
			staticDefinition.constant.valueAsInt = currentValue;
			staticDefinition.constant.typeTableIdx = typeTableIdx;

			u32 newStaticDefIdx = NewStaticDefinition(context->global, &staticDefinition);
			*ArrayAdd(&valueStaticDefs) = newStaticDefIdx;

			// Add scope names one by one since every one could depend on any previous one.
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_STATIC_DEFINITION;
			newScopeName.name = astMember.name;
			newScopeName.staticDefinitionIdx = newStaticDefIdx;
			newScopeName.loc = astMember.loc;
			// Add to the local scope we created above!
			TCAddScopeNames(context, { &newScopeName, 1 });
			*ArrayAdd(&scopeNamesToAdd) = newScopeName;

			*ArrayAdd(&t.enumInfo.names)  = astMember.name;
			*ArrayAdd(&t.enumInfo.values) = currentValue;
			++currentValue;
		}

		TCPopScope(context);
		// Add to outer scope
		TCAddScopeNames(context, scopeNamesToAdd);

		// Update type info
		{
			// Unsafe!
			auto &typeTable = context->global->typeTable.unsafe;
			t.valueIdx = typeTable[typeTableIdx].valueIdx;
			(TypeInfo&)typeTable[typeTableIdx] = t;

			WriteUserFacingTypeInfoToStaticData(context, t);
		}

		TCScope *stackTop = TCGetTopMostScope(context);
		if (stackTop)
			*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;
		else {
			auto globalTypes = context->global->tcGlobalTypeIndices.GetForWrite();
			*DynamicArrayAdd(&globalTypes) = typeTableIdx;
		}

		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		TypeCheckProcedurePrototype(context, &astType->procedurePrototype);

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

u32 InferType(u32 fromType) {
	if (fromType == TYPETABLEIDX_INTEGER)
		return TYPETABLEIDX_S64;
	else if (fromType == TYPETABLEIDX_FLOATING)
		return TYPETABLEIDX_F32;

	return fromType;
}

void TCAddStructMembersToScope(TCContext *context, SourceLocation loc, ASTExpression *valueExpression)
{
	// This procedure spills all members of a struct expression into the current stack.
	// The way we do this is, we add a TCScopeName which has a member access AST node, left hand of
	// which is an arbitrary AST tree branch that evaluates to a struct; and of which right hand is
	// a simple struct member node.
	TypeInfo typeInfo = GetTypeInfo(context, valueExpression->typeTableIdx);
	ASSERT(typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		   typeInfo.typeCategory == TYPECATEGORY_UNION);

	Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
	ArrayInit(&scopeNamesToAdd, typeInfo.structInfo.members.size);

	for (int memberIdx = 0; memberIdx < typeInfo.structInfo.members.size; ++memberIdx) {
		const StructMember *member = &typeInfo.structInfo.members[memberIdx];

		ASTExpression *memberAccessExp = TCNewTreeNode();
		{
			ASTExpression *rightHand = TCNewTreeNode();
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

		if (member->name.size && !member->isUsing) {
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_ASTEXPRESSION;
			newScopeName.name = member->name;
			newScopeName.expression = memberAccessExp;
			newScopeName.loc = loc;
			*ArrayAdd(&scopeNamesToAdd) = newScopeName;
		}
		else {
			// For using/anonymous members we recurse, spilling it's members too.
			TCAddStructMembersToScope(context, loc, memberAccessExp);
		}
	}
	TCAddScopeNames(context, scopeNamesToAdd);
}

bool IsExpressionAType(JobContext *context, ASTExpression *expression)
{
	switch (expression->nodeType) {
	case ASTNODETYPE_TYPE:
		return true;
	case ASTNODETYPE_IDENTIFIER:
		switch (expression->identifier.type) {
		case NAMETYPE_STATIC_DEFINITION:
		{
			u32 defIdx = expression->identifier.staticDefinitionIdx;
			StaticDefinition staticDef = GetStaticDefinition(context, defIdx, false);
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

void GetSourceLocRangeForExpression(ASTExpression *expression,
		SourceLocation *begin, SourceLocation *end)
{
	SourceLocation aBegin, aEnd, bBegin, bEnd;
	switch (expression->nodeType) {
	case ASTNODETYPE_BINARY_OPERATION:
		GetSourceLocRangeForExpression(expression->binaryOperation.leftHand,  &aBegin, &aEnd);
		GetSourceLocRangeForExpression(expression->binaryOperation.rightHand, &bBegin, &bEnd);
		break;
	case ASTNODETYPE_PROCEDURE_CALL:
		GetSourceLocRangeForExpression(expression->procedureCall.procedureExpression, &aBegin, &aEnd);
		GetSourceLocRangeForExpression(*DynamicArrayBack(&expression->procedureCall.arguments), &bBegin, &bEnd);
		break;
	case ASTNODETYPE_MULTIPLE_EXPRESSIONS:
		GetSourceLocRangeForExpression(expression->multipleExpressions.array[0], &aBegin, &aEnd);
		GetSourceLocRangeForExpression(*DynamicArrayBack(&expression->multipleExpressions.array),
				&bBegin, &bEnd);
		break;
	default:
		*begin = *end = expression->any.loc;
		return;
	}
	ASSERT(aBegin.fileIdx == bBegin.fileIdx);
	ASSERT(aBegin.fileIdx == aEnd.fileIdx);
	ASSERT(aBegin.fileIdx == bEnd.fileIdx);
	if (aBegin.character < bBegin.character)
		*begin = aBegin;
	else
		*begin = bBegin;
	if (aEnd.character > bEnd.character)
		*end = aEnd;
	else
		*end = bEnd;
}

void TypeCheckVariableDeclaration(TCContext *context, ASTVariableDeclaration *varDecl)
{
	bool isGlobal = varDecl->isStatic || varDecl->isExternal;

	if (context->onStaticContext && !isGlobal)
		LogError(context->global, varDecl->loc, "Variable on static scope has to be declared either "
				"static or external"_s);

	int varCount = (int)varDecl->nameCount;

	if (varCount > 1) {
		// Do one allocation for both
		varDecl->arrayOfValueIndices = ALLOC_N(LinearAllocator, u32, varCount * 2);
		varDecl->arrayOfTypeIndices  = varDecl->arrayOfValueIndices + varCount;
	}

	varDecl->anonymousVariableValueIdx = U32_MAX;
	varDecl->specifiedTypeIdx = TYPETABLEIDX_Unset;
	if (varDecl->astType) {
		varDecl->specifiedTypeIdx = TypeCheckType(context, {}, varDecl->loc, varDecl->astType);
		ASSERT(varDecl->specifiedTypeIdx != TYPETABLEIDX_Unset);
		if (varDecl->specifiedTypeIdx == TYPETABLEIDX_VOID)
			LogError(context->global, varDecl->loc, "Variable can't be of type void"_s);
		for (int varIdx = 0; varIdx < varCount; ++varIdx)
			*GetVariableTypeIdx(varDecl, varIdx) = varDecl->specifiedTypeIdx;
	}

	if (varDecl->astInitialValue) {
		TypeCheckExpression(context, varDecl->astInitialValue);

		if (IsExpressionAType(context, varDecl->astInitialValue))
			LogError(context->global, varDecl->astInitialValue->any.loc, "Initial value of variable is a "
					"type"_s);

		ASTExpression *astInitialValue = varDecl->astInitialValue;

		if (varDecl->astType) {
			ASSERT(varDecl->specifiedTypeIdx != TYPETABLEIDX_Unset);
			if (astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(context->global, astInitialValue->any.loc, "Trying to initialize a variable with "
						"a void expression"_s);

			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					varDecl->specifiedTypeIdx, astInitialValue);
			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				Print("Variable declaration type and initial type don't match\n");
				ReportTypeCheckError(context, typeCheckResult.errorCode, varDecl->loc,
						varDecl->specifiedTypeIdx, astInitialValue->typeTableIdx);
			}

			astInitialValue->typeTableIdx = typeCheckResult.rightTypeIdx;

			if (typeCheckResult.leftTypeIdx != varDecl->specifiedTypeIdx) {
				varDecl->specifiedTypeIdx = typeCheckResult.leftTypeIdx;
				astInitialValue->typeTableIdx = typeCheckResult.rightTypeIdx;
				for (int varIdx = 0; varIdx < varCount; ++varIdx)
					*GetVariableTypeIdx(varDecl, varIdx) = varDecl->specifiedTypeIdx;
			}
		}
		else if (astInitialValue->nodeType == ASTNODETYPE_PROCEDURE_CALL) {
			// Infer type: return values
			u32 procTypeIdx = astInitialValue->procedureCall.procedureTypeIdx;
			TypeInfoProcedure procTypeInfo = GetTypeInfo(context, procTypeIdx).procedureInfo;
			if (varCount < procTypeInfo.returnTypeIndices.size)
				LogError(context->global, varDecl->loc, "Not enough variables to receive all return "
						"values"_s);
			else if (varCount > procTypeInfo.returnTypeIndices.size)
				LogError(context->global, varDecl->loc, "Too many variables for procedure call return "
						"values"_s);
			for (int varIdx = 0; varIdx < varCount; ++varIdx) {
				u32 returnTypeIdx = procTypeInfo.returnTypeIndices[varIdx];
				ASSERT(returnTypeIdx != TYPETABLEIDX_VOID);
				u32 inferredTypeIdx = InferType(returnTypeIdx);
				*GetVariableTypeIdx(varDecl, varIdx) = inferredTypeIdx;
			}
		}
		else {
			// Infer type: not return values
			if (astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(context->global, astInitialValue->any.loc, "Cannot initialize a variable with a "
						"void expression"_s);

			if (astInitialValue->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				u64 givenValuesCount = astInitialValue->multipleExpressions.array.size;
				// We shouldn't have 1-value multiple expression nodes
				ASSERT(givenValuesCount > 1);
				if (varCount != givenValuesCount) {
					SourceLocation beg, end;
					GetSourceLocRangeForExpression(astInitialValue, &beg, &end);
					if (varCount == 1)
						Log2Error(context->global, varDecl->loc, end,
								"Trying to initialize a variable to multiple values"_s);
					else
						Log2Error(context->global, varDecl->loc, end,
								TPrintF("Trying to initialize %d variables to %d values",
									varCount, givenValuesCount));
				}

				for (int varIdx = 0; varIdx < varCount; ++varIdx) {
					ASTExpression *valueExp = astInitialValue->multipleExpressions.array[varIdx];
					u32 valueTypeIdx = InferType(valueExp->typeTableIdx);
					valueExp->typeTableIdx = valueTypeIdx;
					ASSERT(valueTypeIdx != TYPETABLEIDX_Unset);
					*GetVariableTypeIdx(varDecl, varIdx) = valueTypeIdx;
				}
			}
			else {
				if (varCount > 1) {
					SourceLocation beg, end;
					GetSourceLocRangeForExpression(astInitialValue, &beg, &end);
					Log2Error(context->global, varDecl->loc, end,
							"Trying to initialize multiple variables to a single value"_s);
				}
				u32 valueTypeIdx = InferType(astInitialValue->typeTableIdx);
				astInitialValue->typeTableIdx = valueTypeIdx;
				ASSERT(valueTypeIdx != TYPETABLEIDX_Unset);
				for (int varIdx = 0; varIdx < varCount; ++varIdx)
					*GetVariableTypeIdx(varDecl, varIdx) = valueTypeIdx;
			}
		}
	}

	u32 newValueFlags  = context->global->config.dontPromoteMemoryToRegisters ? VALUEFLAGS_FORCE_MEMORY : 0;
	newValueFlags     |= varDecl->isStatic   ? VALUEFLAGS_ON_STATIC_STORAGE : 0;
	newValueFlags     |= varDecl->isExternal ? VALUEFLAGS_IS_EXTERNAL       : 0;

	Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
	ArrayInit(&scopeNamesToAdd, varCount);

	for (int varIdx = 0; varIdx < varCount; ++varIdx) {
		String varName = *GetVariableName(varDecl, varIdx);
		u32 typeIdx = *GetVariableTypeIdx(varDecl, varIdx);
		u32 valueIdx;
		if (!isGlobal)
			valueIdx = TCNewValue(context, varName, typeIdx, newValueFlags);
		else
			valueIdx = NewGlobalValue(context->global, varName, typeIdx, newValueFlags);
		*GetVariableValueIdx(varDecl, varIdx) = valueIdx;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = varName;
		newScopeName.variableInfo.valueIdx = valueIdx;
		newScopeName.variableInfo.typeTableIdx = typeIdx;
		newScopeName.loc = varDecl->loc;
		*ArrayAdd(&scopeNamesToAdd) = newScopeName;
	}
	TCAddScopeNames(context, scopeNamesToAdd);

	if (varCount == 0) {
		TypeCategory typeCat = GetTypeInfo(context, varDecl->specifiedTypeIdx).typeCategory;
		if (typeCat != TYPECATEGORY_STRUCT && typeCat != TYPECATEGORY_UNION)
			LogError(context->global, varDecl->loc, "Anonymous variable has to be a struct/union!"_s);
		if (isGlobal)
			LogError(context->global, varDecl->loc, "Anonymous variable can't be static!"_s);

		varDecl->anonymousVariableValueIdx = TCNewValue(context, ""_s, varDecl->specifiedTypeIdx,
				newValueFlags);

		ASTExpression *varExp = TCNewTreeNode();
		{
			ASTExpression e = {};
			e.typeTableIdx = varDecl->specifiedTypeIdx;
			e.nodeType = ASTNODETYPE_IDENTIFIER;
			e.identifier.type = NAMETYPE_VARIABLE;
			e.identifier.valueIdx = varDecl->anonymousVariableValueIdx;
			*varExp = e;
		}
		TCAddStructMembersToScope(context, varDecl->loc, varExp);
	}
}

void TypeCheckProcedureParameter(TCContext *context, ASTProcedureParameter *astParam)
{
	if (astParam->astType) {
		astParam->typeTableIdx = TypeCheckType(context, {}, astParam->loc, astParam->astType);

		if (astParam->typeTableIdx == TYPETABLEIDX_VOID)
			LogError(context->global, astParam->loc, "Variable can't be of type void!"_s);
	}

	if (astParam->astInitialValue) {
		TypeCheckExpression(context, astParam->astInitialValue);

		if (astParam->astType) {
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					astParam->typeTableIdx, astParam->astInitialValue);
			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				Print("Variable declaration type and initial type don't match\n");
				ReportTypeCheckError(context, typeCheckResult.errorCode, astParam->loc,
						astParam->typeTableIdx, astParam->astInitialValue->typeTableIdx);
			}
			astParam->typeTableIdx = typeCheckResult.leftTypeIdx;
			astParam->astInitialValue->typeTableIdx = typeCheckResult.rightTypeIdx;
		}
		else {
			if (astParam->astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(context->global, astParam->loc, "Variable can't be of type void!"_s);

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
ReturnCheckResult TCCheckIfReturnsValue(Context *context, ASTExpression *expression)
{
	switch (expression->nodeType) {
	case ASTNODETYPE_RETURN:
	{
		return RETURNCHECKRESULT_ALWAYS;
	}
	case ASTNODETYPE_BLOCK:
	{
		ReturnCheckResult result = RETURNCHECKRESULT_NEVER;
		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			ReturnCheckResult statementResult = TCCheckIfReturnsValue(context, &expression->block.statements[i]);
			if (statementResult > result)
			{
				result = statementResult;
			}
		}
		return result;
	}
	case ASTNODETYPE_IF:
	{
		ReturnCheckResult ifStatement = TCCheckIfReturnsValue(context, expression->ifNode.body);
		ReturnCheckResult elseStatement = RETURNCHECKRESULT_NEVER;
		if (expression->ifNode.elseBody)
			elseStatement = TCCheckIfReturnsValue(context, expression->ifNode.elseBody);

		if (ifStatement == RETURNCHECKRESULT_ALWAYS && elseStatement == RETURNCHECKRESULT_ALWAYS)
			return RETURNCHECKRESULT_ALWAYS;

		if (ifStatement != RETURNCHECKRESULT_NEVER || elseStatement != RETURNCHECKRESULT_NEVER)
			return RETURNCHECKRESULT_SOMETIMES;
	}
	case ASTNODETYPE_WHILE:
	{
		return TCCheckIfReturnsValue(context, expression->whileNode.body);
	}
	case ASTNODETYPE_FOR:
	{
		return TCCheckIfReturnsValue(context, expression->forNode.body);
	}
	}
	return RETURNCHECKRESULT_NEVER;
}

bool TypeCheckProcedurePrototype(TCContext *context, ASTProcedurePrototype *astPrototype)
{
	// Parameters
	bool beginOptionalParameters = false;
	for (int i = 0; i < astPrototype->astParameters.size; ++i) {
		TypeCheckProcedureParameter(context, &astPrototype->astParameters[i]);

		ASTProcedureParameter astParameter = astPrototype->astParameters[i];
		if (!astParameter.astInitialValue) {
			if (beginOptionalParameters)
				LogError(context->global, astParameter.loc,
						"Non-optional parameter after optional parameter found!"_s);
		}
		else
			beginOptionalParameters = true;
	}

	if (astPrototype->astReturnTypes.size) {
		u32 firstTypeIdx = TypeCheckType(context, {}, astPrototype->loc, astPrototype->astReturnTypes[0]);
		if (firstTypeIdx != TYPETABLEIDX_VOID) {
			DynamicArrayInit(&astPrototype->returnTypeIndices, astPrototype->astReturnTypes.size);
			*DynamicArrayAdd(&astPrototype->returnTypeIndices) = firstTypeIdx;
			for (int i = 1; i < astPrototype->astReturnTypes.size; ++i)
				*DynamicArrayAdd(&astPrototype->returnTypeIndices) =
					TypeCheckType(context, {}, astPrototype->loc, astPrototype->astReturnTypes[i]);
		}
	}

	return true;
}

TypeInfo TypeInfoFromASTProcedurePrototype(JobContext *context, ASTProcedurePrototype *astPrototype)
{
	TypeInfo t = {};
	t.size = g_pointerSize;
	t.typeCategory = TYPECATEGORY_PROCEDURE;
	t.procedureInfo.isVarargs = astPrototype->isVarargs;
	t.procedureInfo.callingConvention = astPrototype->callingConvention;

	if (astPrototype->returnTypeIndices.size)
	{
		DynamicArrayInit(&t.procedureInfo.returnTypeIndices, astPrototype->returnTypeIndices.size);
		for (int i = 0; i < astPrototype->returnTypeIndices.size; ++i)
			*DynamicArrayAdd(&t.procedureInfo.returnTypeIndices) = astPrototype->returnTypeIndices[i];
	}

	u64 astParametersCount = astPrototype->astParameters.size;
	if (astParametersCount)
	{
		DynamicArray<ProcedureParameter, LinearAllocator> parameters;
		DynamicArrayInit(&parameters, astParametersCount);

		// Parameters
		for (int i = 0; i < astParametersCount; ++i)
		{
			ASTProcedureParameter astParameter = astPrototype->astParameters[i];

			ProcedureParameter procParam = {};
			procParam.typeTableIdx = astParameter.typeTableIdx;
			if (astParameter.astInitialValue)
			{
				procParam.defaultValue = TryEvaluateConstant(context, astParameter.astInitialValue);
				if (procParam.defaultValue.type == CONSTANTTYPE_INVALID)
					LogError(context->global, astParameter.astInitialValue->any.loc,
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

	if (astPrototype->returnTypeIndices.size)
	{
		DynamicArrayInit(&t.procedureInfo.returnTypeIndices, astPrototype->returnTypeIndices.size);
		for (int i = 0; i < astPrototype->returnTypeIndices.size; ++i)
			*DynamicArrayAdd(&t.procedureInfo.returnTypeIndices) = astPrototype->returnTypeIndices[i];
	}

	return t;
}

ASTExpression TCInlineProcedureCopyTreeBranch(TCContext *context, const ASTExpression *expression)
{
	ASTExpression result;
	result.nodeType = expression->nodeType;
	result.any.loc = expression->any.loc;
	result.typeTableIdx = expression->typeTableIdx;

	switch (expression->nodeType) {
	case ASTNODETYPE_BLOCK:
	{
		ASTBlock astBlock = {};
		DynamicArrayInit(&astBlock.statements, expression->block.statements.size);

		TCPushScope(context);

		for (int i = 0; i < expression->block.statements.size; ++i) {
			ASTExpression statement = TCInlineProcedureCopyTreeBranch(context,
					&expression->block.statements[i]);
			if (statement.nodeType != ASTNODETYPE_INVALID)
				*DynamicArrayAdd(&astBlock.statements) = statement;
		}

		TCPopScope(context);

		result.block = astBlock;
		return result;
	}
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		u32 flags = (context->global->config.dontPromoteMemoryToRegisters ? VALUEFLAGS_FORCE_MEMORY : 0) |
					(varDecl.isStatic   ? VALUEFLAGS_ON_STATIC_STORAGE : 0) |
					(varDecl.isExternal ? VALUEFLAGS_IS_EXTERNAL       : 0);

		u64 varCount = varDecl.nameCount;

		Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
		ArrayInit(&scopeNamesToAdd, varCount);

		if (varCount > 1)
			varDecl.arrayOfValueIndices = (u32 *)LinearAllocator::Alloc(sizeof(u32) * varCount, alignof(u32));
		for (int varIdx = 0; varIdx < varCount; ++varIdx) {
			u32 typeTableIdx = *GetVariableTypeIdx(&varDecl, varIdx);
			String varName = *GetVariableName(&varDecl, varIdx);
			u32 valueIdx;
			if (!varDecl.isStatic && !varDecl.isExternal)
				valueIdx = TCNewValue(context, varName, typeTableIdx, flags);
			else
				valueIdx = NewGlobalValue(context->global, varName, typeTableIdx, flags);
			*GetVariableValueIdx(&varDecl, varIdx) = valueIdx;

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = varName;
			newScopeName.variableInfo.valueIdx = valueIdx;
			newScopeName.variableInfo.typeTableIdx = typeTableIdx;
			newScopeName.loc = varDecl.loc;
			*ArrayAdd(&scopeNamesToAdd) = newScopeName;
		}
		TCAddScopeNames(context, scopeNamesToAdd);

		if (varCount == 0) {
			ASSERT(!varDecl.isStatic && !varDecl.isExternal);
			varDecl.anonymousVariableValueIdx = TCNewValue(context, ""_s, varDecl.specifiedTypeIdx, flags);

			ASTExpression *varExp = TCNewTreeNode();
			*varExp = {
				.nodeType = ASTNODETYPE_IDENTIFIER,
				.typeTableIdx = varDecl.specifiedTypeIdx,
				.identifier = {
					.type = NAMETYPE_VARIABLE,
					.valueIdx = varDecl.anonymousVariableValueIdx
				}
			};
			TCAddStructMembersToScope(context, varDecl.loc, varExp);
		}

		if (varDecl.astInitialValue) {
			ASTExpression *astInitialValue = TCNewTreeNode();
			*astInitialValue = TCInlineProcedureCopyTreeBranch(context, varDecl.astInitialValue);
			varDecl.astInitialValue = astInitialValue;
		}

		result.variableDeclaration = varDecl;
		return result;
	}
	case ASTNODETYPE_IDENTIFIER:
	{
		ASTIdentifier astIdentifier = expression->identifier;

		TCScopeName scopeName = TCFindScopeName(context, expression->any.loc, astIdentifier.string);

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

		// Add scope names
		Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
		ArrayInit(&scopeNamesToAdd, astStaticDef.nameCount);

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_STATIC_DEFINITION;
		newScopeName.staticDefinitionIdx = expression->staticDefinition.staticDefinitionIdx;
		newScopeName.loc = astStaticDef.loc;
		for (u32 i = 0; i < astStaticDef.nameCount; ++i) {
			newScopeName.name = *GetVariableName(&astStaticDef, i);
			*ArrayAdd(&scopeNamesToAdd) = newScopeName;
		}
		TCAddScopeNames(context, scopeNamesToAdd);

		return {};
	}
	case ASTNODETYPE_RETURN:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->returnNode.expression);
		result.returnNode.expression = e;
		return result;
	}
	case ASTNODETYPE_DEFER:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->deferNode.expression);
		result.deferNode.expression = e;
		return result;
	}
	case ASTNODETYPE_USING:
	{
		ASTExpression *usingExp = TCNewTreeNode();
		*usingExp = TCInlineProcedureCopyTreeBranch(context, expression->usingNode.expression);

		if (usingExp->nodeType == ASTNODETYPE_VARIABLE_DECLARATION)
		{
			ASSERT(usingExp->variableDeclaration.nameCount == 1);
			ASTExpression *varExp = TCNewTreeNode();
			*varExp = {
				.nodeType = ASTNODETYPE_IDENTIFIER,
				.typeTableIdx = usingExp->variableDeclaration.typeIdx,
				.identifier = {
					.type = NAMETYPE_VARIABLE,
					.valueIdx = usingExp->variableDeclaration.valueIdx
				},
			};
			TCAddStructMembersToScope(context, usingExp->any.loc, varExp);
		}
		else
			TCAddStructMembersToScope(context, usingExp->any.loc, usingExp);

		result.usingNode.expression = usingExp;
		return result;
	}
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall original = expression->procedureCall;

		ASTProcedureCall astProcCall = original;
		u64 argCount = original.arguments.size;
		if (argCount) {
			DynamicArrayInit(&astProcCall.arguments, argCount);
			for (int argIdx = 0; argIdx < argCount; ++argIdx) {
				ASTExpression *arg = TCNewTreeNode();
				*arg = TCInlineProcedureCopyTreeBranch(context, original.arguments[argIdx]);
				*DynamicArrayAdd(&astProcCall.arguments) = arg;
			}
		}

		ASTExpression *exp = TCNewTreeNode();
		*exp = TCInlineProcedureCopyTreeBranch(context, original.procedureExpression);

		result.procedureCall = astProcCall;
		return result;
	}
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTUnaryOperation astUnary = expression->unaryOperation;
		ASTExpression *e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->unaryOperation.expression);
		astUnary.expression = e;
		result.unaryOperation = astUnary;
		return result;
	}
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTBinaryOperation astBinary = expression->binaryOperation;
		ASTExpression *l = TCNewTreeNode();
		ASTExpression *r = TCNewTreeNode();
		*l = TCInlineProcedureCopyTreeBranch(context, expression->binaryOperation.leftHand);

		// For member access we can just copy
		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
			*r = *expression->binaryOperation.rightHand;
		else
			*r = TCInlineProcedureCopyTreeBranch(context, expression->binaryOperation.rightHand);

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
				ASTExpression *e = TCNewTreeNode();
				*e = TCInlineProcedureCopyTreeBranch(context,
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

		ASTExpression *e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->ifNode.condition);
		astIf.condition = e;

		e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->ifNode.body);
		astIf.body = e;

		if (expression->ifNode.elseBody)
		{
			e = TCNewTreeNode();
			*e = TCInlineProcedureCopyTreeBranch(context, expression->ifNode.elseBody);
			astIf.elseBody = e;
		}

		result.ifNode = astIf;
		return result;
	}
	case ASTNODETYPE_WHILE:
	{
		ASTWhile astWhile = expression->whileNode;

		ASTExpression *e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->whileNode.condition);
		astWhile.condition = e;

		e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->whileNode.body);
		astWhile.body = e;

		result.whileNode = astWhile;
		return result;
	}
	case ASTNODETYPE_FOR:
	{
		ASTFor astFor = expression->forNode;

		ASTExpression *e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, astFor.range);
		astFor.range = e;

		TCPushScope(context);

		FixedArray<TCScopeName, 2> scopeNamesToAdd = {};

		// 'i' will be of type s64 when iterating over arrays/strings.
		u32 indexTypeIdx = TYPETABLEIDX_S64;

		ASTExpression *rangeExp = astFor.range;
		bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			rangeExp->binaryOperation.op == TOKEN_OP_RANGE;
		if (isExplicitRange) {
			indexTypeIdx = rangeExp->binaryOperation.leftHand->typeTableIdx;
			ASSERT(CheckTypesMatch(context,
						rangeExp->binaryOperation.leftHand->typeTableIdx,
						rangeExp->binaryOperation.rightHand->typeTableIdx) == TYPECHECK_COOL);
		}
		else {
			u32 elementTypeTableIdx = TYPETABLEIDX_U8;
			if (rangeExp->typeTableIdx != TYPETABLEIDX_STRING_STRUCT) {
				TypeInfo rangeTypeInfo = GetTypeInfo(context, rangeExp->typeTableIdx);
				if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER)
					rangeTypeInfo = GetTypeInfo(context, rangeTypeInfo.pointerInfo.pointedTypeTableIdx);
				ASSERT(rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY);
				elementTypeTableIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			}

			u32 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementTypeTableIdx);
			u32 elementValueIdx = TCNewValue(context, astFor.itemVariableName,
					pointerToElementTypeIdx, 0);
			astFor.elementValueIdx = elementValueIdx;

			*FixedArrayAdd(&scopeNamesToAdd) = {
				.type = NAMETYPE_VARIABLE,
				.name = astFor.itemVariableName,
				.loc = astFor.loc,
				.variableInfo = { .valueIdx = elementValueIdx, .typeTableIdx = pointerToElementTypeIdx }
			};
		}

		u32 indexValueIdx = TCNewValue(context, astFor.indexVariableName, indexTypeIdx, 0);
		astFor.indexValueIdx = indexValueIdx;

		*FixedArrayAdd(&scopeNamesToAdd) = {
			.type = NAMETYPE_VARIABLE,
			.name = astFor.indexVariableName,
			.loc = astFor.loc,
			.variableInfo = { .valueIdx = indexValueIdx, .typeTableIdx = indexTypeIdx }
		};
		TCAddScopeNames(context, scopeNamesToAdd);

		e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->forNode.body);
		astFor.body = e;

		TCPopScope(context);

		result.forNode = astFor;
		return result;
	}
	case ASTNODETYPE_BREAK:
	case ASTNODETYPE_CONTINUE:
	case ASTNODETYPE_TYPE:
	case ASTNODETYPE_TYPEOF:
	case ASTNODETYPE_SIZEOF:
	case ASTNODETYPE_GARBAGE:
	{
		return *expression;
	}
	case ASTNODETYPE_CAST:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCInlineProcedureCopyTreeBranch(context, expression->castNode.expression);
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
				*DynamicArrayAdd(&astIntrinsic.arguments) = TCInlineProcedureCopyTreeBranch(context,
						&expression->intrinsic.arguments[argIdx]);
			}
		}
		result.intrinsic = astIntrinsic;
		return result;
	}
	default:
		ASSERT(false);
	}
}

void TCAddParametersToScope(TCContext *context, ArrayView<u32> parameterValues,
		const ASTProcedurePrototype *astPrototype)
{
	Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
	ArrayInit(&scopeNamesToAdd, astPrototype->astParameters.size + astPrototype->isVarargs);
	for (int i = 0; i < astPrototype->astParameters.size; ++i) {
		ASTProcedureParameter astParameter = astPrototype->astParameters[i];
		u32 paramValueIdx = parameterValues[i];

		if (astParameter.isUsing) {
			ASTExpression *varExp = TCNewTreeNode();
			{
				ASTExpression e = {};
				e.typeTableIdx = astParameter.typeTableIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.valueIdx = paramValueIdx;
				*varExp = e;
			}
			TCAddStructMembersToScope(context, astParameter.loc, varExp);
		}

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = astParameter.name;
		newScopeName.variableInfo.valueIdx = paramValueIdx;
		newScopeName.variableInfo.typeTableIdx = astParameter.typeTableIdx;
		newScopeName.loc = astParameter.loc;
		*ArrayAdd(&scopeNamesToAdd) = newScopeName;
	}

	// Varargs array
	if (astPrototype->isVarargs) {
		static u32 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

		u32 paramValueIdx = { parameterValues[astPrototype->astParameters.size] };

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = astPrototype->varargsName;
		newScopeName.variableInfo.valueIdx = paramValueIdx;
		newScopeName.variableInfo.typeTableIdx = arrayTableIdx;
		newScopeName.loc = astPrototype->varargsLoc;
		*ArrayAdd(&scopeNamesToAdd) = newScopeName;
	}
	TCAddScopeNames(context, scopeNamesToAdd);
}

u32 NewProcedure(Context *context, Procedure p, bool isExternal)
{
	u32 procedureIdx;
	if (!isExternal)
	{
		auto procedures = context->procedures.GetForWrite();
		procedureIdx = (u32)procedures->count;
		*BucketArrayAdd(&procedures) = p;

		{
			auto inlineCalls = context->tcInlineCalls.Get();
			if (inlineCalls->size <= procedureIdx)
				DynamicArrayAddMany(&inlineCalls, procedureIdx - inlineCalls->size);
			inlineCalls[procedureIdx] = {};
		}
	}
	else
	{
		auto externalProcedures = context->externalProcedures.GetForWrite();
		procedureIdx = ((u32)externalProcedures->count | PROCEDURE_EXTERNAL_BIT);
		*BucketArrayAdd(&externalProcedures) = p;
	}
	return procedureIdx;
}

bool TCPushParametersAndInlineProcedureCall(TCContext *context, ASTProcedureCall *astProcCall)
{
	if (astProcCall->callType != CALLTYPE_STATIC)
		return false;

	Procedure proc = GetProcedureRead(context->global, astProcCall->procedureIdx);

	if (astProcCall->inlineType == CALLINLINETYPE_NEVER_INLINE)
		return false;
	if (!proc.isInline && astProcCall->inlineType != CALLINLINETYPE_ALWAYS_INLINE)
		return false;

	ASSERT(GetTypeInfo(context, proc.typeTableIdx).typeCategory == TYPECATEGORY_PROCEDURE);
	TypeInfoProcedure procTypeInfo = GetTypeInfo(context, proc.typeTableIdx).procedureInfo;

	TCPushScope(context);

	s64 totalArguments = procTypeInfo.parameters.size;
	ArrayInit(&astProcCall->inlineParameterValues, totalArguments + 1);

	for (int argIdx = 0; argIdx < totalArguments; ++argIdx) {
		u32 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
		u32 newValueIdx = TCNewValue(context, SNPrintF(12, "_inlinearg%d", argIdx),
				paramTypeIdx, 0);
		*ArrayAdd(&astProcCall->inlineParameterValues) = newValueIdx;
	}
	if (procTypeInfo.isVarargs) {
		static u32 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);
		u32 newValueIdx = TCNewValue(context, "_inlinevarargs"_s, arrayTableIdx, 0);
		*ArrayAdd(&astProcCall->inlineParameterValues) = newValueIdx;
	}

	TCAddParametersToScope(context, astProcCall->inlineParameterValues, &proc.astPrototype);

	ASTExpression *e = TCNewTreeNode();
	*e = TCInlineProcedureCopyTreeBranch(context, proc.astBody);
	astProcCall->astBodyInlineCopy = e;

	TCPopScope(context);

	return true;
}

bool TCIsPrimitiveOperation(JobContext *context, enum TokenType op, u32 leftTypeIdx, u32 rightTypeIdx)
{
	leftTypeIdx  = StripImplicitlyCastAliases(context->global, leftTypeIdx);
	rightTypeIdx = StripImplicitlyCastAliases(context->global, rightTypeIdx);
	switch (op) {
	case TOKEN_OP_ASSIGNMENT:
		return CheckTypesMatch(context, leftTypeIdx, rightTypeIdx) == TYPECHECK_COOL;
	case TOKEN_OP_MEMBER_ACCESS:
	{
		TypeCategory leftCat  = GetTypeInfo(context, leftTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_STRUCT || leftCat == TYPECATEGORY_UNION;
	} break;
	case TOKEN_OP_ARRAY_ACCESS:
	{
		TypeCategory leftCat  = GetTypeInfo(context, leftTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_ARRAY;
	} break;
	case TOKEN_OP_PLUS:
	case TOKEN_OP_ASSIGNMENT_PLUS:
	case TOKEN_OP_MINUS:
	case TOKEN_OP_ASSIGNMENT_MINUS:
	{
		TypeCategory leftCat  = GetTypeInfo(context, leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_FLOATING &&
			leftCat  != TYPECATEGORY_POINTER)
			return false;

		TypeCategory rightCat = GetTypeInfo(context, rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_FLOATING &&
			rightCat != TYPECATEGORY_POINTER)
			return false;

		if (CheckTypesMatch(context, leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
			return false;

		return true;
	} break;
	case TOKEN_OP_SHIFT_LEFT:
	case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
	case TOKEN_OP_SHIFT_RIGHT:
	case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
	{
		TypeCategory leftCat  = GetTypeInfo(context, leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_ENUM &&
			leftCat  != TYPECATEGORY_POINTER)
			return false;

		TypeCategory rightCat = GetTypeInfo(context, rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_ENUM &&
			rightCat != TYPECATEGORY_POINTER)
			return false;

		// Don't care about sign/size for shifts
		TypeCheckErrorCode typeCheckError = CheckTypesMatch(context, leftTypeIdx, rightTypeIdx);
		if (typeCheckError != TYPECHECK_COOL && typeCheckError != TYPECHECK_SIZE_MISMATCH &&
				typeCheckError != TYPECHECK_SIGN_MISMATCH)
			return false;

		return true;
	} break;
	case TOKEN_OP_BITWISE_AND:
	case TOKEN_OP_BITWISE_OR:
	case TOKEN_OP_BITWISE_XOR:
	{
		TypeCategory leftCat  = GetTypeInfo(context, leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_POINTER &&
			leftCat  != TYPECATEGORY_ENUM)
			return false;

		TypeCategory rightCat = GetTypeInfo(context, rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_POINTER &&
			rightCat != TYPECATEGORY_ENUM)
			return false;

		if (CheckTypesMatch(context, leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
			return false;

		return true;
	} break;
	case TOKEN_OP_EQUALS:
	case TOKEN_OP_NOT_EQUALS:
	case TOKEN_OP_LESS_THAN:
	case TOKEN_OP_LESS_THAN_OR_EQUAL:
	case TOKEN_OP_GREATER_THAN:
	case TOKEN_OP_GREATER_THAN_OR_EQUAL:
	{
		TypeCategory leftCat  = GetTypeInfo(context, leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_POINTER &&
			leftCat  != TYPECATEGORY_ENUM    && leftCat  != TYPECATEGORY_FLOATING)
			return false;

		TypeCategory rightCat = GetTypeInfo(context, rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_POINTER &&
			rightCat != TYPECATEGORY_ENUM    && rightCat != TYPECATEGORY_FLOATING)
			return false;

		if (CheckTypesMatch(context, leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
			return false;

		return true;
	} break;
	case TOKEN_OP_AND:
	case TOKEN_OP_OR:
		return CheckTypesMatch(context, TYPETABLEIDX_BOOL, leftTypeIdx)  == TYPECHECK_COOL &&
			   CheckTypesMatch(context, TYPETABLEIDX_BOOL, rightTypeIdx) == TYPECHECK_COOL;
	default:
	{
		TypeCategory leftCat  = GetTypeInfo(context, leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_FLOATING)
			return false;

		TypeCategory rightCat = GetTypeInfo(context, rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_FLOATING)
			return false;

		if (CheckTypesMatch(context, leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
			return false;

		return true;
	} break;
	}
}

bool TCIsPrimitiveOperation(JobContext *context, enum TokenType op, u32 inputTypeIdx)
{
	switch (op) {
	case TOKEN_OP_MINUS:
	{
		TypeCategory leftCat  = GetTypeInfo(context, inputTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_INTEGER || leftCat == TYPECATEGORY_FLOATING;
	} break;
	case TOKEN_OP_BITWISE_NOT:
	{
		TypeCategory leftCat  = GetTypeInfo(context, inputTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_INTEGER || leftCat == TYPECATEGORY_POINTER;
	} break;
	case TOKEN_OP_NOT:
		return CheckTypesMatch(context, TYPETABLEIDX_BOOL, inputTypeIdx) == TYPECHECK_COOL;
	case TOKEN_OP_DEREFERENCE:
	{
		TypeCategory leftCat  = GetTypeInfo(context, inputTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_POINTER;
	} break;
	case TOKEN_OP_POINTER_TO:
	{
		return true;
	} break;
	default:
	{
		TypeCategory inputCat = GetTypeInfo(context, inputTypeIdx).typeCategory;
		return inputCat == TYPECATEGORY_INTEGER || inputCat == TYPECATEGORY_FLOATING;
	} break;
	}
}

void TCCheckForCyclicInlineCalls(Context *context, SourceLocation loc, u32 callerProcIdx,
		u32 calleeProcIdx)
{
	auto inlineCalls = context->tcInlineCalls.Get();
	// Register inline call
	if (!inlineCalls[callerProcIdx].capacity)
		DynamicArrayInit(&inlineCalls[callerProcIdx], 4);
	*DynamicArrayAdd(&inlineCalls[callerProcIdx]) = { calleeProcIdx, loc };

	// Check for cyclic dependencies
	ArrayView<const InlineCall> inlinedCalls = inlineCalls[calleeProcIdx];
	for (int i = 0; i < inlinedCalls.size; ++i) {
		if (inlinedCalls[i].procedureIdx == callerProcIdx) {
			// @Incomplete: improve error message
			LogErrorNoCrash(context, loc, TPrintF("Procedures "
					"\"%S\" and \"%S\" are trying to inline each other.",
					GetProcedureRead(context, callerProcIdx).name,
					GetProcedureRead(context, calleeProcIdx).name));
			LogNote(context, inlinedCalls[i].loc, "See other call"_s);
			PANIC;
		}
	}
}

String OperatorToString(s32 op);
bool TCLookForOperatorOverload(TCContext *context, ASTExpression *expression)
{
	OperatorOverload overload = {};
	bool foundOverload = false;

	enum TokenType op;
	int paramCount;
	ASTExpression *leftHand = nullptr;
	ASTExpression *rightHand = nullptr;
	if (expression->nodeType == ASTNODETYPE_UNARY_OPERATION) {
		op = expression->unaryOperation.op;
		paramCount = 1;
		leftHand = expression->unaryOperation.expression;
		// These can't have overloads!
		if (TCIsPrimitiveOperation(context, op, leftHand->typeTableIdx))
			return false;
	}
	else {
		op = expression->binaryOperation.op;
		paramCount = 2;
		leftHand = expression->binaryOperation.leftHand;
		rightHand = expression->binaryOperation.rightHand;
		// These can't have overloads!
		if (TCIsPrimitiveOperation(context, op, leftHand->typeTableIdx, rightHand->typeTableIdx))
			return false;
	}

	bool triedOnce = false;
tryAgain:
	{
		auto operatorOverloads = context->global->operatorOverloads.GetForRead();
		for (int overloadIdx = 0; overloadIdx < operatorOverloads->size; ++overloadIdx) {
			OperatorOverload currentOverload = operatorOverloads[overloadIdx];

			if (op != currentOverload.op)
				continue;

			Procedure procedure = GetProcedureRead(context->global, currentOverload.procedureIdx);
			TypeInfo procType = GetTypeInfo(context, procedure.typeTableIdx);
			ASSERT(procType.typeCategory == TYPECATEGORY_PROCEDURE);

			if (paramCount == 1) {
				if (procType.procedureInfo.parameters.size != 1)
					continue;

				u32 leftHandTypeIdx  = procType.procedureInfo.parameters[0].typeTableIdx;

				if (CheckTypesMatch(context, leftHand->typeTableIdx, leftHandTypeIdx) != TYPECHECK_COOL)
					continue;

				if (foundOverload)
					LogError(context->global, expression->any.loc,
							TPrintF("Multiple overloads found for operator %S with operand of "
								"type %S",
								OperatorToString(op),
								TypeInfoToString(context, leftHand->typeTableIdx)));

				overload = currentOverload;
				foundOverload = true;
			}
			else {
				if (procType.procedureInfo.parameters.size != 2)
					continue;

				u32 leftHandTypeIdx  = procType.procedureInfo.parameters[0].typeTableIdx;
				u32 rightHandTypeIdx = procType.procedureInfo.parameters[1].typeTableIdx;

				if (CheckTypesMatch(context, leftHand->typeTableIdx, leftHandTypeIdx) !=
						TYPECHECK_COOL ||
					CheckTypesMatch(context, rightHand->typeTableIdx, rightHandTypeIdx) !=
						TYPECHECK_COOL)
					continue;

				if (foundOverload)
					LogError(context->global, expression->any.loc,
							TPrintF("Multiple overloads found for operator %S with left hand "
								"of type %S and right hand of type %S",
								OperatorToString(op),
								TypeInfoToString(context, leftHand->typeTableIdx),
								TypeInfoToString(context, rightHand->typeTableIdx)));

				overload = currentOverload;
				foundOverload = true;
			}
		}
		if (!foundOverload) {
			// Shouldn't have resumed this job if the overload still doesn't exist.
			if (triedOnce)
				LogCompilerError(context->global, expression->any.loc, "Bad job resume"_s);
			triedOnce = true;

			SwitchJob(context, YIELDREASON_UNKNOWN_OVERLOAD, {
					.loc = expression->any.loc,
					.overload = {
						.op = (u32)op,
						.leftTypeIdx  = leftHand->typeTableIdx,
						.rightTypeIdx = rightHand ? rightHand->typeTableIdx : U32_MAX
					}});
			// Lock again!
			SYSLockForRead(&context->global->operatorOverloads.rwLock);
			goto tryAgain;
		}
	}

	ASSERT(foundOverload);
	Procedure proc = GetProcedureRead(context->global, overload.procedureIdx);

	TypeInfo procTypeInfo = GetTypeInfo(context, proc.typeTableIdx);
	ASSERT(procTypeInfo.typeCategory == TYPECATEGORY_PROCEDURE);

	if (proc.isInline && !proc.isBodyTypeChecked) {
		TCCheckForCyclicInlineCalls(context->global, expression->any.loc, context->currentProcedureIdx,
				overload.procedureIdx);

		auto procedures = context->global->procedures.GetForRead();
		proc = procedures[overload.procedureIdx];
		if (!proc.isBodyTypeChecked) {
			SwitchJob(context, YIELDREASON_PROC_BODY_NOT_READY, { .index = overload.procedureIdx });
			// Lock again!
			SYSLockForRead(&context->global->procedures.rwLock);
			proc = procedures[overload.procedureIdx];
			if (!proc.isBodyTypeChecked)
				LogCompilerError(context->global, expression->any.loc, "Bad job resume"_s);
		}
	}

	ASTProcedureCall astProcCall = {};
	astProcCall.callType = CALLTYPE_STATIC;
	astProcCall.procedureIdx = overload.procedureIdx;
	astProcCall.procedureTypeIdx = proc.typeTableIdx;
	DynamicArrayInit(&astProcCall.arguments, paramCount);
	*DynamicArrayAdd(&astProcCall.arguments) = leftHand;
	if (paramCount > 1)
		*DynamicArrayAdd(&astProcCall.arguments) = rightHand;

	expression->nodeType = ASTNODETYPE_PROCEDURE_CALL;
	expression->procedureCall = astProcCall;

	if (procTypeInfo.procedureInfo.returnTypeIndices.size == 1)
		expression->typeTableIdx = procTypeInfo.procedureInfo.returnTypeIndices[0];

	TCPushParametersAndInlineProcedureCall(context, &expression->procedureCall);
	return true;
}

void GenerateTypeCheckJobs(Context *context, ASTExpression *expression);
void TypeCheckExpression(TCContext *context, ASTExpression *expression)
{
	ASSERT(expression->typeTableIdx == TYPETABLEIDX_Unset);

	switch (expression->nodeType) {
	case ASTNODETYPE_GARBAGE:
	{
		expression->typeTableIdx = TYPETABLEIDX_Anything;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		ASTBlock *astBlock = &expression->block;
		TCPushScope(context);

		for (int i = 0; i < astBlock->statements.size; ++i)
			TypeCheckExpression(context, &astBlock->statements[i]);

		TCPopScope(context);
	} break;
	case ASTNODETYPE_MULTIPLE_EXPRESSIONS:
	{
		ASTMultipleExpressions *astME = &expression->multipleExpressions;
		for (int i = 0; i < astME->array.size; ++i)
			TypeCheckExpression(context, astME->array[i]);

	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration *varDecl = &expression->variableDeclaration;
		TypeCheckVariableDeclaration(context, varDecl);

		if (context->onStaticContext) {
			IRJobArgs *args = ALLOC(LinearAllocator, IRJobArgs);
			*args = {
				.context = context->global,
				.procedureIdx = 0,
				.localValues = {},
				.expression = expression
			};
			RequestNewJob(context->global, JOBTYPE_CODEGEN, IRJobExpression, (void *)args);
		}
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition *astStaticDef = &expression->staticDefinition;

		switch (astStaticDef->expression->nodeType) {
		case ASTNODETYPE_PROCEDURE_DECLARATION:
		{
			TCContext procedureContext = *context;
			BucketArrayInit(&procedureContext.localValues);
			*BucketArrayAdd(&procedureContext.localValues) = {}; // No value number 0?
			// Don't allocate a new array if old is empty.
			if (context->scopeStack.size)
				DynamicArrayInit(&procedureContext.scopeStack, 8);

			procedureContext.onStaticContext = false;

			ASTProcedureDeclaration *astProcDecl = &astStaticDef->expression->procedureDeclaration;
			ASTProcedurePrototype *astPrototype = &astProcDecl->prototype;

			TypeCheckProcedurePrototype(&procedureContext, astPrototype);
			TypeInfo t = TypeInfoFromASTProcedurePrototype(context, astPrototype);

			u32 typeTableIdx = FindOrAddTypeTableIdx(context, t);
			astStaticDef->expression->typeTableIdx = typeTableIdx;

			Procedure procedure = {};
			procedure.typeTableIdx = TYPETABLEIDX_Unset;
			procedure.name = astProcDecl->name;
			procedure.typeTableIdx = typeTableIdx;
			procedure.astBody = astProcDecl->astBody;
			procedure.isInline = astProcDecl->isInline;
			procedure.isExported = astProcDecl->isExported;
			procedure.astPrototype = astProcDecl->prototype;
			DynamicArrayInit(&procedure.parameterValues, 4);
			u32 procedureIdx = NewProcedure(context->global, procedure, astProcDecl->isExternal);
			astProcDecl->procedureIdx = procedureIdx;

			StaticDefinition newStaticDef = {};
			newStaticDef.typeTableIdx = TYPETABLEIDX_Unset;
			newStaticDef.name = *GetVariableName(astStaticDef, 0);
			newStaticDef.definitionType = STATICDEFINITIONTYPE_PROCEDURE;
			newStaticDef.procedureIdx = procedureIdx;
			newStaticDef.typeTableIdx = typeTableIdx;

			u32 newStaticDefIdx = NewStaticDefinition(context->global, &newStaticDef);
			astStaticDef->staticDefinitionIdx = newStaticDefIdx;

			// Add scope name
			{
				Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
				ArrayInit(&scopeNamesToAdd, astStaticDef->nameCount);

				TCScopeName newScopeName;
				newScopeName.type = NAMETYPE_STATIC_DEFINITION;
				newScopeName.staticDefinitionIdx = newStaticDefIdx;
				newScopeName.loc = astStaticDef->loc;
				for (u32 i = 0; i < astStaticDef->nameCount; ++i) {
					newScopeName.name = *GetVariableName(astStaticDef, i);
					*ArrayAdd(&scopeNamesToAdd) = newScopeName;
				}
				TCAddScopeNames(&procedureContext, scopeNamesToAdd);
			}

			TCPushScope(&procedureContext);

			if (procedure.astBody) {
				// Parameters
				ArrayView<ASTProcedureParameter> astParameters = astPrototype->astParameters;
				for (int i = 0; i < astParameters.size; ++i) {
					const ASTProcedureParameter *astParameter = &astParameters[i];
					u32 paramValueIdx = TCNewValue(&procedureContext, astParameter->name, astParameter->typeTableIdx, 0);
					*DynamicArrayAdd(&procedure.parameterValues) = paramValueIdx;
				}
				// Varargs array
				if (astPrototype->isVarargs) {
					static u32 arrayTableIdx = GetTypeInfoArrayOf(context, TYPETABLEIDX_ANY_STRUCT, 0);

					u32 valueIdx = TCNewValue(&procedureContext, astPrototype->varargsName, arrayTableIdx, 0);
					*DynamicArrayAdd(&procedure.parameterValues) = valueIdx;
				}

				TCAddParametersToScope(&procedureContext, procedure.parameterValues, &astProcDecl->prototype);

				if (astPrototype->returnTypeIndices.size) {
					ArrayInit(&procedure.returnValueIndices, astPrototype->returnTypeIndices.size);
					for (int i = 0; i < astPrototype->returnTypeIndices.size; ++i)
						*ArrayAdd(&procedure.returnValueIndices) = TCNewValue(&procedureContext, "_returnValue"_s,
							astPrototype->returnTypeIndices[i], 0);
				}

				procedureContext.currentProcedureIdx = procedureIdx;
				procedureContext.currentReturnTypes = t.procedureInfo.returnTypeIndices;

				TypeCheckExpression(&procedureContext, procedure.astBody);
			}
			procedure.isBodyTypeChecked = true;

			UpdateProcedure(context->global, procedureIdx, &procedure);

			// Wake up any jobs that were waiting for this procedure body
			WakeUpAllByIndex(context->global, YIELDREASON_PROC_BODY_NOT_READY, procedureIdx);

			expression->typeTableIdx = procedure.typeTableIdx;
			TCPopScope(&procedureContext);

			if (procedure.astBody)
			{
				// Check all paths return
				if (t.procedureInfo.returnTypeIndices.size)
				{
					ReturnCheckResult result = TCCheckIfReturnsValue(context->global, procedure.astBody);
					if (result == RETURNCHECKRESULT_SOMETIMES)
						LogError(context->global, expression->any.loc, "Procedure doesn't always return a value"_s);
					else if (result == RETURNCHECKRESULT_NEVER)
						LogError(context->global, expression->any.loc, "Procedure has to return a value"_s);
				}

				// Code gen!
				{
					IRJobArgs *args = ALLOC(LinearAllocator, IRJobArgs);
					*args = {
						.context = context->global,
						.procedureIdx = procedureIdx,
						.localValues = procedureContext.localValues,
						.expression = nullptr
					};
					RequestNewJob(context->global, JOBTYPE_CODEGEN, IRJobProcedure, (void *)args);
				}
			}
		} break;
		case ASTNODETYPE_TYPE:
		case ASTNODETYPE_ALIAS:
		{
			StaticDefinition newStaticDef = {};
			newStaticDef.typeTableIdx = TYPETABLEIDX_Unset;
			newStaticDef.name = *GetVariableName(astStaticDef, 0);
			newStaticDef.definitionType = STATICDEFINITIONTYPE_TYPE;

			u32 newStaticDefIdx = NewStaticDefinition(context->global, &newStaticDef);
			astStaticDef->staticDefinitionIdx = newStaticDefIdx;

			// Add scope name
			{
				Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
				ArrayInit(&scopeNamesToAdd, astStaticDef->nameCount);

				TCScopeName newScopeName;
				newScopeName.type = NAMETYPE_STATIC_DEFINITION;
				newScopeName.staticDefinitionIdx = newStaticDefIdx;
				newScopeName.loc = astStaticDef->loc;
				for (u32 i = 0; i < astStaticDef->nameCount; ++i) {
					newScopeName.name = *GetVariableName(astStaticDef, i);
					*ArrayAdd(&scopeNamesToAdd) = newScopeName;
				}
				TCAddScopeNames(context, scopeNamesToAdd);
			}

			u32 result = TypeCheckType(context, *GetVariableName(astStaticDef, 0),
					expression->any.loc, &astStaticDef->expression->astType);
			astStaticDef->expression->typeTableIdx = result;

			u32 newTypeIdx;
			if (astStaticDef->expression->astType.nodeType == ASTTYPENODETYPE_STRUCT_DECLARATION ||
				astStaticDef->expression->astType.nodeType == ASTTYPENODETYPE_UNION_DECLARATION ||
				astStaticDef->expression->astType.nodeType == ASTTYPENODETYPE_ENUM_DECLARATION)
				newTypeIdx = result;
			else {
				TypeInfo t = {
					.typeCategory = TYPECATEGORY_ALIAS,
					.size = GetTypeInfo(context, result).size,
					.aliasInfo = {
						.name = *GetVariableName(astStaticDef, 0),
						.aliasedTypeIdx = result,
						.doesImplicitlyCast = astStaticDef->expression->nodeType == ASTNODETYPE_ALIAS
					}
				};
				newTypeIdx = FindOrAddTypeTableIdx(context, t);
			}

			newStaticDef.typeTableIdx = newTypeIdx;
			expression->typeTableIdx = newTypeIdx;
			UpdateStaticDefinition(context->global, newStaticDefIdx, &newStaticDef);
		} break;
		default:
		{
			TypeCheckExpression(context, astStaticDef->expression);

			StaticDefinition newStaticDef;

			if (astStaticDef->expression->nodeType == ASTNODETYPE_IDENTIFIER &&
				astStaticDef->expression->identifier.type == NAMETYPE_STATIC_DEFINITION)
			{
				ASSERT(astStaticDef->expression->typeTableIdx != TYPETABLEIDX_Unset);
				u32 identifierStaticDefIdx = astStaticDef->expression->identifier.staticDefinitionIdx;
				newStaticDef = GetStaticDefinition(context, identifierStaticDefIdx, true);
				newStaticDef.name = *GetVariableName(astStaticDef, 0);
				newStaticDef.typeTableIdx = astStaticDef->expression->typeTableIdx;
				expression->typeTableIdx = astStaticDef->expression->typeTableIdx;
			}
			else
			{
				Constant constant = TryEvaluateConstant(context, astStaticDef->expression);

				if (constant.type == CONSTANTTYPE_INVALID)
					LogError(context->global, astStaticDef->expression->any.loc,
							"Failed to evaluate constant"_s);

				newStaticDef = {};
				newStaticDef.typeTableIdx = TYPETABLEIDX_Unset;
				newStaticDef.name = *GetVariableName(astStaticDef, 0);
				newStaticDef.definitionType = STATICDEFINITIONTYPE_CONSTANT;
				newStaticDef.constant = constant;

				u32 constantTypeIdx = astStaticDef->expression->typeTableIdx;
				ASSERT(constantTypeIdx != TYPETABLEIDX_Unset);
				expression->typeTableIdx = constantTypeIdx;
				newStaticDef.typeTableIdx = constantTypeIdx;

			}

			u32 newStaticDefIdx = NewStaticDefinition(context->global, &newStaticDef);
			astStaticDef->staticDefinitionIdx = newStaticDefIdx;

			// Add scope names
			{
				Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
				ArrayInit(&scopeNamesToAdd, astStaticDef->nameCount);

				TCScopeName newScopeName;
				newScopeName.type = NAMETYPE_STATIC_DEFINITION;
				newScopeName.staticDefinitionIdx = newStaticDefIdx;
				newScopeName.loc = astStaticDef->loc;
				for (u32 i = 0; i < astStaticDef->nameCount; ++i) {
					newScopeName.name = *GetVariableName(astStaticDef, i);
					*ArrayAdd(&scopeNamesToAdd) = newScopeName;
				}
				TCAddScopeNames(context, scopeNamesToAdd);
			}
		} break;
		}
	} break;
	case ASTNODETYPE_RETURN:
	{
		ASTExpression **providedReturnValues = nullptr;
		u64 providedReturnValuesCount = 0;
		u64 requiredReturnValuesCount = context->currentReturnTypes.size;

		ASTExpression *returnExp = expression->returnNode.expression;
		if (returnExp != nullptr) {
			TypeCheckExpression(context, returnExp);

			if (returnExp->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				providedReturnValuesCount = returnExp->multipleExpressions.array.size;
				providedReturnValues = returnExp->multipleExpressions.array.data;
			}
			else {
				providedReturnValuesCount = 1;
				providedReturnValues = &returnExp;
			}
		}

		if (providedReturnValuesCount != requiredReturnValuesCount)
			LogError(context->global, returnExp->any.loc, TPrintF("Returning wrong amount of "
					"values: %d required but %d were provided", requiredReturnValuesCount,
					providedReturnValuesCount));

		for (int i = 0; i < providedReturnValuesCount; ++i) {
			ASTExpression *currentExp = providedReturnValues[i];
			if (IsExpressionAType(context, currentExp))
				LogError(context->global, currentExp->any.loc, "Trying to return a type"_s);

			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					context->currentReturnTypes[i], currentExp);
			currentExp->typeTableIdx = typeCheckResult.rightTypeIdx;
			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				ReportTypeCheckError(context, typeCheckResult.errorCode, currentExp->any.loc,
						typeCheckResult.rightTypeIdx, context->currentReturnTypes[i]);
			}
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		TypeCheckExpression(context, expression->deferNode.expression);
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		String string = expression->identifier.string;

		TCScopeName scopeName = TCFindScopeName(context, expression->any.loc, string);
		ASSERT(scopeName.type != NAMETYPE_INVALID);

		expression->identifier.type = scopeName.type;
		switch (scopeName.type)
		{
		case NAMETYPE_VARIABLE:
		{
			expression->identifier.valueIdx = scopeName.variableInfo.valueIdx;

			u32 variableTypeIdx = scopeName.variableInfo.typeTableIdx;
			if (variableTypeIdx == TYPETABLEIDX_Unset)
				LogError(context->global, expression->any.loc, TPrintF("COMPILER ERROR! Variable "
								"\"%S\" not type checked", string));

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
			StaticDefinition staticDefinition = GetStaticDefinition(context,
					scopeName.staticDefinitionIdx, true);
			expression->typeTableIdx = staticDefinition.typeTableIdx;
		} break;
		case NAMETYPE_PRIMITIVE:
		{
			expression->typeTableIdx = scopeName.primitiveTypeTableIdx;
		} break;
		default:
			ASSERT(false);
		}
	} break;
	case ASTNODETYPE_USING:
	{
		ASTExpression *usingExp = expression->usingNode.expression;
		TypeCheckExpression(context, usingExp);

		if (usingExp->nodeType == ASTNODETYPE_VARIABLE_DECLARATION) {
			ASTExpression *varExp = TCNewTreeNode();
			ASSERT(usingExp->variableDeclaration.nameCount == 1);
			{
				ASTExpression e = {};
				e.typeTableIdx = usingExp->variableDeclaration.typeIdx;
				e.nodeType = ASTNODETYPE_IDENTIFIER;
				e.identifier.type = NAMETYPE_VARIABLE;
				e.identifier.valueIdx = usingExp->variableDeclaration.valueIdx;
				*varExp = e;
			}
			TCAddStructMembersToScope(context, usingExp->any.loc, varExp);
		}
		else
			TCAddStructMembersToScope(context, usingExp->any.loc, usingExp);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall *astProcCall = &expression->procedureCall;

		ProcedureCallType callType = CALLTYPE_ASTEXPRESSION;
		u32 procedureIdx = U32_MAX;

		ASTExpression *astProcExp = astProcCall->procedureExpression;
		TypeCheckExpression(context, astProcExp);
		u32 procedureTypeIdx = astProcExp->typeTableIdx;

		if (astProcExp->nodeType == ASTNODETYPE_IDENTIFIER) {
			String procName = astProcExp->identifier.string;
			TCScopeName scopeName = TCFindScopeName(context, astProcCall->loc, procName);
			if (scopeName.type == NAMETYPE_STATIC_DEFINITION) {
				callType = CALLTYPE_STATIC;
				StaticDefinition staticDefinition = GetStaticDefinition(context,
						scopeName.staticDefinitionIdx, false);

				if (staticDefinition.definitionType != STATICDEFINITIONTYPE_PROCEDURE)
					LogError(context->global, expression->any.loc, "Calling a non-procedure"_s);

				procedureIdx = staticDefinition.procedureIdx;
				Procedure proc = GetProcedureRead(context->global, procedureIdx);
				if ((proc.isInline || astProcCall->inlineType == CALLINLINETYPE_ALWAYS_INLINE) &&
						astProcCall->inlineType != CALLINLINETYPE_NEVER_INLINE) {

					TCCheckForCyclicInlineCalls(context->global, astProcExp->any.loc,
							context->currentProcedureIdx, procedureIdx);

					// We need the whole body type checked
					if (!proc.isBodyTypeChecked) {
						auto procedures = context->global->procedures.GetForRead();
						proc = procedures[procedureIdx];
						if (!proc.isBodyTypeChecked) {
							SwitchJob(context, YIELDREASON_PROC_BODY_NOT_READY, { .index = procedureIdx });
							// Lock again!
							SYSLockForRead(&context->global->procedures.rwLock);
							proc = procedures[procedureIdx];
							if (!proc.isBodyTypeChecked)
								LogCompilerError(context->global, expression->any.loc, "Bad job resume"_s);
						}
					}
				}
				procedureTypeIdx = proc.typeTableIdx;
			}
		}

		if (procedureTypeIdx == TYPETABLEIDX_Unset)
			LogCompilerError(context->global, astProcExp->any.loc, TPrintF("Procedure \"%S\" not type "
						"checked", GetProcedureRead(context->global, procedureIdx).name));
		ASSERT(GetTypeInfo(context, procedureTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);

		s64 givenArguments = astProcCall->arguments.size;
		for (int argIdx = 0; argIdx < givenArguments; ++argIdx) {
			ASTExpression *arg = astProcCall->arguments[argIdx];
			TypeCheckExpression(context, arg);
		}

		astProcCall->callType = callType;
		astProcCall->procedureIdx = procedureIdx;
		astProcCall->procedureTypeIdx = procedureTypeIdx;

		ASSERT(GetTypeInfo(context, procedureTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = GetTypeInfo(context, procedureTypeIdx).procedureInfo;

		if (procTypeInfo.returnTypeIndices.size == 0)
			expression->typeTableIdx = TYPETABLEIDX_VOID;
		else if (procTypeInfo.returnTypeIndices.size == 1)
			expression->typeTableIdx = procTypeInfo.returnTypeIndices[0];

		// Type check arguments
		s64 requiredArguments = 0;
		for (int i = 0; i < procTypeInfo.parameters.size; ++i) {
			if (procTypeInfo.parameters[i].defaultValue.type == CONSTANTTYPE_INVALID)
				++requiredArguments;
		}

		String errorProcedureName = {};
		if (astProcExp->nodeType == ASTNODETYPE_IDENTIFIER)
			errorProcedureName = TPrintF(" \"%S\"", astProcExp->identifier.string);

		s64 totalArguments = procTypeInfo.parameters.size;
		if (procTypeInfo.isVarargs) {
			if (requiredArguments > givenArguments)
				LogError(context->global, astProcExp->any.loc,
						TPrintF("Procedure%S needs at least %d arguments but only %d were given",
							errorProcedureName, requiredArguments, givenArguments));
		}
		else {
			if (requiredArguments > givenArguments)
				LogError(context->global, astProcExp->any.loc,
						TPrintF("Procedure%S needs at least %d arguments but only %d were given",
						errorProcedureName, requiredArguments, givenArguments));

			if (givenArguments > totalArguments)
				LogError(context->global, astProcExp->any.loc,
						TPrintF("Procedure%S needs %d arguments but %d were given",
						errorProcedureName, totalArguments, givenArguments));
		}

		s64 argsToCheck = Min(givenArguments, totalArguments);
		for (int argIdx = 0; argIdx < argsToCheck; ++argIdx) {
			ASTExpression *arg = astProcCall->arguments[argIdx];
			u32 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					paramTypeIdx, arg);
			arg->typeTableIdx = typeCheckResult.rightTypeIdx;

			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				String paramStr = TypeInfoToString(context, paramTypeIdx);
				String givenStr = TypeInfoToString(context, arg->typeTableIdx);
				LogError(context->global, arg->any.loc, TPrintF("When calling procedure%S: type of "
							"parameter #%d didn't match (parameter is %S but %S was given)",
							errorProcedureName, argIdx, paramStr, givenStr));
			}
		}

		// Infer the rest (collapse _NUMBER and _FLOATING)
		for (s64 argIdx = argsToCheck; argIdx < givenArguments; ++argIdx) {
			ASTExpression *arg = astProcCall->arguments[argIdx];
			u32 inferred = InferType(arg->typeTableIdx);
			if (inferred != arg->typeTableIdx)
				InferTypesInExpression(context, arg, inferred);
		}

		TCPushParametersAndInlineProcedureCall(context, astProcCall);
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTExpression *input = expression->unaryOperation.expression;
		TypeCheckExpression(context, input);

		if (IsExpressionAType(context, input))
			LogError(context->global, input->any.loc, "Input of unary operator is a type"_s);

		if (TCIsPrimitiveOperation(context, expression->unaryOperation.op, input->typeTableIdx)) {
			u32 expressionType = input->typeTableIdx;
			switch (expression->unaryOperation.op)
			{
			case TOKEN_OP_NOT:
			{
				TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
						expressionType);
				if (typeCheckResult != TYPECHECK_COOL)
					LogError(context->global, expression->any.loc, "Expression can't be cast to boolean"_s);
				expression->typeTableIdx = TYPETABLEIDX_BOOL;
			} break;
			case TOKEN_OP_POINTER_TO:
			{
				// Forbid pointer to temporal values
				if (IsTemporalValue(input))
					LogError(context->global, expression->any.loc, "Trying to get pointer to temporal value"_s);

				ASTExpression *e = input;
				switch (e->nodeType) {
				case ASTNODETYPE_IDENTIFIER:
				{
					if (e->identifier.type == NAMETYPE_VARIABLE)
						TCSetValueFlags(context, e->identifier.valueIdx, VALUEFLAGS_FORCE_MEMORY);
				} break;
				}

				expression->typeTableIdx = GetTypeInfoPointerOf(context, expressionType);
			} break;
			case TOKEN_OP_DEREFERENCE:
			{
				TypeInfo expressionTypeInfo = GetTypeInfo(context, expressionType);
				if (expressionTypeInfo.typeCategory != TYPECATEGORY_POINTER)
					LogError(context->global, expression->any.loc, "Trying to dereference a non pointer"_s);
				expression->typeTableIdx = expressionTypeInfo.pointerInfo.pointedTypeTableIdx;
			} break;
			default:
			{
				expression->typeTableIdx = expressionType;
			}
			};
		}
		else {
			bool found = TCLookForOperatorOverload(context, expression);
			ASSERT(found);
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;
		ASTExpression *rightHand = expression->binaryOperation.rightHand;

		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS) {
			TypeCheckExpression(context, leftHand);

			if (IsExpressionAType(context, leftHand))
				LogError(context->global, leftHand->any.loc, "Left hand of member access operator is a type"_s);

			u32 leftHandTypeIdx = leftHand->typeTableIdx;

			if (rightHand->nodeType != ASTNODETYPE_IDENTIFIER)
				LogError(context->global, rightHand->any.loc, "Expected identifier after member access operator"_s);

			rightHand->identifier.type = NAMETYPE_STRUCT_MEMBER;

			// Get rid of aliases
			u32 structTypeIdx = StripImplicitlyCastAliases(context->global, leftHandTypeIdx);

			TypeInfo structTypeInfo = GetTypeInfo(context, structTypeIdx);

			if (structTypeInfo.typeCategory == TYPECATEGORY_POINTER) {
				u32 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
				structTypeInfo = GetTypeInfo(context, pointedTypeIdx);
			}

			if (structTypeInfo.typeCategory == TYPECATEGORY_ARRAY) {
				// This is only for dynamic size arrays!
				if (structTypeInfo.arrayInfo.count != 0)
					LogError(context->global, expression->any.loc, "Array left of '.' has to be of dynamic size! ([])"_s);

				structTypeInfo = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT);
			}
			else if (structTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
					 structTypeInfo.typeCategory != TYPECATEGORY_UNION) {
				LogError(context->global, expression->any.loc, "Left of '.' has to be a struct/union"_s);
			}

			String memberName = rightHand->identifier.string;
			const StructMember *foundMember = FindStructMemberByName(context, structTypeInfo, memberName);
			if (foundMember)
			{
				rightHand->identifier.structMember = foundMember;
				expression->typeTableIdx = foundMember->typeTableIdx;
			}
			else
				LogError(context->global, expression->any.loc, TPrintF("\"%S\" is not a member of \"%S\"",
						memberName, TypeInfoToString(context, structTypeIdx)));
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
		{
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);

			if (IsExpressionAType(context, leftHand))
				LogError(context->global, leftHand->any.loc, "Input of array access is a type"_s);
			if (IsExpressionAType(context, rightHand))
				LogError(context->global, rightHand->any.loc, "Index of array access is a type"_s);

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
					LogError(context->global, leftHand->any.loc,
							"Expression does not evaluate to an array"_s);
				expression->typeTableIdx = arrayTypeInfo.arrayInfo.elementTypeTableIdx;
			}
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT) {
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);

			if (IsExpressionAType(context, leftHand))
				LogError(context->global, leftHand->any.loc, "Left hand of binary operator is a type"_s);
			if (IsExpressionAType(context, rightHand))
				LogError(context->global, rightHand->any.loc, "Right hand of binary operator is a type"_s);

			if (leftHand->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				u64 leftHandCount = leftHand->multipleExpressions.array.size;

				if (rightHand->nodeType == ASTNODETYPE_PROCEDURE_CALL) {
					// Check all left hand values against all return value types on the called
					// procedure.
					u32 procTypeIdx = rightHand->procedureCall.procedureTypeIdx;
					ArrayView<u32> returnTypeIndices =
						GetTypeInfo(context, procTypeIdx).procedureInfo.returnTypeIndices;
					if (leftHandCount != returnTypeIndices.size)
						LogError(context->global, expression->any.loc, TPrintF("Left hand expression has %d "
									"values, but right hand has %d", leftHandCount, returnTypeIndices.size));

					for (int i = 0; i < leftHandCount; ++i) {
						TypeCheckResult tcResult = CheckTypesMatchAndSpecialize(context,
								returnTypeIndices[i], leftHand->multipleExpressions.array[i]);
						if (tcResult.errorCode != TYPECHECK_COOL) {
							String leftStr =  TypeInfoToString(context, leftHand->multipleExpressions.array[i]->typeTableIdx);
							String rightStr = TypeInfoToString(context, returnTypeIndices[i]);
							LogError(context->global, expression->any.loc, TPrintF("Type mismatch on input "
										"number %d of operator %S (left hand is \"%S\" and right hand is \"%S\")",
										i, OperatorToString(expression->binaryOperation.op), leftStr, rightStr));
						}
						leftHand->multipleExpressions.array[i]->typeTableIdx = tcResult.leftTypeIdx;
					}
				}
				else {
					// Check both sides' expressions against each other.
					if (rightHand->nodeType != ASTNODETYPE_MULTIPLE_EXPRESSIONS)
						LogError(context->global, expression->any.loc, TPrintF("Left hand expression has %d "
									"values, but right hand has 1", leftHandCount));

					u64 rightHandCount = rightHand->multipleExpressions.array.size;
					if (leftHandCount != rightHandCount)
						LogError(context->global, expression->any.loc, TPrintF("Left hand expression has %d "
									"values, but right hand has %d", leftHandCount, rightHandCount));

					for (int i = 0; i < leftHandCount; ++i) {
						TypeCheckResult typeCheckResult =
							CheckTypesMatchAndSpecialize(context,
								leftHand->multipleExpressions.array[i]->typeTableIdx,
								rightHand->multipleExpressions.array[i]);
						leftHand->multipleExpressions.array[i]->typeTableIdx  = typeCheckResult.leftTypeIdx;
						rightHand->multipleExpressions.array[i]->typeTableIdx = typeCheckResult.rightTypeIdx;

						if (typeCheckResult.errorCode != TYPECHECK_COOL)
						{
							String leftStr =  TypeInfoToString(context, typeCheckResult.leftTypeIdx);
							String rightStr = TypeInfoToString(context, typeCheckResult.rightTypeIdx);
							LogError(context->global, expression->any.loc, TPrintF("Type mismatch on input "
										"number %d of operator %S (left hand is \"%S\" and right hand is \"%S\")",
										i, OperatorToString(expression->binaryOperation.op), leftStr, rightStr));
						}
					}
				}
			}
			else {
				if (rightHand->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS)
					LogError(context->global, expression->any.loc, TPrintF("Left hand expression has 1 "
								"value, but right hand has %d",
								rightHand->multipleExpressions.array.size));

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
						leftHand->typeTableIdx, rightHand);
				leftHand->typeTableIdx  = typeCheckResult.leftTypeIdx;
				rightHand->typeTableIdx = typeCheckResult.rightTypeIdx;

				if (typeCheckResult.errorCode != TYPECHECK_COOL) {
					String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
					String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
					LogError(context->global, expression->any.loc, TPrintF("Type mismatch on inputs of "
								"operator %S (left hand is \"%S\" and right hand is \"%S\")",
								OperatorToString(expression->binaryOperation.op),
								leftStr, rightStr));
				}
			}
		}
		else {
			TypeCheckExpression(context, leftHand);
			TypeCheckExpression(context, rightHand);

			if (IsExpressionAType(context, leftHand))
				LogError(context->global, leftHand->any.loc, "Left hand of binary operator is a type"_s);
			if (IsExpressionAType(context, rightHand))
				LogError(context->global, rightHand->any.loc, "Right hand of binary operator is a type"_s);

			if (TCIsPrimitiveOperation(context, expression->binaryOperation.op,
						leftHand->typeTableIdx, rightHand->typeTableIdx)) {
				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
						leftHand->typeTableIdx, rightHand);
				if (leftHand->typeTableIdx != typeCheckResult.leftTypeIdx)
					InferTypesInExpression(context, leftHand, typeCheckResult.leftTypeIdx);

				switch (expression->binaryOperation.op) {
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
						LogError(context->global, expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}
				} break;
				case TOKEN_OP_RANGE:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL) {
						String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
						LogError(context->global, expression->any.loc, TPrintF("Invalid types on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					// Both operands have to be integers
					TypeCategory leftCat = GetTypeInfo(context,
							StripAllAliases(context->global, leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER)
						LogError(context->global, leftHand->any.loc, TPrintF("Left hand of .. operator "
									"does not evaluate to an integer (%S)",
									TypeInfoToString(context, leftHand->typeTableIdx)));

					TypeCategory rightCat = GetTypeInfo(context,
							StripAllAliases(context->global, rightHand->typeTableIdx)).typeCategory;
					if (rightCat != TYPECATEGORY_INTEGER)
						LogError(context->global, rightHand->any.loc, TPrintF("Right hand of .. operator "
									"does not evaluate to an integer (%S)",
									TypeInfoToString(context, typeCheckResult.rightTypeIdx)));
				} break;
				case TOKEN_OP_ASSIGNMENT:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						String leftStr =  TypeInfoToString(context, leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(context, rightHand->typeTableIdx);
						LogError(context->global, expression->any.loc, TPrintF("Type mismatch on inputs of "
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
				case TOKEN_OP_NOT_EQUALS:
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
						LogError(context->global, expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					TypeCategory leftCat  = GetTypeInfo(context,
							StripAllAliases(context->global, leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER &&
						leftCat != TYPECATEGORY_FLOATING &&
						leftCat != TYPECATEGORY_ENUM &&
						leftCat != TYPECATEGORY_POINTER)
						LogError(context->global, expression->any.loc, TPrintF("Invalid types on inputs of "
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
						LogError(context->global, expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					TypeCategory leftCat  = GetTypeInfo(context,
							StripAllAliases(context->global, leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER &&
						leftCat != TYPECATEGORY_FLOATING)
						LogError(context->global, expression->any.loc, TPrintF("Invalid types on inputs of "
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
				case TOKEN_OP_NOT_EQUALS:
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
			else {
				bool found = TCLookForOperatorOverload(context, expression);
				ASSERT(found);
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
						LogError(context->global, leftExp->any.loc, "Expected identifier before '='"_s);

					TypeCheckExpression(context, rightExp);
				}
				else
					TypeCheckExpression(context, memberExp);
			}
			expression->typeTableIdx = TYPETABLEIDX_StructLiteral;
			break;
		case LITERALTYPE_CSTR:
			expression->typeTableIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_S8);
			break;
		default:
			ASSERT(!"Unexpected literal type");
		}
	} break;
	case ASTNODETYPE_IF:
	{
		TypeCheckExpression(context, expression->ifNode.condition);

		u32 conditionType = expression->ifNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context->global, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->ifNode.body);

		if (expression->ifNode.elseBody)
			TypeCheckExpression(context, expression->ifNode.elseBody);
	} break;
	case ASTNODETYPE_IF_STATIC:
	{
		TypeCheckExpression(context, expression->ifStaticNode.condition);

		u32 conditionType = expression->ifStaticNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context->global, expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		Constant conditionResult = TryEvaluateConstant(context, expression->ifStaticNode.condition);
		if (conditionResult.type == CONSTANTTYPE_INVALID)
			LogError(context->global, expression->ifStaticNode.condition->any.loc,
					"Failed to evaluate static if condition"_s);

		bool evaluatesToTrue = conditionResult.valueAsInt != 0;
		expression->ifStaticNode.evaluatesToTrue = evaluatesToTrue;

		if (evaluatesToTrue)
			TypeCheckExpression(context, expression->ifStaticNode.body);

		else if (expression->ifStaticNode.elseBody)
			TypeCheckExpression(context, expression->ifStaticNode.elseBody);
	} break;
	case ASTNODETYPE_WHILE:
	{
		TypeCheckExpression(context, expression->whileNode.condition);

		u32 conditionType = expression->whileNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(context, TYPETABLEIDX_BOOL,
				conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(context->global, expression->any.loc, "While condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(context, expression->whileNode.body);
	} break;
	case ASTNODETYPE_FOR:
	{
		ASTFor *astFor = &expression->forNode;

		ASTExpression *rangeExp = astFor->range;
		bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			rangeExp->binaryOperation.op == TOKEN_OP_RANGE;

		TypeCheckExpression(context, rangeExp);

		TCPushScope(context);

		u32 indexTypeIdx = TYPETABLEIDX_S64;
		if (isExplicitRange) {
			// Infer if it's literal numbers
			InferTypesInExpression(context, rangeExp, indexTypeIdx);
			indexTypeIdx = rangeExp->typeTableIdx;
		}

		u32 indexValueIdx = TCNewValue(context, astFor->indexVariableName, indexTypeIdx, 0);
		astFor->indexValueIdx = indexValueIdx;

		FixedArray<TCScopeName, 2> scopeNamesToAdd = {};

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = astFor->indexVariableName;
		newScopeName.variableInfo.valueIdx = indexValueIdx;
		newScopeName.variableInfo.typeTableIdx = indexTypeIdx;
		newScopeName.loc = expression->any.loc;
		*FixedArrayAdd(&scopeNamesToAdd) = newScopeName;

		if (!isExplicitRange) {
			u32 elementTypeTableIdx = TYPETABLEIDX_U8;
			if (rangeExp->typeTableIdx != TYPETABLEIDX_STRING_STRUCT) {
				TypeInfo rangeTypeInfo = GetTypeInfo(context, rangeExp->typeTableIdx);
				if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER)
					rangeTypeInfo = GetTypeInfo(context, rangeTypeInfo.pointerInfo.pointedTypeTableIdx);

				if (rangeTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
					LogError(context->global, astFor->range->any.loc, "'for' range "
							"expression does not evaluate to an array nor is it a number range "
							"(..)"_s);
				elementTypeTableIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			}

			u32 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementTypeTableIdx);
			u32 elementValueIdx = TCNewValue(context, astFor->itemVariableName,
					pointerToElementTypeIdx, 0);
			astFor->elementValueIdx = elementValueIdx;

			newScopeName.name = astFor->itemVariableName;
			newScopeName.variableInfo.valueIdx = elementValueIdx;
			newScopeName.variableInfo.typeTableIdx = pointerToElementTypeIdx;
			newScopeName.loc = expression->any.loc;
			*FixedArrayAdd(&scopeNamesToAdd) = newScopeName;
		}
		TCAddScopeNames(context, scopeNamesToAdd);

		u32 oldForArray = context->currentForLoopArrayType;
		context->currentForLoopArrayType = astFor->range->typeTableIdx;

		TypeCheckExpression(context, astFor->body);

		// Important to restore whether we yield or not!
		context->currentForLoopArrayType = oldForArray;

		TCPopScope(context);
	} break;
	case ASTNODETYPE_BREAK:
	case ASTNODETYPE_CONTINUE:
	{
	} break;
	case ASTNODETYPE_REMOVE:
	{
		TypeInfo forArrayType = GetTypeInfo(context, context->currentForLoopArrayType);
		if (forArrayType.typeCategory != TYPECATEGORY_ARRAY || forArrayType.arrayInfo.count != 0)
			LogError(context->global, expression->any.loc, "'remove' found but there wasn't a for loop "
					"with a dynamic sized array as range"_s);
	} break;
	case ASTNODETYPE_TYPE:
	case ASTNODETYPE_ALIAS:
	{
		expression->typeTableIdx = TypeCheckType(context, {}, expression->any.loc,
				&expression->astType);
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		TypeCheckExpression(context, expression->typeOfNode.expression);

		static u32 typeInfoPointerTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT);
		expression->typeTableIdx = typeInfoPointerTypeIdx;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		TypeCheckExpression(context, expression->sizeOfNode.expression);
		expression->typeTableIdx = TYPETABLEIDX_U64;
	} break;
	case ASTNODETYPE_CAST:
	{
		TypeCheckExpression(context, expression->castNode.expression);

		u32 typeCheckResult = TypeCheckType(context, {}, expression->any.loc,
				&expression->castNode.astType);

		TypeCheckResult typeSpecializeResult = CheckTypesMatchAndSpecialize(context, typeCheckResult,
				expression->castNode.expression);
		expression->castNode.expression->typeTableIdx = typeSpecializeResult.rightTypeIdx;

		expression->typeTableIdx = typeCheckResult;
	} break;
	case ASTNODETYPE_INTRINSIC:
	{
		if (expression->intrinsic.type == INTRINSIC_UNSET) {
			if (StringEquals(expression->intrinsic.name, "breakpoint"_s))
				expression->intrinsic.type = INTRINSIC_BREAKPOINT;
			else if (StringEquals(expression->intrinsic.name, "sqrt32"_s))
				expression->intrinsic.type = INTRINSIC_SQRT32;
			else if (StringEquals(expression->intrinsic.name, "sqrt64"_s))
				expression->intrinsic.type = INTRINSIC_SQRT64;
			else
				LogError(context->global, expression->any.loc, "Invalid compiler intrinsic"_s);
		}

		FixedArray<u32, 4> argTypes;
		switch (expression->intrinsic.type) {
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
			LogError(context->global, expression->any.loc, "Too many arguments for intrinsic"_s);
		for (int argIdx = 0; argIdx < expression->intrinsic.arguments.size; ++argIdx) {
			ASTExpression *arg = &expression->intrinsic.arguments[argIdx];
			TypeCheckExpression(context, arg);
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(context,
					argTypes[argIdx], arg);
			arg->typeTableIdx = typeCheckResult.rightTypeIdx;

			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				String paramStr = TypeInfoToString(context, argTypes[argIdx]);
				String givenStr = TypeInfoToString(context, arg->typeTableIdx);
				LogError(context->global, arg->any.loc, TPrintF("When calling intrinsic \"%S\": type of "
							"parameter #%d didn't match (parameter is %S but %S was given)",
							expression->intrinsic.name, argIdx, paramStr, givenStr));
			}
		}
		expression->typeTableIdx = TYPETABLEIDX_VOID;
	} break;
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		ASTOperatorOverload *astOverload = &expression->operatorOverload;

		TCContext procedureContext = *context;
		BucketArrayInit(&procedureContext.localValues);
		*BucketArrayAdd(&procedureContext.localValues) = {}; // No value number 0?
		// Don't allocate a new array if old is empty.
		if (context->scopeStack.size)
			DynamicArrayInit(&procedureContext.scopeStack, 8);
		procedureContext.onStaticContext = false;

		static u64 overloadUniqueId = 0;

		TypeCheckProcedurePrototype(&procedureContext, &astOverload->prototype);
		TypeInfo t = TypeInfoFromASTProcedurePrototype(context, &astOverload->prototype);

		u32 typeTableIdx = FindOrAddTypeTableIdx(context, t);

		OperatorOverload overload = {};
		overload.op = astOverload->op;

		Procedure p = {};
		p.typeTableIdx = TYPETABLEIDX_Unset;
		p.name = SNPrintF(18, "__overload%d_%d", overload.op, overloadUniqueId++);
		p.typeTableIdx = typeTableIdx;
		p.astBody = astOverload->astBody;
		p.isInline = astOverload->isInline;
		p.astPrototype = astOverload->prototype;
		DynamicArrayInit(&p.parameterValues, 4);
		overload.procedureIdx = NewProcedure(context->global, p, false);

		astOverload->procedureIdx = overload.procedureIdx;

		TCPushScope(&procedureContext);

		Procedure procedure = GetProcedureRead(context->global, astOverload->procedureIdx);

		// Return values
		u64 returnValueCount = astOverload->prototype.returnTypeIndices.size;
		if (returnValueCount) {
			ArrayInit(&procedure.returnValueIndices, returnValueCount);
			for (int i = 0; i < returnValueCount; ++i) {
				u32 returnType = astOverload->prototype.returnTypeIndices[i];
				*ArrayAdd(&procedure.returnValueIndices) = TCNewValue(&procedureContext, "_returnValue"_s, returnType, 0);
			}
		}

		// Parameters
		u64 paramCount = astOverload->prototype.astParameters.size;
		if (paramCount == 0) {
			LogError(context->global, expression->any.loc, TPrintF(
					"No parameters provided on overload for operator %S.",
					OperatorToString(astOverload->op)));
		}
		else if (paramCount == 1) {
			if (astOverload->op != TOKEN_OP_NOT &&
				astOverload->op != TOKEN_OP_BITWISE_NOT &&
				astOverload->op != TOKEN_OP_MINUS)
				LogError(context->global, expression->any.loc, TPrintF(
							"Only 1 parameter is present on overload for operator %S. "
							"Expected 2.", OperatorToString(astOverload->op)));

			// No overloading builtin operations
			u32 inputHandTypeIdx = astOverload->prototype.astParameters[0].typeTableIdx;
			if (TCIsPrimitiveOperation(context, astOverload->op, inputHandTypeIdx))
				LogError(context->global, expression->any.loc, "Overloading a vanilla operation is forbidden"_s);
		}
		else if (paramCount == 2) {
			if (astOverload->op == TOKEN_OP_NOT ||
				astOverload->op == TOKEN_OP_BITWISE_NOT)
				LogError(context->global, expression->any.loc, TPrintF(
							"2 parameters found on overload for operator %S. "
							"Expected 1.", OperatorToString(astOverload->op)));

			// No overloading builtin operations
			u32 leftHandTypeIdx  = astOverload->prototype.astParameters[0].typeTableIdx;
			u32 rightHandTypeIdx = astOverload->prototype.astParameters[1].typeTableIdx;
			if (TCIsPrimitiveOperation(context, astOverload->op, leftHandTypeIdx, rightHandTypeIdx))
				LogError(context->global, expression->any.loc, "Overloading a vanilla operation is forbidden"_s);
		}
		else {
			LogError(context->global, expression->any.loc, TPrintF(
					"Too many parameters provided on overload for operator %S.",
					OperatorToString(astOverload->op)));
		}

		{
			auto operatorOverloads = context->global->operatorOverloads.GetForWrite();
			*DynamicArrayAdd(&operatorOverloads) = overload;
		}

		// Wake up any job waiting for this overload.
		WakeUpAllByIndex(context->global, YIELDREASON_UNKNOWN_OVERLOAD, astOverload->op);
		static_assert(offsetof(YieldContext, index) == offsetof(YieldContext, overload.op));
		static_assert(  sizeof(YieldContext::index) ==   sizeof(YieldContext::overload.op));

		for (int i = 0; i < paramCount; ++i) {
			ASTProcedureParameter astParameter = astOverload->prototype.astParameters[i];
			u32 paramValueIdx = TCNewValue(&procedureContext, astParameter.name, astParameter.typeTableIdx, 0);
			*DynamicArrayAdd(&procedure.parameterValues) = paramValueIdx;
		}
		// Varargs array
		ASSERT(!astOverload->prototype.isVarargs);

		TCAddParametersToScope(&procedureContext, procedure.parameterValues, &astOverload->prototype);

		if (astOverload->astBody) {
			procedureContext.currentProcedureIdx = astOverload->procedureIdx;
			procedureContext.currentReturnTypes = t.procedureInfo.returnTypeIndices;

			TypeCheckExpression(&procedureContext, astOverload->astBody);
		}
		procedure.isBodyTypeChecked = true;

		UpdateProcedure(context->global, astOverload->procedureIdx, &procedure);
		// Wake up any jobs that were waiting for this procedure body
		WakeUpAllByIndex(context->global, YIELDREASON_PROC_BODY_NOT_READY, astOverload->procedureIdx);

		expression->typeTableIdx = procedure.typeTableIdx;

		TCPopScope(&procedureContext);

		// Check all paths return
		if (astOverload->astBody && t.procedureInfo.returnTypeIndices.size) {
			ReturnCheckResult result = TCCheckIfReturnsValue(context->global, astOverload->astBody);
			if (result == RETURNCHECKRESULT_SOMETIMES)
				LogError(context->global, expression->any.loc, "Procedure doesn't always return a value"_s);
			else if (result == RETURNCHECKRESULT_NEVER)
				LogError(context->global, expression->any.loc, "Procedure has to return a value"_s);
		}
	} break;
	case ASTNODETYPE_INCLUDE:
	{
		String filename = expression->include.filename;
		CompilerAddSourceFile(context->global, filename, expression->any.loc);
	} break;
	case ASTNODETYPE_LINKLIB:
	{
		String filename = expression->linklib.filename;
		*DynamicArrayAdd(&context->global->libsToLink) = filename;

		CTLibrary ctLib = { .name = ChangeFilenameExtension(filename, ".dll"_s),
			.loc = expression->any.loc };
		auto ctLibs = context->global->ctExternalLibraries.Get();
		*DynamicArrayAdd(&ctLibs) = ctLib;

		// Wake up any jobs that are looking for dynamic libraries
		{
			auto jobsWaiting = context->global->waitingJobsByReason[YIELDREASON_NEED_DYNAMIC_LIBRARY].Get();
			for (int i = 0; i < jobsWaiting->size; ++i) {
				u32 jobIdx = jobsWaiting[i];
				EnqueueReadyJob(context->global, jobIdx);
			}
			jobsWaiting->size = 0;
		}
	} break;
	case ASTNODETYPE_DEFINED:
	{
		expression->typeTableIdx = TYPETABLEIDX_BOOL;

		String identifier = expression->definedNode.identifier;
		bool isDefined = false;

		// Current stack
		ArrayView<TCScope> scopeStack = context->scopeStack;
		for (s64 stackIdx = scopeStack.size - 1; stackIdx >= 0; --stackIdx) {
			const TCScope *currentScope = &scopeStack[stackIdx];
			for (int i = 0; i < currentScope->names.size; ++i) {
				const TCScopeName *currentName = &currentScope->names[i];
				if (StringEquals(identifier, currentName->name)) {
					isDefined = true;
					goto done;
				}
			}
		}
		// Global scope
		{
			auto globalNames = context->global->tcGlobalNames.GetForRead();
			for (int i = 0; i < globalNames->size; ++i) {
				const TCScopeName *currentName = &globalNames[i];
				if (StringEquals(identifier, currentName->name)) {
					isDefined = true;
					goto done;
				}
			}
		}
		SwitchJob(context, YIELDREASON_WAITING_FOR_STOP, {});
		// Look one last time...
		{
			auto globalNames = context->global->tcGlobalNames.GetForRead();
			// @Improve: resume loop at the last index we checked?
			for (int i = 0; i < globalNames->size; ++i) {
				const TCScopeName *currentName = &globalNames[i];
				if (StringEquals(identifier, currentName->name)) {
					isDefined = true;
					goto done;
				}
			}
		}
		// Not defined!
		// @Todo: keep track of stuff we assumed not to be defined. If at the end it is, report an
		// error.
done:
		expression->definedNode.isDefined = isDefined;
	} break;
	case ASTNODETYPE_COMPILER_BREAKPOINT:
	{
		if (expression->compilerBreakpointType == COMPILERBREAKPOINT_TYPE_CHECKER)
			BREAK;
	} break;
	case ASTNODETYPE_RUN:
	{
		TCContext tcRunContext = *context;
		BucketArrayInit(&tcRunContext.localValues);
		*BucketArrayAdd(&tcRunContext.localValues) = {}; // No value number 0?

		TypeCheckExpression(&tcRunContext, expression->runNode.expression);
		expression->typeTableIdx = expression->runNode.expression->typeTableIdx;

		BucketArray<IRInstruction, LinearAllocator, 256> irInstructions;
		BucketArrayInit(&irInstructions);

		IRContext irJobContext = {};
		irJobContext.global = context->global;
		irJobContext.irInstructions = &irInstructions;
		irJobContext.localValues = &tcRunContext.localValues;

		IRValue resultIRValue = IRGenFromExpression(&irJobContext, expression->runNode.expression);

		CTRegister runResult = CTRunInstructions(context, tcRunContext.localValues, irInstructions,
				resultIRValue);

		expression->runNode.result = ConstantFromCTRegister(runResult, expression->typeTableIdx);
	} break;
	default:
		LogCompilerError(context->global, expression->any.loc, "Unknown expression type on type checking"_s);
	}
}

void TCJobProc(u32 jobIdx, void *args)
{
	TCJobArgs *argsStruct = (TCJobArgs *)args;

	TCContext *context = ALLOC(LinearAllocator, TCContext);
	context->global = argsStruct->context;
	context->jobIdx = jobIdx;

	ASTExpression *expression = argsStruct->expression;
	context->currentProcedureIdx = U32_MAX;
	context->onStaticContext = true;
	context->currentReturnTypes = {};

	Job *runningJob = GetCurrentJob(context);
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->loc = expression->any.loc;

	String threadName = "TC:???"_s;
	switch (expression->nodeType) {
	case ASTNODETYPE_STATIC_DEFINITION:
		switch (expression->staticDefinition.expression->nodeType) {
		case ASTNODETYPE_PROCEDURE_DECLARATION:
			threadName = SNPrintF(96, "TC:%S - Procedure declaration",
					expression->staticDefinition.name);
			break;
		case ASTNODETYPE_TYPE:
		case ASTNODETYPE_ALIAS:
			threadName = SNPrintF(96, "TC:%S - Type declaration",
					expression->staticDefinition.name);
			break;
		case ASTNODETYPE_IDENTIFIER:
			threadName = SNPrintF(96, "TC:%S - Constant declaration",
					expression->staticDefinition.name);
			break;
		default:
			threadName = SNPrintF(96, "TC:%S - Static definition",
					expression->staticDefinition.name);
			break;
		}
		break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
		threadName = SNPrintF(96, "TC:%S - Variable declaration",
				expression->staticDefinition.name);
		break;
	case ASTNODETYPE_IF_STATIC:
		threadName = "TC:Static if"_s;
		break;
	}
	runningJob->description = threadName;
#endif

	switch (expression->nodeType) {
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	case ASTNODETYPE_RUN:
	case ASTNODETYPE_INCLUDE:
	case ASTNODETYPE_LINKLIB:
	case ASTNODETYPE_IF_STATIC:
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		DynamicArrayInit(&context->scopeStack, 8);
	} break;
	default:
		PANIC;
	}

	TypeCheckExpression(context, expression);

	FinishCurrentJob(context);
}

int GetTypeAlignment(JobContext *context, u32 typeTableIdx)
{
	int alignment = 0;
	TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);

	// Alignment may be specified by the programmer.
	if (typeInfo.alignment) {
		ASSERT(IsPowerOf2(typeInfo.alignment));
		return typeInfo.alignment;
	}

	if (typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		typeInfo.typeCategory == TYPECATEGORY_UNION)
	{
		for (int i = 0; i < typeInfo.structInfo.members.size; ++i) {
			int memberAlignment = GetTypeAlignment(context,
					typeInfo.structInfo.members[i].typeTableIdx);
			if (memberAlignment > alignment)
				alignment = memberAlignment;
		}
	}
	else if (typeInfo.typeCategory == TYPECATEGORY_ARRAY)
		return GetTypeAlignment(context, typeInfo.arrayInfo.elementTypeTableIdx);
	else {
		alignment = 8;
		if (typeInfo.size < 8)
			alignment = NextPowerOf2((int)typeInfo.size);
	}
	ASSERT(IsPowerOf2(alignment));
	return alignment;
}

void TCStructJobProc(u32 jobIdx, void *args)
{
	TCStructJobArgs *argsStruct = (TCStructJobArgs *)args;

	TCContext *context = ALLOC(LinearAllocator, TCContext);
	context->global = argsStruct->context;
	context->jobIdx = jobIdx;

	context->onStaticContext = true;
	context->currentReturnTypes = {};

	Job *runningJob = GetCurrentJob(context);
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->description = SNPrintF(96, "TC:Struct \"%S\"", argsStruct->name);
#endif

	u32 typeTableIdx = argsStruct->typeTableIdx;
	TypeInfo t;
	{
		auto &typeTable = context->global->typeTable.unsafe;
		ASSERT(typeTable[typeTableIdx].typeCategory == TYPECATEGORY_NOT_READY);
		t = typeTable[typeTableIdx];
	}
	t.typeCategory = argsStruct->isUnion ? TYPECATEGORY_UNION : TYPECATEGORY_STRUCT;
	t.structInfo.name = argsStruct->name;

	DynamicArray<StructMember, LinearAllocator> structMembers;
	DynamicArrayInit(&structMembers, 16);

	int largestAlignment = 0;
	for (int memberIdx = 0; memberIdx < argsStruct->astStructDecl.members.size; ++memberIdx) {
		ASTStructMemberDeclaration *astMember = &argsStruct->astStructDecl.members[memberIdx];

		if (astMember->astType == nullptr)
			LogError(context->global, astMember->loc, TPrintF("Type missing in declaration of struct "
					"member \"%S\"", astMember->name));

		if (astMember->value != nullptr)
			LogWarning(context->global, astMember->value->any.loc, TPrintF("Default value found on member "
						"\"%S\". This is not yet supported", astMember->name));

		astMember->typeTableIdx = TypeCheckType(context, {}, astMember->loc, astMember->astType);

		StructMember member = {};
		member.name = astMember->name;
		member.isUsing = astMember->isUsing;
		member.typeTableIdx = astMember->typeTableIdx;

		TypeInfo memberTypeInfo = GetTypeInfo(context, member.typeTableIdx);
		if (astMember->isUsing && memberTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
				memberTypeInfo.typeCategory != TYPECATEGORY_UNION)
			LogError(context->global, astMember->loc, TPrintF("'using' keyword only supported for struct "
					"or union members, but \"%S\" was %S", astMember->name,
					TypeCategoryToString(memberTypeInfo.typeCategory)));

		u64 memberSize = GetTypeInfo(context, member.typeTableIdx).size;
		int alignment = GetTypeAlignment(context, member.typeTableIdx);

		if (alignment > largestAlignment)
			largestAlignment = alignment;

		if (!argsStruct->isUnion) {
			// Struct
			if (t.size & (alignment - 1))
				t.size = (t.size & ~(alignment - 1)) + alignment;
			member.offset = t.size;
			t.size += memberSize;
		}
		else {
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

	// Update stub type
	{
		// Unsafe!
		auto &typeTable = context->global->typeTable.unsafe;
		ASSERT(typeTable[typeTableIdx].typeCategory == TYPECATEGORY_NOT_READY);
		t.valueIdx = typeTable[typeTableIdx].valueIdx;
		(TypeInfo&)typeTable[typeTableIdx] = t;

		WriteUserFacingTypeInfoToStaticData(context, t);
	}

	// Wake up any jobs that were waiting for this type
	WakeUpAllByIndex(context->global, YIELDREASON_TYPE_NOT_READY, typeTableIdx);

	FinishCurrentJob(context);
}

void GenerateTypeCheckJobs(Context *context, ASTExpression *expression) {
	switch (expression->nodeType) {
	case ASTNODETYPE_BLOCK:
	{
		for (int i = 0; i < expression->block.statements.size; ++i)
			GenerateTypeCheckJobs(context, &expression->block.statements[i]);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	case ASTNODETYPE_STATIC_DEFINITION:
	case ASTNODETYPE_RUN:
	case ASTNODETYPE_INCLUDE:
	case ASTNODETYPE_LINKLIB:
	case ASTNODETYPE_IF_STATIC:
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		TCJobArgs *args = ALLOC(LinearAllocator, TCJobArgs);
		*args = {
			.context = context,
			.expression = expression };
		RequestNewJob(context, JOBTYPE_TYPE_CHECK, TCJobProc, (void *)args);
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
		LogCompilerError(context, expression->any.loc, "Invalid expression type found while "
				"generating type checking jobs"_s);
	} break;
	default:
	{
		LogCompilerError(context, expression->any.loc, "Unknown expression type found while "
				"generating type checking jobs"_s);
	}
	}
}

void TypeCheckMain(Context *context)
{
	// Initialize memory and bookkeep of types for static data
	u64 virtualRangeSize = (u64)(STATIC_DATA_VIRTUAL_ADDRESS_END - STATIC_DATA_VIRTUAL_ADDRESS);
	VirtualAlloc(STATIC_DATA_VIRTUAL_ADDRESS, virtualRangeSize, MEM_RESERVE, PAGE_READWRITE);
	VirtualAlloc(STATIC_DATA_VIRTUAL_ADDRESS, 0x100000, MEM_COMMIT, PAGE_READWRITE);
	memset(STATIC_DATA_VIRTUAL_ADDRESS, 0xCC, 0x100000);
	context->staticDataAllocatedSpace = 0x100000;
	context->staticDataSize = 0;
	context->staticDataLock = 0;
	DynamicArrayInit(&context->staticDataPointersToRelocate, 1024);

	HashMapInit(&context->globalValueContents, 64);
	context->globalValuesLock = 0;

	{
		auto staticDefinitions = context->staticDefinitions.GetForWrite();
		BucketArrayInit(&staticDefinitions);
	}

	{
		auto globalValues = context->globalValues.GetForWrite();
		BucketArrayInit(&globalValues);
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
		SpinlockLock(&context->typeTable.lock);
		auto &typeTable = context->typeTable.unsafe;
		BucketArrayInit(&typeTable);
		for (int i = 0; i < TYPETABLEIDX_Count; ++i)
			BucketArrayAdd(&typeTable);

		TypeInfo *typeTableFast = (TypeInfo *)typeTable.buckets[0];

		TypeInfo t = {};
		t.typeCategory = TYPECATEGORY_INTEGER;
		t.integerInfo.isSigned = true;

		t.size = 1;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s8"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S8]  = t;
		t.size = 2;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s16"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S16] = t;
		t.size = 4;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s32"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S32] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_s64"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S64] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_integer"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_INTEGER] = t;

		t.integerInfo.isSigned = false;

		t.size = 1;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u8"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U8]  = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_bool"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_BOOL]  = t;
		t.size = 2;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u16"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U16] = t;
		t.size = 4;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u32"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U32] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_u64"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U64] = t;

		t.size = 16;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_128"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_128] = t;

		t.typeCategory = TYPECATEGORY_FLOATING;
		t.size = 4;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_f32"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F32] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_floating"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_FLOATING] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_f64"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F64] = t;

		t = {};
		t.typeCategory = TYPECATEGORY_INVALID;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_void"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_VOID] = t;

		t.typeCategory = TYPECATEGORY_NOT_READY;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_string_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_STRING_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_array_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ARRAY_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_any_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ANY_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_integer_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_struct_member_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_struct_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_enum_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_pointer_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_array_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_procedure_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_PROCEDURE_STRUCT] = t;
		t.valueIdx = NewGlobalValue(context, "_typeInfo_type_info_alias_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ALIAS_STRUCT] = t;

		SpinlockUnlock(&context->typeTable.lock);

		JobContext fakeJobContext = { context, U32_MAX };
		for (int i = TYPETABLEIDX_PrimitiveBegin; i < TYPETABLEIDX_PrimitiveEnd; ++i)
			// Yeah this needs a job context but it shouldn't need to yield to do this
			WriteUserFacingTypeInfoToStaticData(&fakeJobContext, typeTableFast[i]);

		for (int i = TYPETABLEIDX_BuiltinStructsBegin; i < TYPETABLEIDX_BuiltinStructsEnd; ++i)
			AllocateStaticData(context, typeTableFast[i].valueIdx, sizeof(UserFacingTypeInfoStruct), 8);
	}

	{
		auto globalTypes = context->tcGlobalTypeIndices.GetForWrite();
		DynamicArrayInit(&globalTypes, 64);
		for (int i = 0; i < TYPETABLEIDX_Count; ++i)
			*DynamicArrayAdd(&globalTypes) = i;
	}

	{
		auto globalNames = context->tcGlobalNames.GetForWrite();

		ArrayInit(&context->tcPrimitiveTypes, TYPETABLEIDX_PrimitiveEnd -
				TYPETABLEIDX_PrimitiveBegin);

		DynamicArrayInit(&globalNames, 64);

		TCScopeName scopeNamePrimitive;
		scopeNamePrimitive.type = NAMETYPE_PRIMITIVE;
		scopeNamePrimitive.loc = {};

		scopeNamePrimitive.name = "s8"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S8;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s16"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S16;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S32;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S64;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u8"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U8;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u16"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U16;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U32;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U64;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "f32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_F32;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "f64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_F64;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "bool"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_BOOL;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "void"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_VOID;
		*ArrayAdd(&context->tcPrimitiveTypes) = scopeNamePrimitive;
		*DynamicArrayAdd(&globalNames) = scopeNamePrimitive;
	}

	{
		auto inlineCalls = context->tcInlineCalls.Get();
		DynamicArrayInit(&inlineCalls, 128);
	}

	{
		auto ctLibraries = context->ctExternalLibraries.Get();
		DynamicArrayInit(&ctLibraries, 32);
	}
}
