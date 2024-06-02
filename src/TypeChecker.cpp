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

u32 TCNewValue(TCContext *tcContext, u32 typeTableIdx, u32 flags)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(typeTableIdx > TYPETABLEIDX_Unset);

	u64 idx = tcContext->localValues.count;
	Value *result = BucketArrayAdd(&tcContext->localValues);
#if DEBUG_BUILD
	result->name = {};
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 TCNewValue(TCContext *tcContext, String name, u32 typeTableIdx, u32 flags)
{
	ASSERT(typeTableIdx != 0);
	ASSERT(typeTableIdx > TYPETABLEIDX_Unset);

	u64 idx = tcContext->localValues.count;
	Value *result = BucketArrayAdd(&tcContext->localValues);
#if DEBUG_BUILD
	result->name = name;
#else
	(void) name;
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

u32 TCNewValue(TCContext *tcContext, Value value)
{
	ASSERT(value.typeTableIdx != 0);
	ASSERT(value.typeTableIdx > TYPETABLEIDX_Unset);

	u64 idx = tcContext->localValues.count;
	Value *result = BucketArrayAdd(&tcContext->localValues);
	*result = value;

	ASSERT(idx < U32_MAX);
	return (u32)idx;
}

inline Value *TCGetValue(TCContext *tcContext, u32 valueIdx)
{
	ASSERT(valueIdx > 0);
	ASSERT(!(valueIdx & VALUE_GLOBAL_BIT));
	return &tcContext->localValues[valueIdx];
}

u32 NewGlobalValue(u32 typeTableIdx, u32 flags)
{
	ASSERT(typeTableIdx != 0);

	auto globalValues = g_context->globalValues.GetForWrite();

	u64 idx = globalValues->count;
	Value *result = BucketArrayAdd(&globalValues);
#if DEBUG_BUILD
	result->name = {};
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->externalSymbolName = {};

	ASSERT(idx < U32_MAX);
	idx |= VALUE_GLOBAL_BIT;
	return (u32)idx;
}

u32 NewGlobalValue(String name, u32 typeTableIdx, u32 flags)
{
	ASSERT(typeTableIdx != 0);

	auto globalValues = g_context->globalValues.GetForWrite();

	u64 idx = globalValues->count;
	Value *result = BucketArrayAdd(&globalValues);
#if DEBUG_BUILD
	result->name = name;
#endif
	result->typeTableIdx = typeTableIdx;
	result->flags = flags;
	result->externalSymbolName = StringMinify(name);

	ASSERT(idx < U32_MAX);
	idx |= VALUE_GLOBAL_BIT;
	return (u32)idx;
}

u32 NewGlobalValue(Value value) {
	ASSERT(value.typeTableIdx != 0);

	auto globalValues = g_context->globalValues.GetForWrite();

	u64 idx = globalValues->count;
	Value *result = BucketArrayAdd(&globalValues);
	*result = value;

	ASSERT(idx < U32_MAX);
	idx |= VALUE_GLOBAL_BIT;
	return (u32)idx;
}

inline Value GetGlobalValue(u32 valueIdx) {
	ASSERT(valueIdx & VALUE_GLOBAL_BIT);
	auto globalValues = g_context->globalValues.GetForRead();
	Value result = globalValues[valueIdx & VALUE_GLOBAL_MASK];
	return result;
}

inline void UpdateGlobalValue(u32 valueIdx, Value *value) {
	ASSERT(valueIdx & VALUE_GLOBAL_BIT);
	auto globalValues = g_context->globalValues.GetForWrite();
	globalValues[valueIdx & VALUE_GLOBAL_MASK] = *value;
}

inline void TCSetValueFlags(TCContext *tcContext, u32 valueIdx, u32 flags) {
	if (valueIdx & VALUE_GLOBAL_BIT) {
		auto globalValues = g_context->globalValues.GetForWrite();
		globalValues[valueIdx & VALUE_GLOBAL_MASK].flags |= flags;
	}
	else
		tcContext->localValues[valueIdx].flags |= flags;
}

inline Value TCGetValueRead(TCContext *tcContext, u32 valueIdx) {
	ASSERT(valueIdx > 0);
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(valueIdx);
	else {
		return tcContext->localValues[valueIdx];
	}
}

inline Procedure GetProcedureRead(u32 procedureIdx) {
	Procedure result;
	ASSERT(procedureIdx != 0);
	if (procedureIdx & PROCEDURE_EXTERNAL_BIT) {
		auto externalProcedures = g_context->externalProcedures.GetForRead();
		result = externalProcedures[procedureIdx & PROCEDURE_EXTERNAL_MASK];
	}
	else {
		auto procedures = g_context->procedures.GetForRead();
		result = procedures[procedureIdx];
	}

	return result;
}

inline void UpdateProcedure(u32 procedureIdx, Procedure *value) {
	ASSERT(procedureIdx != 0);
	if (procedureIdx & PROCEDURE_EXTERNAL_BIT) {
		auto externalProcedures = g_context->externalProcedures.GetForWrite();
		externalProcedures[procedureIdx & PROCEDURE_EXTERNAL_MASK] = *value;
	}
	else {
		auto procedures = g_context->procedures.GetForWrite();
		procedures[procedureIdx] = *value;
	}
}

inline ASTExpression *TCNewTreeNode() {
	return ALLOC(ThreadAllocator, ASTExpression);
}

TCScope *TCGetTopMostScope(TCContext *tcContext) {
	if (tcContext->scopeStack.size > 0)
		return DynamicArrayBack(&tcContext->scopeStack);
	else
		return nullptr;
}

inline TypeInfo GetTypeInfo(u32 typeTableIdx)
{
	ASSERT(typeTableIdx > TYPETABLEIDX_Unset);

	// Optimize common path: type IS ready! don't lock anything, just read.
	auto &typeTable = g_context->typeTable.unsafe;
	TypeInfo result = typeTable[typeTableIdx];
	if (result.typeCategory != TYPECATEGORY_NOT_READY)
		return result;
	// Otherwise, lock and read again

	// Lock this, if we need to switch fibers, we don't unlock until we added this fiber to a
	// waiting list.
	// We lock the list of jobs here instead of the type table because no one locks the type table
	// to read it.
	SpinlockLock(&g_context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
	result = typeTable[typeTableIdx];

	if (result.typeCategory == TYPECATEGORY_NOT_READY) {
		// IMPORTANT! The scheduler will unlock jobsWaitingForType once it adds this job to the
		// waiting list, so we don't miss waking it up when updating the type.
		SwitchJob(YIELDREASON_TYPE_NOT_READY, { .index = typeTableIdx });
		// Lock again!
		SpinlockLock(&g_context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);

		result = typeTable[typeTableIdx];
		if (result.typeCategory == TYPECATEGORY_NOT_READY)
			LogCompilerError({}, "Bad job resume"_s);
	}

	SpinlockUnlock(&g_context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
	return result;
}

String TypeInfoToString(u32 typeTableIdx)
{
	if (typeTableIdx == TYPETABLEIDX_Anything)
		return "<anything>"_s;
	if (typeTableIdx == TYPETABLEIDX_VOID)
		return "void"_s;
	if (typeTableIdx == TYPETABLEIDX_INTEGER)
		return "<number>"_s;
	if (typeTableIdx == TYPETABLEIDX_FLOATING)
		return "<floating>"_s;

	TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
	switch (typeInfo.typeCategory) {
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
		String nameOfAliased = TypeInfoToString(typeInfo.aliasInfo.aliasedTypeIdx);
		if (name.size)
			return TPrintF("%S (alias of %S)", name, nameOfAliased);
		return TPrintF("<alias of %S>", nameOfAliased);
	}
	case TYPECATEGORY_POINTER:
		return TStringConcat("^"_s, TypeInfoToString(typeInfo.pointerInfo.pointedTypeTableIdx));
	case TYPECATEGORY_ARRAY:
	{
		String typeStr = TypeInfoToString(typeInfo.arrayInfo.elementTypeTableIdx);
		return TPrintF("[%d] %S", typeInfo.arrayInfo.count, typeStr);
	}
	case TYPECATEGORY_INTEGER:
	{
		if (typeInfo.integerInfo.isSigned) switch (typeInfo.size) {
			case 1: return "s8"_s;
			case 2: return "s16"_s;
			case 4: return "s32"_s;
			case 8: return "s64"_s;
		}
		else switch (typeInfo.size) {
			case 1: return "u8"_s;
			case 2: return "u16"_s;
			case 4: return "u32"_s;
			case 8: return "u64"_s;
		}
	} break;
	case TYPECATEGORY_FLOATING:
	{
		switch (typeInfo.size) {
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
			String paramStr = TypeInfoToString(typeInfo.procedureInfo.parameters[i].typeTableIdx);
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
			String paramStr = TypeInfoToString(typeInfo.procedureInfo.returnTypeIndices[i]);
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

inline void TCPushScope(TCContext *tcContext)
{
	TCScope *newScope = DynamicArrayAdd(&tcContext->scopeStack);

	DynamicArrayInit(&newScope->names, 64);
	DynamicArrayInit(&newScope->typeIndices, 64);
}

inline void TCPopScope(TCContext *tcContext)
{
	--tcContext->scopeStack.size;
}

TCScopeName FindGlobalName(SourceLocation loc, String name)
{
	// Lock this, if we need to switch fibers, we don't unlock until we added this fiber to a
	// waiting list.
	RWSpinlockLockForRead(&g_context->tcGlobalNames.rwLock);
	{
		auto &globalNames = g_context->tcGlobalNames.unsafe;
		for (int i = 0; i < globalNames.count; ++i) {
			const TCScopeName *currentName = &globalNames[i];
			if (StringEquals(name, currentName->name)) {
				RWSpinlockUnlockForRead(&g_context->tcGlobalNames.rwLock);
				return *currentName;
			}
		}
	}

	// IMPORTANT! The scheduler will unlock tcGlobalNames once it adds this job to
	// the waiting list, so we don't miss waking it up when adding the identifier.
	SwitchJob(YIELDREASON_UNKNOWN_IDENTIFIER, { .loc = loc, .identifier = name });

	// Lock again
	RWSpinlockLockForRead(&g_context->tcGlobalNames.rwLock);
	{
		auto &globalNames = g_context->tcGlobalNames.unsafe;
		for (int i = 0; i < globalNames.count; ++i) {
			const TCScopeName *currentName = &globalNames[i];
			if (StringEquals(name, currentName->name)) {
				RWSpinlockUnlockForRead(&g_context->tcGlobalNames.rwLock);
				return *currentName;
			}
		}
	}
	// Shouldn't have resumed this job if the identifier is still missing.
	LogCompilerError(loc, "Bad job resume"_s);
}

TCScopeName TCFindScopeName(TCContext *tcContext, SourceLocation loc, String name)
{
	// Current stack
	ArrayView<TCScope> scopeStack = tcContext->scopeStack;
	for (s64 stackIdx = scopeStack.size - 1; stackIdx >= 0; --stackIdx) {
		const TCScope *currentScope = &scopeStack[stackIdx];
		for (int i = 0; i < currentScope->names.size; ++i) {
			const TCScopeName *currentName = &currentScope->names[i];
			if (StringEquals(name, currentName->name))
				return *currentName;
		}
	}
	// Global scope
	return FindGlobalName(loc, name);
}

inline u32 NewStaticDefinition(StaticDefinition *value)
{
	auto staticDefinitions = g_context->staticDefinitions.GetForWrite();
	u64 result = staticDefinitions->count;
	*BucketArrayAdd(&staticDefinitions) = *value;
	ASSERT(result < U32_MAX);

	return (u32)result;
}

inline StaticDefinition GetStaticDefinition(u32 staticDefinitionIdx,
		bool ensureTypeChecked = false)
{
	StaticDefinition staticDefinition;

	// Optimize most frequent path: def IS ready! no locking.
	auto &staticDefinitions = g_context->staticDefinitions.unsafe;
	staticDefinition = staticDefinitions[staticDefinitionIdx];
	if (staticDefinition.definitionType != STATICDEFINITIONTYPE_NOT_READY &&
			(!ensureTypeChecked || staticDefinition.typeTableIdx != TYPETABLEIDX_Unset))
		return staticDefinition;

	ProfilerBegin("Waiting on static definition");

	// Lock this, if we need to switch fibers, we don't unlock until we added this fiber to a
	// waiting list.
	SYSLockForRead(&g_context->staticDefinitions.rwLock);

	staticDefinition = staticDefinitions[staticDefinitionIdx];
	if (staticDefinition.definitionType == STATICDEFINITIONTYPE_NOT_READY ||
			(ensureTypeChecked && staticDefinition.typeTableIdx == TYPETABLEIDX_Unset)) {
		// IMPORTANT! The scheduler will unlock staticDefinitions once it adds this job to the
		// waiting list, so we don't miss waking it up when adding the identifier.
		SwitchJob(YIELDREASON_STATIC_DEF_NOT_READY, { .index = staticDefinitionIdx });

		// Need to lock these again!
		SYSLockForRead(&g_context->staticDefinitions.rwLock);

		staticDefinition = staticDefinitions[staticDefinitionIdx];

		if (staticDefinition.definitionType == STATICDEFINITIONTYPE_NOT_READY ||
				(ensureTypeChecked && staticDefinition.typeTableIdx == TYPETABLEIDX_Unset))
			LogCompilerError({}, "Bad job resume"_s);
	}
	SYSUnlockForRead(&g_context->staticDefinitions.rwLock);
	ProfilerEnd();
	return staticDefinition;
}

inline void UpdateStaticDefinition(u32 staticDefinitionIdx,
		StaticDefinition *value)
{
	{
		auto staticDefinitions = g_context->staticDefinitions.GetForWrite();
		staticDefinitions[staticDefinitionIdx] = *value;
	}

	if (value->definitionType != STATICDEFINITIONTYPE_NOT_READY &&
		value->typeTableIdx != TYPETABLEIDX_Unset)
		// Wake up any job waiting for this static def to be ready.
		WakeUpAllByIndex(YIELDREASON_STATIC_DEF_NOT_READY, staticDefinitionIdx);
}

u32 FindTypeInStackByName(TCContext *tcContext, SourceLocation loc, String name)
{
	u32 typeTableIdx = TYPETABLEIDX_Unset;

	TCScopeName scopeName = TCFindScopeName(tcContext, loc, name);
	ASSERT(scopeName.type != NAMETYPE_INVALID);

	if (scopeName.type == NAMETYPE_PRIMITIVE)
		return scopeName.primitiveTypeTableIdx;
	else if (scopeName.type != NAMETYPE_STATIC_DEFINITION)
		LogError(loc, TPrintF("\"%S\" is not a type!", name));

	StaticDefinition staticDefinition = GetStaticDefinition(scopeName.staticDefinitionIdx, true);

	if (staticDefinition.definitionType != STATICDEFINITIONTYPE_TYPE)
		LogError(loc, TPrintF("\"%S\" is not a type!", name));

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

void ReportTypeCheckError(TypeCheckErrorCode errorCode, SourceLocation sourceLoc, u32 leftTypeIdx,
		u32 rightTypeIdx)
{
	String leftStr  = TypeInfoToString(leftTypeIdx);
	String rightStr = TypeInfoToString(rightTypeIdx);
	switch (errorCode) {
	case TYPECHECK_SIGN_MISMATCH:
		LogError(sourceLoc, TPrintF(
			"Integer sign mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_SIZE_MISMATCH:
		LogError(sourceLoc, TPrintF(
			"Integer size mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_TYPE_CATEGORY_MISMATCH:
		LogError(sourceLoc, TPrintF(
			"Expression type mismatch! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_POINTED_TYPE_MISMATCH:
		LogError(sourceLoc, TPrintF(
			"Unrelated pointed types! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_ARRAY_SIZE_MISMATCH:
		LogError(sourceLoc, TPrintF(
			"Size of arrays are different! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_STRUCT_MISMATCH:
		LogError(sourceLoc, TPrintF(
			"Expressions evaluate to different structs! (left is %S and right is %S)", leftStr, rightStr));
	case TYPECHECK_MISC_PANIC:
		LogError(sourceLoc, TPrintF(
			"Expression type mismatch! (left is %S and right is %S)", leftStr, rightStr));
	default:
		ASSERT(errorCode == TYPECHECK_COOL);
	}
}

inline u32 StripImplicitlyCastAliases(u32 typeTableIdx) {
	auto &typeTable = g_context->typeTable.unsafe;
	TypeInfo typeInfo = typeTable[typeTableIdx];
	while (typeInfo.typeCategory == TYPECATEGORY_ALIAS &&
		   typeInfo.aliasInfo.doesImplicitlyCast) {
		typeTableIdx = typeInfo.aliasInfo.aliasedTypeIdx;
		typeInfo = typeTable[typeTableIdx];
	}
	return typeTableIdx;
}

inline u32 StripAllAliases(u32 typeTableIdx) {
	auto &typeTable = g_context->typeTable.unsafe;
	TypeInfo typeInfo = typeTable[typeTableIdx];
	while (typeInfo.typeCategory == TYPECATEGORY_ALIAS) {
		typeTableIdx = typeInfo.aliasInfo.aliasedTypeIdx;
		typeInfo = typeTable[typeTableIdx];
	}
	return typeTableIdx;
}

u32 GetTypeInfoPointerOf(u32 inType);
TypeCheckErrorCode CheckTypesMatch(u32 leftTypeIdx, u32 rightTypeIdx)
{
	// Get rid of implicitly cast aliases
	leftTypeIdx  = StripImplicitlyCastAliases(leftTypeIdx);
	rightTypeIdx = StripImplicitlyCastAliases(rightTypeIdx);

	if (leftTypeIdx == TYPETABLEIDX_Anything || rightTypeIdx == TYPETABLEIDX_Anything)
		return TYPECHECK_COOL;

	if ((leftTypeIdx == TYPETABLEIDX_VOID) != (rightTypeIdx == TYPETABLEIDX_VOID))
		return TYPECHECK_TYPE_CATEGORY_MISMATCH;

	if (leftTypeIdx == rightTypeIdx)
		return TYPECHECK_COOL;

	// Allow anything to cast to Any
	if (leftTypeIdx == TYPETABLEIDX_ANY_STRUCT || rightTypeIdx == TYPETABLEIDX_ANY_STRUCT)
		return TYPECHECK_COOL;

	TypeInfo left  = GetTypeInfo(leftTypeIdx);
	TypeInfo right = GetTypeInfo(rightTypeIdx);

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
			u32 leftTypeIdxStripped = StripAllAliases(leftTypeIdx);
			TypeCategory strippedTypeCategory = GetTypeInfo(leftTypeIdxStripped).typeCategory;
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
			u32 rightTypeIdxStripped = StripAllAliases(rightTypeIdx);
			TypeCategory strippedTypeCategory =
				GetTypeInfo(rightTypeIdxStripped).typeCategory;
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
			(right.typeCategory == TYPECATEGORY_INTEGER ||
			right.typeCategory == TYPECATEGORY_FLOATING))
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
		u32 pointedTypeIdxLeft  = StripImplicitlyCastAliases(
				left.pointerInfo.pointedTypeTableIdx);
		u32 pointedTypeIdxRight = StripImplicitlyCastAliases(
				right.pointerInfo.pointedTypeTableIdx);

		// Cast any pointer to void pointer
		if (pointedTypeIdxLeft == TYPETABLEIDX_VOID)
			return TYPECHECK_COOL;

		if (pointedTypeIdxLeft == pointedTypeIdxRight)
			return TYPECHECK_COOL;

		// Allow implicit ^[T] -> ^T
		TypeInfo pointedLeft  = GetTypeInfo(pointedTypeIdxLeft);
		TypeInfo pointedRight = GetTypeInfo(pointedTypeIdxRight);
		if (pointedLeft.typeCategory == TYPECATEGORY_ARRAY &&
			pointedRight.typeCategory != TYPECATEGORY_ARRAY)
		{
			return CheckTypesMatch(pointedLeft.arrayInfo.elementTypeTableIdx,
				right.pointerInfo.pointedTypeTableIdx);
		}
		if (pointedRight.typeCategory == TYPECATEGORY_ARRAY &&
			pointedLeft.typeCategory != TYPECATEGORY_ARRAY)
		{
			return CheckTypesMatch(pointedRight.arrayInfo.elementTypeTableIdx,
			left.pointerInfo.pointedTypeTableIdx);
		}

		// Check pointed types
		TypeCheckErrorCode pointedTypesError = CheckTypesMatch(
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
			TypeCheckErrorCode errorCode = CheckTypesMatch(leftReturnTypeIdx, rightReturnTypeIdx);
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
			TypeCheckErrorCode errorCode = CheckTypesMatch(leftParamTypeIdx, rightParamTypeIdx);
			if (errorCode != TYPECHECK_COOL)
				return errorCode;
		}
		return TYPECHECK_COOL;
	} break;
	}

	return TYPECHECK_MISC_PANIC;
}

const StructMember *FindStructMemberByName(TypeInfo structTypeInfo, String name)
{
	for (int i = 0; i < structTypeInfo.structInfo.members.size; ++i) {
		const StructMember *currentMember = &structTypeInfo.structInfo.members[i];
		if (StringEquals(name, currentMember->name))
			return currentMember;
		if (currentMember->isUsing || currentMember->name.size == 0) {
			// Anonymous structs/unions and using
			TypeInfo memberTypeInfo = GetTypeInfo(currentMember->typeTableIdx);
			ASSERT(memberTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
				   memberTypeInfo.typeCategory == TYPECATEGORY_UNION);
			const StructMember *found = FindStructMemberByName(memberTypeInfo, name);
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

void InferTypesInExpression(ASTExpression *expression, u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
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
		InferTypesInExpression(expression->unaryOperation.expression, typeTableIdx);
		break;
	case ASTNODETYPE_BINARY_OPERATION:
		InferTypesInExpression(expression->binaryOperation.leftHand,  typeTableIdx);
		InferTypesInExpression(expression->binaryOperation.rightHand, typeTableIdx);
		break;
	}
}

struct TypeCheckResult
{
	TypeCheckErrorCode errorCode;
	u32 leftTypeIdx;
	u32 rightTypeIdx;
};
TypeCheckResult CheckTypesMatchAndSpecialize(u32 leftTypeIdx, ASTExpression *rightHand)
{
	u32 rightTypeIdx = rightHand->typeTableIdx;

	ASSERT(leftTypeIdx  != TYPETABLEIDX_Unset);
	ASSERT(rightTypeIdx != TYPETABLEIDX_Unset);

	// Get rid of aliases
	if (leftTypeIdx >= TYPETABLEIDX_Begin)
		leftTypeIdx  = StripImplicitlyCastAliases(leftTypeIdx);
	if (rightTypeIdx >= TYPETABLEIDX_Begin)
		rightTypeIdx = StripImplicitlyCastAliases(rightTypeIdx);

	TypeCheckResult result = { TYPECHECK_COOL, leftTypeIdx, rightTypeIdx };

	if (rightTypeIdx == TYPETABLEIDX_StructLiteral) {
		/*
		Documentation!

		Here we resolve a bag of things inside curly braces {} into struct/array literals, according
		to what types we have on the left hand side. Here are the rules the code is supposed to
		follow while doing so.

		Arrays:
		This one is easy, we interpret the literal as a comma-separated list of values. Regardless
		of the type of each element or how many nested braces there may be.

		Structs/unions:
		With structs it's a little more complicated. We do recurse into struct members inside the
		lhs struct, even if there are no explicit recursive braces on the rhs. The idea is that
		things like 3D vectors, which are often constructed as a union of structs, can still be
		represented simply like {x,y,z}.
		*/

		// Important: keep this symmetrical with the logic over at IRGen!
		ASSERT(rightHand->nodeType == ASTNODETYPE_LITERAL);
		ASSERT(rightHand->literal.type == LITERALTYPE_GROUP);

		u32 structTypeIdx = leftTypeIdx;
		TypeInfo structTypeInfo = GetTypeInfo(structTypeIdx);
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
			// Non-named members
			while (memberIdx < rightHand->literal.members.size) {
				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];
				StructStackFrame currentFrame = structStack[structStack.size - 1];
				TypeInfo currentStructTypeInfo = GetTypeInfo(currentFrame.structTypeIdx);

				if (currentFrame.idx >= currentStructTypeInfo.structInfo.members.size) {
					// Pop struct frame
					--structStack.size;
					if (structStack.size == 0)
						LogError(rightHand->any.loc, TPrintF("Too many values in struct literal, "
								"while fitting struct %S",
								TypeInfoToString(currentFrame.structTypeIdx)));
					continue;
				}

				u32 currentMemberTypeIdx =
					currentStructTypeInfo.structInfo.members[currentFrame.idx].typeTableIdx;
				TypeInfo currentMemberTypeInfo = GetTypeInfo(currentMemberTypeIdx);

				// We don't try to dive into more members when there is an explicit set of braces
				// delimiting a member. Otherwise, something like:
				// { { x, y, z }, { a, b }, { r, g, b } }
				// gets interpreted as a flat string of values, which can be incorrect:
				// { x, y, z, a, b, r, g, b }
				bool isAnotherASTGroupLiteral = literalMemberExp->nodeType == ASTNODETYPE_LITERAL &&
					literalMemberExp->literal.type == LITERALTYPE_GROUP;

				if (!isAnotherASTGroupLiteral &&
					(currentMemberTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
					 currentMemberTypeInfo.typeCategory == TYPECATEGORY_UNION)) {
					// Push struct frame
					structStack[structStack.size++] = { currentMemberTypeIdx, 0 };
					continue;
				}

				if (literalMemberExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
					// Named member assignments handled in next loop
					break;
				else {
					TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(
							currentMemberTypeIdx, literalMemberExp);
					literalMemberExp->typeTableIdx = typeCheckResult.rightTypeIdx;
					if (typeCheckResult.errorCode != TYPECHECK_COOL) {
						Print("Type of struct literal value in position %d and "
								"type of struct member number %d don't match\n", memberIdx, memberIdx);
						ReportTypeCheckError(typeCheckResult.errorCode, rightHand->any.loc,
								currentMemberTypeIdx, literalMemberExp->typeTableIdx);
					}
					++structStack[structStack.size - 1].idx;
				}
				++memberIdx;
			}

			// Named members
			for (; memberIdx < rightHand->literal.members.size; ++memberIdx) {
				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];
				if (literalMemberExp->nodeType != ASTNODETYPE_BINARY_OPERATION ||
					literalMemberExp->binaryOperation.op != TOKEN_OP_ASSIGNMENT)
					LogError(literalMemberExp->any.loc, "Non-named member found after "
							"named members in group literal"_s);

				ASTExpression *leftExp  = literalMemberExp->binaryOperation.leftHand;
				ASTExpression *rightExp = literalMemberExp->binaryOperation.rightHand;

				ASSERT(leftExp->nodeType == ASTNODETYPE_IDENTIFIER); // We check this earlier.
				String memberName = leftExp->identifier.string;

				const StructMember *member = FindStructMemberByName(structTypeInfo, memberName);
				leftExp->identifier.structMember = member;

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(
						member->typeTableIdx, rightExp);
				rightExp->typeTableIdx = typeCheckResult.rightTypeIdx;
				if (typeCheckResult.errorCode != TYPECHECK_COOL) {
					Print("Type of struct literal value \"%S\" and "
							"type of struct member don't match\n", memberName);
					ReportTypeCheckError(typeCheckResult.errorCode, rightHand->any.loc,
							member->typeTableIdx, rightExp->typeTableIdx);
				}
			}

			result.rightTypeIdx = structTypeIdx;
		}
		else if (structTypeInfo.typeCategory == TYPECATEGORY_ARRAY) {
			if (structTypeInfo.arrayInfo.count < rightHand->literal.members.size)
				LogError(rightHand->any.loc, TPrintF("Too many values in array literal: %d on "
						"left side, %d on right side.", structTypeInfo.arrayInfo.count,
						rightHand->literal.members.size));

			for (int memberIdx = 0; memberIdx < rightHand->literal.members.size; ++memberIdx) {
				ASTExpression *literalMemberExp = rightHand->literal.members[memberIdx];

				if (literalMemberExp->nodeType == ASTNODETYPE_IDENTIFIER &&
					literalMemberExp->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
					LogError(literalMemberExp->any.loc, "Named members not allowed in array literals"_s);

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(
						structTypeInfo.arrayInfo.elementTypeTableIdx,
						literalMemberExp);
				literalMemberExp->typeTableIdx = typeCheckResult.rightTypeIdx;
				if (typeCheckResult.errorCode != TYPECHECK_COOL) {
					Print("Type of element %d in array literal doesn't match with type of array", memberIdx);
					ReportTypeCheckError(typeCheckResult.errorCode, rightHand->any.loc,
							structTypeInfo.arrayInfo.elementTypeTableIdx,
							rightHand->literal.members[memberIdx]->typeTableIdx);
				}
			}

			result.rightTypeIdx = structTypeIdx;
		}
		else
			LogCompilerError(rightHand->any.loc, TPrintF("Group literal found but left hand "
					"side was neither struct or array: left is \"%S\"",
					TypeInfoToString(structTypeIdx)));
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
		u32 strippedLeftTypeIdx  = StripAllAliases(leftTypeIdx);
		u32 strippedRightTypeIdx = StripAllAliases(rightTypeIdx);
		TypeCategory strippedLeftTypeCat  = GetTypeInfo(strippedLeftTypeIdx).typeCategory;
		TypeCategory strippedRightTypeCat = GetTypeInfo(strippedRightTypeIdx).typeCategory;

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
			InferTypesInExpression(rightHand, leftTypeIdx);
		}

		else if (leftTypeIdx == TYPETABLEIDX_FLOATING && strippedRightTypeCat == TYPECATEGORY_FLOATING)
			// FLOATING converts to whatever is on the right as long as it's a floating type
			result.leftTypeIdx = rightTypeIdx;
		else if (rightTypeIdx == TYPETABLEIDX_FLOATING && strippedLeftTypeCat == TYPECATEGORY_FLOATING) {
			// FLOATING converts to whatever is on the left as long as it's a floating type
			result.rightTypeIdx = leftTypeIdx;
			InferTypesInExpression(rightHand, leftTypeIdx);
		}

		else
			// Normal type checking
			result.errorCode = CheckTypesMatch(leftTypeIdx, rightTypeIdx);
	}

	return result;
}

bool AreTypeInfosEqual(TypeInfo a, TypeInfo b)
{
	if (a.typeCategory != b.typeCategory)
		return false;

	if (a.size != b.size)
		return false;

	switch (a.typeCategory) {
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
				g_context->typeTable.unsafe[a.procedureInfo.returnTypeIndices[i]];
			TypeInfo bReturnTypeInfo =
				g_context->typeTable.unsafe[b.procedureInfo.returnTypeIndices[i]];
			if (!AreTypeInfosEqual(aReturnTypeInfo, bReturnTypeInfo))
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
				TypeInfo aParamTypeInfo = g_context->typeTable.unsafe[aParam.typeTableIdx];
				TypeInfo bParamTypeInfo = g_context->typeTable.unsafe[bParam.typeTableIdx];
				if (!AreTypeInfosEqual(aParamTypeInfo, bParamTypeInfo))
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

void *AllocateStaticData(u32 valueIdx, u64 size, int alignment)
{
	ASSERT(valueIdx & VALUE_GLOBAL_BIT);

	u8 *result;
	{
		ScopedLockSpin lock(&g_context->staticDataLock);

		result = STATIC_DATA_VIRTUAL_ADDRESS + g_context->staticDataSize;
		// Align
		ASSERT(IsPowerOf2(alignment));
		int alignmentMask = alignment - 1;
		if ((u64)result & alignmentMask) {
			result = (u8 *)(((u64)result + alignment) & (~alignmentMask));
		}

		u8 *end = result + size;
		while (end > STATIC_DATA_VIRTUAL_ADDRESS + g_context->staticDataAllocatedSpace) {
			// Allocate more memory
			void *newMem = SYSCommitMemory(STATIC_DATA_VIRTUAL_ADDRESS + g_context->staticDataAllocatedSpace,
					0x100000);
			memset(newMem, 0xCC, 0x100000);
			g_context->staticDataAllocatedSpace += 0x100000;
		}
		g_context->staticDataSize = result + size - STATIC_DATA_VIRTUAL_ADDRESS;
	}

	if (valueIdx != U32_MAX) {
		{
			// @Improve: same lock for both of these?
			ScopedLockSpin lock(&g_context->globalValuesLock);
			*HashMapGetOrAdd(&g_context->globalValueContents, valueIdx & VALUE_GLOBAL_MASK) = result;
		}

		// Wake up any jobs that were waiting for this global value to be allocated
		WakeUpAllByIndex(YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED, valueIdx);
	}

	return result;
}

inline void AddStaticDataPointerToRelocate(void *ptr)
{
	ASSERT(ptr >= STATIC_DATA_VIRTUAL_ADDRESS &&
		   ptr <  STATIC_DATA_VIRTUAL_ADDRESS_END);

	ASSERT(*(void **)ptr == nullptr || (
		   *(void **)ptr >= STATIC_DATA_VIRTUAL_ADDRESS &&
		   *(void **)ptr <  STATIC_DATA_VIRTUAL_ADDRESS_END));

	ScopedLockSpin lock(&g_context->staticDataLock);
	*DynamicArrayAdd(&g_context->staticDataPointersToRelocate) = ptr;
}

bool DoesTypeContainPointers(u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
	if (typeInfo.typeCategory == TYPECATEGORY_POINTER)
		return true;
	else if (typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			 typeInfo.typeCategory == TYPECATEGORY_UNION) {
		u64 memberCount = typeInfo.structInfo.members.size;
		for (int i = 0; i < memberCount; ++i) {
			// Recurse on each member
			StructMember *member = &typeInfo.structInfo.members[i];
			if (DoesTypeContainPointers(member->typeTableIdx))
				return true;
		}
	}
	return false;
}

void AddStaticDataPointersToRelocateInType(void *ptr, u32 typeTableIdx)
{
	typeTableIdx = StripAllAliases(typeTableIdx);
	TypeInfo typeInfo = GetTypeInfo(typeTableIdx);
	ASSERT(typeInfo.typeCategory != TYPECATEGORY_NOT_READY);

	// We don't allow unions that can have pointers inside since we wouldn't be certain what parts
	// are pointers most of the time.
	// @Todo: actual error message
	if (typeInfo.typeCategory == TYPECATEGORY_UNION) {
		u64 memberCount = typeInfo.structInfo.members.size;
		for (int i = 0; i < memberCount; ++i) {
			StructMember *member = &typeInfo.structInfo.members[i];
			if (DoesTypeContainPointers(member->typeTableIdx))
				ASSERT(!"Unions that have pointers somewhere inside are not allowed");
		}
	}

	if (typeInfo.typeCategory == TYPECATEGORY_POINTER)
		AddStaticDataPointerToRelocate(ptr);
	else if (typeInfo.typeCategory == TYPECATEGORY_STRUCT) {
		u64 memberCount = typeInfo.structInfo.members.size;
		for (int i = 0; i < memberCount; ++i) {
			// Recurse on each member
			StructMember *member = &typeInfo.structInfo.members[i];
			AddStaticDataPointersToRelocateInType((u8 *)ptr + member->offset,
					member->typeTableIdx);
		}
	}
}

inline String CopyStringToStaticData(String string, bool nullTerminate = false)
{
	char *nameCopy = (char *)AllocateStaticData(U32_MAX, string.size + nullTerminate, 1);
	memcpy(nameCopy, string.data, string.size);
	if (nullTerminate)
		nameCopy[string.size] = 0;
	return { string.size, nameCopy };
}

UserFacingTypeInfo *GetUserFacingTypeInfoPointer(u32 typeTableIdx)
{
	UserFacingTypeInfo *result;

	// @Unsafe
	auto &typeTable = g_context->typeTable.unsafe;
	u32 typeInfoValueIdx = typeTable[typeTableIdx].valueIdx;

	SpinlockLock(&g_context->globalValuesLock);
	void **mapValue = HashMapGet(g_context->globalValueContents, typeInfoValueIdx & VALUE_GLOBAL_MASK);
	if (!mapValue) {
		SwitchJob(YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED, { .index = typeInfoValueIdx });
		// Lock again!
		SpinlockLock(&g_context->globalValuesLock);
		mapValue = HashMapGet(g_context->globalValueContents, typeInfoValueIdx & VALUE_GLOBAL_MASK);
		if (!mapValue)
			LogCompilerError({}, "Bad job resume"_s);
	}
	SpinlockUnlock(&g_context->globalValuesLock);

	result = (UserFacingTypeInfo *)*mapValue;
	return result;
}

void WriteUserFacingTypeInfoToStaticData(TypeInfo typeInfo)
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

	SpinlockLock(&g_context->globalValuesLock);
	void **existing = HashMapGet(g_context->globalValueContents, typeInfo.valueIdx & VALUE_GLOBAL_MASK);
	SpinlockUnlock(&g_context->globalValuesLock);
	if (existing)
		out = (UserFacingTypeInfo *)*existing;
	else
		out = (UserFacingTypeInfo *)AllocateStaticData(typeInfo.valueIdx, size, 8);

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
		members.data = (UserFacingStructMember *)AllocateStaticData(U32_MAX, memberCount *
				sizeof(UserFacingStructMember), 8);
		for (int i = 0; i < memberCount; ++i) {
			StructMember *origMember = &typeInfo.structInfo.members[i];
			UserFacingTypeInfo *memberUfti = GetUserFacingTypeInfoPointer(origMember->typeTableIdx);
			UserFacingStructMember member = {
				.name = CopyStringToStaticData(origMember->name),
				.typeInfo = memberUfti,
				.offset = origMember->offset
			};
			members[i] = member;

			AddStaticDataPointerToRelocate(&members[i].name.data);
			AddStaticDataPointerToRelocate(&members[i].typeInfo);
		}

		outStruct->name = CopyStringToStaticData(typeInfo.structInfo.name);
		outStruct->isUnion = typeInfo.typeCategory == TYPECATEGORY_UNION;
		outStruct->memberCount = members.size;
		outStruct->members = members.data;

		AddStaticDataPointerToRelocate(&outStruct->name.data);
		AddStaticDataPointerToRelocate(&outStruct->members);
	} break;
	case TYPECATEGORY_ENUM:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_ENUM;
		UserFacingTypeInfoEnum *outEnum = (UserFacingTypeInfoEnum *)out;
		outEnum->name = typeInfo.enumInfo.name;
		UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(
				typeInfo.enumInfo.typeTableIdx);
		outEnum->typeInfo = ufti;

		u64 memberCount = typeInfo.enumInfo.names.size;
		outEnum->nameCount = memberCount;
		outEnum->valueCount = memberCount;

		if (memberCount) {
			outEnum->names = (String *)AllocateStaticData(U32_MAX, memberCount * sizeof(String), 8);
			outEnum->values = (s64 *)AllocateStaticData(U32_MAX, memberCount * sizeof(s64), 8);
		}
		else {
			outEnum->names = nullptr;
			outEnum->values = nullptr;
		}

		for (int i = 0; i < memberCount; ++i) {
			outEnum->names[i]  = CopyStringToStaticData(typeInfo.enumInfo.names[i]);
			outEnum->values[i] = typeInfo.enumInfo.values[i];

			AddStaticDataPointerToRelocate(&outEnum->names[i].data);
		}

		AddStaticDataPointerToRelocate(&outEnum->typeInfo);
		AddStaticDataPointerToRelocate(&outEnum->names);
		AddStaticDataPointerToRelocate(&outEnum->values);
	} break;
	case TYPECATEGORY_POINTER:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_POINTER;
		UserFacingTypeInfoPointer *outPointer = (UserFacingTypeInfoPointer *)out;
		u32 pointedTypeIdx = typeInfo.pointerInfo.pointedTypeTableIdx;
		if (pointedTypeIdx == TYPETABLEIDX_VOID)
			outPointer->typeInfo = nullptr;
		else {
			UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(pointedTypeIdx);
			outPointer->typeInfo = ufti;
		}

		AddStaticDataPointerToRelocate(&outPointer->typeInfo);
	} break;
	case TYPECATEGORY_ARRAY:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_ARRAY;
		UserFacingTypeInfoArray *outArray = (UserFacingTypeInfoArray *)out;
		outArray->count = typeInfo.arrayInfo.count;
		UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(
				typeInfo.arrayInfo.elementTypeTableIdx);
		outArray->elementTypeInfo = ufti;

		AddStaticDataPointerToRelocate(&outArray->elementTypeInfo);
	} break;
	case TYPECATEGORY_PROCEDURE:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_PROCEDURE;
		UserFacingTypeInfoProcedure *outProcedure = (UserFacingTypeInfoProcedure *)out;
		u64 paramCount = typeInfo.procedureInfo.parameters.size;
		ArrayView<UserFacingTypeInfo *> parameters;
		parameters.size = paramCount;
		parameters.data = (UserFacingTypeInfo **)AllocateStaticData(U32_MAX, paramCount *
				sizeof(UserFacingTypeInfo *), 8);
		for (int i = 0; i < paramCount; ++i) {
			ProcedureParameter *origParam = &typeInfo.procedureInfo.parameters[i];
			UserFacingTypeInfo *paramUfti = GetUserFacingTypeInfoPointer(
					origParam->typeTableIdx);
			parameters[i] = paramUfti;

			AddStaticDataPointerToRelocate(&parameters[i]);
		}
		outProcedure->parameterCount = parameters.size;
		outProcedure->parameters = parameters.data;
		outProcedure->isVarargs = typeInfo.procedureInfo.isVarargs;

		AddStaticDataPointerToRelocate(&outProcedure->parameters);
	} break;
	case TYPECATEGORY_ALIAS:
	{
		out->typeCategory = USERFACINGTYPECATEGORY_ALIAS;
		UserFacingTypeInfoAlias *outAlias = (UserFacingTypeInfoAlias *)out;
		UserFacingTypeInfo *ufti = GetUserFacingTypeInfoPointer(
				typeInfo.aliasInfo.aliasedTypeIdx);
		outAlias->typeInfo = ufti;

		AddStaticDataPointerToRelocate(&outAlias->typeInfo);
	} break;
	}
}

inline u32 AddType(TypeInfo typeInfo)
{
	// Should lock type table before calling
	ASSERT(g_context->typeTable.lock != 0);
	typeInfo.valueIdx = NewGlobalValue(String{}, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);

	u32 typeTableIdx;
	{
		auto *typeTable = &g_context->typeTable.unsafe;

		s64 typeCount = typeTable->count;
		ASSERT(typeCount < U32_MAX); // Out of type memory
		typeTableIdx = (u32)typeCount;

		*(TypeInfo *)BucketArrayAdd(typeTable) = typeInfo;
	}
	SpinlockUnlock(&g_context->typeTable.lock);

	{
		Value value = GetGlobalValue(typeInfo.valueIdx);
#if DEBUG_BUILD
		value.name = SNPrintF(16, "_typeInfo%lld", typeTableIdx);
#endif
		UpdateGlobalValue(typeInfo.valueIdx, &value);
	}

	// Set up static data for user-facing type info
	WriteUserFacingTypeInfoToStaticData(typeInfo);

	return typeTableIdx;
}

u32 FindOrAddTypeTableIdx(TypeInfo typeInfo, u32 checkAfterIdx = TYPETABLEIDX_Begin)
{
	u32 alreadyChecked;
	{
		// Optimize most frequent path. Read without locking.
		auto &typeTable = g_context->typeTable.unsafe;

		u32 tableSize = (u32)typeTable.count;
		for (u32 i = 0; i < tableSize; ++i) {
			TypeInfo t = typeTable[i];
			if (AreTypeInfosEqual(typeInfo, t))
				return i;
		}
		alreadyChecked = tableSize;
	}
	{
		// Check it didn't get added when we released the lock
		// @Speed: ugh...
		SpinlockLock(&g_context->typeTable.lock);
		auto &typeTable = g_context->typeTable.unsafe;

		u32 tableSize = (u32)typeTable.count;
		for (u32 i = checkAfterIdx + 1; i < tableSize; ++i) {
			TypeInfo t = typeTable[i];
			if (AreTypeInfosEqual(typeInfo, t)) {
				SpinlockUnlock(&g_context->typeTable.lock);
				return i;
			}
		}
		return AddType(typeInfo);
	}
}

// Util TypeInfo procedures
u32 GetTypeInfoPointerOf(u32 inType)
{
	ASSERT(inType < (u32)g_context->typeTable.unsafe.count);

	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_POINTER;
	resultTypeInfo.size = g_pointerSize;
	resultTypeInfo.pointerInfo.pointedTypeTableIdx = inType;
	u32 result = FindOrAddTypeTableIdx(resultTypeInfo, inType);
	//Print("Pointer to %S found at distance %d (%u)\n", TypeInfoToString(inType), result - inType, result);
	return result;
}

u32 GetTypeInfoArrayOf(u32 inType, s64 count)
{
	TypeInfo resultTypeInfo = {};
	resultTypeInfo.typeCategory = TYPECATEGORY_ARRAY;
	resultTypeInfo.arrayInfo.elementTypeTableIdx = inType;
	resultTypeInfo.arrayInfo.count = count;
	if (count == 0)
		resultTypeInfo.size = 8 + g_pointerSize;
	else if (inType > TYPETABLEIDX_Begin) {
		s64 elementSize = GetTypeInfo(inType).size;
		resultTypeInfo.size = elementSize * count;
	}
	return FindOrAddTypeTableIdx(resultTypeInfo, inType);
}

u32 TypeCheckType(TCContext *tcContext, String name, SourceLocation loc, ASTType *astType);

void TCAddScopeNames(TCContext *tcContext, ArrayView<TCScopeName> scopeNames)
{
	// Primitives
	for (int i = 0; i < g_context->tcPrimitiveTypes.size; ++i) {
		const TCScopeName *currentName = &g_context->tcPrimitiveTypes[i];
		ASSERT(currentName->type == NAMETYPE_PRIMITIVE);
		for (int j = 0; j < scopeNames.size; ++j) {
			if (StringEquals(scopeNames[j].name, currentName->name))
				LogError(scopeNames[j].loc, TPrintF("Can not use name \"%S\", it is a "
						"language primitive", scopeNames[j].name));
		}
	}

	TCScope *stackTop = TCGetTopMostScope(tcContext);
	if (stackTop) {
		// Check if already exists
		for (int i = 0; i < stackTop->names.size; ++i) {
			TCScopeName *currentName = &stackTop->names[i];
			for (int j = 0; j < scopeNames.size; ++j) {
				if (StringEquals(scopeNames[j].name, currentName->name)) {
					LogErrorNoCrash(scopeNames[j].loc, TPrintF("Name \"%S\" already assigned",
								scopeNames[j].name));
					LogNote(currentName->loc, "First defined here"_s);
					PANIC;
				}
			}
		}

		TCScopeName *newNames = DynamicArrayAddMany(&stackTop->names, scopeNames.size);
		for (int i = 0; i < scopeNames.size; ++i)
			newNames[i] = scopeNames[i];
	}
	else for (int i = 0; i < scopeNames.size; ++i) {
		while (!MTQueueEnqueue(&g_context->tcGlobalNamesToAdd, scopeNames[i])) {
			// If queue is full try to commit some names.
			if (SpinlockTryLock(&g_context->tcGlobalNamesCommitLock)) {
				TCCommitGlobalNames();
				SpinlockUnlock(&g_context->tcGlobalNamesCommitLock);
			}
		}
	}
}

void TCCommitGlobalNames()
{
	// @Speed: optimize
	u64 firstAddedIdx;
	u64 globalNamesCount;
	{
		auto globalNames = g_context->tcGlobalNames.GetForWrite();

		firstAddedIdx = globalNames->count;
		globalNamesCount = firstAddedIdx;

		TCScopeName dequeue;
		while (MTQueueDequeue(&g_context->tcGlobalNamesToAdd, &dequeue)) {
			*BucketArrayAdd(&globalNames) = dequeue;
			++globalNamesCount;
		}
	}

	// No need to lock because we'll just read names added up to this point, and names never
	// change.
	auto &globalNames = g_context->tcGlobalNames.unsafe;

	// Wake up any jobs that were waiting for these names
	auto jobsWaiting = g_context->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		u32 jobIdx = jobsWaiting[i];
		const Job *job = &g_context->jobs.unsafe[jobIdx];
		for (u64 nameIdx = firstAddedIdx; nameIdx < globalNamesCount; ++nameIdx) {
			if (StringEquals(job->yieldContext.identifier, globalNames[nameIdx].name)) {
				EnqueueReadyJob(jobIdx);
				// Remove waiting job
				DynamicArraySwapRemove(&jobsWaiting, i);
				goto outerContinue;
			}
		}
		++i; // Didn't remove waiting job
outerContinue:
		continue;
	}

	// Check if any was added twice
	for (u64 allNameIdx = 0; allNameIdx < globalNamesCount; ++allNameIdx) {
		const TCScopeName *allName = &globalNames[allNameIdx];
		// Iterate over the new names only. We assume all the old ones are unique.
		// Notice we don't assume we didn't just add the same name twice, new names still compare
		// against each other.
		// @Speed: avoid checking pairs of new names twice (a==b and b==a).
		for (u64 addedNameIdx = firstAddedIdx; addedNameIdx < globalNamesCount; ++addedNameIdx) {
			if (addedNameIdx == allNameIdx) continue;
			const TCScopeName *addedName = &globalNames[addedNameIdx];
			if (StringEquals(addedName->name, allName->name)) {
				LogErrorNoCrash(addedName->loc, TPrintF("Name \"%S\" already "
							"assigned", addedName->name));
				LogNote(allName->loc, "First defined here"_s);
				PANIC;
			}
		}
	}
}

void TypeCheckExpression(TCContext *tcContext, ASTExpression *expression);
Constant TryEvaluateConstant(ASTExpression *expression);
u32 TypeCheckStructDeclaration(TCContext *tcContext, String name, bool isUnion,
		ASTStructDeclaration astStructDecl)
{
	u8 alignment = 0;
	if (astStructDecl.alignExp) {
		TypeCheckExpression(tcContext, astStructDecl.alignExp);
		Constant constant = TryEvaluateConstant(astStructDecl.alignExp);
		if (constant.type == CONSTANTTYPE_INVALID)
			LogError(astStructDecl.alignExp->any.loc, "Could not evaluate alignment"_s);
		if (constant.type != CONSTANTTYPE_INTEGER)
			LogError(astStructDecl.alignExp->any.loc, "Alignment expression did not "
					"evaluate to an integer"_s);
		if (constant.valueAsInt > U8_MAX)
			LogError(astStructDecl.alignExp->any.loc, "Alignment specified is too large"_s);
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
		SpinlockLock(&g_context->typeTable.lock);
		ASSERT(g_context->typeTable.unsafe.count < U32_MAX);
		TypeInfo t = {
			.typeCategory = TYPECATEGORY_NOT_READY,
			.alignment = alignment,
			.structInfo = { .name = name }
		};
		typeTableIdx = AddType(t);

		// Allocate static data but don't fill it
		// Note that AddType() didn't allocate or fill this because the type category was NOT_READY.
		AllocateStaticData(g_context->typeTable.unsafe[typeTableIdx].valueIdx,
				sizeof(UserFacingTypeInfoStruct), 8);

		// Declare pointer to struct type so it is close to struct type
		TypeInfo tp = {
			.typeCategory = TYPECATEGORY_POINTER,
			.size = g_pointerSize,
			.pointerInfo = { .pointedTypeTableIdx = typeTableIdx }
		};
		SpinlockLock(&g_context->typeTable.lock);
		AddType(tp);
	}

	TCScope *stackTop = TCGetTopMostScope(tcContext);
	if (stackTop)
		*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;
	else {
		auto globalTypes = g_context->tcGlobalTypeIndices.GetForWrite();
		*DynamicArrayAdd(&globalTypes) = typeTableIdx;
	}

	// Finish type checking struct members on another fiber. This takes care of circular
	// dependencies where two structs each have a member whose type is of the other struct.
	TCStructJobArgs *args = ALLOC(LinearAllocator, TCStructJobArgs);
	*args = {
		.typeTableIdx = typeTableIdx,
		.astStructDecl = astStructDecl,
		.name = name,
		.isUnion = isUnion
	};
	RequestNewJob(JOBTYPE_TYPE_CHECK, TCStructJobProc, (void *)args);

	return typeTableIdx;
}

Constant ConstantFromCTRegister(CTRegister ctRegister, u32 typeTableIdx)
{
	Constant result;
	result.typeTableIdx = typeTableIdx;
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

Constant TryEvaluateConstant(ASTExpression *expression)
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
				constants[i] = TryEvaluateConstant(expression->literal.members[i]);
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
			result = GetStaticDefinition(
					expression->identifier.staticDefinitionIdx, false).constant;
		}
		else
			goto error;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTExpression *in = expression->unaryOperation.expression;
		Constant inValue  = TryEvaluateConstant(in);
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
		Constant leftValue  = TryEvaluateConstant(leftHand);
		Constant rightValue = TryEvaluateConstant(rightHand);
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
		Constant constant = TryEvaluateConstant(expression->castNode.expression);
		bool isFloat = constant.type == CONSTANTTYPE_FLOATING;
		bool castToFloat = GetTypeInfo(expression->typeTableIdx).typeCategory ==
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

bool TypeCheckProcedurePrototype(TCContext *tcContext, ASTProcedurePrototype *prototype);
TypeInfo TypeInfoFromASTProcedurePrototype(ASTProcedurePrototype *prototype);
u32 TypeCheckType(TCContext *tcContext, String name, SourceLocation loc, ASTType *astType)
{
	switch (astType->nodeType) {
	case ASTTYPENODETYPE_IDENTIFIER:
	case ASTTYPENODETYPE_TYPE_ARGUMENT_DECLARATION:
	{
		return FindTypeInStackByName(tcContext, astType->loc, astType->name);
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		u32 elementTypeIdx = TypeCheckType(tcContext, {}, loc, astType->arrayType);
		s64 arrayCount = 0;
		if (astType->arrayCountExp) {
			// @Hack: we type check this multiple times sometimes when instantiating polymorphic
			// procedures.
			astType->arrayCountExp->typeTableIdx = TYPETABLEIDX_Unset;
			TypeCheckExpression(tcContext, astType->arrayCountExp);

			Constant constant = TryEvaluateConstant(astType->arrayCountExp);
			if (constant.type == CONSTANTTYPE_INVALID)
				LogError(loc, "Could not evaluate array count"_s);
			if (constant.type != CONSTANTTYPE_INTEGER)
				LogError(loc, "Array count did not evaluate to an integer"_s);
			arrayCount = constant.valueAsInt;
		}
		return GetTypeInfoArrayOf(elementTypeIdx, arrayCount);
	} break;
	case ASTTYPENODETYPE_POINTER:
	{
		u32 pointedTypeIdx = TypeCheckType(tcContext, {}, loc, astType->pointedType);
		return GetTypeInfoPointerOf(pointedTypeIdx);
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	{
		return TypeCheckStructDeclaration(tcContext, name, false, astType->structDeclaration);
	} break;
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		return TypeCheckStructDeclaration(tcContext, name, true, astType->structDeclaration);
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		u32 innerTypeIdx = TYPETABLEIDX_S64;
		if (astType->enumDeclaration.astType)
		{
			SourceLocation astTypeLoc = astType->enumDeclaration.astType->loc;
			innerTypeIdx = TypeCheckType(tcContext, {}, astTypeLoc,
					astType->enumDeclaration.astType);

			u32 strippedInnerTypeIdx = StripImplicitlyCastAliases(innerTypeIdx);
			if (strippedInnerTypeIdx < TYPETABLEIDX_PrimitiveBegin ||
				strippedInnerTypeIdx > TYPETABLEIDX_PrimitiveEnd)
				LogError(astTypeLoc, "Only primitive types are allowed as enum field types"_s);
		}

		TypeInfo t = {};
		t.typeCategory = TYPECATEGORY_ENUM;
		t.enumInfo.name = name;
		t.enumInfo.typeTableIdx = innerTypeIdx;
		t.size = GetTypeInfo(innerTypeIdx).size;
		u32 typeTableIdx;
		{
			SpinlockLock(&g_context->typeTable.lock);
			typeTableIdx = AddType(t);
		}

		u64 memberCount = astType->enumDeclaration.members.size;

		Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
		ArrayInit(&scopeNamesToAdd, memberCount);

		ArrayInit(&t.enumInfo.names,  memberCount);
		ArrayInit(&t.enumInfo.values, memberCount);

		Array<u32, LinearAllocator> valueStaticDefs;
		ArrayInit(&valueStaticDefs, memberCount);

		TCPushScope(tcContext);

		s64 currentValue = 0;
		for (int memberIdx = 0; memberIdx < memberCount; ++memberIdx) {
			ASTEnumMember astMember = astType->enumDeclaration.members[memberIdx];

			ASTExpression *memberValue = astMember.value;
			if (memberValue)
				TypeCheckExpression(tcContext, memberValue);

			StaticDefinition staticDefinition = {};
			staticDefinition.name = astMember.name;
			staticDefinition.definitionType = STATICDEFINITIONTYPE_CONSTANT;
			staticDefinition.typeTableIdx = typeTableIdx;

			if (astMember.value) {
				Constant constant = TryEvaluateConstant(astMember.value);
				if (constant.type == CONSTANTTYPE_INVALID)
					LogError(astMember.value->any.loc,
							"Failed to evaluate constant in static definition"_s);
				else if (constant.type == CONSTANTTYPE_FLOATING)
					currentValue = (s64)constant.valueAsFloat;
				else
					currentValue = constant.valueAsInt;
			}
			staticDefinition.constant.type = CONSTANTTYPE_INTEGER;
			staticDefinition.constant.valueAsInt = currentValue;
			staticDefinition.constant.typeTableIdx = typeTableIdx;

			u32 newStaticDefIdx = NewStaticDefinition(&staticDefinition);
			*ArrayAdd(&valueStaticDefs) = newStaticDefIdx;

			// Add scope names one by one since every one could depend on any previous one.
			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_STATIC_DEFINITION;
			newScopeName.name = astMember.name;
			newScopeName.staticDefinitionIdx = newStaticDefIdx;
			newScopeName.loc = astMember.loc;
			// Add to the local scope we created above!
			TCAddScopeNames(tcContext, { &newScopeName, 1 });
			*ArrayAdd(&scopeNamesToAdd) = newScopeName;

			*ArrayAdd(&t.enumInfo.names)  = astMember.name;
			*ArrayAdd(&t.enumInfo.values) = currentValue;
			++currentValue;
		}

		TCPopScope(tcContext);
		// Add to outer scope
		TCAddScopeNames(tcContext, scopeNamesToAdd);

		// Update type info
		{
			// Unsafe!
			auto &typeTable = g_context->typeTable.unsafe;
			t.valueIdx = typeTable[typeTableIdx].valueIdx;
			(TypeInfo&)typeTable[typeTableIdx] = t;

			WriteUserFacingTypeInfoToStaticData(t);
		}

		TCScope *stackTop = TCGetTopMostScope(tcContext);
		if (stackTop)
			*DynamicArrayAdd(&stackTop->typeIndices) = typeTableIdx;
		else {
			auto globalTypes = g_context->tcGlobalTypeIndices.GetForWrite();
			*DynamicArrayAdd(&globalTypes) = typeTableIdx;
		}

		return typeTableIdx;
	} break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		TypeCheckProcedurePrototype(tcContext, &astType->procedurePrototype);

		TypeInfo t = TypeInfoFromASTProcedurePrototype(&astType->procedurePrototype);
		u32 typeTableIdx = FindOrAddTypeTableIdx(t);

		return typeTableIdx;
	} break;
	// @Delete
#if 0
	case ASTTYPENODETYPE_TYPE_ARGUMENT_DECLARATION:
	{
		LogError(loc, "Unexpected polymorphic type. Polymorphic types are only allowed on "
				"procedure parameter declarations."_s);
	} break;
#endif
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

void TCAddStructMembersToScope(TCContext *tcContext, SourceLocation loc, ASTExpression *valueExpression)
{
	// This procedure spills all members of a struct expression into the current stack.
	// The way we do this is, we add a TCScopeName which has a member access AST node, left hand of
	// which is an arbitrary AST tree branch that evaluates to a struct; and of which right hand is
	// a simple struct member node.
	TypeInfo typeInfo = GetTypeInfo(valueExpression->typeTableIdx);
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
			TCAddStructMembersToScope(tcContext, loc, memberAccessExp);
		}
	}
	TCAddScopeNames(tcContext, scopeNamesToAdd);
}

bool IsExpressionAType(ASTExpression *expression)
{
	switch (expression->nodeType) {
	case ASTNODETYPE_TYPE:
		return true;
	case ASTNODETYPE_IDENTIFIER:
		switch (expression->identifier.type) {
		case NAMETYPE_STATIC_DEFINITION:
		{
			u32 defIdx = expression->identifier.staticDefinitionIdx;
			StaticDefinition staticDef = GetStaticDefinition(defIdx, false);
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

void TypeCheckVariableDeclaration(TCContext *tcContext, ASTVariableDeclaration *varDecl)
{
	bool isGlobal = varDecl->isStatic || varDecl->isExternal;

	if (tcContext->onStaticContext && !isGlobal)
		LogError(varDecl->loc, "Variable on static scope has to be declared either "
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
		varDecl->specifiedTypeIdx = TypeCheckType(tcContext, {}, varDecl->loc, varDecl->astType);
		ASSERT(varDecl->specifiedTypeIdx != TYPETABLEIDX_Unset);
		if (varDecl->specifiedTypeIdx == TYPETABLEIDX_VOID)
			LogError(varDecl->loc, "Variable can't be of type void"_s);
		for (int varIdx = 0; varIdx < varCount; ++varIdx)
			*GetVariableTypeIdx(varDecl, varIdx) = varDecl->specifiedTypeIdx;
	}

	if (varDecl->astInitialValue) {
		TypeCheckExpression(tcContext, varDecl->astInitialValue);

		if (IsExpressionAType(varDecl->astInitialValue))
			LogError(varDecl->astInitialValue->any.loc, "Initial value of variable is a "
					"type"_s);

		ASTExpression *astInitialValue = varDecl->astInitialValue;

		if (varDecl->astType) {
			ASSERT(varDecl->specifiedTypeIdx != TYPETABLEIDX_Unset);
			if (astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(astInitialValue->any.loc, "Trying to initialize a variable with "
						"a void expression"_s);

			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(
					varDecl->specifiedTypeIdx, astInitialValue);
			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				Print("Variable declaration type and initial type don't match\n");
				ReportTypeCheckError(typeCheckResult.errorCode, varDecl->loc,
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
			TypeInfoProcedure procTypeInfo = GetTypeInfo(procTypeIdx).procedureInfo;
			if (varCount < procTypeInfo.returnTypeIndices.size)
				LogError(varDecl->loc, "Not enough variables to receive all return "
						"values"_s);
			else if (varCount > procTypeInfo.returnTypeIndices.size)
				LogError(varDecl->loc, "Too many variables for procedure call return "
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
				LogError(astInitialValue->any.loc, "Cannot initialize a variable with a "
						"void expression"_s);

			if (astInitialValue->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				u64 givenValuesCount = astInitialValue->multipleExpressions.array.size;
				// We shouldn't have 1-value multiple expression nodes
				ASSERT(givenValuesCount > 1);
				if (varCount != givenValuesCount) {
					SourceLocation beg, end;
					GetSourceLocRangeForExpression(astInitialValue, &beg, &end);
					if (varCount == 1)
						Log2Error(varDecl->loc, end,
								"Trying to initialize a variable to multiple values"_s);
					else
						Log2Error(varDecl->loc, end,
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
					Log2Error(varDecl->loc, end,
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

	u32 newValueFlags  = g_context->config.dontPromoteMemoryToRegisters ? VALUEFLAGS_FORCE_MEMORY : 0;
	newValueFlags     |= varDecl->isStatic   ? VALUEFLAGS_ON_STATIC_STORAGE : 0;
	newValueFlags     |= varDecl->isExternal ? VALUEFLAGS_IS_EXTERNAL       : 0;

	Array<TCScopeName, ThreadAllocator> scopeNamesToAdd;
	ArrayInit(&scopeNamesToAdd, varCount);

	for (int varIdx = 0; varIdx < varCount; ++varIdx) {
		String varName = *GetVariableName(varDecl, varIdx);
		u32 typeIdx = *GetVariableTypeIdx(varDecl, varIdx);
		u32 valueIdx;
		if (!isGlobal)
			valueIdx = TCNewValue(tcContext, varName, typeIdx, newValueFlags);
		else
			valueIdx = NewGlobalValue(varName, typeIdx, newValueFlags);
		*GetVariableValueIdx(varDecl, varIdx) = valueIdx;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = varName;
		newScopeName.variableInfo.valueIdx = valueIdx;
		newScopeName.variableInfo.typeTableIdx = typeIdx;
		newScopeName.loc = varDecl->loc;
		*ArrayAdd(&scopeNamesToAdd) = newScopeName;
	}
	TCAddScopeNames(tcContext, scopeNamesToAdd);

	if (varCount == 0) {
		TypeCategory typeCat = GetTypeInfo(varDecl->specifiedTypeIdx).typeCategory;
		if (typeCat != TYPECATEGORY_STRUCT && typeCat != TYPECATEGORY_UNION)
			LogError(varDecl->loc, "Anonymous variable has to be a struct/union!"_s);
		if (isGlobal)
			LogError(varDecl->loc, "Anonymous variable can't be static!"_s);

		varDecl->anonymousVariableValueIdx = TCNewValue(tcContext, ""_s, varDecl->specifiedTypeIdx,
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
		TCAddStructMembersToScope(tcContext, varDecl->loc, varExp);
	}
}

void TypeCheckProcedureParameter(TCContext *tcContext, ASTProcedureParameter *astParam)
{
	// @Check: who should report this error?
	ASSERT(astParam->astType);
	astParam->typeTableIdx = TypeCheckType(tcContext, {}, astParam->loc, astParam->astType);
	if (astParam->typeTableIdx == TYPETABLEIDX_VOID)
		LogError(astParam->loc, "Variable can't be of type void!"_s);

	if (astParam->astInitialValue) {
		TypeCheckExpression(tcContext, astParam->astInitialValue);

		if (astParam->astType) {
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize( astParam->typeTableIdx,
					astParam->astInitialValue);
			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				Print("Variable declaration type and initial type don't match\n");
				ReportTypeCheckError(typeCheckResult.errorCode, astParam->loc,
						astParam->typeTableIdx, astParam->astInitialValue->typeTableIdx);
			}
			astParam->typeTableIdx = typeCheckResult.leftTypeIdx;
			astParam->astInitialValue->typeTableIdx = typeCheckResult.rightTypeIdx;
		}
		else {
			if (astParam->astInitialValue->typeTableIdx == TYPETABLEIDX_VOID)
				LogError(astParam->loc, "Variable can't be of type void!"_s);

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
ReturnCheckResult TCCheckIfReturnsValue(ASTExpression *expression)
{
	switch (expression->nodeType) {
	case ASTNODETYPE_RETURN:
		return RETURNCHECKRESULT_ALWAYS;
	case ASTNODETYPE_BLOCK:
	{
		ReturnCheckResult result = RETURNCHECKRESULT_NEVER;
		for (int i = 0; i < expression->block.statements.size; ++i) {
			ReturnCheckResult statementResult = TCCheckIfReturnsValue(&expression->block.statements[i]);
			if (statementResult > result) {
				result = statementResult;
			}
		}
		return result;
	}
	case ASTNODETYPE_IF:
	{
		ReturnCheckResult ifStatement = TCCheckIfReturnsValue(expression->ifNode.body);
		ReturnCheckResult elseStatement = RETURNCHECKRESULT_NEVER;
		if (expression->ifNode.elseBody)
			elseStatement = TCCheckIfReturnsValue(expression->ifNode.elseBody);

		if (ifStatement == RETURNCHECKRESULT_ALWAYS && elseStatement == RETURNCHECKRESULT_ALWAYS)
			return RETURNCHECKRESULT_ALWAYS;

		if (ifStatement != RETURNCHECKRESULT_NEVER || elseStatement != RETURNCHECKRESULT_NEVER)
			return RETURNCHECKRESULT_SOMETIMES;
	}
	case ASTNODETYPE_WHILE:
		return TCCheckIfReturnsValue(expression->whileNode.body);
	case ASTNODETYPE_FOR:
		return TCCheckIfReturnsValue(expression->forNode.body);
	}
	return RETURNCHECKRESULT_NEVER;
}

bool DoesTypeReferToPolymorphicType(ASTType *astType)
{
	switch (astType->nodeType) {
	case ASTTYPENODETYPE_IDENTIFIER:
		return false;
	case ASTTYPENODETYPE_POINTER:
		return DoesTypeReferToPolymorphicType(astType->pointedType);
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		ArrayView<ASTStructMemberDeclaration> members = astType->structDeclaration.members;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx) {
			if (DoesTypeReferToPolymorphicType(members[memberIdx].astType))
				return true;
		}
		return false;
	}
	case ASTTYPENODETYPE_ENUM_DECLARATION:
		return DoesTypeReferToPolymorphicType(astType->enumDeclaration.astType);
	case ASTTYPENODETYPE_ARRAY:
		return DoesTypeReferToPolymorphicType(astType->arrayType);
	case ASTTYPENODETYPE_PROCEDURE:
	{
		ArrayView<ASTProcedureParameter> params = astType->procedurePrototype.astParameters;
		for (int paramIdx = 0; paramIdx < params.size; ++paramIdx) {
			if (DoesTypeReferToPolymorphicType(params[paramIdx].astType))
				return true;
		}
		ArrayView<ASTType *> returnTypes = astType->procedurePrototype.astReturnTypes;
		for (int retIdx = 0; retIdx < returnTypes.size; ++retIdx) {
			if (DoesTypeReferToPolymorphicType(returnTypes[retIdx]))
				return true;
		}
		return false;
	}
	case ASTTYPENODETYPE_TYPE_ARGUMENT_DECLARATION:
		return true;
	}
	ASSERT(!"Unexpected type category");
	return false;
}

void TCFindAllPolymorphicTypesInASTType(ASTType *astType, SourceLocation loc,
		DynamicArray<TCScopeName, ThreadAllocator> *scopeNames)
{
	switch (astType->nodeType) {
	case ASTTYPENODETYPE_POINTER:
		TCFindAllPolymorphicTypesInASTType(astType->pointedType, loc, scopeNames);
		break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		ArrayView<ASTStructMemberDeclaration> members = astType->structDeclaration.members;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx)
			TCFindAllPolymorphicTypesInASTType(members[memberIdx].astType, loc, scopeNames);
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
		TCFindAllPolymorphicTypesInASTType(astType->enumDeclaration.astType, loc, scopeNames);
		break;
	case ASTTYPENODETYPE_ARRAY:
		TCFindAllPolymorphicTypesInASTType(astType->arrayType, loc, scopeNames);
		break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		ArrayView<ASTProcedureParameter> params = astType->procedurePrototype.astParameters;
		for (int paramIdx = 0; paramIdx < params.size; ++paramIdx)
			TCFindAllPolymorphicTypesInASTType(params[paramIdx].astType, loc, scopeNames);

		ArrayView<ASTType *> returnTypes = astType->procedurePrototype.astReturnTypes;
		for (int retIdx = 0; retIdx < returnTypes.size; ++retIdx)
			TCFindAllPolymorphicTypesInASTType(returnTypes[retIdx], loc, scopeNames);
	} break;
	case ASTTYPENODETYPE_TYPE_ARGUMENT_DECLARATION:
	{
		if (!scopeNames->size)
			DynamicArrayInit(scopeNames, 2);


		StaticDefinition newStaticDef = {};
		newStaticDef.definitionType = STATICDEFINITIONTYPE_TYPE;
		newStaticDef.name = astType->name;
		newStaticDef.typeTableIdx = TYPETABLEIDX_Unset;
		u32 newStaticDefIdx = NewStaticDefinition(&newStaticDef);

		TCScopeName newScopeName = {};
		newScopeName.type = NAMETYPE_STATIC_DEFINITION;
		newScopeName.loc = loc;
		newScopeName.staticDefinitionIdx = newStaticDefIdx;
		newScopeName.name = astType->name;
		*DynamicArrayAdd(scopeNames) = newScopeName;
	} break;
	}
}

void TCAddPlaceholderTypesForProcedurePrototype(TCContext *tcContext,
		ASTProcedurePrototype *astPrototype)
{
	// Scope names for polymorphic types
	ArrayView<ASTProcedureParameter> astParameters = astPrototype->astParameters;
	{
		DynamicArray<TCScopeName, ThreadAllocator> polymorphicTypesScopeNames = {};

		StaticDefinition newStaticDef = {};
		newStaticDef.definitionType = STATICDEFINITIONTYPE_TYPE;
		newStaticDef.typeTableIdx = TYPETABLEIDX_Anything;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_STATIC_DEFINITION;

		for (int i = 0; i < astParameters.size; ++i) {
			const ASTProcedureParameter *astParameter = &astParameters[i];
			TCFindAllPolymorphicTypesInASTType(astParameter->astType, astParameter->loc,
					&polymorphicTypesScopeNames);
		}
		for (int i = 0; i < polymorphicTypesScopeNames.size; ++i) {
			StaticDefinition staticDef =
				GetStaticDefinition(polymorphicTypesScopeNames[i].staticDefinitionIdx);
			staticDef.typeTableIdx = TYPETABLEIDX_Anything;
			UpdateStaticDefinition(polymorphicTypesScopeNames[i].staticDefinitionIdx, &staticDef);
		}
		if (polymorphicTypesScopeNames.size)
			TCAddScopeNames(tcContext, polymorphicTypesScopeNames);
	}
}

bool TypeCheckProcedurePrototype(TCContext *tcContext, ASTProcedurePrototype *astPrototype)
{
	// Parameters
	bool beginOptionalParameters = false;
	for (int i = 0; i < astPrototype->astParameters.size; ++i) {
		ASTProcedureParameter *astParam = &astPrototype->astParameters[i];
		TypeCheckProcedureParameter(tcContext, astParam);

		if (!astParam->astInitialValue) {
			if (beginOptionalParameters)
				LogError(astParam->loc,
						"Non-optional parameter after optional parameter found!"_s);
		}
		else
			beginOptionalParameters = true;
	}

	if (astPrototype->astReturnTypes.size) {
		u32 firstTypeIdx = TypeCheckType(tcContext, {}, astPrototype->loc, astPrototype->astReturnTypes[0]);
		if (firstTypeIdx != TYPETABLEIDX_VOID) {
			DynamicArrayInit(&astPrototype->returnTypeIndices, astPrototype->astReturnTypes.size);
			*DynamicArrayAdd(&astPrototype->returnTypeIndices) = firstTypeIdx;
			for (int i = 1; i < astPrototype->astReturnTypes.size; ++i)
				*DynamicArrayAdd(&astPrototype->returnTypeIndices) =
					TypeCheckType(tcContext, {}, astPrototype->loc, astPrototype->astReturnTypes[i]);
		}
	}

	return true;
}

TypeInfo TypeInfoFromASTProcedurePrototype(ASTProcedurePrototype *astPrototype)
{
	TypeInfo t = {};
	t.size = g_pointerSize;
	t.typeCategory = TYPECATEGORY_PROCEDURE;
	t.procedureInfo.isVarargs = astPrototype->isVarargs;
	t.procedureInfo.callingConvention = astPrototype->callingConvention;

	if (astPrototype->returnTypeIndices.size) {
		DynamicArrayInit(&t.procedureInfo.returnTypeIndices, astPrototype->returnTypeIndices.size);
		for (int i = 0; i < astPrototype->returnTypeIndices.size; ++i)
			*DynamicArrayAdd(&t.procedureInfo.returnTypeIndices) = astPrototype->returnTypeIndices[i];
	}

	u64 astParametersCount = astPrototype->astParameters.size;
	if (astParametersCount) {
		DynamicArray<ProcedureParameter, LinearAllocator> parameters;
		DynamicArrayInit(&parameters, astParametersCount);

		// Parameters
		for (int i = 0; i < astParametersCount; ++i) {
			ASTProcedureParameter astParam = astPrototype->astParameters[i];

			ProcedureParameter procParam = {};
			procParam.typeTableIdx = astParam.typeTableIdx;
			if (DoesTypeReferToPolymorphicType(astParam.astType))
				procParam.isPolymorphic = true;
			if (astParam.astInitialValue) {
				procParam.defaultValue = TryEvaluateConstant(astParam.astInitialValue);
				if (procParam.defaultValue.type == CONSTANTTYPE_INVALID)
					LogError(astParam.astInitialValue->any.loc,
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

	if (astPrototype->returnTypeIndices.size) {
		DynamicArrayInit(&t.procedureInfo.returnTypeIndices, astPrototype->returnTypeIndices.size);
		for (int i = 0; i < astPrototype->returnTypeIndices.size; ++i)
			*DynamicArrayAdd(&t.procedureInfo.returnTypeIndices) = astPrototype->returnTypeIndices[i];
	}

	return t;
}

ASTExpression TCDeepCopyTreeBranch(TCContext *tcContext, const ASTExpression *expression);

ASTType TCCopyASTType(TCContext *tcContext, const ASTType *astType)
{
	ASTType result = *astType;

	switch (astType->nodeType) {
	case ASTTYPENODETYPE_POINTER:
	{
		ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
		*newASTType = TCCopyASTType(tcContext, astType->pointedType);
		result.pointedType = newASTType;
	} break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		ArrayView<const ASTStructMemberDeclaration> members = astType->structDeclaration.members;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx) {
			ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
			*newASTType = TCCopyASTType(tcContext, members[memberIdx].astType);
			result.structDeclaration.members[memberIdx].astType = newASTType;
		}
		break;
	}
	case ASTTYPENODETYPE_ENUM_DECLARATION:
	{
		ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
		*newASTType = TCCopyASTType(tcContext, astType->enumDeclaration.astType);
		result.enumDeclaration.astType = newASTType;
	} break;
	case ASTTYPENODETYPE_ARRAY:
	{
		ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
		*newASTType = TCCopyASTType(tcContext, astType->arrayType);
		result.arrayType = newASTType;

		if (result.arrayCountExp) {
			ASTExpression *newArrayCountExp = TCNewTreeNode();
			*newArrayCountExp = TCDeepCopyTreeBranch(tcContext, result.arrayCountExp);
			result.arrayCountExp = newArrayCountExp;
		}
	} break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		ArrayView<const ASTProcedureParameter> params = astType->procedurePrototype.astParameters;
		for (int paramIdx = 0; paramIdx < params.size; ++paramIdx) {
			ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
			*newASTType = TCCopyASTType(tcContext, params[paramIdx].astType);
			result.procedurePrototype.astParameters[paramIdx].astType = newASTType;
		}
		ArrayView<ASTType * const> returnTypes = astType->procedurePrototype.astReturnTypes;
		for (int retIdx = 0; retIdx < returnTypes.size; ++retIdx) {
			ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
			*newASTType = TCCopyASTType(tcContext, returnTypes[retIdx]);
			result.procedurePrototype.astReturnTypes[retIdx] = newASTType;
		}
		break;
	}
	}

	return result;
}

ASTExpression TCDeepCopyTreeBranch(TCContext *tcContext, const ASTExpression *expression)
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

		TCPushScope(tcContext);

		for (int i = 0; i < expression->block.statements.size; ++i) {
			ASTExpression statement = TCDeepCopyTreeBranch(tcContext,
					&expression->block.statements[i]);
			if (statement.nodeType != ASTNODETYPE_INVALID)
				*DynamicArrayAdd(&astBlock.statements) = statement;
		}

		TCPopScope(tcContext);

		result.block = astBlock;
		return result;
	}
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
		*newASTType = TCCopyASTType(tcContext, varDecl.astType);
		varDecl.astType = newASTType;

		u32 flags = (g_context->config.dontPromoteMemoryToRegisters ? VALUEFLAGS_FORCE_MEMORY : 0) |
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
				valueIdx = TCNewValue(tcContext, varName, typeTableIdx, flags);
			else
				valueIdx = NewGlobalValue(varName, typeTableIdx, flags);
			*GetVariableValueIdx(&varDecl, varIdx) = valueIdx;

			TCScopeName newScopeName;
			newScopeName.type = NAMETYPE_VARIABLE;
			newScopeName.name = varName;
			newScopeName.variableInfo.valueIdx = valueIdx;
			newScopeName.variableInfo.typeTableIdx = typeTableIdx;
			newScopeName.loc = varDecl.loc;
			*ArrayAdd(&scopeNamesToAdd) = newScopeName;
		}
		TCAddScopeNames(tcContext, scopeNamesToAdd);

		if (varCount == 0) {
			ASSERT(!varDecl.isStatic && !varDecl.isExternal);
			varDecl.anonymousVariableValueIdx = TCNewValue(tcContext, ""_s, varDecl.specifiedTypeIdx, flags);

			ASTExpression *varExp = TCNewTreeNode();
			*varExp = {
				.nodeType = ASTNODETYPE_IDENTIFIER,
				.typeTableIdx = varDecl.specifiedTypeIdx,
				.identifier = {
					.type = NAMETYPE_VARIABLE,
					.valueIdx = varDecl.anonymousVariableValueIdx
				}
			};
			TCAddStructMembersToScope(tcContext, varDecl.loc, varExp);
		}

		if (varDecl.astInitialValue) {
			ASTExpression *astInitialValue = TCNewTreeNode();
			*astInitialValue = TCDeepCopyTreeBranch(tcContext, varDecl.astInitialValue);
			varDecl.astInitialValue = astInitialValue;
		}

		result.variableDeclaration = varDecl;
		return result;
	}
	case ASTNODETYPE_IDENTIFIER:
	{
		ASTIdentifier astIdentifier = expression->identifier;

		TCScopeName scopeName = TCFindScopeName(tcContext, expression->any.loc, astIdentifier.string);

		astIdentifier.type = scopeName.type;
		switch (scopeName.type) {
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
		TCAddScopeNames(tcContext, scopeNamesToAdd);

		return {};
	}
	case ASTNODETYPE_RETURN:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->returnNode.expression);
		result.returnNode.expression = e;
		return result;
	}
	case ASTNODETYPE_DEFER:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->deferNode.expression);
		result.deferNode.expression = e;
		return result;
	}
	case ASTNODETYPE_USING:
	{
		ASTExpression *usingExp = TCNewTreeNode();
		*usingExp = TCDeepCopyTreeBranch(tcContext, expression->usingNode.expression);

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
			TCAddStructMembersToScope(tcContext, usingExp->any.loc, varExp);
		}
		else
			TCAddStructMembersToScope(tcContext, usingExp->any.loc, usingExp);

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
				*arg = TCDeepCopyTreeBranch(tcContext, original.arguments[argIdx]);
				*DynamicArrayAdd(&astProcCall.arguments) = arg;
			}
		}

		ASTExpression *exp = TCNewTreeNode();
		*exp = TCDeepCopyTreeBranch(tcContext, original.procedureExpression);

		result.procedureCall = astProcCall;
		return result;
	}
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTUnaryOperation astUnary = expression->unaryOperation;
		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->unaryOperation.expression);
		astUnary.expression = e;
		result.unaryOperation = astUnary;
		return result;
	}
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTBinaryOperation astBinary = expression->binaryOperation;
		ASTExpression *l = TCNewTreeNode();
		ASTExpression *r = TCNewTreeNode();
		*l = TCDeepCopyTreeBranch(tcContext, expression->binaryOperation.leftHand);

		// For member access we can just copy
		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
			*r = *expression->binaryOperation.rightHand;
		else
			*r = TCDeepCopyTreeBranch(tcContext, expression->binaryOperation.rightHand);

		astBinary.leftHand = l;
		astBinary.rightHand = r;
		result.binaryOperation = astBinary;
		return result;
	}
	case ASTNODETYPE_LITERAL:
	{
		ASTLiteral astLiteral = expression->literal;

		if (expression->literal.type == LITERALTYPE_GROUP) {
			ArrayInit(&astLiteral.members, expression->literal.members.size);
			for (int memberIdx = 0; memberIdx < expression->literal.members.size; ++memberIdx) {
				ASTExpression *e = TCNewTreeNode();
				*e = TCDeepCopyTreeBranch(tcContext, expression->literal.members[memberIdx]);
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
		*e = TCDeepCopyTreeBranch(tcContext, expression->ifNode.condition);
		astIf.condition = e;

		e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->ifNode.body);
		astIf.body = e;

		if (expression->ifNode.elseBody) {
			e = TCNewTreeNode();
			*e = TCDeepCopyTreeBranch(tcContext, expression->ifNode.elseBody);
			astIf.elseBody = e;
		}

		result.ifNode = astIf;
		return result;
	}
	case ASTNODETYPE_WHILE:
	{
		ASTWhile astWhile = expression->whileNode;

		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->whileNode.condition);
		astWhile.condition = e;

		e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->whileNode.body);
		astWhile.body = e;

		result.whileNode = astWhile;
		return result;
	}
	case ASTNODETYPE_FOR:
	{
		ASTFor astFor = expression->forNode;

		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, astFor.range);
		astFor.range = e;

		TCPushScope(tcContext);

		FixedArray<TCScopeName, 2> scopeNamesToAdd = {};

		// 'i' will be of type s64 when iterating over arrays/strings.
		u32 indexTypeIdx = TYPETABLEIDX_S64;

		ASTExpression *rangeExp = astFor.range;
		bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			rangeExp->binaryOperation.op == TOKEN_OP_RANGE;
		if (isExplicitRange) {
			indexTypeIdx = rangeExp->binaryOperation.leftHand->typeTableIdx;
			ASSERT(CheckTypesMatch(rangeExp->binaryOperation.leftHand->typeTableIdx,
						rangeExp->binaryOperation.rightHand->typeTableIdx) == TYPECHECK_COOL);
		}
		else {
			u32 elementTypeTableIdx = TYPETABLEIDX_U8;
			if (rangeExp->typeTableIdx != TYPETABLEIDX_STRING_STRUCT) {
				TypeInfo rangeTypeInfo = GetTypeInfo(rangeExp->typeTableIdx);
				if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER)
					rangeTypeInfo = GetTypeInfo(rangeTypeInfo.pointerInfo.pointedTypeTableIdx);
				ASSERT(rangeTypeInfo.typeCategory == TYPECATEGORY_ARRAY);
				elementTypeTableIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			}

			u32 pointerToElementTypeIdx = GetTypeInfoPointerOf(elementTypeTableIdx);
			u32 elementValueIdx = TCNewValue(tcContext, astFor.itemVariableName,
					pointerToElementTypeIdx, 0);
			astFor.elementValueIdx = elementValueIdx;

			*FixedArrayAdd(&scopeNamesToAdd) = {
				.type = NAMETYPE_VARIABLE,
				.name = astFor.itemVariableName,
				.loc = astFor.loc,
				.variableInfo = { .valueIdx = elementValueIdx, .typeTableIdx = pointerToElementTypeIdx }
			};
		}

		u32 indexValueIdx = TCNewValue(tcContext, astFor.indexVariableName, indexTypeIdx, 0);
		astFor.indexValueIdx = indexValueIdx;

		*FixedArrayAdd(&scopeNamesToAdd) = {
			.type = NAMETYPE_VARIABLE,
			.name = astFor.indexVariableName,
			.loc = astFor.loc,
			.variableInfo = { .valueIdx = indexValueIdx, .typeTableIdx = indexTypeIdx }
		};
		TCAddScopeNames(tcContext, scopeNamesToAdd);

		e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->forNode.body);
		astFor.body = e;

		TCPopScope(tcContext);

		result.forNode = astFor;
		return result;
	}
	case ASTNODETYPE_BREAK:
	case ASTNODETYPE_CONTINUE:
	case ASTNODETYPE_TYPE:
	case ASTNODETYPE_GARBAGE:
	{
		return *expression;
	}
	case ASTNODETYPE_TYPEOF:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->typeOfNode.expression);
		result.typeOfNode.expression = e;
		return result;
	}
	case ASTNODETYPE_SIZEOF:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->sizeOfNode.expression);
		result.sizeOfNode.expression = e;
		return result;
	}
	case ASTNODETYPE_CAST:
	{
		ASTExpression *e = TCNewTreeNode();
		*e = TCDeepCopyTreeBranch(tcContext, expression->castNode.expression);
		result.castNode.expression = e;
		result.castNode.astType = TCCopyASTType(tcContext, &expression->castNode.astType);
		return result;
	}
	case ASTNODETYPE_INTRINSIC:
	{
		ASTIntrinsic astIntrinsic = expression->intrinsic;
		u64 argCount = expression->intrinsic.arguments.size;
		if (argCount) {
			DynamicArrayInit(&astIntrinsic.arguments, argCount);
			for (int argIdx = 0; argIdx < argCount; ++argIdx) {
				*DynamicArrayAdd(&astIntrinsic.arguments) = TCDeepCopyTreeBranch(tcContext,
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

void TCAddParametersToScope(TCContext *tcContext, ArrayView<u32> parameterValues,
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
			TCAddStructMembersToScope(tcContext, astParameter.loc, varExp);
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
		static u32 arrayTableIdx = GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, 0);

		u32 paramValueIdx = { parameterValues[astPrototype->astParameters.size] };

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_VARIABLE;
		newScopeName.name = astPrototype->varargsName;
		newScopeName.variableInfo.valueIdx = paramValueIdx;
		newScopeName.variableInfo.typeTableIdx = arrayTableIdx;
		newScopeName.loc = astPrototype->varargsLoc;
		*ArrayAdd(&scopeNamesToAdd) = newScopeName;
	}
	TCAddScopeNames(tcContext, scopeNamesToAdd);
}

u32 NewProcedure(Procedure p, bool isExternal)
{
	u32 procedureIdx;
	if (!isExternal) {
		auto procedures = g_context->procedures.GetForWrite();
		procedureIdx = (u32)procedures->count;
		*BucketArrayAdd(&procedures) = p;

		{
			auto inlineCalls = g_context->tcInlineCalls.Get();
			if (inlineCalls->size <= procedureIdx)
				DynamicArrayAddMany(&inlineCalls, procedureIdx - inlineCalls->size);
			inlineCalls[procedureIdx] = {};
		}
	}
	else {
		auto externalProcedures = g_context->externalProcedures.GetForWrite();
		procedureIdx = ((u32)externalProcedures->count | PROCEDURE_EXTERNAL_BIT);
		*BucketArrayAdd(&externalProcedures) = p;
	}
	return procedureIdx;
}

bool TCPushParametersAndInlineProcedureCall(TCContext *tcContext, ASTProcedureCall *astProcCall)
{
	if (astProcCall->callType != CALLTYPE_STATIC)
		return false;

	Procedure proc = GetProcedureRead(astProcCall->procedureIdx);

	if (astProcCall->inlineType == CALLINLINETYPE_NEVER_INLINE)
		return false;
	if (!proc.isInline && astProcCall->inlineType != CALLINLINETYPE_ALWAYS_INLINE)
		return false;

	ASSERT(GetTypeInfo(proc.typeTableIdx).typeCategory == TYPECATEGORY_PROCEDURE);
	TypeInfoProcedure procTypeInfo = GetTypeInfo(proc.typeTableIdx).procedureInfo;

	TCPushScope(tcContext);

	s64 totalArguments = procTypeInfo.parameters.size;
	ArrayInit(&astProcCall->inlineParameterValues, totalArguments + 1);

	for (int argIdx = 0; argIdx < totalArguments; ++argIdx) {
		u32 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
		u32 newValueIdx = TCNewValue(tcContext, SNPrintF(12, "_inlinearg%d", argIdx),
				paramTypeIdx, 0);
		*ArrayAdd(&astProcCall->inlineParameterValues) = newValueIdx;
	}
	if (procTypeInfo.isVarargs) {
		static u32 arrayTableIdx = GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, 0);
		u32 newValueIdx = TCNewValue(tcContext, "_inlinevarargs"_s, arrayTableIdx, 0);
		*ArrayAdd(&astProcCall->inlineParameterValues) = newValueIdx;
	}

	TCAddParametersToScope(tcContext, astProcCall->inlineParameterValues, &proc.astPrototype);

	ASTExpression *e = TCNewTreeNode();
	*e = TCDeepCopyTreeBranch(tcContext, proc.astBody);
	astProcCall->astBodyInlineCopy = e;

	TCPopScope(tcContext);

	return true;
}

bool TCIsPrimitiveOperation(enum TokenType op, u32 leftTypeIdx, u32 rightTypeIdx)
{
	leftTypeIdx  = StripImplicitlyCastAliases(leftTypeIdx);
	rightTypeIdx = StripImplicitlyCastAliases(rightTypeIdx);
	switch (op) {
	case TOKEN_OP_ASSIGNMENT:
		return CheckTypesMatch(leftTypeIdx, rightTypeIdx) == TYPECHECK_COOL;
	case TOKEN_OP_MEMBER_ACCESS:
	{
		TypeCategory leftCat  = GetTypeInfo(leftTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_STRUCT || leftCat == TYPECATEGORY_UNION;
	} break;
	case TOKEN_OP_ARRAY_ACCESS:
	{
		TypeCategory leftCat  = GetTypeInfo(leftTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_ARRAY;
	} break;
	case TOKEN_OP_PLUS:
	case TOKEN_OP_ASSIGNMENT_PLUS:
	case TOKEN_OP_MINUS:
	case TOKEN_OP_ASSIGNMENT_MINUS:
	{
		TypeCategory leftCat  = GetTypeInfo(leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_FLOATING &&
			leftCat  != TYPECATEGORY_POINTER)
			return false;

		TypeCategory rightCat = GetTypeInfo(rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_FLOATING &&
			rightCat != TYPECATEGORY_POINTER)
			return false;

		if (CheckTypesMatch(leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
			return false;

		return true;
	} break;
	case TOKEN_OP_SHIFT_LEFT:
	case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
	case TOKEN_OP_SHIFT_RIGHT:
	case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
	{
		TypeCategory leftCat  = GetTypeInfo(leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_ENUM &&
			leftCat  != TYPECATEGORY_POINTER)
			return false;

		TypeCategory rightCat = GetTypeInfo(rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_ENUM &&
			rightCat != TYPECATEGORY_POINTER)
			return false;

		// Don't care about sign/size for shifts
		TypeCheckErrorCode typeCheckError = CheckTypesMatch(leftTypeIdx, rightTypeIdx);
		if (typeCheckError != TYPECHECK_COOL && typeCheckError != TYPECHECK_SIZE_MISMATCH &&
				typeCheckError != TYPECHECK_SIGN_MISMATCH)
			return false;

		return true;
	} break;
	case TOKEN_OP_BITWISE_AND:
	case TOKEN_OP_BITWISE_OR:
	case TOKEN_OP_BITWISE_XOR:
	{
		TypeCategory leftCat  = GetTypeInfo(leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_POINTER &&
			leftCat  != TYPECATEGORY_ENUM)
			return false;

		TypeCategory rightCat = GetTypeInfo(rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_POINTER &&
			rightCat != TYPECATEGORY_ENUM)
			return false;

		if (CheckTypesMatch(leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
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
		TypeCategory leftCat  = GetTypeInfo(leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_POINTER &&
			leftCat  != TYPECATEGORY_ENUM    && leftCat  != TYPECATEGORY_FLOATING)
			return false;

		TypeCategory rightCat = GetTypeInfo(rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_POINTER &&
			rightCat != TYPECATEGORY_ENUM    && rightCat != TYPECATEGORY_FLOATING)
			return false;

		if (CheckTypesMatch(leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
			return false;

		return true;
	} break;
	case TOKEN_OP_AND:
	case TOKEN_OP_OR:
		return CheckTypesMatch(TYPETABLEIDX_BOOL, leftTypeIdx)  == TYPECHECK_COOL &&
			   CheckTypesMatch(TYPETABLEIDX_BOOL, rightTypeIdx) == TYPECHECK_COOL;
	default:
	{
		TypeCategory leftCat  = GetTypeInfo(leftTypeIdx).typeCategory;
		if (leftCat  != TYPECATEGORY_INTEGER && leftCat  != TYPECATEGORY_FLOATING)
			return false;

		TypeCategory rightCat = GetTypeInfo(rightTypeIdx).typeCategory;
		if (rightCat != TYPECATEGORY_INTEGER && rightCat != TYPECATEGORY_FLOATING)
			return false;

		if (CheckTypesMatch(leftTypeIdx, rightTypeIdx) != TYPECHECK_COOL)
			return false;

		return true;
	} break;
	}
}

bool TCIsPrimitiveOperation(enum TokenType op, u32 inputTypeIdx)
{
	switch (op) {
	case TOKEN_OP_MINUS:
	{
		TypeCategory leftCat  = GetTypeInfo(inputTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_INTEGER || leftCat == TYPECATEGORY_FLOATING;
	} break;
	case TOKEN_OP_BITWISE_NOT:
	{
		TypeCategory leftCat  = GetTypeInfo(inputTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_INTEGER || leftCat == TYPECATEGORY_POINTER;
	} break;
	case TOKEN_OP_NOT:
		return CheckTypesMatch(TYPETABLEIDX_BOOL, inputTypeIdx) == TYPECHECK_COOL;
	case TOKEN_OP_DEREFERENCE:
	{
		TypeCategory leftCat  = GetTypeInfo(inputTypeIdx).typeCategory;
		return leftCat == TYPECATEGORY_POINTER;
	} break;
	case TOKEN_OP_POINTER_TO:
	{
		return true;
	} break;
	default:
	{
		TypeCategory inputCat = GetTypeInfo(inputTypeIdx).typeCategory;
		return inputCat == TYPECATEGORY_INTEGER || inputCat == TYPECATEGORY_FLOATING;
	} break;
	}
}

void TCCheckForCyclicInlineCalls(SourceLocation loc, u32 callerProcIdx,
		u32 calleeProcIdx)
{
	auto inlineCalls = g_context->tcInlineCalls.Get();
	// Register inline call
	if (!inlineCalls[callerProcIdx].capacity)
		DynamicArrayInit(&inlineCalls[callerProcIdx], 4);
	*DynamicArrayAdd(&inlineCalls[callerProcIdx]) = { calleeProcIdx, loc };

	// Check for cyclic dependencies
	ArrayView<const InlineCall> inlinedCalls = inlineCalls[calleeProcIdx];
	for (int i = 0; i < inlinedCalls.size; ++i) {
		if (inlinedCalls[i].procedureIdx == callerProcIdx) {
			// @Incomplete: improve error message
			LogErrorNoCrash(loc, TPrintF("Procedures "
					"\"%S\" and \"%S\" are trying to inline each other.",
					GetProcedureRead(callerProcIdx).name,
					GetProcedureRead(calleeProcIdx).name));
			LogNote(inlinedCalls[i].loc, "See other call"_s);
			PANIC;
		}
	}
}

String OperatorToString(s32 op);
bool TCLookForOperatorOverload(TCContext *tcContext, ASTExpression *expression)
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
		if (TCIsPrimitiveOperation(op, leftHand->typeTableIdx))
			return false;
	}
	else {
		op = expression->binaryOperation.op;
		paramCount = 2;
		leftHand = expression->binaryOperation.leftHand;
		rightHand = expression->binaryOperation.rightHand;
		// These can't have overloads!
		if (TCIsPrimitiveOperation(op, leftHand->typeTableIdx, rightHand->typeTableIdx))
			return false;
	}

	bool triedOnce = false;
tryAgain:
	{
		auto operatorOverloads = g_context->operatorOverloads.GetForRead();
		for (int overloadIdx = 0; overloadIdx < operatorOverloads->size; ++overloadIdx) {
			OperatorOverload currentOverload = operatorOverloads[overloadIdx];

			if (op != currentOverload.op)
				continue;

			Procedure procedure = GetProcedureRead(currentOverload.procedureIdx);
			TypeInfo procType = GetTypeInfo(procedure.typeTableIdx);
			ASSERT(procType.typeCategory == TYPECATEGORY_PROCEDURE);

			if (paramCount == 1) {
				if (procType.procedureInfo.parameters.size != 1)
					continue;

				u32 leftHandTypeIdx  = procType.procedureInfo.parameters[0].typeTableIdx;

				if (CheckTypesMatch(leftHand->typeTableIdx, leftHandTypeIdx) != TYPECHECK_COOL)
					continue;

				if (foundOverload)
					LogError(expression->any.loc,
							TPrintF("Multiple overloads found for operator %S with operand of "
								"type %S",
								OperatorToString(op),
								TypeInfoToString(leftHand->typeTableIdx)));

				overload = currentOverload;
				foundOverload = true;
			}
			else {
				if (procType.procedureInfo.parameters.size != 2)
					continue;

				u32 leftHandTypeIdx  = procType.procedureInfo.parameters[0].typeTableIdx;
				u32 rightHandTypeIdx = procType.procedureInfo.parameters[1].typeTableIdx;

				if (CheckTypesMatch(leftHand->typeTableIdx, leftHandTypeIdx) != TYPECHECK_COOL ||
					CheckTypesMatch(rightHand->typeTableIdx, rightHandTypeIdx) != TYPECHECK_COOL)
					continue;

				if (foundOverload)
					LogError(expression->any.loc,
							TPrintF("Multiple overloads found for operator %S with left hand "
								"of type %S and right hand of type %S",
								OperatorToString(op),
								TypeInfoToString(leftHand->typeTableIdx),
								TypeInfoToString(rightHand->typeTableIdx)));

				overload = currentOverload;
				foundOverload = true;
			}
		}
		if (!foundOverload) {
			// Shouldn't have resumed this job if the overload still doesn't exist.
			if (triedOnce)
				LogCompilerError(expression->any.loc, "Bad job resume"_s);
			triedOnce = true;

			SwitchJob(YIELDREASON_UNKNOWN_OVERLOAD, {
					.loc = expression->any.loc,
					.overload = {
						.op = (u32)op,
						.leftTypeIdx  = leftHand->typeTableIdx,
						.rightTypeIdx = rightHand ? rightHand->typeTableIdx : U32_MAX
					}});
			// Lock again!
			SYSLockForRead(&g_context->operatorOverloads.rwLock);
			goto tryAgain;
		}
	}

	ASSERT(foundOverload);
	Procedure proc = GetProcedureRead(overload.procedureIdx);

	TypeInfo procTypeInfo = GetTypeInfo(proc.typeTableIdx);
	ASSERT(procTypeInfo.typeCategory == TYPECATEGORY_PROCEDURE);

	if (proc.isInline && !proc.isBodyTypeChecked) {
		TCCheckForCyclicInlineCalls(expression->any.loc, tcContext->currentProcedureIdx,
				overload.procedureIdx);

		auto procedures = g_context->procedures.GetForRead();
		proc = procedures[overload.procedureIdx];
		if (!proc.isBodyTypeChecked) {
			SwitchJob(YIELDREASON_PROC_BODY_NOT_READY, { .index = overload.procedureIdx });
			// Lock again!
			SYSLockForRead(&g_context->procedures.rwLock);
			proc = procedures[overload.procedureIdx];
			if (!proc.isBodyTypeChecked)
				LogCompilerError(expression->any.loc, "Bad job resume"_s);
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

	TCPushParametersAndInlineProcedureCall(tcContext, &expression->procedureCall);
	return true;
}

u32 TCInstantiateProcedure(TCContext *tcContext, u32 procedureIdx, ArrayView<u32> givenTypesRaw)
{
	Array<u32, ThreadAllocator> givenTypes;
	ArrayInit(&givenTypes, givenTypesRaw.size);
	for (int i = 0; i < givenTypesRaw.size; ++i) {
		*ArrayAdd(&givenTypes) = StripImplicitlyCastAliases(givenTypesRaw[i]);
	}
	// Try to find an existing instance first
	u32 polyIdx, polyInstIdx;
	{
		PolymorphicProcedure *poly = nullptr;
		auto polyInstances = g_context->polymorphicProcedures.GetForWrite();

		for (polyIdx = 0; polyIdx < polyInstances->size; ++polyIdx) {
			PolymorphicProcedure *curPoly = &polyInstances[polyIdx];
			if (curPoly->procedureIdx == procedureIdx) {
				poly = curPoly;
				break;
			}
		}
		if (!poly)
			goto noMatchingInstance;

		for (polyInstIdx = 0; polyInstIdx < poly->instances.size; ++polyInstIdx) {
			const PolymorphicProcedureInstance *instance = &poly->instances[polyInstIdx];
			for (int polyTypeIdx = 0; polyTypeIdx < instance->polymorphicTypeIndices.size; ++polyTypeIdx) {
				if (instance->polymorphicTypeIndices[polyTypeIdx] != givenTypes[polyTypeIdx]) {
					goto next;
				}
			}

			if (instance->procedureIdx == U32_MAX) {
				// Wait for procedure to be created
				SwitchJob(YIELDREASON_POLYMORPHIC_PROC_NOT_CREATED, {
						.polyInstance = {
							.procedureIdx = procedureIdx,
							.instanceIdx = u32(polyInstIdx) }
						});
				// Lock again!
				SYSLockForRead(&g_context->polymorphicProcedures.rwLock);

				poly = &polyInstances[polyIdx];
				instance = &poly->instances[polyInstIdx];

				if (instance->procedureIdx == U32_MAX)
					LogCompilerError({}, "Bad job resume"_s);
			}

			return instance->procedureIdx;
next:
			continue;
		}
noMatchingInstance:
		if (!poly) {
			polyIdx = u32(polyInstances->size);
			poly = DynamicArrayAdd(&polyInstances);
			*poly = { .procedureIdx = procedureIdx };
			DynamicArrayInit(&poly->instances, 16);
		}
		polyInstIdx = u32(poly->instances.size);
		PolymorphicProcedureInstance *newInstance = DynamicArrayAdd(&poly->instances);
		*newInstance = { .procedureIdx = U32_MAX }; // We didn't create the procedure yet.
		for (int i = 0; i < givenTypes.size; ++i)
			*FixedArrayAdd(&newInstance->polymorphicTypeIndices) = givenTypes[i];
	}

	Procedure proc = GetProcedureRead(procedureIdx);

	// @Improve: this is no better than C++ name mangling
	for (int i = 0; i < givenTypes.size; ++i)
		proc.name = TPrintF("%S_%u", proc.name, givenTypes[i]);

	// Copy ASTTypes
	{
		ArrayView<ASTProcedureParameter> oldAstParameters = proc.astPrototype.astParameters;
		DynamicArrayInit(&proc.astPrototype.astParameters, oldAstParameters.size);
		for (int i = 0; i < oldAstParameters.size; ++i) {
			ASTProcedureParameter *astParameter = DynamicArrayAdd(&proc.astPrototype.astParameters);
			*astParameter = oldAstParameters[i];
			ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
			*newASTType = TCCopyASTType(tcContext, oldAstParameters[i].astType);
			astParameter->astType = newASTType;
		}
		ArrayView<ASTType *> oldAstReturnTypes = proc.astPrototype.astReturnTypes;
		DynamicArrayInit(&proc.astPrototype.astReturnTypes, oldAstReturnTypes.size);
		for (int i = 0; i < oldAstReturnTypes.size; ++i) {
			ASTType **astReturnType = DynamicArrayAdd(&proc.astPrototype.astReturnTypes);
			ASTType *newASTType = ALLOC(LinearAllocator, ASTType);
			*newASTType = TCCopyASTType(tcContext, oldAstReturnTypes[i]);
			*astReturnType = newASTType;
		}
	}

	TCContext procedureContext = *tcContext;
	BucketArrayInit(&procedureContext.localValues);
	*BucketArrayAdd(&procedureContext.localValues) = {}; // No value number 0?
	DynamicArrayInit(&procedureContext.scopeStack, 8);

	procedureContext.onStaticContext = false;

	TCPushScope(&procedureContext);

	// Scope names for polymorphic types
	{
		ArrayView<ASTProcedureParameter> astParameters = proc.astPrototype.astParameters;
		DynamicArray<TCScopeName, ThreadAllocator> polymorphicTypesScopeNames = {};

		StaticDefinition newStaticDef = {};
		newStaticDef.definitionType = STATICDEFINITIONTYPE_TYPE;

		TCScopeName newScopeName;
		newScopeName.type = NAMETYPE_STATIC_DEFINITION;

		for (int i = 0; i < astParameters.size; ++i) {
			const ASTProcedureParameter *astParameter = &astParameters[i];
			TCFindAllPolymorphicTypesInASTType(astParameter->astType, astParameter->loc,
					&polymorphicTypesScopeNames);
		}
		ASSERT(polymorphicTypesScopeNames.size == givenTypes.size);
		for (int i = 0; i < givenTypes.size; ++i) {
			StaticDefinition staticDef =
				GetStaticDefinition(polymorphicTypesScopeNames[i].staticDefinitionIdx);
			staticDef.typeTableIdx = givenTypes[i];
			UpdateStaticDefinition(polymorphicTypesScopeNames[i].staticDefinitionIdx, &staticDef);
		}
		if (polymorphicTypesScopeNames.size)
			TCAddScopeNames(&procedureContext, polymorphicTypesScopeNames);
	}

	TypeCheckProcedurePrototype(&procedureContext, &proc.astPrototype);
	TypeInfo t = TypeInfoFromASTProcedurePrototype(&proc.astPrototype);
	proc.typeTableIdx = FindOrAddTypeTableIdx(t);
	u32 newProcIdx = NewProcedure(proc, false);

	// Update register with the new procedure index.
	{
		PolymorphicProcedure *poly = nullptr;
		auto polyInstances = g_context->polymorphicProcedures.GetForWrite();
		poly = &polyInstances[polyIdx];
		PolymorphicProcedureInstance *instance = &poly->instances[polyInstIdx];

		instance->procedureIdx = newProcIdx;

		// Wake up any jobs that were waiting for this instance to generate a new procedure
		{
			auto jobsWaiting = g_context->waitingJobsByReason[YIELDREASON_POLYMORPHIC_PROC_NOT_CREATED].Get();
			for (int i = 0; i < jobsWaiting->size; ) {
				u32 jobIdx = jobsWaiting[i];
				const Job *job = &g_context->jobs.unsafe[jobIdx];
				if (job->yieldContext.polyInstance.procedureIdx == procedureIdx &&
					job->yieldContext.polyInstance.instanceIdx == polyInstIdx) {
					EnqueueReadyJob(jobIdx);
					// Remove
					DynamicArraySwapRemove(&jobsWaiting, i);
				}
				else
					++i;
			}
		}
	}

	// Parameters
	ArrayView<ASTProcedureParameter> astParameters = proc.astPrototype.astParameters;
	for (int i = 0; i < astParameters.size; ++i) {
		const ASTProcedureParameter *astParameter = &astParameters[i];
		u32 paramValueIdx = TCNewValue(&procedureContext, astParameter->name, astParameter->typeTableIdx, 0);
		*DynamicArrayAdd(&proc.parameterValues) = paramValueIdx;
	}
	// Varargs array
	if (proc.astPrototype.isVarargs) {
		static u32 arrayTableIdx = GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, 0);

		u32 valueIdx = TCNewValue(&procedureContext, proc.astPrototype.varargsName, arrayTableIdx, 0);
		*DynamicArrayAdd(&proc.parameterValues) = valueIdx;
	}

	TCAddParametersToScope(&procedureContext, proc.parameterValues, &proc.astPrototype);

	if (proc.astPrototype.returnTypeIndices.size) {
		ArrayInit(&proc.returnValueIndices, proc.astPrototype.returnTypeIndices.size);
		for (int i = 0; i < proc.astPrototype.returnTypeIndices.size; ++i)
			*ArrayAdd(&proc.returnValueIndices) = TCNewValue(&procedureContext, "_returnValue"_s,
				proc.astPrototype.returnTypeIndices[i], 0);
	}

	procedureContext.currentProcedureIdx = newProcIdx;
	procedureContext.currentReturnTypes = t.procedureInfo.returnTypeIndices;

	// Deep-copy the body syntax tree
	{
		ASTExpression *astBodyCopy = TCNewTreeNode();
		*astBodyCopy = TCDeepCopyTreeBranch(&procedureContext, proc.astBody);
		proc.astBody = astBodyCopy;
	}

	TypeCheckExpression(&procedureContext, proc.astBody);

	proc.isBodyTypeChecked = true;

	UpdateProcedure(newProcIdx, &proc);

	// Wake up any jobs that were waiting for this proc body
	WakeUpAllByIndex(YIELDREASON_PROC_BODY_NOT_READY, newProcIdx);

	TCPopScope(&procedureContext);

	// Code gen!
	{
		IRJobArgs *args = ALLOC(LinearAllocator, IRJobArgs);
		*args = {
			.procedureIdx = newProcIdx,
			.localValues = procedureContext.localValues,
			.expression = nullptr
		};
		RequestNewJob(JOBTYPE_CODEGEN, IRJobProcedure, (void *)args);
	}

	return newProcIdx;
}

void TCExtractPolymorphicTypes(ASTType *astType, u32 givenTypeIdx,
		DynamicArray<u32, ThreadAllocator> *polymorphicTypes)
{
	// First these that don't need the type info
	if (astType->nodeType == ASTTYPENODETYPE_IDENTIFIER)
		return;
	if (astType->nodeType == ASTTYPENODETYPE_TYPE_ARGUMENT_DECLARATION) {
		if (polymorphicTypes->size == 0)
			DynamicArrayInit(polymorphicTypes, 2);
		*DynamicArrayAdd(polymorphicTypes) = InferType(givenTypeIdx);
		return;
	}

	TypeInfo typeInfo = GetTypeInfo(givenTypeIdx);

	switch (astType->nodeType) {
	case ASTTYPENODETYPE_POINTER:
		ASSERT(typeInfo.typeCategory == TYPECATEGORY_POINTER);
		TCExtractPolymorphicTypes(astType->pointedType, typeInfo.pointerInfo.pointedTypeTableIdx,
				polymorphicTypes);
		break;
	case ASTTYPENODETYPE_STRUCT_DECLARATION:
	case ASTTYPENODETYPE_UNION_DECLARATION:
	{
		ASSERT(typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			   typeInfo.typeCategory == TYPECATEGORY_UNION);
		u64 memberCount = astType->structDeclaration.members.size;
		ASSERT(typeInfo.structInfo.members.size == memberCount);
		for (int memberIdx = 0; memberIdx < memberCount; ++memberIdx) {
			ASTStructMemberDeclaration *astMember = &astType->structDeclaration.members[memberIdx];
			u32 memberTypeIdx = typeInfo.structInfo.members[memberIdx].typeTableIdx;
			TCExtractPolymorphicTypes(astMember->astType, memberTypeIdx, polymorphicTypes);
		}
	} break;
	case ASTTYPENODETYPE_ENUM_DECLARATION:
		ASSERT(typeInfo.typeCategory == TYPECATEGORY_ENUM);
		TCExtractPolymorphicTypes(astType->enumDeclaration.astType,
				typeInfo.enumInfo.typeTableIdx, polymorphicTypes);
		break;
	case ASTTYPENODETYPE_ARRAY:
		ASSERT(typeInfo.typeCategory == TYPECATEGORY_ARRAY);
		TCExtractPolymorphicTypes(astType->arrayType, typeInfo.arrayInfo.elementTypeTableIdx,
				polymorphicTypes);
		break;
	case ASTTYPENODETYPE_PROCEDURE:
	{
		ASSERT(typeInfo.typeCategory == TYPECATEGORY_PROCEDURE);
		ArrayView<ASTProcedureParameter> astParameters = astType->procedurePrototype.astParameters;
		ASSERT(astParameters.size == typeInfo.procedureInfo.parameters.size);
		for (int paramIdx = 0; paramIdx < astParameters.size; ++paramIdx) {
			ASTProcedureParameter *astParam = &astParameters[paramIdx];
			u32 paramTypeIdx = typeInfo.procedureInfo.parameters[paramIdx].typeTableIdx;
			TCExtractPolymorphicTypes(astParam->astType, paramTypeIdx, polymorphicTypes);
		}
	} break;
	default:
		ASSERT(false);
	}
}

void GenerateTypeCheckJobs(ASTExpression *expression);
void TypeCheckExpression(TCContext *tcContext, ASTExpression *expression)
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
		TCPushScope(tcContext);

		for (int i = 0; i < astBlock->statements.size; ++i)
			TypeCheckExpression(tcContext, &astBlock->statements[i]);

		TCPopScope(tcContext);
	} break;
	case ASTNODETYPE_MULTIPLE_EXPRESSIONS:
	{
		ASTMultipleExpressions *astME = &expression->multipleExpressions;
		for (int i = 0; i < astME->array.size; ++i)
			TypeCheckExpression(tcContext, astME->array[i]);

	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration *varDecl = &expression->variableDeclaration;
		TypeCheckVariableDeclaration(tcContext, varDecl);

		if (tcContext->onStaticContext) {
			IRJobArgs *args = ALLOC(LinearAllocator, IRJobArgs);
			*args = {
				.procedureIdx = 0,
				.localValues = {},
				.expression = expression
			};
			RequestNewJob(JOBTYPE_CODEGEN, IRJobExpression, (void *)args);
		}
	} break;
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		ASTStaticDefinition *astStaticDef = &expression->staticDefinition;

		switch (astStaticDef->expression->nodeType) {
		case ASTNODETYPE_PROCEDURE_DECLARATION:
		{
			// Set up a workspace scope, we'll add things like polymorphic types to it.
			TCPushScope(tcContext);

			ASTProcedureDeclaration *astProcDecl = &astStaticDef->expression->procedureDeclaration;
			ASTProcedurePrototype *astPrototype = &astProcDecl->prototype;

			TCAddPlaceholderTypesForProcedurePrototype(tcContext, astPrototype);
			TypeCheckProcedurePrototype(tcContext, astPrototype);
			TypeInfo t = TypeInfoFromASTProcedurePrototype(astPrototype);

			bool isPolymorphic = false;
			for (int i = 0; i < t.procedureInfo.parameters.size; ++i) {
				if (t.procedureInfo.parameters[i].isPolymorphic) {
					isPolymorphic = true;
					break;
				}
			}

			u32 typeTableIdx = FindOrAddTypeTableIdx(t);
			astStaticDef->expression->typeTableIdx = typeTableIdx;
			expression->typeTableIdx = typeTableIdx;

			Procedure procedure = {};
			procedure.typeTableIdx = typeTableIdx;
			procedure.name = astProcDecl->name;
			procedure.astBody = astProcDecl->astBody;
			procedure.isInline = astProcDecl->isInline;
			procedure.isExported = astProcDecl->isExported;
			procedure.astPrototype = astProcDecl->prototype;
			DynamicArrayInit(&procedure.parameterValues, 4);
			u32 procedureIdx = NewProcedure(procedure, astProcDecl->isExternal);
			astProcDecl->procedureIdx = procedureIdx;

			StaticDefinition newStaticDef = {};
			newStaticDef.typeTableIdx = typeTableIdx;
			newStaticDef.name = *GetVariableName(astStaticDef, 0);
			newStaticDef.definitionType = STATICDEFINITIONTYPE_PROCEDURE;
			newStaticDef.procedureIdx = procedureIdx;

			u32 newStaticDefIdx = NewStaticDefinition(&newStaticDef);
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
				TCAddScopeNames(tcContext, scopeNamesToAdd);
			}

			if (procedure.astBody) {
				// Check all paths return
				if (t.procedureInfo.returnTypeIndices.size) {
					ReturnCheckResult result = TCCheckIfReturnsValue(procedure.astBody);
					if (result == RETURNCHECKRESULT_SOMETIMES)
						LogError(expression->any.loc, "Procedure doesn't always return a value"_s);
					else if (result == RETURNCHECKRESULT_NEVER)
						LogError(expression->any.loc, "Procedure has to return a value"_s);
				}
			}

			if (procedure.astBody && !isPolymorphic) {
				TCContext procedureContext = *tcContext;
				BucketArrayInit(&procedureContext.localValues);
				*BucketArrayAdd(&procedureContext.localValues) = {}; // No value number 0?
				DynamicArrayInit(&procedureContext.scopeStack, 8);

				TCPushScope(&procedureContext);

				procedureContext.onStaticContext = false;

				// Copy top-most scope to this context (our workspace scope)
				{
					TCScope *topScope = TCGetTopMostScope(tcContext);
					TCAddScopeNames(&procedureContext, topScope->names);
				}

				// Parameters
				ArrayView<ASTProcedureParameter> astParameters = procedure.astPrototype.astParameters;
				for (int i = 0; i < astParameters.size; ++i) {
					const ASTProcedureParameter *astParameter = &astParameters[i];
					u32 paramValueIdx = TCNewValue(&procedureContext, astParameter->name, astParameter->typeTableIdx, 0);
					*DynamicArrayAdd(&procedure.parameterValues) = paramValueIdx;
				}
				// Varargs array
				if (astPrototype->isVarargs) {
					static u32 arrayTableIdx = GetTypeInfoArrayOf(TYPETABLEIDX_ANY_STRUCT, 0);

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

				procedure.isBodyTypeChecked = true;

				UpdateProcedure(procedureIdx, &procedure);

				// Wake up any jobs that were waiting for this procedure body
				WakeUpAllByIndex(YIELDREASON_PROC_BODY_NOT_READY, procedureIdx);

				TCPopScope(&procedureContext);

				// Code gen!
				{
					IRJobArgs *args = ALLOC(LinearAllocator, IRJobArgs);
					*args = {
						.procedureIdx = procedureIdx,
						.localValues = procedureContext.localValues,
						.expression = nullptr
					};
					RequestNewJob(JOBTYPE_CODEGEN, IRJobProcedure, (void *)args);
				}
			}
			TCPopScope(tcContext);

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
				TCAddScopeNames(tcContext, scopeNamesToAdd);
			}
		} break;
		case ASTNODETYPE_TYPE:
		case ASTNODETYPE_ALIAS:
		{
			StaticDefinition newStaticDef = {};
			newStaticDef.typeTableIdx = TYPETABLEIDX_Unset;
			newStaticDef.name = *GetVariableName(astStaticDef, 0);
			newStaticDef.definitionType = STATICDEFINITIONTYPE_TYPE;

			u32 newStaticDefIdx = NewStaticDefinition(&newStaticDef);
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
				TCAddScopeNames(tcContext, scopeNamesToAdd);
			}

			u32 result = TypeCheckType(tcContext, *GetVariableName(astStaticDef, 0),
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
					.size = GetTypeInfo(result).size,
					.aliasInfo = {
						.name = *GetVariableName(astStaticDef, 0),
						.aliasedTypeIdx = result,
						.doesImplicitlyCast = astStaticDef->expression->nodeType == ASTNODETYPE_ALIAS
					}
				};
				newTypeIdx = FindOrAddTypeTableIdx(t);
			}

			newStaticDef.typeTableIdx = newTypeIdx;
			expression->typeTableIdx = newTypeIdx;
			UpdateStaticDefinition(newStaticDefIdx, &newStaticDef);
		} break;
		default:
		{
			TypeCheckExpression(tcContext, astStaticDef->expression);

			StaticDefinition newStaticDef;

			if (astStaticDef->expression->nodeType == ASTNODETYPE_IDENTIFIER &&
				astStaticDef->expression->identifier.type == NAMETYPE_STATIC_DEFINITION)
			{
				ASSERT(astStaticDef->expression->typeTableIdx != TYPETABLEIDX_Unset);
				u32 identifierStaticDefIdx = astStaticDef->expression->identifier.staticDefinitionIdx;
				newStaticDef = GetStaticDefinition(identifierStaticDefIdx, true);
				newStaticDef.name = *GetVariableName(astStaticDef, 0);
				newStaticDef.typeTableIdx = astStaticDef->expression->typeTableIdx;
				expression->typeTableIdx = astStaticDef->expression->typeTableIdx;
			}
			else {
				Constant constant = TryEvaluateConstant(astStaticDef->expression);

				if (constant.type == CONSTANTTYPE_INVALID)
					LogError(astStaticDef->expression->any.loc,
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

			u32 newStaticDefIdx = NewStaticDefinition(&newStaticDef);
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
				TCAddScopeNames(tcContext, scopeNamesToAdd);
			}
		} break;
		}
	} break;
	case ASTNODETYPE_RETURN:
	{
		ASTExpression **providedReturnValues = nullptr;
		u64 providedReturnValuesCount = 0;
		u64 requiredReturnValuesCount = tcContext->currentReturnTypes.size;

		ASTExpression *returnExp = expression->returnNode.expression;
		if (returnExp != nullptr) {
			TypeCheckExpression(tcContext, returnExp);

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
			LogError(returnExp->any.loc, TPrintF("Returning wrong amount of "
					"values: %d required but %d were provided", requiredReturnValuesCount,
					providedReturnValuesCount));

		for (int i = 0; i < providedReturnValuesCount; ++i) {
			ASTExpression *currentExp = providedReturnValues[i];
			if (IsExpressionAType(currentExp))
				LogError(currentExp->any.loc, "Trying to return a type"_s);

			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(
					tcContext->currentReturnTypes[i], currentExp);
			currentExp->typeTableIdx = typeCheckResult.rightTypeIdx;
			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				ReportTypeCheckError(typeCheckResult.errorCode, currentExp->any.loc,
						typeCheckResult.rightTypeIdx, tcContext->currentReturnTypes[i]);
			}
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		TypeCheckExpression(tcContext, expression->deferNode.expression);
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		String string = expression->identifier.string;

		TCScopeName scopeName = TCFindScopeName(tcContext, expression->any.loc, string);
		ASSERT(scopeName.type != NAMETYPE_INVALID);

		expression->identifier.type = scopeName.type;
		switch (scopeName.type)
		{
		case NAMETYPE_VARIABLE:
		{
			expression->identifier.valueIdx = scopeName.variableInfo.valueIdx;

			u32 variableTypeIdx = scopeName.variableInfo.typeTableIdx;
			if (variableTypeIdx == TYPETABLEIDX_Unset)
				LogError(expression->any.loc, TPrintF("COMPILER ERROR! Variable "
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
			StaticDefinition staticDefinition = GetStaticDefinition(scopeName.staticDefinitionIdx,
					true);
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
		TypeCheckExpression(tcContext, usingExp);

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
			TCAddStructMembersToScope(tcContext, usingExp->any.loc, varExp);
		}
		else
			TCAddStructMembersToScope(tcContext, usingExp->any.loc, usingExp);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall *astProcCall = &expression->procedureCall;

		ProcedureCallType callType = CALLTYPE_ASTEXPRESSION;
		u32 procedureIdx = U32_MAX;

		ASTExpression *astProcExp = astProcCall->procedureExpression;
		TypeCheckExpression(tcContext, astProcExp);
		u32 procedureTypeIdx = astProcExp->typeTableIdx;

		if (astProcExp->nodeType == ASTNODETYPE_IDENTIFIER) {
			String procName = astProcExp->identifier.string;
			TCScopeName scopeName = TCFindScopeName(tcContext, astProcCall->loc, procName);
			if (scopeName.type == NAMETYPE_STATIC_DEFINITION) {
				callType = CALLTYPE_STATIC;
				StaticDefinition staticDefinition = GetStaticDefinition(
						scopeName.staticDefinitionIdx, false);

				if (staticDefinition.definitionType != STATICDEFINITIONTYPE_PROCEDURE)
					LogError(expression->any.loc, "Calling a non-procedure"_s);

				procedureIdx = staticDefinition.procedureIdx;
				Procedure proc = GetProcedureRead(procedureIdx);
				procedureTypeIdx = proc.typeTableIdx;
			}
		}

		if (procedureTypeIdx == TYPETABLEIDX_Unset)
			LogCompilerError(astProcExp->any.loc, TPrintF("Procedure \"%S\" not type "
						"checked", GetProcedureRead(procedureIdx).name));
		ASSERT(GetTypeInfo(procedureTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);

		s64 givenArguments = astProcCall->arguments.size;
		for (int argIdx = 0; argIdx < givenArguments; ++argIdx) {
			ASTExpression *arg = astProcCall->arguments[argIdx];
			TypeCheckExpression(tcContext, arg);
		}

		String errorProcedureName = {};
		if (astProcExp->nodeType == ASTNODETYPE_IDENTIFIER)
			errorProcedureName = TPrintF(" \"%S\"", astProcExp->identifier.string);

		ASSERT(GetTypeInfo(procedureTypeIdx).typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = GetTypeInfo(procedureTypeIdx).procedureInfo;
		s64 totalArguments = procTypeInfo.parameters.size;

		astProcCall->callType = callType;
		astProcCall->procedureIdx = procedureIdx;
		astProcCall->procedureTypeIdx = procedureTypeIdx;

		// Type check arguments
		s64 requiredArguments = 0;
		for (int i = 0; i < procTypeInfo.parameters.size; ++i) {
			if (procTypeInfo.parameters[i].defaultValue.type == CONSTANTTYPE_INVALID)
				++requiredArguments;
		}

		if (procTypeInfo.isVarargs) {
			if (requiredArguments > givenArguments)
				LogError(astProcExp->any.loc,
						TPrintF("Procedure%S needs at least %d arguments but only %d were given",
							errorProcedureName, requiredArguments, givenArguments));
		}
		else {
			if (requiredArguments > givenArguments)
				LogError(astProcExp->any.loc,
						TPrintF("Procedure%S needs at least %d arguments but only %d were given",
						errorProcedureName, requiredArguments, givenArguments));

			if (givenArguments > totalArguments)
				LogError(astProcExp->any.loc,
						TPrintF("Procedure%S needs %d arguments but %d were given",
						errorProcedureName, totalArguments, givenArguments));
		}

		s64 argsToCheck = Min(givenArguments, totalArguments);
		for (int argIdx = 0; argIdx < argsToCheck; ++argIdx) {
			ASTExpression *arg = astProcCall->arguments[argIdx];
			u32 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(paramTypeIdx, arg);
			arg->typeTableIdx = typeCheckResult.rightTypeIdx;

			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				String paramName = {};
				if (callType == CALLTYPE_STATIC)
					paramName = TPrintF(" \"%S\"",
							GetProcedureRead(procedureIdx).astPrototype.astParameters[argIdx].name);
				String paramStr = TypeInfoToString(paramTypeIdx);
				String givenStr = TypeInfoToString(arg->typeTableIdx);
				LogError(arg->any.loc, TPrintF("When calling procedure%S: type of "
							"parameter %d%S didn't match (parameter is %S but %S was given)",
							errorProcedureName, argIdx+1, paramName, paramStr, givenStr));
			}
		}

		if (callType == CALLTYPE_STATIC) {
			DynamicArray<u32, ThreadAllocator> polymorphicTypes = {};

			Procedure proc = GetProcedureRead(procedureIdx);
			s64 argsToCheck_ = Min(givenArguments, totalArguments);
			for (int argIdx = 0; argIdx < argsToCheck_; ++argIdx) {
				ASTExpression *arg = astProcCall->arguments[argIdx];
				TCExtractPolymorphicTypes(proc.astPrototype.astParameters[argIdx].astType,
						arg->typeTableIdx, &polymorphicTypes);
			}

			if (polymorphicTypes.size) {
				u32 newProcIdx = TCInstantiateProcedure(tcContext, procedureIdx, polymorphicTypes);
				astProcCall->procedureIdx = newProcIdx;
				astProcCall->procedureTypeIdx = GetProcedureRead(newProcIdx).typeTableIdx;
				procTypeInfo = GetTypeInfo(astProcCall->procedureTypeIdx).procedureInfo;

				// Re-check argument types
				for (int argIdx = 0; argIdx < argsToCheck; ++argIdx) {
					ASTExpression *arg = astProcCall->arguments[argIdx];
					u32 paramTypeIdx = procTypeInfo.parameters[argIdx].typeTableIdx;
					TypeCheckErrorCode typeCheckError = CheckTypesMatch(paramTypeIdx, arg->typeTableIdx);

					if (typeCheckError != TYPECHECK_COOL) {
						String paramName = TPrintF(" \"%S\"",
									GetProcedureRead(procedureIdx).astPrototype.astParameters[argIdx].name);
						String paramStr = TypeInfoToString(paramTypeIdx);
						String givenStr = TypeInfoToString(arg->typeTableIdx);
						LogErrorNoCrash(arg->any.loc, TPrintF("When calling procedure%S: type of "
									"parameter %d%S didn't match (parameter is %S but %S was given)",
									errorProcedureName, argIdx+1, paramName, paramStr, givenStr));

						String procName = GetProcedureRead(procedureIdx).name;
						String procTypeStr = TypeInfoToString(astProcCall->procedureTypeIdx);
						LogNote(astProcCall->loc, TPrintF("Calling polymorphic procedure instance: %S%S",
									procName, procTypeStr));

						PANIC;
					}
				}
			}
			else {
				if ((proc.isInline || astProcCall->inlineType == CALLINLINETYPE_ALWAYS_INLINE) &&
						astProcCall->inlineType != CALLINLINETYPE_NEVER_INLINE) {

					TCCheckForCyclicInlineCalls(astProcExp->any.loc,
							tcContext->currentProcedureIdx, procedureIdx);

					// We need the whole body type checked
					if (!proc.isBodyTypeChecked) {
						auto procedures = g_context->procedures.GetForRead();
						proc = procedures[procedureIdx];
						if (!proc.isBodyTypeChecked) {
							SwitchJob(YIELDREASON_PROC_BODY_NOT_READY, { .index = procedureIdx });
							// Lock again!
							SYSLockForRead(&g_context->procedures.rwLock);
							proc = procedures[procedureIdx];
							if (!proc.isBodyTypeChecked)
								LogCompilerError(expression->any.loc, "Bad job resume"_s);
						}
					}
				}
			}
		}

		// Infer the rest (collapse _NUMBER and _FLOATING)
		for (s64 argIdx = argsToCheck; argIdx < givenArguments; ++argIdx) {
			ASTExpression *arg = astProcCall->arguments[argIdx];
			u32 inferred = InferType(arg->typeTableIdx);
			if (inferred != arg->typeTableIdx)
				InferTypesInExpression(arg, inferred);
		}

		if (procTypeInfo.returnTypeIndices.size == 0)
			expression->typeTableIdx = TYPETABLEIDX_VOID;
		else if (procTypeInfo.returnTypeIndices.size == 1)
			expression->typeTableIdx = procTypeInfo.returnTypeIndices[0];

		TCPushParametersAndInlineProcedureCall(tcContext, astProcCall);
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		ASTExpression *input = expression->unaryOperation.expression;
		TypeCheckExpression(tcContext, input);

		if (IsExpressionAType(input))
			LogError(input->any.loc, "Input of unary operator is a type"_s);

		if (TCIsPrimitiveOperation(expression->unaryOperation.op, input->typeTableIdx)) {
			u32 expressionType = input->typeTableIdx;
			switch (expression->unaryOperation.op) {
			case TOKEN_OP_NOT:
			{
				TypeCheckErrorCode typeCheckResult = CheckTypesMatch(TYPETABLEIDX_BOOL,
						expressionType);
				if (typeCheckResult != TYPECHECK_COOL)
					LogError(expression->any.loc, "Expression can't be cast to boolean"_s);
				expression->typeTableIdx = TYPETABLEIDX_BOOL;
			} break;
			case TOKEN_OP_POINTER_TO:
			{
				// Forbid pointer to temporal values
				if (IsTemporalValue(input))
					LogError(expression->any.loc, "Trying to get pointer to temporal value"_s);

				ASTExpression *e = input;
				switch (e->nodeType) {
				case ASTNODETYPE_IDENTIFIER:
				{
					if (e->identifier.type == NAMETYPE_VARIABLE)
						TCSetValueFlags(tcContext, e->identifier.valueIdx, VALUEFLAGS_FORCE_MEMORY);
				} break;
				}

				expression->typeTableIdx = GetTypeInfoPointerOf(expressionType);
			} break;
			case TOKEN_OP_DEREFERENCE:
			{
				TypeInfo expressionTypeInfo = GetTypeInfo(expressionType);
				if (expressionTypeInfo.typeCategory != TYPECATEGORY_POINTER)
					LogError(expression->any.loc, "Trying to dereference a non pointer"_s);
				expression->typeTableIdx = expressionTypeInfo.pointerInfo.pointedTypeTableIdx;
			} break;
			default:
				expression->typeTableIdx = expressionType;
			};
		}
		else {
			bool found = TCLookForOperatorOverload(tcContext, expression);
			ASSERT(found);
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;
		ASTExpression *rightHand = expression->binaryOperation.rightHand;

		if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS) {
			TypeCheckExpression(tcContext, leftHand);

			if (IsExpressionAType(leftHand))
				LogError(leftHand->any.loc, "Left hand of member access operator is a type"_s);

			u32 leftHandTypeIdx = leftHand->typeTableIdx;

			if (rightHand->nodeType != ASTNODETYPE_IDENTIFIER)
				LogError(rightHand->any.loc, "Expected identifier after member access operator"_s);

			rightHand->identifier.type = NAMETYPE_STRUCT_MEMBER;

			// Get rid of aliases
			u32 structTypeIdx = StripImplicitlyCastAliases(leftHandTypeIdx);

			TypeInfo structTypeInfo = GetTypeInfo(structTypeIdx);

			if (structTypeInfo.typeCategory == TYPECATEGORY_POINTER) {
				u32 pointedTypeIdx = structTypeInfo.pointerInfo.pointedTypeTableIdx;
				structTypeInfo = GetTypeInfo(pointedTypeIdx);
			}

			if (structTypeInfo.typeCategory == TYPECATEGORY_ARRAY) {
				// This is only for dynamic size arrays!
				if (structTypeInfo.arrayInfo.count != 0)
					LogError(expression->any.loc, "Array left of '.' has to be of dynamic size! ([])"_s);

				structTypeInfo = GetTypeInfo(TYPETABLEIDX_ARRAY_STRUCT);
			}
			else if (structTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
					 structTypeInfo.typeCategory != TYPECATEGORY_UNION) {
				LogError(expression->any.loc, "Left of '.' has to be a struct/union"_s);
			}

			String memberName = rightHand->identifier.string;
			const StructMember *foundMember = FindStructMemberByName(structTypeInfo, memberName);
			if (foundMember) {
				rightHand->identifier.structMember = foundMember;
				expression->typeTableIdx = foundMember->typeTableIdx;
			}
			else
				LogError(expression->any.loc, TPrintF("\"%S\" is not a member of \"%S\"",
						memberName, TypeInfoToString(structTypeIdx)));
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS) {
			TypeCheckExpression(tcContext, leftHand);
			TypeCheckExpression(tcContext, rightHand);

			if (IsExpressionAType(leftHand))
				LogError(leftHand->any.loc, "Input of array access is a type"_s);
			if (IsExpressionAType(rightHand))
				LogError(rightHand->any.loc, "Index of array access is a type"_s);

			u32 arrayType = leftHand->typeTableIdx;
			TypeInfo arrayTypeInfo = GetTypeInfo(arrayType);
			if (arrayTypeInfo.typeCategory == TYPECATEGORY_POINTER) {
				u32 pointedTypeIdx = arrayTypeInfo.pointerInfo.pointedTypeTableIdx;
				arrayType = pointedTypeIdx;
				arrayTypeInfo = GetTypeInfo(pointedTypeIdx);
			}

			if (arrayType == TYPETABLEIDX_STRING_STRUCT)
				expression->typeTableIdx = TYPETABLEIDX_U8;
			else {
				if (arrayTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
					LogError(leftHand->any.loc,
							"Expression does not evaluate to an array"_s);
				expression->typeTableIdx = arrayTypeInfo.arrayInfo.elementTypeTableIdx;
			}
		}
		else if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT) {
			TypeCheckExpression(tcContext, leftHand);
			TypeCheckExpression(tcContext, rightHand);

			if (IsExpressionAType(leftHand))
				LogError(leftHand->any.loc, "Left hand of binary operator is a type"_s);
			if (IsExpressionAType(rightHand))
				LogError(rightHand->any.loc, "Right hand of binary operator is a type"_s);

			if (leftHand->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS) {
				u64 leftHandCount = leftHand->multipleExpressions.array.size;

				if (rightHand->nodeType == ASTNODETYPE_PROCEDURE_CALL) {
					// Check all left hand values against all return value types on the called
					// procedure.
					u32 procTypeIdx = rightHand->procedureCall.procedureTypeIdx;
					ArrayView<u32> returnTypeIndices =
						GetTypeInfo(procTypeIdx).procedureInfo.returnTypeIndices;
					if (leftHandCount != returnTypeIndices.size)
						LogError(expression->any.loc, TPrintF("Left hand expression has %d "
									"values, but right hand has %d", leftHandCount, returnTypeIndices.size));

					for (int i = 0; i < leftHandCount; ++i) {
						TypeCheckResult tcResult = CheckTypesMatchAndSpecialize(returnTypeIndices[i],
								leftHand->multipleExpressions.array[i]);
						if (tcResult.errorCode != TYPECHECK_COOL) {
							String leftStr  = TypeInfoToString(leftHand->multipleExpressions.array[i]->typeTableIdx);
							String rightStr = TypeInfoToString(returnTypeIndices[i]);
							LogError(expression->any.loc, TPrintF("Type mismatch on input "
										"number %d of operator %S (left hand is \"%S\" and right hand is \"%S\")",
										i, OperatorToString(expression->binaryOperation.op), leftStr, rightStr));
						}
						leftHand->multipleExpressions.array[i]->typeTableIdx = tcResult.leftTypeIdx;
					}
				}
				else {
					// Check both sides' expressions against each other.
					if (rightHand->nodeType != ASTNODETYPE_MULTIPLE_EXPRESSIONS)
						LogError(expression->any.loc, TPrintF("Left hand expression has %d "
									"values, but right hand has 1", leftHandCount));

					u64 rightHandCount = rightHand->multipleExpressions.array.size;
					if (leftHandCount != rightHandCount)
						LogError(expression->any.loc, TPrintF("Left hand expression has %d "
									"values, but right hand has %d", leftHandCount, rightHandCount));

					for (int i = 0; i < leftHandCount; ++i) {
						TypeCheckResult typeCheckResult =
							CheckTypesMatchAndSpecialize(leftHand->multipleExpressions.array[i]->typeTableIdx,
								rightHand->multipleExpressions.array[i]);
						leftHand->multipleExpressions.array[i]->typeTableIdx  = typeCheckResult.leftTypeIdx;
						rightHand->multipleExpressions.array[i]->typeTableIdx = typeCheckResult.rightTypeIdx;

						if (typeCheckResult.errorCode != TYPECHECK_COOL)
						{
							String leftStr =  TypeInfoToString(typeCheckResult.leftTypeIdx);
							String rightStr = TypeInfoToString(typeCheckResult.rightTypeIdx);
							LogError(expression->any.loc, TPrintF("Type mismatch on input "
										"number %d of operator %S (left hand is \"%S\" and right hand is \"%S\")",
										i, OperatorToString(expression->binaryOperation.op), leftStr, rightStr));
						}
					}
				}
			}
			else {
				if (rightHand->nodeType == ASTNODETYPE_MULTIPLE_EXPRESSIONS)
					LogError(expression->any.loc, TPrintF("Left hand expression has 1 "
								"value, but right hand has %d",
								rightHand->multipleExpressions.array.size));

				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(
						leftHand->typeTableIdx, rightHand);
				leftHand->typeTableIdx  = typeCheckResult.leftTypeIdx;
				rightHand->typeTableIdx = typeCheckResult.rightTypeIdx;

				if (typeCheckResult.errorCode != TYPECHECK_COOL) {
					String leftStr  = TypeInfoToString(leftHand->typeTableIdx);
					String rightStr = TypeInfoToString(rightHand->typeTableIdx);
					LogError(expression->any.loc, TPrintF("Type mismatch on inputs of "
								"operator %S (left hand is \"%S\" and right hand is \"%S\")",
								OperatorToString(expression->binaryOperation.op),
								leftStr, rightStr));
				}
			}
		}
		else {
			TypeCheckExpression(tcContext, leftHand);
			TypeCheckExpression(tcContext, rightHand);

			if (IsExpressionAType(leftHand))
				LogError(leftHand->any.loc, "Left hand of binary operator is a type"_s);
			if (IsExpressionAType(rightHand))
				LogError(rightHand->any.loc, "Right hand of binary operator is a type"_s);

			if (TCIsPrimitiveOperation(expression->binaryOperation.op,
						leftHand->typeTableIdx, rightHand->typeTableIdx)) {
				TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(leftHand->typeTableIdx,
						rightHand);
				if (leftHand->typeTableIdx != typeCheckResult.leftTypeIdx)
					InferTypesInExpression(leftHand, typeCheckResult.leftTypeIdx);

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
						String leftStr  = TypeInfoToString(leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(rightHand->typeTableIdx);
						LogError(expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}
				} break;
				case TOKEN_OP_RANGE:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL) {
						String leftStr =  TypeInfoToString(leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(rightHand->typeTableIdx);
						LogError(expression->any.loc, TPrintF("Invalid types on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					// Both operands have to be integers
					TypeCategory leftCat = GetTypeInfo(
							StripAllAliases(leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER)
						LogError(leftHand->any.loc, TPrintF("Left hand of .. operator "
									"does not evaluate to an integer (%S)",
									TypeInfoToString(leftHand->typeTableIdx)));

					TypeCategory rightCat = GetTypeInfo(
							StripAllAliases(rightHand->typeTableIdx)).typeCategory;
					if (rightCat != TYPECATEGORY_INTEGER)
						LogError(rightHand->any.loc, TPrintF("Right hand of .. operator "
									"does not evaluate to an integer (%S)",
									TypeInfoToString(typeCheckResult.rightTypeIdx)));
				} break;
				case TOKEN_OP_ASSIGNMENT:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						String leftStr =  TypeInfoToString(leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(rightHand->typeTableIdx);
						LogError(expression->any.loc, TPrintF("Type mismatch on inputs of "
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
					if (typeCheckResult.errorCode != TYPECHECK_COOL) {
						String leftStr =  TypeInfoToString(leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(rightHand->typeTableIdx);
						LogError(expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					TypeCategory leftCat  = GetTypeInfo(
							StripAllAliases(leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER &&
						leftCat != TYPECATEGORY_FLOATING &&
						leftCat != TYPECATEGORY_ENUM &&
						leftCat != TYPECATEGORY_POINTER)
						LogError(expression->any.loc, TPrintF("Invalid types on inputs of "
									"operator %S (left hand is \"%S\"",
									OperatorToString(expression->binaryOperation.op),
									TypeInfoToString(leftHand->typeTableIdx)));
				} break;
				default:
				{
					if (typeCheckResult.errorCode != TYPECHECK_COOL)
					{
						String leftStr =  TypeInfoToString(leftHand->typeTableIdx);
						String rightStr = TypeInfoToString(rightHand->typeTableIdx);
						LogError(expression->any.loc, TPrintF("Type mismatch on inputs of "
									"operator %S (left hand is \"%S\" and right hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									leftStr, rightStr));
					}

					TypeCategory leftCat  = GetTypeInfo(
							StripAllAliases(leftHand->typeTableIdx)).typeCategory;
					if (leftCat != TYPECATEGORY_INTEGER &&
						leftCat != TYPECATEGORY_FLOATING)
						LogError(expression->any.loc, TPrintF("Invalid types on inputs of "
									"operator %S (left hand is \"%S\")",
									OperatorToString(expression->binaryOperation.op),
									TypeInfoToString(leftHand->typeTableIdx)));
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
				bool found = TCLookForOperatorOverload(tcContext, expression);
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
						LogError(leftExp->any.loc, "Expected identifier before '='"_s);

					TypeCheckExpression(tcContext, rightExp);
				}
				else
					TypeCheckExpression(tcContext, memberExp);
			}
			expression->typeTableIdx = TYPETABLEIDX_StructLiteral;
			break;
		case LITERALTYPE_CSTR:
			expression->typeTableIdx = TYPETABLEIDX_S8_PTR;
			break;
		default:
			ASSERT(!"Unexpected literal type");
		}
	} break;
	case ASTNODETYPE_IF:
	{
		TypeCheckExpression(tcContext, expression->ifNode.condition);

		u32 conditionType = expression->ifNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(TYPETABLEIDX_BOOL, conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(tcContext, expression->ifNode.body);

		if (expression->ifNode.elseBody)
			TypeCheckExpression(tcContext, expression->ifNode.elseBody);
	} break;
	case ASTNODETYPE_IF_STATIC:
	{
		TypeCheckExpression(tcContext, expression->ifStaticNode.condition);

		u32 conditionType = expression->ifStaticNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(TYPETABLEIDX_BOOL, conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(expression->any.loc, "If condition doesn't evaluate to a boolean"_s);

		Constant conditionResult = TryEvaluateConstant(expression->ifStaticNode.condition);
		if (conditionResult.type == CONSTANTTYPE_INVALID)
			LogError(expression->ifStaticNode.condition->any.loc,
					"Failed to evaluate static if condition"_s);

		bool evaluatesToTrue = conditionResult.valueAsInt != 0;
		expression->ifStaticNode.evaluatesToTrue = evaluatesToTrue;

		if (evaluatesToTrue)
			TypeCheckExpression(tcContext, expression->ifStaticNode.body);

		else if (expression->ifStaticNode.elseBody)
			TypeCheckExpression(tcContext, expression->ifStaticNode.elseBody);
	} break;
	case ASTNODETYPE_WHILE:
	{
		TypeCheckExpression(tcContext, expression->whileNode.condition);

		u32 conditionType = expression->whileNode.condition->typeTableIdx;
		TypeCheckErrorCode typeCheckResult = CheckTypesMatch(TYPETABLEIDX_BOOL, conditionType);
		if (typeCheckResult != TYPECHECK_COOL)
			LogError(expression->any.loc, "While condition doesn't evaluate to a boolean"_s);

		TypeCheckExpression(tcContext, expression->whileNode.body);
	} break;
	case ASTNODETYPE_FOR:
	{
		ASTFor *astFor = &expression->forNode;

		ASTExpression *rangeExp = astFor->range;
		bool isExplicitRange = rangeExp->nodeType == ASTNODETYPE_BINARY_OPERATION &&
			rangeExp->binaryOperation.op == TOKEN_OP_RANGE;

		TypeCheckExpression(tcContext, rangeExp);

		TCPushScope(tcContext);

		u32 indexTypeIdx = TYPETABLEIDX_S64;
		if (isExplicitRange) {
			// Infer if it's literal numbers
			InferTypesInExpression(rangeExp, indexTypeIdx);
			indexTypeIdx = rangeExp->typeTableIdx;
		}

		u32 indexValueIdx = TCNewValue(tcContext, astFor->indexVariableName, indexTypeIdx, 0);
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
				TypeInfo rangeTypeInfo = GetTypeInfo(rangeExp->typeTableIdx);
				if (rangeTypeInfo.typeCategory == TYPECATEGORY_POINTER)
					rangeTypeInfo = GetTypeInfo(rangeTypeInfo.pointerInfo.pointedTypeTableIdx);

				if (rangeTypeInfo.typeCategory != TYPECATEGORY_ARRAY)
					LogError(astFor->range->any.loc, "'for' range "
							"expression does not evaluate to an array nor is it a number range "
							"(..)"_s);
				elementTypeTableIdx = rangeTypeInfo.arrayInfo.elementTypeTableIdx;
			}

			u32 pointerToElementTypeIdx = GetTypeInfoPointerOf(elementTypeTableIdx);
			u32 elementValueIdx = TCNewValue(tcContext, astFor->itemVariableName,
					pointerToElementTypeIdx, 0);
			astFor->elementValueIdx = elementValueIdx;

			newScopeName.name = astFor->itemVariableName;
			newScopeName.variableInfo.valueIdx = elementValueIdx;
			newScopeName.variableInfo.typeTableIdx = pointerToElementTypeIdx;
			newScopeName.loc = expression->any.loc;
			*FixedArrayAdd(&scopeNamesToAdd) = newScopeName;
		}
		TCAddScopeNames(tcContext, scopeNamesToAdd);

		u32 oldForArray = tcContext->currentForLoopArrayType;
		tcContext->currentForLoopArrayType = astFor->range->typeTableIdx;

		TypeCheckExpression(tcContext, astFor->body);

		// Important to restore whether we yield or not!
		tcContext->currentForLoopArrayType = oldForArray;

		TCPopScope(tcContext);
	} break;
	case ASTNODETYPE_BREAK:
	case ASTNODETYPE_CONTINUE:
	{
	} break;
	case ASTNODETYPE_REMOVE:
	{
		TypeInfo forArrayType = GetTypeInfo(tcContext->currentForLoopArrayType);
		if (forArrayType.typeCategory != TYPECATEGORY_ARRAY || forArrayType.arrayInfo.count != 0)
			LogError(expression->any.loc, "'remove' found but there wasn't a for loop "
					"with a dynamic sized array as range"_s);
	} break;
	case ASTNODETYPE_TYPE:
	case ASTNODETYPE_ALIAS:
	{
		expression->typeTableIdx = TypeCheckType(tcContext, {}, expression->any.loc,
				&expression->astType);
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		TypeCheckExpression(tcContext, expression->typeOfNode.expression);

		static u32 typeInfoPointerTypeIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_PTR;
		expression->typeTableIdx = typeInfoPointerTypeIdx;
	} break;
	case ASTNODETYPE_SIZEOF:
	{
		TypeCheckExpression(tcContext, expression->sizeOfNode.expression);
		expression->typeTableIdx = TYPETABLEIDX_U64;
	} break;
	case ASTNODETYPE_CAST:
	{
		TypeCheckExpression(tcContext, expression->castNode.expression);

		u32 typeCheckResult = TypeCheckType(tcContext, {}, expression->any.loc,
				&expression->castNode.astType);

		TypeCheckResult typeSpecializeResult = CheckTypesMatchAndSpecialize(typeCheckResult,
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
				LogError(expression->any.loc, "Invalid compiler intrinsic"_s);
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
			LogError(expression->any.loc, "Too many arguments for intrinsic"_s);
		for (int argIdx = 0; argIdx < expression->intrinsic.arguments.size; ++argIdx) {
			ASTExpression *arg = &expression->intrinsic.arguments[argIdx];
			TypeCheckExpression(tcContext, arg);
			TypeCheckResult typeCheckResult = CheckTypesMatchAndSpecialize(argTypes[argIdx], arg);
			arg->typeTableIdx = typeCheckResult.rightTypeIdx;

			if (typeCheckResult.errorCode != TYPECHECK_COOL) {
				String paramStr = TypeInfoToString(argTypes[argIdx]);
				String givenStr = TypeInfoToString(arg->typeTableIdx);
				LogError(arg->any.loc, TPrintF("When calling intrinsic \"%S\": type of "
							"parameter #%d didn't match (parameter is %S but %S was given)",
							expression->intrinsic.name, argIdx, paramStr, givenStr));
			}
		}
		expression->typeTableIdx = TYPETABLEIDX_VOID;
	} break;
	case ASTNODETYPE_OPERATOR_OVERLOAD:
	{
		ASTOperatorOverload *astOverload = &expression->operatorOverload;

		TCContext procedureContext = *tcContext;
		BucketArrayInit(&procedureContext.localValues);
		*BucketArrayAdd(&procedureContext.localValues) = {}; // No value number 0?
		// Don't allocate a new array if old is empty.
		if (tcContext->scopeStack.size)
			DynamicArrayInit(&procedureContext.scopeStack, 8);
		procedureContext.onStaticContext = false;

		static u64 overloadUniqueId = 0;

		TypeCheckProcedurePrototype(&procedureContext, &astOverload->prototype);
		TypeInfo t = TypeInfoFromASTProcedurePrototype(&astOverload->prototype);

		u32 typeTableIdx = FindOrAddTypeTableIdx(t);

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
		overload.procedureIdx = NewProcedure(p, false);

		astOverload->procedureIdx = overload.procedureIdx;

		TCPushScope(&procedureContext);

		Procedure procedure = GetProcedureRead(astOverload->procedureIdx);

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
			LogError(expression->any.loc, TPrintF(
					"No parameters provided on overload for operator %S.",
					OperatorToString(astOverload->op)));
		}
		else if (paramCount == 1) {
			if (astOverload->op != TOKEN_OP_NOT &&
				astOverload->op != TOKEN_OP_BITWISE_NOT &&
				astOverload->op != TOKEN_OP_MINUS)
				LogError(expression->any.loc, TPrintF(
							"Only 1 parameter is present on overload for operator %S. "
							"Expected 2.", OperatorToString(astOverload->op)));

			// No overloading builtin operations
			u32 inputHandTypeIdx = astOverload->prototype.astParameters[0].typeTableIdx;
			if (TCIsPrimitiveOperation(astOverload->op, inputHandTypeIdx))
				LogError(expression->any.loc, "Overloading a vanilla operation is forbidden"_s);
		}
		else if (paramCount == 2) {
			if (astOverload->op == TOKEN_OP_NOT ||
				astOverload->op == TOKEN_OP_BITWISE_NOT)
				LogError(expression->any.loc, TPrintF(
							"2 parameters found on overload for operator %S. "
							"Expected 1.", OperatorToString(astOverload->op)));

			// No overloading builtin operations
			u32 leftHandTypeIdx  = astOverload->prototype.astParameters[0].typeTableIdx;
			u32 rightHandTypeIdx = astOverload->prototype.astParameters[1].typeTableIdx;
			if (TCIsPrimitiveOperation(astOverload->op, leftHandTypeIdx, rightHandTypeIdx))
				LogError(expression->any.loc, "Overloading a vanilla operation is forbidden"_s);
		}
		else {
			LogError(expression->any.loc, TPrintF(
					"Too many parameters provided on overload for operator %S.",
					OperatorToString(astOverload->op)));
		}

		{
			auto operatorOverloads = g_context->operatorOverloads.GetForWrite();
			*DynamicArrayAdd(&operatorOverloads) = overload;
		}

		// Wake up any job waiting for this overload.
		WakeUpAllByIndex(YIELDREASON_UNKNOWN_OVERLOAD, astOverload->op);
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

		UpdateProcedure(astOverload->procedureIdx, &procedure);
		// Wake up any jobs that were waiting for this procedure body
		WakeUpAllByIndex(YIELDREASON_PROC_BODY_NOT_READY, astOverload->procedureIdx);

		expression->typeTableIdx = procedure.typeTableIdx;

		TCPopScope(&procedureContext);

		// Check all paths return
		if (astOverload->astBody && t.procedureInfo.returnTypeIndices.size) {
			ReturnCheckResult result = TCCheckIfReturnsValue(astOverload->astBody);
			if (result == RETURNCHECKRESULT_SOMETIMES)
				LogError(expression->any.loc, "Procedure doesn't always return a value"_s);
			else if (result == RETURNCHECKRESULT_NEVER)
				LogError(expression->any.loc, "Procedure has to return a value"_s);
		}
	} break;
	case ASTNODETYPE_INCLUDE:
	{
		String filename = expression->include.filename;
		CompilerAddSourceFile(filename, expression->any.loc);
	} break;
	case ASTNODETYPE_LINKLIB:
	{
		String filename = expression->linklib.filename;
		*DynamicArrayAdd(&g_context->libsToLink) = filename;

#if IS_WINDOWS
		String dynamicLibExt = ".dll"_s;
#else
		String dynamicLibExt = ".so"_s;
#endif
		CTLibrary ctLib = { .name = ChangeFilenameExtension(filename, dynamicLibExt),
			.loc = expression->any.loc };
		auto ctLibs = g_context->ctExternalLibraries.Get();
		*DynamicArrayAdd(&ctLibs) = ctLib;

		// Wake up any jobs that are looking for dynamic libraries
		{
			auto jobsWaiting = g_context->waitingJobsByReason[YIELDREASON_NEED_DYNAMIC_LIBRARY].Get();
			for (int i = 0; i < jobsWaiting->size; ++i) {
				u32 jobIdx = jobsWaiting[i];
				EnqueueReadyJob(jobIdx);
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
		ArrayView<TCScope> scopeStack = tcContext->scopeStack;
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
			auto globalNames = g_context->tcGlobalNames.GetForRead();
			for (int i = 0; i < globalNames->count; ++i) {
				const TCScopeName *currentName = &globalNames[i];
				if (StringEquals(identifier, currentName->name)) {
					isDefined = true;
					goto done;
				}
			}
		}
		SwitchJob(YIELDREASON_WAITING_FOR_STOP, {});
		// Look one last time...
		{
			auto globalNames = g_context->tcGlobalNames.GetForRead();
			// @Improve: resume loop at the last index we checked?
			for (int i = 0; i < globalNames->count; ++i) {
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
		TCContext tcRunContext = *tcContext;
		tcRunContext.onStaticContext = false;
		BucketArrayInit(&tcRunContext.localValues);
		*BucketArrayAdd(&tcRunContext.localValues) = {}; // No value number 0?

		TypeCheckExpression(&tcRunContext, expression->runNode.expression);
		expression->typeTableIdx = expression->runNode.expression->typeTableIdx;

		BucketArray<IRInstruction, LinearAllocator, 256> irInstructions;
		BucketArrayInit(&irInstructions);

		IRContext irJobContext = {};
		irJobContext.irInstructions = &irInstructions;
		irJobContext.localValues = &tcRunContext.localValues;
		DynamicArrayInit(&irJobContext.irStack, 16);
		BucketArrayInit(&irJobContext.irLabels);

		IRValue resultIRValue = IRGenFromExpression(&irJobContext, expression->runNode.expression);

		CTRegister runResult = CTRunInstructions(tcRunContext.localValues, irInstructions,
				resultIRValue);

		if (resultIRValue.valueType != IRVALUETYPE_INVALID)
			expression->runNode.result = ConstantFromCTRegister(runResult, expression->typeTableIdx);
	} break;
	default:
		LogCompilerError(expression->any.loc, "Unknown expression type on type checking"_s);
	}
}

void TCJobProc(void *args)
{
	TCJobArgs *argsStruct = (TCJobArgs *)args;

	TCContext *tcContext = ALLOC(LinearAllocator, TCContext);

	ASTExpression *expression = argsStruct->expression;
	tcContext->currentProcedureIdx = U32_MAX;
	tcContext->onStaticContext = true;
	tcContext->currentReturnTypes = {};

#if DEBUG_BUILD || USE_PROFILER_API
	String jobDescription = "TC:???"_s;
	switch (expression->nodeType) {
	case ASTNODETYPE_STATIC_DEFINITION:
		switch (expression->staticDefinition.expression->nodeType) {
		case ASTNODETYPE_PROCEDURE_DECLARATION:
			jobDescription = SNPrintF(96, "TC:%S - Procedure declaration",
					expression->staticDefinition.name);
			break;
		case ASTNODETYPE_TYPE:
		case ASTNODETYPE_ALIAS:
			jobDescription = SNPrintF(96, "TC:%S - Type declaration",
					expression->staticDefinition.name);
			break;
		case ASTNODETYPE_IDENTIFIER:
			jobDescription = SNPrintF(96, "TC:%S - Constant declaration",
					expression->staticDefinition.name);
			break;
		default:
			jobDescription = SNPrintF(96, "TC:%S - Static definition",
					expression->staticDefinition.name);
			break;
		}
		break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
		jobDescription = SNPrintF(96, "TC:%S - Variable declaration",
				expression->staticDefinition.name);
		break;
	case ASTNODETYPE_IF_STATIC:
		jobDescription = "TC:Static if"_s;
		break;
	}
#endif

	ProfilerBegin("Running job", StringToCStr(jobDescription, ThreadAllocator::Alloc),
			PROFILER_COLOR(204, 178, 10));

	Job *runningJob = GetCurrentJob();
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->loc = expression->any.loc;
	runningJob->description = jobDescription;
#endif

	switch (expression->nodeType) {
	case ASTNODETYPE_VARIABLE_DECLARATION:
		break;
	case ASTNODETYPE_STATIC_DEFINITION:
	case ASTNODETYPE_RUN:
	case ASTNODETYPE_INCLUDE:
	case ASTNODETYPE_LINKLIB:
	case ASTNODETYPE_IF_STATIC:
	case ASTNODETYPE_OPERATOR_OVERLOAD:
		DynamicArrayInit(&tcContext->scopeStack, 8);
		break;
	default:
		PANIC;
	}

	TypeCheckExpression(tcContext, expression);

	ProfilerEnd();

	FinishCurrentJob();
}

int GetTypeAlignment(u32 typeTableIdx)
{
	int alignment = 0;
	TypeInfo typeInfo = GetTypeInfo(typeTableIdx);

	// Alignment may be specified by the programmer.
	if (typeInfo.alignment) {
		ASSERT(IsPowerOf2(typeInfo.alignment));
		return typeInfo.alignment;
	}

	if (typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		typeInfo.typeCategory == TYPECATEGORY_UNION)
	{
		for (int i = 0; i < typeInfo.structInfo.members.size; ++i) {
			int memberAlignment = GetTypeAlignment(typeInfo.structInfo.members[i].typeTableIdx);
			if (memberAlignment > alignment)
				alignment = memberAlignment;
		}
	}
	else if (typeInfo.typeCategory == TYPECATEGORY_ARRAY)
		alignment = GetTypeAlignment(typeInfo.arrayInfo.elementTypeTableIdx);
	else {
		alignment = 8;
		if (typeInfo.size < 8)
			alignment = NextPowerOf2((int)typeInfo.size);
	}
	ASSERT(IsPowerOf2(alignment));
	return alignment;
}

void TCStructJobProc(void *args)
{
	TCStructJobArgs *argsStruct = (TCStructJobArgs *)args;

	TCContext *tcContext = ALLOC(LinearAllocator, TCContext);

	tcContext->onStaticContext = true;
	tcContext->currentReturnTypes = {};

#if DEBUG_BUILD || USE_PROFILER_API
	String jobDescription = SNPrintF(96, "TC:Members of struct \"%S\"", argsStruct->name);
#endif

	ProfilerBegin("Running job", StringToCStr(jobDescription, ThreadAllocator::Alloc),
			PROFILER_COLOR(178, 204, 10));

	Job *runningJob = GetCurrentJob();
	runningJob->state = JOBSTATE_RUNNING;
#if DEBUG_BUILD
	runningJob->description = jobDescription;
#endif

	u32 typeTableIdx = argsStruct->typeTableIdx;
	TypeInfo t;
	{
		auto &typeTable = g_context->typeTable.unsafe;
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
			LogError(astMember->loc, TPrintF("Type missing in declaration of struct "
					"member \"%S\"", astMember->name));

		if (astMember->value != nullptr)
			LogWarning(astMember->value->any.loc, TPrintF("Default value found on member "
						"\"%S\". This is not yet supported", astMember->name));

		astMember->typeTableIdx = TypeCheckType(tcContext, {}, astMember->loc, astMember->astType);

		StructMember member = {};
		member.name = astMember->name;
		member.isUsing = astMember->isUsing;
		member.typeTableIdx = astMember->typeTableIdx;

		TypeInfo memberTypeInfo = GetTypeInfo(member.typeTableIdx);
		if (astMember->isUsing && memberTypeInfo.typeCategory != TYPECATEGORY_STRUCT &&
				memberTypeInfo.typeCategory != TYPECATEGORY_UNION)
			LogError(astMember->loc, TPrintF("'using' keyword only supported for struct "
					"or union members, but \"%S\" was %S", astMember->name,
					TypeCategoryToString(memberTypeInfo.typeCategory)));

		u64 memberSize = GetTypeInfo(member.typeTableIdx).size;
		int alignment = GetTypeAlignment(member.typeTableIdx);

		if (alignment > largestAlignment)
			largestAlignment = alignment;

		if (!argsStruct->isUnion) {
			// Struct
			if (t.size & (alignment - 1))
				t.size = (t.size & ~(alignment - 1)) + alignment;
			ASSERT(t.size <= S32_MAX);
			member.offset = (s32)t.size;
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
		auto &typeTable = g_context->typeTable.unsafe;
		ASSERT(typeTable[typeTableIdx].typeCategory == TYPECATEGORY_NOT_READY);
		t.valueIdx = typeTable[typeTableIdx].valueIdx;
		(TypeInfo&)typeTable[typeTableIdx] = t;

		WriteUserFacingTypeInfoToStaticData(t);
	}

	// Wake up any jobs that were waiting for this type
	WakeUpAllByIndex(YIELDREASON_TYPE_NOT_READY, typeTableIdx);

	ProfilerEnd();

	FinishCurrentJob();
}

void GenerateTypeCheckJobs(ASTExpression *expression)
{
	switch (expression->nodeType) {
	case ASTNODETYPE_BLOCK:
	{
		for (int i = 0; i < expression->block.statements.size; ++i)
			GenerateTypeCheckJobs(&expression->block.statements[i]);
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
		*args = { .expression = expression };
		RequestNewJob(JOBTYPE_TYPE_CHECK, TCJobProc, (void *)args);
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
		LogCompilerError(expression->any.loc, "Invalid expression type found while "
				"generating type checking jobs"_s);
	} break;
	default:
	{
		LogCompilerError(expression->any.loc, "Unknown expression type found while "
				"generating type checking jobs"_s);
	}
	}
}

void TypeCheckMain()
{
	// Initialize memory and bookkeep of types for static data
	u64 virtualRangeSize = (u64)(STATIC_DATA_VIRTUAL_ADDRESS_END - STATIC_DATA_VIRTUAL_ADDRESS);
	SYSReserveMemory(STATIC_DATA_VIRTUAL_ADDRESS, virtualRangeSize);
	SYSCommitMemory(STATIC_DATA_VIRTUAL_ADDRESS, 0x100000);
	memset(STATIC_DATA_VIRTUAL_ADDRESS, 0xCC, 0x100000);
	g_context->staticDataAllocatedSpace = 0x100000;
	g_context->staticDataSize = 0;
	g_context->staticDataLock = 0;
	DynamicArrayInit(&g_context->staticDataPointersToRelocate, 1024);

	HashMapInit(&g_context->globalValueContents, 64);
	g_context->globalValuesLock = 0;

	{
		auto staticDefinitions = g_context->staticDefinitions.GetForWrite();
		BucketArrayInit(&staticDefinitions);
	}

	{
		auto globalValues = g_context->globalValues.GetForWrite();
		BucketArrayInit(&globalValues);
	}

	{
		auto procedures = g_context->procedures.GetForWrite();
		BucketArrayInit(&procedures);
		// Procedure 0 is invalid
		*BucketArrayAdd(&procedures) = {};
	}

	{
		auto externalProcedures = g_context->externalProcedures.GetForWrite();
		BucketArrayInit(&externalProcedures);
		*BucketArrayAdd(&externalProcedures) = {};
	}

	{
		auto polymorphicProcedures = g_context->polymorphicProcedures.GetForWrite();
		DynamicArrayInit(&polymorphicProcedures, 128);
	}

	{
		auto operatorOverloads = g_context->operatorOverloads.GetForWrite();
		DynamicArrayInit(&operatorOverloads, 32);
	}

	{
		SpinlockLock(&g_context->typeTable.lock);
		auto &typeTable = g_context->typeTable.unsafe;
		BucketArrayInit(&typeTable);
		for (int i = 0; i < TYPETABLEIDX_Count; ++i)
			BucketArrayAdd(&typeTable);

		TypeInfo *typeTableFast = (TypeInfo *)typeTable.buckets[0];

		TypeInfo t = {};
		t.typeCategory = TYPECATEGORY_INVALID;

		t.size = 0;
		t.valueIdx = NewGlobalValue("<anything>"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_Anything]  = t;
		t.valueIdx = NewGlobalValue("<struct literal>"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_StructLiteral]  = t;
		t.valueIdx = NewGlobalValue("<unset>"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_Unset]  = t;

		t.typeCategory = TYPECATEGORY_INTEGER;
		t.integerInfo.isSigned = true;

		t.size = 1;
		t.valueIdx = NewGlobalValue("_typeInfo_s8"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S8]  = t;
		t.size = 2;
		t.valueIdx = NewGlobalValue("_typeInfo_s16"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S16] = t;
		t.size = 4;
		t.valueIdx = NewGlobalValue("_typeInfo_s32"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S32] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue("_typeInfo_s64"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S64] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_integer"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_INTEGER] = t;

		t.integerInfo.isSigned = false;

		t.size = 1;
		t.valueIdx = NewGlobalValue("_typeInfo_u8"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U8]  = t;
		t.valueIdx = NewGlobalValue("_typeInfo_bool"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_BOOL]  = t;
		t.size = 2;
		t.valueIdx = NewGlobalValue("_typeInfo_u16"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U16] = t;
		t.size = 4;
		t.valueIdx = NewGlobalValue("_typeInfo_u32"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U32] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue("_typeInfo_u64"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U64] = t;

		t.size = 16;
		t.valueIdx = NewGlobalValue("_typeInfo_128"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_128] = t;

		t.typeCategory = TYPECATEGORY_FLOATING;
		t.size = 4;
		t.valueIdx = NewGlobalValue("_typeInfo_f32"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F32] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_floating"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_FLOATING] = t;
		t.size = 8;
		t.valueIdx = NewGlobalValue("_typeInfo_f64"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F64] = t;

		t = {};
		t.typeCategory = TYPECATEGORY_INVALID;
		t.valueIdx = NewGlobalValue("_typeInfo_void"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_VOID] = t;

		t.typeCategory = TYPECATEGORY_NOT_READY;
		t.valueIdx = NewGlobalValue("_typeInfo_string_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_STRING_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_array_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ARRAY_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_any_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ANY_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_integer_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_struct_member_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_struct_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_enum_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_pointer_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_array_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_procedure_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_PROCEDURE_STRUCT] = t;
		t.valueIdx = NewGlobalValue("_typeInfo_type_info_alias_struct"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ALIAS_STRUCT] = t;

		// Pointers
		TypeInfo tp = {};
		tp.size = g_pointerSize;
		tp.typeCategory = TYPECATEGORY_POINTER;

		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_S8;
		tp.valueIdx = NewGlobalValue("_typeInfo_s8_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S8_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_S16;
		tp.valueIdx = NewGlobalValue("_typeInfo_s16_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S16_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_S32;
		tp.valueIdx = NewGlobalValue("_typeInfo_s32_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S32_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_S64;
		tp.valueIdx = NewGlobalValue("_typeInfo_s64_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_S64_PTR] = tp;

		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_U8;
		tp.valueIdx = NewGlobalValue("_typeInfo_u8_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U8_PTR]  = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_U16;
		tp.valueIdx = NewGlobalValue("_typeInfo_u16_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U16_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_U32;
		tp.valueIdx = NewGlobalValue("_typeInfo_u32_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U32_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_U64;
		tp.valueIdx = NewGlobalValue("_typeInfo_u64_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_U64_PTR] = tp;

		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_F32;
		tp.valueIdx = NewGlobalValue("_typeInfo_f32_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F32_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_F64;
		tp.valueIdx = NewGlobalValue("_typeInfo_f64_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_F64_PTR] = tp;

		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_BOOL;
		tp.valueIdx = NewGlobalValue("_typeInfo_bool_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_BOOL_PTR]  = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_VOID;
		tp.valueIdx = NewGlobalValue("_typeInfo_void_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_VOID_PTR] = tp;

		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_STRING_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_string_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_STRING_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_ARRAY_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_array_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ARRAY_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_ANY_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_any_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_ANY_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_integer_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_struct_member_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_struct_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_enum_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_pointer_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_array_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_PROCEDURE_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_procedure_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_PROCEDURE_STRUCT_PTR] = tp;
		tp.pointerInfo.pointedTypeTableIdx = TYPETABLEIDX_TYPE_INFO_ALIAS_STRUCT;
		tp.valueIdx = NewGlobalValue("_typeInfo_type_info_alias_struct_ptr"_s, TYPETABLEIDX_Unset, VALUEFLAGS_ON_STATIC_STORAGE);
		typeTableFast[TYPETABLEIDX_TYPE_INFO_ALIAS_STRUCT_PTR] = tp;

		SpinlockUnlock(&g_context->typeTable.lock);

		for (int i = 1; i <= TYPETABLEIDX_Unset; ++i)
			// Yeah this needs a job context but it shouldn't need to yield to do this
			WriteUserFacingTypeInfoToStaticData(typeTableFast[i]);

		for (int i = TYPETABLEIDX_PrimitiveBegin; i < TYPETABLEIDX_PrimitiveEnd; ++i)
			// Yeah this needs a job context but it shouldn't need to yield to do this
			WriteUserFacingTypeInfoToStaticData(typeTableFast[i]);

		for (int i = TYPETABLEIDX_BuiltinStructsBegin; i < TYPETABLEIDX_BuiltinStructsEnd; ++i)
			AllocateStaticData(typeTableFast[i].valueIdx, sizeof(UserFacingTypeInfoStruct), 8);
	}

	{
		auto globalTypes = g_context->tcGlobalTypeIndices.GetForWrite();
		DynamicArrayInit(&globalTypes, 64);
		for (int i = 0; i < TYPETABLEIDX_Count; ++i)
			*DynamicArrayAdd(&globalTypes) = i;
	}

	{
		auto globalNames = g_context->tcGlobalNames.GetForWrite();

		ArrayInit(&g_context->tcPrimitiveTypes, TYPETABLEIDX_PrimitiveEnd -
				TYPETABLEIDX_PrimitiveBegin);

		BucketArrayInit(&globalNames);

		TCScopeName scopeNamePrimitive;
		scopeNamePrimitive.type = NAMETYPE_PRIMITIVE;
		scopeNamePrimitive.loc = {};

		scopeNamePrimitive.name = "s8"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S8;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s16"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S16;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S32;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "s64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_S64;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u8"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U8;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u16"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U16;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U32;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "u64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_U64;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "f32"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_F32;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "f64"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_F64;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "bool"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_BOOL;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;

		scopeNamePrimitive.name = "void"_s;
		scopeNamePrimitive.primitiveTypeTableIdx = TYPETABLEIDX_VOID;
		*ArrayAdd(&g_context->tcPrimitiveTypes) = scopeNamePrimitive;
		*BucketArrayAdd(&globalNames) = scopeNamePrimitive;
	}
	MTQueueInit<LinearAllocator>(&g_context->tcGlobalNamesToAdd, 128);

	{
		auto inlineCalls = g_context->tcInlineCalls.Get();
		DynamicArrayInit(&inlineCalls, 128);
	}

	{
		auto ctLibraries = g_context->ctExternalLibraries.Get();
		DynamicArrayInit(&ctLibraries, 32);
	}
}
