const u32 VALUE_INVALID_IDX = U32_MAX;
const u32 VALUE_GLOBAL_BIT  = 0x80000000;
const u32 VALUE_GLOBAL_MASK = 0x7FFFFFFF;
enum ValueFlags
{
	VALUEFLAGS_FORCE_MEMORY         = 0x1,
	VALUEFLAGS_IS_EXTERNAL          = 0x2,
	VALUEFLAGS_ON_STATIC_STORAGE    = 0x4
};

struct Value
{
#if DEBUG_BUILD
	String name;
#endif
	u32 typeTableIdx;
	u32 flags;
	SmallString externalSymbolName;
};

enum TypeCategory : u8
{
	TYPECATEGORY_INVALID,
	TYPECATEGORY_NOT_READY,

	TYPECATEGORY_ValidBegin,
	TYPECATEGORY_INTEGER,
	TYPECATEGORY_FLOATING,
	TYPECATEGORY_STRUCT,
	TYPECATEGORY_UNION,
	TYPECATEGORY_ENUM,
	TYPECATEGORY_POINTER,
	TYPECATEGORY_ARRAY,
	TYPECATEGORY_PROCEDURE,
	TYPECATEGORY_ALIAS
};

struct TypeInfo;

struct TypeInfoInteger
{
	s32 isSigned;
};

struct StructMember
{
	String name;
	u32 typeTableIdx;
	bool isUsing;
	u32 offset;
};
struct TypeInfoStruct
{
	String name;
	Array<StructMember, LinearAllocator> members;
};

struct TypeInfoEnum
{
	String name;
	u32 typeTableIdx;
	Array<String, LinearAllocator> names;
	Array<s64, LinearAllocator> values;
};

struct TypeInfoPointer
{
	u32 pointedTypeTableIdx;
};

struct TypeInfoArray
{
	u32 elementTypeTableIdx;
	u64 count;
};

struct ProcedureParameter {
	u32 typeTableIdx;
	Constant defaultValue;
};
struct TypeInfoProcedure {
	bool isVarargs;
	Array<ProcedureParameter, LinearAllocator> parameters;
	DynamicArray<u32, LinearAllocator> returnTypeIndices;
	CallingConvention callingConvention;
};

struct TypeInfoAlias
{
	String name;
	u32 aliasedTypeIdx;
	bool doesImplicitlyCast;
};

struct TypeInfo
{
	TypeCategory typeCategory;
	u8 alignment;
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
		TypeInfoAlias aliasInfo;
	};
};

enum UserFacingTypeCategory : u8
{
	USERFACINGTYPECATEGORY_INTEGER   = 0,
	USERFACINGTYPECATEGORY_FLOATING  = 1,
	USERFACINGTYPECATEGORY_STRUCT    = 2,
	USERFACINGTYPECATEGORY_ENUM      = 3,
	USERFACINGTYPECATEGORY_POINTER   = 4,
	USERFACINGTYPECATEGORY_ARRAY     = 5,
	USERFACINGTYPECATEGORY_PROCEDURE = 6,
	USERFACINGTYPECATEGORY_ALIAS     = 7
};

struct UserFacingTypeInfo;

struct UserFacingStructMember
{
	String name;
	UserFacingTypeInfo *typeInfo;
	u64 offset;
};

struct UserFacingTypeInfo
{
	UserFacingTypeCategory typeCategory;
	u64 size;
};

struct UserFacingTypeInfoInteger : UserFacingTypeInfo
{
	s32 isSigned;
};

struct UserFacingTypeInfoStruct : UserFacingTypeInfo
{
	String name;
	s32 isUnion;
	u64 memberCount;
	UserFacingStructMember *members;
};

struct UserFacingTypeInfoEnum : UserFacingTypeInfo
{
	String name;
	UserFacingTypeInfo *typeInfo;
	u64 nameCount;
	String *names;
	u64 valueCount;
	s64 *values;
};

struct UserFacingTypeInfoPointer : UserFacingTypeInfo
{
	UserFacingTypeInfo *typeInfo;
};

struct UserFacingTypeInfoArray : UserFacingTypeInfo
{
	u64 count;
	UserFacingTypeInfo *elementTypeInfo;
};

struct UserFacingTypeInfoProcedure : UserFacingTypeInfo
{
	u64 parameterCount;
	UserFacingTypeInfo **parameters;
	s32 isVarargs;
};

struct UserFacingTypeInfoAlias : UserFacingTypeInfo
{
	UserFacingTypeInfo *typeInfo;
};

enum StaticDefinitionType
{
	STATICDEFINITIONTYPE_NOT_READY = 0,
	STATICDEFINITIONTYPE_TYPE,
	STATICDEFINITIONTYPE_PROCEDURE,
	STATICDEFINITIONTYPE_CONSTANT
};
struct StaticDefinition
{
	String name;
	StaticDefinitionType definitionType;
	u32 typeTableIdx;
	union
	{
		u32 procedureIdx;
		Constant constant;
	};
};

struct OperatorOverload
{
	enum TokenType op;
	u32 procedureIdx;
};

struct TCScopeName
{
	NameType type;
	String name;
	SourceLocation loc;
	union
	{
		u32 primitiveTypeTableIdx;
		struct
		{
			u32 valueIdx;
			u32 typeTableIdx;
		} variableInfo;
		const StructMember *structMember;
		ASTExpression *expression;
		u32 staticDefinitionIdx;
		u32 typeTableIdx;
	};
};
struct TCScope
{
	DynamicArray<TCScopeName, ThreadAllocator> names;
	DynamicArray<u32, ThreadAllocator> typeIndices;
};

struct TCGlobalScope
{
	DynamicArray<TCScopeName, LinearAllocator> names;
	DynamicArray<u32, LinearAllocator> typeIndices;
};

const u32 PROCEDURE_EXTERNAL_BIT  = 0x80000000;
const u32 PROCEDURE_EXTERNAL_MASK = 0x7FFFFFFF;
struct IRInstruction;
struct Procedure
{
	String name;
	DynamicArray<u32, LinearAllocator> parameterValues;
	ASTExpression *astBody;
	ASTProcedurePrototype astPrototype;
	bool isInline;
	bool isExported;
	bool isBodyTypeChecked;
	bool isIRReady;
	Array<u32, LinearAllocator> returnValueIndices;
	u32 typeTableIdx; // Type of the procedure
	BucketArray<Value, LinearAllocator, 256> localValues;
	BucketArray<IRInstruction, LinearAllocator, 256> irInstructions;
};

struct InlineCall
{
	u32 procedureIdx;
	SourceLocation loc;
};

struct TCJobArgs
{
	ASTExpression *expression;
};

struct TCStructJobArgs
{
	u32 typeTableIdx;
	ASTStructDeclaration astStructDecl;
	String name;
	bool isUnion;
};

void GenerateTypeCheckJobs(ASTExpression *expression);
void TCStructJobProc(void *args);
void TCCommitGlobalNames();

struct TCContext
{
	ASTExpression *expression;
	bool onStaticContext;
	u32 currentProcedureIdx;
	DynamicArray<TCScope, ThreadAllocator> scopeStack;
	ArrayView<u32> currentReturnTypes;
	u32 currentForLoopArrayType;
	BucketArray<Value, LinearAllocator, 256> localValues;
};
