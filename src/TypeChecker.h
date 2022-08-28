const u32 VALUE_INVALID_IDX = U32_MAX;
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
	u32 typeTableIdx;
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
	u32 typeTableIdx;
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
	u64 offset;
};
struct TypeInfoStruct
{
	String name;
	Array<StructMember, FrameAllocator> members;
};

struct TypeInfoEnum
{
	String name;
	u32 typeTableIdx;
	Array<String, FrameAllocator> names;
	Array<s64, FrameAllocator> values;
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

struct ProcedureParameter
{
	u32 typeTableIdx;
	Constant defaultValue;
};
struct TypeInfoProcedure
{
	u32 returnTypeTableIdx;
	bool isVarargs;
	Array<ProcedureParameter, FrameAllocator> parameters;
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

enum StaticDefinitionType
{
	STATICDEFINITIONTYPE_NOT_READY,
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
		s32 procedureIdx;
		Constant constant;
	};
};

struct OperatorOverload
{
	enum TokenType op;
	s32 procedureIdx;
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
	};
};
struct TCScope
{
	DynamicArray<TCScopeName, PhaseAllocator> names;
	DynamicArray<u32, PhaseAllocator> typeIndices;
};

struct Procedure
{
	String name;
	DynamicArray<u32, FrameAllocator> parameterValues;
	ASTExpression *astBody;
	ASTProcedurePrototype astPrototype;
	bool isInline;
	bool isExported;
	bool isBodyTypeChecked;
	u32 returnValueIdx;
	u32 typeTableIdx; // Type of the procedure
};

struct TCJob
{
	u32 jobIdx;
	ASTExpression *expression;
	DynamicArray<TCScope, PhaseAllocator> scopeStack;
	u32 currentReturnType;
	u32 currentForLoopArrayType;
};

struct TCResultWithType
{
	bool success;
	u32 typeTableIdx;
};

struct Context;
struct TCJobArgs
{
	Context *context;
	u32 jobIdx;
	ASTExpression *expression;
};

enum TCJobState
{
	TCJOBSTATE_RUNNING,
	TCJOBSTATE_SLEEPING,
	// WAITING_FOR_STOP jobs want to wait until no jobs are running to make a decision.
	// As of time of write, only #defined does this to determine if something isn't defined anywhere
	// before continuing.
	TCJOBSTATE_WAITING_FOR_STOP,
	TCJOBSTATE_DONE,
};