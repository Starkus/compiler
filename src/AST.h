struct ASTExpression;

struct ASTBase
{
	SourceLocation loc;
};

enum LiteralType
{
	LITERALTYPE_INTEGER,
	LITERALTYPE_FLOATING,
	LITERALTYPE_CHARACTER,
	LITERALTYPE_STRING,
	LITERALTYPE_GROUP
};
struct ASTLiteral : ASTBase
{
	LiteralType type;
	union
	{
		s64 integer;
		f64 floating;
		char character;
		String string;
		Array<ASTExpression *, FrameAllocator> members;
	};
};

struct ASTUnaryOperation : ASTBase
{
	enum TokenType op;
	ASTExpression *expression;
};

struct ASTBinaryOperation : ASTBase
{
	enum TokenType op;
	ASTExpression *leftHand;
	ASTExpression *rightHand;
};

enum TCValueType
{
	TCVALUETYPE_VALUE,
	TCVALUETYPE_PARAMETER,
};
struct TCValue
{
	u32 type : 1;
	u32 valueIdx : 31;
};

struct Value;
struct StructMember;
struct StaticDefinition;
enum NameType
{
	NAMETYPE_VARIABLE,
	NAMETYPE_STRUCT_MEMBER,
	NAMETYPE_ASTEXPRESSION,
	NAMETYPE_STATIC_DEFINITION
};
struct ASTIdentifier : ASTBase
{
	String string;

	// Type check
	NameType type;
	union
	{
		TCValue tcValue;
		const StructMember *structMember;
		ASTExpression *expression;
		StaticDefinition *staticDefinition;
	};
};

struct ASTBlock : ASTBase
{
	DynamicArray<ASTExpression, FrameAllocator> statements;

	// Type check
	bool scopePushed;
	s32 typeCheckingIdx;
};

struct ASTType;
struct ASTStructMemberDeclaration : ASTBase
{
	String name;
	ASTType *astType;
	ASTExpression *value;
	bool isUsing;

	u32 typeTableIdx;
};
struct ASTStructDeclaration : ASTBase
{
	bool isUnion;
	DynamicArray<ASTStructMemberDeclaration, FrameAllocator> members;
};

struct ASTEnumMember
{
	String name;
	ASTExpression *value;
};
struct ASTEnumDeclaration : ASTBase
{
	ASTType *astType;
	DynamicArray<ASTEnumMember, FrameAllocator> members;
};

enum CallingConvention
{
	CC_DEFAULT,
	CC_WIN64,
	CC_LINUX64
};

struct ASTType;
struct ASTProcedureParameter;
struct ASTProcedurePrototype : ASTBase
{
	DynamicArray<ASTProcedureParameter, FrameAllocator> astParameters;
	ASTType *astReturnType;

	CallingConvention callingConvention;
	bool isVarargs;
	String varargsName;
	SourceLocation varargsLoc;

	// Type check
	u32 returnTypeIdx;
};

enum ASTTypeNodeType
{
	ASTTYPENODETYPE_INVALID,
	ASTTYPENODETYPE_IDENTIFIER,
	ASTTYPENODETYPE_POINTER,
	ASTTYPENODETYPE_STRUCT_DECLARATION,
	ASTTYPENODETYPE_ENUM_DECLARATION,
	ASTTYPENODETYPE_ARRAY,
	ASTTYPENODETYPE_PROCEDURE
};
struct ASTType : ASTBase
{
	ASTTypeNodeType nodeType;
	union
	{
		String name;
		ASTStructDeclaration structDeclaration;
		ASTEnumDeclaration enumDeclaration;
		ASTType *pointedType;
		struct
		{
			ASTType *arrayType;
			u64 arrayCount;
		};
		ASTProcedurePrototype procedurePrototype;
	};
};

struct ASTVariableDeclaration : ASTBase
{
	String name;
	ASTExpression *astInitialValue;
	ASTType *astType;
	bool isInline;
	bool isStatic;
	bool isExternal;

	// TypeCheck
	bool addedScopeName;
	u32 valueIdx;
	u32 typeTableIdx;
};

struct ASTProcedureParameter : ASTBase
{
	String name;
	ASTExpression *astInitialValue;
	ASTType *astType;
	bool isUsing;

	// TypeCheck
	u32 valueIdx;
	u32 typeTableIdx;
};

struct ASTProcedureDeclaration : ASTBase
{
	ASTProcedurePrototype prototype;
	String name;
	bool isInline;
	bool isExternal;
	ASTExpression *astBody;

	// Type check
	s32 procedureIdx;
	bool checkedPrototype;
};

struct ASTOperatorOverload : ASTBase
{
	ASTProcedurePrototype prototype;
	ASTExpression *astBody;
	enum TokenType op;
	bool isInline;

	// Type check
	bool overloadRegistered;
	bool checkedPrototype;
	s32 procedureIdx;
};

struct ASTStaticDefinition : ASTBase
{
	String name;
	ASTExpression *expression;

	// Type check
	StaticDefinition *staticDef;
};

enum ProcedureCallType
{
	CALLTYPE_STATIC,
	CALLTYPE_VALUE,
	CALLTYPE_ASTEXPRESSION
};
struct ASTProcedureCall : ASTBase
{
	String name;
	DynamicArray<ASTExpression, FrameAllocator> arguments;

	// Type check
	u32 procedureTypeIdx;
	ProcedureCallType callType;
	bool procedureFound;
	u32 parameterTypeCheckingIdx;
	union
	{
		s32 procedureIdx;
		TCValue tcValue;
		ASTExpression *expression;
	};
	ASTExpression *astBodyInlineCopy;
};

enum IntrinsicType
{
	INTRINSIC_UNSET,
	INTRINSIC_BREAKPOINT,
	INTRINSIC_SQRT32,
	INTRINSIC_SQRT64,
};
struct ASTIntrinsic : ASTBase
{
	String name;
	DynamicArray<ASTExpression, FrameAllocator> arguments;

	// Type check
	IntrinsicType type;
};

struct ASTIf : ASTBase
{
	ASTExpression *condition;
	ASTExpression *body;
	ASTExpression *elseBody;
	SourceLocation elseLoc;
};

struct ASTWhile : ASTBase
{
	ASTExpression *condition;
	ASTExpression *body;
};

struct ASTFor : ASTBase
{
	String indexVariableName;
	String itemVariableName;
	ASTExpression *range;
	ASTExpression *body;

	// Type check
	bool scopePushed;
	u32 indexValueIdx;
	u32 elementValueIdx;
	u32 elementTypeTableIdx;
};

struct ASTSimple : ASTBase
{
	ASTExpression *expression;
};

struct ASTCast : ASTBase
{
	ASTType astType;
	ASTExpression *expression;
};

struct ASTRoot : ASTBase
{
	ASTBlock block;
};

enum ASTNodeType
{
	ASTNODETYPE_INVALID,
	ASTNODETYPE_IDENTIFIER,
	ASTNODETYPE_LITERAL,
	ASTNODETYPE_TYPE,
	ASTNODETYPE_ALIAS,
	ASTNODETYPE_BLOCK,
	ASTNODETYPE_UNARY_OPERATION,
	ASTNODETYPE_BINARY_OPERATION,
	ASTNODETYPE_VARIABLE_DECLARATION,
	ASTNODETYPE_PROCEDURE_DECLARATION,
	ASTNODETYPE_STRUCT_DECLARATION,
	ASTNODETYPE_ENUM_DECLARATION,
	ASTNODETYPE_STATIC_DEFINITION,
	ASTNODETYPE_OPERATOR_OVERLOAD,
	ASTNODETYPE_PROCEDURE_CALL,
	ASTNODETYPE_INTRINSIC,
	ASTNODETYPE_IF,
	ASTNODETYPE_WHILE,
	ASTNODETYPE_FOR,
	ASTNODETYPE_CONTINUE,
	ASTNODETYPE_REMOVE,
	ASTNODETYPE_BREAK,
	ASTNODETYPE_RETURN,
	ASTNODETYPE_DEFER,
	ASTNODETYPE_USING,
	ASTNODETYPE_TYPEOF,
	ASTNODETYPE_SIZEOF,
	ASTNODETYPE_CAST,
	ASTNODETYPE_GARBAGE
};
struct ASTExpression
{
	ASTNodeType nodeType;
	union
	{
		ASTBase any;
		ASTIdentifier identifier;
		ASTLiteral literal;
		ASTType astType;
		ASTBlock block;
		ASTUnaryOperation unaryOperation;
		ASTBinaryOperation binaryOperation;
		ASTVariableDeclaration variableDeclaration;
		ASTProcedureDeclaration procedureDeclaration;
		ASTStructDeclaration structDeclaration;
		ASTEnumDeclaration enumDeclaration;
		ASTStaticDefinition staticDefinition;
		ASTOperatorOverload operatorOverload;
		ASTProcedureCall procedureCall;
		ASTIntrinsic intrinsic;
		ASTIf ifNode;
		ASTWhile whileNode;
		ASTFor forNode;
		ASTSimple returnNode;
		ASTSimple deferNode;
		ASTSimple usingNode;
		ASTSimple typeOfNode;
		ASTSimple sizeOfNode;
		ASTCast castNode;
	};

	// Filled in during type checking
	u32 typeTableIdx;
};

enum TypeTableIndices : u32
{
	TYPETABLEIDX_ANYTHING = 1,
	TYPETABLEIDX_STRUCT_LITERAL = 2,
	TYPETABLEIDX_UNSET = 3,

	TYPETABLEIDX_Begin = 10,
	TYPETABLEIDX_PRIMITIVE_BEGIN = 10,

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

	TYPETABLEIDX_128,

	TYPETABLEIDX_STRING_STRUCT,
	TYPETABLEIDX_ARRAY_STRUCT,
	TYPETABLEIDX_ANY_STRUCT,
	TYPETABLEIDX_TYPE_INFO_STRUCT,
	TYPETABLEIDX_TYPE_INFO_INTEGER_STRUCT,
	TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT,
	TYPETABLEIDX_TYPE_INFO_STRUCT_STRUCT,
	TYPETABLEIDX_TYPE_INFO_ENUM_STRUCT,
	TYPETABLEIDX_TYPE_INFO_POINTER_STRUCT,
	TYPETABLEIDX_TYPE_INFO_ARRAY_STRUCT,

	TYPETABLEIDX_COUNT
};
