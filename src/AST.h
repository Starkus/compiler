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
		Array<ASTExpression *, LinearAllocator> members;
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

struct Value;
struct StructMember;
enum NameType
{
	NAMETYPE_INVALID,
	NAMETYPE_PRIMITIVE,
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
		u32 valueIdx;
		const StructMember *structMember;
		ASTExpression *expression;
		u32 staticDefinitionIdx;
	};
};

struct ASTBlock : ASTBase
{
	DynamicArray<ASTExpression, LinearAllocator> statements;
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
	DynamicArray<ASTStructMemberDeclaration, LinearAllocator> members;
};

struct ASTEnumMember
{
	String name;
	ASTExpression *value;
	SourceLocation loc;
};
struct ASTEnumDeclaration : ASTBase
{
	ASTType *astType;
	DynamicArray<ASTEnumMember, LinearAllocator> members;
};

enum CallingConvention : u8
{
	CC_DEFAULT,
	CC_WIN64,
	CC_LINUX64
};

struct ASTType;
struct ASTProcedureParameter;
struct ASTProcedurePrototype : ASTBase
{
	DynamicArray<ASTProcedureParameter, LinearAllocator> astParameters;
	ASTType *astReturnType;

	String varargsName;
	SourceLocation varargsLoc;
	bool isVarargs;
	CallingConvention callingConvention;

	// Type check
	u32 returnTypeIdx;
};

enum ASTTypeNodeType
{
	ASTTYPENODETYPE_INVALID,
	ASTTYPENODETYPE_IDENTIFIER,
	ASTTYPENODETYPE_POINTER,
	ASTTYPENODETYPE_STRUCT_DECLARATION,
	ASTTYPENODETYPE_UNION_DECLARATION,
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
	ASTExpression *astBody;
	bool isInline;
	bool isExternal;
	bool isExported;

	// Type check
	u32 procedureIdx;
};

struct ASTOperatorOverload : ASTBase
{
	ASTProcedurePrototype prototype;
	ASTExpression *astBody;
	enum TokenType op;
	bool isInline;

	// Type check
	u32 procedureIdx;
};

struct ASTStaticDefinition : ASTBase
{
	String name;
	ASTExpression *expression;

	// Type check
	u32 staticDefinitionIdx;
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
	HybridArray<ASTExpression *, 4, LinearAllocator> arguments;

	// Type check
	u32 procedureTypeIdx;
	ProcedureCallType callType;
	union
	{
		u32 procedureIdx;
		u32 valueIdx;
		ASTExpression *expression;
	};
	ASTExpression *astBodyInlineCopy;
	Array<u32, LinearAllocator> inlineParameterValues;
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
	DynamicArray<ASTExpression, LinearAllocator> arguments;

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

struct ASTIfStatic : ASTBase
{
	ASTExpression *condition;
	ASTExpression *body;
	ASTExpression *elseBody;
	SourceLocation elseLoc;
	bool evaluatesToTrue;
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

struct ASTInclude : ASTBase
{
	String filename;
};

struct ASTDefined : ASTBase
{
	String identifier;
	bool isDefined;
};

struct ASTRoot : ASTBase
{
	ASTBlock block;
};

enum ASTNodeType : u8
{
	ASTNODETYPE_INVALID,
	ASTNODETYPE_IDENTIFIER,
	ASTNODETYPE_LITERAL,
	ASTNODETYPE_TYPE,
	ASTNODETYPE_ALIAS,
	ASTNODETYPE_BLOCK,
	ASTNODETYPE_INCLUDE,
	ASTNODETYPE_LINKLIB,
	ASTNODETYPE_UNARY_OPERATION,
	ASTNODETYPE_BINARY_OPERATION,
	ASTNODETYPE_VARIABLE_DECLARATION,
	ASTNODETYPE_PROCEDURE_DECLARATION,
	ASTNODETYPE_STATIC_DEFINITION,
	ASTNODETYPE_OPERATOR_OVERLOAD,
	ASTNODETYPE_PROCEDURE_CALL,
	ASTNODETYPE_INTRINSIC,
	ASTNODETYPE_IF,
	ASTNODETYPE_IF_STATIC,
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
	ASTNODETYPE_DEFINED,
	ASTNODETYPE_GARBAGE
};
struct ASTExpression
{
	ASTNodeType nodeType;

	// Filled in during type checking
	u32 typeTableIdx;

	union
	{
		ASTBase any;
		ASTIdentifier identifier;
		ASTLiteral literal;
		ASTType astType;
		ASTBlock block;
		ASTInclude include;
		ASTInclude linklib;
		ASTUnaryOperation unaryOperation;
		ASTBinaryOperation binaryOperation;
		ASTVariableDeclaration variableDeclaration;
		ASTProcedureDeclaration procedureDeclaration;
		ASTStaticDefinition staticDefinition;
		ASTOperatorOverload operatorOverload;
		ASTProcedureCall procedureCall;
		ASTIntrinsic intrinsic;
		ASTIf ifNode;
		ASTIfStatic ifStaticNode;
		ASTWhile whileNode;
		ASTFor forNode;
		ASTSimple returnNode;
		ASTSimple deferNode;
		ASTSimple usingNode;
		ASTSimple typeOfNode;
		ASTSimple sizeOfNode;
		ASTDefined definedNode;
		ASTCast castNode;
	};
};

enum TypeTableIndices : u32
{
	TYPETABLEIDX_Anything = 1,
	TYPETABLEIDX_StructLiteral = 2,
	TYPETABLEIDX_Unset = 3,

	TYPETABLEIDX_Begin = 10,
	TYPETABLEIDX_PrimitiveBegin = 10,

	TYPETABLEIDX_S8 = TYPETABLEIDX_PrimitiveBegin,
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

	TYPETABLEIDX_128,

	TYPETABLEIDX_PrimitiveEnd = TYPETABLEIDX_128,

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

	TYPETABLEIDX_Count
};
