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
	LITERALTYPE_STRUCT
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
		Array<ASTExpression *> members;
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
	TCVALUETYPE_INVALID,
	TCVALUETYPE_VALUE,
	TCVALUETYPE_PARAMETER,
};
struct TCValue
{
	TCValueType type;
	union
	{
		u32 valueIdx;
		u32 parameterIdx;
	};
};

struct Value;
struct StructMember;
struct StaticDefinition;
enum NameType
{
	NAMETYPE_VARIABLE,
	NAMETYPE_STRUCT_MEMBER,
	NAMETYPE_STRUCT_MEMBER_CHAIN,
	NAMETYPE_STATIC_DEFINITION
};
struct ASTIdentifier : ASTBase
{
	String string;
	bool isUsing;

	// Type check
	NameType type;
	union
	{
		TCValue tcValue;
		struct
		{
			TCValue tcValueBase;
			const StructMember *structMember;
		} structMemberInfo;
		struct
		{
			TCValue tcValueBase;
			Array<const StructMember *> offsets;
		} structMemberChain;
		StaticDefinition *staticDefinition;
	};
};

struct ASTBlock : ASTBase
{
	DynamicArray<ASTExpression, malloc, realloc> statements;

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

	s64 typeTableIdx;
};
struct ASTStructDeclaration : ASTBase
{
	bool isUnion;
	DynamicArray<ASTStructMemberDeclaration, malloc, realloc> members;
};

struct ASTEnumMember
{
	String name;
	ASTExpression *value;
};
struct ASTEnumDeclaration : ASTBase
{
	ASTType *astType;
	DynamicArray<ASTEnumMember, malloc, realloc> members;
};

struct ASTType;
struct ASTVariableDeclaration;
struct ASTProcedurePrototype : ASTBase
{
	bool isVarargs;
	String varargsName;
	ASTType *astReturnType;
	DynamicArray<ASTVariableDeclaration, malloc, realloc> astParameters;

	// Type check
	s64 returnTypeIdx;
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
	bool isUsing;

	// TypeCheck
	bool addedScopeName;
	u32 valueIdx;
	s64 typeTableIdx;
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

struct ASTStaticDefinition : ASTBase
{
	String name;
	ASTExpression *expression;

	// Type check
	StaticDefinition *staticDef;
};

struct ASTProcedureCall : ASTBase
{
	String name;
	DynamicArray<ASTExpression, malloc, realloc> arguments;

	// Type check
	s64 procedureTypeIdx;
	bool isIndirect;
	bool procedureFound;
	u32 parameterTypeCheckingIdx;
	union
	{
		s32 procedureIdx;
		TCValue tcValue;
	};
};

enum IntrinsicType
{
	INTRINSIC_UNSET,
	INTRINSIC_SQRT32,
	INTRINSIC_SQRT64,
};
struct ASTIntrinsic : ASTBase
{
	String name;
	DynamicArray<ASTExpression, malloc, realloc> arguments;

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
	ASTExpression *range;
	ASTExpression *body;

	// Type check
	bool scopePushed;
	u32 indexValueIdx;
	u32 elementValueIdx;
	s64 elementTypeTableIdx;
};

struct ASTReturn : ASTBase
{
	ASTExpression *expression;
};

struct ASTDefer : ASTBase
{
	ASTExpression *expression;
};

struct ASTTypeOf : ASTBase
{
	ASTExpression *expression;
};

struct ASTSizeOf : ASTBase
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
	ASTNODETYPE_BLOCK,
	ASTNODETYPE_UNARY_OPERATION,
	ASTNODETYPE_BINARY_OPERATION,
	ASTNODETYPE_VARIABLE_DECLARATION,
	ASTNODETYPE_PROCEDURE_DECLARATION,
	ASTNODETYPE_STRUCT_DECLARATION,
	ASTNODETYPE_ENUM_DECLARATION,
	ASTNODETYPE_STATIC_DEFINITION,
	ASTNODETYPE_PROCEDURE_CALL,
	ASTNODETYPE_INTRINSIC,
	ASTNODETYPE_IF,
	ASTNODETYPE_WHILE,
	ASTNODETYPE_FOR,
	ASTNODETYPE_CONTINUE,
	ASTNODETYPE_BREAK,
	ASTNODETYPE_RETURN,
	ASTNODETYPE_DEFER,
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
		ASTProcedureCall procedureCall;
		ASTIntrinsic intrinsic;
		ASTIf ifNode;
		ASTWhile whileNode;
		ASTFor forNode;
		ASTReturn returnNode;
		ASTDefer deferNode;
		ASTTypeOf typeOfNode;
		ASTSizeOf sizeOfNode;
		ASTCast castNode;
	};

	// Filled in during type checking
	s64 typeTableIdx;
};

enum TypeTableIndices
{
	TYPETABLEIDX_ANYTHING = -6,
	TYPETABLEIDX_STRUCT_LITERAL = -5,
	TYPETABLEIDX_UNSET = -1,

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

	TYPETABLEIDX_128,

	TYPETABLEIDX_COUNT
};
