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
	LITERALTYPE_STRING
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
		u32 valueIdx;
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

struct ASTBlock : ASTBase
{
	DynamicArray<ASTExpression, malloc, realloc> statements;
};

struct ASTType;
struct ASTStructMemberDeclaration : ASTBase
{
	String name;
	ASTType *astType;
	ASTExpression *value;
	bool isUsing;

	u64 typeTableIdx;
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

enum ASTTypeNodeType
{
	ASTTYPENODETYPE_INVALID,
	ASTTYPENODETYPE_IDENTIFIER,
	ASTTYPENODETYPE_POINTER,
	ASTTYPENODETYPE_STRUCT_DECLARATION,
	ASTTYPENODETYPE_ENUM_DECLARATION,
	ASTTYPENODETYPE_ARRAY
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
	};
};

struct ASTVariableDeclaration : ASTBase
{
	u32 valueIdx;
	ASTExpression *astInitialValue;
	ASTType *astType;
	bool isUsing;

	// TypeCheck
	s64 typeTableIdx;
};

struct Procedure;
struct ASTProcedureDeclaration : ASTBase
{
	s32 procedureIdx;
	ASTType *astReturnType;
	DynamicArray<ASTVariableDeclaration, malloc, realloc> astParameters;
};

struct ASTStaticDefinition : ASTBase
{
	String name;
	ASTExpression *expression;
};

struct ASTProcedureCall : ASTBase
{
	String name;
	s32 procedureIdx;
	DynamicArray<ASTExpression, malloc, realloc> arguments;
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
	ASTNODETYPE_IF,
	ASTNODETYPE_WHILE,
	ASTNODETYPE_FOR,
	ASTNODETYPE_BREAK,
	ASTNODETYPE_RETURN,
	ASTNODETYPE_DEFER,
	ASTNODETYPE_TYPEOF,
	ASTNODETYPE_CAST
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
		ASTIf ifNode;
		ASTWhile whileNode;
		ASTFor forNode;
		ASTReturn returnNode;
		ASTDefer deferNode;
		ASTTypeOf typeOfNode;
		ASTCast castNode;
	};

	// Filled in during type checking
	s64 typeTableIdx;
};
