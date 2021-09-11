struct ASTExpression;

struct ASTBase
{
	SourceLocation loc;
};

enum LiteralType
{
	LITERALTYPE_INTEGER,
	LITERALTYPE_FLOATING,
	LITERALTYPE_STRING
};
struct ASTLiteral : ASTBase
{
	LiteralType type;
	union
	{
		s64 integer;
		f64 floating;
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

struct Variable;
struct StructMember;
struct StaticDefinition;
enum NameType
{
	NAMETYPE_VARIABLE,
	NAMETYPE_STRUCT_MEMBER,
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
		Variable *variable;
		struct
		{
			Variable *base;
			Array<StructMember *> offsets;
		} structMemberInfo;
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
	Variable *variable;
	ASTExpression *value;
	ASTType *astType;
	bool isUsing;
};

struct Procedure;
struct ASTProcedureDeclaration : ASTBase
{
	Procedure *procedure;
	ASTType *astReturnType;
	DynamicArray<ASTVariableDeclaration, malloc, realloc> astParameters;
};

struct ASTStaticDefinition : ASTBase
{
	String name;
	union
	{
		ASTExpression *expression;
	};
};

struct ASTProcedureCall : ASTBase
{
	String name;
	Procedure *procedure;
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

struct ASTReturn : ASTBase
{
	ASTExpression *expression;
};

struct ASTDefer : ASTBase
{
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
	ASTNODETYPE_BREAK,
	ASTNODETYPE_RETURN,
	ASTNODETYPE_DEFER
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
		ASTReturn returnNode;
		ASTDefer deferNode;
	};

	// Filled in during type checking
	s64 typeTableIdx;
};
