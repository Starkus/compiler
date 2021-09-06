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
struct ASTVariable : ASTBase
{
	String name;
	Variable *variable;
};

struct StructMember;
struct ASTStructMember : ASTVariable
{
	// Filled by type checker
	StructMember *structMember;
	// Note: saving this pointer assumes that once a struct is declared, further members cannot be
	// added to it or, at least, adding them don't trigger a reallocation of the preexisting
	// members.
};

struct ASTBlock : ASTBase
{
	DynamicArray<ASTExpression, malloc, realloc> statements;
};

enum ASTTypeNodeType
{
	ASTTYPENODETYPE_IDENTIFIER,
	ASTTYPENODETYPE_POINTER,
	ASTTYPENODETYPE_ARRAY
};
struct ASTType : ASTBase
{
	ASTTypeNodeType nodeType;
	union
	{
		String name;
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
};

struct ASTStructMemberDeclaration : ASTBase
{
	String name;
	ASTType *astType;
	ASTExpression *value;

	u64 typeTableIdx;
};

struct Procedure;
struct ASTProcedureDeclaration : ASTBase
{
	Procedure *procedure;
	ASTType *astReturnType;
	DynamicArray<ASTVariableDeclaration, malloc, realloc> astParameters;
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

struct ASTStruct : ASTBase
{
	String name;
	DynamicArray<ASTStructMemberDeclaration, malloc, realloc> members;
};

struct ASTRoot : ASTBase
{
	ASTBlock block;
};

enum ASTNodeType
{
	ASTNODETYPE_INVALID,
	ASTNODETYPE_VARIABLE,
	ASTNODETYPE_STRUCT_MEMBER,
	ASTNODETYPE_LITERAL,
	ASTNODETYPE_TYPE,
	ASTNODETYPE_BLOCK,
	ASTNODETYPE_UNARY_OPERATION,
	ASTNODETYPE_BINARY_OPERATION,
	ASTNODETYPE_VARIABLE_DECLARATION,
	ASTNODETYPE_STRUCT_DECLARATION,
	ASTNODETYPE_PROCEDURE_DECLARATION,
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
		ASTVariable variable;
		ASTStructMember structMember;
		ASTLiteral literal;
		ASTBlock block;
		ASTUnaryOperation unaryOperation;
		ASTBinaryOperation binaryOperation;
		ASTVariableDeclaration variableDeclaration;
		ASTProcedureDeclaration procedureDeclaration;
		ASTProcedureCall procedureCall;
		ASTIf ifNode;
		ASTWhile whileNode;
		ASTReturn returnNode;
		ASTDefer deferNode;
		ASTStruct structNode;
	};

	// Filled in during type checking
	u64 typeTableIdx;
};
