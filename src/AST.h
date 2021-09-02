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

struct ASTBlock : ASTBase
{
	DynamicArray<ASTExpression, malloc, realloc> statements;
};

struct Type
{
	s64 typeTableIdx;
	s32 pointerLevels;
	u64 arrayCount;
};

struct ASTVariableDeclaration : ASTBase
{
	String typeName;
	ASTExpression *value;

	Variable *variable;
};

struct ASTStructMember : ASTBase
{
	String name;
	String typeName;
	ASTExpression *value;

	Type type;
};

struct ASTProcedureCall : ASTBase
{
	String name;
	bool isExternal;
	DynamicArray<ASTExpression, malloc, realloc> arguments;
};

struct ASTProcedureDeclaration : ASTBase
{
	String name;
	String returnTypeName;
	bool isVarargs;
	bool isExternal;
	DynamicArray<ASTVariableDeclaration, malloc, realloc> parameters;
	ASTExpression *body;

	Type returnType;
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
	DynamicArray<ASTStructMember, malloc, realloc> members;
};

struct TypeInfo;
struct ASTRoot : ASTBase
{
	ASTBlock block;
};

enum ASTNodeType
{
	ASTNODETYPE_INVALID,
	ASTNODETYPE_VARIABLE,
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
	Type type;
};
