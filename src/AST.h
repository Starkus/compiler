struct ASTExpression;

struct ASTBase
{
	const char *file;
	s64 line;
	s64 character;
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

struct ASTVariable : ASTBase
{
	String name;
};

struct TCVariable;
struct ASTBlock : ASTBase
{
	DynamicArray<ASTExpression> statements;
};

struct Type
{
	s64 typeTableIdx;
	s32 pointerLevels;
	u64 arrayCount;
};

struct ASTVariableDeclaration : ASTBase
{
	String name;
	String typeName;
	ASTExpression *value;

	Type type;
};

struct ASTProcedureCall : ASTBase
{
	String name;
	DynamicArray<ASTExpression> arguments;
};

struct ASTProcedureDeclaration : ASTBase
{
	String name;
	String returnTypeName;
	DynamicArray<ASTVariableDeclaration> parameters;
	ASTExpression *body;

	Type returnType;
};

struct ASTIf : ASTBase
{
	ASTExpression *condition;
	ASTExpression *body;
	ASTExpression *elseNode;
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

struct ASTStruct : ASTBase
{
	String name;
	DynamicArray<ASTVariableDeclaration> members;
};

struct TypeInfo;
struct ASTRoot : ASTBase
{
	ASTBlock block;
	DynamicArray<TypeInfo> typeTable;
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
	ASTNODETYPE_RETURN
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
		ASTStruct structNode;
	};

	// Filled in during type checking
	Type type;
};
