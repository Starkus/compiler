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
	s32 op;
	ASTExpression *expression;
};

struct ASTBinaryOperation : ASTBase
{
	s32 op;
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

struct ASTType : ASTBase
{
	String name;
	bool isPointer;

	s64 typeTableIdx = -1;
};

struct ASTVariableDeclaration : ASTBase
{
	String name;
	ASTType *type;
	ASTExpression *value;
};

struct ASTProcedureCall : ASTBase
{
	String name;
	DynamicArray<ASTExpression> arguments;
};

struct ASTProcedureDeclaration : ASTBase
{
	String name;
	ASTType *returnType;
	DynamicArray<ASTVariableDeclaration> parameters;
	ASTExpression *body;
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
	ASTNodeType type;
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
	s64 typeTableIdx = -1;
};
