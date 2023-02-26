enum TokenType : u8
{
	TOKEN_INVALID			= 0,
	TOKEN_IDENTIFIER		= 1,

	TOKEN_LITERAL_NUMBER	= 2,
	TOKEN_LITERAL_STRING	= 3,
	TOKEN_LITERAL_CHARACTER	= 4,

	TOKEN_KEYWORD_Begin		= 5,
	TOKEN_KEYWORD_IF		= 5,
	TOKEN_KEYWORD_ELSE		= 6,
	TOKEN_KEYWORD_RETURN	= 7,
	TOKEN_KEYWORD_WHILE		= 8,
	TOKEN_KEYWORD_FOR		= 9,
	TOKEN_KEYWORD_BREAK		= 10,
	TOKEN_KEYWORD_CONTINUE	= 11,
	TOKEN_KEYWORD_REMOVE	= 12,
	TOKEN_KEYWORD_STRUCT	= 13,
	TOKEN_KEYWORD_UNION		= 14,
	TOKEN_KEYWORD_ENUM		= 15,
	TOKEN_KEYWORD_DEFER		= 24,
	TOKEN_KEYWORD_USING		= 25,
	TOKEN_KEYWORD_TYPEOF	= 26,
	TOKEN_KEYWORD_SIZEOF	= 27,
	TOKEN_KEYWORD_CAST		= 28,
	TOKEN_KEYWORD_End		= 32,

	TOKEN_ASCII_Begin		= '!', // 33
	TOKEN_ASCII_End			= '~', // 126

	TOKEN_OP_Begin			= 128,
	TOKEN_OP_Statement_Begin	= 128,
	TOKEN_OP_ASSIGNMENT_Begin	= 128,
	TOKEN_OP_ASSIGNMENT				= 128,
	TOKEN_OP_ASSIGNMENT_PLUS		= 129,
	TOKEN_OP_ASSIGNMENT_MINUS		= 130,
	TOKEN_OP_ASSIGNMENT_MULTIPLY	= 131,
	TOKEN_OP_ASSIGNMENT_DIVIDE		= 132,
	TOKEN_OP_ASSIGNMENT_MODULO		= 133,
	TOKEN_OP_ASSIGNMENT_SHIFT_LEFT	= 134,
	TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT	= 135,
	TOKEN_OP_ASSIGNMENT_OR			= 136,
	TOKEN_OP_ASSIGNMENT_AND			= 137,
	TOKEN_OP_ASSIGNMENT_BITWISE_OR	= 138,
	TOKEN_OP_ASSIGNMENT_BITWISE_XOR	= 139,
	TOKEN_OP_ASSIGNMENT_BITWISE_AND	= 140,
	TOKEN_OP_ASSIGNMENT_End		= 140,

	TOKEN_OP_VARIABLE_DECLARATION = 141,
	TOKEN_OP_VARIABLE_DECLARATION_STATIC = 142,
	TOKEN_OP_STATIC_DEF		= 143,
	TOKEN_OP_Statement_End	= 143,

	TOKEN_OP_EQUALS			= 144,
	TOKEN_OP_NOT_EQUALS		= 145,
	TOKEN_OP_LESS_THAN		= 146,
	TOKEN_OP_LESS_THAN_OR_EQUAL = 147,
	TOKEN_OP_GREATER_THAN	= 148,
	TOKEN_OP_GREATER_THAN_OR_EQUAL = 149,
	TOKEN_OP_PLUS			= 150,
	TOKEN_OP_MINUS			= 151,
	TOKEN_OP_MULTIPLY		= 152,
	TOKEN_OP_DIVIDE			= 153,
	TOKEN_OP_MODULO			= 154,
	TOKEN_OP_SHIFT_LEFT		= 155,
	TOKEN_OP_SHIFT_RIGHT	= 156,
	TOKEN_OP_ARROW			= 157,
	TOKEN_OP_POINTER_TO		= 158, // Same as BITWISE_XOR!
	TOKEN_OP_DEREFERENCE	= 159,
	TOKEN_OP_RANGE			= 160,
	TOKEN_OP_AND			= 161,
	TOKEN_OP_OR				= 162,
	TOKEN_OP_NOT			= 163,
	TOKEN_OP_BITWISE_AND	= 164,
	TOKEN_OP_BITWISE_OR		= 165,
	TOKEN_OP_BITWISE_XOR	= 158, // Same as POINTER_TO!
	TOKEN_OP_BITWISE_NOT	= 166,
	TOKEN_OP_MEMBER_ACCESS	= 167,
	TOKEN_OP_ARRAY_ACCESS	= 168,
	TOKEN_OP_End			= 168,

	TOKEN_DIRECTIVE_Begin		= 169,
	TOKEN_DIRECTIVE_TYPE		= 169,
	TOKEN_DIRECTIVE_ALIAS		= 170,
	TOKEN_DIRECTIVE_INLINE		= 171,
	TOKEN_DIRECTIVE_NOINLINE	= 172,
	TOKEN_DIRECTIVE_EXTERNAL	= 173,
	TOKEN_DIRECTIVE_EXPORT		= 174,
	TOKEN_DIRECTIVE_CALLING_CONVENTION = 175,
	TOKEN_DIRECTIVE_INTRINSIC	= 176,
	TOKEN_DIRECTIVE_OPERATOR	= 177,
	TOKEN_DIRECTIVE_INCLUDE		= 183,
	TOKEN_DIRECTIVE_LINKLIB		= 184,
	TOKEN_DIRECTIVE_IF			= 185,
	TOKEN_DIRECTIVE_DEFINED		= 186,
	TOKEN_DIRECTIVE_BREAK		= 187,
	TOKEN_DIRECTIVE_CSTR		= 188,
	TOKEN_DIRECTIVE_RUN			= 189,
	TOKEN_DIRECTIVE_ALIGN		= 190,
	TOKEN_DIRECTIVE_End			= 191,

	TOKEN_INVALID_DIRECTIVE = 240,
	TOKEN_END_OF_FILE		= 255,
};

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
	LITERALTYPE_GROUP,
	LITERALTYPE_CSTR
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

enum ConstantType
{
	CONSTANTTYPE_INVALID = 0,
	CONSTANTTYPE_INTEGER,
	CONSTANTTYPE_FLOATING,
	CONSTANTTYPE_GROUP,
	CONSTANTTYPE_STRING
};
struct Constant
{
	ConstantType type;
	union {
		s64 valueAsInt;
		f64 valueAsFloat;
		Array<Constant, LinearAllocator> valueAsGroup;
		String valueAsString;
	};
	u32 typeTableIdx;
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
	ASTExpression *alignExp;
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
	DynamicArray<ASTType *, LinearAllocator> astReturnTypes;

	String varargsName;
	SourceLocation varargsLoc;
	bool isVarargs;
	CallingConvention callingConvention;

	// Type check
	DynamicArray<u32, LinearAllocator> returnTypeIndices;
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
	ASTTYPENODETYPE_PROCEDURE,
	ASTTYPENODETYPE_TYPE_ARGUMENT_DECLARATION
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
			ASTExpression *arrayCountExp;
		};
		ASTProcedurePrototype procedurePrototype;
	};
};

struct ASTVariableDeclaration : ASTBase
{
	u32 nameCount;
	// Either embedded or external array of names. Depends on nameCount.
	union {
		String name;
		String *arrayOfNames;
	};
	ASTExpression *astInitialValue;
	ASTType *astType;
	bool isInline;
	bool isStatic;
	bool isExternal;

	// TypeCheck
	u32 anonymousVariableValueIdx;
	u32 specifiedTypeIdx;
	// These also depend on nameCount
	union {
		u32 valueIdx;
		u32 *arrayOfValueIndices;
	};
	union {
		u32 typeIdx;
		u32 *arrayOfTypeIndices;
	};
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
	u32 nameCount;
	union {
		String name;
		String *arrayOfNames;
	};
	ASTExpression *expression;

	// Type check
	u32 staticDefinitionIdx;
};

enum ProcedureCallType
{
	CALLTYPE_STATIC,
	CALLTYPE_ASTEXPRESSION
};
enum CallInlineType
{
	CALLINLINETYPE_DONT_CARE,
	CALLINLINETYPE_ALWAYS_INLINE,
	CALLINLINETYPE_NEVER_INLINE,
};
struct ASTProcedureCall : ASTBase
{
	ASTExpression *procedureExpression;
	DynamicArray<ASTExpression *, LinearAllocator> arguments;
	CallInlineType inlineType;

	// Type check
	u32 procedureTypeIdx;
	ProcedureCallType callType;
	u32 procedureIdx; // Only for static calls
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

enum CompilerBreakpointType
{
	COMPILERBREAKPOINT_Invalid = 0,
	COMPILERBREAKPOINT_PARSER,
	COMPILERBREAKPOINT_TYPE_CHECKER,
	COMPILERBREAKPOINT_IR_GEN,
	COMPILERBREAKPOINT_CODE_GEN,
	COMPILERBREAKPOINT_Count
};
String compilerBreakpointTypeStrings[] =
{
	"parser"_s,
	"typechecker"_s,
	"irgen"_s,
	"codegen"_s
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

struct ASTRun : ASTBase
{
	ASTExpression *expression;
	Constant result;
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

struct ASTMultipleExpressions : ASTBase
{
	DynamicArray<ASTExpression *, LinearAllocator> array;
};

struct ASTRoot : ASTBase
{
	ASTBlock block;
};

enum ASTNodeType : u8
{
	ASTNODETYPE_INVALID,

	ASTNODETYPE_Statement_Begin,
	ASTNODETYPE_BLOCK,
	ASTNODETYPE_INCLUDE,
	ASTNODETYPE_LINKLIB,
	ASTNODETYPE_VARIABLE_DECLARATION,
	ASTNODETYPE_STATIC_DEFINITION,
	ASTNODETYPE_OPERATOR_OVERLOAD,
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
	ASTNODETYPE_COMPILER_BREAKPOINT,
	ASTNODETYPE_Statement_End,

	ASTNODETYPE_Expression_Begin,
	ASTNODETYPE_IDENTIFIER,
	ASTNODETYPE_LITERAL,
	ASTNODETYPE_TYPE,
	ASTNODETYPE_ALIAS,
	ASTNODETYPE_MULTIPLE_EXPRESSIONS,
	ASTNODETYPE_UNARY_OPERATION,
	ASTNODETYPE_BINARY_OPERATION,
	ASTNODETYPE_PROCEDURE_DECLARATION,
	ASTNODETYPE_PROCEDURE_CALL,
	ASTNODETYPE_INTRINSIC,
	ASTNODETYPE_TYPEOF,
	ASTNODETYPE_SIZEOF,
	ASTNODETYPE_CAST,
	ASTNODETYPE_RUN,
	ASTNODETYPE_DEFINED,
	ASTNODETYPE_GARBAGE,
	ASTNODETYPE_Expression_End
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
		ASTRun runNode;
		ASTDefined definedNode;
		ASTCast castNode;
		ASTMultipleExpressions multipleExpressions;
		CompilerBreakpointType compilerBreakpointType;
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
	TYPETABLEIDX_BuiltinStructsBegin = TYPETABLEIDX_128,

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
	TYPETABLEIDX_TYPE_INFO_PROCEDURE_STRUCT,
	TYPETABLEIDX_TYPE_INFO_ALIAS_STRUCT,

	TYPETABLEIDX_BuiltinStructsEnd,
	TYPETABLEIDX_Count = TYPETABLEIDX_BuiltinStructsEnd
};
