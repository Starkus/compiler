struct Tokenizer
{
	const char *cursor;
	const char *end;
	u32 line;
	u32 fileIdx;
	const char *beginningOfLine;
};

struct Context;
struct ParseJobArgs
{
	Context *context;
	u32 fileIdx;
};

struct JobRequest
{
	u32 jobIdx;
	void (*proc)(void *);
	void *args;
};

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
	TOKEN_OP_ASSIGNMENT_Begin = 128,
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
	TOKEN_OP_ASSIGNMENT_End	= 140,

	TOKEN_OP_EQUALS			= 141,
	TOKEN_OP_NOT_EQUALS		= 142,
	TOKEN_OP_LESS_THAN		= 143,
	TOKEN_OP_LESS_THAN_OR_EQUAL = 144,
	TOKEN_OP_GREATER_THAN	= 145,
	TOKEN_OP_GREATER_THAN_OR_EQUAL = 146,
	TOKEN_OP_PLUS			= 147,
	TOKEN_OP_MINUS			= 148,
	TOKEN_OP_MULTIPLY		= 149,
	TOKEN_OP_DIVIDE			= 150,
	TOKEN_OP_MODULO			= 151,
	TOKEN_OP_SHIFT_LEFT		= 152,
	TOKEN_OP_SHIFT_RIGHT	= 153,
	TOKEN_OP_ARROW			= 154,
	TOKEN_OP_POINTER_TO		= 155, // Same as BITWISE_XOR!
	TOKEN_OP_DEREFERENCE	= 156,
	TOKEN_OP_VARIABLE_DECLARATION = 157,
	TOKEN_OP_VARIABLE_DECLARATION_STATIC = 158,
	TOKEN_OP_STATIC_DEF		= 159,
	TOKEN_OP_RANGE			= 160,
	TOKEN_OP_AND			= 161,
	TOKEN_OP_OR				= 162,
	TOKEN_OP_NOT			= 163,
	TOKEN_OP_BITWISE_AND	= 164,
	TOKEN_OP_BITWISE_OR		= 165,
	TOKEN_OP_BITWISE_XOR	= 155, // Same as POINTER_TO!
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

struct FatSourceLocation
{
	const char *beginingOfLine;
	u32 lineSize;
	u32 size;
	u32 line;
	u32 column;
};

struct SourceFile
{
	String name;
	SourceLocation includeLoc;
	const char *buffer;
	u64 size;
};

struct Token
{
	enum TokenType type;
	u16 size;
	SourceLocation loc;
};

void ParseJobProc(void *);
