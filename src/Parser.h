struct Tokenizer
{
	const char *cursor;
	const char *end;
	s64 line;
	const char *beginningOfLine;
};

enum TokenType
{
	TOKEN_INVALID,
	TOKEN_IDENTIFIER,

	TOKEN_LITERAL_NUMBER,
	TOKEN_LITERAL_STRING,
	TOKEN_LITERAL_CHARACTER,

	TOKEN_KEYWORD_BEGIN,
	TOKEN_KEYWORD_IF = TOKEN_KEYWORD_BEGIN,
	TOKEN_KEYWORD_ELSE,
	TOKEN_KEYWORD_RETURN,
	TOKEN_KEYWORD_WHILE,
	TOKEN_KEYWORD_FOR,
	TOKEN_KEYWORD_BREAK,
	TOKEN_KEYWORD_STRUCT,
	TOKEN_KEYWORD_UNION,
	TOKEN_KEYWORD_ENUM,
	TOKEN_KEYWORD_TYPE,
	TOKEN_KEYWORD_EXTERNAL,
	TOKEN_KEYWORD_DEFER,
	TOKEN_KEYWORD_USING,
	TOKEN_KEYWORD_TYPEOF,
	TOKEN_KEYWORD_CAST,
	TOKEN_KEYWORD_END,

	TOKEN_ASCII_BEGIN = '!', // 33
	TOKEN_ASCII_END = '~' + 1, // 127

	TOKEN_OP_BEGIN,
	TOKEN_OP_ASSIGNMENT = TOKEN_OP_BEGIN,
	TOKEN_OP_EQUALS,
	TOKEN_OP_LESS_THAN,
	TOKEN_OP_LESS_THAN_OR_EQUAL,
	TOKEN_OP_GREATER_THAN,
	TOKEN_OP_GREATER_THAN_OR_EQUAL,
	TOKEN_OP_PLUS,
	TOKEN_OP_MINUS,
	TOKEN_OP_MULTIPLY,
	TOKEN_OP_DIVIDE,
	TOKEN_OP_MODULO,
	TOKEN_OP_PLUS_EQUALS,
	TOKEN_OP_MINUS_EQUALS,
	TOKEN_OP_MULTIPLY_EQUALS,
	TOKEN_OP_DIVIDE_EQUALS,
	TOKEN_OP_MODULO_EQUALS,
	TOKEN_OP_SHIFT_LEFT,
	TOKEN_OP_SHIFT_RIGHT,
	TOKEN_OP_ARROW,
	TOKEN_OP_POINTER_TO,
	TOKEN_OP_DEREFERENCE,
	TOKEN_OP_VARIABLE_DECLARATION,
	TOKEN_OP_VARIABLE_DECLARATION_STATIC,
	TOKEN_OP_STATIC_DEF,
	TOKEN_OP_RANGE,
	TOKEN_OP_AND,
	TOKEN_OP_OR,
	TOKEN_OP_NOT,
	TOKEN_OP_BITWISE_AND,
	TOKEN_OP_BITWISE_OR,
	TOKEN_OP_BITWISE_NOT,
	TOKEN_OP_MEMBER_ACCESS,
	TOKEN_OP_ARRAY_ACCESS,
	TOKEN_OP_END,

	TOKEN_END_OF_FILE
};

struct SourceLocation
{
	String file;
	s64 line;
	s64 character;
	s64 size;
	const u8 *fileBuffer; // @Improve: reference to file records instead of having both file name and buffer in all tokens
};

struct Token
{
	enum TokenType type;
	union
	{
		String string;
		struct
		{
			s64 size;
			const char *begin;
		};
	};

	SourceLocation loc;
};
