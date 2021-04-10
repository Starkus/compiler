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
	TOKEN_LITERAL_CHAR,

	TOKEN_KEYWORD_BEGIN,
	TOKEN_KEYWORD_IF = TOKEN_KEYWORD_BEGIN,
	TOKEN_KEYWORD_ELSE,
	TOKEN_KEYWORD_RETURN,
	TOKEN_KEYWORD_WHILE,
	TOKEN_KEYWORD_BREAK,
	TOKEN_KEYWORD_STRUCT,
	TOKEN_KEYWORD_END,

	TOKEN_ASCII_BEGIN = '!', // 33
	TOKEN_ASCII_END = '~' + 1, // 127

	TOKEN_OP_BEGIN,
	TOKEN_OP_ASSIGNMENT = TOKEN_OP_BEGIN,
	TOKEN_OP_EQUALS,
	TOKEN_OP_PLUS,
	TOKEN_OP_MINUS,
	TOKEN_OP_MULTIPLY,
	TOKEN_OP_DIVIDE,
	TOKEN_OP_ARROW,
	TOKEN_OP_ADDRESSOF,
	TOKEN_OP_DEREFERENCE,
	TOKEN_OP_VARIABLE_DECLARATION,
	TOKEN_OP_STATIC_DEF,
	TOKEN_OP_AND,
	TOKEN_OP_OR,
	TOKEN_OP_NOT,
	TOKEN_OP_BITWISE_AND,
	TOKEN_OP_BITWISE_OR,
	TOKEN_OP_BITWISE_NOT,
	TOKEN_OP_MEMBER_ACCESS,
	TOKEN_OP_END,

	TOKEN_END_OF_FILE
};

struct Token
{
	s32 type;
	union
	{
		String string;
		struct
		{
			s64 size;
			const char *begin;
		};
	};

	const char *file;
	s64 line;
	s64 character;
};

const String TokenTypeToString(s32 type)
{
	switch (type)
	{
	case TOKEN_INVALID:
		return "<Invalid>"_s;
	case TOKEN_IDENTIFIER:
		return "<Identifier>"_s;

	case TOKEN_LITERAL_CHAR:
		return "<Literal char>"_s;
	case TOKEN_LITERAL_NUMBER:
		return "<Literal number>"_s;
	case TOKEN_LITERAL_STRING:
		return "<Literal string>"_s;

	case TOKEN_OP_ASSIGNMENT:
		return "< == >"_s;
	case TOKEN_OP_EQUALS:
		return "< = >"_s;
	case TOKEN_OP_PLUS:
		return "< + >"_s;
	case TOKEN_OP_MINUS:
		return "< - >"_s;
	case TOKEN_OP_MULTIPLY:
		return "< * >"_s;
	case TOKEN_OP_DIVIDE:
		return "< / >"_s;
	case TOKEN_OP_DEREFERENCE:
		return "< & >"_s;
	case TOKEN_OP_ARROW:
		return "< -> >"_s;
	case TOKEN_OP_VARIABLE_DECLARATION:
		return "< : >"_s;
	case TOKEN_OP_STATIC_DEF:
		return "< :: >"_s;

	case TOKEN_END_OF_FILE:
		return "<EOF>"_s;
	}

	if (type >= TOKEN_KEYWORD_BEGIN && type < TOKEN_KEYWORD_END)
		return "<Keyword>"_s;
	if (type >= TOKEN_OP_BEGIN && type < TOKEN_OP_END)
		return "<Operator>"_s;

	char *str = (char *)FrameAlloc(5);
	strcpy(str, "<'~'>");
	str[2] = (char)type;
	return { 5, str };
}

const String TokenToString(Token *token)
{
	if (token->type >= TOKEN_KEYWORD_BEGIN && token->type < TOKEN_KEYWORD_END)
		return token->string;

	return TokenTypeToString(token->type);
}

inline bool IsTokenKeyword(Token *token)
{
	return token->type >= TOKEN_KEYWORD_BEGIN && token->type < TOKEN_KEYWORD_END;
}

inline bool IsTokenOperator(Token *token)
{
	return token->type >= TOKEN_OP_BEGIN && token->type < TOKEN_OP_END;
}

s32 GetOperatorPrecedence(s32 op)
{
	switch (op)
	{
		case TOKEN_OP_ASSIGNMENT:
			return 0;
		case TOKEN_OP_EQUALS:
			return 1;
		case TOKEN_OP_PLUS:
		case TOKEN_OP_MINUS:
			return 2;
		case TOKEN_OP_MULTIPLY:
		case TOKEN_OP_DIVIDE:
			return 3;
		case TOKEN_OP_AND:
		case TOKEN_OP_OR:
		case TOKEN_OP_BITWISE_AND:
		case TOKEN_OP_BITWISE_OR:
			return 4;
		case TOKEN_OP_NOT:
		case TOKEN_OP_BITWISE_NOT:
			return 5;
		case TOKEN_OP_MEMBER_ACCESS:
			return 6;
		case TOKEN_OP_ADDRESSOF:
		case TOKEN_OP_DEREFERENCE:
			return 7;
	}
	return -1;
}

int EatWhitespace(Tokenizer *tokenizer)
{
	int nAdvanced = 0;
	while (*tokenizer->cursor && IsWhitespace(*tokenizer->cursor))
	{
		if (*tokenizer->cursor == '\n')
		{
			++tokenizer->line;
			tokenizer->beginningOfLine = tokenizer->cursor + 1;
		}
		++tokenizer->cursor;
		++nAdvanced;
	}
	return nAdvanced;
}

int EatRestOfLine(Tokenizer *tokenizer)
{
	int nAdvanced = 0;
	for (;; ++tokenizer->cursor)
	{
		char c = *tokenizer->cursor;
		if (!c)
			break;

		if (c == '\\')
		{
			++tokenizer->cursor;
		}
		else if (c == '\n')
		{
			++tokenizer->cursor;
			++tokenizer->line;
			tokenizer->beginningOfLine = tokenizer->cursor;
			break;
		}
		++nAdvanced;
	}
	return nAdvanced;
}

void ProcessCppComment(Tokenizer *tokenizer)
{
	EatRestOfLine(tokenizer);
}

void ProcessCComment(Tokenizer *tokenizer)
{
	tokenizer->cursor += 2;
	for (;; ++tokenizer->cursor)
	{
		if (!*tokenizer->cursor)
			break;

		if (*tokenizer->cursor == '*' && *(tokenizer->cursor + 1) == '/')
		{
			tokenizer->cursor += 2;
			break;
		}
	}
}

inline bool TokenIsStr(Token *token, const char *str)
{
	return token->type == TOKEN_IDENTIFIER &&
		token->size == (s64)strlen(str) &&
		strncmp(token->begin, str, token->size) == 0;
}

inline bool TokenIsEqual(Token *a, Token *b)
{
	if (a->type != b->type)
		return false;
	if (a->type == TOKEN_IDENTIFIER)
		return a->size == b->size && strncmp(a->begin, b->begin, a->size) == 0;
	return true;
}

Token ReadTokenAndAdvance(Tokenizer *tokenizer)
{
	Token result = {};

	EatWhitespace(tokenizer);

	while (*tokenizer->cursor == '/')
	{
		if (*(tokenizer->cursor + 1) == '/')
		{
			ProcessCppComment(tokenizer);
			EatWhitespace(tokenizer);
		}
		else if (*(tokenizer->cursor + 1) == '*')
		{
			ProcessCComment(tokenizer);
			EatWhitespace(tokenizer);
		}
		else
			break;
	}

	result.begin = tokenizer->cursor;
	result.line = tokenizer->line;
	result.character = tokenizer->cursor - tokenizer->beginningOfLine;

	if (!*tokenizer->cursor || tokenizer->cursor >= tokenizer->end)
	{
		result.type = TOKEN_END_OF_FILE;
		return result;
	}

	if (*tokenizer->cursor == '"')
	{
		result.type = TOKEN_LITERAL_STRING;
		++tokenizer->cursor;
		result.begin = tokenizer->cursor;

		while (*tokenizer->cursor && *tokenizer->cursor != '"')
		{
			if (*tokenizer->cursor == '\\')
			{
				++result.size;
				++tokenizer->cursor;
			}
			++result.size;
			++tokenizer->cursor;
		}
		++tokenizer->cursor;
	}
	else if (*tokenizer->cursor == '\'')
	{
		result.type = TOKEN_LITERAL_CHAR;
		++tokenizer->cursor;
		if (*tokenizer->cursor == '\\')
		{
			++tokenizer->cursor;
		}
		++tokenizer->cursor;
		ASSERT(*tokenizer->cursor == '\'');
		++tokenizer->cursor;
	}
	else if (IsAlpha(*tokenizer->cursor) || *tokenizer->cursor == '_')
	{
		result.type = TOKEN_IDENTIFIER;
		while (IsAlpha(*tokenizer->cursor) || IsNumeric(*tokenizer->cursor) || *tokenizer->cursor == '_')
		{
			++result.size;
			++tokenizer->cursor;
		}

		// Keywords
		if (TokenIsStr(&result, "if"))
			result.type = TOKEN_KEYWORD_IF;
		else if (TokenIsStr(&result, "else"))
			result.type = TOKEN_KEYWORD_ELSE;
		else if (TokenIsStr(&result, "return"))
			result.type = TOKEN_KEYWORD_RETURN;
		else if (TokenIsStr(&result, "while"))
			result.type = TOKEN_KEYWORD_WHILE;
		else if (TokenIsStr(&result, "break"))
			result.type = TOKEN_KEYWORD_BREAK;
		else if (TokenIsStr(&result, "struct"))
			result.type = TOKEN_KEYWORD_STRUCT;
	}
	else if (IsNumeric(*tokenizer->cursor))
	{
		result.type = TOKEN_LITERAL_NUMBER;
		bool done = false;
		if (*tokenizer->cursor == '0')
		{
			done = true;
			++result.size;
			++tokenizer->cursor;
			ASSERT(!IsNumeric(*tokenizer->cursor));
			if (*tokenizer->cursor == 'x' ||
					*tokenizer->cursor == 'X' ||
					*tokenizer->cursor == 'b' ||
					*tokenizer->cursor == '.')
			{
				done = false;
				++result.size;
				++tokenizer->cursor;
			}
		}
		if (!done)
		{
			while (IsNumericHex(*tokenizer->cursor) || *tokenizer->cursor == '.')
			{
				++result.size;
				++tokenizer->cursor;
			}
			if (*tokenizer->cursor == 'f')
			{
				++result.size;
				++tokenizer->cursor;
			}
		}
	}
	else if (*tokenizer->cursor >= TOKEN_ASCII_BEGIN && *tokenizer->cursor < TOKEN_ASCII_END)
	{
		char next = *(tokenizer->cursor + 1);
		switch (*tokenizer->cursor)
		{
		case '=':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_EQUALS;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_ASSIGNMENT;
		} break;
		case ':':
		{
			if (next == ':')
			{
				result.type = TOKEN_OP_STATIC_DEF;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_VARIABLE_DECLARATION;
		} break;
		case '+':
		{
			result.type = TOKEN_OP_PLUS;
		} break;
		case '-':
		{
			if (next == '>')
			{
				result.type = TOKEN_OP_ARROW;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_MINUS;
		} break;
		case '*':
		{
			result.type = TOKEN_OP_MULTIPLY;
		} break;
		case '/':
		{
			result.type = TOKEN_OP_DIVIDE;
		} break;
		case '&':
		{
			if (next == '&')
			{
				result.type = TOKEN_OP_AND;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_BITWISE_AND;
		} break;
		case '|':
		{
			if (next == '|')
			{
				result.type = TOKEN_OP_OR;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_BITWISE_OR;
		} break;
		case '!':
		{
			result.type = TOKEN_OP_NOT;
		} break;
		case '~':
		{
			result.type = TOKEN_OP_BITWISE_NOT;
		} break;
		case '.':
		{
			result.type = TOKEN_OP_MEMBER_ACCESS;
		} break;
		default:
		{
			result.type = *tokenizer->cursor;
		}
		};
		++tokenizer->cursor;
		result.size = tokenizer->cursor - result.begin;
	}
	else
	{
		result.type = TOKEN_INVALID;
		result.begin = tokenizer->cursor;
		result.size = 1;
		++tokenizer->cursor;
	}

	return result;
}

void TokenizeFile(const u8 *fileBuffer, u64 fileSize, DynamicArray<Token> &tokens,
		const char *filename, void *(*reallocFunc)(void *, u64))
{
	Tokenizer tokenizer = {};
	tokenizer.cursor = (char *)fileBuffer;
	tokenizer.beginningOfLine = (char *)fileBuffer;
	tokenizer.end = (char *)(fileBuffer + fileSize);
	tokenizer.line = 1; // Line numbers start at 1 not 0.
	while (true)
	{
		Token newToken = ReadTokenAndAdvance(&tokenizer);

		newToken.file = filename;
		*DynamicArrayAdd<Token>(&tokens, reallocFunc) = newToken;

		if (newToken.type == TOKEN_END_OF_FILE)
			break;
	}
}
