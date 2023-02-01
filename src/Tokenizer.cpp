inline bool IsTokenKeyword(Token *token)
{
	return token->type >= TOKEN_KEYWORD_Begin && token->type <= TOKEN_KEYWORD_End;
}

inline bool IsOperatorToken(enum TokenType type)
{
	return type >= TOKEN_OP_Begin && type <= TOKEN_OP_End;
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

		if (*tokenizer->cursor == '\\')
		{
		}
		else if (*tokenizer->cursor == '\n')
		{
			++tokenizer->line;
			tokenizer->beginningOfLine = tokenizer->cursor;
		}
		else if (*tokenizer->cursor == '*' && *(tokenizer->cursor + 1) == '/')
		{
			tokenizer->cursor += 2;
			break;
		}
	}
}

String TokenToString(Context *context, Token token)
{
	SourceFile sourceFile = context->sourceFiles[token.loc.fileIdx];
	String result = { token.size, (const char *)sourceFile.buffer + token.loc.character };
	if (token.type == TOKEN_LITERAL_STRING || token.type == TOKEN_LITERAL_CHARACTER)
	{
		result.size -= 2;
		++result.data;
	}
	return result;
}

String TokenTypeToString(s32 type)
{
	switch (type)
	{
	case TOKEN_INVALID:
		return "<Invalid>"_s;
	case TOKEN_IDENTIFIER:
		return "<Identifier>"_s;

	case TOKEN_LITERAL_CHARACTER:
		return "<Literal char>"_s;
	case TOKEN_LITERAL_NUMBER:
		return "<Literal number>"_s;
	case TOKEN_LITERAL_STRING:
		return "<Literal string>"_s;

	case TOKEN_OP_ASSIGNMENT:
		return "< = >"_s;
	case TOKEN_OP_ASSIGNMENT_PLUS:
		return "< += >"_s;
	case TOKEN_OP_ASSIGNMENT_MINUS:
		return "< -= >"_s;
	case TOKEN_OP_ASSIGNMENT_MULTIPLY:
		return "< *= >"_s;
	case TOKEN_OP_ASSIGNMENT_DIVIDE:
		return "< /= >"_s;
	case TOKEN_OP_ASSIGNMENT_MODULO:
		return "< %= >"_s;
	case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
		return "< <<= >"_s;
	case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
		return "< >>= >"_s;
	case TOKEN_OP_ASSIGNMENT_OR:
		return "< ||= >"_s;
	case TOKEN_OP_ASSIGNMENT_AND:
		return "< &&= >"_s;
	case TOKEN_OP_EQUALS:
		return "< == >"_s;
	case TOKEN_OP_GREATER_THAN:
		return "< > >"_s;
	case TOKEN_OP_GREATER_THAN_OR_EQUAL:
		return "< >= >"_s;
	case TOKEN_OP_LESS_THAN:
		return "< < >"_s;
	case TOKEN_OP_LESS_THAN_OR_EQUAL:
		return "< <= >"_s;
	case TOKEN_OP_PLUS:
		return "< + >"_s;
	case TOKEN_OP_MINUS:
		return "< - >"_s;
	case TOKEN_OP_MULTIPLY:
		return "< * >"_s;
	case TOKEN_OP_DIVIDE:
		return "< / >"_s;
	case TOKEN_OP_MODULO:
		return "< % >"_s;
	case TOKEN_OP_SHIFT_LEFT:
		return "< << >"_s;
	case TOKEN_OP_SHIFT_RIGHT:
		return "< >> >"_s;
	case TOKEN_OP_POINTER_TO:
		return "< ^ >"_s;
	case TOKEN_OP_DEREFERENCE:
		return "< @ >"_s;
	case TOKEN_OP_ARRAY_ACCESS:
		return "< [] >"_s;
	case TOKEN_OP_ARROW:
		return "< -> >"_s;
	case TOKEN_OP_VARIABLE_DECLARATION:
		return "< : >"_s;
	case TOKEN_OP_VARIABLE_DECLARATION_STATIC:
		return "< :s >"_s;
	case TOKEN_OP_STATIC_DEF:
		return "< :: >"_s;
	case TOKEN_OP_RANGE:
		return "< .. >"_s;
	}

	if (type >= TOKEN_KEYWORD_Begin && type <= TOKEN_KEYWORD_End)
		return "<Keyword>"_s;
	if (type >= TOKEN_DIRECTIVE_Begin && type <= TOKEN_DIRECTIVE_End)
		return "<Directive>"_s;
	if (type >= TOKEN_OP_Begin && type <= TOKEN_OP_End)
		return "<Operator>"_s;

	char *str = (char *)ThreadAllocator::Alloc(5, 1);
	strncpy(str, "<'~'>", 5);
	str[2] = (char)type;
	return { 5, str };
}

inline String TokenToStringOrType(Context *context, Token token)
{
	if (token.type >= TOKEN_KEYWORD_Begin && token.type <= TOKEN_KEYWORD_End)
		return TokenToString(context, token);

	return TokenTypeToString(token.type);
}

inline bool TokenIsStr(Context *context, Token token, String str)
{
	if (token.type != TOKEN_IDENTIFIER || token.size != str.size)
		return false;

	String tokenStr = TokenToString(context, token);
	return StringEquals(tokenStr, str);
}

inline bool TokenIsEqual(Context *context, Token a, Token b)
{
	if (a.type != b.type)
		return false;
	if (a.type != TOKEN_IDENTIFIER)
		return true;
	String aStr = TokenToString(context, a);
	String bStr = TokenToString(context, b);
	return StringEquals(aStr, bStr);
}

enum TokenType CalculateTokenType(Context *context, const Tokenizer *tokenizer)
{
	const char * const begin = tokenizer->cursor;

	if (*begin == '"')
		return TOKEN_LITERAL_STRING;
	else if (*begin == '\'')
		return TOKEN_LITERAL_CHARACTER;
	else if (IsAlpha(*begin) || *begin == '_')
	{
		String identifier = { 1, begin };
		for (const char *scan = begin + 1; IsAlpha(*scan) || IsNumeric(*scan) || *scan == '_'; ++scan)
			++identifier.size;

		// Keywords
		if (StringEquals(identifier, "if"_s))
			return TOKEN_KEYWORD_IF;
		else if (StringEquals(identifier, "else"_s))
			return TOKEN_KEYWORD_ELSE;
		else if (StringEquals(identifier, "return"_s))
			return TOKEN_KEYWORD_RETURN;
		else if (StringEquals(identifier, "while"_s))
			return TOKEN_KEYWORD_WHILE;
		else if (StringEquals(identifier, "for"_s))
			return TOKEN_KEYWORD_FOR;
		else if (StringEquals(identifier, "break"_s))
			return TOKEN_KEYWORD_BREAK;
		else if (StringEquals(identifier, "continue"_s))
			return TOKEN_KEYWORD_CONTINUE;
		else if (StringEquals(identifier, "remove"_s))
			return TOKEN_KEYWORD_REMOVE;
		else if (StringEquals(identifier, "struct"_s))
			return TOKEN_KEYWORD_STRUCT;
		else if (StringEquals(identifier, "union"_s))
			return TOKEN_KEYWORD_UNION;
		else if (StringEquals(identifier, "enum"_s))
			return TOKEN_KEYWORD_ENUM;
		else if (StringEquals(identifier, "defer"_s))
			return TOKEN_KEYWORD_DEFER;
		else if (StringEquals(identifier, "using"_s))
			return TOKEN_KEYWORD_USING;
		else if (StringEquals(identifier, "typeof"_s))
			return TOKEN_KEYWORD_TYPEOF;
		else if (StringEquals(identifier, "sizeof"_s))
			return TOKEN_KEYWORD_SIZEOF;
		else if (StringEquals(identifier, "cast"_s))
			return TOKEN_KEYWORD_CAST;

		return TOKEN_IDENTIFIER;
	}
	else if (IsNumeric(*begin))
		return TOKEN_LITERAL_NUMBER;
	else if (*begin >= TOKEN_ASCII_Begin && *begin <= TOKEN_ASCII_End)
	{
		char next = *(begin + 1);
		switch (*begin)
		{
		case '#':
		{
			String directive = { 1, begin };
			for (const char *scan = begin + 1; IsAlpha(*scan); ++scan)
				++directive.size;

			if (StringEquals(directive, "#include"_s))
				return TOKEN_DIRECTIVE_INCLUDE;
			else if (StringEquals(directive, "#linklib"_s))
				return TOKEN_DIRECTIVE_LINKLIB;
			else if (StringEquals(directive, "#type"_s))
				return TOKEN_DIRECTIVE_TYPE;
			else if (StringEquals(directive, "#alias"_s))
				return TOKEN_DIRECTIVE_ALIAS;
			else if (StringEquals(directive, "#inline"_s))
				return TOKEN_DIRECTIVE_INLINE;
			else if (StringEquals(directive, "#noinline"_s))
				return TOKEN_DIRECTIVE_NOINLINE;
			else if (StringEquals(directive, "#external"_s))
				return TOKEN_DIRECTIVE_EXTERNAL;
			else if (StringEquals(directive, "#export"_s))
				return TOKEN_DIRECTIVE_EXPORT;
			else if (StringEquals(directive, "#convention"_s))
				return TOKEN_DIRECTIVE_CALLING_CONVENTION;
			else if (StringEquals(directive, "#intrinsic"_s))
				return TOKEN_DIRECTIVE_INTRINSIC;
			else if (StringEquals(directive, "#operator"_s))
				return TOKEN_DIRECTIVE_OPERATOR;
			else if (StringEquals(directive, "#if"_s))
				return TOKEN_DIRECTIVE_IF;
			else if (StringEquals(directive, "#defined"_s))
				return TOKEN_DIRECTIVE_DEFINED;
			else if (StringEquals(directive, "#break"_s))
				return TOKEN_DIRECTIVE_BREAK;
			else if (StringEquals(directive, "#cstr"_s))
				return TOKEN_DIRECTIVE_CSTR;
			else if (StringEquals(directive, "#run"_s))
				return TOKEN_DIRECTIVE_RUN;
			else if (StringEquals(directive, "#align"_s))
				return TOKEN_DIRECTIVE_ALIGN;
			else
				return TOKEN_INVALID_DIRECTIVE;
		} break;
		case '=':
		{
			if (next == '=')
				return TOKEN_OP_EQUALS;
			else
				return TOKEN_OP_ASSIGNMENT;
		} break;
		case '<':
		{
			if (next == '=')
				return TOKEN_OP_LESS_THAN_OR_EQUAL;
			else if (next == '<')
				if (*(begin + 2) == '=')
					return TOKEN_OP_ASSIGNMENT_SHIFT_LEFT;
				else
					return TOKEN_OP_SHIFT_LEFT;
			else
				return TOKEN_OP_LESS_THAN;
		} break;
		case '>':
		{
			if (next == '=')
				return TOKEN_OP_GREATER_THAN_OR_EQUAL;
			else if (next == '>')
				if (*(begin + 2) == '=')
					return TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT;
				else
					return TOKEN_OP_SHIFT_RIGHT;
			else
				return TOKEN_OP_GREATER_THAN;
		} break;
		case ':':
		{
			if (next == ':')
				return TOKEN_OP_STATIC_DEF;
			else if (next == 's')
				return TOKEN_OP_VARIABLE_DECLARATION_STATIC;
			else
				return TOKEN_OP_VARIABLE_DECLARATION;
		} break;
		case '+':
		{
			if (next == '=')
				return TOKEN_OP_ASSIGNMENT_PLUS;
			else
				return TOKEN_OP_PLUS;
		} break;
		case '-':
		{
			if (next == '=')
				return TOKEN_OP_ASSIGNMENT_MINUS;
			else if (next == '>')
				return TOKEN_OP_ARROW;
			else
				return TOKEN_OP_MINUS;
		} break;
		case '*':
		{
			if (next == '=')
				return TOKEN_OP_ASSIGNMENT_MULTIPLY;
			else
				return TOKEN_OP_MULTIPLY;
		} break;
		case '/':
		{
			if (next == '=')
				return TOKEN_OP_ASSIGNMENT_DIVIDE;
			else
				return TOKEN_OP_DIVIDE;
		} break;
		case '%':
		{
			if (next == '=')
				return TOKEN_OP_ASSIGNMENT_MODULO;
			else
				return TOKEN_OP_MODULO;
		} break;
		case '|':
		{
			if (next == '|')
				if (*(begin + 2) == '=')
					return TOKEN_OP_ASSIGNMENT_OR;
				else
					return TOKEN_OP_OR;
			else if (next == '=')
				return TOKEN_OP_ASSIGNMENT_BITWISE_OR;
			else
				return TOKEN_OP_BITWISE_OR;
		} break;
		case '&':
		{
			if (next == '&')
				if (*(begin + 2) == '=')
					return TOKEN_OP_ASSIGNMENT_AND;
				else
					return TOKEN_OP_AND;
			else if (next == '=')
				return TOKEN_OP_ASSIGNMENT_BITWISE_AND;
			else
				return TOKEN_OP_BITWISE_AND;
		} break;
		case '^':
		{
			if (next == '=')
				return TOKEN_OP_ASSIGNMENT_BITWISE_XOR;
			else
				return TOKEN_OP_POINTER_TO;
		} break;
		case '@':
		{
			return TOKEN_OP_DEREFERENCE;
		} break;
		case '[':
		{
			return TOKEN_OP_ARRAY_ACCESS;
		} break;
		case '!':
		{
			if (next == '=')
				return TOKEN_OP_NOT_EQUALS;
			else
				return TOKEN_OP_NOT;
		} break;
		case '~':
		{
			return TOKEN_OP_BITWISE_NOT;
		} break;
		case '.':
		{
			if (next == '.')
				return TOKEN_OP_RANGE;
			else if (IsNumeric(next))
				return TOKEN_LITERAL_NUMBER;
			else
				return TOKEN_OP_MEMBER_ACCESS;
		} break;
		default:
		{
			return (enum TokenType)*begin;
		}
		};
	}
	else
		return TOKEN_INVALID;
}

u16 CalculateTokenSize(Context *context, const Tokenizer *tokenizer, enum TokenType tokenType)
{
	const char *begin = tokenizer->cursor;

	switch (tokenType)
	{
	case TOKEN_LITERAL_STRING:
	{
		const char *scan = begin + 1;
		while (scan < tokenizer->end && *scan && *scan != '"')
		{
			if (*scan == '\\')
				++scan;
			++scan;
		}
		ASSERT((1 + scan - begin) <= U16_MAX);
		return (u16)(1 + scan - begin);
	}
	case TOKEN_LITERAL_CHARACTER:
	{
		if (*(begin + 1) == '\\')
			return 4;
		return 3;
	}
	case TOKEN_IDENTIFIER:
	{
		u16 size = 0;
		for (const char *scan = begin;
				IsAlpha(*scan) || IsNumeric(*scan) || *scan == '_';
				++scan)
			++size;

		return size;
	}
	case TOKEN_INVALID_DIRECTIVE:
	{
		ASSERT(*begin == '#');
		u16 size = 1;
		for (const char *scan = begin + 1;
				IsAlpha(*scan) || IsNumeric(*scan) || *scan == '_';
				++scan)
			++size;

		return size;
	}
	case TOKEN_LITERAL_NUMBER:
	{
		const char *scan = begin;
		if (*scan == '-')
			++scan;
		if (*scan == '0')
		{
			++scan;
			switch (*scan)
			{
			case 'x':
			case 'X':
			{
				++scan;
				while (IsNumericHex(*scan))
					++scan;
			} goto numberDone;
			case 'b':
			{
				++scan;
				while ((*scan & 0xFE) == '0') // 0 or 1
					++scan;
			} goto numberDone;
			}
		}
		// Normal base parsing
		{
			bool foundADot = false;
			bool foundAnE = false;
			while (true)
			{
				if ((*scan & (~0x20)) == 'E') // E or e
				{
					if (foundAnE)
						goto numberDone;
					foundAnE = true;
					++scan;
					if (*scan == '-' || *scan == '+')
						++scan;
				}

				if (*scan == '.')
				{
					if (foundADot)
						goto numberDone;
					else if (*(scan + 1) == '.')
						// .. is an operator
						goto numberDone;

					foundADot = true;
				}
				else if (!IsNumeric(*scan))
					goto numberDone;

				++scan;
			}
		}
	numberDone:
		ASSERT(scan - begin <= U16_MAX);
		return (u16)(scan - begin);
	}
	case TOKEN_KEYWORD_IF:
	case TOKEN_OP_EQUALS:
	case TOKEN_OP_SHIFT_LEFT:
	case TOKEN_OP_LESS_THAN_OR_EQUAL:
	case TOKEN_OP_GREATER_THAN_OR_EQUAL:
	case TOKEN_OP_SHIFT_RIGHT:
	case TOKEN_OP_STATIC_DEF:
	case TOKEN_OP_VARIABLE_DECLARATION_STATIC:
	case TOKEN_OP_ARROW:
	case TOKEN_OP_ASSIGNMENT_PLUS:
	case TOKEN_OP_ASSIGNMENT_MINUS:
	case TOKEN_OP_ASSIGNMENT_MULTIPLY:
	case TOKEN_OP_ASSIGNMENT_DIVIDE:
	case TOKEN_OP_ASSIGNMENT_MODULO:
	case TOKEN_OP_ASSIGNMENT_BITWISE_OR:
	case TOKEN_OP_ASSIGNMENT_BITWISE_AND:
	case TOKEN_OP_ASSIGNMENT_BITWISE_XOR:
	case TOKEN_OP_OR:
	case TOKEN_OP_AND:
	case TOKEN_OP_NOT_EQUALS:
	case TOKEN_OP_RANGE:
		return 2;
	case TOKEN_DIRECTIVE_IF:
	case TOKEN_KEYWORD_FOR:
	case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
	case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
	case TOKEN_OP_ASSIGNMENT_OR:
	case TOKEN_OP_ASSIGNMENT_AND:
		return 3;
	case TOKEN_DIRECTIVE_RUN:
	case TOKEN_KEYWORD_ELSE:
	case TOKEN_KEYWORD_ENUM:
	case TOKEN_KEYWORD_CAST:
		return 4;
	case TOKEN_DIRECTIVE_TYPE:
	case TOKEN_DIRECTIVE_CSTR:
	case TOKEN_KEYWORD_WHILE:
	case TOKEN_KEYWORD_BREAK:
	case TOKEN_KEYWORD_UNION:
	case TOKEN_KEYWORD_DEFER:
	case TOKEN_KEYWORD_USING:
		return 5;
	case TOKEN_DIRECTIVE_ALIAS:
	case TOKEN_DIRECTIVE_BREAK:
	case TOKEN_DIRECTIVE_ALIGN:
	case TOKEN_KEYWORD_RETURN:
	case TOKEN_KEYWORD_REMOVE:
	case TOKEN_KEYWORD_STRUCT:
	case TOKEN_KEYWORD_TYPEOF:
	case TOKEN_KEYWORD_SIZEOF:
		return 6;
	case TOKEN_DIRECTIVE_INLINE:
	case TOKEN_DIRECTIVE_EXPORT:
		return 7;
	case TOKEN_DIRECTIVE_INCLUDE:
	case TOKEN_DIRECTIVE_LINKLIB:
	case TOKEN_DIRECTIVE_DEFINED:
	case TOKEN_KEYWORD_CONTINUE:
		return 8;
	case TOKEN_DIRECTIVE_EXTERNAL:
	case TOKEN_DIRECTIVE_OPERATOR:
	case TOKEN_DIRECTIVE_NOINLINE:
		return 9;
	case TOKEN_DIRECTIVE_INTRINSIC:
		return 10;
	case TOKEN_DIRECTIVE_CALLING_CONVENTION:
		return 11;
	default:
		return 1;
	}
}

Token ReadNewToken(PContext *context, Tokenizer *tokenizer, const char *fileBuffer)
{
	EatWhitespace(tokenizer);

	Token newToken;
	newToken.type = CalculateTokenType(context->global, tokenizer);
	newToken.size = CalculateTokenSize(context->global, tokenizer, newToken.type);
	newToken.loc.fileIdx = tokenizer->fileIdx;
	newToken.loc.character = (s32)(tokenizer->cursor - fileBuffer);
	return newToken;
}

FatSourceLocation ExpandSourceLocation(Context *context, SourceLocation loc)
{
	SourceFile sourceFile;
	{
		ScopedLockSpin filesLock(&context->filesLock);
		sourceFile = context->sourceFiles[loc.fileIdx];
	}

	FatSourceLocation result;
	result.beginingOfLine = (const char *)sourceFile.buffer;
	result.lineSize = 0;
	result.line = 1;
	result.column = 1;

	ASSERT(loc.character < sourceFile.size);

	const char *scan = (const char *)sourceFile.buffer;
	for (u32 charIdx = 0; charIdx < loc.character; ++charIdx) {
		if (*scan == '\n') {
			++result.line;
			result.column = 1;
			result.beginingOfLine = ++scan;
		}
		else {
			++result.column;
			++scan;
		}
	}

	Tokenizer tokenizer = {
		scan,
		(const char *)sourceFile.buffer + sourceFile.size,
		result.line,
		loc.fileIdx,
		nullptr
	};
	enum TokenType tokenType = CalculateTokenType(context, &tokenizer);
	result.size = CalculateTokenSize(context, &tokenizer, tokenType);

	for (const char *lineScan = result.beginingOfLine; *lineScan && *lineScan != '\n'; ++lineScan)
		++result.lineSize;

	return result;
}

void TokenizeFile(PContext *context, u32 fileIdx)
{
	Tokenizer tokenizer = {};
	SourceFile file;
	{
		ScopedLockSpin filesLock(&context->global->filesLock);
		file = context->global->sourceFiles[fileIdx];
	}
	tokenizer.fileIdx = fileIdx;
	tokenizer.cursor = (char *)file.buffer;
	tokenizer.beginningOfLine = (char *)file.buffer;
	tokenizer.end = (char *)(file.buffer + file.size);
	tokenizer.line = 1; // Line numbers start at 1 not 0.

	const char *fileBuffer = file.buffer;

	while (true) {
		EatWhitespace(&tokenizer);

		while (*tokenizer.cursor == '/') {
			if (*(tokenizer.cursor + 1) == '/') {
				ProcessCppComment(&tokenizer);
				EatWhitespace(&tokenizer);
			}
			else if (*(tokenizer.cursor + 1) == '*') {
				ProcessCComment(&tokenizer);
				EatWhitespace(&tokenizer);
			}
			else
				break;
		}

		if (!*tokenizer.cursor || tokenizer.cursor >= tokenizer.end)
			break;

		Token newToken = ReadNewToken(context, &tokenizer, fileBuffer);

		if (newToken.type == TOKEN_INVALID_DIRECTIVE)
			LogError(context->global, newToken.loc, "Invalid parser directive"_s);

		*BucketArrayAdd(&context->tokens) = newToken;

		tokenizer.cursor += newToken.size;
	}

	*BucketArrayAdd(&context->tokens) = { TOKEN_END_OF_FILE, 0,
		{ fileIdx, (u32)(tokenizer.cursor - file.buffer)-1 } };
}
