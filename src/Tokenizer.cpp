const String TokenTypeToString(s32 type)
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

	case TOKEN_END_OF_FILE:
		return "<EOF>"_s;
	}

	if (type >= TOKEN_KEYWORD_Begin && type <= TOKEN_KEYWORD_End)
		return "<Keyword>"_s;
	if (type >= TOKEN_OP_Begin && type <= TOKEN_OP_End)
		return "<Operator>"_s;

	char *str = (char *)FrameAlloc(5);
	strncpy(str, "<'~'>", 5);
	str[2] = (char)type;
	return { 5, str };
}

// @Speed: pass token by copy?
const String TokenToString(Token *token)
{
	if (token->type >= TOKEN_KEYWORD_Begin && token->type <= TOKEN_KEYWORD_End)
		return token->string;

	return TokenTypeToString(token->type);
}

inline bool IsTokenKeyword(Token *token)
{
	return token->type >= TOKEN_KEYWORD_Begin && token->type <= TOKEN_KEYWORD_End;
}

inline bool IsTokenOperator(Token *token)
{
	return token->type >= TOKEN_OP_Begin && token->type <= TOKEN_OP_End;
}

s32 GetOperatorPrecedence(s32 op)
{
	// Even means evaluated left to right with things of same precedence.
	// Odd means evaluated right to left with things of same precedence.
	switch (op)
	{
		case TOKEN_OP_ASSIGNMENT:
			return 1;
		case TOKEN_OP_EQUALS:
		case TOKEN_OP_GREATER_THAN:
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
		case TOKEN_OP_LESS_THAN:
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
		case TOKEN_OP_RANGE:
			return 2;
		case TOKEN_OP_PLUS:
		case TOKEN_OP_MINUS:
			return 4;
		case TOKEN_OP_MULTIPLY:
		case TOKEN_OP_DIVIDE:
		case TOKEN_OP_MODULO:
			return 6;
		case TOKEN_OP_SHIFT_LEFT:
		case TOKEN_OP_SHIFT_RIGHT:
		case TOKEN_OP_AND:
		case TOKEN_OP_OR:
		case TOKEN_OP_BITWISE_AND:
		case TOKEN_OP_BITWISE_OR:
			return 8;
		case TOKEN_OP_NOT:
		case TOKEN_OP_BITWISE_NOT:
			return 10;
		case TOKEN_OP_POINTER_TO:
		case TOKEN_OP_DEREFERENCE:
			return 12;
		case TOKEN_OP_ARRAY_ACCESS:
		case TOKEN_OP_MEMBER_ACCESS:
			return 14;
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
	result.loc.line = tokenizer->line;
	result.loc.character = tokenizer->cursor - tokenizer->beginningOfLine;

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
		result.type = TOKEN_LITERAL_CHARACTER;
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
		else if (TokenIsStr(&result, "for"))
			result.type = TOKEN_KEYWORD_FOR;
		else if (TokenIsStr(&result, "break"))
			result.type = TOKEN_KEYWORD_BREAK;
		else if (TokenIsStr(&result, "struct"))
			result.type = TOKEN_KEYWORD_STRUCT;
		else if (TokenIsStr(&result, "union"))
			result.type = TOKEN_KEYWORD_UNION;
		else if (TokenIsStr(&result, "enum"))
			result.type = TOKEN_KEYWORD_ENUM;
		else if (TokenIsStr(&result, "type"))
			result.type = TOKEN_KEYWORD_TYPE;
		else if (TokenIsStr(&result, "external"))
			result.type = TOKEN_KEYWORD_EXTERNAL;
		else if (TokenIsStr(&result, "defer"))
			result.type = TOKEN_KEYWORD_DEFER;
		else if (TokenIsStr(&result, "using"))
			result.type = TOKEN_KEYWORD_USING;
		else if (TokenIsStr(&result, "typeof"))
			result.type = TOKEN_KEYWORD_TYPEOF;
		else if (TokenIsStr(&result, "cast"))
			result.type = TOKEN_KEYWORD_CAST;
	}
	else if (IsNumeric(*tokenizer->cursor))
	{
		result.type = TOKEN_LITERAL_NUMBER;
		if (*tokenizer->cursor == '0')
		{
			++result.size;
			++tokenizer->cursor;

			switch (*tokenizer->cursor)
			{
			case 'x':
			case 'X':
			{
				++result.size;
				++tokenizer->cursor;
				while (IsNumericHex(*tokenizer->cursor))
				{
					++result.size;
					++tokenizer->cursor;
				}
			} goto numberDone;
			case 'b':
			{
				++result.size;
				++tokenizer->cursor;
				while (*tokenizer->cursor == '0' || *tokenizer->cursor == '1')
				{
					++result.size;
					++tokenizer->cursor;
				}
			} goto numberDone;
			}
		}
		// Normal base parsing
		{
			bool foundADot = false;
			while (true)
			{
				if (*tokenizer->cursor == '.')
				{
					if (foundADot)
						goto numberDone;
					else if (*(tokenizer->cursor + 1) == '.')
						// .. is an operator
						goto numberDone;

					foundADot = true;
				}
				else if (!IsNumeric(*tokenizer->cursor))
					goto numberDone;

				++result.size;
				++tokenizer->cursor;
			}
		}
numberDone:
		;
	}
	else if (*tokenizer->cursor >= TOKEN_ASCII_Begin && *tokenizer->cursor <= TOKEN_ASCII_End)
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
		case '<':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_LESS_THAN_OR_EQUAL;
				++tokenizer->cursor;
			}
			else if (next == '<')
			{
				if (*(tokenizer->cursor + 2) == '=')
				{
					result.type = TOKEN_OP_ASSIGNMENT_SHIFT_LEFT;
					tokenizer->cursor += 2;
				}
				else
				{
					result.type = TOKEN_OP_SHIFT_LEFT;
					++tokenizer->cursor;
				}
			}
			else
				result.type = TOKEN_OP_LESS_THAN;
		} break;
		case '>':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_GREATER_THAN_OR_EQUAL;
				++tokenizer->cursor;
			}
			else if (next == '>')
			{
				if (*(tokenizer->cursor + 2) == '=')
				{
					result.type = TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT;
					tokenizer->cursor += 2;
				}
				else
				{
					result.type = TOKEN_OP_SHIFT_RIGHT;
					++tokenizer->cursor;
				}
			}
			else
				result.type = TOKEN_OP_GREATER_THAN;
		} break;
		case ':':
		{
			if (next == ':')
			{
				result.type = TOKEN_OP_STATIC_DEF;
				++tokenizer->cursor;
			}
			else if (next == 's')
			{
				result.type = TOKEN_OP_VARIABLE_DECLARATION_STATIC;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_VARIABLE_DECLARATION;
		} break;
		case '+':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_PLUS;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_PLUS;
		} break;
		case '-':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_MINUS;
				++tokenizer->cursor;
			}
			else if (next == '>')
			{
				result.type = TOKEN_OP_ARROW;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_MINUS;
		} break;
		case '*':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_MULTIPLY;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_MULTIPLY;
		} break;
		case '/':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_DIVIDE;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_DIVIDE;
		} break;
		case '%':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_MODULO;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_MODULO;
		} break;
		case '|':
		{
			if (next == '|')
			{
				if (*(tokenizer->cursor + 2) == '=')
				{
					result.type = TOKEN_OP_ASSIGNMENT_OR;
					tokenizer->cursor += 2;
				}
				else
				{
					result.type = TOKEN_OP_OR;
					++tokenizer->cursor;
				}
			}
			else if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_BITWISE_OR;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_BITWISE_OR;
		} break;
		case '&':
		{
			if (next == '&')
			{
				if (*(tokenizer->cursor + 2) == '=')
				{
					result.type = TOKEN_OP_ASSIGNMENT_AND;
					tokenizer->cursor += 2;
				}
				else
				{
					result.type = TOKEN_OP_AND;
					++tokenizer->cursor;
				}
			}
			else if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_BITWISE_AND;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_BITWISE_AND;
		} break;
		case '^':
		{
			if (next == '=')
			{
				result.type = TOKEN_OP_ASSIGNMENT_BITWISE_XOR;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_POINTER_TO;
		} break;
		case '@':
		{
			result.type = TOKEN_OP_DEREFERENCE;
		} break;
		case '[':
		{
			result.type = TOKEN_OP_ARRAY_ACCESS;
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
			if (next == '.')
			{
				result.type = TOKEN_OP_RANGE;
				++tokenizer->cursor;
			}
			else
				result.type = TOKEN_OP_MEMBER_ACCESS;
		} break;
		default:
		{
			result.type = (enum TokenType)*tokenizer->cursor;
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

void TokenizeFile(Context *context)
{
	Tokenizer tokenizer = {};
	tokenizer.cursor = (char *)context->fileBuffer;
	tokenizer.beginningOfLine = (char *)context->fileBuffer;
	tokenizer.end = (char *)(context->fileBuffer + context->fileSize);
	tokenizer.line = 1; // Line numbers start at 1 not 0.
	while (true)
	{
		Token newToken = ReadTokenAndAdvance(&tokenizer);

		newToken.loc.file = context->filename;
		newToken.loc.fileBuffer = context->fileBuffer;
		newToken.loc.size = newToken.size;

		if (newToken.type == TOKEN_END_OF_FILE)
			break;

		*BucketArrayAdd(&context->tokens) = newToken;
	}
}
