inline String StringConcat(String a, String b)
{
	s64 size = a.size + b.size;
	char *buffer = (char *)FrameAlloc(size);
	String result = { size, buffer };
	memcpy(buffer, a.data, a.size);
	memcpy(buffer + a.size, b.data, b.size);
	return result;
}

const char *StringToCStr(String *str, void *(*allocFunc)(u64))
{
	char *buffer = (char *)allocFunc(str->size + 1);
	strncpy(buffer, str->data, str->size);
	buffer[str->size] = 0;
	return buffer;
}

String CStrToString(const char *cstr)
{
	return { (s64)strlen(cstr), cstr };
}

inline void ChangeExtension(char *buffer, const char *newExtension)
{
	char *lastDot = 0;
	for (char *scan = buffer; *scan; ++scan)
		if (*scan == '.')
			lastDot = scan;
	ASSERT(lastDot);
	strcpy(lastDot + 1, newExtension);
}

inline bool StringEquals(String a, String b)
{
	return a.size == b.size && strncmp(a.data, b.data, a.size) == 0;
}

inline bool IsAlpha(char c)
{
	return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

inline bool IsNumeric(char c)
{
	return c >= '0' && c <= '9';
}

inline bool IsNumericHex(char c)
{
	return (c >= '0' && c <= '9') ||
		(c >= 'a' && c <= 'f') ||
		(c >= 'A' && c <= 'F');
}

inline bool IsWhitespace(char c)
{
	return c == ' ' ||
		c == '\t' ||
		c == '\n' ||
		c == '\r';
}

s64 IntFromString(String string)
{
	s64 result = 0;
	const char *scan = string.data;
	for (int i = 0; i < string.size; ++i)
	{
		char c = *scan++;
		ASSERT(IsNumeric(c));
		s64 digit = c - '0';
		result *= 10;
		result += digit;
	}
	return result;
}

s64 IntFromStringHex(String string)
{
	s64 result = 0;
	for (int i = 0; i < string.size; ++i)
	{
		char c = string.data[i];

		s64 digit = -1;
		digit = c - '0' * (c >= '0' && c <= '9');
		digit += (0xA - 'a') * (c >= 'a' && c <= 'f');
		digit += (0xA - 'A') * (c >= 'A' && c <= 'F');
		ASSERT(digit >= 0);

		result = result << 4;
		result += digit;
	}
	return result;
}

inline String operator""_s(const char *str, u64 size)
{
	return { (s64)size, str };
}
