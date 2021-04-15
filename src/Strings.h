struct String
{
	s64 size;
	const char *data;

	operator bool() const
	{
		return data != nullptr;
	}
};

const char *StringToCStr(String *str, void *(*allocFunc)(u64))
{
	char *buffer = (char *)allocFunc(str->size + 1);
	strncpy(buffer, str->data, str->size);
	buffer[str->size] = 0;
	return buffer;
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

inline String operator""_s(const char *str, u64 size)
{
	return { (s64)size, str };
}
