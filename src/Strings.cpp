inline String operator""_s(const char *str, u64 size)
{
	return { (s64)size, str };
}

inline String StringConcat(String a, String b)
{
	s64 size = a.size + b.size;
	char *buffer = (char *)FrameAlloc(size);
	String result = { size, buffer };
	memcpy(buffer, a.data, a.size);
	memcpy(buffer + a.size, b.data, b.size);
	return result;
}

const char *StringToCStr(String str, void *(*allocFunc)(u64))
{
	char *buffer = (char *)allocFunc(str.size + 1);
	strncpy(buffer, str.data, str.size);
	buffer[str.size] = 0;
	return buffer;
}

String CStrToString(const char *cstr)
{
	return { (s64)strlen(cstr), cstr };
}

String StupidStrToString(const wchar_t *wstr, void *(*allocFunc)(u64))
{
	s64 size = 0;
	for (const wchar_t *scan = wstr; *scan; ++scan)
		++size;
	char *buffer = (char *)allocFunc(size);
	char *dstScan = buffer;
	for (const wchar_t *scan = wstr; *scan; ++scan)
		*dstScan++ = (char)*scan;
	return { size, buffer };
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
	int i = 0;
	const char *scan = string.data;
	bool isNegative = false;
	if (*scan == '-')
	{
		isNegative = true;
		++i;
		++scan;
	}
	for (; i < string.size; ++i)
	{
		char c = *scan++;
		ASSERT(IsNumeric(c));
		s64 digit = c - '0';
		result *= 10;
		result += digit;
	}
	if (isNegative)
		result = -result;
	return result;
}

s64 IntFromStringHex(String string)
{
	s64 result = 0;
	int i = 0;
	const char *scan = string.data;
	bool isNegative = false;
	if (*scan == '-')
	{
		isNegative = true;
		++i;
		++scan;
	}
	for (; i < string.size; ++i)
	{
		char c = *scan++;

		s64 digit = -1;
		digit = c - '0' * (c >= '0' && c <= '9');
		digit += (0xA - 'a') * (c >= 'a' && c <= 'f');
		digit += (0xA - 'A') * (c >= 'A' && c <= 'F');
		ASSERT(digit >= 0);

		result = result << 4;
		result += digit;
	}
	if (isNegative)
		result = -result;
	return result;
}

f64 F64FromString(String string)
{
	bool isNegative = false;
	if (string.data[0] == '-')
	{
		isNegative = true;
		++string.data;
		--string.size;
	}

	s64 leftPart;
	s64 rightPart;
	s64 fractionDigits = 0;
	const char *scan = string.data;
	for (int i = 0; i < string.size; ++i)
		if (*scan++ == '.')
		{
			leftPart  = IntFromString({ i, string.data });
			++i;
			fractionDigits = string.size - i;
			rightPart = IntFromString({ fractionDigits, string.data + i });
			goto foundDot;
		}
	leftPart  = IntFromString(string);
	rightPart = 0;
foundDot:

	if (leftPart == 0 && rightPart == 0)
		return 0;

	int exponent = 0;
	if (leftPart) exponent = 63 - Nlz64(leftPart);

	u64 mantissa = leftPart;
	u64 fraction = rightPart;
	u64 divTable[] = {
		1,					10,					100,				1000,
		10000,				100000,				1000000,			10000000,
		100000000,			1000000000,			10000000000,		100000000000,
		1000000000000,		10000000000000,		100000000000000,	1000000000000000,
		10000000000000000,	100000000000000000,	1000000000000000000,10000000000000000000 };
	u64 div = divTable[fractionDigits];
	while (true)
	{
		if (fraction >= div)
		{
			mantissa |= 1;
			fraction -= div;
		}
		if (fraction <= 0) break;
		if (mantissa & 0x0010000000000000)
		{
			// Rounding
			if ((fraction << 1) >= div / 2)
				mantissa |= 1;
			break;
		}
		fraction *= 2;
		mantissa <<= 1;

		if (mantissa == 0) --exponent;
	}

	s8 mantissaShift = Nlz64(mantissa) - 11;
	if (mantissaShift > 0)
		mantissa <<= mantissaShift;
	else
		mantissa >>= -mantissaShift;

	u64 biasedExponent = 1023 + exponent;
	u64 shiftedExponent = biasedExponent << 52;

	u64 signBit = (u64)isNegative << 63;

	union
	{
		u64 floatBits;
		f64 result;
	};
	floatBits = signBit | (shiftedExponent & 0x7FF0000000000000) | (mantissa & 0xFFFFFFFFFFFFF);
	return result;
}
