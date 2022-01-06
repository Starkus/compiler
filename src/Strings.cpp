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
	u64 resultU = 0;
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
		u64 moveLeft = resultU * 10;
		// Greater or equal because resultU can be 0
		ASSERT(moveLeft / 10 == resultU && moveLeft + digit >= resultU);
		resultU = moveLeft + digit;
	}
	s64 result;
	if (isNegative)
	{
		ASSERT(resultU < -S64_MIN);
		result = -(s64)resultU;
	}
	else
	{
		ASSERT(resultU < S64_MAX);
		result = (s64)resultU;
	}
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

		ASSERT(!(result & 0xF000000000000000));
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

	String leftSideString = {};
	String rightSideString = {};
	String exponentString = {};
	{
		const char *scan = string.data;
		for (int i = 0; i < string.size; ++i)
		{
			if (*scan == '.')
			{
				ASSERT(leftSideString.size == 0);
				leftSideString = { i, string.data };
				rightSideString = { string.size - i - 1, string.data + i + 1 };
			}
			else if (*scan == 'e' || *scan == 'E')
			{
				ASSERT(exponentString.size == 0);
				exponentString = { string.size - i - 1, string.data + i + 1 };
				if (leftSideString.size == 0)
				{
					leftSideString = { i, string.data };
				}
				else
				{
					rightSideString = { i - leftSideString - 1, string.data + leftSideString.size + 1 };
				}
			}
			++scan;
		}
	}

	// 32 byte margin on either side, 80 bytes total for all digits
	char buffer[144] = {};
	{
		int totalCount = 0;
		char *cursor = buffer + 32;
		const char *scan = leftSideString.data;
		for (int i = 0; i < leftSideString.size && totalCount < 80; ++i, ++totalCount)
		{
			ASSERT(IsNumeric(*scan));
			*cursor++ = *scan++ - '0';
		}
		scan = rightSideString.data;
		for (int i = 0; i < rightSideString.size && totalCount < 80; ++i, ++totalCount)
		{
			ASSERT(IsNumeric(*scan));
			*cursor++ = *scan++ - '0';
		}
	}
	u64 leftPart = 0;
	u64 rightPart = 0;
	int fractionDigits = 0;
	int totalDigits = 0;
	int sciNot = 0;
	int correctionExp = 0;

	if (exponentString.size) sciNot = (int)IntFromString(exponentString);
	ASSERT(-32 <= sciNot && sciNot <= 32);

	{
		const char *scan = buffer + 32;
		for (int i = 0; i < leftSideString.size + sciNot; ++i)
		{
			if (totalDigits >= 20) // Limit digits to 20. It's enough resolution for the 52 bit mantissa.
			{
				// We will multiply by 10 the final resulting f64 for every digit we ignored.
				correctionExp = (int)leftSideString.size + sciNot - i;
				break;
			}
			u64 digit = *scan++;
			u64 moveLeft = leftPart * 10;
			ASSERT(moveLeft + digit >= leftPart);
			leftPart = moveLeft + digit;
			if (leftPart > 0)
				++totalDigits;
		}
		scan = buffer + 32 + leftSideString.size + sciNot;
		for (int i = sciNot; i < rightSideString.size; ++i)
		{
			if (totalDigits >= 20) // Limit digits to 20. It's enough resolution for the 52 bit mantissa.
				break;
			s64 digit = *scan++;
			u64 moveLeft = rightPart * 10;
			if (moveLeft / 10 != rightPart || moveLeft + digit < rightPart) // Max resolution reached
				break;
			rightPart = moveLeft + digit;
			++fractionDigits;
			if (leftPart > 0 || rightPart > 0)
				++totalDigits;
		}
	}

	if (fractionDigits > 20)
	{
		// We build the float with all the digits we have, then divide by 10 at the end until the
		// number is correct.
		correctionExp = 20 - fractionDigits;
		fractionDigits = 20;
	}

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
	u64 div;
	if (fractionDigits == 20)
	{
		// 10^19 doesn't fit in 64 bits. Reduce instead

		// Doubling 'fraction' here would overflow. Instead we divide 'div' by 2 until it fits in 64
		// bits (which results in 10^19 / 8).
		div = 12500000000000000000;

		mantissa <<= 3; // Multiply by 2 three times
		ASSERT(!(mantissa & 0xFFF0000000000000)); // We shouldn't run out of bits yet.

		if (mantissa == 0) exponent -= 3;
	}
	else
		div = divTable[fractionDigits];
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

		// Prevent overflow
		if (fraction & 0x8000000000000000)
		{
			ASSERT(!(div & 1));
			div >>= 1;
		}
		else
			fraction <<= 1;
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

	if (correctionExp > 0)
		result *= divTable[correctionExp];
	else if (correctionExp < 0)
		result /= divTable[-correctionExp];

	return result;
}
