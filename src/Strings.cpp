inline String operator""_s(const char *str, u64 size)
{
	return { (s64)size, str };
}

inline String StringConcat(String a, String b)
{
	s64 size = a.size + b.size;
	char *buffer = (char *)FrameAllocator::Alloc(size);
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

	int leftCount = 0;
	int rightCount = 0;
	int sciNot = 0;
	// 32 byte margin on either side, 80 bytes total for all digits.
	// The reason for the margin is so we can just start reading up to 32 bytes more to the left if
	// we have a negative scientific notation exponent.
	char buffer[144] = {};
	{
		bool foundDot = false;
		int totalCount = 0;
		char *cursor = buffer + 32;
		const char *scan = string.data;
		for (int i = 0; i < string.size && totalCount < 80; ++i, ++scan)
		{
			if (*scan == '.')
				foundDot = true;
			else if (*scan == 'e' || *scan == 'E')
			{
				sciNot = (int)IntFromString({ string.size - i - 1, scan + 1 });
				ASSERT(-32 <= sciNot && sciNot <= 32);
				break;
			}
			else
			{
				ASSERT(IsNumeric(*scan));
				*cursor++ = *scan - '0';
				foundDot ? ++rightCount : ++leftCount;
				++totalCount;
			}
		}
	}
	u64 mantissa = 0;
	u64 fraction = 0;
	int fractionDigits = 0;
	int totalDigits = 0;
	int correctionExp = 0;

	{
		const char *scan = buffer + 32;
		for (int i = 0; i < leftCount + sciNot; ++i)
		{
			if (totalDigits >= 20) // Limit digits to 20. It's enough resolution for the 52 bit mantissa.
			{
				// We will multiply by 10 the final resulting f64 for every digit we ignored.
				correctionExp = (int)leftCount + sciNot - i;
				break;
			}
			u64 digit = *scan++;
			u64 moveLeft = mantissa * 10;
			ASSERT(moveLeft + digit >= mantissa);
			mantissa = moveLeft + digit;
			if (mantissa > 0)
				++totalDigits;
		}
		scan = buffer + 32 + leftCount + sciNot;
		for (int i = sciNot; i < rightCount; ++i)
		{
			if (totalDigits >= 20) // Limit digits to 20. It's enough resolution for the 52 bit mantissa.
				break;
			s64 digit = *scan++;
			u64 moveLeft = fraction * 10;
			if (moveLeft / 10 != fraction || moveLeft + digit < fraction) // Max resolution reached
				break;
			fraction = moveLeft + digit;
			++fractionDigits;
			if (mantissa > 0 || fraction > 0)
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

	// Special case: 0
	if (mantissa == 0 && fraction == 0)
		return isNegative ? -0.0 : 0.0;

	int exponent = 0;
	// We will shift mantissa until the first leading 1 is on bit 53. The effect is that we
	// effectively divide mantissa by 2 by the number of trailing zeroes. For example, if our wholes
	// side is 8, we will divide it until it's 1.
	// Because of this, we set the exponent equal to the number of trailing zeroes. So in our
	// example 8 is 0b1000, which results in 2^3 * 1 because it has 3 trailing zeroes.
	if (mantissa) exponent = 63 - Nlz64(mantissa);

	// Table of multiples of 10 so we can divide/multiply by 10 many times in one go.
	u64 divTable[] = {
		1,					10,					100,				1000,
		10000,				100000,				1000000,			10000000,
		100000000,			1000000000,			10000000000,		100000000000,
		1000000000000,		10000000000000,		100000000000000,	1000000000000000,
		10000000000000000,	100000000000000000,	1000000000000000000,10000000000000000000 };
	u64 div;
	if (fractionDigits == 20)
	{
		// 10^19 doesn't fit in 64 bits. We will pretend to work with 10^19.
		// Checking if fraction is greater than our 10^19 is unnecessary since a 64 bit number can
		// never be greater than 10^19. Nor (10^19)/2, nor (10^19)/4.

		// Doubling 'fraction' here would overflow. Instead we divide 'div' by 2 until it fits in 64
		// bits (which results in 10^19 / 8).
		div = 12500000000000000000;

		// @Check: isn't mantissa always 0 here? 20 fraction digits means all our digits went into
		// the fraction part.
		mantissa <<= 3; // Multiply by 2 three times
		ASSERT(!(mantissa & 0xFFF0000000000000)); // We shouldn't run out of bits yet.

		if (mantissa == 0) exponent -= 3;
	}
	else
		div = divTable[fractionDigits];
	while (true)
	{
		// The idea for the fraction part is:
		//   * We multiply both the fraction part and the current mantissa by 2.
		//   * If the fraction part begins with a 1 (in base 10) we remove it and set lowest bit on
		// mantissa to 1.
		//   * Repeat until fraction part is 0 or we run out of mantissa bits.
		if (fraction >= div)
		{
			mantissa |= 1;
			fraction -= div;
		}
		if (fraction <= 0) break;

		// Prevent overflow
		if (fraction & 0x8000000000000000)
		{
			// Doubling fraction would overflow the U64. We can divide 'div' by 2 instead, but it
			// should be an even number.
			ASSERT(!(div & 1));
			div >>= 1;
		}
		else
			fraction <<= 1;

		if (mantissa & 0x0010000000000000)
		{
			// We ran out of precision bits on mantissa. Just round up and get out.
			// Rounding
			if (fraction >= div)
				mantissa += 1;
			break;
		}

		mantissa <<= 1;
		if (mantissa == 0) --exponent;
	}

	// Shift mantissa until most significant 1 is in bit 53.
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
