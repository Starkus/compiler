struct String
{
	u64 size;
	const char *data;

	operator bool() const
	{
		return data != nullptr;
	}
};

enum ParseNumberErrorCode
{
	PARSENUMBERRROR_OK,
	PARSENUMBERRROR_INVALID_CHARACTER,
	PARSENUMBERRROR_OVERFLOW,
	PARSENUMBERRROR_UNDERFLOW,
	PARSENUMBERRROR_INVALID_EXPONENT
};
struct ParseNumberResult
{
	ParseNumberErrorCode error;
	s64 number;
};
struct ParseFloatResult
{
	ParseNumberErrorCode error;
	f64 number;
};

inline String operator""_s(const char *str, u64 size);

inline String TStringConcat(String a, String b);
inline String SStringConcat(String a, String b);
const char *StringToCStr(String str, void *(*allocFunc)(u64, int));
String CStrToString(const char *cstr);
inline void ChangeExtension(char *buffer, const char *newExtension);
inline bool StringEquals(String a, String b);
inline bool IsAlpha(char c);
inline bool IsNumeric(char c);
inline bool IsNumericHex(char c);
inline bool IsWhitespace(char c);
ParseNumberResult IntFromString(String string);
ParseNumberResult IntFromStringHex(String string);
ParseFloatResult F64FromString(String string);
