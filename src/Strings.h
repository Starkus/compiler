struct String
{
	s64 size;
	const char *data;

	operator bool() const
	{
		return data != nullptr;
	}
};

inline String operator""_s(const char *str, u64 size);

inline String StringConcat(String a, String b);
const char *StringToCStr(String str, void *(*allocFunc)(u64));
String CStrToString(const char *cstr);
inline void ChangeExtension(char *buffer, const char *newExtension);
inline bool StringEquals(String a, String b);
inline bool IsAlpha(char c);
inline bool IsNumeric(char c);
inline bool IsNumericHex(char c);
inline bool IsWhitespace(char c);
s64 IntFromString(String string);
s64 IntFromStringHex(String string);
f64 F64FromString(String string);
