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
