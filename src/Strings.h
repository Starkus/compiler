struct String
{
	s64 size;
	const char *data;

	operator bool() const
	{
		return data != nullptr;
	}
};
