#define ALLOC(FUNC, TYPE) (TYPE *)FUNC(sizeof(TYPE))
struct Memory
{
	void *frameMem, *framePtr;
	static const u64 frameSize = 64 * 1024 * 1024;
};
