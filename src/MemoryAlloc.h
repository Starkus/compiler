#define ALLOC(FUNC, TYPE) (TYPE *)FUNC(sizeof(TYPE))
struct Memory
{
	void *frameMem, *framePtr;
	void *phaseMem, *phasePtr;
	static const u64 frameSize = 64 * 1024 * 1024;
	static const u64 phaseSize = 64 * 1024 * 1024;
};
