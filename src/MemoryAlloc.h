#define ALLOC(FUNC, TYPE) (TYPE *)FUNC(sizeof(TYPE))
struct Memory
{
	void *frameMem, *framePtr;
	void *phaseMem, *phasePtr;
	static const u64 frameSize = 64 * 1024 * 1024;
	static const u64 phaseSize = 64 * 1024 * 1024;
	Mutex frameMutex, phaseMutex;
};

class FrameAllocator
{
	public:
	static void *Alloc(u64 size);
	static void *Realloc(void *ptr, u64 newSize);
	static void Free(void *ptr);
	static void Wipe();
};

class PhaseAllocator
{
	public:
	static void *Alloc(u64 size);
	static void *Realloc(void *ptr, u64 newSize);
	static void Free(void *ptr);
	static void Wipe();
};

class HeapAllocator
{
	public:
	static void *Alloc(u64 size);
	static void *Realloc(void *ptr, u64 newSize);
	static void Free(void *ptr);
	static void Wipe();
};
