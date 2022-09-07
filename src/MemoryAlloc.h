#define ALLOC(FUNC, TYPE) (TYPE *)FUNC(sizeof(TYPE))
struct Memory
{
	u32 tlsIndex;

	void *linearMem, *linearMemPtr;
	static const u64 linearMemSize = 64 * 1024 * 1024;
	Mutex linearMemMutex;
};

class LinearAllocator
{
	public:
	static void *Alloc(u64 size);
	static void *Realloc(void *ptr, u64 newSize);
	static void Free(void *ptr);
	static void Wipe();
};

class ThreadAllocator
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
