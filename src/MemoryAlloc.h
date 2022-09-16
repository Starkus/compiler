#define ALLOC(ALLOCATOR, TYPE) (TYPE *)ALLOCATOR::Alloc(sizeof(TYPE), alignof(TYPE))
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
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
	static void Wipe();
};

class ThreadAllocator
{
	public:
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
	static void Wipe();
};

class HeapAllocator
{
	public:
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
	static void Wipe();
};
