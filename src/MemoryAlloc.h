#define ALLOC(ALLOCATOR, TYPE) (TYPE *)ALLOCATOR::Alloc(sizeof(TYPE), alignof(TYPE))
struct Memory {
	u32 tlsIndex;
	u32 flsIndex;

	void *linearMem, *linearMemPtr;
	static const u64 linearMemSize = 64 * 1024 * 1024;
	Mutex linearMemMutex;
};

class LinearAllocator {
public:
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
};

class ThreadAllocator {
public:
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
};

class JobAllocator {
public:
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
};

class HeapAllocator {
public:
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
};
