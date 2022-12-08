#define ALLOC(ALLOCATOR, TYPE) (TYPE *)ALLOCATOR::Alloc(sizeof(TYPE), alignof(TYPE))
#define ALLOC_N(ALLOCATOR, TYPE, N) (TYPE *)ALLOCATOR::Alloc(sizeof(TYPE) * N, alignof(TYPE))
struct Memory {
	u32 tlsIndex;
	u32 flsIndex;

	void *linearMem, *linearMemPtr;
	static const u64 linearMemSize = 64 * 1024 * 1024;
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

class HeapAllocator {
public:
	static void *Alloc(u64 size, int alignment);
	static void *Realloc(void *ptr, u64 newSize, int alignment);
	static void Free(void *ptr);
};
