#define ENABLE_ALIGNMENT 1

void MemoryInit(Memory *memory)
{
	memory->linearMemPtr = memory->linearMem;
}

void MemoryInitThread(u64 size)
{
	t_threadMem = SYSAlloc(size);
	t_threadMemPtr = t_threadMem;
	t_threadMemSize = size;
}

void *LinearAllocator::Alloc(u64 size, int alignment)
{
	ProfilerScope scope("Linear Alloc", nullptr, PROFILER_COLOR(0xBB, 0xBB, 0x10));

	// Alignment
	ASSERT(IsPowerOf2(alignment));
	int alignmentMask = alignment - 1;

	u64 result;
	while (true)
	{
		u64 originalPtr = (u64)g_memory->linearMemPtr;
		result = originalPtr;

#if ENABLE_ALIGNMENT
		u64 shift = 0;
		if (result & alignmentMask) shift = alignment;
		result += shift;
		result &= ~alignmentMask;
#endif

		u64 newPtr = result + size;
		u64 storedPtr = AtomicCompareExchange64((s64 *)&g_memory->linearMemPtr,
				newPtr, originalPtr);
		if (storedPtr == originalPtr) break;
	}

#if DEBUG_BUILD
	ASSERT((u8 *)result + size < (u8 *)g_memory->linearMem + Memory::linearMemSize); // Out of memory!
	for (int i = 0; i < Min(size, 8); ++i)
		if (*((u8 *)(result + i)) != 0x00) CRASH; // Watch for memory corruption
#endif

	return (void *)result;
}
void *LinearAllocator::Realloc(void *ptr, u64 oldSize, u64 newSize, int alignment)
{
	void *newBlock = Alloc(newSize, alignment);
	if (ptr)
		memcpy(newBlock, ptr, oldSize);

	return newBlock;
}
void LinearAllocator::Free(void *ptr)
{
	(void) ptr;
}

void *ThreadAllocator::Alloc(u64 size, int alignment)
{
#if DEBUG_BUILD
	ASSERT((u8 *)t_threadMemPtr + size < (u8 *)t_threadMem + t_threadMemSize); // Out of memory!
	if (*((u64 *)t_threadMemPtr) != 0x0000000000000000) CRASH; // Watch for memory corruption
#endif
	u64 result = (u64)t_threadMemPtr;

#if ENABLE_ALIGNMENT
	// Alignment
	ASSERT(IsPowerOf2(alignment));
	int alignmentMask = alignment - 1;

	u64 shift = 0;
	if (result & alignmentMask) shift = alignment;
	result += shift;
	result &= ~alignmentMask;
#endif

	t_threadMemPtr = (u8 *)result + size;

	return (void *)result;
}
void *ThreadAllocator::Realloc(void *ptr, u64 oldSize, u64 newSize, int alignment)
{
	void *newBlock = Alloc(newSize, alignment);
	if (ptr)
		memcpy(newBlock, ptr, oldSize);

	return newBlock;
}
void ThreadAllocator::Free(void *ptr)
{
	(void) ptr;
}

void *HeapAllocator::Alloc(u64 size, int alignment)
{
	(void)alignment;
	return malloc(size);
}
void *HeapAllocator::Realloc(void *ptr, u64, u64 newSize, int)
{
	return realloc(ptr, newSize);
}
void HeapAllocator::Free(void *ptr)
{
	free(ptr);
}
