#define ENABLE_ALIGNMENT 1

void MemoryInit(Memory *memory)
{
	memory->linearMemPtr = memory->linearMem;
	memory->linearMemMutex = SYSCreateMutex();
}

void MemoryInitThread(u64 size)
{
	t_threadMem = SYSAlloc(size);
	t_threadMemPtr = t_threadMem;
	t_threadMemSize = size;
}

void MemoryInitJob(u64 size)
{
	JobDataCommon *jobData = (JobDataCommon *)SYSGetFiberData(g_memory->flsIndex);
	jobData->jobMem = SYSAlloc(size);
	jobData->jobMemPtr = jobData->jobMem;
	jobData->jobMemSize = size;
}

void *LinearAllocator::Alloc(u64 size, int alignment)
{
	ProfileScope scope("Linear Alloc", nullptr, PROFILER_COLOR(0xBB, 0xBB, 0x10));

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
	for (int i = 0; i < Min(size, 8); ++i)
		if (*((u8 *)(result + i)) != 0x00) CRASH; // Watch for memory corruption
	ASSERT((u8 *)result + size < (u8 *)g_memory->linearMem + Memory::linearMemSize); // Out of memory!
#endif

	return (void *)result;
}
void *LinearAllocator::Realloc(void *ptr, u64 newSize, int alignment)
{
	SYSMutexLock(g_memory->linearMemMutex);

	void *newBlock = Alloc(newSize, alignment);
	if (ptr)
		memcpy(newBlock, ptr, newSize);

	SYSMutexUnlock(g_memory->linearMemMutex);

	return newBlock;
}
void LinearAllocator::Free(void *ptr)
{
	(void) ptr;
}

void *ThreadAllocator::Alloc(u64 size, int alignment)
{
#if DEBUG_BUILD
	if (*((u64 *)t_threadMemPtr) != 0x0000000000000000) CRASH; // Watch for memory corruption
	ASSERT((u8 *)t_threadMemPtr + size < (u8 *)t_threadMem +
			t_threadMemSize); // Out of memory!
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
void *ThreadAllocator::Realloc(void *ptr, u64 newSize, int alignment)
{
	void *newBlock = Alloc(newSize, alignment);
	if (ptr)
		memcpy(newBlock, ptr, newSize);

	return newBlock;
}
void ThreadAllocator::Free(void *ptr)
{
	(void) ptr;
}

void *JobAllocator::Alloc(u64 size, int alignment)
{
	JobDataCommon *jobData = (JobDataCommon *)SYSGetFiberData(g_memory->flsIndex);

#if DEBUG_BUILD
	if (*((u64 *)jobData->jobMemPtr) != 0x0000000000000000) CRASH; // Watch for memory corruption
	ASSERT((u8 *)jobData->jobMemPtr + size < (u8 *)jobData->jobMem +
			jobData->jobMemSize); // Out of memory!
#endif
	u64 result = (u64)jobData->jobMemPtr;

#if ENABLE_ALIGNMENT
	// Alignment
	ASSERT(IsPowerOf2(alignment));
	int alignmentMask = alignment - 1;

	u64 shift = 0;
	if (result & alignmentMask) shift = alignment;
	result += shift;
	result &= ~alignmentMask;
#endif

	jobData->jobMemPtr = (u8 *)result + size;

	return (void *)result;
}
void *JobAllocator::Realloc(void *ptr, u64 newSize, int alignment)
{
	void *newBlock = Alloc(newSize, alignment);
	if (ptr)
		memcpy(newBlock, ptr, newSize);

	return newBlock;
}
void JobAllocator::Free(void *ptr)
{
	(void) ptr;
}

void *HeapAllocator::Alloc(u64 size, int alignment)
{
	(void)alignment;
	return malloc(size);
}
void *HeapAllocator::Realloc(void *ptr, u64 newSize, int alignment)
{
	(void)alignment;
	return realloc(ptr, newSize);
}
void HeapAllocator::Free(void *ptr)
{
	free(ptr);
}
