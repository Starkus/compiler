#define ENABLE_ALIGNMENT 1

void MemoryInit(Memory *memory)
{
#if DEBUG_BUILD
	memset(memory->linearMem, 0xCD, Memory::linearMemSize);
#endif
	memory->linearMemPtr = memory->linearMem;
	memory->linearMemMutex = SYSCreateMutex();
}

void MemoryInitThread(u64 size)
{
	ThreadDataCommon *threadData = (ThreadDataCommon *)SYSGetThreadData(g_memory->tlsIndex);
	threadData->threadMem = SYSAlloc(size);
	threadData->threadMemPtr = threadData->threadMem;
	threadData->threadMemSize = size;
#if DEBUG_BUILD
	memset(threadData->threadMem, 0x55, size);
#endif
}

void *LinearAllocator::Alloc(u64 size, int alignment)
{
#if USE_PROFILER_API
	performanceAPI.BeginEvent("Linear Alloc", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xBB, 0xBB, 0x10));
#endif

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
		u64 storedPtr = _InterlockedCompareExchange64((LONG64 *)&g_memory->linearMemPtr,
				(LONG64)newPtr, (LONG64)originalPtr);
		if (storedPtr == originalPtr) break;
	}

#if DEBUG_BUILD
	if (*((u64 *)result) != 0xCDCDCDCDCDCDCDCD) CRASH; // Watch for memory corruption
	ASSERT((u8 *)result + size < (u8 *)g_memory->linearMem + Memory::linearMemSize); // Out of memory!
#endif

#if USE_PROFILER_API
	performanceAPI.EndEvent();
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
void LinearAllocator::Wipe()
{
#if DEBUG_BUILD
	memset(g_memory->linearMem, 0xCD, Memory::linearMemSize);
#endif

	g_memory->linearMemPtr = g_memory->linearMem;
}

void *ThreadAllocator::Alloc(u64 size, int alignment)
{
#if USE_PROFILER_API
	//performanceAPI.BeginEvent("Thread Alloc", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xBB, 0xBB, 0x10));
#endif
	ThreadDataCommon *threadData = (ThreadDataCommon *)SYSGetThreadData(g_memory->tlsIndex);

#if DEBUG_BUILD
	if (*((u64 *)threadData->threadMemPtr) != 0x5555555555555555) CRASH; // Watch for memory corruption
	ASSERT((u8 *)threadData->threadMemPtr + size < (u8 *)threadData->threadMem +
			threadData->threadMemSize); // Out of memory!
#endif
	u64 result = (u64)threadData->threadMemPtr;

#if ENABLE_ALIGNMENT
	// Alignment
	ASSERT(IsPowerOf2(alignment));
	int alignmentMask = alignment - 1;

	u64 shift = 0;
	if (result & alignmentMask) shift = alignment;
	result += shift;
	result &= ~alignmentMask;
#endif

	threadData->threadMemPtr = (u8 *)result + size;

#if USE_PROFILER_API
	//performanceAPI.EndEvent();
#endif

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
void ThreadAllocator::Wipe()
{
	ASSERT(false);
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
void HeapAllocator::Wipe()
{
	ASSERT(false);
}
