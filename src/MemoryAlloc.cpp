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
	ThreadDataCommon *threadData = (ThreadDataCommon *)TlsGetValue(g_memory->tlsIndex);
	threadData->threadMem = SYSAlloc(size);
	threadData->threadMemPtr = threadData->threadMem;
	threadData->threadMemSize = size;
#if DEBUG_BUILD
	memset(threadData->threadMem, 0x55, size);
#endif
}

void *LinearAllocator::Alloc(u64 size)
{
#if USE_PROFILER_API
	performanceAPI.BeginEvent("Linear Alloc", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xBB, 0xBB, 0x10));
#endif

	// Alignment
	int alignment = size > 16 ? 16 : NextPowerOf2((int)size);
	int alignmentMask = alignment - 1;

	void *result;
	while (true)
	{
		void *originalPtr = g_memory->linearMemPtr;
		result = originalPtr;

#if ENABLE_ALIGNMENT
		if ((u64)result & alignmentMask)
			result = (void *)(((u64)result & ~alignmentMask) + alignment);
#endif

		void *newPtr = (u8 *)result + size;
		void *storedPtr = (void *)_InterlockedCompareExchange64((LONG64 *)&g_memory->linearMemPtr,
				(LONG64)newPtr, (LONG64)originalPtr);
		if (storedPtr == originalPtr)
			break;
	}

#if DEBUG_BUILD
	if (*((u64 *)result) != 0xCDCDCDCDCDCDCDCD) CRASH; // Watch for memory corruption
#endif
	ASSERT((u8 *)result + size < (u8 *)g_memory->linearMem + Memory::linearMemSize); // Out of memory!

#if USE_PROFILER_API
	performanceAPI.EndEvent();
#endif

	return result;
}
void *LinearAllocator::Realloc(void *ptr, u64 newSize)
{
	SYSMutexLock(g_memory->linearMemMutex);

	void *newBlock = Alloc(newSize);
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

void *ThreadAllocator::Alloc(u64 size)
{
#if USE_PROFILER_API
	performanceAPI.BeginEvent("Thread Alloc", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xBB, 0xBB, 0x10));
#endif
	ThreadDataCommon *threadData = (ThreadDataCommon *)TlsGetValue(g_memory->tlsIndex);

#if DEBUG_BUILD
	if (*((u64 *)threadData->threadMemPtr) != 0x5555555555555555) CRASH; // Watch for memory corruption
	ASSERT((u8 *)threadData->threadMemPtr + size < (u8 *)threadData->threadMem +
			threadData->threadMemSize); // Out of memory!
#endif
	void *result;

#if ENABLE_ALIGNMENT
	// Alignment
	int alignment = size > 16 ? 16 : NextPowerOf2((int)size);
	int alignmentMask = alignment - 1;
	if ((u64)threadData->threadMemPtr & alignmentMask)
		threadData->threadMemPtr = (void *)(((u64)threadData->threadMemPtr & ~alignmentMask) + alignment);
#endif

	result = threadData->threadMemPtr;
	threadData->threadMemPtr = (u8 *)threadData->threadMemPtr + size;

#if USE_PROFILER_API
	performanceAPI.EndEvent();
#endif

	return result;
}
void *ThreadAllocator::Realloc(void *ptr, u64 newSize)
{
	void *newBlock = Alloc(newSize);
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

void *HeapAllocator::Alloc(u64 size)
{
	return malloc(size);
}
void *HeapAllocator::Realloc(void *ptr, u64 newSize)
{
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
