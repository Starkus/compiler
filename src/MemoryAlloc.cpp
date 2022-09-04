#define ENABLE_ALIGNMENT 1

void MemoryInit(Memory *memory)
{
#if DEBUG_BUILD
	memset(memory->frameMem, 0xCD, Memory::frameSize);
	memset(memory->phaseMem, 0x55, Memory::phaseSize);
#endif

	memory->framePtr = memory->frameMem;
	memory->phasePtr = memory->phaseMem;

	memory->frameMutex = SYSCreateMutex();
	memory->phaseMutex = SYSCreateMutex();
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

void *FrameAllocator::Alloc(u64 size)
{
#if USE_PROFILER_API
	performanceAPI.BeginEvent("Linear Alloc", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xBB, 0xBB, 0x10));
#endif
	SYSMutexLock(g_memory->frameMutex);

#if DEBUG_BUILD
	if (*((u64 *)g_memory->framePtr) != 0xCDCDCDCDCDCDCDCD) CRASH; // Watch for memory corruption
#endif
	ASSERT((u8 *)g_memory->framePtr + size < (u8 *)g_memory->frameMem + Memory::frameSize); // Out of memory!
	void *result;

#if ENABLE_ALIGNMENT
	// Alignment
	int alignment = size > 16 ? 16 : NextPowerOf2((int)size);
	int alignmentMask = alignment - 1;
	if ((u64)g_memory->framePtr & alignmentMask)
		g_memory->framePtr = (void *)(((u64)g_memory->framePtr & ~alignmentMask) + alignment);
#endif

	result = g_memory->framePtr;
	g_memory->framePtr = (u8 *)g_memory->framePtr + size;

	SYSMutexUnlock(g_memory->frameMutex);

#if USE_PROFILER_API
	performanceAPI.EndEvent();
#endif

	return result;
}
void *FrameAllocator::Realloc(void *ptr, u64 newSize)
{
	SYSMutexLock(g_memory->frameMutex);

	void *newBlock = Alloc(newSize);
	memcpy(newBlock, ptr, newSize);

	SYSMutexUnlock(g_memory->frameMutex);

	return newBlock;
}
void FrameAllocator::Free(void *ptr)
{
	(void) ptr;
}
void FrameAllocator::Wipe()
{
#if DEBUG_BUILD
	memset(g_memory->frameMem, 0xCD, Memory::frameSize);
#endif

	g_memory->framePtr = g_memory->frameMem;
}

void *PhaseAllocator::Alloc(u64 size)
{
#if USE_PROFILER_API
	performanceAPI.BeginEvent("Phase Alloc", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xBB, 0xBB, 0x10));
#endif

	SYSMutexLock(g_memory->phaseMutex);

#if DEBUG_BUILD
	if (*((u64 *)g_memory->phasePtr) != 0x5555555555555555) CRASH; // Watch for memory corruption
#endif
	ASSERT((u8 *)g_memory->phasePtr + size < (u8 *)g_memory->phaseMem + Memory::phaseSize); // Out of memory!
	void *result;

#if ENABLE_ALIGNMENT
	// Alignment
	int alignment = size > 16 ? 16 : NextPowerOf2((int)size);
	int alignmentMask = alignment - 1;
	if ((u64)g_memory->phasePtr & alignmentMask)
		g_memory->phasePtr = (void *)(((u64)g_memory->phasePtr & ~alignmentMask) + alignment);
#endif

	result = g_memory->phasePtr;
	g_memory->phasePtr = (u8 *)g_memory->phasePtr + size;

	SYSMutexUnlock(g_memory->phaseMutex);

#if USE_PROFILER_API
	performanceAPI.EndEvent();
#endif

	return result;
}
void *PhaseAllocator::Realloc(void *ptr, u64 newSize)
{
	SYSMutexLock(g_memory->phaseMutex);

	void *newBlock = Alloc(newSize);
	memcpy(newBlock, ptr, newSize);

	SYSMutexUnlock(g_memory->phaseMutex);

	return newBlock;
}
void PhaseAllocator::Free(void *ptr)
{
	(void) ptr;
}
void PhaseAllocator::Wipe()
{
#if DEBUG_BUILD
	memset(g_memory->phaseMem, 0x55, Memory::phaseSize);
#endif

	g_memory->phasePtr = g_memory->phaseMem;
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
