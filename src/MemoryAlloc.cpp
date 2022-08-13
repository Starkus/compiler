#define ENABLE_ALIGNMENT 1

void MemoryInit(Memory *memory)
{
#if DEBUG_BUILD
	memset(memory->frameMem, 0xCD, Memory::frameSize);
	memset(memory->phaseMem, 0x55, Memory::phaseSize);
#endif

	memory->framePtr = memory->frameMem;
	memory->phasePtr = memory->phaseMem;
}

void *FrameAllocator::Alloc(u64 size)
{
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

	return result;
}
void *FrameAllocator::Realloc(void *ptr, u64 newSize)
{
	//Print("WARNING: FRAME REALLOC\n");

	void *newBlock = Alloc(newSize);
	memcpy(newBlock, ptr, newSize);
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

	return result;
}
void *PhaseAllocator::Realloc(void *ptr, u64 newSize)
{
	//Print("WARNING: FRAME REALLOC\n");

	void *newBlock = Alloc(newSize);
	memcpy(newBlock, ptr, newSize);
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
