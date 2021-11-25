void MemoryInit(Memory *memory)
{
#if DEBUG_BUILD
	memset(memory->frameMem, 0xCD, Memory::frameSize);
#endif

	memory->framePtr = memory->frameMem;
}

// FRAME
void *FrameAlloc(u64 size)
{
	if (*((u64 *)g_memory->framePtr) != 0xCDCDCDCDCDCDCDCD) // Watch for memory corruption
		CRASH;
	ASSERT((u8 *)g_memory->framePtr + size < (u8 *)g_memory->frameMem + Memory::frameSize); // Out of memory!
	void *result;

	// Alignment
	//if ((u64)g_memory->framePtr & 7)
		//g_memory->framePtr = (void *)(((u64)g_memory->framePtr & ~7) + 8);

	result = g_memory->framePtr;
	g_memory->framePtr = (u8 *)g_memory->framePtr + size;

	return result;
}
void *FrameRealloc(void *ptr, u64 newSize)
{
	//Print("WARNING: FRAME REALLOC\n");

	void *newBlock = FrameAlloc(newSize);
	memcpy(newBlock, ptr, newSize);
	return newBlock;
}
void FrameFree(void *ptr)
{
	(void) ptr;
}
void FrameWipe()
{
#if DEBUG_BUILD
	memset(g_memory->frameMem, 0xCD, Memory::frameSize);
#endif

	g_memory->framePtr = g_memory->frameMem;
}
