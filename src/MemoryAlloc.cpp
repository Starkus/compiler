void MemoryInit(Memory *memory)
{
	memory->framePtr = memory->frameMem;
}

// FRAME
void *FrameAlloc(u64 size)
{
	ASSERT((u8 *)g_memory->framePtr + size < (u8 *)g_memory->frameMem + Memory::frameSize); // Out of memory!
	void *result;

	result = g_memory->framePtr;
	g_memory->framePtr = (u8 *)g_memory->framePtr + size;

	return result;
}
void *FrameRealloc(void *ptr, u64 newSize)
{
	//Log("WARNING: FRAME REALLOC\n");

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
	g_memory->framePtr = g_memory->frameMem;
}
