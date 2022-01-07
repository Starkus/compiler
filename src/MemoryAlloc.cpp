void MemoryInit(Memory *memory)
{
#if DEBUG_BUILD
	memset(memory->frameMem, 0xCD, Memory::frameSize);
	memset(memory->phaseMem, 0x55, Memory::phaseSize);
#endif

	memory->framePtr = memory->frameMem;
	memory->phasePtr = memory->phaseMem;
}

class FrameAllocator
{
	public:
	static void *Alloc(u64 size)
	{
		if (*((u64 *)g_memory->framePtr) != 0xCDCDCDCDCDCDCDCD) // Watch for memory corruption
			CRASH;
		ASSERT((u8 *)g_memory->framePtr + size < (u8 *)g_memory->frameMem + Memory::frameSize); // Out of memory!
		void *result;

		// Alignment
		int alignment = size > 16 ? 16 : NextPowerOf2((int)size);
		int alignmentMask = alignment - 1;
		if ((u64)g_memory->framePtr & alignmentMask)
			g_memory->framePtr = (void *)(((u64)g_memory->framePtr & ~alignmentMask) + alignment);

		result = g_memory->framePtr;
		g_memory->framePtr = (u8 *)g_memory->framePtr + size;

		return result;
	}
	static void *Realloc(void *ptr, u64 newSize)
	{
		//Print("WARNING: FRAME REALLOC\n");

		void *newBlock = FrameAllocator::Alloc(newSize);
		memcpy(newBlock, ptr, newSize);
		return newBlock;
	}
	static void Free(void *ptr)
	{
		(void) ptr;
	}
	static void Wipe()
	{
#if DEBUG_BUILD
		memset(g_memory->frameMem, 0xCD, Memory::frameSize);
#endif

		g_memory->framePtr = g_memory->frameMem;
	}
};

class PhaseAllocator
{
	public:
	static void *Alloc(u64 size)
	{
		if (*((u64 *)g_memory->phasePtr) != 0x5555555555555555) CRASH; // Watch for memory corruption
		ASSERT((u8 *)g_memory->phasePtr + size < (u8 *)g_memory->phaseMem + Memory::phaseSize); // Out of memory!
		void *result;

		// Alignment
		int alignment = size > 16 ? 16 : NextPowerOf2((int)size);
		int alignmentMask = alignment - 1;
		if ((u64)g_memory->phasePtr & alignmentMask)
			g_memory->phasePtr = (void *)(((u64)g_memory->phasePtr & ~alignmentMask) + alignment);

		result = g_memory->phasePtr;
		g_memory->phasePtr = (u8 *)g_memory->phasePtr + size;

		return result;
	}
	static void *Realloc(void *ptr, u64 newSize)
	{
		//Print("WARNING: FRAME REALLOC\n");

		void *newBlock = FrameAllocator::Alloc(newSize);
		memcpy(newBlock, ptr, newSize);
		return newBlock;
	}
	static void Free(void *ptr)
	{
		(void) ptr;
	}
	static void Wipe()
	{
#if DEBUG_BUILD
		memset(g_memory->phaseMem, 0x55, Memory::phaseSize);
#endif

		g_memory->phasePtr = g_memory->phaseMem;
	}
};

class HeapAllocator
{
	public:
	static void *Alloc(u64 size)
	{
		return malloc(size);
	}
	static void *Realloc(void *ptr, u64 newSize)
	{
		return realloc(ptr, newSize);
	}
	static void Free(void *ptr)
	{
		free(ptr);
	}
	static void Wipe()
	{
		ASSERT(false);
	}
};
