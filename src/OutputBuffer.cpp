#define OUTPUT_BUFFER_PREFERRED_VIRTUAL_ADDRESS ((u8 *)0x0000088000000000)
#define OUTPUT_BUFFER_PAGE_SIZE (0x100000)

void OutputBufferInit()
{
	void *base = SYSReserveMemory(OUTPUT_BUFFER_PREFERRED_VIRTUAL_ADDRESS, 0x100000000);
	ASSERT(base);
	void *firstPage = SYSCommitMemory(base, OUTPUT_BUFFER_PAGE_SIZE);
	ASSERT(firstPage);
	g_context->outputBufferMem = (u8 *)base;
	g_context->outputBufferCapacity = OUTPUT_BUFFER_PAGE_SIZE;
	g_context->outputBufferSize = 0;

#if DEBUG_BUILD
	memset(firstPage, 0xCC, OUTPUT_BUFFER_PAGE_SIZE);
#endif
}

void OutputBufferAllocatePage()
{
	void *newPage = SYSCommitMemory(g_context->outputBufferMem + g_context->outputBufferCapacity,
			OUTPUT_BUFFER_PAGE_SIZE);
	ASSERT(newPage);
	g_context->outputBufferCapacity += OUTPUT_BUFFER_PAGE_SIZE;

#if DEBUG_BUILD
	memset(newPage, 0xCC, OUTPUT_BUFFER_PAGE_SIZE);
#endif
}

u64 OutputBufferPut(u64 count, const void *data)
{
	u64 requiredSize = g_context->outputBufferOffset + count;
	while (requiredSize >= g_context->outputBufferCapacity)
		OutputBufferAllocatePage();

	memcpy(g_context->outputBufferMem + g_context->outputBufferOffset, data, count);
	g_context->outputBufferOffset += count;

	if (g_context->outputBufferOffset > g_context->outputBufferSize)
		g_context->outputBufferSize = g_context->outputBufferOffset;

	return count;
}

u64 OutputBufferRead(u64 count, void *buffer)
{
	ASSERT(g_context->outputBufferOffset + count <= g_context->outputBufferSize);

	memcpy(buffer, g_context->outputBufferMem + g_context->outputBufferOffset, count);
	g_context->outputBufferOffset += count;

	return count;
}

void OutputBufferSeek(u64 offset)
{
	while (offset >= g_context->outputBufferCapacity)
		OutputBufferAllocatePage();
	g_context->outputBufferOffset = offset;

	if (g_context->outputBufferOffset > g_context->outputBufferSize)
		g_context->outputBufferSize = g_context->outputBufferOffset;
}

void OutputBufferAlign(int alignment)
{
	ASSERT(IsPowerOf2(alignment));
	int alignmentMask = alignment - 1;
	u64 offset = g_context->outputBufferOffset;
	if (offset & alignmentMask) offset += alignment;
	offset &= ~alignmentMask;

	OutputBufferSeek(offset);
}

void OutputBufferReset()
{
	// Free all but first page
	s64 bytesToFree = g_context->outputBufferCapacity - OUTPUT_BUFFER_PAGE_SIZE;
	if (bytesToFree > 0)
		SYSFreeMemory(g_context->outputBufferMem + OUTPUT_BUFFER_PAGE_SIZE, bytesToFree);
	g_context->outputBufferSize = 0;
	g_context->outputBufferOffset = 0;
	g_context->outputBufferCapacity = OUTPUT_BUFFER_PAGE_SIZE;
}

void OutputBufferWriteToFile(String filename)
{
	FileHandle outputFile = SYSOpenFileWrite(filename);
	SYSWriteFile(outputFile, g_context->outputBufferMem, g_context->outputBufferSize);
	SYSCloseFile(outputFile);
}

u64 OutputBufferPrint(const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;

	va_list args;
	va_start(args, format);
	u64 size = stbsp_vsprintf(buffer, format, args);
	va_end(args);

	OutputBufferPut(size, buffer);

#if DEBUG_BUILD
	memset(t_threadMemPtr, 0x00, size + 1);
#endif

	return size;
}
