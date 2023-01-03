#define OUTPUT_BUFFER_PREFERRED_VIRTUAL_ADDRESS ((u8 *)0x0000088000000000)
#define OUTPUT_BUFFER_PAGE_SIZE (0x100000)

void OutputBufferInit(Context *context)
{
	void *base = VirtualAlloc(OUTPUT_BUFFER_PREFERRED_VIRTUAL_ADDRESS, 0x1000000000, MEM_RESERVE,
			PAGE_READWRITE);
	ASSERT(base);
	void *firstPage = VirtualAlloc(base, OUTPUT_BUFFER_PAGE_SIZE, MEM_COMMIT, PAGE_READWRITE);
	ASSERT(firstPage);
	context->outputBufferMem = (u8 *)base;
	context->outputBufferCapacity = OUTPUT_BUFFER_PAGE_SIZE;
	context->outputBufferSize = 0;

#if DEBUG_BUILD
	memset(firstPage, 0xCC, OUTPUT_BUFFER_PAGE_SIZE);
#endif
}

void OutputBufferAllocatePage(Context *context)
{
	void *newPage = VirtualAlloc(context->outputBufferMem + context->outputBufferCapacity,
			OUTPUT_BUFFER_PAGE_SIZE, MEM_COMMIT, PAGE_READWRITE);
	ASSERT(newPage);
	context->outputBufferCapacity += OUTPUT_BUFFER_PAGE_SIZE;

#if DEBUG_BUILD
	memset(newPage, 0xCC, OUTPUT_BUFFER_PAGE_SIZE);
#endif
}

u64 OutputBufferPut(Context *context, u64 count, const void *data)
{
	u64 requiredSize = context->outputBufferOffset + count;
	while (requiredSize >= context->outputBufferCapacity)
		OutputBufferAllocatePage(context);

	memcpy(context->outputBufferMem + context->outputBufferOffset, data, count);
	context->outputBufferOffset += count;

	if (context->outputBufferOffset > context->outputBufferSize)
		context->outputBufferSize = context->outputBufferOffset;

	return count;
}

u64 OutputBufferRead(Context *context, u64 count, void *buffer)
{
	ASSERT(context->outputBufferOffset + count <= context->outputBufferSize);

	memcpy(buffer, context->outputBufferMem + context->outputBufferOffset, count);
	context->outputBufferOffset += count;

	return count;
}

void OutputBufferSeek(Context *context, u64 offset)
{
	while (offset >= context->outputBufferCapacity)
		OutputBufferAllocatePage(context);
	context->outputBufferOffset = offset;

	if (context->outputBufferOffset > context->outputBufferSize)
		context->outputBufferSize = context->outputBufferOffset;
}

void OutputBufferAlign(Context *context, int alignment)
{
	ASSERT(IsPowerOf2(alignment));
	int alignmentMask = alignment - 1;
	u64 offset = context->outputBufferOffset;
	if (offset & alignmentMask) offset += alignment;
	offset &= ~alignmentMask;

	OutputBufferSeek(context, offset);
}

void OutputBufferReset(Context *context)
{
	// Free all but first page
	s64 bytesToFree = context->outputBufferCapacity - OUTPUT_BUFFER_PAGE_SIZE;
	if (bytesToFree > 0)
		VirtualFree(context->outputBufferMem + OUTPUT_BUFFER_PAGE_SIZE, bytesToFree, MEM_DECOMMIT);
	context->outputBufferSize = 0;
	context->outputBufferOffset = 0;
	context->outputBufferCapacity = OUTPUT_BUFFER_PAGE_SIZE;
}

void OutputBufferWriteToFile(Context *context, String filename)
{
	FileHandle outputFile = SYSOpenFileWrite(filename);
	SYSWriteFile(outputFile, context->outputBufferMem, context->outputBufferSize);
	SYSCloseFile(outputFile);
}

u64 OutputBufferPrint(Context *context, const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;

	va_list args;
	va_start(args, format);
	u64 size = stbsp_vsprintf(buffer, format, args);
	va_end(args);

	OutputBufferPut(context, size, buffer);

#if DEBUG_BUILD
	memset(t_threadMemPtr, 0x00, size + 1);
#endif

	return size;
}
