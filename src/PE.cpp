void X64GenerateObjectFile(String outputFilename)
{
	ProfilerBegin("Generating output image");
	const int dataSectionIdx = 0;
	const int codeSectionIdx = 1;

	OutputBufferReset();

	u64 procCount = g_context->procedures.unsafe.count;
	ArrayInit(&g_procedureAddresses, procCount);
	g_procedureAddresses.size = procCount;

	DynamicArrayInit(&g_relocations, 1024);

	DynamicArray<IMAGE_SYMBOL, ThreadAllocator> symbolTable;
	DynamicArrayInit(&symbolTable, 1024);
	/*
		The symbol table shall have a fixed order, so we can know the symbol table
		index of stuff before actually making the table.

		*	First a symbol for each section
		*	All external procedures, in order
		*	All public procedures, in order
	*/

	DynamicArray<String, ThreadAllocator> stringTable;
	DynamicArrayInit(&stringTable, 1024);
	u32 stringTableOffset = 4; // Start after the string table size

	IMAGE_FILE_HEADER header;
	header.Machine = IMAGE_FILE_MACHINE_AMD64;
	header.NumberOfSections = 2;
	header.PointerToSymbolTable = 0;
	header.NumberOfSymbols = 0;
	header.SizeOfOptionalHeader = 0;
	header.Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE;

	IMAGE_SECTION_HEADER dataSectionHeader;
	memcpy(&dataSectionHeader.Name, "data\0\0\0\0", 8);
	dataSectionHeader.Misc.VirtualSize = 0;
	dataSectionHeader.VirtualAddress = 0;
	dataSectionHeader.SizeOfRawData = 0;
	dataSectionHeader.PointerToRawData = 0;
	dataSectionHeader.PointerToRelocations = 0;
	dataSectionHeader.PointerToLinenumbers = 0;
	dataSectionHeader.NumberOfRelocations = 0;
	dataSectionHeader.NumberOfLinenumbers = 0;
	dataSectionHeader.Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA;

	IMAGE_SECTION_HEADER codeSectionHeader;
	memcpy(&codeSectionHeader.Name, "code\0\0\0\0", 8);
	codeSectionHeader.Misc.VirtualSize = 0;
	codeSectionHeader.VirtualAddress = 0;
	codeSectionHeader.SizeOfRawData = 0;
	codeSectionHeader.PointerToRawData = 0;
	codeSectionHeader.PointerToRelocations = 0;
	codeSectionHeader.PointerToLinenumbers = 0;
	codeSectionHeader.NumberOfRelocations = 0;
	codeSectionHeader.NumberOfLinenumbers = 0;
	codeSectionHeader.Characteristics = IMAGE_SCN_CNT_CODE;

	// We write the headers later, skip for now
	OutputBufferSeek(sizeof(header) + sizeof(dataSectionHeader) +
			sizeof(codeSectionHeader));

	// data section
	OutputBufferAlign(16);
	u64 dataSectionOffset = g_context->outputBufferOffset;
	OutputBufferPut(g_context->staticDataSize, STATIC_DATA_VIRTUAL_ADDRESS);

	u32 startOfStaticDataSymbolIdx = (u32)symbolTable.size;
	{
		// Symbols for sections
		IMAGE_SYMBOL symbol;
		memcpy(&symbol.N.ShortName, "sectdata", 8);
		symbol.Value = 0;
		symbol.SectionNumber = dataSectionIdx + 1; // 1-based
		symbol.Type = 0;
		symbol.StorageClass = IMAGE_SYM_CLASS_STATIC;
		symbol.NumberOfAuxSymbols = 0;
		*DynamicArrayAdd(&symbolTable) = symbol;

		memcpy(&symbol.N.ShortName, "sectcode", 8);
		symbol.SectionNumber = codeSectionIdx + 1; // 1-based
		*DynamicArrayAdd(&symbolTable) = symbol;
	}

	// Remap pointers
	qsort(g_context->staticDataPointersToRelocate.data,
			g_context->staticDataPointersToRelocate.size,
			sizeof(void *), ComparePointers);

	u64 staticDataPtrCount = g_context->staticDataPointersToRelocate.size;
	u64 uniqueStaticDataPtrCount = 0;
	void *lastPtr = nullptr;
	for (int ptrIdx = 0; ptrIdx < staticDataPtrCount; ++ptrIdx) {
		void *ptr = g_context->staticDataPointersToRelocate.data[ptrIdx];
		// Skip duplicates (this is important cause the linker WILL reallocate these twice
		// additively)
		if (ptr == lastPtr)
			continue;
		lastPtr = ptr;

		++uniqueStaticDataPtrCount;

		u64 fileOffset = (u64)ptr - (u64)STATIC_DATA_VIRTUAL_ADDRESS + dataSectionOffset;

		u64 dataPtr = *(u64 *)ptr;
		if (dataPtr != 0) {
			ASSERT(dataPtr >= (u64)STATIC_DATA_VIRTUAL_ADDRESS &&
				   dataPtr < (u64)STATIC_DATA_VIRTUAL_ADDRESS_END);
			u64 remappedPtr = dataPtr - (u64)STATIC_DATA_VIRTUAL_ADDRESS;

			OutputBufferSeek(fileOffset);
			OutputBufferPut(8, &remappedPtr);
		}
	}

	// code section
	OutputBufferSeek(g_context->outputBufferSize);
	OutputBufferAlign(16);
	u64 codeSectionOffset = g_context->outputBufferOffset;
	xed_tables_init();
	X64EncodeAllInstructions();
	u64 codeSectionSize = g_context->outputBufferOffset - codeSectionOffset;

	// Add procedures to symbol table
	u32 externalProcCount = (u32)g_context->externalProcedures.unsafe.count;
	for (u32 procIdx = 1; procIdx < externalProcCount; ++procIdx) {
		Procedure proc = g_context->externalProcedures.unsafe[procIdx];
		// Add name to string table
		u32 nameStringTableOffset = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = proc.name;
		stringTableOffset += (u32)proc.name.size + 1; // + null terminator

		IMAGE_SYMBOL symbol;
		symbol.N.Name.Short = 0;
		symbol.N.Name.Long = nameStringTableOffset;
		symbol.Value = 0;
		symbol.SectionNumber = 0;
		symbol.Type = 0;
		symbol.StorageClass = IMAGE_SYM_CLASS_EXTERNAL;
		symbol.NumberOfAuxSymbols = 0;
		*DynamicArrayAdd(&symbolTable) = symbol;
	}
	for (u32 procIdx = 1; procIdx < procCount; ++procIdx) {
		Procedure proc = g_context->procedures.unsafe[procIdx];
		// Add name to string table
		u32 nameStringTableOffset = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = proc.name;
		stringTableOffset += (u32)proc.name.size + 1; // + null terminator

		u32 offsetWithinSection = (u32)g_procedureAddresses[procIdx] - (u32)codeSectionOffset;

		IMAGE_SYMBOL symbol;
		symbol.N.Name.Short = 0;
		symbol.N.Name.Long = nameStringTableOffset;
		symbol.Value = offsetWithinSection;
		symbol.SectionNumber = codeSectionIdx + 1; // 1-based
		symbol.Type = 0;
		symbol.StorageClass = proc.isExported ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC;
		symbol.NumberOfAuxSymbols = 0;
		*DynamicArrayAdd(&symbolTable) = symbol;
	}

	// Remap pointers
	for (int relIdx = 0; relIdx < g_relocations.size; ++relIdx) {
		Relocation relocation = g_relocations[relIdx];
		switch (relocation.type) {
		case RELOCATIONTYPE_PROCEDURE:
		{
			u64 procOffset = g_procedureAddresses[relocation.procedureIdx];
			s32 offsetInBuffer;
			OutputBufferSeek(relocation.destOffset);
			OutputBufferRead(4, &offsetInBuffer);
			// We need to fit these into u32's
			ASSERT(procOffset <= U32_MAX);
			ASSERT(relocation.destOffset <= U32_MAX);

			s64 finalOffset = (s64)procOffset - (4 + (s64)relocation.destOffset) + offsetInBuffer -
				relocation.offsetShift;
			ASSERT(finalOffset >= S32_MIN && finalOffset <= S32_MAX);
			OutputBufferSeek(relocation.destOffset);
			OutputBufferPut(4, &finalOffset);
		} break;
		case RELOCATIONTYPE_LABEL:
		{
#if DEBUG_BUILD
			s32 offsetInBuffer;
			OutputBufferSeek(relocation.destOffset);
			OutputBufferRead(4, &offsetInBuffer);
			ASSERT(offsetInBuffer == 0xCCCCCCCC);
#endif
			s64 finalOffset = (s64)relocation.label->address - (4 + (s64)relocation.destOffset) -
				relocation.offsetShift;
			ASSERT(finalOffset >= S32_MIN && finalOffset <= S32_MAX);
			OutputBufferSeek(relocation.destOffset);
			OutputBufferPut(4, &finalOffset);
		} break;
		}
	}

	// Static data relocation table
	OutputBufferSeek(g_context->outputBufferSize);
	OutputBufferAlign(16);
	u64 dataRelocationTableOffset = g_context->outputBufferOffset;
	IMAGE_RELOCATION imageRelocation;
	imageRelocation.Type = IMAGE_REL_AMD64_ADDR64;
	imageRelocation.SymbolTableIndex = startOfStaticDataSymbolIdx;
	lastPtr = nullptr;
	for (int ptrIdx = 0; ptrIdx < staticDataPtrCount; ++ptrIdx) {
		void *ptr = g_context->staticDataPointersToRelocate.data[ptrIdx];
		if (ptr == lastPtr)
			continue;
		lastPtr = ptr;

		u64 sectionOffset = (u64)ptr - (u64)STATIC_DATA_VIRTUAL_ADDRESS;
		ASSERT(sectionOffset <= U32_MAX);
		imageRelocation.VirtualAddress = (u32)sectionOffset;
		OutputBufferPut(sizeof(imageRelocation), &imageRelocation);
	}

	// Code relocation table
	OutputBufferSeek(g_context->outputBufferSize);
	OutputBufferAlign(16);
	u64 codeRelocationTableOffset = g_context->outputBufferOffset;
	u64 codeRelocationCount = 0;
	for (int relIdx = 0; relIdx < g_relocations.size; ++relIdx) {
		Relocation relocation = g_relocations[relIdx];

		static_assert(IMAGE_REL_AMD64_REL32 + 5 == IMAGE_REL_AMD64_REL32_5);
		ASSERT(relocation.offsetShift >= 0 && relocation.offsetShift <= 5);
		imageRelocation.Type = IMAGE_REL_AMD64_REL32 + (u16)relocation.offsetShift;

		switch (relocation.type) {
		case RELOCATIONTYPE_EXTERNAL_PROCEDURE:
		{
			u64 sectionOffset = relocation.destOffset - codeSectionOffset;
			ASSERT(sectionOffset <= U32_MAX);
			imageRelocation.VirtualAddress = (u32)sectionOffset;
			// +2 for section symbols, -1 because procedure index is 1-based.
			imageRelocation.SymbolTableIndex = 1 + relocation.procedureIdx;
			OutputBufferPut(sizeof(imageRelocation), &imageRelocation);
			++codeRelocationCount;
		} break;
		case RELOCATIONTYPE_STATIC_DATA:
		{
			u64 sectionOffset = relocation.destOffset - codeSectionOffset;
			ASSERT(sectionOffset <= U32_MAX);
			imageRelocation.VirtualAddress = (u32)sectionOffset;
			imageRelocation.SymbolTableIndex = dataSectionIdx; // Refer to symbol table order comment
			OutputBufferPut(sizeof(imageRelocation), &imageRelocation);
			++codeRelocationCount;
		} break;
		}
	}

	// Symbol table
	OutputBufferAlign(16);
	u64 symbolTableOffset = g_context->outputBufferOffset;
	for (int symbolIdx = 0; symbolIdx < symbolTable.size; ++symbolIdx)
		OutputBufferPut(sizeof(symbolTable.data[0]), &symbolTable[symbolIdx]);

	// String table
	u64 stringTableStart = g_context->outputBufferOffset;
	u32 stringTableTotalSize = stringTableOffset;
	OutputBufferPut(4, &stringTableTotalSize);
	u8 zero = 0;
	for (int stringIdx = 0; stringIdx < stringTable.size; ++stringIdx) {
		String str = stringTable[stringIdx];
		OutputBufferPut(str.size, str.data);
		OutputBufferPut(1, &zero);
	}
	// Assert we had the total size right
	ASSERT(g_context->outputBufferOffset == stringTableStart + stringTableTotalSize);

	// Fix headers
	ASSERT(symbolTableOffset <= U32_MAX);
	header.PointerToSymbolTable = (u32)symbolTableOffset;
	ASSERT(symbolTable.size <= U32_MAX);
	header.NumberOfSymbols = (u32)symbolTable.size;

	ASSERT(g_context->staticDataSize <= U32_MAX);
	dataSectionHeader.SizeOfRawData = (u32)g_context->staticDataSize;
	ASSERT(dataSectionOffset <= U32_MAX);
	dataSectionHeader.PointerToRawData = (u32)dataSectionOffset;
	ASSERT(dataRelocationTableOffset <= U32_MAX);
	dataSectionHeader.PointerToRelocations = (u32)dataRelocationTableOffset;
	ASSERT(uniqueStaticDataPtrCount <= U16_MAX);
	dataSectionHeader.NumberOfRelocations = (u16)uniqueStaticDataPtrCount;

	ASSERT(g_context->staticDataSize <= U32_MAX);
	codeSectionHeader.SizeOfRawData = (u32)codeSectionSize;
	ASSERT(codeSectionOffset <= U32_MAX);
	codeSectionHeader.PointerToRawData = (u32)codeSectionOffset;
	ASSERT(codeRelocationTableOffset <= U32_MAX);
	codeSectionHeader.PointerToRelocations = (u32)codeRelocationTableOffset;
	ASSERT(codeRelocationCount <= U16_MAX);
	codeSectionHeader.NumberOfRelocations = (u16)codeRelocationCount;

	// Write headers
	OutputBufferSeek(0);
	OutputBufferPut(sizeof(header), &header);
	OutputBufferPut(sizeof(dataSectionHeader), &dataSectionHeader);
	OutputBufferPut(sizeof(codeSectionHeader), &codeSectionHeader);

	OutputBufferWriteToFile(ChangeFilenameExtension(outputFilename, ".obj"_s));

	if (!g_context->config.silent)
		TimerSplit("Generating output image"_s);
}
