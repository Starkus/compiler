#include <elf.h>

void X64GenerateObjectFile(String outputFilename)
{
	ProfilerBegin("Generating output image");

	enum {
		SECTIONIDX_NULL = 0,
		SECTIONIDX_DATA = 1,
		SECTIONIDX_CODE = 2,
		SECTIONIDX_SYMBOLTABLE = 3,
		SECTIONIDX_STRINGTABLE = 4,
		SECTIONIDX_RELDATA = 5,
		SECTIONIDX_RELCODE = 6,
		SECTIONIDX_Count
	};

	OutputBufferReset();

	u64 procCount = g_context->procedures.unsafe.count;
	ArrayInit(&g_procedureAddresses, procCount);
	g_procedureAddresses.size = procCount;

	DynamicArrayInit(&g_relocations, 1024);

	DynamicArray<Elf64_Sym, ThreadAllocator> symbolTable;
	DynamicArrayInit(&symbolTable, 1024);
	*DynamicArrayAdd(&symbolTable) = {};

	Array<u32, ThreadAllocator> extProcSymbolIndices;
	u32 externalProcCount = (u32)g_context->externalProcedures.unsafe.count;
	ArrayInit(&extProcSymbolIndices, externalProcCount);
	extProcSymbolIndices.size = externalProcCount;

	DynamicArray<String, ThreadAllocator> stringTable;
	DynamicArrayInit(&stringTable, 1024);
	u32 stringTableOffset = 0;

	Elf64_Ehdr header = {};
	header.e_ident[EI_MAG0] = ELFMAG0;
	header.e_ident[EI_MAG1] = ELFMAG1;
	header.e_ident[EI_MAG2] = ELFMAG2;
	header.e_ident[EI_MAG3] = ELFMAG3;
	header.e_ident[EI_CLASS] = ELFCLASS64;
	header.e_ident[EI_DATA] = ELFDATA2LSB;
	header.e_ident[EI_VERSION] = EV_CURRENT;
	header.e_ident[EI_OSABI] = ELFOSABI_LINUX;
	header.e_ident[EI_ABIVERSION] = 0;
	header.e_ident[EI_PAD] = 0;
	header.e_type = ET_REL;
	header.e_machine = EM_X86_64;
	header.e_version = EV_CURRENT;
	header.e_entry = 0;
	header.e_phoff = 0;
	header.e_shoff = sizeof(Elf64_Ehdr);
	header.e_ehsize = sizeof(Elf64_Ehdr);
	header.e_phentsize = 0;
	header.e_phnum = 0;
	header.e_shentsize = sizeof(Elf64_Shdr);
	header.e_shnum = SECTIONIDX_Count;
	header.e_shstrndx = SECTIONIDX_STRINGTABLE;

	Elf64_Shdr nullSectionHeader = {};
	nullSectionHeader.sh_type = SHT_NULL;

	Elf64_Shdr dataSectionHeader = {};
	String sectionName = "data"_s;
	dataSectionHeader.sh_name = stringTableOffset;
	*DynamicArrayAdd(&stringTable) = sectionName;
	stringTableOffset += (u32)sectionName.size + 1; // + null terminator
	dataSectionHeader.sh_type = SHT_PROGBITS;
	dataSectionHeader.sh_flags = SHF_WRITE | SHF_ALLOC;
	dataSectionHeader.sh_addr = 0;
	dataSectionHeader.sh_size = 0;
	dataSectionHeader.sh_offset = 0;
	dataSectionHeader.sh_addralign = 16;
	dataSectionHeader.sh_entsize = 0;

	Elf64_Shdr codeSectionHeader = {};
	sectionName = "code"_s;
	codeSectionHeader.sh_name = stringTableOffset;
	*DynamicArrayAdd(&stringTable) = sectionName;
	stringTableOffset += (u32)sectionName.size + 1; // + null terminator
	codeSectionHeader.sh_type = SHT_PROGBITS;
	codeSectionHeader.sh_flags = SHF_ALLOC | SHF_EXECINSTR;
	codeSectionHeader.sh_addr = 0;
	codeSectionHeader.sh_size = 0;
	codeSectionHeader.sh_offset = 0;
	codeSectionHeader.sh_addralign = 16;
	codeSectionHeader.sh_entsize = 0;

	Elf64_Shdr symbolSectionHeader = {};
	sectionName = "symtab"_s;
	symbolSectionHeader.sh_name = stringTableOffset;
	*DynamicArrayAdd(&stringTable) = sectionName;
	stringTableOffset += (u32)sectionName.size + 1; // + null terminator
	symbolSectionHeader.sh_type = SHT_SYMTAB;
	symbolSectionHeader.sh_flags = SHF_ALLOC;
	symbolSectionHeader.sh_addr = 0;
	symbolSectionHeader.sh_size = 0;
	symbolSectionHeader.sh_link = SECTIONIDX_STRINGTABLE;
	symbolSectionHeader.sh_offset = 0;
	symbolSectionHeader.sh_addralign = 16;
	symbolSectionHeader.sh_entsize = sizeof(Elf64_Sym);

	Elf64_Shdr stringSectionHeader = {};
	sectionName = "strtab"_s;
	stringSectionHeader.sh_name = stringTableOffset;
	*DynamicArrayAdd(&stringTable) = sectionName;
	stringTableOffset += (u32)sectionName.size + 1; // + null terminator
	stringSectionHeader.sh_type = SHT_STRTAB;
	stringSectionHeader.sh_flags = 0;
	stringSectionHeader.sh_addr = 0;
	stringSectionHeader.sh_size = 0;
	stringSectionHeader.sh_offset = 0;
	stringSectionHeader.sh_addralign = 16;
	stringSectionHeader.sh_entsize = 0;

	Elf64_Shdr relDataSectionHeader = {};
	sectionName = "reldata"_s;
	relDataSectionHeader.sh_name = stringTableOffset;
	*DynamicArrayAdd(&stringTable) = sectionName;
	stringTableOffset += (u32)sectionName.size + 1; // + null terminator
	relDataSectionHeader.sh_type = SHT_RELA;
	relDataSectionHeader.sh_flags = SHF_ALLOC;
	relDataSectionHeader.sh_addr = 0;
	relDataSectionHeader.sh_size = 0;
	relDataSectionHeader.sh_link = SECTIONIDX_SYMBOLTABLE;
	relDataSectionHeader.sh_info = SECTIONIDX_DATA;
	relDataSectionHeader.sh_offset = 0;
	relDataSectionHeader.sh_addralign = 8;
	relDataSectionHeader.sh_entsize = sizeof(Elf64_Rela);

	Elf64_Shdr relCodeSectionHeader = {};
	sectionName = "relcode"_s;
	relCodeSectionHeader.sh_name = stringTableOffset;
	*DynamicArrayAdd(&stringTable) = sectionName;
	stringTableOffset += (u32)sectionName.size + 1; // + null terminator
	relCodeSectionHeader.sh_type = SHT_RELA;
	relCodeSectionHeader.sh_flags = SHF_ALLOC;
	relCodeSectionHeader.sh_addr = 0;
	relCodeSectionHeader.sh_size = 0;
	relCodeSectionHeader.sh_link = SECTIONIDX_SYMBOLTABLE;
	relCodeSectionHeader.sh_info = SECTIONIDX_CODE;
	relCodeSectionHeader.sh_offset = 0;
	relCodeSectionHeader.sh_addralign = 8;
	relCodeSectionHeader.sh_entsize = sizeof(Elf64_Rela);

	// We write the headers later, skip for now
	OutputBufferSeek(sizeof(header) + sizeof(Elf64_Shdr) * SECTIONIDX_Count);

	// data section
	OutputBufferAlign(16);
	u64 dataSectionOffset = g_context->outputBufferOffset;
	OutputBufferPut(g_context->staticDataSize, STATIC_DATA_VIRTUAL_ADDRESS);

	u32 startOfStaticDataSymbolIdx = (u32)symbolTable.size;
	{
		// Symbols for sections
		Elf64_Sym symbol;
		String symbolName = "sectdata"_s;
		symbol.st_name = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = symbolName;
		stringTableOffset += (u32)symbolName.size + 1; // + null terminator
		symbol.st_value = 0;
		symbol.st_size = 0;
		symbol.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
		symbol.st_other = STV_DEFAULT;
		symbol.st_shndx = SECTIONIDX_DATA;
		*DynamicArrayAdd(&symbolTable) = symbol;

		symbolName = "sectcode"_s;
		symbol.st_name = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = symbolName;
		stringTableOffset += (u32)symbolName.size + 1; // + null terminator
		symbol.st_shndx = SECTIONIDX_CODE; // 1-based
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
	// Not exported local procedures
	for (u32 procIdx = 1; procIdx < procCount; ++procIdx) {
		Procedure proc = g_context->procedures.unsafe[procIdx];
		if (proc.isExported) continue;

		// Add name to string table
		u32 nameStringTableOffset = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = proc.name;
		stringTableOffset += (u32)proc.name.size + 1; // + null terminator

		u32 offsetWithinSection = (u32)g_procedureAddresses[procIdx] - (u32)codeSectionOffset;

		Elf64_Sym symbol;
		symbol.st_name = nameStringTableOffset;
		symbol.st_value = offsetWithinSection;
		symbol.st_size = 0;
		symbol.st_info = ELF64_ST_INFO(STB_LOCAL, STT_FUNC);
		symbol.st_other = STV_DEFAULT;
		symbol.st_shndx = SECTIONIDX_CODE;
		*DynamicArrayAdd(&symbolTable) = symbol;
	}

	u64 localSymbolCount = symbolTable.size;

	// Exported local procedures
	for (u32 procIdx = 1; procIdx < procCount; ++procIdx) {
		Procedure proc = g_context->procedures.unsafe[procIdx];
		if (!proc.isExported) continue;

		// Add name to string table
		u32 nameStringTableOffset = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = proc.name;
		stringTableOffset += (u32)proc.name.size + 1; // + null terminator

		u32 offsetWithinSection = (u32)g_procedureAddresses[procIdx] - (u32)codeSectionOffset;

		Elf64_Sym symbol;
		symbol.st_name = nameStringTableOffset;
		symbol.st_value = offsetWithinSection;
		symbol.st_size = 0;
		symbol.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
		symbol.st_other = STV_DEFAULT;
		symbol.st_shndx = SECTIONIDX_CODE;
		*DynamicArrayAdd(&symbolTable) = symbol;
	}
	// External procedures (all are exported)
	for (u32 procIdx = 1; procIdx < externalProcCount; ++procIdx) {
		Procedure proc = g_context->externalProcedures.unsafe[procIdx];
		// Add name to string table
		u32 nameStringTableOffset = stringTableOffset;
		*DynamicArrayAdd(&stringTable) = proc.name;
		stringTableOffset += (u32)proc.name.size + 1; // + null terminator

		extProcSymbolIndices[procIdx] = symbolTable.size;

		Elf64_Sym symbol;
		symbol.st_name = nameStringTableOffset;
		symbol.st_value = 0;
		symbol.st_size = 0;
		symbol.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
		symbol.st_other = STV_DEFAULT;
		symbol.st_shndx = 0;
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
	Elf64_Rela imageRelocation;
	imageRelocation.r_info = R_X86_64_64;
	imageRelocation.r_info |= (u64)startOfStaticDataSymbolIdx << 32;
	lastPtr = nullptr;
	for (int ptrIdx = 0; ptrIdx < staticDataPtrCount; ++ptrIdx) {
		void *ptr = g_context->staticDataPointersToRelocate.data[ptrIdx];
		if (ptr == lastPtr)
			continue;
		lastPtr = ptr;

		u64 sectionOffset = (u64)ptr - (u64)STATIC_DATA_VIRTUAL_ADDRESS;
		ASSERT(sectionOffset <= U32_MAX);
		imageRelocation.r_offset = (u32)sectionOffset;

		imageRelocation.r_addend = (*(u64 *)ptr) - (u64)STATIC_DATA_VIRTUAL_ADDRESS;

		OutputBufferPut(sizeof(imageRelocation), &imageRelocation);
	}
	u64 dataRelocationTableSize = g_context->outputBufferOffset - dataRelocationTableOffset;

	// Code relocation table
	OutputBufferSeek(g_context->outputBufferSize);
	OutputBufferAlign(16);
	u64 codeRelocationTableOffset = g_context->outputBufferOffset;
	u64 codeRelocationCount = 0;
	for (int relIdx = 0; relIdx < g_relocations.size; ++relIdx) {
		Relocation relocation = g_relocations[relIdx];

		// Seems like in ELF this is not relative to the RIP after the target instruction, but
		// to the start of the relocated 32 bit word itself. So we need to fix up for it
		// ourselves. The new RIP would be this offset +4 bytes (size of the relocated word) +
		// whatever extra bytes the instruction has (we store this in relocation.offsetShift).
		imageRelocation.r_addend = -4 - relocation.offsetShift;

		switch (relocation.type) {
		case RELOCATIONTYPE_EXTERNAL_PROCEDURE:
		{
			u64 sectionOffset = relocation.destOffset - codeSectionOffset;
			ASSERT(sectionOffset <= U32_MAX);
			imageRelocation.r_offset = (u32)sectionOffset;
			imageRelocation.r_info = R_X86_64_PC32;
			imageRelocation.r_info |= (u64)(extProcSymbolIndices[relocation.procedureIdx]) << 32;
			OutputBufferPut(sizeof(imageRelocation), &imageRelocation);
			++codeRelocationCount;
		} break;
		case RELOCATIONTYPE_STATIC_DATA:
		{
			u64 sectionOffset = relocation.destOffset - codeSectionOffset;
			ASSERT(sectionOffset <= U32_MAX);
			imageRelocation.r_offset = (u32)sectionOffset;
			imageRelocation.r_info = R_X86_64_PC32;
			imageRelocation.r_info |= (u64)startOfStaticDataSymbolIdx << 32;

			// @Cleanup
			s32 offsetIntoDataSection;
			OutputBufferPeek(relocation.destOffset, sizeof(s32), &offsetIntoDataSection);
			imageRelocation.r_addend += offsetIntoDataSection;

			OutputBufferPut(sizeof(imageRelocation), &imageRelocation);
			++codeRelocationCount;
		} break;
		}
	}
	u64 codeRelocationTableSize = g_context->outputBufferOffset - codeRelocationTableOffset;

	// Symbol table
	OutputBufferAlign(16);
	u64 symbolSectionOffset = g_context->outputBufferOffset;
	u64 symbolTableOffset = g_context->outputBufferOffset;
	for (int symbolIdx = 0; symbolIdx < symbolTable.size; ++symbolIdx)
		OutputBufferPut(sizeof(symbolTable.data[0]), &symbolTable[symbolIdx]);
	u64 symbolSectionSize = g_context->outputBufferOffset - symbolSectionOffset;

	// String table
	OutputBufferAlign(16);
	u64 stringSectionOffset = g_context->outputBufferOffset;
	u32 stringSectionSize = stringTableOffset;
	u8 zero = 0;
	for (int stringIdx = 0; stringIdx < stringTable.size; ++stringIdx) {
		String str = stringTable[stringIdx];
		OutputBufferPut(str.size, str.data);
		OutputBufferPut(1, &zero);
	}
	// Assert we had the total size right
	ASSERT(g_context->outputBufferOffset == stringSectionOffset + stringSectionSize);

	// Fix headers

	ASSERT(g_context->staticDataSize <= U32_MAX);
	dataSectionHeader.sh_size = (u32)g_context->staticDataSize;
	ASSERT(dataSectionOffset <= U32_MAX);
	dataSectionHeader.sh_offset = (u32)dataSectionOffset;

	ASSERT(g_context->staticDataSize <= U32_MAX);
	codeSectionHeader.sh_size = (u32)codeSectionSize;
	ASSERT(codeSectionOffset <= U32_MAX);
	codeSectionHeader.sh_offset = (u32)codeSectionOffset;

	ASSERT(g_context->staticDataSize <= U32_MAX);
	symbolSectionHeader.sh_size = (u32)symbolSectionSize;
	ASSERT(codeSectionOffset <= U32_MAX);
	symbolSectionHeader.sh_offset = (u32)symbolSectionOffset;
	symbolSectionHeader.sh_info = localSymbolCount;

	ASSERT(g_context->staticDataSize <= U32_MAX);
	stringSectionHeader.sh_size = (u32)stringSectionSize;
	ASSERT(codeSectionOffset <= U32_MAX);
	stringSectionHeader.sh_offset = (u32)stringSectionOffset;

	ASSERT(g_context->staticDataSize <= U32_MAX);
	relDataSectionHeader.sh_size = (u32)dataRelocationTableSize;
	ASSERT(codeSectionOffset <= U32_MAX);
	relDataSectionHeader.sh_offset = (u32)dataRelocationTableOffset;

	ASSERT(g_context->staticDataSize <= U32_MAX);
	relCodeSectionHeader.sh_size = (u32)codeRelocationTableSize;
	ASSERT(codeSectionOffset <= U32_MAX);
	relCodeSectionHeader.sh_offset = (u32)codeRelocationTableOffset;

	// Write headers
	OutputBufferSeek(0);
	OutputBufferPut(sizeof(header), &header);
	OutputBufferPut(sizeof(nullSectionHeader), &nullSectionHeader);
	OutputBufferPut(sizeof(dataSectionHeader), &dataSectionHeader);
	OutputBufferPut(sizeof(codeSectionHeader), &codeSectionHeader);
	OutputBufferPut(sizeof(symbolSectionHeader), &symbolSectionHeader);
	OutputBufferPut(sizeof(stringSectionHeader), &stringSectionHeader);
	OutputBufferPut(sizeof(relDataSectionHeader), &relDataSectionHeader);
	OutputBufferPut(sizeof(relCodeSectionHeader), &relCodeSectionHeader);

	OutputBufferWriteToFile(ChangeFilenameExtension(outputFilename, ".o"_s));

	if (!g_context->config.silent)
		TimerSplit("Generating output image"_s);
}
