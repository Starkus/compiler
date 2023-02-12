void X64WriteTextAssemblyFile(String outputFilename)
{
	OutputBufferReset();

#if IS_WINDOWS
	OutputBufferPrint("_DATA SEGMENT\n");
#else
	OutputBufferPrint("section .data\n");
#endif

	OutputBufferPrint("ALIGN 16\n");

	{
		PROFILER_SCOPE("Writing all static variables");

		OutputBufferPrint("__start_of_static_data:\n");

		qsort(g_context->staticDataPointersToRelocate.data,
				g_context->staticDataPointersToRelocate.size,
				sizeof(void *), ComparePointers);

		{
			u8 *scan = STATIC_DATA_VIRTUAL_ADDRESS;
			u8 *end  = scan + g_context->staticDataSize;
			u64 nextPointerIdx = 0;
			void *nextPointerToRelocate = (u8 *)g_context->staticDataPointersToRelocate[nextPointerIdx];
			while (scan < end) {
				u8 *current = scan;
				if (scan == nextPointerToRelocate) {
					u64 qword = *(u64 *)scan;

					// Assert the pointer points to static data memory (or is null)
					ASSERT(qword == 0 || (
							qword >= (u64)STATIC_DATA_VIRTUAL_ADDRESS &&
							qword < (u64)STATIC_DATA_VIRTUAL_ADDRESS_END));

					if (qword != 0) {
						u64 offset = qword - (u64)STATIC_DATA_VIRTUAL_ADDRESS;
						OutputBufferPrint("DQ __start_of_static_data + 0%llXh", offset);
					}
					else
						OutputBufferPrint("DQ 00h ;nullptr");
					scan += 8;

					// Find next pointer to relocate, skipping duplicates
					++nextPointerIdx;
					u64 pointersCount = g_context->staticDataPointersToRelocate.size;
					for (void *nextPtr = 0; nextPointerIdx < pointersCount; ++nextPointerIdx) {
						nextPtr = g_context->staticDataPointersToRelocate[nextPointerIdx];
						if (nextPtr != nextPointerToRelocate) {
							nextPointerToRelocate = nextPtr;
							break;
						}
					}
				}
				// If there's a whole quad word to read...
				else if (scan <= end-8) {
					u64 qword = *(u64 *)scan;

					// @Delete
#if DEBUG_BUILD
					ASSERT(qword < (u64)STATIC_DATA_VIRTUAL_ADDRESS ||
						   qword > (u64)STATIC_DATA_VIRTUAL_ADDRESS_END);
#endif

					OutputBufferPrint("DQ 0%.16llXh", qword);
					scan += 8;
				}
				else {
					OutputBufferPrint("DB 0%.2Xh", *scan++);
				}
				OutputBufferPrint("\t\t; static_data + 0x%llX\n",
						(u64)(current - STATIC_DATA_VIRTUAL_ADDRESS));
			}
		}

#if IS_WINDOWS
		OutputBufferPrint("_DATA ENDS\n");
		OutputBufferPrint("_BSS SEGMENT\n");
#else
		OutputBufferPrint("section .bss\n");
#endif
	}

#if IS_WINDOWS
	OutputBufferPrint("_BSS ENDS\n");
#endif

#if IS_LINUX
	u64 procedureCount = g_context->procedures.GetForRead()->count;
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx) {
		Procedure proc = GetProcedureRead(procedureIdx);
		if (proc.isExported)
			OutputBufferPrint("GLOBAL %S\n", proc.name);
	}
#endif

	{
		PROFILER_SCOPE("Writing external variables");

		auto externalVars = g_context->irExternalVariables.GetForRead();
		for (int varIdx = 0; varIdx < externalVars->size; ++varIdx) {
			Value v = GetGlobalValue(externalVars[varIdx]);
			s64 size = GetTypeInfo(v.typeTableIdx).size;
			String type;
			switch (size) {
				case 1: type = "BYTE"_s; break;
				case 2: type = "WORD"_s; break;
				case 4: type = "DWORD"_s; break;
				case 8: type = "QWORD"_s; break;
				default: type = "QWORD"_s;
			}
			String name = StringExpand(v.externalSymbolName);
#if IS_WINDOWS
			OutputBufferPrint("EXTRN %S:%S\n", name, type);
#else
			OutputBufferPrint("EXTERN %S\n", name);
#endif
		}
	}

	{
		PROFILER_SCOPE("Writing external procedures");

		auto externalProcedures = g_context->externalProcedures.GetForRead();
		u64 externalProcedureCount = externalProcedures->count;
		for (u32 procedureIdx = 1; procedureIdx < externalProcedureCount; ++procedureIdx) {
			String procName = externalProcedures[procedureIdx].name;
#if IS_WINDOWS
			OutputBufferPrint("EXTRN %S:proc\n", procName);
#else
			OutputBufferPrint("EXTERN %S\n", procName);
#endif
		}
	}

#if IS_WINDOWS
	OutputBufferPrint("_TEXT SEGMENT\n");
#else
	OutputBufferPrint("section .text\n");
#endif

	// Code
	X64PrintInstructions();

#if IS_WINDOWS
	OutputBufferPrint("_TEXT ENDS\n");
	OutputBufferPrint("END\n");
#endif

	OutputBufferWriteToFile(ChangeFilenameExtension(outputFilename, ".asm"_s));
}
