enum ConsoleColor {
	CONSOLE_RESET_COLOR,
	CONSOLE_BLACK_TXT = 30,
	CONSOLE_RED_TXT,
	CONSOLE_GREEN_TXT,
	CONSOLE_YELLOW_TXT,
	CONSOLE_BLUE_TXT,
	CONSOLE_MAGENTA_TXT,
	CONSOLE_CYAN_TXT,
	CONSOLE_WHITE_TXT,

	CONSOLE_BLACK_BKG = 40,
	CONSOLE_RED_BKG,
	CONSOLE_GREEN_BKG,
	CONSOLE_YELLOW_BKG,
	CONSOLE_BLUE_BKG,
	CONSOLE_MAGENTA_BKG,
	CONSOLE_CYAN_BKG,
	CONSOLE_WHITE_BKG
};

inline void ConsoleSetColor(ConsoleColor color, bool bright = false, bool underline = false)
{
	if (g_context->config.useEscapeSequences)
		Print("\x1b[%d;%d;%dm", color, bright ? 1 : 22, underline ? 4 : 24);
}

FatSourceLocation ExpandSourceLocation(SourceLocation loc);

void __Log(SourceLocation loc, String str, String prefix, ConsoleColor color, bool bright
#if DEBUG_BUILD
		, const char *inFile, const char *inFunc, int inLine
#endif
		)
{
	ScopedLockMutex lock(g_context->consoleMutex);

	FatSourceLocation fatLoc = {};
	if (loc.fileIdx != 0) {
		fatLoc = ExpandSourceLocation(loc);
		String filename = g_context->sourceFiles[loc.fileIdx].name;

		ConsoleSetColor(CONSOLE_BLACK_TXT, true);

		// Info
		Print("%S %d:%d ", filename, fatLoc.line, fatLoc.column);
	}

	// String
	if (prefix.size) {
		ConsoleSetColor(color, bright, true);
		PrintString(prefix);
	}
	ConsoleSetColor(color, bright);
	Print("%S%S\n", prefix.size ? " "_s : String{}, str);

	if (loc.fileIdx != 0) {
		// Source line
		ConsoleSetColor(CONSOLE_BLACK_TXT, true);
		Print("... ");
		const char *line = fatLoc.beginingOfLine;
		u64 col = fatLoc.column - 1;
		u64 tokenSize = fatLoc.size;

		ConsoleSetColor(CONSOLE_RESET_COLOR);
		PrintString({ col, line });
		ConsoleSetColor(CONSOLE_CYAN_TXT);
		PrintString({ tokenSize, line + col });
		ConsoleSetColor(CONSOLE_RESET_COLOR);
		PrintString({ fatLoc.lineSize - col - tokenSize, line + col + tokenSize });
		Print("\n");

		// Token underline
		if (!g_context->config.useEscapeSequences) {
			ConsoleSetColor(CONSOLE_BLACK_TXT, true);
			Print("... ");

			for (u32 i = 0; i < col; ++i) {
				if (fatLoc.beginingOfLine[i] == '\t')
					Print("\t");
				else
					Print(" ");
			}
			for (u32 i = 0; i < fatLoc.size; ++i)
				Print("^");
			Print("\n");
		}
	}

#if DEBUG_BUILD
	ConsoleSetColor(CONSOLE_BLACK_TXT, true);
	Print("~~~ In %s - %s:%d\n", inFunc, inFile, inLine);
	ConsoleSetColor(CONSOLE_RESET_COLOR, true);
#endif

	ConsoleSetColor(CONSOLE_RESET_COLOR);
}

void __LogRange(SourceLocation locBegin, SourceLocation locEnd, String str, String prefix,
		ConsoleColor color, bool bright
#if DEBUG_BUILD
		, const char *inFile, const char *inFunc, int inLine
#endif
		)
{
	ScopedLockMutex lock(g_context->consoleMutex);

	ASSERT(locBegin.fileIdx == locEnd.fileIdx);
	FatSourceLocation fatLocBegin = ExpandSourceLocation(locBegin);
	FatSourceLocation fatLocEnd   = ExpandSourceLocation(locEnd);
	String filename = g_context->sourceFiles[locBegin.fileIdx].name;

	ConsoleSetColor(CONSOLE_BLACK_TXT, true);

	// Info
	Print("%S %d:%d ", filename, fatLocBegin.line, fatLocBegin.column);

	// String
	if (prefix.size) {
		ConsoleSetColor(color, bright, true);
		PrintString(prefix);
	}
	ConsoleSetColor(color, bright);
	Print("%S%S\n", prefix.size ? " "_s : String{}, str);

	// Source line
	ConsoleSetColor(CONSOLE_BLACK_TXT, true);
	Print("... ");
	const char *line = fatLocBegin.beginingOfLine;
	u64 col = fatLocBegin.column - 1;
	u64 tokenSize = fatLocEnd.column - fatLocBegin.column + fatLocEnd.size;

	// Limit to first line
	if (col + tokenSize > fatLocBegin.lineSize)
		tokenSize = fatLocBegin.lineSize - col;

	ConsoleSetColor(CONSOLE_RESET_COLOR);
	PrintString({ col, line });
	ConsoleSetColor(CONSOLE_CYAN_TXT);
	PrintString({ tokenSize, line + col });
	ConsoleSetColor(CONSOLE_RESET_COLOR);
	PrintString({ fatLocBegin.lineSize - col - tokenSize, line + col + tokenSize });
	Print("\n");

	// Token underline
	if (!g_context->config.useEscapeSequences) {
		ConsoleSetColor(CONSOLE_BLACK_TXT, true);
		Print("... ");

		for (u32 i = 0; i < col; ++i) {
			if (line[i] == '\t')
				Print("\t");
			else
				Print(" ");
		}
		for (u32 i = 0; i < tokenSize; ++i)
			Print("^");
		Print("\n");
	}

#if DEBUG_BUILD
	ConsoleSetColor(CONSOLE_BLACK_TXT, true);
	Print("~~~ In %s - %s:%d\n", inFunc, inFile, inLine);
	ConsoleSetColor(CONSOLE_RESET_COLOR, true);
#endif

	ConsoleSetColor(CONSOLE_RESET_COLOR);
}

#define Log(loc, str) do {\
	__Log(loc, str, {},					 CONSOLE_RESET_COLOR,	false DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#define LogErrorNoCrash(loc, str) do {\
	__Log(loc, str, "ERROR:"_s,			 CONSOLE_RED_TXT,		false DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#define LogWarning(loc, str) do {\
	__Log(loc, str, "WARNING:"_s,		 CONSOLE_YELLOW_TXT,	false DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#define LogNote(loc, str) do {\
	__Log(loc, str, "NOTE:"_s,			 CONSOLE_WHITE_TXT,		true  DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#define LogCompilerError(loc, str) do {\
	__Log(loc, str, "COMPILER ERROR:"_s, CONSOLE_RED_TXT,		true  DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
	PANIC; \
} while (0)

#define Log2(locBegin, locEnd, str) do {\
	__LogRange(locBegin, locEnd, str, {},			CONSOLE_RESET_COLOR, false DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#define Log2ErrorNoCrash(locBegin, locEnd, str) do {\
	__LogRange(locBegin, locEnd, str, "ERROR:"_s,	CONSOLE_RED_TXT,	 false DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#define Log2Warning(locBegin, locEnd, str) do {\
	__LogRange(locBegin, locEnd, str, "WARNING:"_s,	CONSOLE_YELLOW_TXT,	 false DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#define Log2Note(locBegin, locEnd, str) do {\
	__LogRange(locBegin, locEnd, str, "NOTE:"_s,	CONSOLE_WHITE_TXT,	 true  DEBUG_ONLY(, __FILE__, __func__, __LINE__)); \
} while (0)

#if EXIT_ON_FIRST_ERROR
#define LogError(loc, str) do {\
	LogErrorNoCrash(loc, str); \
	PANIC; \
} while(0)
#define Log2Error(locBegin, locEnd, str) do {\
	Log2ErrorNoCrash(locBegin, locEnd, str); \
	PANIC; \
} while(0)
#else
#define LogError(loc, str) do {\
	LogErrorNoCrash(loc, str); \
	SwitchJob(YIELDREASON_FAILED, {}); \
} while(0)
#define Log2Error(locBegin, locEnd, str) do {\
	Log2ErrorNoCrash(locBegin, locEnd, str); \
	SwitchJob(YIELDREASON_FAILED, {}); \
} while(0)
#endif
