#include <windows.h>
#include <strsafe.h>

#define STB_SPRINTF_IMPLEMENTATION
#include "stb/stb_sprintf.h"

void Log(const char *format, ...);

#include "General.h"
#include "Maths.h"
#include "MemoryAlloc.h"
#include "Containers.h"
#include "Strings.h"

Memory *g_memory;
HANDLE g_hStdout;

void Log(const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);

	stbsp_vsprintf(buffer, format, args);
	OutputDebugStringA(buffer);

	// Stdout
	DWORD bytesWritten;
	WriteFile(g_hStdout, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	va_end(args);
}

const String TPrintF(const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);
	s64 size = stbsp_vsprintf(buffer, format, args);
	va_end(args);

	g_memory->framePtr = (u8 *)g_memory->framePtr + size;

	return { size, buffer };
}

#include "MemoryAlloc.cpp"
#include "Strings.cpp"
#include "Parser.h"
#include "AST.h"

struct Token;
struct ASTRoot;
struct ASTExpression;
struct TypeInfo;
struct TCScope;
struct IRProcedure;
struct IRStaticVariable;
struct IRScope;
struct Context
{
	String filename;
	u8 *fileBuffer;
	u64 fileSize;

	// Parsing
	BucketArray<Token, 1024, malloc, realloc> tokens;
	u64 currentTokenIdx;
	Token *token;
	ASTRoot *astRoot;
	BucketArray<ASTExpression, 1024, malloc, realloc> treeNodes;

	// Type check
	DynamicArray<TypeInfo, malloc, realloc> typeTable;
	DynamicArray<TCScope, malloc, realloc> tcStack;
	Type currentReturnType;

	// IR
	DynamicArray<IRProcedure, malloc, realloc> irProcedures;
	DynamicArray<IRStaticVariable, malloc, realloc> irStaticVariables;
	DynamicArray<IRScope, malloc, realloc> irStack;
	u64 currentProcedureIdx;
	u64 currentRegisterId;
	u64 currentLabelId;
	String currentBreakLabel;
};

#include "Tokenizer.cpp"

inline void PrintError(Context *context, SourceLocation loc, const String errorStr)
{
	Log("ERROR! %.*s %d:%d\n... %.*s\n", loc.file.size, loc.file.data, loc.line, loc.character,
			errorStr.size, errorStr.data);

	// Print line
	const char *beginningOfLine = nullptr;
	int size = 0;
	{
		int l = 1;
		for (const char *scan = (const char *)context->fileBuffer; *scan; ++scan)
		{
			if (l == loc.line)
			{
				beginningOfLine = scan;
				break;
			}
			if (*scan == '\n')
				++l;
		}
		for (const char *scan = beginningOfLine; ; ++scan)
		{
			if (!*scan || *scan == '\n')
				break;
			++size;
		}
	}
	Log("... %.*s\n... ", size, beginningOfLine);

	int shift = 0;
	for (int i = 0; i < loc.character; ++i)
	{
		if (beginningOfLine[i] == '\t')
			shift += 4;
		else
			++shift;
	}

	for (int i = 0; i < shift; ++i)
		Log(" ");
	for (int i = 0; i < loc.size; ++i)
		Log("^");
	Log("\n");

	CRASH;
}

inline void PrintWarning(SourceLocation loc, const String errorStr)
{
	Log("WARNING! %s:%d\n... %.*s\n", loc.file, loc.line, errorStr.size, errorStr.data);
}

void AssertToken(Context *context, Token *token, int type)
{
	if (token->type != type)
	{
		const String tokenTypeGot = TokenToString(token);
		const String tokenTypeExp = TokenTypeToString(type);
		const String errorStr = TPrintF("Expected token of type %.*s but got %.*s",
				tokenTypeExp.size, tokenTypeExp.data, tokenTypeGot.size, tokenTypeGot.data);
		PrintError(context, token->loc, errorStr);
	}
}

void UnexpectedTokenError(Context *context, Token *token)
{
	const String tokenType = TokenTypeToString(token->type);
	const String errorStr = TPrintF("Unexpected token of type %.*s",
			tokenType.size, tokenType.data);
	PrintError(context, token->loc, errorStr);
}

#include "PrintAST.cpp"
#include "Parser.cpp"
#include "TypeChecker.cpp"
#include "IRGen.cpp"
#include "WriteToC.cpp"

bool Win32ReadEntireFile(const char *filename, u8 **fileBuffer, u64 *fileSize, void *(*allocFunc)(u64))
{
	char fullname[MAX_PATH];
	DWORD written = GetCurrentDirectory(MAX_PATH, fullname);
	fullname[written++] = '/';
	strcpy(fullname + written, filename);

	HANDLE file = CreateFileA(
			fullname,
			GENERIC_READ,
			FILE_SHARE_READ,
			nullptr,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	DWORD error = GetLastError();
	ASSERT(file != INVALID_HANDLE_VALUE);

	if (file == INVALID_HANDLE_VALUE)
	{
		*fileBuffer = nullptr;
	}
	else
	{
		DWORD fileSizeDword = GetFileSize(file, nullptr);
		ASSERT(fileSizeDword);
		*fileSize = (u64)fileSizeDword;
		error = GetLastError();

		*fileBuffer = (u8 *)allocFunc(*fileSize);
		DWORD bytesRead;
		bool success = ReadFile(
				file,
				*fileBuffer,
				fileSizeDword,
				&bytesRead,
				nullptr
				);
		ASSERT(success);
		ASSERT(bytesRead == *fileSize);

		CloseHandle(file);
	}

	return error == ERROR_SUCCESS;
}

int main(int argc, char **argv)
{
	g_hStdout = GetStdHandle(STD_OUTPUT_HANDLE);

	if (argc != 2)
		return 1;

	const char *filename = argv[1];

	// Allocate memory
	Memory memory;
	g_memory = &memory;
	memory.frameMem = VirtualAlloc(0, Memory::frameSize, MEM_COMMIT, PAGE_READWRITE);
	MemoryInit(&memory);

	Context context = {};
	context.filename = CStrToString(filename);
	BucketArrayInit(&context.tokens);

	Win32ReadEntireFile(filename, &context.fileBuffer, &context.fileSize, FrameAlloc);

	TokenizeFile(&context);

	GenerateSyntaxTree(&context);

	TypeCheckMain(&context);

	IRGenMain(&context);

	WriteToC(&context);

	Log("Compilation success\n");
	return 0;
}
