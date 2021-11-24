#include <windows.h>
#include <strsafe.h>
#include <shlobj_core.h>
#include <Shlobj.h>

#define STB_SPRINTF_IMPLEMENTATION
#include "stb/stb_sprintf.h"

void Print(const char *format, ...);

#include "General.h"
#include "Config.h"
#include "Maths.h"
#include "MemoryAlloc.h"
#include "Containers.h"
#include "Strings.h"

Memory *g_memory;
HANDLE g_hStdout;
HANDLE g_hStderr;

void Print(const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);

	stbsp_vsprintf(buffer, format, args);
	OutputDebugStringA(buffer);

	// Stdout
	DWORD bytesWritten;
	WriteFile(g_hStdout, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	// Log file
	static HANDLE logFileHandle = CreateFileA(
			"log.txt",
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	WriteFile(logFileHandle, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

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

struct Config
{
	bool silent;
	bool windowsSubsystem;
};

struct Token;
struct ASTRoot;
struct ASTExpression;
struct ASTType;
struct TypeInfo;
struct StaticDefinition;
struct TCScope;
struct IRStaticVariable;
struct IRScope;
struct IRProcedureScope;
struct IRLabel;
struct IRInstruction;
struct BasicBlock;
struct InterferenceGraphNode;
struct X64Instruction;
struct Context
{
	Config config;

	String filename;
	u8 *fileBuffer;
	u64 fileSize;

	// Parsing
	BucketArray<Token, 1024, malloc, realloc> tokens;
	u64 currentTokenIdx;
	Token *token;
	ASTRoot *astRoot;
	BucketArray<ASTExpression, 1024, malloc, realloc> treeNodes;
	BucketArray<ASTType, 1024, malloc, realloc> astTypeNodes;

	// Type check
	BucketArray<Variable, 512, malloc, realloc> variables;
	BucketArray<Procedure, 512, malloc, realloc> procedures;
	BucketArray<StaticDefinition, 512, malloc, realloc> staticDefinitions;
	BucketArray<TypeInfo, 1024, malloc, realloc> typeTable;
	DynamicArray<TCScope, malloc, realloc> tcStack;
	s64 tcCurrentReturnType;

	// IR
	DynamicArray<IRStaticVariable, malloc, realloc> irStaticVariables;
	DynamicArray<IRScope, malloc, realloc> irStack;
	DynamicArray<IRProcedureScope, malloc, realloc> irProcedureStack;
	BucketArray<IRLabel, 1024, malloc, realloc> irLabels;
	IRLabel *currentBreakLabel;

	// Backend
	HANDLE outputFile;

	BucketArray<BasicBlock, 512, malloc, realloc> basicBlocks;
	DynamicArray<BasicBlock *, malloc, realloc> leafBasicBlocks;
	DynamicArray<InterferenceGraphNode, malloc, realloc> interferenceGraph;
	BucketArray<X64Instruction, 128, malloc, realloc> patchedInstructions;
};

#include "Tokenizer.cpp"

void Log(Context *context, SourceLocation loc, String str)
{
	Print("%S %d:%d %S\n", loc.file, loc.line, loc.character, str);

	// Print line
	const char *beginningOfLine = nullptr;
	int size = 0;
	{
		int l = 1;
		for (const char *scan = (const char *)loc.fileBuffer; *scan; ++scan)
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
	Print("... %.*s\n... ", size, beginningOfLine);

	int shift = 0;
	for (int i = 0; i < loc.character; ++i)
	{
		if (beginningOfLine[i] == '\t')
			shift += 4;
		else
			++shift;
	}

	for (int i = 0; i < shift; ++i)
		Print(" ");
	for (int i = 0; i < loc.size; ++i)
		Print("^");
	Print("\n");
}

inline void LogErrorNoCrash(Context *context, SourceLocation loc, String errorStr)
{
	Log(context, loc, StringConcat("ERROR: "_s, errorStr));
}

#define LogError(context, loc, str) do { LogErrorNoCrash(context, loc, str); CRASH; } while(0)

inline void LogWarning(Context *context, SourceLocation loc, String str)
{
	Log(context, loc, StringConcat("WARNING: "_s, str));
}

inline void LogNote(Context *context, SourceLocation loc, String str)
{
	Log(context, loc, StringConcat("NOTE: "_s, str));
}

void AssertToken(Context *context, Token *token, int type)
{
	if (token->type != type)
	{
		const String tokenTypeGot = TokenToString(token);
		const String tokenTypeExp = TokenTypeToString(type);
		const String errorStr = TPrintF("Expected token of type %S but got %S",
				tokenTypeExp, tokenTypeGot);
		LogError(context, token->loc, errorStr);
	}
}

void UnexpectedTokenError(Context *context, Token *token)
{
	const String tokenType = TokenTypeToString(token->type);
	const String errorStr = TPrintF("Unexpected token of type %S", tokenType);
	LogError(context, token->loc, errorStr);
}

#include "Parser.cpp"
#include "PrintAST.cpp"
#include "TypeChecker.cpp"
#include "IRGen.cpp"
#include "PrintIR.cpp"
//#include "Optimize.cpp"
//#include "WriteToC.cpp"
#include "x64.cpp"

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
	g_hStderr = GetStdHandle(STD_ERROR_HANDLE);

	if (argc < 2)
		return 1;

	// Allocate memory
	Memory memory;
	g_memory = &memory;
	memory.frameMem = VirtualAlloc(0, Memory::frameSize, MEM_COMMIT, PAGE_READWRITE);
	MemoryInit(&memory);

	Context context = {};
	BucketArrayInit(&context.tokens);

	DynamicArray<String, malloc, realloc> inputFiles;
	DynamicArrayInit(&inputFiles, 8);
	*DynamicArrayAdd(&inputFiles) = "basic.emi"_s;
	*DynamicArrayAdd(&inputFiles) = "print.emi"_s;
	for (int argIdx = 1; argIdx < argc; ++argIdx)
	{
		char *arg = argv[argIdx];
		if (arg[0] == '-')
		{
			if (strcmp("-silent", arg) == 0)
				context.config.silent = true;
			else if (strcmp("-windowsSubsystem", arg) == 0)
				context.config.windowsSubsystem = true;
		}
		else
		{
			*DynamicArrayAdd(&inputFiles) = CStrToString(arg);
		}
	}
	ASSERT(inputFiles.size > 1);

	for (int i = 0; i < inputFiles.size; ++i)
	{
		context.filename = inputFiles[i];
		Win32ReadEntireFile(StringToCStr(inputFiles[i], FrameAlloc), &context.fileBuffer,
				&context.fileSize, FrameAlloc);
		TokenizeFile(&context);
	}
	Token eofToken = { TOKEN_END_OF_FILE };
	*BucketArrayAdd(&context.tokens) = eofToken;

	GenerateSyntaxTree(&context);
#if PRINT_AST_TREE
	if (!context.config.silent)
		PrintAST(&context);
#endif

	TypeCheckMain(&context);

	IRGenMain(&context);

#if PRINT_IR
	if (!context.config.silent)
	{
		Print("PRE-OPTIMIZING IR\n");
		PrintIRInstructions(&context);
	}
#endif

	BackendConvert(&context);

	//OptimizerMain(&context);

#if 0//PRINT_IR
	if (!context.config.silent)
	{
		Print("POST-OPTIMIZING IR\n");
		PrintIRInstructions(&context);
	}
#endif

	//BackendMain(&context);

	Print("Compilation success\n");
	return 0;
}
