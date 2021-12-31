#include <windows.h>
#include <strsafe.h>
#include <shlobj_core.h>
#include <Shlobj.h>

#define STB_SPRINTF_IMPLEMENTATION
#include "stb/stb_sprintf.h"

long long Print(const char *format, ...);

#include "General.h"
#include "Config.h"
#include "Maths.h"
#include "MemoryAlloc.h"
#include "Containers.h"
#include "Strings.h"

Memory *g_memory;
HANDLE g_hStdout;
HANDLE g_hStderr;

s64 Print(const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);
	OutputDebugStringA(buffer);

	// Stdout
	DWORD bytesWritten;
	WriteFile(g_hStdout, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	// Log file
	static HANDLE logFileHandle = CreateFileA(
			"output/log.txt",
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	WriteFile(logFileHandle, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

#if DEBUG_BUILD
	memset(g_memory->framePtr, 0xCD, size + 1);
#endif

	va_end(args);
	return size;
}

const String TPrintF(const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);
	s64 size = stbsp_vsprintf(buffer, format, args);
	va_end(args);

	g_memory->framePtr = (u8 *)g_memory->framePtr + size + 1;

	return { size, buffer };
}

#include "MemoryAlloc.cpp"
#include "Strings.cpp"
#include "Parser.h"
#include "AST.h"

struct Config
{
	bool logAST;
	bool logIR;
	bool logAllocationInfo;
};

struct Procedure;
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
struct BEInstruction;
struct Context
{
	Config config;

	DynamicArray<SourceFile, malloc, realloc> sourceFiles;
	DynamicArray<String, malloc, realloc> libsToLink;
	s32 currentFileIdx;

	// Parsing
	BucketArray<Token, 1024, malloc, realloc> tokens;
	u64 currentTokenIdx;
	Token *token;
	ASTRoot *astRoot;
	BucketArray<ASTExpression, 1024, malloc, realloc> treeNodes;
	BucketArray<ASTType, 1024, malloc, realloc> astTypeNodes;
	BucketArray<String, 1024, malloc, realloc> stringLiterals;

	// Type check
	BucketArray<Value, 512, malloc, realloc> values;
	BucketArray<Procedure, 512, malloc, realloc> procedures;
	BucketArray<Procedure, 128, malloc, realloc> externalProcedures;
	BucketArray<StaticDefinition, 512, malloc, realloc> staticDefinitions;
	BucketArray<const TypeInfo, 1024, malloc, realloc> typeTable;
	DynamicArray<TCScope, malloc, realloc> tcStack;
	s64 tcCurrentReturnType;

	// IR
	DynamicArray<IRStaticVariable, malloc, realloc> irStaticVariables;
	DynamicArray<u32, malloc, realloc> irExternalVariables;
	DynamicArray<IRScope, malloc, realloc> irStack;
	DynamicArray<IRProcedureScope, malloc, realloc> irProcedureStack;
	BucketArray<IRLabel, 1024, malloc, realloc> irLabels;
	IRLabel *currentBreakLabel;
	IRLabel *currentContinueLabel;

	// Backend
	HANDLE outputFile;

	BucketArray<BasicBlock, 512, malloc, realloc> beBasicBlocks;
	DynamicArray<BasicBlock *, malloc, realloc> beLeafBasicBlocks;
	DynamicArray<InterferenceGraphNode, malloc, realloc> beInterferenceGraph;
	BucketArray<BEInstruction, 128, malloc, realloc> bePatchedInstructions;
};

inline bool Win32FileExists(const char *filename)
{
	DWORD attrib = GetFileAttributes(filename);
	return attrib != INVALID_FILE_ATTRIBUTES && attrib != FILE_ATTRIBUTE_DIRECTORY;
}

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

String GetSourceLine(Context *context, s32 fileIdx, s32 line)
{
	SourceFile sourceFile = context->sourceFiles[fileIdx];
	String sourceLine = {};
	{
		int l = 1;
		for (const char *scan = (const char *)sourceFile.buffer; *scan; ++scan)
		{
			if (l == line)
			{
				sourceLine.data = scan;
				break;
			}
			if (*scan == '\n')
				++l;
		}
		for (const char *scan = sourceLine.data; ; ++scan)
		{
			if (!*scan || *scan == '\n' || *scan == '\r')
				break;
			++sourceLine.size;
		}
	}
	return sourceLine;
}

const String TokenTypeToString(s32 type)
{
	switch (type)
	{
	case TOKEN_INVALID:
		return "<Invalid>"_s;
	case TOKEN_IDENTIFIER:
		return "<Identifier>"_s;

	case TOKEN_LITERAL_CHARACTER:
		return "<Literal char>"_s;
	case TOKEN_LITERAL_NUMBER:
		return "<Literal number>"_s;
	case TOKEN_LITERAL_STRING:
		return "<Literal string>"_s;

	case TOKEN_OP_ASSIGNMENT:
		return "< = >"_s;
	case TOKEN_OP_ASSIGNMENT_PLUS:
		return "< += >"_s;
	case TOKEN_OP_ASSIGNMENT_MINUS:
		return "< -= >"_s;
	case TOKEN_OP_ASSIGNMENT_MULTIPLY:
		return "< *= >"_s;
	case TOKEN_OP_ASSIGNMENT_DIVIDE:
		return "< /= >"_s;
	case TOKEN_OP_ASSIGNMENT_MODULO:
		return "< %= >"_s;
	case TOKEN_OP_ASSIGNMENT_SHIFT_LEFT:
		return "< <<= >"_s;
	case TOKEN_OP_ASSIGNMENT_SHIFT_RIGHT:
		return "< >>= >"_s;
	case TOKEN_OP_ASSIGNMENT_OR:
		return "< ||= >"_s;
	case TOKEN_OP_ASSIGNMENT_AND:
		return "< &&= >"_s;
	case TOKEN_OP_EQUALS:
		return "< == >"_s;
	case TOKEN_OP_GREATER_THAN:
		return "< > >"_s;
	case TOKEN_OP_GREATER_THAN_OR_EQUAL:
		return "< >= >"_s;
	case TOKEN_OP_LESS_THAN:
		return "< < >"_s;
	case TOKEN_OP_LESS_THAN_OR_EQUAL:
		return "< <= >"_s;
	case TOKEN_OP_PLUS:
		return "< + >"_s;
	case TOKEN_OP_MINUS:
		return "< - >"_s;
	case TOKEN_OP_MULTIPLY:
		return "< * >"_s;
	case TOKEN_OP_DIVIDE:
		return "< / >"_s;
	case TOKEN_OP_MODULO:
		return "< % >"_s;
	case TOKEN_OP_SHIFT_LEFT:
		return "< << >"_s;
	case TOKEN_OP_SHIFT_RIGHT:
		return "< >> >"_s;
	case TOKEN_OP_POINTER_TO:
		return "< ^ >"_s;
	case TOKEN_OP_DEREFERENCE:
		return "< @ >"_s;
	case TOKEN_OP_ARRAY_ACCESS:
		return "< [] >"_s;
	case TOKEN_OP_ARROW:
		return "< -> >"_s;
	case TOKEN_OP_VARIABLE_DECLARATION:
		return "< : >"_s;
	case TOKEN_OP_VARIABLE_DECLARATION_STATIC:
		return "< :s >"_s;
	case TOKEN_OP_STATIC_DEF:
		return "< :: >"_s;
	case TOKEN_OP_RANGE:
		return "< .. >"_s;

	case TOKEN_END_OF_FILE:
		return "<EOF>"_s;
	}

	if (type >= TOKEN_KEYWORD_Begin && type <= TOKEN_KEYWORD_End)
		return "<Keyword>"_s;
	if (type >= TOKEN_OP_Begin && type <= TOKEN_OP_End)
		return "<Operator>"_s;

	char *str = (char *)FrameAlloc(5);
	strncpy(str, "<'~'>", 5);
	str[2] = (char)type;
	return { 5, str };
}

// @Speed: pass token by copy?
const String TokenToString(Token *token)
{
	if (token->type >= TOKEN_KEYWORD_Begin && token->type <= TOKEN_KEYWORD_End)
		return token->string;

	return TokenTypeToString(token->type);
}

void Log(Context *context, SourceLocation loc, String str)
{
	SourceFile sourceFile = context->sourceFiles[loc.fileIdx];
	Print("%S %d:%d %S\n", sourceFile.name, loc.line, loc.character, str);

	String sourceLine = GetSourceLine(context, loc.fileIdx, loc.line);
	Print("... %S\n... ", sourceLine);

	int shift = 0;
	for (int i = 0; i < loc.character; ++i)
	{
		if (sourceLine.data[i] == '\t')
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

#include "Tokenizer.cpp"
#include "Parser.cpp"
#include "PrintAST.cpp"
#include "TypeChecker.cpp"
#include "IRGen.cpp"
#include "PrintIR.cpp"
#include "x64.cpp"

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
	DynamicArrayInit(&context.sourceFiles, 8);
	DynamicArrayInit(&context.libsToLink, 8);
	BucketArrayInit(&context.tokens);

	DynamicArray<String, FrameAlloc, FrameRealloc> inputFiles;
	DynamicArrayInit(&inputFiles, 8);
	*DynamicArrayAdd(&inputFiles) = "core/basic.emi"_s;
	*DynamicArrayAdd(&inputFiles) = "core/print.emi"_s;
	for (int argIdx = 1; argIdx < argc; ++argIdx)
	{
		char *arg = argv[argIdx];
		if (arg[0] == '-')
		{
			if (strcmp("-logAST", arg) == 0)
				context.config.logAST = true;
			else if (strcmp("-logIR", arg) == 0)
				context.config.logIR = true;
			else if (strcmp("-logAllocationInfo", arg) == 0)
				context.config.logAllocationInfo = true;
			else
				Print("Unknown option \"%s\"\n", arg);
		}
		else
		{
			*DynamicArrayAdd(&inputFiles) = CStrToString(arg);
		}
	}
	ASSERT(inputFiles.size > 1);

	for (int i = 0; i < inputFiles.size; ++i)
	{
		const char *filenameCstr = StringToCStr(inputFiles[i], FrameAlloc);
		if (!Win32FileExists(filenameCstr))
			LogError(&context, {}, TPrintF("Input source file \"%S\" doesn't exist!", inputFiles[i]));

		SourceFile newSourceFile = { inputFiles[i] };
		Win32ReadEntireFile(filenameCstr, &newSourceFile.buffer,
				&newSourceFile.size, FrameAlloc);
		context.currentFileIdx = (s32)context.sourceFiles.size;
		*DynamicArrayAdd(&context.sourceFiles) = newSourceFile;
		TokenizeFile(&context);
	}
	Token eofToken = { TOKEN_END_OF_FILE };
	*BucketArrayAdd(&context.tokens) = eofToken;

	GenerateSyntaxTree(&context);

	if (context.config.logAST)
		PrintAST(&context);

	TypeCheckMain(&context);

	IRGenMain(&context);

	if (context.config.logIR)
	{
		PrintIRInstructions(&context);
	}

	BackendMain(&context);

	Print("Compilation success\n");
	return 0;
}
