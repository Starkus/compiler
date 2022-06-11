#include "General.h"
#include "Strings.h"
#include "MemoryAlloc.h"

#define USE_PROFILER_API 0

const String TPrintF(const char *format, ...);

#if _MSC_VER
#include "Windows.cpp"
#else
#include "Linux.cpp"
#endif

#define STB_SPRINTF_IMPLEMENTATION
#include "stb/stb_sprintf.h"

#include "Config.h"
#include "Maths.h"
#include "Containers.h"

#if USE_PROFILER_API
#include "Superluminal/PerformanceAPI_loader.h"

PerformanceAPI_Functions performanceAPI;
#endif

Memory *g_memory;
FileHandle g_hStdout;
FileHandle g_hStderr;

s64 Print(const char *format, ...)
{
	// Log file
	static FileHandle logFileHandle = SYSOpenFileWrite("output/log.txt"_s);

	char *buffer = (char *)g_memory->phasePtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);
#if _MSC_VER
	OutputDebugStringA(buffer);
#endif

	// Stdout
	SYSWriteFile(g_hStdout, buffer, strlen(buffer));

	// Log file
	SYSWriteFile(logFileHandle, buffer, strlen(buffer));

#if DEBUG_BUILD
	memset(g_memory->phasePtr, 0x55, size + 1);
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

u64 g_firstPerfCounter;
u64 g_lastPerfCounter;
u64 g_perfFrequency;
void SetUpTimers()
{
	g_firstPerfCounter = SYSPerformanceCounter();
	g_lastPerfCounter  = g_firstPerfCounter;
	g_perfFrequency = SYSPerformanceFrequency();
}
void TimerSplit(String message)
{
	u64 newPerfCounter = SYSPerformanceCounter();
	f64 time = (f64)(newPerfCounter - g_firstPerfCounter) / (f64)g_perfFrequency;
	f64 deltaTime = (f64)(newPerfCounter - g_lastPerfCounter) / (f64)g_perfFrequency;
	g_lastPerfCounter = newPerfCounter;
	Print("%f - %f - %S\n", time, deltaTime, message);
}
u64 CycleCountBegin()
{
	return __rdtsc();
}
u64 CycleCountEnd(u64 begin)
{
	u64 newPerfCounter = __rdtsc();
	return newPerfCounter - begin;
}

#include "MemoryAlloc.cpp"
#include "Strings.cpp"
#include "Parser.h"
#include "AST.h"
#include "IRGen.h"

struct Config
{
	bool dontPromoteMemoryToRegisters;
	bool logAST;
	bool logIR;
	bool logAllocationInfo;
};

struct InterferenceGraph
{
	u32 count;
	u32 capacity;
	u32 *valueIndices;
	u8 *removed;
	DynamicArray<u32, PhaseAllocator> *edges; // @Improve: eugh
};

#define OUTPUT_BUFFER_BUCKET_SIZE 8192
struct TCJob;
struct Procedure;
struct TypeInfo;
struct StaticDefinition;
struct TCScope;
struct BasicBlock;
struct BEInstruction;
struct Context
{
	Config config;

	DynamicArray<SourceFile, HeapAllocator> sourceFiles;
	DynamicArray<String, HeapAllocator> libsToLink;

	// Parsing
	BucketArray<Token, HeapAllocator, 1024> tokens;
	u64 currentTokenIdx;
	Token *token;
	ASTRoot *astRoot;
	BucketArray<ASTExpression, HeapAllocator, 1024> treeNodes;
	BucketArray<ASTType, HeapAllocator, 1024> astTypeNodes;
	BucketArray<String, HeapAllocator, 1024> stringLiterals;

	// Type check
	DynamicArray<TCJob, PhaseAllocator> tcJobs;
	s32 currentTCJob;
	BucketArray<Value, HeapAllocator, 2048> values;
	BucketArray<Procedure, HeapAllocator, 512> procedures;
	BucketArray<Procedure, HeapAllocator, 128> externalProcedures;
	BucketArray<StaticDefinition, HeapAllocator, 512> staticDefinitions;
	BucketArray<const TypeInfo, HeapAllocator, 1024> typeTable;
	TCScope *tcGlobalScope;
	s64 tcCurrentReturnType;
	s64 tcCurrentForLoopArrayType;

	// IR
	DynamicArray<IRStaticVariable, HeapAllocator> irStaticVariables;
	DynamicArray<u32, HeapAllocator> irExternalVariables;
	DynamicArray<IRScope, PhaseAllocator> irStack;
	DynamicArray<IRProcedureScope, PhaseAllocator> irProcedureStack;
	BucketArray<IRLabel, HeapAllocator, 1024> irLabels;
	IRLabel *currentBreakLabel;
	IRLabel *currentContinueLabel;
	IRLabel *currentContinueSkipIncrementLabel;
	struct {
		IRValue arrayValue;
		IRValue indexValue;
	} irCurrentForLoopInfo;

	// Backend
	BucketArray<u8, PhaseAllocator, OUTPUT_BUFFER_BUCKET_SIZE> outputBuffer;
	BucketArray<BasicBlock, PhaseAllocator, 512> beBasicBlocks;
	DynamicArray<BasicBlock *, PhaseAllocator> beLeafBasicBlocks;
	InterferenceGraph beInterferenceGraph;
	BucketArray<BEInstruction, PhaseAllocator, 128> bePatchedInstructions;
};

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

	char *str = (char *)FrameAllocator::Alloc(5);
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
	for (u32 i = 0; i < loc.character; ++i)
	{
		if (sourceLine.data[i] == '\t')
			shift += 4;
		else
			++shift;
	}

	for (int i = 0; i < shift; ++i)
		Print(" ");
	for (u32 i = 0; i < loc.size; ++i)
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

bool CompilerAddSourceFile(Context *context, String filename, SourceLocation loc)
{
	FileHandle file = SYSOpenFileRead(filename);
	if (file == SYS_INVALID_FILE_HANDLE)
		LogError(context, loc,
				TPrintF("Included source file \"%S\" doesn't exist!", filename));

	for (int i = 0; i < context->sourceFiles.size; ++i)
	{
		String currentFilename = context->sourceFiles[i].name;
		FileHandle currentFile = SYSOpenFileRead(currentFilename);

		if (SYSAreSameFile(file, currentFile))
		{
			LogWarning(context, loc, TPrintF("File included twice: \"%S\"", filename));
			LogNote(context, context->sourceFiles[i].includeLoc, "First included here"_s);
			return false;
		}
	}

	SourceFile newSourceFile = { filename, loc };
	SYSReadEntireFile(file, &newSourceFile.buffer, &newSourceFile.size, FrameAllocator::Alloc);

	*DynamicArrayAdd(&context->sourceFiles) = newSourceFile;

	return true;
}

#include "Tokenizer.cpp"
#include "Parser.cpp"
#include "TypeChecker.cpp"
#include "PrintAST.cpp"
#include "IRGen.cpp"
#include "PrintIR.cpp"
#include "x64.cpp"

int main(int argc, char **argv)
{
#if USE_PROFILER_API
	PerformanceAPI_LoadFrom(L"external/Superluminal/PerformanceAPI.dll", &performanceAPI);
#endif

	SetUpTimers();

#if _MSC_VER
	g_hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	g_hStderr = GetStdHandle(STD_ERROR_HANDLE);
#else
	g_hStdout = fileno(stdout);
	g_hStderr = fileno(stderr);
#endif

	if (argc < 2)
		return 1;

	// Allocate memory
	Memory memory;
	g_memory = &memory;
	memory.frameMem = SYSAlloc(Memory::frameSize);
	memory.phaseMem = SYSAlloc(Memory::phaseSize);
	MemoryInit(&memory);

	Context context = {};

	DynamicArrayInit(&context.sourceFiles, 16);
	DynamicArrayInit(&context.libsToLink, 8);
	BucketArrayInit(&context.tokens);

	DynamicArray<String, FrameAllocator> inputFiles;
	DynamicArrayInit(&inputFiles, 16);
	*DynamicArrayAdd(&inputFiles) = "core/basic.emi"_s;
	*DynamicArrayAdd(&inputFiles) = "core/print.emi"_s;
#if _MSC_VER
	*DynamicArrayAdd(&inputFiles) = "core/basic_windows.emi"_s;
#else
	*DynamicArrayAdd(&inputFiles) = "core/basic_linux.emi"_s;
#endif
	for (int argIdx = 1; argIdx < argc; ++argIdx)
	{
		char *arg = argv[argIdx];
		if (arg[0] == '-')
		{
			if (strcmp("-noPromote", arg) == 0)
				context.config.dontPromoteMemoryToRegisters = true;
			else if (strcmp("-logAST", arg) == 0)
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
	ASSERT(inputFiles.size > 2);

	TimerSplit("Initialization"_s);

	for (int i = 0; i < inputFiles.size; ++i)
		CompilerAddSourceFile(&context, inputFiles[i], {});

	for (int i = 0; i < context.sourceFiles.size; ++i)
		TokenizeFile(&context, i);

	Token eofToken = { TOKEN_END_OF_FILE };
	*BucketArrayAdd(&context.tokens) = eofToken;

	TimerSplit("Tokenizer"_s);
	PhaseAllocator::Wipe();

	GenerateSyntaxTree(&context);

	if (context.config.logAST)
		PrintAST(&context);

	TimerSplit("Generating AST"_s);
	PhaseAllocator::Wipe();

	TypeCheckMain(&context);

	if (context.config.logAST)
		PrintAST(&context);

	TimerSplit("Type checking"_s);
	PhaseAllocator::Wipe();

	IRGenMain(&context);

	PrintIRInstructions(&context);

	TimerSplit("IR generation"_s);
	PhaseAllocator::Wipe();

	BackendMain(&context);

	Print("Compilation success\n");
	return 0;
}
