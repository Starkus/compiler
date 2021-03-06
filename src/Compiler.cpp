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
	bool dontCallAssembler;
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
struct OperatorOverload;
struct StaticDefinition;
struct TCScope;
struct BasicBlock;
struct BEInstruction;
struct Context
{
	Config config;

	DynamicArray<SourceFile, HeapAllocator> sourceFiles;
	DynamicArray<String, HeapAllocator> libsToLink;

	// Parsing -----
	BucketArray<Token, HeapAllocator, 1024> tokens;
	u64 currentTokenIdx;
	Token *token;
	ASTRoot *astRoot;
	BucketArray<ASTExpression, HeapAllocator, 1024> treeNodes;
	BucketArray<ASTType, HeapAllocator, 1024> astTypeNodes;
	BucketArray<String, HeapAllocator, 1024> stringLiterals;

	// Type check -----
	DynamicArray<TCJob, PhaseAllocator> tcJobs;
	s32 currentTCJob;
	BucketArray<Value, HeapAllocator, 2048> values;
	BucketArray<Procedure, HeapAllocator, 512> procedures;
	BucketArray<Procedure, HeapAllocator, 128> externalProcedures;
	DynamicArray<OperatorOverload, HeapAllocator> operatorOverloads;
	BucketArray<StaticDefinition, HeapAllocator, 512> staticDefinitions;

	/* Don't add types to the type table by hand without checking what AddType() does! */
	BucketArray<const TypeInfo, HeapAllocator, 1024> typeTable;

	TCScope *tcGlobalScope;
	u32 tcCurrentReturnType;
	u32 tcCurrentForLoopArrayType;

	// IR -----
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

	// Backend -----
	BucketArray<u8, PhaseAllocator, OUTPUT_BUFFER_BUCKET_SIZE> outputBuffer;
	BucketArray<BasicBlock, PhaseAllocator, 512> beBasicBlocks;
	DynamicArray<BasicBlock *, PhaseAllocator> beLeafBasicBlocks;
	InterferenceGraph beInterferenceGraph;
	BucketArray<BEInstruction, PhaseAllocator, 128> bePatchedInstructions;
};

FatSourceLocation ExpandSourceLocation(Context *context, SourceLocation loc);
void Log(Context *context, SourceLocation loc, String str)
{
	FatSourceLocation fatLoc = ExpandSourceLocation(context, loc);

	// Info
	SourceFile sourceFile = context->sourceFiles[loc.fileIdx];
	Print("%S %d:%d %S\n", sourceFile.name, fatLoc.line, fatLoc.character, str);

	// Source line
	Print("... %.*s\n... ", fatLoc.lineSize, fatLoc.beginingOfLine);

	// Token underline
	for (u32 i = 0; i < fatLoc.character; ++i)
	{
		if (fatLoc.beginingOfLine[i] == '\t')
			Print("\t");
		else
			Print(" ");
	}
	for (u32 i = 0; i < fatLoc.size; ++i)
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
			else if (strcmp("-noBuildExecutable", arg) == 0)
				context.config.dontCallAssembler = true;
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

	if (context.config.logIR)
		PrintIRInstructions(&context);

	TimerSplit("IR generation"_s);
	PhaseAllocator::Wipe();

	BackendMain(&context);

	Print("Compilation success\n");

	return 0;
}
