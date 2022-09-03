#if _MSC_VER
#include "PlatformWindows.h"
#endif
#include "General.h"
#include "Strings.h"
#include "MemoryAlloc.h"

#define USE_PROFILER_API 1

const String TPrintF(const char *format, ...);

#if _MSC_VER
#include "PlatformWindows.cpp"
#else
#include "PlatformLinux.cpp"
#endif

#define STB_SPRINTF_IMPLEMENTATION
#include "stb/stb_sprintf.h"

#include "Config.h"
#include "Maths.h"
#include "Containers.h"
#include "Multithreading.cpp"

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

	SYSMutexLock(g_memory->phaseMutex);

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

	SYSMutexUnlock(g_memory->phaseMutex);

	va_end(args);
	return size;
}

const String TPrintF(const char *format, ...)
{
	SYSMutexLock(g_memory->frameMutex);

	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);
	s64 size = stbsp_vsprintf(buffer, format, args);
	va_end(args);

	g_memory->framePtr = (u8 *)g_memory->framePtr + size + 1;

	SYSMutexUnlock(g_memory->frameMutex);

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
#include "TypeChecker.h"
#include "IRGen.h"
#include "Backend.h"
#include "x64.h"

struct Config
{
	bool dontPromoteMemoryToRegisters;
	bool dontCallAssembler;
	bool logAST;
	bool logIR;
	bool logAllocationInfo;
};

#define OUTPUT_BUFFER_BUCKET_SIZE 8192
struct Procedure;
struct TypeInfo;
struct OperatorOverload;
struct StaticDefinition;
struct TCScope;
struct Context
{
	Config config;

	u32 tlsIndex;

	volatile u32 filesLock;
	DynamicArray<SourceFile, HeapAllocator> sourceFiles;
	DynamicArray<String, HeapAllocator> libsToLink;

	// Parsing -----
	DynamicArray<HANDLE, HeapAllocator> parseThreads;
	RWContainer<DynamicArray<TCJobState, HeapAllocator>> parseJobStates;
	DynamicArray<BucketArray<Token, HeapAllocator, 1024>, HeapAllocator> fileTokens;
	DynamicArray<ASTRoot, HeapAllocator> fileASTRoots;
	DynamicArray<RWContainer<BucketArray<ASTExpression, HeapAllocator, 1024>>, HeapAllocator> fileTreeNodes;
	DynamicArray<RWContainer<BucketArray<ASTType, HeapAllocator, 1024>>, HeapAllocator> fileTypeNodes;

	// Type check -----
	RWContainer<DynamicArray<HANDLE, HeapAllocator>> tcThreads;
	RWContainer<DynamicArray<TCJobState, HeapAllocator>> tcJobStates;
	RWContainer<BucketArray<Value, HeapAllocator, 1024>> values;
	RWContainer<BucketArray<Procedure, HeapAllocator, 512>> procedures;
	RWContainer<BucketArray<Procedure, HeapAllocator, 128>> externalProcedures;
	RWContainer<DynamicArray<OperatorOverload, HeapAllocator>> operatorOverloads;
	RWContainer<BucketArray<StaticDefinition, HeapAllocator, 512>> staticDefinitions;

	/* Don't add types to the type table by hand without checking what AddType() does! */
	RWContainer<BucketArray<const TypeInfo, HeapAllocator, 1024>> typeTable;

	RWContainer<TCScope> tcGlobalScope;
	RWContainer<HashMap<String, CONDITION_VARIABLE, HeapAllocator>> tcConditionVariables;

	// IR -----
	RWContainer<DynamicArray<HANDLE, HeapAllocator>> irThreads;
	SRWLOCK proceduresLock;
	RWContainer<BucketArray<String, HeapAllocator, 1024>> stringLiterals;
	RWContainer<DynamicArray<IRStaticVariable, HeapAllocator>> irStaticVariables;
	RWContainer<DynamicArray<u32, HeapAllocator>> irExternalVariables;

	// Backend -----
	BucketArray<u8, PhaseAllocator, OUTPUT_BUFFER_BUCKET_SIZE> outputBuffer;
	RWContainer<DynamicArray<BEFinalProcedure, PhaseAllocator>> beFinalProcedureData;
};

struct ThreadData
{
	u32 fileIdx;
	u64 currentTokenIdx;
	Token *token;
};

struct IRThreadData
{
	s32 procedureIdx;
	BucketArray<IRInstruction, FrameAllocator, 256> irInstructions;
	DynamicArray<IRScope, PhaseAllocator> irStack;
	BucketArray<IRLabel, HeapAllocator, 1024> irLabels;
	IRLabel *returnLabel;
	IRLabel *currentBreakLabel;
	IRLabel *currentContinueLabel;
	IRLabel *currentContinueSkipIncrementLabel;
	struct {
		IRValue arrayValue;
		IRValue indexValue;
	} irCurrentForLoopInfo;
	u32 returnValueIdx;
	u32 shouldReturnValueIdx;
	BucketArray<Value, HeapAllocator, 1024> localValues;

	// Back end
	BucketArray<BEInstruction, PhaseAllocator, 1024> beInstructions;
	u64 stackSize;
	s64 allocatedParameterCount;
	DynamicArray<u32, PhaseAllocator> spilledValues;
	BucketArray<BasicBlock, PhaseAllocator, 512> beBasicBlocks;
	BasicBlock * beLeafBasicBlock;
	InterferenceGraph beInterferenceGraph;
	BucketArray<BEInstruction, PhaseAllocator, 128> bePatchedInstructions;
	Array<u64, PhaseAllocator> valueIsXmmBits;
	u32 x64SpilledParametersRead[32];
	u32 x64SpilledParametersWrite[32];
};

FatSourceLocation ExpandSourceLocation(Context *context, SourceLocation loc);
void __Log(Context *context, SourceLocation loc, String str, const char *inFile, const char *inFunc,
		int inLine)
{
	FatSourceLocation fatLoc = ExpandSourceLocation(context, loc);

	// Info
	SourceFile sourceFile;
	{
		ScopedLockSpin filesLock(&context->filesLock);
		sourceFile = context->sourceFiles[loc.fileIdx];
	}
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

#if DEBUG_BUILD
	Print("~~~ In %s - %s:%d\n", inFunc, inFile, inLine);
#endif
}

#define Log(context, loc, str) \
	do { __Log(context, loc, str, __FILE__, __func__, __LINE__); } while (0)

#define LogErrorNoCrash(context, loc, str) \
	do { __Log(context, loc, StringConcat("ERROR: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define LogError(context, loc, str) \
	do { LogErrorNoCrash(context, loc, str); PANIC; } while(0)

#define LogWarning(context, loc, str) \
	do { __Log(context, loc, StringConcat("WARNING: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define LogNote(context, loc, str) \
	do { __Log(context, loc, StringConcat("NOTE: "_s, str), __FILE__, __func__, __LINE__); } while (0)

bool CompilerAddSourceFile(Context *context, String filename, SourceLocation loc)
{
	FileHandle file = SYSOpenFileRead(filename);
	if (file == SYS_INVALID_FILE_HANDLE)
		LogError(context, loc,
				TPrintF("Included source file \"%S\" doesn't exist!", filename));

	ScopedLockSpin sourceFiles(&context->filesLock);
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

	ASTRoot *newRoot = DynamicArrayAdd(&context->fileASTRoots);
	DynamicArrayInit(&newRoot->block.statements, 4096);

	BucketArrayInit(DynamicArrayAdd(&context->fileTokens));

	RWContainer<BucketArray<ASTExpression, HeapAllocator, 1024>> newTreeNodes;
	BucketArrayInit(&newTreeNodes.content);
	*DynamicArrayAdd(&context->fileTreeNodes) = newTreeNodes;

	RWContainer<BucketArray<ASTType, HeapAllocator, 1024>> newTypeNodes;
	BucketArrayInit(&newTypeNodes.content);
	*DynamicArrayAdd(&context->fileTypeNodes) = newTypeNodes;

	auto parseJobStates = context->parseJobStates.GetForWrite();
	ParseJobArgs *args = ALLOC(PhaseAllocator::Alloc, ParseJobArgs);
	u32 jobIdx = (u32)parseJobStates->size;
	*args = { context, (u32)context->sourceFiles.size - 1, jobIdx };
	*DynamicArrayAdd(&parseJobStates) = TCJOBSTATE_RUNNING;
	*DynamicArrayAdd(&context->parseThreads) = (HANDLE)_beginthread(ParseJobProc, 0, (void *)args);

	return true;
}

#include "Tokenizer.cpp"
#include "Parser.cpp"
#include "TypeChecker.cpp"
#include "PrintAST.cpp"
#include "IRGen.cpp"
//#include "PrintIR.cpp" // @Fix
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

	// Allocate memory
	Memory memory;
	g_memory = &memory;
	memory.frameMem = SYSAlloc(Memory::frameSize);
	memory.phaseMem = SYSAlloc(Memory::phaseSize);
	MemoryInit(&memory);

	if (argc < 2)
	{
		Print("Usage: compiler [options] <source file>\n");
		return 1;
	}

	Context context = {};

	context.tlsIndex = TlsAlloc();
	ThreadData mainThreadData;
	TlsSetValue(context.tlsIndex, &mainThreadData);

	DynamicArrayInit(&context.sourceFiles, 16);
	DynamicArrayInit(&context.libsToLink, 8);

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

	ParserMain(&context);
	TypeCheckMain(&context);
	IRGenMain(&context);
	BackendMain(&context);

	for (int i = 0; i < inputFiles.size; ++i)
		CompilerAddSourceFile(&context, inputFiles[i], {});

	TimerSplit("Read input files"_s);

	for (int threadIdx = 0; threadIdx < context.parseThreads.size; ++threadIdx)
		WaitForSingleObject(context.parseThreads[threadIdx], INFINITE);

	// Unsafe reads!
	for (int threadIdx = 0; threadIdx < context.tcThreads.content.size; ++threadIdx)
		WaitForSingleObject(context.tcThreads.content[threadIdx], INFINITE);

	for (int threadIdx = 0; threadIdx < context.irThreads.content.size; ++threadIdx)
		WaitForSingleObject(context.irThreads.content[threadIdx], INFINITE);

	BackendGenerateOutputFile(&context);

	TimerSplit("Done"_s);
	Print("Compilation success\n");

	return 0;
}
