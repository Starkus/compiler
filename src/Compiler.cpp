#include <stdint.h>
typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float f32;
typedef double f64;

#if _MSC_VER
#include "PlatformWindows.h"
#else
#include "PlatformLinux.h"
#endif

#include "General.h"
#include "Strings.h"
#include "MemoryAlloc.h"

#define STB_SPRINTF_IMPLEMENTATION
#include "stb/stb_sprintf.h"

#if USE_PROFILER_API
#include "Superluminal/PerformanceAPI_loader.h"
PerformanceAPI_Functions performanceAPI;
#endif
#include "Profiler.cpp"

// To properly turn this off, we'd need to make sure we don't leak locks anywhere we call LogError.
#define EXIT_ON_FIRST_ERROR 1

enum TCYieldReason : u32
{
	// Reasons without an associated list of waiting jobs are negative
	TCYIELDREASON_DONE	 = (u32)-3,
	TCYIELDREASON_FAILED = (u32)-2,
	TCYIELDREASON_READY	 = (u32)-1,

	TCYIELDREASON_UNKNOWN_IDENTIFIER = 0,
	TCYIELDREASON_UNKNOWN_OVERLOAD,
	TCYIELDREASON_STATIC_DEF_NOT_READY,
	TCYIELDREASON_PROC_BODY_NOT_READY,
	TCYIELDREASON_PROC_IR_NOT_READY,
	TCYIELDREASON_TYPE_NOT_READY,
	TCYIELDREASON_GLOBAL_VALUE_NOT_READY,
	// WAITING_FOR_STOP jobs want to wait until no jobs are running to make a decision.
	// As of time of write, only #defined does this to determine if something isn't defined anywhere
	// before continuing.
	TCYIELDREASON_WAITING_FOR_STOP,

	TCYIELDREASON_Count
};

union TCYieldContext
{
	String identifier;
	u32 index;
};

struct TCJob
{
	Fiber fiber;

	// Some data about why the job yielded execution.
	TCYieldContext context;
};

#include "Config.h"
#include "Maths.h"
#include "Multithreading.h"
#include "Containers.h"

THREADLOCAL u32 t_threadIndex;
THREADLOCAL void *t_threadMem, *t_threadMemPtr;
THREADLOCAL u64 t_threadMemSize;

String TPrintF(const char *format, ...);

#if _MSC_VER
#include "PlatformWindows.cpp"
#else
#include "PlatformLinux.cpp"
#endif

#include "Multithreading.cpp"

Memory *g_memory;
FileHandle g_hStdout;
FileHandle g_hStderr;
u32 g_numberOfThreads;
u32 g_threadIdxCreateFibers;
u32 g_threadIdxDeleteFibers;

s64 Print(const char *format, ...)
{
	// Log file
	static FileHandle logFileHandle = SYSOpenFileWrite("output/log.txt"_s);

	char *buffer = (char *)t_threadMemPtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);
#if IS_WINDOWS
	OutputDebugStringA(buffer);
#endif

	// Stdout
	SYSWriteFile(g_hStdout, buffer, strlen(buffer));

	// Log file
	SYSWriteFile(logFileHandle, buffer, strlen(buffer));

#if DEBUG_BUILD
	memset(t_threadMemPtr, 0x00, size + 1);
#endif

	va_end(args);
	return size;
}

String TPrintF(const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;

	va_list args;
	va_start(args, format);
	u64 size = stbsp_vsprintf(buffer, format, args);
	va_end(args);

	t_threadMemPtr = (u8 *)t_threadMemPtr + size + 1;

	return { size, buffer };
}

String SNPrintF(int maxSize, const char *format, ...)
{
	char *buffer = (char *)LinearAllocator::Alloc(maxSize, 1);

	va_list args;
	va_start(args, format);
	u64 size = stbsp_vsnprintf(buffer, maxSize, format, args);
	va_end(args);

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
#include "CompileTime.h"
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
#define STATIC_DATA_VIRTUAL_ADDRESS ((u8 *)0x0000044000000000)
#define STATIC_DATA_VIRTUAL_ADDRESS_END ((u8 *)0x0000045000000000)
struct Context
{
	Config config;

	u32 tlsIndex;
	u32 flsIndex;

	Mutex consoleMutex;

	volatile u32 filesLock;
	DynamicArray<SourceFile, HeapAllocator> sourceFiles;
	DynamicArray<String, HeapAllocator> libsToLink;

	MTQueue<Fiber> readyJobs;

	volatile s32 threadsDoingWork;
	volatile s32 failedJobsCount;

	volatile u32 staticDataLock;
	u64 staticDataSize;
	u64 staticDataAllocatedSpace;
	// We keep track of the types of things stored in static data in order to know what memory is
	// pointers to relocate them.
	DynamicArray<void *, HeapAllocator> staticDataPointersToRelocate;

	volatile u32 globalValuesLock;
	HashMap<u32, void *, LinearAllocator> globalValueContents;

	// Type check -----
	FixedArray<MXContainer<DynamicArray<TCJob, HeapAllocator>>, TCYIELDREASON_Count>
		waitingJobsByReason;

	MTQueue<JobRequest> jobsToCreate;
	MTQueue<Fiber> fibersToDelete;

	Array<TCScopeName, HeapAllocator> tcPrimitiveTypes;

	RWContainer<BucketArray<Value, HeapAllocator, 1024>> globalValues;
	RWContainer<BucketArray<Procedure, HeapAllocator, 512>> procedures;
	RWContainer<BucketArray<Procedure, HeapAllocator, 128>> externalProcedures;
	RWContainer<DynamicArray<OperatorOverload, HeapAllocator>> operatorOverloads;
	RWContainer<BucketArray<StaticDefinition, HeapAllocator, 512>> staticDefinitions;

	/* Don't add types to the type table by hand without checking what AddType() does! */
	SLContainer<BucketArray<const TypeInfo, HeapAllocator, 1024>> typeTable; // Lock only to add

	RWContainer<DynamicArray<TCScopeName, LinearAllocator>> tcGlobalNames;
	RWContainer<DynamicArray<u32, LinearAllocator>> tcGlobalTypeIndices;
	RWContainer<DynamicArray<DynamicArray<u32, LinearAllocator>, LinearAllocator>> tcInlineCalls;

	// IR -----
	RWContainer<BucketArray<StringLiteral, HeapAllocator, 1024>> stringLiterals;
	RWContainer<BucketArray<StringLiteral, HeapAllocator, 128>> cStringLiterals;
	RWContainer<BucketArray<FloatLiteral,  HeapAllocator, 1024>> f32Literals;
	RWContainer<BucketArray<FloatLiteral,  HeapAllocator, 1024>> f64Literals;
	RWContainer<DynamicArray<IRStaticVariable, HeapAllocator>> irStaticVariables;
	RWContainer<DynamicArray<u32, HeapAllocator>> irExternalVariables;

	// Backend -----
	BucketArray<u8, HeapAllocator, OUTPUT_BUFFER_BUCKET_SIZE> outputBuffer;
	RWContainer<DynamicArray<BEFinalProcedure, HeapAllocator>> beFinalProcedureData;
};

struct ParseJobData
{
	u32 fileIdx;
	u64 currentTokenIdx;
	Token *token;
	BucketArray<Token, HeapAllocator, 1024> tokens;
	ASTRoot astRoot;
	BucketArray<ASTExpression, HeapAllocator, 1024> astTreeNodes;
	BucketArray<ASTType, HeapAllocator, 1024> astTypes;
};

struct TCJobData
{
	ASTExpression *expression;
	bool onStaticContext;
	u32 currentProcedureIdx;
	DynamicArray<TCScope, ThreadAllocator> scopeStack;
	ArrayView<u32> currentReturnTypes;
	u32 currentForLoopArrayType;
	BucketArray<Value, LinearAllocator, 256> localValues;
};

struct IRJobData
{
	u32 procedureIdx;
	BucketArray<Value, LinearAllocator, 256> *localValues;
	BucketArray<IRInstruction, LinearAllocator, 256> *irInstructions;
	BucketArray<IRLabel, LinearAllocator, 256> irLabels;
	DynamicArray<IRScope, ThreadAllocator> irStack;
	IRLabel *returnLabel;
	IRLabel *currentBreakLabel;
	IRLabel *currentContinueLabel;
	IRLabel *currentContinueSkipIncrementLabel;
	struct {
		IRValue arrayValue;
		IRValue indexValue;
	} irCurrentForLoopInfo;
	ArrayView<u32> returnValueIndices;
	u32 shouldReturnValueIdx;

	// Back end
	BucketArray<BEInstruction, LinearAllocator, 1024> beInstructions;
	u64 stackSize;
	s64 allocatedParameterCount;
	DynamicArray<u32, ThreadAllocator> spilledValues;
	BucketArray<BasicBlock, ThreadAllocator, 512> beBasicBlocks;
	BasicBlock *beLeafBasicBlock;
	InterferenceGraph beInterferenceGraph;
	BucketArray<BEInstruction, LinearAllocator, 128> bePatchedInstructions;
	Array<u64, ThreadAllocator> valueIsXmmBits;
	u32 x64SpilledParametersRead[32];
	u32 x64SpilledParametersWrite[32];
};

FatSourceLocation ExpandSourceLocation(Context *context, SourceLocation loc);
void __Log(Context *context, SourceLocation loc, String str,
			const char *inFile, const char *inFunc, int inLine)
{
	if (loc.fileIdx != 0) {
		FatSourceLocation fatLoc = ExpandSourceLocation(context, loc);
		String filename = context->sourceFiles[loc.fileIdx].name;

		ScopedLockMutex lock(context->consoleMutex);

		// Info
		Print("%S %d:%d %S\n", filename, fatLoc.line, fatLoc.character, str);

		// Source line
		Print("... %.*s\n... ", fatLoc.lineSize, fatLoc.beginingOfLine);

		// Token underline
		for (u32 i = 0; i < fatLoc.character - 1; ++i)
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
	else {
		Print("[unknown source location] %S\n", str);
	}

#if DEBUG_BUILD
	Print("~~~ In %s - %s:%d\n", inFunc, inFile, inLine);
#endif
}

void __LogRange(Context *context, SourceLocation locBegin, SourceLocation locEnd, String str,
			const char *inFile, const char *inFunc, int inLine)
{
	FatSourceLocation fatLocBegin = ExpandSourceLocation(context, locBegin);
	FatSourceLocation fatLocEnd   = ExpandSourceLocation(context, locEnd);
	String filename = context->sourceFiles[locBegin.fileIdx].name;

	ScopedLockMutex lock(context->consoleMutex);

	// Info
	Print("%S %d:%d %S\n", filename, fatLocBegin.line, fatLocBegin.character, str);

	// Source line
	Print("... %.*s\n... ", fatLocBegin.lineSize, fatLocBegin.beginingOfLine);

	// Token underline
	u32 underlineCount = fatLocEnd.character - fatLocBegin.character + fatLocEnd.size;
	for (u32 i = 0; i < fatLocBegin.character - 1; ++i)
	{
		if (fatLocBegin.beginingOfLine[i] == '\t')
			Print("\t");
		else
			Print(" ");
	}
	for (u32 i = 0; i < underlineCount; ++i) {
		char c = fatLocBegin.beginingOfLine[fatLocBegin.character + i];
		if (c == '\n' || c == '\r')
			break;
		Print("^");
	}
	Print("\n");

#if DEBUG_BUILD
	Print("~~~ In %s - %s:%d\n", inFunc, inFile, inLine);
#endif
}

NOINLINE void SwitchJob(Context *context, TCYieldReason yieldReason, TCYieldContext yieldContext);

#define Log(context, loc, str) \
	do { __Log(context, loc, str, __FILE__, __func__, __LINE__); } while (0)

#define LogErrorNoCrash(context, loc, str) \
	do { __Log(context, loc, TStringConcat("ERROR: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define LogWarning(context, loc, str) \
	do { __Log(context, loc, TStringConcat("WARNING: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define LogNote(context, loc, str) \
	do { __Log(context, loc, TStringConcat("NOTE: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define Log2(context, locBegin, locEnd, str) \
	do { __LogRange(context, locBegin, locEnd, str, __FILE__, __func__, __LINE__); } while (0)

#define Log2ErrorNoCrash(context, locBegin, locEnd, str) \
	do { __LogRange(context, locBegin, locEnd, TStringConcat("ERROR: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define Log2Warning(context, locBegin, locEnd, str) \
	do { __LogRange(context, locBegin, locEnd, TStringConcat("WARNING: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define Log2Note(context, locBegin, locEnd, str) \
	do { __LogRange(context, locBegin, locEnd, TStringConcat("NOTE: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#if EXIT_ON_FIRST_ERROR
#define LogError(context, loc, str) \
	do { LogErrorNoCrash(context, loc, str); PANIC; } while(0)
#define Log2Error(context, locBegin, locEnd, str) \
	do { Log2ErrorNoCrash(context, locBegin, locEnd, str); SwitchJob(context, TCYIELDREASON_FAILED, {}); } while(0)
#else
#define LogError(context, loc, str) \
	do { LogErrorNoCrash(context, loc, str); PANIC; } while(0)
#define Log2Error(context, locBegin, locEnd, str) \
	do { Log2ErrorNoCrash(context, locBegin, locEnd, str); SwitchJob(context, TCYIELDREASON_FAILED, {}); } while(0)
#endif

void EnqueueReadyJob(Context *context, Fiber fiber);
bool CompilerAddSourceFile(Context *context, String filename, SourceLocation loc) {
	FileHandle file = SYSOpenFileRead(filename);
	if (file == SYS_INVALID_FILE_HANDLE)
		LogError(context, loc,
				TPrintF("Included source file \"%S\" doesn't exist!", filename));

	for (int i = 0; i < context->sourceFiles.size; ++i) {
		String currentFilename = context->sourceFiles[i].name;
		FileHandle currentFile = SYSOpenFileRead(currentFilename);

		if (SYSAreSameFile(file, currentFile)) {
			// Don't think we should to warn this...
			//LogWarning(context, loc, TPrintF("File included twice: \"%S\"", filename));
			//LogNote(context, context->sourceFiles[i].includeLoc, "First included here"_s);
			return false;
		}
	}

	SourceFile newSourceFile = { filename, loc };
	SYSReadEntireFile(file, &newSourceFile.buffer, &newSourceFile.size, LinearAllocator::Alloc);

	u32 fileIdx;
	{
		ScopedLockSpin sourceFilesLock(&context->filesLock);
		fileIdx = (u32)context->sourceFiles.size;
		*DynamicArrayAdd(&context->sourceFiles) = newSourceFile;
	}

	ParseJobArgs *args = ALLOC(LinearAllocator, ParseJobArgs);
	*args = {
		.context = context,
		.fileIdx = fileIdx };
	Fiber fiber = SYSCreateFiber(ParseJobProc, (void *)args);

	EnqueueReadyJob(context, fiber);

	return true;
}

struct ThreadArgs
{
	Context *context;
	u32 threadIndex;
};

#include "Scheduler.cpp"
#include "Tokenizer.cpp"
#include "PrintAST.cpp"
#include "Parser.cpp"
#include "TypeChecker.cpp"
#include "CompileTime.cpp"
#include "IRGen.cpp"
#include "PrintIR.cpp"
#include "x64.cpp"

int main(int argc, char **argv)
{
	SetUpTimers();

#if IS_WINDOWS
	g_hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	g_hStderr = GetStdHandle(STD_ERROR_HANDLE);
#else
	g_hStdout = fileno(stdout);
	g_hStderr = fileno(stderr);
#endif

	Context context = {};
	context.tlsIndex = SYSAllocThreadData();
	context.flsIndex = SYSAllocFiberData();
	context.consoleMutex = SYSCreateMutex();

	// Allocate memory
	Memory memory;
	g_memory = &memory;
	memory.tlsIndex = context.tlsIndex;
	memory.flsIndex = context.flsIndex;
	memory.linearMem = SYSAlloc(Memory::linearMemSize);
	MemoryInit(&memory);

	MemoryInitThread(128 * 1024 * 1024);

	if (argc < 2) {
		Print("Usage: compiler [options] <source file>\n");
		return 1;
	}

#if USE_PROFILER_API
	PerformanceAPI_LoadFrom(L"external/Superluminal/PerformanceAPI.dll", &performanceAPI);
	if (performanceAPI.BeginEvent == nullptr) {
		Print("ERROR! Couldn't load profiler API DLL!\n");
		return 1;
	}
#endif

	DynamicArrayInit(&context.sourceFiles, 16);
	++context.sourceFiles.size; // 0 is invalid file
	DynamicArrayInit(&context.libsToLink, 8);

	DynamicArray<String, LinearAllocator> inputFiles;
	DynamicArrayInit(&inputFiles, 16);
	*DynamicArrayAdd(&inputFiles) = "core/basic.emi"_s;
	*DynamicArrayAdd(&inputFiles) = "core/print.emi"_s;
#if IS_WINDOWS
	*DynamicArrayAdd(&inputFiles) = "core/basic_windows.emi"_s;
#else
	*DynamicArrayAdd(&inputFiles) = "core/basic_linux.emi"_s;
#endif
	for (int argIdx = 1; argIdx < argc; ++argIdx) {
		char *arg = argv[argIdx];
		if (arg[0] == '-') {
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
			*DynamicArrayAdd(&inputFiles) = CStrToString(arg);
	}
	ASSERT(inputFiles.size > 2);

	TimerSplit("Initialization"_s);

	ParserMain(&context);
	TypeCheckMain(&context);
	IRGenMain(&context);
	BackendMain(&context);

	for (int i = 0; i < inputFiles.size; ++i)
		CompilerAddSourceFile(&context, inputFiles[i], {});

	TimerSplit("Create starting jobs"_s);

	SYSTEM_INFO win32SystemInfo;
	GetSystemInfo(&win32SystemInfo);
	int threadCount = win32SystemInfo.dwNumberOfProcessors;
	g_numberOfThreads = threadCount;
	g_threadIdxCreateFibers = 0;
	g_threadIdxDeleteFibers = Min(threadCount, 1);

	Array<ThreadHandle, LinearAllocator> threads;
	Array<ThreadArgs,   LinearAllocator> threadArgs;
	ArrayInit(&threads,    threadCount);
	ArrayInit(&threadArgs, threadCount);
	threads.size    = threadCount;
	threadArgs.size = threadCount;
	for (int i = 0; i < threadCount; ++i) {
		threadArgs[i] = { &context, (u32)i };
		threads[i] = SYSCreateThread(WorkerThreadProc, &threadArgs[i]);
	}

	SYSWaitForThreads(threadCount, threads.data);

	TimerSplit("Multithreaded parse/analyze/codegen phase"_s);

	s32 failedJobsCount = context.failedJobsCount;
	if (failedJobsCount > 0) {
		if (failedJobsCount == 1)
			Print("A job failed. Aborting.\n");
		else
			Print("%d jobs failed. Aborting.\n", failedJobsCount);
		return 1;
	}

	ASSERT(!TCIsAnyOtherJobRunning(&context));
	BackendGenerateOutputFile(&context);

	TimerSplit("Done"_s);
	Print("Compilation success\n");

	return 0;
}
