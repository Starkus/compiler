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

#define USE_PROFILER_API IS_WINDOWS

const String TPrintF(const char *format, ...);

#define STB_SPRINTF_IMPLEMENTATION
#include "stb/stb_sprintf.h"

#if USE_PROFILER_API
#include "Superluminal/PerformanceAPI_loader.h"
PerformanceAPI_Functions performanceAPI;
#endif
#include "Profiler.cpp"

#include "Config.h"
#include "Maths.h"
#include "Containers.h"

#if _MSC_VER
#include "PlatformWindows.cpp"
#else
#include "PlatformLinux.cpp"
#endif

#include "Multithreading.cpp"

Memory *g_memory;
FileHandle g_hStdout;
FileHandle g_hStderr;

struct ThreadDataCommon
{
	void *threadMem, *threadMemPtr;
	u64 threadMemSize;

	// This is a little silly, but we need to make jobs available to be picked up by another thread
	// AFTER we do the SwitchJob (fiber switch) call.
	u32 lastJobIdx;
};

struct JobDataCommon
{
	void *jobMem, *jobMemPtr;
	u64 jobMemSize;
	u32 jobIdx;
};

enum JobState : u32
{
	JOBSTATE_START,
	JOBSTATE_RUNNING,
	JOBSTATE_SLEEPING,
	// WAITING_FOR_STOP jobs want to wait until no jobs are running to make a decision.
	// As of time of write, only #defined does this to determine if something isn't defined anywhere
	// before continuing.
	JOBSTATE_WAITING_FOR_STOP,
	JOBSTATE_DONE,
};

struct Job
{
	Fiber fiber;
	u32 isRunning;
	JobState state;
};

s64 Print(const char *format, ...)
{
	// Log file
	static FileHandle logFileHandle = SYSOpenFileWrite("output/log.txt"_s);

	ThreadDataCommon *threadData = (ThreadDataCommon *)SYSGetThreadData(g_memory->tlsIndex);

	char *buffer = (char *)threadData->threadMemPtr;

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
	memset(threadData->threadMemPtr, 0x55, size + 1);
#endif

	va_end(args);
	return size;
}

const String TPrintF(const char *format, ...)
{
	ThreadDataCommon *threadData = (ThreadDataCommon *)SYSGetThreadData(g_memory->tlsIndex);

	char *buffer = (char *)threadData->threadMemPtr;

	va_list args;
	va_start(args, format);
	s64 size = stbsp_vsprintf(buffer, format, args);
	va_end(args);

	threadData->threadMemPtr = (u8 *)threadData->threadMemPtr + size + 1;

	return { size, buffer };
}

const String SNPrintF(const char *format, int maxSize, ...)
{
	char *buffer = (char *)LinearAllocator::Alloc(maxSize, 1);

	va_list args;
	va_start(args, maxSize);
	s64 size = stbsp_vsnprintf(buffer, maxSize, format, args);
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
struct TCScopeName;
struct Context
{
	Config config;

	u32 tlsIndex;
	u32 flsIndex;

	Mutex consoleMutex;

	volatile u32 filesLock;
	DynamicArray<SourceFile, HeapAllocator> sourceFiles;
	DynamicArray<String, HeapAllocator> libsToLink;

	MXContainer<DynamicArray<Job, HeapAllocator>> jobs;

	// Parsing -----
	DynamicArray<BucketArray<Token, HeapAllocator, 1024>, HeapAllocator> fileTokens;
	DynamicArray<ASTRoot, HeapAllocator> fileASTRoots;
	DynamicArray<RWContainer<BucketArray<ASTExpression, HeapAllocator, 1024>>, HeapAllocator> fileTreeNodes;
	DynamicArray<RWContainer<BucketArray<ASTType, HeapAllocator, 1024>>, HeapAllocator> fileTypeNodes;

	// Type check -----
	Array<TCScopeName, HeapAllocator> tcPrimitiveTypes;

	RWContainer<BucketArray<Value, HeapAllocator, 1024>> globalValues;
	RWContainer<BucketArray<Procedure, HeapAllocator, 512>> procedures;
	RWContainer<BucketArray<Procedure, HeapAllocator, 128>> externalProcedures;
	RWContainer<DynamicArray<OperatorOverload, HeapAllocator>> operatorOverloads;
	RWContainer<BucketArray<StaticDefinition, HeapAllocator, 512>> staticDefinitions;
	ConditionVariable operatorOverloadsConditionVariable;

	/* Don't add types to the type table by hand without checking what AddType() does! */
	RWContainer<BucketArray<const TypeInfo, HeapAllocator, 1024>> typeTable;

	RWContainer<DynamicArray<TCScopeName, LinearAllocator>> tcGlobalNames;
	RWContainer<DynamicArray<u32, LinearAllocator>> tcGlobalTypeIndices;

	// IR -----
	RWContainer<BucketArray<String, HeapAllocator, 1024>> stringLiterals;
	RWContainer<DynamicArray<IRStaticVariable, HeapAllocator>> irStaticVariables;
	RWContainer<DynamicArray<u32, HeapAllocator>> irExternalVariables;

	// Backend -----
	BucketArray<u8, HeapAllocator, OUTPUT_BUFFER_BUCKET_SIZE> outputBuffer;
	RWContainer<DynamicArray<BEFinalProcedure, HeapAllocator>> beFinalProcedureData;
};

struct ParseJobData : JobDataCommon
{
	u32 fileIdx;
	u64 currentTokenIdx;
	Token *token;
};

struct TCJobData : ParseJobData
{
	ASTExpression *expression;
	bool onStaticContext;
	DynamicArray<TCScope, JobAllocator> scopeStack;
	ArrayView<u32> currentReturnTypes;
	u32 currentForLoopArrayType;
	BucketArray<Value, HeapAllocator, 1024> localValues;
};

struct IRJobData : JobDataCommon
{
	u32 procedureIdx;
	BucketArray<IRInstruction, LinearAllocator, 256> irInstructions;
	DynamicArray<IRScope, JobAllocator> irStack;
	BucketArray<IRLabel, HeapAllocator, 1024> irLabels;
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
	BucketArray<Value, HeapAllocator, 1024> localValues;

	// Back end
	BucketArray<BEInstruction, HeapAllocator, 1024> beInstructions;
	u64 stackSize;
	s64 allocatedParameterCount;
	DynamicArray<u32, JobAllocator> spilledValues;
	BucketArray<BasicBlock, JobAllocator, 512> beBasicBlocks;
	BasicBlock * beLeafBasicBlock;
	InterferenceGraph beInterferenceGraph;
	BucketArray<BEInstruction, HeapAllocator, 128> bePatchedInstructions;
	Array<u64, JobAllocator> valueIsXmmBits;
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
		// @Speed: shouldn't need this lock
		ScopedLockSpin filesLock(&context->filesLock);
		sourceFile = context->sourceFiles[loc.fileIdx];
	}

	ScopedLockMutex lock(context->consoleMutex);

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
	do { __Log(context, loc, TStringConcat("ERROR: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define LogError(context, loc, str) \
	do { LogErrorNoCrash(context, loc, str); PANIC; } while(0)

#define LogWarning(context, loc, str) \
	do { __Log(context, loc, TStringConcat("WARNING: "_s, str), __FILE__, __func__, __LINE__); } while (0)

#define LogNote(context, loc, str) \
	do { __Log(context, loc, TStringConcat("NOTE: "_s, str), __FILE__, __func__, __LINE__); } while (0)

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
	SYSReadEntireFile(file, &newSourceFile.buffer, &newSourceFile.size, LinearAllocator::Alloc);

	*DynamicArrayAdd(&context->sourceFiles) = newSourceFile;

	ASTRoot *newRoot = DynamicArrayAdd(&context->fileASTRoots);
	DynamicArrayInit(&newRoot->block.statements, 4096);

	BucketArrayInit(DynamicArrayAdd(&context->fileTokens));

	RWContainer<BucketArray<ASTExpression, HeapAllocator, 1024>> newTreeNodes;
	BucketArrayInit(&newTreeNodes.unsafe);
	*DynamicArrayAdd(&context->fileTreeNodes) = newTreeNodes;

	RWContainer<BucketArray<ASTType, HeapAllocator, 1024>> newTypeNodes;
	BucketArrayInit(&newTypeNodes.unsafe);
	*DynamicArrayAdd(&context->fileTypeNodes) = newTypeNodes;

	{
		auto jobs = context->jobs.Get();

		u32 jobIdx = (u32)jobs->size;
		ParseJobArgs *args = ALLOC(LinearAllocator, ParseJobArgs);
		*args = {
			.context = context,
			.fileIdx = (u32)context->sourceFiles.size - 1,
			.jobIdx = jobIdx };
		Fiber fiber = SYSCreateFiber(ParseJobProc, (void *)args);

		Job newJob = { fiber, 0, JOBSTATE_START };
		*DynamicArrayAdd(&jobs) = newJob;
	}

	return true;
}

int WorkerThreadProc(void *arg)
{
	Context *context = (Context *)arg;

	ThreadDataCommon threadData = {};
	threadData.lastJobIdx = U32_MAX;
	SYSSetThreadData(context->tlsIndex, &threadData);
	MemoryInitThread(1 * 1024 * 1024);

	SYSConvertThreadToFiber();

	Fiber fiber;
	{
		auto jobs = context->jobs.Get();
		u32 jobCount = (u32)jobs->size;
		for (u32 i = 0; i < jobCount; ++i)
		{
			if ((*jobs)[i].state != JOBSTATE_DONE && !(*jobs)[i].isRunning)
			{
				(*jobs)[i].isRunning = true;
				fiber = (*jobs)[i].fiber;
				goto newJob;
			}
		}
		return 0;
	}

newJob:
	SYSSwitchToFiber(fiber);
	return 0;
}

void SwitchJob(Context *context)
{
	ThreadDataCommon *threadData = (ThreadDataCommon *)SYSGetThreadData(context->tlsIndex);
	if (threadData->lastJobIdx != U32_MAX)
	{
		auto jobs = context->jobs.Get();
		(*jobs)[threadData->lastJobIdx].isRunning = 0;
	}

	JobDataCommon *jobData = (JobDataCommon *)SYSGetFiberData(context->flsIndex);
	Fiber nextFiber = SYS_INVALID_FIBER_HANDLE;
	{
		ProfileScope scope("Switch job", nullptr, PROFILER_COLOR(0x10, 0x10, 0xA0));

		auto jobs = context->jobs.Get();
		u32 jobCount = (u32)jobs->size;
		u32 startJobIdx = jobData->jobIdx + 1;
		if (startJobIdx >= jobCount)
			startJobIdx -= jobCount;
		u32 currentJobIdx = startJobIdx;
		for (u32 i = 0; i < jobCount; ++i)
		{
			Job job = (*jobs)[currentJobIdx];
			if (job.state != JOBSTATE_DONE && !job.isRunning)
			{
				(*jobs)[currentJobIdx].isRunning = 1;
				nextFiber = (*jobs)[currentJobIdx].fiber;
				goto newJob;
			}
			++currentJobIdx;
			if (currentJobIdx >= jobCount)
				currentJobIdx -= jobCount;
		}
		return;
	}
newJob:
	threadData->lastJobIdx = jobData->jobIdx;
	SYSSwitchToFiber(nextFiber);
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

	ThreadDataCommon threadData = {};
	SYSSetThreadData(context.tlsIndex, &threadData);
	MemoryInitThread(1 * 1024 * 1024);

	if (argc < 2)
	{
		Print("Usage: compiler [options] <source file>\n");
		return 1;
	}

	DynamicArrayInit(&context.sourceFiles, 16);
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

	ThreadHandle threads[8];
	for (int i = 0; i < 8; ++i)
		threads[i] = SYSCreateThread(WorkerThreadProc, &context);

	SYSWaitForThreads(ArrayCount(threads), threads);

	TimerSplit("Multithreaded parse/analyze/codegen phase"_s);

	BackendGenerateOutputFile(&context);

	TimerSplit("Done"_s);
	Print("Compilation success\n");

	return 0;
}
