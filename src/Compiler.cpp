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
};

enum JobState : u32
{
	JOBSTATE_READY,
	JOBSTATE_UNKNOWN_IDENTIFIER,
	JOBSTATE_UNKNOWN_OVERLOAD,
	JOBSTATE_STATIC_DEF_NOT_READY,
	JOBSTATE_PROC_BODY_NOT_READY,
	JOBSTATE_TYPE_NOT_READY,
	// WAITING_FOR_STOP jobs want to wait until no jobs are running to make a decision.
	// As of time of write, only #defined does this to determine if something isn't defined anywhere
	// before continuing.
	JOBSTATE_WAITING_FOR_STOP,
	JOBSTATE_DONE
};

union JobYieldContext {
	String identifier;
	u32 index;
};

struct Job {
	Fiber fiber;

	// Some data about why the job yielded execution.
	JobYieldContext context;
};

#include "Config.h"
#include "Maths.h"
#include "Containers.h"

__declspec(thread) u32 t_threadIndex;
__declspec(thread) Fiber t_schedulerFiber;
__declspec(thread) Fiber t_previousFiber = SYS_INVALID_FIBER_HANDLE;
__declspec(thread) JobState t_previousYieldReason;
__declspec(thread) JobYieldContext t_previousYieldContext;

#if _MSC_VER
#include "PlatformWindows.cpp"
#else
#include "PlatformLinux.cpp"
#endif

#include "Multithreading.cpp"

Memory *g_memory;
FileHandle g_hStdout;
FileHandle g_hStderr;

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
	memset(threadData->threadMemPtr, 0x00, size + 1);
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
struct Context
{
	Config config;

	u32 tlsIndex;
	u32 flsIndex;

	Mutex consoleMutex;

	volatile u32 filesLock;
	DynamicArray<SourceFile, HeapAllocator> sourceFiles;
	DynamicArray<String, HeapAllocator> libsToLink;

	//MXContainer<BucketArray<Job, HeapAllocator, 1024>> jobs;

	Array<Fiber, HeapAllocator> readyJobs;
	volatile u32 readyQueueHead;
	volatile u32 readyQueueTail;
	volatile u32 readyQueueHeadLock;
	volatile u32 readyQueueTailLock;

	volatile s32 threadsDoingWork;

	SLContainer<DynamicArray<Job, HeapAllocator>> jobsWaitingForIdentifier;
	MXContainer<DynamicArray<Job, HeapAllocator>> jobsWaitingForOverload;
	MXContainer<DynamicArray<Job, HeapAllocator>> jobsWaitingForStaticDef;
	MXContainer<DynamicArray<Job, HeapAllocator>> jobsWaitingForProcedure;
	MXContainer<DynamicArray<Job, HeapAllocator>> jobsWaitingForType;
	MXContainer<DynamicArray<Job, HeapAllocator>> jobsWaitingForDeadStop;

	// Parsing -----

	// Type check -----
	Array<TCScopeName, HeapAllocator> tcPrimitiveTypes;

	RWContainer<BucketArray<Value, HeapAllocator, 1024>> globalValues;
	RWContainer<BucketArray<Procedure, HeapAllocator, 512>> procedures;
	RWContainer<BucketArray<Procedure, HeapAllocator, 128>> externalProcedures;
	RWContainer<DynamicArray<OperatorOverload, HeapAllocator>> operatorOverloads;
	RWContainer<BucketArray<StaticDefinition, HeapAllocator, 512>> staticDefinitions;

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
	BucketArray<Token, HeapAllocator, 1024> tokens;
	ASTRoot astRoot;
	BucketArray<ASTExpression, HeapAllocator, 1024> astTreeNodes;
	BucketArray<ASTType, HeapAllocator, 1024> astTypes;
};

struct TCJobData : JobDataCommon
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

	SYSSpinlockLock(&context->readyQueueTailLock);

	u32 queueIdx = context->readyQueueTail;
	context->readyJobs[queueIdx] = fiber;
	context->readyQueueTail = (context->readyQueueTail + 1) % context->readyJobs.size;

	SYSSpinlockUnlock(&context->readyQueueTailLock);

	return true;
}

struct ThreadArgs {
	Context *context;
	u32 threadIndex;
};

// Procedure to switch to a different job.
// We leave the information in thread local storage for the scheduler fiber.
// Call this when a job finishes too, the scheduler will delete the fiber and free resources.
void SwitchJob(Context *context, JobState yieldReason,
		JobYieldContext yieldContext) {
	t_previousFiber = GetCurrentFiber();
	t_previousYieldReason = yieldReason;
	t_previousYieldContext = yieldContext;
	SYSSwitchToFiber(t_schedulerFiber);
}

// Fiber that swaps jobs around.
// The reason to have a separate scheduler fiber is so we can queue the caller fiber right away,
// without risking another thread picking it up while it's still running on this one (since all this
// logic would be running in the fiber that wants to yield).
void SchedulerProc(Context *context) {
	while (true) {
		Fiber nextFiber = SYS_INVALID_FIBER_HANDLE;
		u32 queueIdx = U32_MAX;
		while (true) {
			SYSSpinlockLock(&context->readyQueueHeadLock);

			u32 head = context->readyQueueHead;
			u32 tail = context->readyQueueTail;
			if (head != tail) {
				queueIdx = context->readyQueueHead;
				context->readyQueueHead = (head + 1) % context->readyJobs.size;
			}

			SYSSpinlockUnlock(&context->readyQueueHeadLock);

			if (queueIdx == U32_MAX) {
				s32 threadsDoingWork = _InterlockedDecrement((LONG volatile *)&context->threadsDoingWork);
				if (threadsDoingWork == 0) {
#define WAKE_UP_ONE(_waitingJobs) \
					{ \
						auto jobsWaiting = context-> _waitingJobs .Get(); \
						if (jobsWaiting->size) { \
							Job *job = &(*jobsWaiting)[0]; \
							\
							SYSSpinlockLock(&context->readyQueueTailLock); \
							\
							context->readyJobs[context->readyQueueTail] = job->fiber; \
							context->readyQueueTail = (context->readyQueueTail + 1) % context->readyJobs.size; \
							\
							SYSSpinlockUnlock(&context->readyQueueTailLock); \
							\
							/* Remove */ \
							*job = (*jobsWaiting)[--jobsWaiting->size]; \
							\
							_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork); \
							continue; \
						} \
					}
					WAKE_UP_ONE(jobsWaitingForDeadStop)
					WAKE_UP_ONE(jobsWaitingForIdentifier)
					WAKE_UP_ONE(jobsWaitingForOverload)
					WAKE_UP_ONE(jobsWaitingForProcedure)
					WAKE_UP_ONE(jobsWaitingForStaticDef)
					WAKE_UP_ONE(jobsWaitingForType)
#undef WAKE_UP_ONE

					// Give up!
					return;
				}
				// @Improve: This is silly but shouldn't happen too often...
				Sleep(1);
				_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork);
			}
			else break;
		}

		nextFiber = context->readyJobs[queueIdx];
		context->readyJobs[queueIdx] = (Fiber)0xFEEEFEEEFEEEFEEE;

		SYSSwitchToFiber(nextFiber);

		// Queue previous job now that its fiber is not running
		ASSERT(t_previousFiber != (Fiber)0xDEADBEEFDEADBEEF);
		if (t_previousFiber != SYS_INVALID_FIBER_HANDLE) {
			Job job;
			job.fiber = t_previousFiber;
			job.context = t_previousYieldContext;
			switch (t_previousYieldReason) {
			case JOBSTATE_DONE:
			{
				DeleteFiber(t_previousFiber);
			} break;
			case JOBSTATE_WAITING_FOR_STOP:
			{
				auto jobs = context->jobsWaitingForDeadStop.Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case JOBSTATE_UNKNOWN_IDENTIFIER:
			{
				auto jobs = context->jobsWaitingForIdentifier.Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->tcGlobalNames.rwLock);
			} break;
			case JOBSTATE_UNKNOWN_OVERLOAD:
			{
				auto jobs = context->jobsWaitingForOverload.Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case JOBSTATE_PROC_BODY_NOT_READY:
			{
				auto jobs = context->jobsWaitingForProcedure.Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case JOBSTATE_STATIC_DEF_NOT_READY:
			{
				auto jobs = context->jobsWaitingForStaticDef.Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->staticDefinitions.rwLock);
			} break;
			case JOBSTATE_TYPE_NOT_READY:
			{
				auto jobs = context->jobsWaitingForType.Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->typeTable.rwLock);
			} break;
			default:
				ASSERTF(false, "Previous fiber is %llx, reason is %d", t_previousFiber,
						t_previousYieldReason);
			}
		}
	}
}

// Procedure where worker threads begin executing
int WorkerThreadProc(void *args)
{
	ThreadArgs *threadArgs = (ThreadArgs *)args;
	Context *context = threadArgs->context;

	t_threadIndex = threadArgs->threadIndex;

	ThreadDataCommon threadData = {};
	threadData.lastJobIdx = U32_MAX;
	SYSSetThreadData(context->tlsIndex, &threadData);
	MemoryInitThread(1 * 1024 * 1024);

	_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork);

	t_schedulerFiber = SYSConvertThreadToFiber();

	SchedulerProc(context);

	return 0;
}

#include "Tokenizer.cpp"
#include "PrintAST.cpp"
#include "Parser.cpp"
#include "TypeChecker.cpp"
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

	const int threadCount = 8;
	ThreadHandle threads[threadCount];
	ThreadArgs threadArgs[threadCount];
	for (int i = 0; i < threadCount; ++i) {
		threadArgs[i] = { &context, (u32)i };
		threads[i] = SYSCreateThread(WorkerThreadProc, &threadArgs[i]);
	}

	SYSWaitForThreads(threadCount, threads);

	TimerSplit("Multithreaded parse/analyze/codegen phase"_s);

	ASSERT(!TCIsAnyOtherJobRunning(&context));
	BackendGenerateOutputFile(&context);

	TimerSplit("Done"_s);
	Print("Compilation success\n");

	return 0;
}
