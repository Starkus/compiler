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

#include "Config.h"

#if ENABLE_STATS
struct {
	volatile s32 jobSwitches;
	volatile s32 dynamicArrayReallocs;
	volatile s32 threadAllocReallocWaste;
	volatile s32 linearAllocReallocWaste;
	volatile s32 hashSetMapHit0;
	volatile s32 hashSetMapHit1;
	volatile s32 hashSetMapHit2;
	volatile s32 hashSetMapHit3;
	volatile s32 hashSetMapHitMore;
	volatile s32 hashSetMapRehashes;
	volatile s32 coalescingSuccesses;
	volatile s32 movsRemoved;
} g_stats = {};
#endif

#if IS_WINDOWS
#include "PlatformWindows.h"
#elif IS_LINUX
#include "PlatformLinux.h"
#else
#error "Unsupported operating system"
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

#include "Maths.h"
#include "Multithreading.h"
#include "Containers.h"

// Keep in sync with core/core.emi
enum OutputType
{
	OUTPUTTYPE_EXECUTABLE = 0,
	OUTPUTTYPE_LIBRARY_STATIC = 1,
	OUTPUTTYPE_LIBRARY_DYNAMIC = 2,
	OUTPUTTYPE_Count
};

THREADLOCAL u32 t_threadIndex;
THREADLOCAL void *t_threadMem, *t_threadMemPtr;
THREADLOCAL u64 t_threadMemSize;

String TPrintF(const char *format, ...);
struct Context;

Context *g_context;
Memory *g_memory;
FileHandle g_hStdin;
FileHandle g_hStdout;
FileHandle g_hStderr;
FileHandle g_logFileHandle;
Array<ThreadHandle, LinearAllocator> g_threads;
Array<Fiber, LinearAllocator> g_mainFibers;

#if _MSC_VER
#include "PlatformWindows.cpp"
#else
#include "PlatformLinux.cpp"
#endif

#include "Multithreading.cpp"

// Mainly intended to print console escape sequences
inline void PrintNoLog(String str)
{
	SYSWriteFile(g_hStdout, (void *)str.data, str.size);
}

s64 PrintString(String str)
{
#if IS_WINDOWS
	char *buffer = (char *)alloca(str.size + 1);
	memcpy(buffer, str.data, str.size);
	buffer[str.size] = 0;
	OutputDebugStringA(buffer);
#endif

	// Stdout
	SYSWriteFile(g_hStdout,       (void *)str.data, str.size);

	// Log file
	SYSWriteFile(g_logFileHandle, (void *)str.data, str.size);

	return str.size;
}

s64 Print(const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;
#if DEBUG_BUILD
	if (*(u64 *)buffer != 0) PANIC;
#endif

	va_list args;
	va_start(args, format);

	u64 size = stbsp_vsprintf(buffer, format, args);

	PrintString({ size, buffer });

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
#include "Scheduler.h"
#include "AST.h"
#include "Parser.h"
#include "TypeChecker.h"
#include "IRGen.h"
#include "CompileTime.h"
#include "Backend.h"
#include "x64.h"

struct Config
{
	bool silent;
	bool useEscapeSequences;
	bool dontPromoteMemoryToRegisters;
	bool dontCallAssembler;
	bool logAST;
	bool logIR;
	bool logAllocationInfo;
};

#define OUTPUT_BUFFER_BUCKET_SIZE 8192
#define STATIC_DATA_VIRTUAL_ADDRESS ((u8 *)0x0000044000000000)
#define STATIC_DATA_VIRTUAL_ADDRESS_END ((u8 *)0x0000044100000000)
struct Context
{
	Config config;

	Mutex consoleMutex;

	volatile u32 filesLock;
	DynamicArray<SourceFile, HeapAllocator> sourceFiles;
	DynamicArray<String, HeapAllocator> libsToLink;

	// Scheduler
	volatile bool done;
	SLContainer<BucketArray<Job, HeapAllocator, 512>> jobs; // Lock to add
	MTQueue<u32> readyJobs;
	volatile s32 failedJobsCount;
	SLContainer<DynamicArray<u32, HeapAllocator>> waitingJobsByReason[YIELDREASON_Count];

	volatile u32 threadStatesLock;
	Array<ThreadState, HeapAllocator> threadStates;

	// Static data
	volatile u32 staticDataLock;
	u64 staticDataSize;
	u64 staticDataAllocatedSpace;
	// We keep track of the types of things stored in static data in order to know what memory is
	// pointers to relocate them.
	DynamicArray<void *, HeapAllocator> staticDataPointersToRelocate;

	volatile u32 globalValuesLock;
	HashMap<u32, void *, LinearAllocator> globalValueContents;

	u8 *outputBufferMem;
	u64 outputBufferSize;
	u64 outputBufferOffset;
	u64 outputBufferCapacity;

	// Type check -----
	Array<TCScopeName, HeapAllocator> tcPrimitiveTypes;

	RWContainer<BucketArray<Value, HeapAllocator, 1024>> globalValues;
	RWContainer<BucketArray<Procedure, HeapAllocator, 512>> procedures;
	RWContainer<BucketArray<Procedure, HeapAllocator, 128>> externalProcedures;
	RWContainer<DynamicArray<PolymorphicProcedure, HeapAllocator>> polymorphicProcedures;
	RWContainer<DynamicArray<OperatorOverload, HeapAllocator>> operatorOverloads;
	RWContainer<BucketArray<StaticDefinition, HeapAllocator, 512>> staticDefinitions;

	/* Don't add types to the type table by hand without checking what AddType() does! */
	SLContainer<BucketArray<const TypeInfo, HeapAllocator, 1024>> typeTable; // Lock only to add

	SLRWContainer<BucketArray<TCScopeName, HeapAllocator, 1024>> tcGlobalNames;
	MTQueue<TCScopeName> tcGlobalNamesToAdd;
	volatile u32 tcGlobalNamesCommitLock; // Lock so multiple threads don't try to commit names at once.
	RWContainer<DynamicArray<u32, LinearAllocator>> tcGlobalTypeIndices;
	MXContainer<DynamicArray<DynamicArray<InlineCall, LinearAllocator>, LinearAllocator>> tcInlineCalls;

	// IR -----
	RWContainer<BucketArray<StringLiteral, HeapAllocator, 1024>> stringLiterals;
	RWContainer<BucketArray<StringLiteral, HeapAllocator, 128>> cStringLiterals;
	RWContainer<BucketArray<FloatLiteral,  HeapAllocator, 1024>> f32Literals;
	RWContainer<BucketArray<FloatLiteral,  HeapAllocator, 1024>> f64Literals;
	RWContainer<DynamicArray<u32, HeapAllocator>> irExternalVariables;

	// Compile Time -----
	MXContainer<DynamicArray<CTLibrary, HeapAllocator>> ctExternalLibraries;

	// Backend -----
	RWContainer<DynamicArray<BEFinalProcedure, HeapAllocator>> beFinalProcedureData;
};

struct ThreadArgs
{
	u32 threadIndex;
};

#include "Log.cpp"

bool CompilerAddSourceFile(String filename, SourceLocation loc)
{
	String fullName;

	if (SYSIsAbsolutePath(filename)) {
		fullName = filename;
		goto foundFullName;
	}

	// Working path relative
	fullName = SYSExpandPathWorkingDirectoryRelative(filename);
	if (SYSFileExists(fullName))
		goto foundFullName;

	// Linux only: search in /usr/include/fabric
#if IS_LINUX
	fullName = TStringConcat("/usr/include/fabric/"_s, filename);
	if (SYSFileExists(fullName))
		goto foundFullName;
#endif

	fullName = filename;

foundFullName:
	FileHandle file = SYSOpenFileRead(fullName);
	if (file == SYS_INVALID_FILE_HANDLE)
		LogError(loc, TPrintF("Could not find source file \"%S\"", filename));

	for (int i = 1; i < g_context->sourceFiles.size; ++i) {
		String currentFilename = g_context->sourceFiles[i].name;
		FileHandle currentFile = SYSOpenFileRead(currentFilename);
		if (currentFile == SYS_INVALID_FILE_HANDLE)
			continue;

		if (SYSAreSameFile(file, currentFile)) {
			// Don't think we should to warn this...
			//LogWarning(loc, TPrintF("File included twice: \"%S\"", filename));
			//LogNote(g_context->sourceFiles[i].includeLoc, "First included here"_s);
			return false;
		}
	}

	SourceFile newSourceFile = { fullName, loc };
	SYSReadEntireFile(file, &newSourceFile.buffer, &newSourceFile.size, LinearAllocator::Alloc);

	u32 fileIdx;
	{
		ScopedLockSpin sourceFilesLock(&g_context->filesLock);
		fileIdx = (u32)g_context->sourceFiles.size;
		*DynamicArrayAdd(&g_context->sourceFiles) = newSourceFile;
	}

	ParseJobArgs *args = ALLOC(LinearAllocator, ParseJobArgs);
	*args = { .fileIdx = fileIdx };
	RequestNewJob(JOBTYPE_PARSE, ParseJobProc, (void *)args);

	return true;
}

#include "OutputBuffer.cpp"
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
	g_hStdin  = GetStdHandle(STD_INPUT_HANDLE);
	g_hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	g_hStderr = GetStdHandle(STD_ERROR_HANDLE);

	DWORD initialConsoleMode;
	GetConsoleMode(g_hStdout, &initialConsoleMode);
	SetConsoleMode(g_hStdout, initialConsoleMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
#else
	g_hStdin  = fileno(stdin);
	g_hStdout = fileno(stdout);
	g_hStderr = fileno(stderr);
#endif

	Context *context = ALLOC(HeapAllocator, Context);
	*context = {};
	g_context = context;
	context->consoleMutex = SYSCreateMutex();

	// Allocate memory
	Memory memory;
	g_memory = &memory;
	memory.linearMem = SYSAlloc(Memory::linearMemSize);
	MemoryInit(&memory);

	MemoryInitThread(128 * 1024 * 1024);

	g_logFileHandle = SYSOpenFileWrite("output/log.txt"_s);

#if USE_PROFILER_API
	PerformanceAPI_LoadFrom(L"external/Superluminal/PerformanceAPI.dll", &performanceAPI);
	if (performanceAPI.BeginEvent == nullptr) {
		LogCompilerError({}, "Couldn't load profiler API DLL"_s);
		return 1;
	}
#endif

	BucketArrayInit(&context->jobs.unsafe);

	DynamicArrayInit(&context->sourceFiles, 16);
	++context->sourceFiles.size; // 0 is invalid file
	DynamicArrayInit(&context->libsToLink, 8);

	DynamicArray<String, LinearAllocator> inputFileNames;
	DynamicArrayInit(&inputFileNames, 16);
	*DynamicArrayAdd(&inputFileNames) = "core/core.fab"_s;
#if IS_WINDOWS
	*DynamicArrayAdd(&inputFileNames) = "core/core_windows.fab"_s;
#else
	*DynamicArrayAdd(&inputFileNames) = "core/core_linux.fab"_s;
#endif
	context->config.useEscapeSequences = true;
	for (int argIdx = 1; argIdx < argc; ++argIdx) {
		char *arg = argv[argIdx];
		if (arg[0] == '-') {
			if (strcmp("-silent", arg) == 0)
				context->config.silent = true;
			else if (strcmp("-noEscapeSequences", arg) == 0)
				context->config.useEscapeSequences = false;
			else if (strcmp("-noPromote", arg) == 0)
				context->config.dontPromoteMemoryToRegisters = true;
			else if (strcmp("-noBuildExecutable", arg) == 0)
				context->config.dontCallAssembler = true;
			else if (strcmp("-logAST", arg) == 0)
				context->config.logAST = true;
			else if (strcmp("-logIR", arg) == 0)
				context->config.logIR = true;
			else if (strcmp("-logAllocationInfo", arg) == 0)
				context->config.logAllocationInfo = true;
			else
				Print("Unknown option \"%s\"\n", arg);
		}
		else
			*DynamicArrayAdd(&inputFileNames) = CStrToString(arg);
	}

	if (argc < 2 && !SYSIsInputPipePresent()) {
		LogErrorNoCrash({}, "Received no input files"_s);
		LogNote({}, "Usage: compiler [options] <source file(s)>"_s);
		return 1;
	}

	if (!context->config.silent)
		TimerSplit("Initialization"_s);

	OutputBufferInit();

	ParserMain();
	TypeCheckMain();
	IRGenMain();
	BackendMain();

	// Allow passing source code through a pipe
	if (SYSIsInputPipePresent()) {
		SourceFile newSourceFile = { "<stdin>"_s, {} };
		// Dynamic buffer of bytes
		DynamicArray<u8, HeapAllocator> fileBuffer;
		DynamicArrayInit(&fileBuffer, 1024);
		// Read everything on stdin
		while (true) {
			u8 byte;
			if (SYSReadFile(g_hStdin, &byte, 1) != 1)
				break;
			*DynamicArrayAdd(&fileBuffer) = byte;
		}
		newSourceFile.buffer = (char *)fileBuffer.data;
		newSourceFile.size = fileBuffer.size;

		u32 fileIdx;
		{
			ScopedLockSpin sourceFilesLock(&g_context->filesLock);
			fileIdx = (u32)g_context->sourceFiles.size;
			*DynamicArrayAdd(&g_context->sourceFiles) = newSourceFile;
		}

		ParseJobArgs *args = ALLOC(LinearAllocator, ParseJobArgs);
		*args = { .fileIdx = fileIdx };
		RequestNewJob(JOBTYPE_PARSE, ParseJobProc, (void *)args);
	}

	for (int i = 0; i < inputFileNames.size; ++i)
		CompilerAddSourceFile(inputFileNames[i], {});

	if (!context->config.silent)
		TimerSplit("Create starting jobs"_s);

	int threadCount = SYSGetProcessorCount();
	Array<ThreadArgs, LinearAllocator> threadArgs;
	ArrayInit(&g_threads, threadCount);
	ArrayInit(&g_mainFibers, threadCount);
	ArrayInit(&threadArgs, threadCount);
	ArrayInit(&context->threadStates, threadCount);
	g_threads.size    = threadCount;
	g_mainFibers.size = threadCount;
	threadArgs.size   = threadCount;
	context->threadStates.size = threadCount;
	for (int i = 0; i < threadCount; ++i) {
		context->threadStates[i] = THREADSTATE_WORKING;
		g_mainFibers[i] = SYS_INVALID_FIBER_HANDLE;
		threadArgs[i] = { (u32)i };
		g_threads[i] = SYSCreateThread(WorkerThreadProc, &threadArgs[i]);
	}

	SYSWaitForThreads(threadCount, g_threads.data);

	if (!context->config.silent)
		TimerSplit("Multithreaded parse/analyze/codegen phase"_s);

	s32 failedJobsCount = context->failedJobsCount;
	if (failedJobsCount > 0) {
		if (failedJobsCount == 1)
			Print("A job failed. Aborting.\n");
		else
			Print("%d jobs failed. Aborting.\n", failedJobsCount);
		return 1;
	}

	// Report errors
	bool errorsFound = false;
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			LogErrorNoCrash(yieldContext.loc, TPrintF("Identifier \"%S\" never found",
						yieldContext.identifier));
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
			auto &globalNames = context->tcGlobalNames.unsafe;
			for (int nameIdx = 0; nameIdx < globalNames.count; ++nameIdx) {
				const TCScopeName *currentName = &globalNames[nameIdx];
				if (StringEquals(yieldContext.identifier, currentName->name))
					LogCompilerError(currentName->loc, "Identifier is there!"_s);
			}
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}

	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_OVERLOAD].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			if (yieldContext.overload.rightTypeIdx != U32_MAX) {
				LogErrorNoCrash(yieldContext.loc, TPrintF("Operator '%S' not found for "
							"types \"%S\" and \"%S\"",
							OperatorToString(yieldContext.overload.op),
							TypeInfoToString(yieldContext.overload.leftTypeIdx),
							TypeInfoToString(yieldContext.overload.rightTypeIdx)));
#if DEBUG_BUILD
				LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			}
			else {
				LogErrorNoCrash(yieldContext.loc, TPrintF("Operator '%S' not found for "
							"type \"%S\"",
							OperatorToString(yieldContext.overload.op),
							TypeInfoToString(yieldContext.overload.leftTypeIdx)));
#if DEBUG_BUILD
				LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			}
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_STATIC_DEF_NOT_READY].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			StaticDefinition staticDef = GetStaticDefinition(yieldContext.index);
			LogErrorNoCrash(yieldContext.loc, TPrintF("Static definition '%S' never "
						"type-checked", staticDef.name));
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_PROC_BODY_NOT_READY].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			Procedure proc = GetProcedureRead(yieldContext.index);
			LogErrorNoCrash(yieldContext.loc, TPrintF("Body of procedure '%S' never "
						"type-checked", proc.name));
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_PROC_IR_NOT_READY].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			Procedure proc = GetProcedureRead(yieldContext.index);
			LogErrorNoCrash(yieldContext.loc, TPrintF("Code of procedure '%S' never "
						"generated", proc.name));
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_POLYMORPHIC_PROC_NOT_CREATED].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			LogCompilerErrorNoCrash(yieldContext.loc, "Polymorphic instance never created a procedure"_s);
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			LogErrorNoCrash(yieldContext.loc, TPrintF("Type '%S' never finished "
						"type-checking", TypeInfoToString(yieldContext.index)));
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			Value v = context->globalValues.unsafe[yieldContext.index & VALUE_GLOBAL_MASK];
			LogErrorNoCrash(yieldContext.loc, TPrintF("Value of static variable '%S' "
						"never computed", GetValueName(v)));
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_NEED_DYNAMIC_LIBRARY].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			LogErrorNoCrash(yieldContext.loc, TPrintF("Could not find external procedure "
						"\"%S\" at compile time", yieldContext.identifier));
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_WAITING_FOR_STOP].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->yieldContext;
			LogErrorNoCrash(yieldContext.loc, "Job waiting for dead-stop never resumed "
						"for some reason"_s);
#if DEBUG_BUILD
			LogNote(job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	if (errorsFound)
		LogError({}, "Errors were found. Aborting"_s);

	ASSERT(MTQueueIsEmpty(&g_context->readyJobs));
	ASSERT(MTQueueIsEmpty(&g_context->tcGlobalNamesToAdd));

	for (int i = 0; i < g_context->jobs.unsafe.count; ++i) {
		Job job = g_context->jobs.unsafe[i];
		ASSERT(job.state == JOBSTATE_FINISHED);
	}

	BackendGenerateOutputFile();

	if (!g_context->config.silent) {
		TimerSplit("Done"_s);

#if ENABLE_STATS
		Print("STATS:\n");
		Print("  Job switches: %d\n", g_stats.jobSwitches);
		Print("  Thread allocator bytes wasted by realloc: 0x%X\n", g_stats.threadAllocReallocWaste);
		Print("  Linear allocator bytes used: 0x%X\n", (u64)g_memory->linearMemPtr - (u64)g_memory->linearMem);
		Print("  Linear allocator bytes wasted by realloc: 0x%X\n", g_stats.linearAllocReallocWaste);
		Print("  Dynamic array reallocs: %d\n", g_stats.dynamicArrayReallocs);
		Print("  Hash set/map hits: %d\n", g_stats.hashSetMapHit0);
		Print("  Hash set/map hits 2 its: %d\n", g_stats.hashSetMapHit1);
		Print("  Hash set/map hits 3 its: %d\n", g_stats.hashSetMapHit2);
		Print("  Hash set/map hits 4 its: %d\n", g_stats.hashSetMapHit3);
		Print("  Hash set/map hits 5+ its: %d\n", g_stats.hashSetMapHitMore);
		Print("  Hash set/map rehashes: %d\n", g_stats.hashSetMapRehashes);
		Print("  Registers coalesced: %d\n", g_stats.coalescingSuccesses);
		Print("  MOVs removed: %d\n", g_stats.movsRemoved);
#endif

		ConsoleSetColor(CONSOLE_GREEN_TXT);
		Print("Compilation success\n");
		ConsoleSetColor(CONSOLE_RESET_COLOR);
	}

#if IS_WINDOWS
	SetConsoleMode(g_hStdout, initialConsoleMode);
#endif

	return 0;
}
