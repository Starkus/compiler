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

#define USE_OWN_ASSEMBLER 1

// To properly turn this off, we'd need to make sure we don't leak locks anywhere we call LogError.
#define EXIT_ON_FIRST_ERROR 1

#include "Config.h"
#include "Maths.h"
#include "Multithreading.h"
#include "Containers.h"

// Keep in sync with core/basic.emi
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

struct JobContext
{
	Context *global;
	u32 jobIdx;
};

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
#define STATIC_DATA_VIRTUAL_ADDRESS_END ((u8 *)0x0000045000000000)
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
	MXContainer<DynamicArray<u32, HeapAllocator>> waitingJobsByReason[YIELDREASON_Count];

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
	RWContainer<DynamicArray<OperatorOverload, HeapAllocator>> operatorOverloads;
	RWContainer<BucketArray<StaticDefinition, HeapAllocator, 512>> staticDefinitions;

	/* Don't add types to the type table by hand without checking what AddType() does! */
	SLContainer<BucketArray<const TypeInfo, HeapAllocator, 1024>> typeTable; // Lock only to add

	SLRWContainer<DynamicArray<TCScopeName, LinearAllocator>> tcGlobalNames;
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
	Context *context;
	u32 threadIndex;
};

#include "Log.cpp"

bool CompilerAddSourceFile(Context *context, String filename, SourceLocation loc)
{
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
	RequestNewJob(context, JOBTYPE_PARSE, ParseJobProc, (void *)args);

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

	BucketArrayInit(&context->jobs.unsafe);

	DynamicArrayInit(&context->sourceFiles, 16);
	++context->sourceFiles.size; // 0 is invalid file
	DynamicArrayInit(&context->libsToLink, 8);

	DynamicArray<String, LinearAllocator> inputFiles;
	DynamicArrayInit(&inputFiles, 16);
	*DynamicArrayAdd(&inputFiles) = "core/basic.emi"_s;
	*DynamicArrayAdd(&inputFiles) = "core/print.emi"_s;
#if IS_WINDOWS
	*DynamicArrayAdd(&inputFiles) = "core/basic_windows.emi"_s;
#else
	*DynamicArrayAdd(&inputFiles) = "core/basic_linux.emi"_s;
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
			*DynamicArrayAdd(&inputFiles) = CStrToString(arg);
	}
	ASSERT(inputFiles.size > 2);

	if (!context->config.silent)
		TimerSplit("Initialization"_s);

	OutputBufferInit(context);

	ParserMain(context);
	TypeCheckMain(context);
	IRGenMain(context);
	BackendMain(context);

	for (int i = 0; i < inputFiles.size; ++i)
		CompilerAddSourceFile(context, inputFiles[i], {});

	if (!context->config.silent)
		TimerSplit("Create starting jobs"_s);

	SYSTEM_INFO win32SystemInfo;
	GetSystemInfo(&win32SystemInfo);
	int threadCount = win32SystemInfo.dwNumberOfProcessors;

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
		threadArgs[i] = { context, (u32)i };
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
			YieldContext yieldContext = job->context;
			LogErrorNoCrash(context, yieldContext.loc, TPrintF("Identifier \"%S\" never found",
						yieldContext.identifier));
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
			auto &globalNames = context->tcGlobalNames.unsafe;
			for (int nameIdx = 0; nameIdx < globalNames.size; ++nameIdx) {
				const TCScopeName *currentName = &globalNames[nameIdx];
				if (StringEquals(yieldContext.identifier, currentName->name))
					LogCompilerError(context, currentName->loc, "Identifier is there!"_s);
			}
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}

	JobContext fakeJobContext = { context, U32_MAX };

	{
		auto waitingJobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_OVERLOAD].Get();
		for (int i = 0; i < waitingJobs->size; ++i) {
			u32 jobIdx = waitingJobs[i];
			const Job *job = &context->jobs.unsafe[jobIdx];
			YieldContext yieldContext = job->context;
			if (yieldContext.overload.rightTypeIdx != U32_MAX) {
				LogErrorNoCrash(context, yieldContext.loc, TPrintF("Operator '%S' not found for "
							"types \"%S\" and \"%S\"",
							OperatorToString(yieldContext.overload.op),
							TypeInfoToString(&fakeJobContext, yieldContext.overload.leftTypeIdx),
							TypeInfoToString(&fakeJobContext, yieldContext.overload.rightTypeIdx)));
#if DEBUG_BUILD
				LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			}
			else {
				LogErrorNoCrash(context, yieldContext.loc, TPrintF("Operator '%S' not found for "
							"type \"%S\"",
							OperatorToString(yieldContext.overload.op),
							TypeInfoToString(&fakeJobContext, yieldContext.overload.leftTypeIdx)));
#if DEBUG_BUILD
				LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
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
			YieldContext yieldContext = job->context;
			StaticDefinition staticDef = GetStaticDefinition(&fakeJobContext, yieldContext.index);
			LogErrorNoCrash(context, yieldContext.loc, TPrintF("Static definition '%S' never "
						"type-checked", staticDef.name));
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
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
			YieldContext yieldContext = job->context;
			Procedure proc = GetProcedureRead(context, yieldContext.index);
			LogErrorNoCrash(context, yieldContext.loc, TPrintF("Body of procedure '%S' never "
						"type-checked", proc.name));
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
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
			YieldContext yieldContext = job->context;
			Procedure proc = GetProcedureRead(context, yieldContext.index);
			LogErrorNoCrash(context, yieldContext.loc, TPrintF("Code of procedure '%S' never "
						"generated", proc.name));
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
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
			YieldContext yieldContext = job->context;
			LogErrorNoCrash(context, yieldContext.loc, TPrintF("Type '%S' never finished "
						"type-checking", TypeInfoToString(&fakeJobContext, yieldContext.index)));
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
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
			YieldContext yieldContext = job->context;
			Value v = context->globalValues.unsafe[yieldContext.index & VALUE_GLOBAL_MASK];
			LogErrorNoCrash(context, yieldContext.loc, TPrintF("Value of static variable '%S' "
						"never computed", GetValueName(v)));
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
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
			YieldContext yieldContext = job->context;
			LogErrorNoCrash(context, yieldContext.loc, TPrintF("Could not find external procedure "
						"\"%S\" at compile time", yieldContext.identifier));
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
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
			YieldContext yieldContext = job->context;
			LogErrorNoCrash(context, yieldContext.loc, "Job waiting for dead-stop never resumed "
						"for some reason"_s);
#if DEBUG_BUILD
			LogNote(context, job->loc, TPrintF("On job: \"%S\"", job->description));
#endif
			errorsFound = true;
		}
		waitingJobs->size = 0;
	}
	if (errorsFound)
		LogError(context, {}, "Errors were found. Aborting"_s);

	ASSERT(MTQueueIsEmpty(&context->readyJobs));

	for (int i = 0; i < context->jobs.unsafe.count; ++i) {
		Job job = context->jobs.unsafe[i];
		ASSERT(job.state == JOBSTATE_FINISHED);
	}

#if USE_OWN_ASSEMBLER
	BackendGenerateWindowsObj(context);
#else
	BackendGenerateOutputFile(context);
#endif

	if (!context->config.silent) {
		TimerSplit("Done"_s);
		ConsoleSetColor(CONSOLE_GREEN_TXT);
		Print("Compilation success\n");
		ConsoleSetColor(CONSOLE_RESET_COLOR);
	}

#if IS_WINDOWS
	SetConsoleMode(g_hStdout, initialConsoleMode);
#endif

	return 0;
}
