inline bool SYSFileExists(String filename)
{
	const char *cstr = StringToCStr(filename, ThreadAllocator::Alloc);
	struct stat buffer;
	if (stat(cstr, &buffer) != 0)
		return false;
	// Regular files only
	return S_ISREG(buffer.st_mode);
}

inline bool SYSDirectoryExists(String filename)
{
	const char *cstr = StringToCStr(filename, ThreadAllocator::Alloc);
	struct stat buffer;
	if (stat(cstr, &buffer) != 0)
		return false;
	return S_ISDIR(buffer.st_mode);
}

inline bool SYSIsAbsolutePath(String filename)
{
	return filename.size && filename.data[0] == '/';
}

String SYSExpandPathCompilerRelative(String relativePath)
{
	ASSERT(!SYSIsAbsolutePath(relativePath));

	char relativeCStr[SYS_MAX_PATH];
	strncpy(relativeCStr, relativePath.data, relativePath.size);
	relativeCStr[relativePath.size] = 0;
	char *absolutePath = (char *)ThreadAllocator::Alloc(SYS_MAX_PATH, 1);
	realpath(relativeCStr, absolutePath);

	String result;
	result.data = absolutePath;
	result.size = strlen(absolutePath);
	return result;
}

String SYSExpandPathWorkingDirectoryRelative(String relativePath)
{
	ASSERT(!SYSIsAbsolutePath(relativePath));
	String result;

	char *absolutePath = (char *)ThreadAllocator::Alloc(SYS_MAX_PATH, 1);
	result.data = absolutePath;

	getcwd(absolutePath, SYS_MAX_PATH);
	s64 written = strlen(absolutePath);
	absolutePath[written++] = '/';
	strncpy(absolutePath + written, relativePath.data, relativePath.size);
	absolutePath[written + relativePath.size] = 0;

	result.size = written + relativePath.size;

	return result;
}

FileHandle SYSOpenFileRead(String filename)
{
	String fullname = filename;
	if (!SYSIsAbsolutePath(filename))
		fullname = SYSExpandPathWorkingDirectoryRelative(filename);

	FileHandle file = open(StringToCStr(fullname, ThreadAllocator::Alloc), O_RDONLY);
	if (file == SYS_INVALID_FILE_HANDLE) {
		switch (errno) {
		case EACCES:
			Print("Cannot open file \"%S\" for read, access denied\n", fullname);
			break;
		case ENOENT:
			// No error if file doesn't exist, just return invalid handle.
			break;
		default:
			Print("Cannot open file \"%S\" for read, error: %s\n", fullname, strerror(errno));
			break;
		}
	}
	return file;
}

FileHandle SYSOpenFileWrite(String filename)
{
	String fullname = filename;
	if (!SYSIsAbsolutePath(filename))
		fullname = SYSExpandPathWorkingDirectoryRelative(filename);

	FileHandle file = open(fullname.data, O_WRONLY | O_CREAT | O_TRUNC,
			S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
	if (file == SYS_INVALID_FILE_HANDLE) {
		switch (errno) {
		case EACCES:
			Print("Cannot open file \"%S\" for write, access denied\n", fullname);
			break;
		case ENOENT:
			// No error if file doesn't exist, just return invalid handle.
			break;
		default:
			Print("Cannot open file \"%S\" for write, error: %s\n", fullname, strerror(errno));
			break;
		}
	}
	return file;
}

void *SYSReserveMemory(void *addressHint, u64 size)
{
	void *result = mmap(addressHint, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (result == MAP_FAILED) {
		Print("mmap error: %s\n", strerror(errno));
		return nullptr;
	}
	return result;
}

void *SYSCommitMemory(void *address, u64 size)
{
	int error = mprotect(address, size, PROT_READ | PROT_WRITE);
#if DEBUG_BUILD
	if (error)
		Print("mprotect error: %s\n", strerror(errno));
#endif
	ASSERT(error == 0);
	return address;
}

void SYSFreeMemory(void *address, u64 size)
{
	int error = munmap(address, size);
#if DEBUG_BUILD
	if (error)
		Print("munmap error: %s\n", strerror(errno));
#endif
	ASSERT(error == 0);
}

s64 SYSWriteFile(FileHandle fileHandle, void *buffer, s64 size)
{
	return write(fileHandle, buffer, size);
}

s64 SYSReadFile(FileHandle file, void *buffer, u64 size)
{
	return read(file, buffer, size);
}

u64 SYSGetFileSize(FileHandle file)
{
	struct stat fileStat;
	fstat(file, &fileStat);
	return fileStat.st_size;
}

void SYSReadEntireFile(FileHandle file, const char **fileBuffer, u64 *fileSize,
		void *(*allocFunc)(u64, int))
{
	if (file == SYS_INVALID_FILE_HANDLE)
		*fileBuffer = nullptr;
	else {
		*fileSize = SYSGetFileSize(file);
		ASSERT(*fileSize);
		char *buffer = (char *)allocFunc(*fileSize, 1);
		u64 bytesRead = read(file, buffer, *fileSize);
		ASSERT(bytesRead == *fileSize);
		*fileBuffer = buffer;
	}
}

bool SYSAreSameFile(FileHandle file1, FileHandle file2) {
	struct stat stat1, stat2;
	ASSERT(file1 != SYS_INVALID_FILE_HANDLE);
	ASSERT(file2 != SYS_INVALID_FILE_HANDLE);
	fstat(file1, &stat1);
	fstat(file2, &stat2);
	return stat1.st_ino == stat2.st_ino;
}

inline u64 SYSPerformanceCounter()
{
	timespec t;
	clock_gettime(CLOCK_MONOTONIC_RAW, &t);
	return t.tv_nsec + t.tv_sec * 1000000000;
}

inline u64 SYSPerformanceFrequency()
{
	timespec t;
	clock_getres(CLOCK_MONOTONIC_RAW, &t);
	return t.tv_nsec * 1000000000;
}

inline void SYSCloseFile(FileHandle file)
{
	close(file);
}

inline void *SYSAlloc(u64 size)
{
	return valloc(size);
}

inline void SYSFree(void *ptr)
{
	free(ptr);
}

void SYSCreateDirectory(String pathname)
{
	char pathnameCStr[SYS_MAX_PATH];
	strncpy(pathnameCStr, pathname.data, pathname.size);
	pathnameCStr[pathname.size] = 0;
	mkdir(pathnameCStr, 0777);
}

void SYSRunAssembler(String outputPath, String outputFilename, String extraArguments, bool silent)
{
#if 1
	String yasmCmd = TPrintF("yasm -f elf64 -g dwarf2 %S %S -o %S%c",
			extraArguments,
			ChangeFilenameExtension(outputFilename, ".asm"_s),
			ChangeFilenameExtension(outputFilename, ".o"_s),
			0);
	int status = system(yasmCmd.data);
	if (status) {
		if (status > 255) status = 255;
		Print("Error executing yasm! Error 0x%.2x\n", status);
		exit(status);
	}
#else
	int status;
	pid_t pid = fork();

	if (pid == 0) {
		const char *filenameCstr = StringToCStr(ChangeFilenameExtension(outputFilename, ".asm"_s),
				ThreadAllocator::Alloc);
		const char *yasmArgs[] = {
			"yasm",
			"-f elf64",
			"-F dwarf",
			"-g",
			filenameCstr,
			0
		};
		if (execvp("yasm", (char **)yasmArgs) == -1) {
			Print("Error executing yasm!\n");
			exit(1);
		}
	}
#endif
}

void SYSRunLinker(String outputPath, String outputFilename, OutputType outputType,
		ArrayView<String> exportedSymbols, String extraArguments, bool silent)
{
#if 1
	String linkerProgram = "ld"_s;
	if (outputType == OUTPUTTYPE_LIBRARY_STATIC) {
		String arCmd = TPrintF("ar rcs %S %S %S%c",
			ChangeFilenameExtension(outputFilename, ".a"_s),
			ChangeFilenameExtension(outputFilename, ".o"_s),
			extraArguments,
			0);
		int status = system(arCmd.data);
		if (status) {
			if (status > 255) status = 255;
			Print("Error executing ar! Error 0x%.2x\n", status);
			exit(status);
		}
	}
	else if (outputType == OUTPUTTYPE_LIBRARY_DYNAMIC) {
		String linkerCmd = TPrintF("%S -shared %S %S -o %S%c",
			linkerProgram,
			ChangeFilenameExtension(outputFilename, ".o"_s),
			extraArguments,
			ChangeFilenameExtension(outputFilename, ".so"_s),
			0);
		int status = system(linkerCmd.data);
		if (status) {
			if (status > 255) status = 255;
			Print("Error executing linker %S! Error 0x%.2x\n", linkerProgram, status);
			exit(status);
		}
	}
	else {
		String linkerCmd = TPrintF("%S %S %S -o %S -e __LinuxStart%c",
			linkerProgram,
			ChangeFilenameExtension(outputFilename, ".o"_s),
			extraArguments,
			ChangeFilenameExtension(outputFilename, {}),
			0);
		int status = system(linkerCmd.data);
		if (status) {
			if (status > 255) status = 255;
			Print("Error executing linker %S! Error 0x%.2x\n", linkerProgram, status);
			exit(status);
		}
	}
#else
	int status;
	pid_t pid = fork();

	if (pid == 0)
	{
		String outputArg = TPrintF("-o %S%c", SYSExpandPathWorkingDirectoryRelative("output/out"_s),
				0);
		const char *moldArgs[] =
		{
			"mold",
			SYSExpandPathWorkingDirectoryRelative("output/out.o"_s).data,
			outputArg.data,
			"-e __LinuxMain",
			0
		};
		if (execvp("mold", (char **)moldArgs) == -1)
		{
			Print("Error executing mold!\n");
			exit(1);
		}
	}
#endif
}

inline ThreadHandle SYSCreateThread(int (*start)(void *), void *args)
{
	pthread_t thread;
	int error = pthread_create(&thread, nullptr, (void *(*)(void *))start, args);
	ASSERT(error == 0);
	return thread;
}

inline void SYSWaitForThread(ThreadHandle thread)
{
	int res = pthread_join(thread, nullptr);
	ASSERT(res == 0);
}

inline void SYSWaitForThreads(int count, ThreadHandle *threads)
{
	for (int i = 0; i < count; ++i)
	{
		int res = pthread_join(threads[i], nullptr);
		ASSERT(res == 0);
	}
}

inline void SYSExitThread(int errorCode)
{
	int *retval = ALLOC(LinearAllocator, int);
	*retval = errorCode;
	pthread_exit(retval);
}

inline ThreadHandle SYSGetCurrentThread()
{
	return pthread_self();
}

void SYSSetThreadDescription(ThreadHandle thread, String string) {
	char buffer[32];
	strncpy(buffer, string.data, 32);
	if (string.size < 32)
		buffer[string.size] = 0;
	pthread_setname_np(thread, buffer);
}

inline Mutex SYSCreateMutex() {
	Mutex mutex;
	pthread_mutex_init(&mutex, nullptr);
	return mutex;
}

inline void SYSMutexLock(Mutex &mutex) {
	pthread_mutex_lock(&mutex);
}

inline void SYSMutexUnlock(Mutex &mutex) {
	pthread_mutex_unlock(&mutex);
}

inline void SYSCreateRWLock(RWLock *lock) {
	pthread_rwlock_init(lock, nullptr);
}

inline void SYSLockForRead(RWLock *lock) {
	pthread_rwlock_rdlock(lock);
}

inline void SYSUnlockForRead(RWLock *lock) {
	pthread_rwlock_unlock(lock);
}

inline void SYSLockForWrite(RWLock *lock) {
	pthread_rwlock_wrlock(lock);
}

inline void SYSUnlockForWrite(RWLock *lock) {
	pthread_rwlock_unlock(lock);
}

#if 0
struct ConditionVariable {
	pthread_mutex_t mutex;
	DynamicArray<sem_t, LinearAllocator> semaphores;
};

inline void SYSCreateConditionVariable(ConditionVariable *conditionVar) {
	pthread_mutex_init(&conditionVar->mutex, nullptr);
	DynamicArrayInit(&conditionVar->semaphores, 8);
}

inline void SYSSleepConditionVariableRead(ConditionVariable *conditionVar, RWLock *lock) {
	pthread_mutex_lock(&conditionVar->mutex);
	sem_t *newSemaphore = DynamicArrayAdd(&conditionVar->semaphores);
	sem_init(newSemaphore, 0, 0);
	pthread_mutex_unlock(&conditionVar->mutex);

	SYSUnlockForRead(lock);
	int error = sem_wait(newSemaphore);
	if (error != 0 && errno != EINTR) {
		Print("Error on sem_wait\n");
		PANIC;
	}
	sem_destroy(newSemaphore);
	SYSLockForRead(lock);
}

inline void SYSWakeAllConditionVariable(ConditionVariable *conditionVar) {
	pthread_mutex_lock(&conditionVar->mutex);
	for (int i = 0; i < conditionVar->semaphores.size; ++i)
		sem_post(&conditionVar->semaphores[i]);
	conditionVar->semaphores.size = 0;
	pthread_mutex_unlock(&conditionVar->mutex);
}
#else
typedef pthread_cond_t ConditionVariable;
inline void SYSCreateConditionVariable(ConditionVariable *conditionVar) {
	pthread_cond_init(conditionVar, nullptr);
}

inline void SYSSleepConditionVariableRead(ConditionVariable *conditionVar, Mutex *lock) {
	pthread_cond_wait(conditionVar, lock);
}

inline void SYSWakeAllConditionVariable(ConditionVariable *conditionVar)
{
	pthread_cond_broadcast(conditionVar);
}
#endif

inline void SYSSleep(int milliseconds)
{
	struct timespec t = { 0, milliseconds * 1000 };
	nanosleep(&t, nullptr);
}

inline s32 AtomicCompareExchange(volatile s32 *destination, s32 exchange, s32 comparand)
{
	__atomic_compare_exchange(destination, &comparand, &exchange, false, __ATOMIC_SEQ_CST,
			__ATOMIC_SEQ_CST);
	return comparand;
}

inline s64 AtomicCompareExchange64(volatile s64 *destination, s64 exchange, s64 comparand)
{
	__atomic_compare_exchange(destination, &comparand, &exchange, false, __ATOMIC_SEQ_CST,
			__ATOMIC_SEQ_CST);
	return comparand;
}

inline s32 AtomicIncrementGetNew(volatile s32 *destination)
{
	return __atomic_add_fetch(destination, 1, __ATOMIC_SEQ_CST);
}

// Fibers!
// @Improve: infinite storage for fibers
ucontext_t fiberContexts[8192];
u32 g_fiberCount = 0;
__thread u32 t_currentFiber = U32_MAX;
__thread ucontext_t t_originalContext;

inline Fiber SYSGetCurrentFiber()
{
	return t_currentFiber;
}

inline Fiber SYSCreateFiber(void (*start)(void *), void *args)
{
	const u64 fiberStackSize = 1 * 1024 * 1024; // 1MB
	void *fiberStack = SYSAlloc(fiberStackSize);
	u32 fiberIdx = __atomic_fetch_add(&g_fiberCount, 1, __ATOMIC_SEQ_CST);
	ASSERT(fiberIdx < ArrayCount(fiberContexts)); // Out of fibers!
	ucontext_t *fiberContext = &fiberContexts[fiberIdx];
	int res = getcontext(fiberContext);
	ASSERT(res == 0);
	fiberContext->uc_stack.ss_sp = fiberStack;
	fiberContext->uc_stack.ss_size = fiberStackSize;
	fiberContext->uc_link = nullptr;
	makecontext(fiberContext, (void (*)())start, 1, args);
	return fiberIdx;
}

inline Fiber SYSConvertThreadToFiber()
{
	u32 fiberIdx = __atomic_fetch_add(&g_fiberCount, 1, __ATOMIC_SEQ_CST);
	t_currentFiber = fiberIdx;
	ASSERT(fiberIdx < ArrayCount(fiberContexts)); // Out of fibers!
	ucontext_t *fiberContext = &fiberContexts[fiberIdx];
	int res = getcontext(fiberContext);
	ASSERT(res == 0);
	return fiberIdx;
}

inline void SYSDeleteFiber(Fiber fiber)
{
	// @Todo: can we delete something?
}

inline void SYSPrepareFiberForExit()
{
	// Nothing on linux?
}

inline void SYSSwitchToFiber(Fiber fiber)
{
	ASSERT(fiber != SYS_INVALID_FIBER_HANDLE);
	Fiber oldFiber = t_currentFiber;
	t_currentFiber = fiber;
	if (oldFiber != SYS_INVALID_FIBER_HANDLE) {
		fiberContexts[fiber].uc_link = &fiberContexts[oldFiber];
		swapcontext(&fiberContexts[oldFiber], &fiberContexts[fiber]);
	}
	else {
		fiberContexts[fiber].uc_link = &t_originalContext;
		swapcontext(&t_originalContext, &fiberContexts[fiber]);
	}
}

extern "C" u64 SYSCallProcedureDynamically_linuxcc(void *start, u64 gpArgCount, void *gpArgValues,
		u64 xmmArgCount, void *xmmArgValues, u64 stackArgCount, void *stackArgValues);

extern "C" u64 SYSCallProcedureDynamically_windowscc(void *start, u64 argCount, void *argValues);

void *SYSLoadDynamicLibrary(String filename)
{
	const char *cstr = StringToCStr(filename, ThreadAllocator::Alloc);
	void *result = dlopen(cstr, RTLD_LAZY);
	if (result) return result;

	// Look in working path
	String workingPathRelative = SYSExpandPathWorkingDirectoryRelative(filename);
	// this string is null terminated
	result = dlopen(workingPathRelative.data, RTLD_LAZY);
	if (result) return result;

	// Look in compiler path
	String compilerPathRelative = SYSExpandPathCompilerRelative(TStringConcat("bin/"_s, filename));
	// this string is null terminated
	result = dlopen(compilerPathRelative.data, RTLD_LAZY);

#if IS_LINUX
	// Look in compiler path
	String installPathRelative = TPrintF("/usr/lib/fabric/%S%c", filename, 0);
	// this string is null terminated
	result = dlopen(installPathRelative.data, RTLD_LAZY);
#endif

	return result;
}

inline void *SYSGetProcAddress(void *lib, const char *procedureCStr)
{
	return dlsym(lib, procedureCStr);
}

inline int SYSGetProcessorCount()
{
	return sysconf(_SC_NPROCESSORS_ONLN);
}

inline bool SYSIsInputPipePresent()
{
	return !isatty(STDIN_FILENO);
}
