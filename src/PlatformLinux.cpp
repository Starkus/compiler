String SYSExpandPathCompilerRelative(String relativePath)
{
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
	String result;

	char *absolutePath = (char *)ThreadAllocator::Alloc(SYS_MAX_PATH, 1);
	result.data = absolutePath;

	ASSERT(getcwd(absolutePath, SYS_MAX_PATH));
	s64 written = strlen(absolutePath);
	absolutePath[written++] = '/';
	strncpy(absolutePath + written, relativePath.data, relativePath.size);
	absolutePath[written + relativePath.size] = 0;

	result.size = written + relativePath.size;

	return result;
}

FileHandle SYSOpenFileRead(String filename)
{
	String fullname = SYSExpandPathWorkingDirectoryRelative(filename);
	return open(fullname.data, O_RDONLY);
}

FileHandle SYSOpenFileWrite(String filename)
{
	String fullname = SYSExpandPathWorkingDirectoryRelative(filename);
	return open(fullname.data, O_WRONLY | O_CREAT | O_TRUNC,
			S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
}

s64 SYSWriteFile(FileHandle fileHandle, void *buffer, s64 size)
{
	return write(fileHandle, buffer, size);
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
	else
	{
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
	ASSERT(clock_gettime(CLOCK_MONOTONIC_RAW, &t) == 0);
	return t.tv_nsec + t.tv_sec * 1000000000;
}

inline u64 SYSPerformanceFrequency()
{
	timespec t;
	ASSERT(clock_getres(CLOCK_MONOTONIC_RAW, &t) == 0);
	return t.tv_nsec * 1000000000;
}

void SYSCloseFile(FileHandle file)
{
	close(file);
}

void *SYSAlloc(u64 size)
{
	return valloc(size);
}

void SYSCreateDirectory(String pathname)
{
	char pathnameCStr[SYS_MAX_PATH];
	strncpy(pathnameCStr, pathname.data, pathname.size);
	pathnameCStr[pathname.size] = 0;
	mkdir(pathnameCStr, 0777);
}

void SYSRunAssembler(String outputPath, String extraArguments)
{
#if 1
	String yasmCmd = TPrintF("yasm -f elf64 -g dwarf2 %S %S -o %S%c",
			extraArguments,
			SYSExpandPathWorkingDirectoryRelative("output/out.asm"_s),
			SYSExpandPathWorkingDirectoryRelative("output/out.o"_s), 0);
	int status = system(yasmCmd.data);
	if (status)
	{
		if (status > 255) status = 255;
		Print("Error executing yasm! Error 0x%.2x\n", status);
		exit(status);
	}
#else
	int status;
	pid_t pid = fork();

	if (pid == 0)
	{
		const char *yasmArgs[] =
		{
			"yasm",
			"-f elf64",
			"-F dwarf",
			"-g",
			SYSExpandPathWorkingDirectoryRelative("output/out.asm"_s).data,
			0
		};
		if (execvp("yasm", (char **)yasmArgs) == -1)
		{
			Print("Error executing yasm!\n");
			exit(1);
		}
	}
#endif
}

void SYSRunLinker(String outputPath, bool makeLibrary, String extraArguments)
{
#if 1
	if (makeLibrary)
	{
		String arCmd = TPrintF("ar rcs %S %S %S%c",
			extraArguments,
			SYSExpandPathWorkingDirectoryRelative("output/out.a"_s),
			SYSExpandPathWorkingDirectoryRelative("output/out.o"_s), 0);
		int status = system(arCmd.data);
		if (status)
		{
			if (status > 255) status = 255;
			Print("Error executing ar! Error 0x%.2x\n", status);
			exit(status);
		}
	}
	else
	{
		String moldCmd = TPrintF("mold %S %S -o %S -e __LinuxMain%c",
			SYSExpandPathWorkingDirectoryRelative("output/out.o"_s),
			extraArguments,
			SYSExpandPathWorkingDirectoryRelative("output/out"_s), 0);
		int status = system(moldCmd.data);
		if (status)
		{
			if (status > 255) status = 255;
			Print("Error executing mold! Error 0x%.2x\n", status);
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
	return CreateThread(nullptr, 0, (DWORD (*)(void *))start, args, 0, nullptr);
}

inline void SYSWaitForThread(ThreadHandle thread)
{
	WaitForSingleObject(thread, INFINITE);
}

inline ThreadHandle SYSGetCurrentThread()
{
	return GetCurrentThread();
}

inline u32 SYSAllocThreadData()
{
	u32 key;
	ASSERT(pthread_key_create(&key, nullptr) == 0);
	return key;
}

inline void *SYSGetThreadData(u32 key)
{
	return pthread_get_specific(key);
}

inline bool SYSSetThreadData(u32 key, void *value)
{
	return pthread_set_specific(key, value) == 0;
}

void SYSSetThreadDescription(ThreadHandle thread, String string)
{
	char buffer[32];
	strncpy(buffer, string.data, string.size);
	if (string.size < 32)
		buffer[string.size] = 0;
	pthread_setname_np(thread, buffer);
}

inline Mutex SYSCreateMutex()
{
	Mutex mutex;
	ASSERT(pthread_mutex_init(&mutex) == 0);
	return mutex;
}

inline void SYSMutexLock(Mutex &mutex)
{
	pthread_mutex_lock(&mutex);
}

inline void SYSMutexUnlock(Mutex &mutex)
{
	pthread_mutex_unlock(&mutex);
}

inline void SYSCreateRWLock(RWLock *lock)
{
	pthread_rwlock_init(lock, nullptr);
}

inline void SYSLockForRead(RWLock *lock)
{
	pthread_rwlock_rdlock(lock);
}

inline void SYSUnlockForRead(RWLock *lock)
{
	pthread_rwlock_unlock(lock);
}

inline void SYSLockForWrite(RWLock *lock)
{
	pthread_rwlock_wrlock(lock);
}

inline void SYSUnlockForWrite(RWLock *lock)
{
	pthread_rwlock_unlock(lock);
}

struct ConditionVariable
{
	pthread_mutex_t mutex;
	DynamicArray<sem_t, LinearAllocator> semaphores;
};

inline void SYSCreateConditionVariable(ConditionVariable *conditionVar)
{
	ASSERT(pthread_mutex_init(&conditionVar->mutex) == 0);
	DynamicArrayInit(&conditionVar->semaphores, 8);
}

inline void SYSSleepConditionVariableRead(ConditionVariable *conditionVar, RWLock *lock)
{
	pthread_mutex_lock(&conditionVar->mutex);
	sem_t *newSemaphore = DynamicArrayAdd(&conditionVar->semaphores);
	sem_init(newSemaphore, 0, 0);
	pthread_mutex_unlock(&conditionVar->mutex);

	SYSLockForRead(lock);
	sem_wait(newSemaphore);
	SYSUnlockForRead(lock);
}

inline void SYSWakeAllConditionVariable(ConditionVariable *conditionVar)
{
	pthread_mutex_lock(&conditionVar->mutex);
	for (int i = 0; i < conditionVar->semaphores.size; ++i)
		sem_post(&conditionVar->semaphores[i]);
	pthread_mutex_unlock(&conditionVar->mutex);
}

inline void SYSSpinlockLock(volatile u32 *locked)
{
	int oldLocked = 1;
retry:
	int eax = 0;
	asm volatile("xacquire lock cmpxchg %2, %1"
					: "+a" (eax), "+m" (*locked)
					: "r" (oldLocked) : "memory", "cc");
	if (eax)
		goto ret;

pause:
	if (!*locked)
		goto retry;
	_mm_pause();
	goto pause;

ret:
	return;
}

inline void SYSSpinlockUnlock(volatile u32 *locked)
{
    asm volatile("xrelease movl %1, %0" : "+m"(*locked) : "i"(0) : "memory");
}
