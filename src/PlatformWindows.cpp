String StupidStrToString(const wchar_t *wstr, void *(*allocFunc)(u64, int))
{
	u64 size = 0;
	for (const wchar_t *scan = wstr; *scan; ++scan)
		++size;
	char *buffer = (char *)allocFunc(size, 1);
	char *dstScan = buffer;
	for (const wchar_t *scan = wstr; *scan; ++scan)
		*dstScan++ = (char)*scan;
	return { size, buffer };
}

inline bool SYSFileExists(String filename)
{
	DWORD attrib = GetFileAttributesA(StringToCStr(filename, ThreadAllocator::Alloc));
	return attrib != INVALID_FILE_ATTRIBUTES && attrib != FILE_ATTRIBUTE_DIRECTORY;
}

inline bool SYSIsAbsolutePath(String path)
{
	if (path.size < 3) return false;
	if (!IsAlpha(path.data[0])) return false;
	if (path.data[1] != ':') return false;
	if (path.data[2] != '/' && path.data[2] != '\\') return false;
	return true;
}

String SYSExpandPathCompilerRelative(String relativePath)
{
	ASSERT(!SYSIsAbsolutePath(relativePath));
	String result;

	char *absolutePath = (char *)ThreadAllocator::Alloc(SYS_MAX_PATH, 1);
	result.data = absolutePath;

	DWORD written = GetModuleFileNameA(nullptr, absolutePath, SYS_MAX_PATH);
	int slashCounter = 0;
	for (char *scan = &absolutePath[written - 1]; scan > absolutePath; --scan)
	{
		if (*scan == '/' || *scan == '\\')
			++slashCounter;
		if (slashCounter == 2)
		{
			strncpy(scan + 1, relativePath.data, relativePath.size);
			scan[relativePath.size + 1] = 0;

			result.size = scan + relativePath.size + 1 - absolutePath;
			return result;
		}
	}
	CRASH;
}

String SYSExpandPathWorkingDirectoryRelative(String relativePath)
{
	ASSERT(!SYSIsAbsolutePath(relativePath));
	String result;

	char *absolutePath = (char *)ThreadAllocator::Alloc(SYS_MAX_PATH, 1);
	result.data = absolutePath;

	DWORD written = GetCurrentDirectory(MAX_PATH, absolutePath);
	absolutePath[written++] = '/';
	strncpy(absolutePath + written, relativePath.data, relativePath.size);
	absolutePath[written + relativePath.size] = 0;

	result.size = written + relativePath.size;

	return result;
}

FileHandle SYSOpenFileRead(String filename)
{
	const char *cFilename = StringToCStr(filename, ThreadAllocator::Alloc);
	HANDLE file = CreateFileA(
			cFilename,
			GENERIC_READ,
			FILE_SHARE_READ,
			nullptr,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	DWORD error = GetLastError();
	if (error == ERROR_SUCCESS && file != INVALID_HANDLE_VALUE)
		return file;

	String absolutePath = SYSExpandPathWorkingDirectoryRelative(filename);
	ASSERT(absolutePath.data[absolutePath.size] == 0);
	file = CreateFileA(
			absolutePath.data, // We know this string is null terminated.
			GENERIC_READ,
			FILE_SHARE_READ,
			nullptr,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	error = GetLastError();
	if (error == ERROR_SUCCESS && file != INVALID_HANDLE_VALUE)
		return file;

	// This exe's full name, up to second-to-last slash, plus filename.
	absolutePath = SYSExpandPathCompilerRelative(filename);
	ASSERT(absolutePath.data[absolutePath.size] == 0);
	file = CreateFileA(
			absolutePath.data, // We know this string is null terminated.
			GENERIC_READ,
			FILE_SHARE_READ,
			nullptr,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);

	return file;
}

FileHandle SYSOpenFileWrite(String filename)
{
	const char *cFilename = StringToCStr(filename, ThreadAllocator::Alloc);
	HANDLE file = CreateFileA(
			cFilename,
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	DWORD error = GetLastError();
	if ((error == ERROR_SUCCESS || error == ERROR_ALREADY_EXISTS) && file != INVALID_HANDLE_VALUE)
		return file;

	String absolutePath = SYSExpandPathWorkingDirectoryRelative(filename);
	ASSERT(absolutePath.data[absolutePath.size] == 0);
	file = CreateFileA(
			absolutePath.data,
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);

	return file;
}

inline void *SYSReserveMemory(void *addressHint, u64 size)
{
	void *result = VirtualAlloc(addressHint, size, MEM_RESERVE, PAGE_READWRITE);
	if (!result && addressHint)
		result = VirtualAlloc(nullptr, size, MEM_RESERVE, PAGE_READWRITE);
	return result;
}

inline void *SYSCommitMemory(void *address, u64 size)
{
	return VirtualAlloc(address, size, MEM_COMMIT, PAGE_READWRITE);
}

void SYSFreeMemory(void *address, u64 size)
{
	VirtualFree(address, size, MEM_DECOMMIT);
}

s64 SYSWriteFile(FileHandle file, void *buffer, s64 size)
{
	DWORD written;
	ASSERT((DWORD)size == size);
	WriteFile(file, buffer, (DWORD)size, &written, nullptr);
	s64 writtenS64 = written;
	return writtenS64;
}

s64 SYSReadFile(FileHandle file, void *buffer, u64 size)
{
	DWORD bytesRead;
	bool success = ReadFile(
			file,
			buffer,
			(DWORD)size,
			&bytesRead,
			nullptr
			);
	if (!success) return 0;
	return bytesRead;
}

u64 SYSGetFileSize(FileHandle file)
{
	DWORD fileSizeDword = GetFileSize(file, nullptr);
	return (u64)fileSizeDword;
}

void SYSReadEntireFile(FileHandle file, const char **fileBuffer, u64 *fileSize,
		void *(*allocFunc)(u64, int))
{
	if (file == INVALID_HANDLE_VALUE)
		*fileBuffer = nullptr;
	else
	{
		*fileSize = SYSGetFileSize(file);
		ASSERT(*fileSize);
		DWORD error = GetLastError();
		ASSERT(error == ERROR_SUCCESS);

		char *buffer = (char *)allocFunc(*fileSize, 1);
		DWORD bytesRead;
		bool success = ReadFile(
				file,
				buffer,
				(DWORD)*fileSize,
				&bytesRead,
				nullptr
				);
		ASSERT(success);
		ASSERT(bytesRead == *fileSize);
		*fileBuffer = buffer;
	}
}

bool SYSAreSameFile(FileHandle file1, FileHandle file2) {
	BY_HANDLE_FILE_INFORMATION info1 = { 0 };
	BY_HANDLE_FILE_INFORMATION info2 = { 0 };
	if (GetFileInformationByHandle(file1, &info1) &&
		GetFileInformationByHandle(file2, &info2))
	{
		return info1.nFileIndexHigh       == info2.nFileIndexHigh &&
			   info1.nFileIndexLow        == info2.nFileIndexLow &&
			   info1.dwVolumeSerialNumber == info2.dwVolumeSerialNumber;
	}
	return false;
}

void SYSCloseFile(FileHandle file)
{
	CloseHandle(file);
}

inline u64 SYSPerformanceCounter()
{
	LARGE_INTEGER largeInteger;
	QueryPerformanceCounter(&largeInteger);
	return largeInteger.QuadPart;
}

inline u64 SYSPerformanceFrequency()
{
	LARGE_INTEGER largeInteger;
	QueryPerformanceFrequency(&largeInteger);
	return largeInteger.QuadPart;
}

// Use the ExpandPath procedures instead of this.
#if 0
String SYSGetFullPathName(String filename)
{
	char filenameCStr[SYS_MAX_PATH];
	strncpy(filenameCStr, filename.data, filename.size);
	char *buffer = (char *)ThreadAllocator::Alloc(SYS_MAX_PATH, 1);
	String outputPath;
	outputPath.size = GetFullPathNameA(filenameCStr, SYS_MAX_PATH, buffer, nullptr);
	outputPath.data = buffer;
	return outputPath;
}
#endif

inline void *SYSAlloc(u64 size)
{
	return VirtualAlloc(0, size, MEM_COMMIT, PAGE_READWRITE);
}

inline void SYSFree(void *memory)
{
	VirtualFree(memory, 0, MEM_RELEASE);
}

void SYSCreateDirectory(String pathname)
{
	char pathnameCStr[SYS_MAX_PATH];
	strncpy(pathnameCStr, pathname.data, pathname.size);
	pathnameCStr[pathname.size] = 0;
	CreateDirectoryA(pathnameCStr, nullptr);
}

String msvcPath = {};
String windowsSDKPath = {};
String windowsSDKVersion = {};

void Win32FindVSAndWindowsSDK()
{
	PWSTR programFilesPathWstr;
	SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, 0, NULL, &programFilesPathWstr);
	String programFilesPath = StupidStrToString(programFilesPathWstr, ThreadAllocator::Alloc);

	String visualStudioPath = TPrintF("%S\\Microsoft Visual Studio", programFilesPath);
	{
		// Get anything starting with 20...
		String wildcard = TPrintF("%S\\20*", visualStudioPath);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, ThreadAllocator::Alloc), &foundData);
		const char *newestVersionStr = nullptr;
		int newestVersion = 0;
		if (findHandle != INVALID_HANDLE_VALUE) while (true)
		{
			int foundVersion = atoi(foundData.cFileName);
			if (foundVersion > newestVersion)
			{
				newestVersion = foundVersion;
				newestVersionStr = foundData.cFileName;
			}
			if (!FindNextFileA(findHandle, &foundData)) break;
		}
		visualStudioPath = TPrintF("%S\\%s", visualStudioPath, newestVersionStr);
	}

	// MSVC path
	{
		String buildToolsPath = TPrintF("%S\\BuildTools", visualStudioPath);
		String enterprisePath = TPrintF("%S\\Enterprise", visualStudioPath);
		String professionalPath = TPrintF("%S\\Professional", visualStudioPath);
		String communityPath = TPrintF("%S\\Community", visualStudioPath);
		if (GetFileAttributes(StringToCStr(buildToolsPath, ThreadAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = buildToolsPath;
		else if (GetFileAttributes(StringToCStr(enterprisePath, ThreadAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = enterprisePath;
		else if (GetFileAttributes(StringToCStr(professionalPath, ThreadAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = professionalPath;
		else if (GetFileAttributes(StringToCStr(communityPath, ThreadAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = communityPath;
		msvcPath = TPrintF("%S\\VC\\Tools\\MSVC", msvcPath);

		String wildcard = TStringConcat(msvcPath, "\\*"_s);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, ThreadAllocator::Alloc), &foundData);
		if (findHandle != INVALID_HANDLE_VALUE)
		{
			while (foundData.cFileName[0] == '.')
				FindNextFileA(findHandle, &foundData);
			msvcPath = TPrintF("%S\\%s", msvcPath, foundData.cFileName);
		}
	}

	windowsSDKPath = TPrintF("%S\\Windows Kits\\10", programFilesPath);
	windowsSDKVersion = {};
	{
		String wildcard = TPrintF("%S\\include\\10.*", windowsSDKPath);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, ThreadAllocator::Alloc), &foundData);
		s64 highestTuple[4] = {};
		const char *latestVersionName = nullptr;
		if (findHandle != INVALID_HANDLE_VALUE) while (true)
		{
			s64 tuple[4];
			int numDigits = 0;
			int foundNumbers = 0;
			// Parse tuple
			for (const char *scan = foundData.cFileName; ; ++scan)
			{
				if (*scan == '.' || *scan == 0)
				{
					ParseNumberResult parseResult = IntFromString({ (u64)numDigits, scan - numDigits });
					ASSERT(parseResult.error == PARSENUMBERRROR_OK);
					tuple[foundNumbers++] = parseResult.number;
					numDigits = 0;
				}
				else
					++numDigits;
				if (!*scan) break;
			}
			// Replace if greater
			for (int i = 0; i < 4; ++i)
			{
				if (tuple[i] < highestTuple[i])
					goto nextTuple;
				if (tuple[i] > highestTuple[i])
					break;
				// If same keep going
			}
			memcpy(highestTuple, tuple, sizeof(highestTuple));
			latestVersionName = foundData.cFileName;
nextTuple:
			if (!FindNextFileA(findHandle, &foundData)) break;
		}
		windowsSDKVersion.size = strlen(latestVersionName);
		char *buffer = (char *)LinearAllocator::Alloc(windowsSDKVersion.size, 1);
		memcpy(buffer, latestVersionName, windowsSDKVersion.size);
		windowsSDKVersion.data = buffer;
	}
}

void SYSRunAssembler(String outputPath, String outputFilename, String extraArguments, bool silent)
{
	if (!msvcPath.size)
		Win32FindVSAndWindowsSDK();

	// Run MASM
	String ml64CommandLine = TPrintF(
			"%S\\bin\\Hostx64\\x64\\ml64.exe " // msvcPath
			"%S " // input filename
			"/Fo %S " // output filename
			"/nologo /c "
			"/Zd "
			"/Zi "
			"/Fm "
			"%S " // extraArguments
			"%c",
			msvcPath,
			ChangeFilenameExtension(outputFilename, ".asm"_s),
			ChangeFilenameExtension(outputFilename, ".obj"_s),
			extraArguments,
			0
			);

	STARTUPINFO startupInfo = {};
	PROCESS_INFORMATION processInformation = {};
	startupInfo.cb = sizeof(STARTUPINFO);
	if (!CreateProcessA(
			NULL,
			(LPSTR)ml64CommandLine.data,
			NULL,
			NULL,
			false,
			silent ? CREATE_NO_WINDOW : 0,
			NULL,
			StringToCStr(outputPath, ThreadAllocator::Alloc),
			&startupInfo,
			&processInformation
			))
	{
		Print("Failed to call ml64.exe (%d)\n", GetLastError());
		PANIC;
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);

	DWORD exitCode;
	GetExitCodeProcess(processInformation.hProcess, &exitCode);
	if (exitCode != 0)
	{
		Print("ml64.exe returned an error (%d)\n", exitCode);
		exit(exitCode);
	}
	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);
}

void SYSRunLinker(String outputPath, String outputFilename, OutputType outputType,
		ArrayView<String> exportedSymbols, String extraArguments, bool silent)
{
	if (!msvcPath.size)
		Win32FindVSAndWindowsSDK();

	PROCESS_INFORMATION processInformation = {};

	String workingPath = SYSExpandPathWorkingDirectoryRelative({});
	ASSERT(workingPath.data[workingPath.size] == 0);

	String exports = {};
	char *exportsBuffer = (char *)t_threadMemPtr;
	char *scan = exportsBuffer;
	for (int i = 0; i < exportedSymbols.size; ++i) {
		memcpy(scan, " /EXPORT:", 9);
		scan += 9;
		String symbol = exportedSymbols[i];
		memcpy(scan, symbol.data, symbol.size);
		scan += symbol.size;
	}
	exports = { .size = (u64)(scan - exportsBuffer), .data = exportsBuffer };
	t_threadMemPtr = scan;

	// Executable/DLL
	if (outputType == OUTPUTTYPE_EXECUTABLE || outputType == OUTPUTTYPE_LIBRARY_DYNAMIC) {
		String linkCommandLine = TPrintF(
				"\"%S\\bin\\Hostx64\\x64\\link.exe\" " // msvcPath
				"%S " // obj name
				"/nologo "
				"/ignore:4216 " // Warning about exporting entry point
				"%S " // exports
				"/debug:full "
				"/entry:__WindowsMain "
				"/opt:ref "
				"/incremental:no "
				"/nodefaultlib "
				"/map "
#if !FINAL_BUILD
				"/dynamicbase:no "
#endif
				"%S " // extraArguments
				"/libpath:\"%S\" " // compiler bin path
				"/libpath:\"%S\" " // workingPath
				"/libpath:\"%S\\lib\\x64\" " // msvcPath
				"/libpath:\"%S\\lib\\%S\\ucrt\\x64\" " // windowsSDKPath, windowsSDKVersion
				"/libpath:\"%S\\lib\\%S\\um\\x64\" " // windowsSDKPath, windowsSDKVersion
				"%S " // /DLL /NOENTRY if should make a dll
				"/out:\"%S\"" // executable/DLL name
				"%c", // \0
				msvcPath,
				ChangeFilenameExtension(outputFilename, ".obj"_s),
				exports,
				extraArguments,
				SYSExpandPathCompilerRelative("bin"_s),
				workingPath,
				msvcPath,
				windowsSDKPath, windowsSDKVersion,
				windowsSDKPath, windowsSDKVersion,
				outputType == OUTPUTTYPE_LIBRARY_DYNAMIC ? "/DLL /NOENTRY"_s : ""_s,
				ChangeFilenameExtension(outputFilename,
						outputType == OUTPUTTYPE_LIBRARY_DYNAMIC ? ".dll"_s : ".exe"_s),
				0
				);

		STARTUPINFO startupInfo = {};
		startupInfo.cb = sizeof(STARTUPINFO);
		if (!CreateProcessA(
				NULL,
				(LPSTR)linkCommandLine.data,
				NULL,
				NULL,
				false,
				silent ? CREATE_NO_WINDOW : 0,
				NULL,
				StringToCStr(outputPath, ThreadAllocator::Alloc),
				&startupInfo,
				&processInformation
				)) {
			Print("Failed to call link.exe (%d)\n", GetLastError());
			PANIC;
		}
		DWORD exitCode;
		GetExitCodeProcess(processInformation.hProcess, &exitCode);
		if (exitCode != 0 && exitCode != ERROR_NO_MORE_ITEMS) { // ??? wtf this error
			Print("link.exe returned an error (%d)\n", exitCode);
			exit(exitCode);
		}
	}
	// Make .lib
	else if (outputType == OUTPUTTYPE_LIBRARY_STATIC) {
		String libCommandLine = TPrintF(
				"\"%S\\bin\\Hostx64\\x64\\lib.exe\" " // msvcPath
				"%S " // obj name
				"/nologo "
				"%S " // extraArguments
				"/libpath:\"%S\" " // compiler bin path
				"/libpath:\"%S\" " // workingPath
				"/libpath:\"%S\\lib\\x64\" " // msvcPath
				"/libpath:\"%S\\lib\\%S\\ucrt\\x64\" " // windowsSDKPath, windowsSDKVersion
				"/libpath:\"%S\\lib\\%S\\um\\x64\" " // windowsSDKPath, windowsSDKVersion
				"/out:\"%S\"" // lib name
				"%c", // \0
				msvcPath,
				ChangeFilenameExtension(outputFilename, ".obj"_s),
				extraArguments,
				SYSExpandPathCompilerRelative("bin"_s),
				workingPath,
				msvcPath,
				windowsSDKPath, windowsSDKVersion,
				windowsSDKPath, windowsSDKVersion,
				ChangeFilenameExtension(outputFilename, ".lib"_s),
				0
				);

		STARTUPINFO startupInfo = {};
		startupInfo.cb = sizeof(STARTUPINFO);
		if (!CreateProcessA(
				NULL,
				(LPSTR)libCommandLine.data,
				NULL,
				NULL,
				false,
				silent ? CREATE_NO_WINDOW : 0,
				NULL,
				StringToCStr(outputPath, ThreadAllocator::Alloc),
				&startupInfo,
				&processInformation
				)) {
			Print("Failed to call lib.exe (%d)\n", GetLastError());
			PANIC;
		}
		DWORD exitCode;
		GetExitCodeProcess(processInformation.hProcess, &exitCode);
		if (exitCode != 0 && exitCode != ERROR_NO_MORE_ITEMS) { // ??? wtf this error
			Print("lib.exe returned an error (%d)\n", exitCode);
			exit(exitCode);
		}
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);

	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);
}

inline void SYSSleep(int milliseconds)
{
	Sleep(milliseconds);
}

inline ThreadHandle SYSCreateThread(int (*start)(void *), void *args)
{
	return CreateThread(nullptr, 0, (DWORD (*)(void *))start, args, 0, nullptr);
}

inline void SYSWaitForThread(ThreadHandle thread)
{
	WaitForSingleObject(thread, INFINITE);
}

inline void SYSWaitForThreads(u64 count, ThreadHandle *threads)
{
	WaitForMultipleObjects((DWORD)count, threads, true, INFINITE);
}

inline void SYSExitThread(int errorCode)
{
	ExitThread(errorCode);
}

inline ThreadHandle SYSGetCurrentThread()
{
	return GetCurrentThread();
}

void SYSSetThreadDescription(ThreadHandle thread, String string) {
	char buffer[512];
	char *dst = buffer;
	const char *src = string.data;
	for (int i = 0; i < string.size; ++i)
	{
		*dst++ = *src++;
		*dst++ = 0;
	}
	*dst++ = 0;
	*dst++ = 0;
	SetThreadDescription(thread, (PCWSTR)buffer);
}

Fiber g_runningFibers[8] = { SYS_INVALID_FIBER_HANDLE };

inline Fiber SYSGetCurrentFiber()
{
	return GetCurrentFiber();
}

inline Fiber SYSCreateFiber(void (*start)(void *), void *args) {
	const u64 fiberStackSize = 1 * 1024 * 1024; // 1MB
	PROFILER_SCOPE("Creating a fiber");

	Fiber fiber = CreateFiber(fiberStackSize, start, args);
	ProfilerRegisterFiber(fiber);
	return fiber;
}

inline Fiber SYSConvertThreadToFiber() {
	Fiber fiber = ConvertThreadToFiber(nullptr);
	ProfilerRegisterFiber(fiber);
	return fiber;
}

inline void SYSDeleteFiber(Fiber fiber) {
	PROFILER_SCOPE("Deleting a fiber");
	for (int i = 0; i < ArrayCount(g_runningFibers); ++i)
		if (g_runningFibers[i] == fiber) {
			Print("CRASH: Trying to delete a running fiber (%llX)\n", fiber);
			PANIC;
		}

	ProfilerUnregisterFiber(fiber);
	DeleteFiber(fiber);
}

// Call this before a fiber ends
inline void SYSPrepareFiberForExit() {
	g_runningFibers[t_threadIndex] = 0;

	ProfilerUnregisterFiber(GetCurrentFiber());
}

inline void SYSSwitchToFiber(Fiber fiber) {
	ASSERT(fiber != SYS_INVALID_FIBER_HANDLE);

	for (int i = 0; i < ArrayCount(g_runningFibers); ++i)
		if (g_runningFibers[i] == fiber) {
			Print("CRASH: Trying to run fiber on two threads at once (%llX)\n", fiber);
			PANIC;
		}
	g_runningFibers[t_threadIndex] = fiber;

#if USE_PROFILER_API
	Fiber currentFiber = GetCurrentFiber();
	ProfilerBeginFiberSwitch(currentFiber, fiber);
#endif
	SwitchToFiber(fiber);
#if USE_PROFILER_API
	ProfilerEndFiberSwitch(currentFiber);
#endif
}

inline Mutex SYSCreateMutex() {
	return { CreateMutexA(nullptr, false, nullptr) };
}

inline void SYSMutexLock(Mutex mutex) {
	WaitForSingleObject(mutex.mutexHandle, INFINITE);
}

inline void SYSMutexUnlock(Mutex mutex) {
	ReleaseMutex(mutex.mutexHandle);
}

inline void SYSCreateRWLock(RWLock *lock) {
	InitializeSRWLock(lock);
}

inline void SYSLockForRead(RWLock *lock) {
	AcquireSRWLockShared(lock);
}

inline void SYSUnlockForRead(RWLock *lock) {
	ReleaseSRWLockShared(lock);
}

inline void SYSLockForWrite(RWLock *lock) {
	AcquireSRWLockExclusive(lock);
}

inline void SYSUnlockForWrite(RWLock *lock) {
	ReleaseSRWLockExclusive(lock);
}

inline void SYSCreateConditionVariable(ConditionVariable *conditionVar)
{
	InitializeConditionVariable(conditionVar);
}

inline void SYSSleepConditionVariableRead(ConditionVariable *conditionVar, RWLock *lock)
{
	SleepConditionVariableSRW(conditionVar, lock, INFINITE, CONDITION_VARIABLE_LOCKMODE_SHARED);
}

inline void SYSWakeAllConditionVariable(ConditionVariable *conditionVar)
{
	WakeAllConditionVariable(conditionVar);
}

inline s32 AtomicCompareExchange(volatile s32 *destination, s32 exchange, s32 comparand)
{
	return _InterlockedCompareExchange((volatile LONG *)destination, (LONG)exchange, (LONG)comparand);
}

inline s64 AtomicCompareExchange64(volatile s64 *destination, s64 exchange, s64 comparand)
{
	return _InterlockedCompareExchange64((volatile LONG64 *)destination, (LONG64)exchange, (LONG64)comparand);
}

inline s32 AtomicIncrementGetNew(volatile s32 *destination)
{
	return _InterlockedIncrement((LONG volatile *)destination);
}

extern "C" u64 SYSCallProcedureDynamically_windowscc(void *start, u64 argCount, void *argValues);

extern "C" u64 SYSCallProcedureDynamically_linuxcc(void *start, u64 gpArgCount, void *gpArgValues,
		u64 xmmArgCount, void *xmmArgValues, u64 stackArgCount, void *stackArgValues);

void *SYSLoadDynamicLibrary(String filename)
{
	const char *cstr = StringToCStr(filename, ThreadAllocator::Alloc);
	HMODULE result = LoadLibraryA(cstr);
	if (result) return result;

	// Look in working path
	String workingPathRelative = SYSExpandPathWorkingDirectoryRelative(filename);
	// this string is null terminated
	result = LoadLibraryA(workingPathRelative.data);
	if (result) return result;

	// Look in compiler path
	String compilerPathRelative = SYSExpandPathCompilerRelative(TStringConcat("bin/"_s, filename));
	// this string is null terminated
	result = LoadLibraryA(compilerPathRelative.data);
	return result;
}

inline void *SYSGetProcAddress(void *lib, const char *procedureCStr)
{
	return (void *)GetProcAddress((HMODULE)lib, procedureCStr);
}

inline int SYSGetProcessorCount()
{
	SYSTEM_INFO win32SystemInfo;
	GetSystemInfo(&win32SystemInfo);
	return win32SystemInfo.dwNumberOfProcessors;
}

inline bool SYSIsInputPipePresent()
{
	HANDLE stdIn = GetStdHandle(STD_INPUT_HANDLE);
	DWORD type = GetFileType(stdIn);
	return type == FILE_TYPE_PIPE || type == FILE_TYPE_DISK;
}
