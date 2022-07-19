#include <windows.h>
#include <strsafe.h>
#include <shlobj_core.h>
#include <Shlobj.h>

typedef HANDLE FileHandle;
#define SYS_INVALID_FILE_HANDLE INVALID_HANDLE_VALUE
#define SYS_MAX_PATH MAX_PATH
#define BREAK __debugbreak()
#define ASSUME(expr) __assume(expr)

String StupidStrToString(const wchar_t *wstr, void *(*allocFunc)(u64))
{
	s64 size = 0;
	for (const wchar_t *scan = wstr; *scan; ++scan)
		++size;
	char *buffer = (char *)allocFunc(size);
	char *dstScan = buffer;
	for (const wchar_t *scan = wstr; *scan; ++scan)
		*dstScan++ = (char)*scan;
	return { size, buffer };
}

inline bool SYSFileExists(const char *filename)
{
	DWORD attrib = GetFileAttributesA(filename);
	return attrib != INVALID_FILE_ATTRIBUTES && attrib != FILE_ATTRIBUTE_DIRECTORY;
}

String SYSExpandPathCompilerRelative(String relativePath)
{
	String result;

	char *absolutePath = (char *)PhaseAllocator::Alloc(SYS_MAX_PATH);
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
	String result;

	char *absolutePath = (char *)PhaseAllocator::Alloc(SYS_MAX_PATH);
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
	String absolutePath = SYSExpandPathWorkingDirectoryRelative(filename);

	HANDLE file = CreateFileA(
			absolutePath.data, // We know this string is null terminated.
			GENERIC_READ,
			FILE_SHARE_READ,
			nullptr,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);

	DWORD error = GetLastError();
	if (error != ERROR_SUCCESS || file == INVALID_HANDLE_VALUE)
	{
		// This exe's full name, up to second-to-last slash, plus filename.
		absolutePath = SYSExpandPathCompilerRelative(filename);

		file = CreateFileA(
				absolutePath.data, // We know this string is null terminated.
				GENERIC_READ,
				FILE_SHARE_READ,
				nullptr,
				OPEN_EXISTING,
				FILE_ATTRIBUTE_NORMAL,
				nullptr
				);
	}

	error = GetLastError();
	if (error != ERROR_SUCCESS || file == INVALID_HANDLE_VALUE)
	{
		Print("Failed to read file \"%S\"", filename);
		CRASH;
	}

	return file;
}

FileHandle SYSOpenFileWrite(String filename)
{
	String absolutePath = SYSExpandPathWorkingDirectoryRelative(filename);

	HANDLE result = CreateFileA(
			absolutePath.data,
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	return result;
}

s64 SYSWriteFile(FileHandle file, void *buffer, s64 size)
{
	DWORD written;
	ASSERT((DWORD)size == size);
	WriteFile(file, buffer, (DWORD)size, &written, nullptr);
	s64 writtenS64 = written;
	return writtenS64;
}

u64 SYSGetFileSize(FileHandle file)
{
	DWORD fileSizeDword = GetFileSize(file, nullptr);
	return (u64)fileSizeDword;
}

void SYSReadEntireFile(FileHandle file, char **fileBuffer, u64 *fileSize, void *(*allocFunc)(u64))
{
	if (file == INVALID_HANDLE_VALUE)
		*fileBuffer = nullptr;
	else
	{
		*fileSize = SYSGetFileSize(file);
		ASSERT(*fileSize);
		DWORD error = GetLastError();
		ASSERT(error == ERROR_SUCCESS);

		*fileBuffer = (char *)allocFunc(*fileSize);
		DWORD bytesRead;
		bool success = ReadFile(
				file,
				*fileBuffer,
				(DWORD)*fileSize,
				&bytesRead,
				nullptr
				);
		ASSERT(success);
		ASSERT(bytesRead == *fileSize);
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
	char *buffer = (char *)PhaseAllocator::Alloc(SYS_MAX_PATH);
	String outputPath;
	outputPath.size = GetFullPathNameA(filenameCStr, SYS_MAX_PATH, buffer, nullptr);
	outputPath.data = buffer;
	return outputPath;
}
#endif

void *SYSAlloc(u64 size)
{
	return VirtualAlloc(0, size, MEM_COMMIT, PAGE_READWRITE);
}

void SYSCreateDirectory(String pathname)
{
	char pathnameCStr[SYS_MAX_PATH];
	strncpy(pathnameCStr, pathname.data, pathname.size);
	CreateDirectoryA(pathnameCStr, nullptr);
}

void SYSRunAssemblerAndLinker(String outputPath, String extraAssemblerArguments,
		String extraLinkerArguments)
{
	// Run MASM
	PWSTR programFilesPathWstr;
	SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, 0, NULL, &programFilesPathWstr);
	String programFilesPath = StupidStrToString(programFilesPathWstr, PhaseAllocator::Alloc);

	String visualStudioPath = TPrintF("%S\\Microsoft Visual Studio", programFilesPath);
	{
		// Get anything starting with 20...
		String wildcard = TPrintF("%S\\20*", visualStudioPath);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, PhaseAllocator::Alloc), &foundData);
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

	String msvcPath = {};
	{
		String buildToolsPath = TPrintF("%S\\BuildTools", visualStudioPath);
		String enterprisePath = TPrintF("%S\\Enterprise", visualStudioPath);
		String professionalPath = TPrintF("%S\\Professional", visualStudioPath);
		String communityPath = TPrintF("%S\\Community", visualStudioPath);
		if (GetFileAttributes(StringToCStr(buildToolsPath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = buildToolsPath;
		else if (GetFileAttributes(StringToCStr(enterprisePath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = enterprisePath;
		else if (GetFileAttributes(StringToCStr(professionalPath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = professionalPath;
		else if (GetFileAttributes(StringToCStr(communityPath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = communityPath;
		msvcPath = TPrintF("%S\\VC\\Tools\\MSVC", msvcPath);

		String wildcard = StringConcat(msvcPath, "\\*"_s);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, PhaseAllocator::Alloc), &foundData);
		if (findHandle != INVALID_HANDLE_VALUE)
		{
			while (foundData.cFileName[0] == '.')
				FindNextFileA(findHandle, &foundData);
			msvcPath = TPrintF("%S\\%s", msvcPath, foundData.cFileName);
		}
	}

	String windowsSDKPath = TPrintF("%S\\Windows Kits\\10", programFilesPath);
	String windowsSDKVersion = {};
	{
		String wildcard = TPrintF("%S\\include\\10.*", windowsSDKPath);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, PhaseAllocator::Alloc), &foundData);
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
					tuple[foundNumbers++] = IntFromString({ numDigits, scan - numDigits });
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
		windowsSDKVersion = CStrToString(latestVersionName);
	}

	String commandLine = TPrintF(
			"%S\\bin\\Hostx64\\x64\\ml64.exe " // msvcPath
			"out.asm "
			"/nologo /c "
			"/Zd "
			"/Zi "
			"/Fm "
			"%S " // extraAssemblerArguments
			"%c",
			msvcPath,
			extraAssemblerArguments,
			0
			);

	STARTUPINFO startupInfo = {};
	PROCESS_INFORMATION processInformation = {};
	startupInfo.cb = sizeof(STARTUPINFO);
	if (!CreateProcessA(
			NULL,
			(LPSTR)commandLine.data,
			NULL,
			NULL,
			false,
			0,
			NULL,
			outputPath.data,
			&startupInfo,
			&processInformation
			))
	{
		Print("Failed to call ml64.exe (%d)\n", GetLastError());
		CRASH;
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);

	DWORD exitCode;
	GetExitCodeProcess(processInformation.hProcess, &exitCode);
	if (exitCode != 0)
	{
		Print("ml64.exe returned an error (%d)\n", exitCode);
		CRASH;
	}
	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);

	commandLine = TPrintF(
			"%S\\bin\\Hostx64\\x64\\link.exe " // msvcPath
			"out.obj "
			"/nologo "
			"kernel32.lib "
			"user32.lib "
			"gdi32.lib "
			"winmm.lib "
			"/nologo "
			"/debug:full "
			"/entry:__WindowsMain "
			"/opt:ref "
			"/incremental:no "
			"/dynamicbase:no "
			"%S " // extraLinkerArguments
			"/libpath:\"%S\\lib\\x64\" " // msvcPath
			"/libpath:\"%S\\lib\\%S\\ucrt\\x64\" " // windowsSDKPath, windowsSDKVersion
			"/libpath:\"%S\\lib\\%S\\um\\x64\" " // windowsSDKPath, windowsSDKVersion
			"/out:out.exe%c",
			msvcPath,
			extraLinkerArguments,
			msvcPath,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			0
			);

	startupInfo = {};
	processInformation = {};
	startupInfo.cb = sizeof(STARTUPINFO);
	if (!CreateProcessA(
			NULL,
			(LPSTR)commandLine.data,
			NULL,
			NULL,
			false,
			0,
			NULL,
			outputPath.data,
			&startupInfo,
			&processInformation
			))
	{
		Print("Failed to call link.exe (%d)\n", GetLastError());
		CRASH;
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);

	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);
}
