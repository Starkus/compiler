#include <memory.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <immintrin.h>
#include <linux/limits.h>
#include <linux/unistd.h>
#include <stdio.h>

typedef int FileHandle;

#define SYS_INVALID_FILE_HANDLE ((s64)-1)
#define SYS_MAX_PATH PATH_MAX

#define BREAK asm("int $3")
#define ASSUME(expr) do { __builtin_assume(expr); (void)(expr); } while(0)

String SYSExpandPathCompilerRelative(String relativePath)
{
	char relativeCStr[SYS_MAX_PATH];
	strncpy(relativeCStr, relativePath.data, relativePath.size);
	relativeCStr[relativePath.size] = 0;
	char *absolutePath = (char *)PhaseAllocator::Alloc(SYS_MAX_PATH);
	realpath(relativeCStr, absolutePath);

	String result;
	result.data = absolutePath;
	result.size = strlen(absolutePath);
	return result;
}

String SYSExpandPathWorkingDirectoryRelative(String relativePath)
{
	String result;

	char *absolutePath = (char *)PhaseAllocator::Alloc(SYS_MAX_PATH);
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

void SYSReadEntireFile(FileHandle file, char **fileBuffer, u64 *fileSize, void *(*allocFunc)(u64))
{
	if (file == SYS_INVALID_FILE_HANDLE)
		*fileBuffer = nullptr;
	else
	{
		*fileSize = SYSGetFileSize(file);
		ASSERT(*fileSize);
		*fileBuffer = (char *)allocFunc(*fileSize);
		u64 bytesRead = read(file, *fileBuffer, *fileSize);
		ASSERT(bytesRead == *fileSize);
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

void SYSRunAssemblerAndLinker(String outputPath, String extraAssemblerArguments,
		String extraLinkerArguments)
{
#if 1
	String nasmCmd = TPrintF("nasm -f elf64 -F dwarf -g %S%c",
			SYSExpandPathWorkingDirectoryRelative("output/out.asm"_s), 0);
	int status = system(nasmCmd.data);
	if (status)
	{
		Print("Error executing nasm!\n");
		exit(status);
	}

	String ldCmd = TPrintF("ld %S -o %S -e __LinuxMain%c",
		SYSExpandPathWorkingDirectoryRelative("output/out.o"_s),
		SYSExpandPathWorkingDirectoryRelative("output/out"_s), 0);
	status = system(ldCmd.data);
	if (status)
	{
		Print("Error executing ld!\n");
		exit(status);
	}
#else
	int status;
	pid_t pid = fork();

	if (pid == 0)
	{
		const char *nasmArgs[] =
		{
			"nasm",
			"-f elf64",
			"-F dwarf",
			"-g",
			SYSExpandPathWorkingDirectoryRelative("output/out.asm"_s).data,
			0
		};
		if (execvp("nasm", (char **)nasmArgs) == -1)
		{
			Print("Error executing nasm!\n");
			exit(1);
		}
	}
	else
	{
		while (waitpid(pid, &status, WNOHANG) == 0)
			sleep(1);

		String outputArg = TPrintF("-o %S%c", SYSExpandPathWorkingDirectoryRelative("output/out"_s),
				0);
		const char *ldArgs[] =
		{
			"ld",
			SYSExpandPathWorkingDirectoryRelative("output/out.o"_s).data,
			outputArg.data,
			"-e __LinuxMain",
			0
		};
		if (execvp("ld", (char **)ldArgs) == -1)
		{
			Print("Error executing ld!\n");
			exit(1);
		}
	}
#endif
}
