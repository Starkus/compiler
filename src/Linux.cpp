#include <memory.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <immintrin.h>
#include <linux/limits.h>

typedef int FileHandle;

#define SYS_INVALID_FILE_HANDLE ((s64)-1)
#define SYS_MAX_PATH PATH_MAX

#define BREAK asm("int $3")
#define ASSUME(expr) do { __builtin_assume(expr); (void)(expr); } while(0)
//#define ASSUME(expr)

FileHandle SYSOpenFileRead(String filename)
{
	char fullname[PATH_MAX];
	ASSERT(getcwd(fullname, SYS_MAX_PATH));
	s64 written = strlen(fullname);
	fullname[written++] = '/';
	strncpy(fullname + written, filename.data, filename.size);
	fullname[written + filename.size] = 0;
	return open(fullname, O_RDONLY);
}

FileHandle SYSOpenFileWrite(String filename)
{
	char fullname[PATH_MAX];
	ASSERT(getcwd(fullname, SYS_MAX_PATH));
	s64 written = strlen(fullname);
	fullname[written++] = '/';
	strncpy(fullname + written, filename.data, filename.size);
	fullname[written + filename.size] = 0;
	return open(fullname, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
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

void SYSReadEntireFile(FileHandle file, u8 **fileBuffer, u64 *fileSize, void *(*allocFunc)(u64))
{
	if (file == SYS_INVALID_FILE_HANDLE)
		*fileBuffer = nullptr;
	else
	{
		*fileSize = SYSGetFileSize(file);
		ASSERT(*fileSize);
		*fileBuffer = (u8 *)allocFunc(*fileSize);
		u64 bytesRead = read( file, *fileBuffer, *fileSize);
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

String SYSGetFullPathName(String filename)
{
	char filenameCStr[SYS_MAX_PATH];
	strncpy(filenameCStr, filename.data, filename.size);
	filenameCStr[filename.size] = 0;
	char *buffer = (char *)PhaseAllocator::Alloc(SYS_MAX_PATH);
	realpath(filenameCStr, buffer);

	String outputPath;
	outputPath.data = buffer;
	outputPath.size = strlen(buffer);
	return outputPath;
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
