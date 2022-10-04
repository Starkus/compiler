#include <memory.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <immintrin.h>
#include <linux/limits.h>
#include <linux/unistd.h>
#include <stdio.h>

#define IS_LINUX 1

typedef int FileHandle;

#define SYS_INVALID_FILE_HANDLE ((s64)-1)
#define SYS_MAX_PATH PATH_MAX

#if DEBUG_BUILD
#define BREAK asm("int $3")
#else
#define BREAK
#endif

#define PANIC do { BREAK; exit(1); } while(0)

#define ASSUME(expr) do { __builtin_assume(expr); (void)(expr); } while(0)

struct Mutex
{
	mutex_t mutexHandle;
};
