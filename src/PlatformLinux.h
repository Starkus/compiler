#include <memory.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <immintrin.h>
#include <linux/limits.h>
#include <linux/unistd.h>
#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>
#include <errno.h>
#include <ucontext.h>

#define IS_LINUX 1

typedef int FileHandle;
typedef pthread_t ThreadHandle;
typedef pthread_mutex_t Mutex;
typedef pthread_rwlock_t RWLock;
typedef u32 Fiber;

#define SYS_INVALID_FILE_HANDLE ((s64)-1)
#define SYS_INVALID_THREAD_HANDLE ((pthread_t)-1)
#define SYS_INVALID_FIBER_HANDLE (0xFFFFFFFF)
#define SYS_MAX_PATH PATH_MAX

#define BREAK asm("int $3")
#if DEBUG_BUILD
#define DEBUGBREAK BREAK
#else
#define DEBUGBREAK
#endif

#define PANIC do { DEBUGBREAK; exit(1); } while(0)

#define ASSUME(expr) do { __builtin_assume(expr); /*(void)(expr);*/ } while(0)
