#include <windows.h>
#include <strsafe.h>
#include <shlobj_core.h>
#include <Shlobj.h>
#include <process.h>

#define IS_WINDOWS 1
#define IS_MSVC 1

typedef HANDLE FileHandle;
typedef HANDLE ThreadHandle;
typedef void *Fiber;
typedef SRWLOCK RWLock;
typedef CONDITION_VARIABLE ConditionVariable;
#define SYS_INVALID_FILE_HANDLE INVALID_HANDLE_VALUE
#define SYS_INVALID_THREAD_HANDLE INVALID_HANDLE_VALUE
#define SYS_INVALID_FIBER_HANDLE nullptr
#define SYS_MAX_PATH MAX_PATH
#define THREADLOCAL __declspec(thread)
#if DEBUG_BUILD
#define BREAK __debugbreak()
#else
#define BREAK
#endif
#define ASSUME(expr) __assume(expr)
#define PANIC do { BREAK; exit(1); } while(0)

struct Mutex {
	HANDLE mutexHandle;
};

