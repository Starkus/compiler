#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <strsafe.h>
#include <shlobj_core.h>
#include <Shlobj.h>
#include <process.h>
#include <intrin.h>

// I hate this OS
#undef near
#undef far

typedef HANDLE FileHandle;
typedef HANDLE ThreadHandle;
typedef void *Fiber;
typedef SRWLOCK RWLock;
typedef CONDITION_VARIABLE ConditionVariable;
#define SYS_INVALID_FILE_HANDLE INVALID_HANDLE_VALUE
#define SYS_INVALID_THREAD_HANDLE INVALID_HANDLE_VALUE
#define SYS_INVALID_FIBER_HANDLE nullptr
#define SYS_MAX_PATH MAX_PATH
#define NOINLINE __declspec(noinline)
#define BREAK __debugbreak()
#if DEBUG_BUILD
#define DEBUGBREAK BREAK
#else
#define DEBUGBREAK
#endif
#define ASSUME(expr) __assume(expr)
#define PANIC do { DEBUGBREAK; exit(1); } while(0)

#if IS_CLANG
// We make thread local variables 'volatile' on Clang since there's no fiber-safe TLS option.
#define THREADLOCAL volatile __declspec(thread)
#else
#define THREADLOCAL __declspec(thread)
#endif

struct Mutex {
	HANDLE mutexHandle;
};

