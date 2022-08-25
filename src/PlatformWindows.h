#include <windows.h>
#include <strsafe.h>
#include <shlobj_core.h>
#include <Shlobj.h>
#include <process.h>

typedef HANDLE FileHandle;
#define SYS_INVALID_FILE_HANDLE INVALID_HANDLE_VALUE
#define SYS_MAX_PATH MAX_PATH
#if DEBUG_BUILD
#define BREAK __debugbreak()
#else
#define BREAK
#endif
#define ASSUME(expr) __assume(expr)
#define PANIC do { BREAK; exit(1); } while(0)

struct Mutex
{
	HANDLE mutexHandle;
};

