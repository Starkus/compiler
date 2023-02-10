#include <stdio.h>

#if __clang__
#define IS_CLANG 1
#define IS_MSVC  0
#elif _MSC_VER
#define IS_CLANG 0
#define IS_MSVC  1
#else
// Probably gcc or the like, assuming clang should work...
#define IS_CLANG 1
#define IS_MSVC  0
#endif

#if DEBUG_BUILD
#define DEBUG_ONLY(...) __VA_ARGS__
#else
#define DEBUG_ONLY(...)
#endif

s64 Print(const char *format, ...);

#define U8_MAX 0xFF
#define U16_MAX 0xFFFF
#define U32_MAX 0xFFFFFFFF
#define U64_MAX 0xFFFFFFFFFFFFFFFF

#define S8_MIN  ((s8)0x80)
#define S16_MIN ((s16)0x8000)
#define S32_MIN ((s32)0x80000000)
#define S64_MIN ((s64)0x8000000000000000)

#define S8_MAX  ((s8)0x7F)
#define S16_MAX ((s16)0x7FFF)
#define S32_MAX ((s32)0x7FFFFFFF)
#define S64_MAX ((s64)0x7FFFFFFFFFFFFFFF)

#define CRASH do { *((int*)0xDEADBEEFDEADBEEF) = 1; ASSUME(0); } while (false)

#if DEBUG_BUILD
#define ASSERT(expr) do { \
	if (!(expr)) { \
		Print("!!!ASSERT FAILED!!!\n>   Expression: { %s }\n> %s:%d\n", #expr, __FILE__, __LINE__); \
		DEBUGBREAK; \
	} \
	ASSUME(expr); \
} while (false)
#define ASSERTF(expr, format, ...) do { \
	if (!(expr)) { \
		Print("!!!ASSERT FAILED!!!\n>   " format " { %s }\n> %s:%d\n" , ## __VA_ARGS__, \
				#expr, __FILE__, __LINE__); \
		DEBUGBREAK; \
	} \
	ASSUME(expr); \
} while (false)
#define ASSERTC(expr) do { \
	if constexpr (!(expr)) { \
		Print("!!!ASSERT FAILED!!!\n>   Expression: { %s }\n> %s:%d\n", #expr, __FILE__, __LINE__); \
		DEBUGBREAK; \
	} \
	ASSUME(expr); \
} while (false)
#else
#define ASSERT(expr) do { ASSUME(expr); } while (false)
#define ASSERTF(expr, ...) do { ASSUME(expr); } while (false)
#define ASSERTC(expr) do { ASSUME(expr); } while (false)
#endif

#define NOMANGLE extern "C"

#if IS_MSVC
#define UNUSED
#else
#define UNUSED __attribute__((unused))
#endif

#define ArrayCount(array) (sizeof(array) / sizeof(array[0]))
