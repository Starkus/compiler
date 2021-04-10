#include <stdint.h>
#include <stdio.h>

// I hate this programming language
#undef near
#undef far

#if DEBUG_BUILD
#define DEBUG_ONLY(...) __VA_ARGS__
#else
#define DEBUG_ONLY(...)
#endif

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float f32;
typedef double f64;

#define U8_MAX 0xFF
#define U16_MAX 0xFFFF
#define U32_MAX 0xFFFFFFFF
#define U64_MAX 0xFFFFFFFFFFFFFFFF

#define S8_MIN ((i8)0xFF)
#define S16_MIN ((i16)0xFFFF)
#define S32_MIN ((i32)0xFFFFFFFF)
#define S64_MIN ((i64)0xFFFFFFFFFFFFFFFF)

#define S8_MAX ((i8)0x7F)
#define S16_MAX ((i16)0x7FFF)
#define S32_MAX ((i32)0x7FFFFFFF)
#define S64_MAX ((i64)0x7FFFFFFFFFFFFFFF)

#define CRASH do { *((int*)0) = 1; } while (false)

#if DEBUG_BUILD
#define ASSERT(expr) do { if (!(expr)) { Log("!!!ASSERT FAILED!!!\n>   Expression: { %s }\n>   %s:%d\n", #expr, __FILE__, __LINE__); __debugbreak(); } } while (false)
#else
#define ASSERT(expr)
#endif

#define NOMANGLE extern "C"

#define ArrayCount(array) (sizeof(array) / sizeof(array[0]))
