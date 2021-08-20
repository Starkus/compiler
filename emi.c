#include <stdint.h>

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

typedef union Register
{
	u8  u8_;
	u16 u16_;
	u32 u32_;
	u64 u64_;

	s8  s8_;
	s16 s16_;
	s32 s32_;
	s64 s64_;

	f32 f32_;
	f64 f64_;

	void *ptr;
} Register;
