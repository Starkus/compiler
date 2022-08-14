#include <math.h>

#if _MSC_VER
#include <intrin.h>
#endif

const f32 PI = 3.1415926535897932384626433832795f;
const f32 HALFPI = 1.5707963267948966192313216916398f;
const f32 PI2 = 6.283185307179586476925286766559f;
const f64 PI_64 = 3.1415926535897932384626433832795;
const f64 HALFPI_64 = 1.5707963267948966192313216916398;
const f64 PI2_64 = 6.283185307179586476925286766559;

inline u8 Nlz(u32 x)
{
#if _MSC_VER
	unsigned long i;
	if (_BitScanReverse(&i, x))
		return 31 - (u8)i;
	return 32;
#else
	return __builtin_clz(x);
#endif
}

inline u8 Ntz(u32 n)
{
#if _MSC_VER
	unsigned long i;
	_BitScanForward(&i, n);
	return (u8)i;
#else
	return __builtin_ctz(n);
#endif
}

inline u32 CountOnes(u32 n)
{
#if _MSC_VER
	return __popcnt(n);
#else
	return __builtin_popcount(n);
#endif
}

inline u32 NextPowerOf2(u32 n)
{
	return 0x80000000 >> (Nlz(n - 1) - 1);
}

inline u32 LastPowerOf2(u32 n)
{
	return 0x80000000 >> Nlz(n);
}

inline u8 Nlz64(u64 x)
{
#if _MSC_VER
	unsigned long i;
	if (_BitScanReverse64(&i, x))
		return 63 - (u8)i;
	return 64;
#else
	return __builtin_clzll(x);
#endif
}

inline  u8 Ntz64(u64 n)
{
#if _MSC_VER
	unsigned long i;
	_BitScanForward64(&i, n);
	return (u8)i;
#else
	return __builtin_ctzll(n);
#endif
}

inline u64 CountOnes64(u64 n)
{
#if _MSC_VER
	return __popcnt64(n);
#else
	return __builtin_popcountll(n);
#endif
}

inline u64 NextPowerOf264(u64 n)
{
	return 0x8000000000000000 >> (Nlz64(n - 1) - 1);
}

inline u64 LastPowerOf264(u64 n)
{
	return 0x8000000000000000 >> Nlz64(n);
}

inline bool IsPowerOf2(u32 n)
{
	return CountOnes(n) == 1;
}

inline bool IsPowerOf264(u64 n)
{
	return CountOnes64(n) == 1;
}

inline s64 AlignTo(s64 n, s64 alignment)
{
	ASSERT(IsPowerOf264(alignment));
	s64 mask = alignment - 1;
	if ((n & mask) != 0)
		n = (n & ~mask) + alignment;
	return n;
}

inline f32 Pow(f32 n, f32 e)
{
	return powf(n, e);
}

inline f32 Abs(f32 n)
{
	return fabsf(n);
}

inline f32 Floor(f32 n)
{
	return floorf(n);
}

inline f32 Round(f32 n)
{
	return roundf(n);
}

inline f32 Ceil(f32 n)
{
	return ceilf(n);
}

#define Min(a, b) (a > b ? b : a)

#define Max(a, b) (a > b ? a : b)

inline f32 Fmod(f32 n, f32 d)
{
	return fmodf(n, d);
}

inline f32 Sign(f32 n)
{
	return f32(n > 0) - f32(n < 0);
}

inline f32 Sin(f32 theta)
{
	return sinf(theta);
}

inline f32 Cos(f32 theta)
{
	return cosf(theta);
}

inline f32 Tan(f32 theta)
{
	return tanf(theta);
}

inline f32 Asin(f32 n)
{
	return asinf(n);
}

inline f32 Acos(f32 n)
{
	return acosf(n);
}

inline f32 Atan2(f32 a, f32 b)
{
	// Note to myself because I keep forgetting:
	// If called with x, y this will tell you the signed angle difference with +Y.
	// You can swap around/negate the components to get an angle with respect to a different axis.
	return atan2f(a, b);
}

inline f64 Fmod64(f64 n, f64 d)
{
	return fmod(n, d);
}

inline f64 Sin64(f64 theta)
{
	return sin(theta);
}

inline f64 Cos64(f64 theta)
{
	return cos(theta);
}

inline f64 Tan64(f64 theta)
{
	return tan(theta);
}

inline f32 Sqrt(f32 n)
{
	return sqrtf(n);
}

#if _MSC_VER
// Retarded compiler
inline constexpr u32 CountOnesConstexpr(u32 n)
{
	u32 count = 0;
	u32 bit = 1;
	for (int i = 0; i < 32; ++i)
	{
		if (n & bit) ++count;
		n <<= 1;
	}
	return count;
}

inline constexpr u64 CountOnes64Constexpr(u64 n)
{
	u64 count = 0;
	u64 bit = 1;
	for (int i = 0; i < 64; ++i)
	{
		if (n & bit) ++count;
		n <<= 1;
	}
	return count;
}

inline constexpr u8 NlzConstexpr(u32 n)
{
	u8 count = 0;
	for (int i = 0; i < 32; ++i)
	{
		if (n & 0x80000000)
			break;
		n <<= 1;
		++count;
	}
	return count;
}

inline constexpr u8 NtzConstexpr(u32 n)
{
	u8 count = 0;
	for (int i = 0; i < 32; ++i)
	{
		if (n & 1)
			break;
		n >>= 1;
		++count;
	}
	return count;
}

inline constexpr u8 Nlz64Constexpr(u64 n)
{
	u8 count = 0;
	for (int i = 0; i < 64; ++i)
	{
		if (n & 0x8000000000000000)
			break;
		n <<= 1;
		++count;
	}
	return count;
}

inline constexpr u8 Ntz64Constexpr(u64 n)
{
	u8 count = 0;
	for (int i = 0; i < 64; ++i)
	{
		if (n & 1)
			break;
		n >>= 1;
		++count;
	}
	return count;
}
#endif
