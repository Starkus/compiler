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

typedef uint8_t* ptr;
typedef int8_t strct;
typedef int8_t arr;

#pragma pack(push, 1) // We'll pack these manually
typedef struct ProgramTypeInfoInteger
{
	u8 typeCategory;
	s32 isSigned;
	s64 size;
} ProgramTypeInfoInteger;
typedef struct ProgramTypeInfoFloating
{
	u8 typeCategory;
	s64 size;
} ProgramTypeInfoFloating;
typedef struct ProgramStructMemberInfo
{
	s64 nameSize;
	u8 *nameData;
	void *typeInfo;
	u64 offset;
} ProgramStructMemberInfo;
typedef struct ProgramTypeInfoStruct
{
	u8 typeCategory;
	s64 nameSize;
	u8 *nameData;
	s32 isUnion;
	s64 memberCount;
	void *memberData;
	u64 size;
} ProgramTypeInfoStruct;
typedef struct ProgramTypeInfoEnum
{
	u8 typeCategory;
	s64 nameSize;
	u8 *nameData;
	void *typeInfo;
} ProgramTypeInfoEnum;
typedef struct ProgramTypeInfoPointer
{
	u8 typeCategory;
	void *typeInfo;
} ProgramTypeInfoPointer;
typedef struct ProgramTypeInfoArray
{
	u8 typeCategory;
	u64 count;
	void *typeInfo;
} ProgramTypeInfoArray;
#pragma pack(pop)

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

	ptr ptr_;
	strct strct_;
	arr arr_;
} Register;

typedef struct String
{
	u64 size;
	u8 *data;
} String;

inline Register FromU64(u64 value)
{
	Register r;
	r.u64_ = value;
	return r;
}

inline Register FromS64(s64 value)
{
	Register r;
	r.s64_ = value;
	return r;
}

Register Main();
int main(int argc, char **argv)
{
	return Main().s64_;
}
