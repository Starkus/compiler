union CTRegister
{
	u8  asU8;
	u16 asU16;
	u32 asU32;
	u64 asU64;
	s8  asS8;
	s16 asS16;
	s32 asS32;
	s64 asS64;
	f32 asF32;
	f64 asF64;
	CTRegister *asPtr;
};

ArrayView<const CTRegister *> CTRunProcedure(Context *context, u32 procedureIdx,
		ArrayView<CTRegister *> parameters);
