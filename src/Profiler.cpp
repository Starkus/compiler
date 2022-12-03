#if USE_PROFILER_API
#define PROFILER_COLOR(r,g,b) (PERFORMANCEAPI_MAKE_COLOR(r,g,b))
struct ProfilerScope {
	[[nodiscard]] ProfilerScope(const char *label, const char *context = nullptr,
			u32 color = PERFORMANCEAPI_DEFAULT_COLOR){
		performanceAPI.BeginEvent(label, context, color);
	}
	~ProfilerScope() {
		performanceAPI.EndEvent();
	}
};
inline void ProfilerBegin(const char *label, const char *context = nullptr,
			u32 color = PERFORMANCEAPI_DEFAULT_COLOR) {
	performanceAPI.BeginEvent(label, context, color);
}
inline void ProfilerEnd() {
	performanceAPI.EndEvent();
}
inline void ProfilerRegisterFiber(Fiber fiber) {
	performanceAPI.RegisterFiber((u64)fiber);
}
inline void ProfilerUnregisterFiber(Fiber fiber) {
	performanceAPI.UnregisterFiber((u64)fiber);
}
inline void ProfilerBeginFiberSwitch(Fiber currentFiber, Fiber nextFiber) {
	performanceAPI.BeginFiberSwitch((u64)currentFiber, (u64)nextFiber);
}
inline void ProfilerEndFiberSwitch(Fiber fiber) {
	performanceAPI.EndFiberSwitch((u64)fiber);
}
#else
#define PROFILER_COLOR(r,g,b) (0)
struct ProfilerScope {
	[[nodiscard]] ProfileScope(const char *label, const char *context = nullptr,
			u32 color = 0) {
	}
};
#define ProfilerBegin(...)
#define ProfilerEnd(...)
#define ProfilerRegisterFiber(...)
#define ProfilerUnregisterFiber(...)
#define ProfilerBeginFiberSwitch(...)
#define ProfilerEndFiberSwitch(...)
#endif
