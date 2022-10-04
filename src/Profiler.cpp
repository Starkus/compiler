#if USE_PROFILER_API
#define PROFILER_COLOR(r,g,b) (PERFORMANCEAPI_MAKE_COLOR(r,g,b))
struct ProfileScope {
	[[nodiscard]] ProfileScope(const char *label, const char *context = nullptr,
			u32 color = PERFORMANCEAPI_DEFAULT_COLOR){
		performanceAPI.BeginEvent(label, context, color);
	}
	~ProfileScope() {
		performanceAPI.EndEvent();
	}
};
inline void ProfileBegin(const char *label, const char *context = nullptr,
			u32 color = PERFORMANCEAPI_DEFAULT_COLOR) {
	performanceAPI.BeginEvent(label, context, color);
}
inline void ProfileEnd() {
	performanceAPI.EndEvent();
}
#else
#define PROFILER_COLOR(r,g,b) (0)
struct ProfileScope {
	[[nodiscard]] ProfileScope(const char *label, const char *context = nullptr,
			u32 color = 0) {
	}
};
#define ProfileBegin(...)
#define ProfileEnd(...)
#endif
