struct SourceLocation
{
	u32 fileIdx;
	u32 character;
};

enum ThreadState
{
	THREADSTATE_WORKING,
	THREADSTATE_LOOKING_FOR_JOBS,
	THREADSTATE_GIVING_UP,
	THREADSTATE_TERMINATED
};

enum YieldReason : u32
{
	// Reasons without an associated list of waiting jobs are negative
	YIELDREASON_DONE	 = (u32)-3,
	YIELDREASON_FAILED = (u32)-2,
	YIELDREASON_READY	 = (u32)-1,

	YIELDREASON_UNKNOWN_IDENTIFIER = 0,
	YIELDREASON_UNKNOWN_OVERLOAD,
	YIELDREASON_STATIC_DEF_NOT_READY,
	YIELDREASON_PROC_BODY_NOT_READY,
	YIELDREASON_PROC_IR_NOT_READY,
	YIELDREASON_TYPE_NOT_READY,
	YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED,
	YIELDREASON_NEED_DYNAMIC_LIBRARY,
	// WAITING_FOR_STOP jobs want to wait until no jobs are running to make a decision.
	// As of time of write, only #defined does this to determine if something isn't defined anywhere
	// before continuing.
	YIELDREASON_WAITING_FOR_STOP,

	YIELDREASON_Count
};

struct YieldContext
{
	SourceLocation loc;
	union {
		String identifier;
		u32 index;
		struct {
			u32 op;
			u32 leftTypeIdx;
			u32 rightTypeIdx;
		} overload;
	};
};

enum JobType
{
	JOBTYPE_INVALID = 0,
	JOBTYPE_PARSE,
	JOBTYPE_TYPE_CHECK,
	JOBTYPE_CODEGEN
};
enum JobState
{
	JOBSTATE_INIT,
	JOBSTATE_RUNNING,
	JOBSTATE_SUSPENDED,
	JOBSTATE_FINISHED
};
struct Job
{
	void (*startProcedure)(u32, void *);
	void *args;
	Fiber fiber;
	JobType type;
	JobState state;

#if DEBUG_BUILD
	SourceLocation loc;
	String description;
#endif

	// Some data about why the job yielded execution.
	YieldContext context;
};

void SwitchJob(JobContext *context, YieldReason yieldReason, YieldContext yieldContext);
u32 RequestNewJob(Context *context, JobType type, void (*proc)(u32, void *), void *args);
