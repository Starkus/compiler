THREADLOCAL Fiber t_schedulerFiber;
THREADLOCAL Fiber t_previousFiber = SYS_INVALID_FIBER_HANDLE;
THREADLOCAL TCYieldReason t_previousYieldReason;
THREADLOCAL TCYieldContext t_previousYieldContext;

inline void EnqueueReadyJob(Context *context, Fiber fiber)
{
	while (!MTQueueEnqueue(&context->readyJobs, fiber))
		while (context->readyJobs.head == context->readyJobs.tail)
			Sleep(0);
}

// Procedure to switch to a different job.
// We leave the information in thread local storage for the scheduler fiber.
// Call this when a job finishes too, the scheduler will delete the fiber and free resources.
// @Check: why does inlining this procedure lead to fibers running on multiple threads???
NOINLINE void SwitchJob(Context *context, TCYieldReason yieldReason, TCYieldContext yieldContext)
{
	t_previousFiber = GetCurrentFiber();
	t_previousYieldReason = yieldReason;
	t_previousYieldContext = yieldContext;
	SYSSwitchToFiber(t_schedulerFiber);
}

// Fiber that swaps jobs around.
// The reason to have a separate scheduler fiber is so we can queue the caller fiber right away,
// without risking another thread picking it up while it's still running on this one (since all this
// logic would be running in the fiber that wants to yield).
void SchedulerProc(Context *context)
{
	while (true) {
		// Queue previous job now that its fiber is not running
		ASSERT(t_previousFiber != (Fiber)0xDEADBEEFDEADBEEF);
		ASSERT(t_previousFiber != t_schedulerFiber);
		if (t_previousFiber != SYS_INVALID_FIBER_HANDLE) {
			TCJob job;
			job.fiber = t_previousFiber;
			job.context = t_previousYieldContext;
			switch (t_previousYieldReason) {
			case TCYIELDREASON_DONE:
			{
				SYSDeleteFiber(t_previousFiber);
			} break;
			case TCYIELDREASON_WAITING_FOR_STOP:
			{
				auto jobs = context->jobsWaitingForDeadStop.Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case TCYIELDREASON_UNKNOWN_IDENTIFIER:
			{
				// IMPORTANT! jobsWaitingForIdentifier should be locked before calling SwitchJob!
				*DynamicArrayAdd(&context->jobsWaitingForIdentifier.unsafe) = job;
				SYSMutexUnlock(context->jobsWaitingForIdentifier.lock);
			} break;
			case TCYIELDREASON_UNKNOWN_OVERLOAD:
			{
				auto jobs = context->jobsWaitingForOverload.Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case TCYIELDREASON_PROC_BODY_NOT_READY:
			{
				auto jobs = context->jobsWaitingForProcedure.Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case TCYIELDREASON_STATIC_DEF_NOT_READY:
			{
				// IMPORTANT! jobsWaitingForStaticDef should be locked before calling SwitchJob!
				*DynamicArrayAdd(&context->jobsWaitingForStaticDef.unsafe) = job;
				SYSMutexUnlock(context->jobsWaitingForStaticDef.lock);
			} break;
			case TCYIELDREASON_TYPE_NOT_READY:
			{
				// IMPORTANT! jobsWaitingForType should be locked before calling SwitchJob!
				*DynamicArrayAdd(&context->jobsWaitingForType.unsafe) = job;
				SYSMutexUnlock(context->jobsWaitingForType.lock);
			} break;
			default:
				ASSERTF(false, "Previous fiber is %llx, reason is %d", t_previousFiber,
						t_previousYieldReason);
			}
		}
		t_previousFiber = (Fiber)0xDEADBEEFDEADBEEF;

		// Try to get next fiber to run
		Fiber nextFiber = SYS_INVALID_FIBER_HANDLE;
		while (true) {
			Fiber *dequeue = MTQueueDequeue(&context->readyJobs);
			if (dequeue) {
				nextFiber = *dequeue;
				*dequeue = (Fiber)0xFEEEFEEEFEEEFEEE;
				break;
			}
			else {
				s32 threadsDoingWork = _InterlockedDecrement((LONG volatile *)&context->threadsDoingWork);
				if (threadsDoingWork == 0) {
#define WAKE_UP_ONE(_waitingJobs) \
					{ \
						auto jobsWaiting = context-> _waitingJobs .Get(); \
						if (jobsWaiting->size) { \
							TCJob *job = &(*jobsWaiting)[0]; \
							nextFiber = job->fiber; \
							\
							/* Remove */ \
							DynamicArrayRemoveOrdered(&jobsWaiting, 0); \
							\
							_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork); \
							break; \
						} \
					}
					WAKE_UP_ONE(jobsWaitingForDeadStop)
					WAKE_UP_ONE(jobsWaitingForIdentifier)
					WAKE_UP_ONE(jobsWaitingForOverload)
					WAKE_UP_ONE(jobsWaitingForProcedure)
					WAKE_UP_ONE(jobsWaitingForStaticDef)
					WAKE_UP_ONE(jobsWaitingForType)
#undef WAKE_UP_ONE

					// Give up!
					break;
				}
				// @Improve: This is silly but shouldn't happen too often...
				Sleep(0);
				_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork);
			}
		}

		if (nextFiber != SYS_INVALID_FIBER_HANDLE)
			SYSSwitchToFiber(nextFiber);
		else return;
	}
}

// Procedure where worker threads begin executing
int WorkerThreadProc(void *args)
{
	ThreadArgs *threadArgs = (ThreadArgs *)args;
	Context *context = threadArgs->context;

	t_threadIndex = threadArgs->threadIndex;

	MemoryInitThread(1 * 1024 * 1024);

	_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork);

	t_schedulerFiber = SYSConvertThreadToFiber();
	//Print("Scheduler fiber for thread %d is %llX\n", t_threadIndex, t_schedulerFiber);

	SchedulerProc(context);

	SYSPrepareFiberForExit();

	return 0;
}
