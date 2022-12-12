THREADLOCAL Fiber t_schedulerFiber;
THREADLOCAL Fiber t_previousFiber = SYS_INVALID_FIBER_HANDLE;
THREADLOCAL TCYieldReason t_previousYieldReason;
THREADLOCAL TCYieldContext t_previousYieldContext;

#define DEFER_FIBER_CREATION 1
#define DEFER_FIBER_DELETION 1

inline void EnqueueReadyJob(Context *context, Fiber fiber)
{
	while (!MTQueueEnqueue(&context->readyJobs, fiber))
		while (context->readyJobs.head == context->readyJobs.tail)
			Sleep(0);
}

inline void RequestNewJob(Context *context, void (*proc)(void *), void *args)
{
#if DEFER_FIBER_CREATION
	if (t_threadIndex == g_threadIdxCreateFibers) {
		Fiber newFiber = SYSCreateFiber(proc, args);
		EnqueueReadyJob(context, newFiber);
	}
	else {
		JobRequest job = { proc, args };
		if (!MTQueueEnqueue(&context->jobsToCreate, job)) {
			Fiber newFiber = SYSCreateFiber(proc, args);
			EnqueueReadyJob(context, newFiber);
		}
	}
#else
	Fiber newFiber = SYSCreateFiber(proc, args);
	EnqueueReadyJob(context, newFiber);
#endif
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
			case TCYIELDREASON_FAILED:
			{
				_InterlockedIncrement((LONG volatile *)&context->failedJobsCount); \
				// Fall through
			}
			case TCYIELDREASON_DONE:
			{
#if DEFER_FIBER_DELETION
				// Try to schedule this fiber for deletion. If queue is full for some reason, just
				// delete now.
				if (!MTQueueEnqueue(&context->fibersToDelete, t_previousFiber))
					SYSDeleteFiber(t_previousFiber);
#else
				SYSDeleteFiber(t_previousFiber);
#endif
			} break;
			case TCYIELDREASON_WAITING_FOR_STOP:
			{
				auto jobs = context->jobsWaitingForDeadStop.Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case TCYIELDREASON_UNKNOWN_IDENTIFIER:
			{
				// IMPORTANT! tcGlobalNames should be locked before calling SwitchJob!
				auto jobs = context->jobsWaitingForIdentifier.Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->tcGlobalNames.rwLock);
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
				// IMPORTANT! staticDefinitions should be locked before calling SwitchJob!
				auto jobs = context->jobsWaitingForStaticDef.Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->staticDefinitions.rwLock);
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

#if DEFER_FIBER_DELETION
		// Task one of the threads on deleting fibers
		if (t_threadIndex == g_threadIdxDeleteFibers) {
			Fiber fiberToDelete;
			while (MTQueueDequeue(&context->fibersToDelete, &fiberToDelete))
				SYSDeleteFiber(fiberToDelete);
		}
#endif

		// Try to get next fiber to run
		Fiber nextFiber = SYS_INVALID_FIBER_HANDLE;
		while (true) {
#if DEFER_FIBER_CREATION
			if (t_threadIndex == g_threadIdxCreateFibers) {
				JobRequest jobToCreate;
				while (MTQueueDequeue(&context->jobsToCreate, &jobToCreate)) {
					Fiber newFiber = SYSCreateFiber(jobToCreate.proc, jobToCreate.args);
					EnqueueReadyJob(context, newFiber);
				}
			}
#endif

			Fiber dequeue;
			if (MTQueueDequeue(&context->readyJobs, &dequeue)) {
				nextFiber = dequeue;
				dequeue = (Fiber)0xFEEEFEEEFEEEFEEE;
				break;
			}
			else {
				s32 threadsDoingWork = _InterlockedDecrement((LONG volatile *)&context->threadsDoingWork);
				if (threadsDoingWork == 0) {
#define WAKE_UP_ONE(_waitingJobs) \
					{ \
						auto jobsWaiting = context-> _waitingJobs .Get(); \
						if (jobsWaiting->size) { \
							_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork); \
							TCJob *job = &(*jobsWaiting)[0]; \
							nextFiber = job->fiber; \
							DynamicArrayRemoveOrdered(&jobsWaiting, 0); \
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

#if DEFER_FIBER_CREATION
					// Fallback, create the fiber on whatever thread and keep going
					JobRequest jobToCreate;
					if (MTQueueDequeue(&context->jobsToCreate, &jobToCreate)) {
						_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork);
						nextFiber = SYSCreateFiber(jobToCreate.proc, jobToCreate.args);
						break;
					}
#endif

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

	MemoryInitThread(512 * 1024 * 1024);

	_InterlockedIncrement((LONG volatile *)&context->threadsDoingWork);

	t_schedulerFiber = SYSConvertThreadToFiber();
	//Print("Scheduler fiber for thread %d is %llX\n", t_threadIndex, t_schedulerFiber);

	SchedulerProc(context);

	SYSPrepareFiberForExit();

	return 0;
}
