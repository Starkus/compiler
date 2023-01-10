THREADLOCAL Fiber t_schedulerFiber;
THREADLOCAL Job t_runningJob = { .fiber = SYS_INVALID_FIBER_HANDLE };
THREADLOCAL YieldReason t_previousYieldReason;
THREADLOCAL YieldContext t_previousYieldContext;

#define DEFER_FIBER_CREATION 0
#define DEFER_FIBER_DELETION 0

inline void EnqueueReadyJob(Context *context, Job job)
{
	while (!MTQueueEnqueue(&context->readyJobs, job))
		while (context->readyJobs.head == context->readyJobs.tail)
			Sleep(0);
}

inline void RequestNewJob(Context *context, void (*proc)(void *), void *args)
{
#if DEFER_FIBER_CREATION
	if (t_threadIndex == g_threadIdxCreateFibers) {
		Job newJob = {};
		newJob.fiber = SYSCreateFiber(proc, args);
		EnqueueReadyJob(context, newJob);
	}
	else {
		JobRequest jobRequest = { proc, args };
		if (!MTQueueEnqueue(&context->jobsToCreate, jobRequest)) {
			Job newJob = {};
			newJob.fiber = SYSCreateFiber(proc, args);
			EnqueueReadyJob(context, newJob);
		}
	}
#else
	Job newJob = {};
	newJob.fiber = SYSCreateFiber(proc, args);
	EnqueueReadyJob(context, newJob);
#endif
}

void WakeUpAllByIndex(Context *context, YieldReason reason, u32 index)
{
	auto jobsWaiting = context->waitingJobsByReason[reason].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		Job *job = &jobsWaiting[i];
		if (job->context.index == index) {
			EnqueueReadyJob(context, *job);
			// Remove
			*job = jobsWaiting[--jobsWaiting->size];
		}
		else
			++i;
	}
}

void WakeUpAllByName(Context *context, YieldReason reason, String name)
{
	auto jobsWaiting = context->waitingJobsByReason[reason].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		Job *job = &jobsWaiting[i];
		if (StringEquals(job->context.identifier, name)) {
			EnqueueReadyJob(context, *job);
			// Remove
			*job = jobsWaiting[--jobsWaiting->size];
		}
		else
			++i;
	}
}

// Procedure to switch to a different job.
// We leave the information in thread local storage for the scheduler fiber.
// Call this when a job finishes too, the scheduler will delete the fiber and free resources.
// @Check: why does inlining this procedure lead to fibers running on multiple threads???
NOINLINE void SwitchJob(Context *context, YieldReason yieldReason, YieldContext yieldContext)
{
	ASSERT(t_runningJob.fiber == GetCurrentFiber());
	t_previousYieldReason = yieldReason;
	t_previousYieldContext = yieldContext;
	t_runningJob.context = yieldContext;
	SYSSwitchToFiber(t_schedulerFiber);
}

// Fiber that swaps jobs around.
// The reason to have a separate scheduler fiber is so we can queue the caller fiber right away,
// without risking another thread picking it up while it's still running on this one (since all this
// logic would be running in the fiber that wants to yield).
void SchedulerProc(Context *context)
{
loop:
	// Queue previous job now that its fiber is not running
	ASSERT(t_runningJob.fiber != (Fiber)0xDEADBEEFDEADBEEF);
	ASSERT(t_runningJob.fiber != t_schedulerFiber);
	if (t_runningJob.fiber != SYS_INVALID_FIBER_HANDLE) {
		switch (t_previousYieldReason) {
		case YIELDREASON_FAILED:
		{
			_InterlockedIncrement((LONG volatile *)&context->failedJobsCount);
			// Fall through
		}
		case YIELDREASON_DONE:
		{
#if DEFER_FIBER_DELETION
			// Try to schedule this fiber for deletion. If queue is full for some reason, just
			// delete now.
			if (!MTQueueEnqueue(&context->fibersToDelete, t_runningJob.fiber))
				SYSDeleteFiber(t_runningJob.fiber);
#else
			SYSDeleteFiber(t_runningJob.fiber);
#endif
		} break;
		case YIELDREASON_WAITING_FOR_STOP:
		case YIELDREASON_GLOBAL_VALUE_NOT_READY:
		{
			auto jobs = context->waitingJobsByReason[t_previousYieldReason].Get();
			*DynamicArrayAdd(&jobs) = t_runningJob;
		} break;
		case YIELDREASON_PROC_BODY_NOT_READY:
		case YIELDREASON_PROC_IR_NOT_READY:
		{
			// IMPORTANT! procedures should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[t_previousYieldReason].Get();
			*DynamicArrayAdd(&jobs) = t_runningJob;
			SYSUnlockForRead(&context->procedures.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_OVERLOAD:
		{
			// IMPORTANT! operatorOverloads should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_OVERLOAD].Get();
			*DynamicArrayAdd(&jobs) = t_runningJob;
			SYSUnlockForRead(&context->operatorOverloads.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_IDENTIFIER:
		{
			// IMPORTANT! tcGlobalNames should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
			*DynamicArrayAdd(&jobs) = t_runningJob;
			SYSUnlockForRead(&context->tcGlobalNames.rwLock);
		} break;
		case YIELDREASON_STATIC_DEF_NOT_READY:
		{
			// IMPORTANT! staticDefinitions should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_STATIC_DEF_NOT_READY].Get();
			*DynamicArrayAdd(&jobs) = t_runningJob;
			SYSUnlockForRead(&context->staticDefinitions.rwLock);
		} break;
		case YIELDREASON_TYPE_NOT_READY:
		{
			// IMPORTANT! waitingJobsByReason[YIELDREASON_TYPE_NOT_READY] should be locked
			// before calling SwitchJob!
			*DynamicArrayAdd(&context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].unsafe) =
				t_runningJob;
			SYSMutexUnlock(context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
		} break;
		default:
			ASSERTF(false, "Previous fiber is %llx, reason is %d", t_runningJob.fiber,
					t_previousYieldReason);
		}
	}
	t_runningJob = { .fiber = (Fiber)0xDEADBEEFDEADBEEF };

	SpinlockLock(&context->threadStatesLock);
	context->threadStates[t_threadIndex] = THREADSTATE_LOOKING_FOR_JOBS;
	SpinlockUnlock(&context->threadStatesLock);

#if DEFER_FIBER_DELETION
	// Task one of the threads on deleting fibers
	if (t_threadIndex == g_threadIdxDeleteFibers) {
		Fiber fiberToDelete;
		while (MTQueueDequeue(&context->fibersToDelete, &fiberToDelete))
			SYSDeleteFiber(fiberToDelete);
	}
#endif

	// Try to get next fiber to run
	Job nextJob = { .fiber = SYS_INVALID_FIBER_HANDLE };
	while (!context->done) {
#if DEFER_FIBER_CREATION
		if (t_threadIndex == g_threadIdxCreateFibers) {
			JobRequest jobToCreate;
			while (MTQueueDequeue(&context->jobsToCreate, &jobToCreate)) {
				Job newJob = {};
				newJob.fiber = SYSCreateFiber(jobToCreate.proc, jobToCreate.args);
				EnqueueReadyJob(context, newJob);
			}
		}
#endif

		Job dequeue;
		if (MTQueueDequeue(&context->readyJobs, &dequeue)) {
			nextJob = dequeue;
			break;
		}
		else {
			bool triggerStop = true;
			SpinlockLock(&context->threadStatesLock);
			// If any thread is running something, don't give up.
			for (int threadIdx = t_threadIndex; threadIdx < context->threadStates.size; ++threadIdx) {
				ThreadState state = context->threadStates[threadIdx];
				if (state == THREADSTATE_WORKING) {
					triggerStop = false;
					goto okWontStop;
				}
			}
			// We make threads give up in order. If any thread before this one hasn't given up,
			// don't give up.
			for (int threadIdx = 0; threadIdx < (int)t_threadIndex; ++threadIdx) {
				ThreadState state = context->threadStates[threadIdx];
				if (state != THREADSTATE_GIVING_UP) {
					triggerStop = false;
					goto okWontStop;
				}
			}
okWontStop:
			SpinlockUnlock(&context->threadStatesLock);

			if (triggerStop) {
				// Wake up a job waiting for dead stop
				{
					auto jobsWaiting = context->waitingJobsByReason[YIELDREASON_WAITING_FOR_STOP].Get();
					if (jobsWaiting->size) {
						Job *job = &jobsWaiting[0];
						nextJob = *job;
						DynamicArrayRemoveOrdered(&jobsWaiting, 0);

						goto switchFiber;
					}
				}

#if DEFER_FIBER_CREATION
				// Fallback, create the fiber on whatever thread and keep going
				JobRequest jobToCreate;
				if (MTQueueDequeue(&context->jobsToCreate, &jobToCreate)) {
					nextJob = { .fiber = SYSCreateFiber(jobToCreate.proc, jobToCreate.args) };
					goto switchFiber;
				}
#endif

				SpinlockLock(&context->threadStatesLock);
				context->threadStates[t_threadIndex] = THREADSTATE_GIVING_UP;

				// Check if every thread gave up, if so, we are done.
				for (int threadIdx = 0; threadIdx < context->threadStates.size; ++threadIdx) {
					if (context->threadStates[threadIdx] != THREADSTATE_GIVING_UP) {
						SpinlockUnlock(&context->threadStatesLock);
						goto stillBelieve;
					}
				}
				SpinlockUnlock(&context->threadStatesLock);

				// Give up!
				context->done = true;
			}
stillBelieve:
			// @Improve: This is silly but shouldn't happen too often...
			Sleep(0);
		}
	}

switchFiber:
	if (nextJob.fiber == SYS_INVALID_FIBER_HANDLE)
		return;

	SpinlockLock(&context->threadStatesLock);
	context->threadStates[t_threadIndex] = THREADSTATE_WORKING;

	// Restore hope to jobs
	for (int threadIdx = t_threadIndex; threadIdx < context->threadStates.size; ++threadIdx) {
		// If we triggered a deadstop everyone should have given up.
		if (context->threadStates[threadIdx] == THREADSTATE_GIVING_UP)
			context->threadStates[threadIdx] = THREADSTATE_LOOKING_FOR_JOBS;
	}
	SpinlockUnlock(&context->threadStatesLock);

	t_runningJob = nextJob;
	SYSSwitchToFiber(nextJob.fiber);

	goto loop;
}

// Procedure where worker threads begin executing
int WorkerThreadProc(void *args)
{
	ThreadArgs *threadArgs = (ThreadArgs *)args;
	Context *context = threadArgs->context;

	t_threadIndex = threadArgs->threadIndex;

	SYSSetThreadDescription(SYSGetCurrentThread(), SNPrintF(16, "Worker #%d", threadArgs->threadIndex));

	MemoryInitThread(512 * 1024 * 1024);

	t_schedulerFiber = SYSConvertThreadToFiber();

	SchedulerProc(context);

	SpinlockLock(&context->threadStatesLock);
	ASSERT(context->threadStates[t_threadIndex] == THREADSTATE_GIVING_UP);
	context->threadStates[t_threadIndex] = THREADSTATE_TERMINATED;
	SpinlockUnlock(&context->threadStatesLock);

	SYSPrepareFiberForExit();

	return 0;
}
