THREADLOCAL u32 t_currentJobIdx;
THREADLOCAL Fiber t_fiberToDelete = SYS_INVALID_FIBER_HANDLE;

inline Job *GetCurrentJob()
{
	return &g_context->jobs.unsafe[t_currentJobIdx];
}

inline void EnqueueReadyJob(u32 jobIdx)
{
	while (!MTQueueEnqueue(&g_context->readyJobs, jobIdx)) {
		// Lockless sleep while queue is full
		while (g_context->readyJobs.head == g_context->readyJobs.tail)
			SYSSleep(0);
	}
}

u32 RequestNewJob(JobType type, void (*proc)(void *), void *args)
{
	SpinlockLock(&g_context->jobs.lock);
	u32 newJobIdx = (u32)g_context->jobs.unsafe.count;
	Job *newJob = BucketArrayAdd(&g_context->jobs.unsafe);
	SpinlockUnlock(&g_context->jobs.lock);
	*newJob = {
		.startProcedure = proc,
		.args = args,
		.type = type,
		.state = JOBSTATE_INIT };

	EnqueueReadyJob(newJobIdx);

	return newJobIdx;
}

void WakeUpAllByIndex(YieldReason reason, u32 index)
{
	auto jobsWaiting = g_context->waitingJobsByReason[reason].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		u32 jobIdx = jobsWaiting[i];
		const Job *job = &g_context->jobs.unsafe[jobIdx];
		if (job->yieldContext.index == index) {
			EnqueueReadyJob(jobIdx);
			// Remove
			DynamicArraySwapRemove(&jobsWaiting, i);
		}
		else
			++i;
	}
}

void WakeUpAllByName(YieldReason reason, String name)
{
	auto jobsWaiting = g_context->waitingJobsByReason[reason].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		u32 jobIdx = jobsWaiting[i];
		const Job *job = &g_context->jobs.unsafe[jobIdx];
		if (StringEquals(job->yieldContext.identifier, name)) {
			EnqueueReadyJob(jobIdx);
			// Remove
			DynamicArraySwapRemove(&jobsWaiting, i);
		}
		else
			++i;
	}
}

struct SchedulerArgs
{
	u32 previousJobIdx;
	YieldReason yieldReason;
	YieldContext yieldContext;
};

// Fiber that swaps jobs around.
// The reason to have a separate scheduler fiber is so we can queue the caller fiber right away,
// without risking another thread picking it up while it's still running on this one (since all this
// logic would be running in the fiber that wants to yield).
void SchedulerProc(void *args)
{
	SchedulerArgs *argsStruct = (SchedulerArgs *)args;
	u32 previousJobIdx = argsStruct->previousJobIdx;

	// Queue previous job now that its fiber is not running
	if (previousJobIdx != U32_MAX) {
		Job *previousJob = &g_context->jobs.unsafe[previousJobIdx];
		YieldReason yieldReason = argsStruct->yieldReason;

		ASSERT(previousJob->state == JOBSTATE_RUNNING);
		previousJob->state = JOBSTATE_SUSPENDED;

		switch (yieldReason) {
		case YIELDREASON_FAILED:
		{
			AtomicIncrementGetNew(&g_context->failedJobsCount);
			previousJob->state = JOBSTATE_FINISHED;
		} break;
		case YIELDREASON_WAITING_FOR_STOP:
		{
			auto jobs = g_context->waitingJobsByReason[yieldReason].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
		} break;
		case YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED:
		{
			// IMPORTANT! globalValuesLock should be locked before calling SwitchJob!
			auto jobs = g_context->waitingJobsByReason[yieldReason].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SpinlockUnlock(&g_context->globalValuesLock);
		} break;
		case YIELDREASON_NEED_DYNAMIC_LIBRARY:
		{
			// IMPORTANT! ctExternalLibraries should be locked before calling SwitchJob!
			auto jobs = g_context->waitingJobsByReason[YIELDREASON_NEED_DYNAMIC_LIBRARY].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSMutexUnlock(g_context->ctExternalLibraries.lock);
		} break;
		case YIELDREASON_PROC_BODY_NOT_READY:
		case YIELDREASON_PROC_IR_NOT_READY:
		{
			// IMPORTANT! procedures should be locked before calling SwitchJob!
			auto jobs = g_context->waitingJobsByReason[yieldReason].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSUnlockForRead(&g_context->procedures.rwLock);
		} break;
		case YIELDREASON_POLYMORPHIC_PROC_NOT_CREATED:
		{
			// IMPORTANT! polymorphicProcedures should be locked before calling SwitchJob!
			auto jobs = g_context->waitingJobsByReason[yieldReason].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSUnlockForWrite(&g_context->polymorphicProcedures.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_OVERLOAD:
		{
			// IMPORTANT! operatorOverloads should be locked before calling SwitchJob!
			auto jobs = g_context->waitingJobsByReason[YIELDREASON_UNKNOWN_OVERLOAD].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSUnlockForRead(&g_context->operatorOverloads.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_IDENTIFIER:
		{
			// IMPORTANT! tcGlobalNames should be locked before calling SwitchJob!
			auto jobs = g_context->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			RWSpinlockUnlockForRead(&g_context->tcGlobalNames.rwLock);
		} break;
		case YIELDREASON_STATIC_DEF_NOT_READY:
		{
			// IMPORTANT! staticDefinitions should be locked before calling SwitchJob!
			auto jobs = g_context->waitingJobsByReason[YIELDREASON_STATIC_DEF_NOT_READY].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSUnlockForRead(&g_context->staticDefinitions.rwLock);
		} break;
		case YIELDREASON_TYPE_NOT_READY:
		{
			// IMPORTANT! waitingJobsByReason[YIELDREASON_TYPE_NOT_READY] should be locked
			// before calling SwitchJob!
			*DynamicArrayAdd(&g_context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].unsafe) =
				previousJobIdx;
			SpinlockUnlock(&g_context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
		} break;
		default:
			ASSERTF(false, "Previous fiber is %llx, reason is %d", previousJob->fiber,
					yieldReason);
		}
	}
	previousJobIdx = U32_MAX;

loop:
	SpinlockLock(&g_context->threadStatesLock);
	g_context->threadStates[t_threadIndex] = THREADSTATE_LOOKING_FOR_JOBS;
	SpinlockUnlock(&g_context->threadStatesLock);

	// Try to get next fiber to run
	u32 nextJobIdx = U32_MAX;
	while (!g_context->done) {
		u32 dequeue;
		if (MTQueueDequeue(&g_context->readyJobs, &dequeue)) {
			nextJobIdx = dequeue;
			goto switchFiber;
		}
		else if (!MTQueueIsEmpty(&g_context->tcGlobalNamesToAdd) &&
				SpinlockTryLock(&g_context->tcGlobalNamesCommitLock)) {
			TCCommitGlobalNames();
			SpinlockUnlock(&g_context->tcGlobalNamesCommitLock);
			continue;
		}
		else {
			bool triggerStop = true;
			SpinlockLock(&g_context->threadStatesLock);
			// If any thread is running something, don't give up.
			for (int threadIdx = t_threadIndex; threadIdx < g_context->threadStates.size; ++threadIdx) {
				ThreadState state = g_context->threadStates[threadIdx];
				if (state == THREADSTATE_WORKING) {
					triggerStop = false;
					goto okWontStop;
				}
			}
			// If nothing's running but there are things in queues, don't give up either.
			if (!MTQueueIsEmpty(&g_context->tcGlobalNamesToAdd)) {
				triggerStop = false;
				goto okWontStop;
			}
			// We make threads give up in order. If any thread before this one hasn't given up,
			// don't give up.
			for (int threadIdx = 0; threadIdx < (int)t_threadIndex; ++threadIdx) {
				ThreadState state = g_context->threadStates[threadIdx];
				if (state != THREADSTATE_GIVING_UP) {
					triggerStop = false;
					goto okWontStop;
				}
			}
okWontStop:
			SpinlockUnlock(&g_context->threadStatesLock);

			if (triggerStop) {
				// Wake up a job waiting for dead stop
				{
					auto jobsWaiting = g_context->waitingJobsByReason[YIELDREASON_WAITING_FOR_STOP].Get();
					if (jobsWaiting->size) {
						nextJobIdx = jobsWaiting[0];
						DynamicArrayRemoveOrdered(&jobsWaiting, 0);

						goto switchFiber;
					}
				}

				// If no one was waiting for dead stop...
				SpinlockLock(&g_context->threadStatesLock);
				g_context->threadStates[t_threadIndex] = THREADSTATE_GIVING_UP;

				// Check if every thread gave up, if so, we are done.
				for (int threadIdx = 0; threadIdx < g_context->threadStates.size; ++threadIdx) {
					if (g_context->threadStates[threadIdx] != THREADSTATE_GIVING_UP) {
						SpinlockUnlock(&g_context->threadStatesLock);
						goto stillBelieve;
					}
				}
				SpinlockUnlock(&g_context->threadStatesLock);

				// Give up!
				g_context->done = true;
				goto finish;
			}
stillBelieve:
			// @Improve: This is silly but shouldn't happen too often...
			SYSSleep(0);
		}
	}
	goto finish;

switchFiber:
	{
		ASSERT(nextJobIdx != U32_MAX);

		SpinlockLock(&g_context->threadStatesLock);
		g_context->threadStates[t_threadIndex] = THREADSTATE_WORKING;

		// Restore hope to jobs
		for (int threadIdx = t_threadIndex; threadIdx < g_context->threadStates.size; ++threadIdx) {
			// If we triggered a deadstop everyone should have given up.
			if (g_context->threadStates[threadIdx] == THREADSTATE_GIVING_UP)
				g_context->threadStates[threadIdx] = THREADSTATE_LOOKING_FOR_JOBS;
		}
		SpinlockUnlock(&g_context->threadStatesLock);

		t_currentJobIdx = nextJobIdx;
		Job *nextJob = &g_context->jobs.unsafe[nextJobIdx];
		if (nextJob->state == JOBSTATE_INIT) {
			nextJob->state = JOBSTATE_RUNNING;
			nextJob->startProcedure(nextJob->args);
			// If we come back here afterwards, this job should be finished
			ASSERT(nextJob->state == JOBSTATE_FINISHED);
		}
		else {
			nextJob->state = JOBSTATE_RUNNING;
			t_fiberToDelete = SYSGetCurrentFiber();

			Fiber fiber = nextJob->fiber;
			ASSERT(fiber != SYS_INVALID_FIBER_HANDLE);
			nextJob->fiber = SYS_INVALID_FIBER_HANDLE;

			SYSSwitchToFiber(fiber);
		}

		goto loop;
	}

finish:
	t_fiberToDelete = SYSGetCurrentFiber();

	Fiber mainFiber = g_mainFibers[t_threadIndex];
	//Print("Switching to main fiber of thread %d (%p)\n", t_threadIndex, mainFiber);
	ASSERT(SYSGetCurrentFiber() != mainFiber);
	SYSSwitchToFiber(mainFiber);
}

// Procedure to switch to a different job.
// We leave the information in thread local storage for the scheduler fiber.
inline void SwitchJob(YieldReason yieldReason, YieldContext yieldContext)
{
	ASSERT(yieldReason != YIELDREASON_DONE); // Call FinishCurrentJob instead.

	// Can't yield from an invalid job! (This is probably the main thread and the multithread phase
	// hasn't started/is over, where there is no good reason to yield.)
	ASSERT(t_currentJobIdx != U32_MAX);

	Job *previousJob = GetCurrentJob();
	ASSERT(previousJob->state == JOBSTATE_RUNNING);

#if ENABLE_STATS
	AtomicIncrementGetNew(&g_stats.jobSwitches);
#endif

	previousJob->fiber = SYSGetCurrentFiber();
	previousJob->yieldContext = yieldContext;

	SchedulerArgs *args = ALLOC(LinearAllocator, SchedulerArgs);
	*args = {
		.previousJobIdx = t_currentJobIdx,
		.yieldReason = yieldReason,
		.yieldContext = yieldContext
	};
	Fiber newFiber = SYSCreateFiber(SchedulerProc, args);
	SYSSwitchToFiber(newFiber);

	// If we come back from a fiber that switched jobs because theirs was done, no need to keep it
	// around.
	if (t_fiberToDelete != SYS_INVALID_FIBER_HANDLE) {
		SYSDeleteFiber(t_fiberToDelete);
		t_fiberToDelete = SYS_INVALID_FIBER_HANDLE;
	}
}

inline void FinishCurrentJob()
{
	Job *previousJob = GetCurrentJob();
	ASSERT(previousJob->state == JOBSTATE_RUNNING);
	previousJob->state = JOBSTATE_FINISHED;
	previousJob->fiber = SYS_INVALID_FIBER_HANDLE;
}

// Procedure where worker threads begin executing
int WorkerThreadProc(void *args)
{
	ThreadArgs *threadArgs = (ThreadArgs *)args;
	u32 threadIdx = threadArgs->threadIndex;

	t_threadIndex = threadIdx;

	SYSSetThreadDescription(SYSGetCurrentThread(), SNPrintF(16, "Worker #%d", threadIdx));

	MemoryInitThread(512 * 1024 * 1024);

	g_mainFibers[threadIdx] = SYSConvertThreadToFiber();

	SchedulerArgs *auxArgs = ALLOC(LinearAllocator, SchedulerArgs);
	*auxArgs = {
		.previousJobIdx = U32_MAX
	};
	Fiber newFiber = SYSCreateFiber(SchedulerProc, auxArgs);
	SYSSwitchToFiber(newFiber);

	if (t_fiberToDelete != SYS_INVALID_FIBER_HANDLE)
		SYSDeleteFiber(t_fiberToDelete);

	SpinlockLock(&g_context->threadStatesLock);
	ASSERT(g_context->threadStates[threadIdx] == THREADSTATE_GIVING_UP);
	g_context->threadStates[threadIdx] = THREADSTATE_TERMINATED;
	SpinlockUnlock(&g_context->threadStatesLock);

	SYSPrepareFiberForExit();
	return 0;
}
