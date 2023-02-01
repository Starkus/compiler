THREADLOCAL Fiber t_fiberToDelete = SYS_INVALID_FIBER_HANDLE;

inline Job *GetCurrentJob(JobContext *context)
{
	return &context->global->jobs.unsafe[context->jobIdx];
}

inline void EnqueueReadyJob(Context *context, u32 jobIdx)
{
	while (!MTQueueEnqueue(&context->readyJobs, jobIdx)) {
		// Lockless sleep while queue is full
		while (context->readyJobs.head == context->readyJobs.tail)
			Sleep(0);
	}
}

u32 RequestNewJob(Context *context, JobType type, void (*proc)(u32, void *), void *args)
{
	SpinlockLock(&context->jobs.lock);
	u32 newJobIdx = (u32)context->jobs.unsafe.count;
	Job *newJob = BucketArrayAdd(&context->jobs.unsafe);
	SpinlockUnlock(&context->jobs.lock);
	*newJob = {
		.startProcedure = proc,
		.args = args,
		.type = type,
		.state = JOBSTATE_INIT };

	EnqueueReadyJob(context, newJobIdx);

	return newJobIdx;
}

void WakeUpAllByIndex(Context *context, YieldReason reason, u32 index)
{
	auto jobsWaiting = context->waitingJobsByReason[reason].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		u32 jobIdx = jobsWaiting[i];
		const Job *job = &context->jobs.unsafe[jobIdx];
		if (job->context.index == index) {
			EnqueueReadyJob(context, jobIdx);
			// Remove
			DynamicArraySwapRemove(&jobsWaiting, i);
		}
		else
			++i;
	}
}

void WakeUpAllByName(Context *context, YieldReason reason, String name)
{
	auto jobsWaiting = context->waitingJobsByReason[reason].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		u32 jobIdx = jobsWaiting[i];
		const Job *job = &context->jobs.unsafe[jobIdx];
		if (StringEquals(job->context.identifier, name)) {
			EnqueueReadyJob(context, jobIdx);
			// Remove
			DynamicArraySwapRemove(&jobsWaiting, i);
		}
		else
			++i;
	}
}

void SchedulerProc(Context *context, u32 previousJobIdx, YieldReason yieldReason,
		YieldContext yieldContext);

struct AuxiliaryFiberArgs
{
	Context *context;
	u32 previousJobIdx;
	YieldReason yieldReason;
	YieldContext yieldContext;
};
void AuxiliaryFiberProc(void *arg)
{
	AuxiliaryFiberArgs *args = (AuxiliaryFiberArgs *)arg;
	Context *context = args->context;
	SchedulerProc(context, args->previousJobIdx, args->yieldReason, args->yieldContext);

	// If SchedulerProc ever returns it's because this thread is done.
	SpinlockLock(&context->threadStatesLock);
	ASSERT(context->threadStates[t_threadIndex] == THREADSTATE_GIVING_UP);
	context->threadStates[t_threadIndex] = THREADSTATE_TERMINATED;
	SpinlockUnlock(&context->threadStatesLock);

	SYSPrepareFiberForExit();
	ExitThread(0);
}

// Procedure to switch to a different job.
// We leave the information in thread local storage for the scheduler fiber.
// Call this when a job finishes too, the scheduler will delete the fiber and free resources.
inline void SwitchJob(JobContext *context, YieldReason yieldReason, YieldContext yieldContext)
{
	ASSERT(yieldReason != YIELDREASON_DONE); // Call FinishCurrentJob instead.

	Job *previousJob = GetCurrentJob(context);
	ASSERT(previousJob->state == JOBSTATE_RUNNING);

	previousJob->fiber = GetCurrentFiber();
	previousJob->context = yieldContext;

	AuxiliaryFiberArgs args = {
		.context = context->global,
		.previousJobIdx = context->jobIdx,
		.yieldReason = yieldReason,
		.yieldContext = yieldContext
	};
	Fiber newFiber = SYSCreateFiber(AuxiliaryFiberProc, &args);
	SYSSwitchToFiber(newFiber);

	// If we come back from a fiber that switched jobs because theirs was done, no need to keep it
	// around.
	if (t_fiberToDelete != SYS_INVALID_FIBER_HANDLE)
		SYSDeleteFiber(t_fiberToDelete);
}

inline void FinishCurrentJob(JobContext *context)
{
	Job *previousJob = GetCurrentJob(context);
	ASSERT(previousJob->state == JOBSTATE_RUNNING);
	previousJob->state = JOBSTATE_FINISHED;
	previousJob->fiber = SYS_INVALID_FIBER_HANDLE;
}

// Fiber that swaps jobs around.
// The reason to have a separate scheduler fiber is so we can queue the caller fiber right away,
// without risking another thread picking it up while it's still running on this one (since all this
// logic would be running in the fiber that wants to yield).
void SchedulerProc(Context *context, u32 previousJobIdx, YieldReason yieldReason,
		YieldContext yieldContext)
{
loop:
	// Queue previous job now that its fiber is not running
	if (previousJobIdx != U32_MAX) {
		Job *previousJob = &context->jobs.unsafe[previousJobIdx];

		ASSERT(previousJob->state == JOBSTATE_RUNNING);
		previousJob->state = JOBSTATE_SUSPENDED;

		switch (yieldReason) {
		case YIELDREASON_FAILED:
		{
			_InterlockedIncrement((LONG volatile *)&context->failedJobsCount);
			previousJob->state = JOBSTATE_FINISHED;
		} break;
		case YIELDREASON_WAITING_FOR_STOP:
		{
			auto jobs = context->waitingJobsByReason[yieldReason].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
		} break;
		case YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED:
		{
			// IMPORTANT! globalValuesLock should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[yieldReason].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SpinlockUnlock(&context->globalValuesLock);
		} break;
		case YIELDREASON_NEED_DYNAMIC_LIBRARY:
		{
			// IMPORTANT! ctExternalLibraries should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_NEED_DYNAMIC_LIBRARY].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSMutexUnlock(context->ctExternalLibraries.lock);
		} break;
		case YIELDREASON_PROC_BODY_NOT_READY:
		case YIELDREASON_PROC_IR_NOT_READY:
		{
			// IMPORTANT! procedures should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[yieldReason].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSUnlockForRead(&context->procedures.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_OVERLOAD:
		{
			// IMPORTANT! operatorOverloads should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_OVERLOAD].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSUnlockForRead(&context->operatorOverloads.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_IDENTIFIER:
		{
			// IMPORTANT! tcGlobalNames should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			RWSpinlockUnlockForRead(&context->tcGlobalNames.rwLock);
		} break;
		case YIELDREASON_STATIC_DEF_NOT_READY:
		{
			// IMPORTANT! staticDefinitions should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_STATIC_DEF_NOT_READY].Get();
			*DynamicArrayAdd(&jobs) = previousJobIdx;
			SYSUnlockForRead(&context->staticDefinitions.rwLock);
		} break;
		case YIELDREASON_TYPE_NOT_READY:
		{
			// IMPORTANT! waitingJobsByReason[YIELDREASON_TYPE_NOT_READY] should be locked
			// before calling SwitchJob!
			*DynamicArrayAdd(&context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].unsafe) =
				previousJobIdx;
			SYSMutexUnlock(context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
		} break;
		default:
			ASSERTF(false, "Previous fiber is %llx, reason is %d", previousJob->fiber,
					yieldReason);
		}
	}
	previousJobIdx = U32_MAX;

	SpinlockLock(&context->threadStatesLock);
	context->threadStates[t_threadIndex] = THREADSTATE_LOOKING_FOR_JOBS;
	SpinlockUnlock(&context->threadStatesLock);

	// Try to get next fiber to run
	u32 nextJobIdx = U32_MAX;
	while (!context->done) {
		u32 dequeue;
		if (MTQueueDequeue(&context->readyJobs, &dequeue)) {
			nextJobIdx = dequeue;
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
						nextJobIdx = jobsWaiting[0];
						DynamicArrayRemoveOrdered(&jobsWaiting, 0);

						goto switchFiber;
					}
				}

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
	if (nextJobIdx == U32_MAX)
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

	Job *nextJob = &context->jobs.unsafe[nextJobIdx];
	if (nextJob->state == JOBSTATE_INIT) {
		nextJob->state = JOBSTATE_RUNNING;
		nextJob->startProcedure(nextJobIdx, nextJob->args);
	}
	else {
		t_fiberToDelete = GetCurrentFiber();

		Fiber fiber = nextJob->fiber;
		nextJob->fiber = SYS_INVALID_FIBER_HANDLE;
		ASSERT(fiber != SYS_INVALID_FIBER_HANDLE);
		nextJob->state = JOBSTATE_RUNNING;
		SYSSwitchToFiber(fiber);
	}

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

	SYSConvertThreadToFiber();

	SchedulerProc(context, U32_MAX, YIELDREASON_Count, {});

	SpinlockLock(&context->threadStatesLock);
	ASSERT(context->threadStates[t_threadIndex] == THREADSTATE_GIVING_UP);
	context->threadStates[t_threadIndex] = THREADSTATE_TERMINATED;
	SpinlockUnlock(&context->threadStatesLock);

	SYSPrepareFiberForExit();

	ExitThread(0);
}
