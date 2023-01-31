THREADLOCAL u32 t_runningJobIdx = U32_MAX;
THREADLOCAL void *t_jobData = nullptr;
THREADLOCAL YieldReason t_previousYieldReason;
THREADLOCAL YieldContext t_previousYieldContext;

inline Job *GetCurrentJob(Context *context)
{
	return &context->jobs.unsafe[t_runningJobIdx];
}

inline void EnqueueReadyJob(Context *context, u32 jobIdx)
{
	while (!MTQueueEnqueue(&context->readyJobs, jobIdx)) {
		// Lockless sleep while queue is full
		while (context->readyJobs.head == context->readyJobs.tail)
			Sleep(0);
	}
}

u32 RequestNewJob(Context *context, JobType type, void (*proc)(void *), void *args)
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

void SchedulerProc(Context *context);

void AuxiliaryFiberProc(void *arg)
{
	Context *context = (Context *)arg;
	SchedulerProc(context);

	// If SchedulerProc ever returns it's because this thread is done.
	SpinlockLock(&context->threadStatesLock);
	ASSERT(context->threadStates[t_threadIndex] == THREADSTATE_GIVING_UP);
	context->threadStates[t_threadIndex] = THREADSTATE_TERMINATED;
	SpinlockUnlock(&context->threadStatesLock);

	SYSPrepareFiberForExit();
}

// Procedure to switch to a different job.
// We leave the information in thread local storage for the scheduler fiber.
// Call this when a job finishes too, the scheduler will delete the fiber and free resources.
inline void SwitchJob(Context *context, YieldReason yieldReason, YieldContext yieldContext)
{
	ASSERT(yieldReason != YIELDREASON_DONE); // Call FinishCurrentJob instead.

	Job *previousJob = GetCurrentJob(context);
	ASSERT(previousJob->state == JOBSTATE_RUNNING);
	u32 previousJobIdx = t_runningJobIdx;

	t_jobData = nullptr;
	t_previousYieldReason = yieldReason;
	t_previousYieldContext = yieldContext;
	previousJob->fiber = GetCurrentFiber();
	previousJob->context = yieldContext;

	Fiber newFiber = SYSCreateFiber(AuxiliaryFiberProc, context);
	SYSSwitchToFiber(newFiber);

	ASSERT(t_runningJobIdx == previousJobIdx);
}

inline void FinishCurrentJob(Context *context)
{
	Job *previousJob = GetCurrentJob(context);
	ASSERT(previousJob->state == JOBSTATE_RUNNING);
	previousJob->state = JOBSTATE_FINISHED;
	previousJob->fiber = SYS_INVALID_FIBER_HANDLE;
	t_runningJobIdx = U32_MAX;
	t_jobData = nullptr;
}

// Fiber that swaps jobs around.
// The reason to have a separate scheduler fiber is so we can queue the caller fiber right away,
// without risking another thread picking it up while it's still running on this one (since all this
// logic would be running in the fiber that wants to yield).
void SchedulerProc(Context *context)
{
loop:
	// Queue previous job now that its fiber is not running
	if (t_runningJobIdx != U32_MAX) {
		Job *runningJob = &context->jobs.unsafe[t_runningJobIdx];

		ASSERT(runningJob->state == JOBSTATE_RUNNING);
		runningJob->state = JOBSTATE_SUSPENDED;

		switch (t_previousYieldReason) {
		case YIELDREASON_FAILED:
		{
			_InterlockedIncrement((LONG volatile *)&context->failedJobsCount);
			runningJob->state = JOBSTATE_FINISHED;
		} break;
		case YIELDREASON_WAITING_FOR_STOP:
		{
			auto jobs = context->waitingJobsByReason[t_previousYieldReason].Get();
			*DynamicArrayAdd(&jobs) = t_runningJobIdx;
		} break;
		case YIELDREASON_GLOBAL_VALUE_NOT_ALLOCATED:
		{
			// IMPORTANT! globalValuesLock should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[t_previousYieldReason].Get();
			*DynamicArrayAdd(&jobs) = t_runningJobIdx;
			SpinlockUnlock(&context->globalValuesLock);
		} break;
		case YIELDREASON_NEED_DYNAMIC_LIBRARY:
		{
			// IMPORTANT! ctExternalLibraries should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_NEED_DYNAMIC_LIBRARY].Get();
			*DynamicArrayAdd(&jobs) = t_runningJobIdx;
			SYSMutexUnlock(context->ctExternalLibraries.lock);
		} break;
		case YIELDREASON_PROC_BODY_NOT_READY:
		case YIELDREASON_PROC_IR_NOT_READY:
		{
			// IMPORTANT! procedures should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[t_previousYieldReason].Get();
			*DynamicArrayAdd(&jobs) = t_runningJobIdx;
			SYSUnlockForRead(&context->procedures.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_OVERLOAD:
		{
			// IMPORTANT! operatorOverloads should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_OVERLOAD].Get();
			*DynamicArrayAdd(&jobs) = t_runningJobIdx;
			SYSUnlockForRead(&context->operatorOverloads.rwLock);
		} break;
		case YIELDREASON_UNKNOWN_IDENTIFIER:
		{
			// IMPORTANT! tcGlobalNames should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
			*DynamicArrayAdd(&jobs) = t_runningJobIdx;
			RWSpinlockUnlockForRead(&context->tcGlobalNames.rwLock);
		} break;
		case YIELDREASON_STATIC_DEF_NOT_READY:
		{
			// IMPORTANT! staticDefinitions should be locked before calling SwitchJob!
			auto jobs = context->waitingJobsByReason[YIELDREASON_STATIC_DEF_NOT_READY].Get();
			*DynamicArrayAdd(&jobs) = t_runningJobIdx;
			SYSUnlockForRead(&context->staticDefinitions.rwLock);
		} break;
		case YIELDREASON_TYPE_NOT_READY:
		{
			// IMPORTANT! waitingJobsByReason[YIELDREASON_TYPE_NOT_READY] should be locked
			// before calling SwitchJob!
			*DynamicArrayAdd(&context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].unsafe) =
				t_runningJobIdx;
			SYSMutexUnlock(context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
		} break;
		default:
			ASSERTF(false, "Previous fiber is %llx, reason is %d", runningJob->fiber,
					t_previousYieldReason);
		}
	}
	t_runningJobIdx = U32_MAX;

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
	t_runningJobIdx = nextJobIdx;
	t_jobData = nextJob->jobData;
	if (nextJob->state == JOBSTATE_INIT) {
		nextJob->state = JOBSTATE_RUNNING;
		nextJob->startProcedure(nextJob->args);
	}
	else {
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

	SchedulerProc(context);

	SpinlockLock(&context->threadStatesLock);
	ASSERT(context->threadStates[t_threadIndex] == THREADSTATE_GIVING_UP);
	context->threadStates[t_threadIndex] = THREADSTATE_TERMINATED;
	SpinlockUnlock(&context->threadStatesLock);

	SYSPrepareFiberForExit();

	return 0;
}
