THREADLOCAL Fiber t_schedulerFiber;
THREADLOCAL Fiber t_previousFiber = SYS_INVALID_FIBER_HANDLE;
THREADLOCAL YieldReason t_previousYieldReason;
THREADLOCAL YieldContext t_previousYieldContext;

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

void WakeUpAllByIndex(Context *context, YieldReason reason, u32 index)
{
	auto jobsWaiting = context->waitingJobsByReason[reason].Get();
	for (int i = 0; i < jobsWaiting->size; ) {
		Job *job = &jobsWaiting[i];
		if (job->context.index == index) {
			EnqueueReadyJob(context, job->fiber);
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
			EnqueueReadyJob(context, job->fiber);
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
			Job job;
			job.fiber = t_previousFiber;
			job.context = t_previousYieldContext;
			switch (t_previousYieldReason) {
			case YIELDREASON_FAILED:
			{
				_InterlockedIncrement((LONG volatile *)&context->failedJobsCount); \
				// Fall through
			}
			case YIELDREASON_DONE:
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
			case YIELDREASON_WAITING_FOR_STOP:
			case YIELDREASON_GLOBAL_VALUE_NOT_READY:
			{
				auto jobs = context->waitingJobsByReason[t_previousYieldReason].Get();
				*DynamicArrayAdd(&jobs) = job;
			} break;
			case YIELDREASON_PROC_BODY_NOT_READY:
			case YIELDREASON_PROC_IR_NOT_READY:
			{
				// IMPORTANT! procedures should be locked before calling SwitchJob!
				auto jobs = context->waitingJobsByReason[t_previousYieldReason].Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->procedures.rwLock);
			} break;
			case YIELDREASON_UNKNOWN_OVERLOAD:
			{
				// IMPORTANT! operatorOverloads should be locked before calling SwitchJob!
				auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_OVERLOAD].Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->operatorOverloads.rwLock);
			} break;
			case YIELDREASON_UNKNOWN_IDENTIFIER:
			{
				// IMPORTANT! tcGlobalNames should be locked before calling SwitchJob!
				auto jobs = context->waitingJobsByReason[YIELDREASON_UNKNOWN_IDENTIFIER].Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->tcGlobalNames.rwLock);
			} break;
			case YIELDREASON_STATIC_DEF_NOT_READY:
			{
				// IMPORTANT! staticDefinitions should be locked before calling SwitchJob!
				auto jobs = context->waitingJobsByReason[YIELDREASON_STATIC_DEF_NOT_READY].Get();
				*DynamicArrayAdd(&jobs) = job;
				SYSUnlockForRead(&context->staticDefinitions.rwLock);
			} break;
			case YIELDREASON_TYPE_NOT_READY:
			{
				// IMPORTANT! waitingJobsByReason[YIELDREASON_TYPE_NOT_READY] should be locked
				// before calling SwitchJob!
				*DynamicArrayAdd(&context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].unsafe) = job;
				SYSMutexUnlock(context->waitingJobsByReason[YIELDREASON_TYPE_NOT_READY].lock);
			} break;
			default:
				ASSERTF(false, "Previous fiber is %llx, reason is %d", t_previousFiber,
						t_previousYieldReason);
			}
		}
		t_previousFiber = (Fiber)0xDEADBEEFDEADBEEF;

		context->threadStates[t_threadIndex] = THREADSTATE_LOOKING_FOR_JOBS;

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
		while (!context->done) {
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
				bool triggerStop = true;
				// We make threads give up in order
				for (int threadIdx = 0; threadIdx < (int)t_threadIndex; ++threadIdx) {
					ThreadState state = context->threadStates[threadIdx];
					if (state != THREADSTATE_GIVING_UP) {
						triggerStop = false;
						break;
					}
				}
				for (int threadIdx = t_threadIndex; threadIdx < context->threadStates.size; ++threadIdx) {
					ThreadState state = context->threadStates[threadIdx];
					if (state != THREADSTATE_LOOKING_FOR_JOBS) {
						triggerStop = false;
						break;
					}
				}
				if (triggerStop) {
					// Wake up a job waiting for dead stop
					{
						auto jobsWaiting = context->waitingJobsByReason[YIELDREASON_WAITING_FOR_STOP].Get();
						if (jobsWaiting->size) {
							Job *job = &jobsWaiting[0];
							nextFiber = job->fiber;
							DynamicArrayRemoveOrdered(&jobsWaiting, 0);
							goto switchFiber;
						}
					}

					// Lets try to make it work without this
#if 0
					// Try waking up some random job
					for (int yieldReason = 0; yieldReason < YIELDREASON_Count; ++yieldReason) {
						if (yieldReason == YIELDREASON_WAITING_FOR_STOP) continue;
						auto jobsWaiting = context->waitingJobsByReason[yieldReason].Get();
						if (jobsWaiting->size) {
							Job *job = &jobsWaiting[0];
							nextFiber = job->fiber;
							DynamicArrayRemoveOrdered(&jobsWaiting, 0);
							goto switchFiber;
						}
					}
#endif

#if DEFER_FIBER_CREATION
					// Fallback, create the fiber on whatever thread and keep going
					JobRequest jobToCreate;
					if (MTQueueDequeue(&context->jobsToCreate, &jobToCreate)) {
						nextFiber = SYSCreateFiber(jobToCreate.proc, jobToCreate.args);
						goto switchFiber;
					}
#endif

					context->threadStates[t_threadIndex] = THREADSTATE_GIVING_UP;

					// Check if every thread gave up, if so, we are done.
					for (int threadIdx = 0; threadIdx < context->threadStates.size; ++threadIdx) {
						if (context->threadStates[threadIdx] != THREADSTATE_GIVING_UP)
							goto stillBelieve;
					}
					// Give up!
					context->done = true;
				}
stillBelieve:
				// @Improve: This is silly but shouldn't happen too often...
				Sleep(0);
			}
		}

switchFiber:
		if (nextFiber != SYS_INVALID_FIBER_HANDLE) {
			context->threadStates[t_threadIndex] = THREADSTATE_WORKING;
			SYSSwitchToFiber(nextFiber);
		}
		else return;
	}
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
	//Print("Scheduler fiber for thread %d is %llX\n", t_threadIndex, t_schedulerFiber);

	SchedulerProc(context);

	context->threadStates[t_threadIndex] = THREADSTATE_TERMINATED;

	SYSPrepareFiberForExit();

	return 0;
}
