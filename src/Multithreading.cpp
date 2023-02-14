inline void SpinlockLock(volatile u32 *locked)
{
#if IS_MSVC
	//ProfileScope scope("Spinlock acquiring", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xA0, 0x30, 0x10));
	// Stupid MSVC uses long for this intrinsic but it's the same size as u32.
	static_assert(sizeof(long) == sizeof(u32));
	for (;;) {
		long oldLocked = _InterlockedCompareExchange_HLEAcquire((volatile long *)locked, 1, 0);
		if (!oldLocked)
			return;

		for (;;) {
			if (!*locked)
				break;
			_mm_pause();
		}
	}
#else
	// GCC/Clang
	int oldLocked = 1;
retry:
	int eax = 0;
	int success;
	asm volatile("xacquire lock cmpxchg %3, %1"
					: "+a" (eax), "+m" (*locked), "=@cce" (success)
					: "r" (oldLocked) : "memory", "cc");
	if (success)
		return;

pause:
	if (!*locked)
		goto retry;
	_mm_pause();
	goto pause;
#endif
}

inline void SpinlockUnlock(volatile u32 *locked)
{
	ASSERT(*locked == 1);
#if IS_MSVC
	static_assert(sizeof(long) == sizeof(u32));
	_Store_HLERelease((volatile long *)locked, 0);
#else
	// GCC/Clang
	asm volatile("xrelease movl %1, %0" : "+m"(*locked) : "i"(0) : "memory");
#endif
}

const u32 RWSPINLOCK_WRITE_REQUESTED_BIT = 0x40000000;
const u32 RWSPINLOCK_WRITE_LOCKED		 = 0x80000000;
const u32 RWSPINLOCK_LOCK_COUNT_MASK = ~(RWSPINLOCK_WRITE_REQUESTED_BIT | RWSPINLOCK_WRITE_LOCKED);
const u32 RWSPINLOCK_READ_COUNT_MAX	 = RWSPINLOCK_LOCK_COUNT_MASK;

inline void RWSpinlockLockForRead(volatile u32 *lock)
{
	PROFILER_SCOPE("RW Spinlock lock for read");
#if IS_MSVC
	for (;;) {
		u32 expected = *lock;
		if (expected < RWSPINLOCK_READ_COUNT_MAX) {
			u32 desired = expected + 1;
			u32 oldLock = (u32)_InterlockedCompareExchange_HLEAcquire((volatile long *)lock,
					desired, expected);
			if (oldLock == expected)
				break;
		}
		_mm_pause();
	}
#else
	// GCC/Clang
	for (;;) {
		u32 expected = *lock;
		if (expected < RWSPINLOCK_READ_COUNT_MAX) {
			u32 desired = expected + 1;
			int success;
			asm volatile(
					"xacquire lock cmpxchg %3, %1"
							: "+a" (expected), "+m" (*lock), "=@cce" (success)
							: "r" (desired) : "memory", "cc");
			if (success) return;
		}
		_mm_pause();
	}
#endif
}

inline void RWSpinlockUnlockForRead(volatile u32 *lock)
{
	PROFILER_SCOPE("RW Spinlock unlock for read");
#if DEBUG_BUILD
	u32 lockValue = *lock;
	ASSERT((lockValue & RWSPINLOCK_LOCK_COUNT_MASK) > 0);
	ASSERT((lockValue & RWSPINLOCK_WRITE_LOCKED) == 0);
#endif

#if IS_MSVC
	// Don't know of a way to emit an 'xrelease lock dec' on MSVC...
	_InterlockedExchangeAdd_HLERelease((volatile long *)lock, -1);
#else
	// GCC/Clang
	asm volatile("xrelease lock decl %1" : "+m"(*lock) : : "memory");
#endif
}

inline void RWSpinlockLockForWrite(volatile u32 *lock)
{
	PROFILER_SCOPE("RW Spinlock lock for write");
#if IS_MSVC
	for (;;) {
		// Request, to stop new read locks
		if (!(*lock & RWSPINLOCK_WRITE_REQUESTED_BIT))
			_InterlockedOr((volatile long *)lock, RWSPINLOCK_WRITE_REQUESTED_BIT);

		u32 expected = RWSPINLOCK_WRITE_REQUESTED_BIT; // Only this bit
		u32 desired = expected | RWSPINLOCK_WRITE_LOCKED;
		u32 oldLock = (u32)_InterlockedCompareExchange_HLEAcquire((volatile long *)lock, desired,
				expected);
		if (oldLock == expected)
			return;

		for (;;) {
			if ((*lock & RWSPINLOCK_LOCK_COUNT_MASK) == 0)
				break;
			_mm_pause();
		}
	}
#else
	// GCC/Clang
	int oldLock = -1;
retry:
	int eax = 0;
	int success;
	asm volatile("xacquire lock cmpxchg %3, %1"
					: "+a" (eax), "+m" (*lock), "=@cce" (success)
					: "r" (oldLock) : "memory", "cc");
	if (success) return;
pause:
	if (!*lock)
		goto retry;
	_mm_pause();
	goto pause;
#endif
}

inline void RWSpinlockUnlockForWrite(volatile u32 *lock)
{
	PROFILER_SCOPE("RW Spinlock unlock for write");
#if DEBUG_BUILD
	u32 lockValue = *lock;
	ASSERT(lockValue & RWSPINLOCK_WRITE_LOCKED);
#endif

#if IS_MSVC
	_Store_HLERelease((volatile long *)lock, 0);
#else
	// GCC/Clang
	asm volatile("xrelease movl %1, %0" : "+m"(*lock) : "i"(0) : "memory");
#endif
}

class [[nodiscard]] ScopedLockRead
{
public:
	RWLock *lock;
	ScopedLockRead(RWLock *aLock) {
		lock = aLock;
		SYSLockForRead(lock);
	}
	~ScopedLockRead() {
		SYSUnlockForRead(lock);
	}
};

class [[nodiscard]] ScopedLockWrite
{
public:
	RWLock *lock;
	ScopedLockWrite(RWLock *aLock) {
		lock = aLock;
		SYSLockForWrite(lock);
	}
	~ScopedLockWrite() {
		SYSUnlockForWrite(lock);
	}
};

class [[nodiscard]] ScopedLockSpin
{
public:
	volatile u32 *lock;
	ScopedLockSpin(volatile u32 *aLock) {
		lock = aLock;
		SpinlockLock(lock);
	}
	~ScopedLockSpin() {
		SpinlockUnlock(lock);
	}
};

class [[nodiscard]] ScopedLockMutex
{
public:
	Mutex mutex;
	ScopedLockMutex(Mutex aMutex)
	{
		mutex = aMutex;
		SYSMutexLock(mutex);
	}
	~ScopedLockMutex()
	{
		SYSMutexUnlock(mutex);
	}
};

template <typename T>
class RWContainer
{
public:
	T unsafe;
	RWLock rwLock;

	RWContainer() {
		SYSCreateRWLock(&rwLock);
	}

	T &LockForRead() {
		SYSLockForRead(&rwLock);
		return unsafe;
	}
	void UnlockForRead() {
		SYSUnlockForRead(&rwLock);
	}
	T &LockForWrite() {
		SYSLockForWrite(&rwLock);
		return unsafe;
	}
	void UnlockForWrite() {
		SYSUnlockForWrite(&rwLock);
	}

	template <typename T2>
	class HandleRead
	{
	public:
		RWContainer<T2> *safeContainer;

		HandleRead(RWContainer<T2> *safe) {
			safeContainer = safe;
			safeContainer->LockForRead();
		}
		~HandleRead() {
			safeContainer->UnlockForRead();
		}

		const T2 &operator*() { return safeContainer->unsafe; }
		const T2 *operator->() { return &safeContainer->unsafe; }
		const T2 *operator&() { return &safeContainer->unsafe; }

		const decltype(safeContainer->unsafe[0]) &operator[](s64 index) {
			return safeContainer->unsafe[index];
		}
	};

	HandleRead<T> GetForRead() {
		return HandleRead<T>(this);
	}

	template <typename T2>
	class HandleWrite
	{
	public:
		RWContainer<T2> *safeContainer;

		HandleWrite(RWContainer<T2> *safe) {
			safe->LockForWrite();
			safeContainer = safe;
		}
		~HandleWrite() {
			safeContainer->UnlockForWrite();
		}

		T2 &operator*() { return safeContainer->unsafe; }
		T2 *operator->() { return &safeContainer->unsafe; }
		T2 *operator&() { return &safeContainer->unsafe; }

		const decltype(safeContainer->unsafe[0]) &operator[](s64 index) {
			return safeContainer->unsafe[index];
		}
	};

	HandleWrite<T> GetForWrite() {
		return HandleWrite<T>(this);
	}
};

template <typename T>
class SLContainer
{
public:
	T unsafe;
	volatile u32 lock;

	const T &Lock() {
		SpinlockLock(&lock);
		//ProfileBegin("Spinlock locked", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xA0, 0x30, 0x10));
		return unsafe;
	}
	void Unlock() {
		//ProfileEnd();
		SpinlockUnlock(&lock);
	}

	template <typename T2>
	class Handle
	{
	public:
		SLContainer<T2> *safeContainer;

		Handle(SLContainer<T2> *safe) {
			safe->Lock();
			safeContainer = safe;
		}
		~Handle() { safeContainer->Unlock(); }

		T2 &operator*() { return safeContainer->unsafe; }
		T2 *operator->() { return &safeContainer->unsafe; }
		T2 *operator&() { return &safeContainer->unsafe; }

		const decltype(safeContainer->unsafe[0]) &operator[](s64 index) {
			return safeContainer->unsafe[index];
		}
	};

	Handle<T> Get() {
		return Handle<T>(this);
	}
};

template <typename T>
class SLRWContainer
{
public:
	T unsafe;
	volatile u32 rwLock;

	SLRWContainer() {
		rwLock = 0;
	}

	T &LockForRead() {
		RWSpinlockLockForRead(&rwLock);
		return unsafe;
	}
	void UnlockForRead() {
		RWSpinlockUnlockForRead(&rwLock);
	}
	T &LockForWrite() {
		RWSpinlockLockForWrite(&rwLock);
		return unsafe;
	}
	void UnlockForWrite() {
		RWSpinlockUnlockForWrite(&rwLock);
	}

	template <typename T2>
	class HandleRead
	{
	public:
		SLRWContainer<T2> *safeContainer;

		HandleRead(SLRWContainer<T2> *safe) {
			safeContainer = safe;
			safeContainer->LockForRead();
		}
		~HandleRead() {
			safeContainer->UnlockForRead();
		}

		const T2 &operator*() { return safeContainer->unsafe; }
		const T2 *operator->() { return &safeContainer->unsafe; }
		const T2 *operator&() { return &safeContainer->unsafe; }

		const decltype(safeContainer->unsafe[0]) &operator[](s64 index) {
			return safeContainer->unsafe[index];
		}
	};

	HandleRead<T> GetForRead() {
		return HandleRead<T>(this);
	}

	template <typename T2>
	class HandleWrite
	{
	public:
		SLRWContainer<T2> *safeContainer;

		HandleWrite(SLRWContainer<T2> *safe) {
			safe->LockForWrite();
			safeContainer = safe;
		}
		~HandleWrite() {
			safeContainer->UnlockForWrite();
		}

		T2 &operator*() { return safeContainer->unsafe; }
		T2 *operator->() { return &safeContainer->unsafe; }
		T2 *operator&() { return &safeContainer->unsafe; }

		const decltype(safeContainer->unsafe[0]) &operator[](s64 index) {
			return safeContainer->unsafe[index];
		}
	};

	HandleWrite<T> GetForWrite() {
		return HandleWrite<T>(this);
	}
};

template <typename T>
class MXContainer
{
public:
	T unsafe;
	Mutex lock;

	MXContainer() {
		lock = SYSCreateMutex();
	}
	const T &Lock() {
		SYSMutexLock(lock);
		return unsafe;
	}
	void Unlock() {
		SYSMutexUnlock(lock);
	}

	template <typename T2>
	class Handle
	{
	public:
		MXContainer<T2> *safeContainer;

		Handle(MXContainer<T2> *safe) {
			safe->Lock();
			safeContainer = safe;
		}
		~Handle() { safeContainer->Unlock(); }

		T2 &operator*() { return safeContainer->unsafe; }
		T2 *operator->() { return &safeContainer->unsafe; }
		T2 *operator&() { return &safeContainer->unsafe; }

		const decltype(safeContainer->unsafe[0]) &operator[](s64 index) {
			return safeContainer->unsafe[index];
		}
	};

	Handle<T> Get() {
		return Handle<T>(this);
	}
};
