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
	asm volatile("xacquire lock cmpxchg %2, %1"
					: "+a" (eax), "+m" (*locked)
					: "r" (oldLocked) : "memory", "cc");
	if (!eax)
		goto ret;

pause:
	if (!*locked)
		goto retry;
	_mm_pause();
	goto pause;

ret:
	return;
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

inline void RWSpinlockLockForRead(volatile s32 *lock)
{
#if IS_MSVC
	for (;;) {
		s32 expected = *lock;
		if (expected >= 0) {
			s32 desired = expected + 1;
			if (_InterlockedCompareExchange_HLEAcquire((volatile long *)lock, desired, expected) ==
					expected)
				break;
		}
		_mm_pause();
	}
#else
	// GCC/Clang
#error not implemented
#endif
}

inline void RWSpinlockUnlockForRead(volatile s32 *lock)
{
	ASSERT(*lock > 0);
#if IS_MSVC
	// Don't know of a way to emit an 'xrelease lock dec' on MSVC...
	_InterlockedExchangeAdd_HLERelease((volatile long *)lock, -1);
#else
	// GCC/Clang
#error not implemented
#endif
}

inline void RWSpinlockLockForWrite(volatile s32 *lock)
{
#if IS_MSVC
	for (;;) {
		long oldLock = _InterlockedCompareExchange_HLEAcquire((volatile long *)lock, -1, 0);
		if (!oldLock)
			return;

		for (;;) {
			if (!*lock)
				break;
			_mm_pause();
		}
	}
#else
	// GCC/Clang
#error not implemented
#endif
}

inline void RWSpinlockUnlockForWrite(volatile s32 *lock)
{
	ASSERT(*lock == -1);
#if IS_MSVC
	_Store_HLERelease((volatile long *)lock, 0);
#else
	// GCC/Clang
#error not implemented
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
	volatile s32 rwLock;

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
