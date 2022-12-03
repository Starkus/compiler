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
	asm volatile("xrelease movl %1, %0" : "+m"(*locked) : "i"(0) : "memory");
#endif
}

class [[nodiscard]] ScopedLockRead
{
public:
	RWLock *lock;
	ScopedLockRead(RWLock *aLock)
	{
		lock = aLock;
		SYSLockForRead(lock);
	}
	~ScopedLockRead()
	{
		SYSUnlockForRead(lock);
	}
};

class [[nodiscard]] ScopedLockWrite
{
public:
	RWLock *lock;
	ScopedLockWrite(RWLock *aLock)
	{
		lock = aLock;
		SYSLockForWrite(lock);
	}
	~ScopedLockWrite()
	{
		SYSUnlockForWrite(lock);
	}
};

class [[nodiscard]] ScopedLockSpin
{
public:
	volatile u32 *lock;
	ScopedLockSpin(volatile u32 *aLock)
	{
		lock = aLock;
		SpinlockLock(lock);
	}
	~ScopedLockSpin()
	{
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

	RWContainer()
	{
		SYSCreateRWLock(&rwLock);
	}

	T &LockForRead()
	{
		SYSLockForRead(&rwLock);
		return unsafe;
	}

	void UnlockForRead()
	{
		SYSUnlockForRead(&rwLock);
	}

	T &LockForWrite()
	{
		SYSLockForWrite(&rwLock);
		return unsafe;
	}

	void UnlockForWrite()
	{
		SYSUnlockForWrite(&rwLock);
	}

	template <typename T2>
	class HandleRead
	{
	public:
		RWContainer<T2> *safeContainer;

		HandleRead(RWContainer<T2> *safe)
		{
			safeContainer = safe;
			safeContainer->LockForRead();
		}

		~HandleRead()
		{
			safeContainer->UnlockForRead();
		}

		const T2 &operator*()
		{
			return safeContainer->unsafe;
		}

		const T2 *operator->()
		{
			return &safeContainer->unsafe;
		}

		const T2 *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	HandleRead<T> GetForRead()
	{
		return HandleRead<T>(this);
	}

	template <typename T2>
	class HandleWrite
	{
	public:
		RWContainer<T2> *safeContainer;

		HandleWrite(RWContainer<T2> *safe)
		{
			safe->LockForWrite();
			safeContainer = safe;
		}

		~HandleWrite()
		{
			safeContainer->UnlockForWrite();
		}

		T2 &operator*()
		{
			return safeContainer->unsafe;
		}

		T2 *operator->()
		{
			return &safeContainer->unsafe;
		}

		T2 *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	HandleWrite<T> GetForWrite()
	{
		return HandleWrite<T>(this);
	}
};

template <typename T>
class SLContainer
{
public:
	T unsafe;
	volatile u32 lock;

	const T &Lock()
	{
		SpinlockLock(&lock);
		//ProfileBegin("Spinlock locked", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xA0, 0x30, 0x10));
		return unsafe;
	}

	void Unlock()
	{
		//ProfileEnd();
		SpinlockUnlock(&lock);
	}

	template <typename T2>
	class Handle
	{
	public:
		SLContainer<T2> *safeContainer;

		Handle(SLContainer<T2> *safe)
		{
			safe->Lock();
			safeContainer = safe;
		}

		~Handle()
		{
			safeContainer->Unlock();
		}

		T2 &operator*()
		{
			return safeContainer->unsafe;
		}

		T2 *operator->()
		{
			return &safeContainer->unsafe;
		}

		T2 *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	Handle<T> Get()
	{
		return Handle<T>(this);
	}
};

template <typename T>
class MXContainer
{
public:
	T unsafe;
	Mutex lock;

	MXContainer()
	{
		lock = SYSCreateMutex();
	}

	const T &Lock()
	{
		SYSMutexLock(lock);
		return unsafe;
	}

	void Unlock()
	{
		SYSMutexUnlock(lock);
	}

	template <typename T2>
	class Handle
	{
	public:
		MXContainer<T2> *safeContainer;

		Handle(MXContainer<T2> *safe)
		{
			safe->Lock();
			safeContainer = safe;
		}

		~Handle()
		{
			safeContainer->Unlock();
		}

		T2 &operator*()
		{
			return safeContainer->unsafe;
		}

		T2 *operator->()
		{
			return &safeContainer->unsafe;
		}

		T2 *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	Handle<T> Get()
	{
		return Handle<T>(this);
	}
};
