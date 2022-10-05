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
		SYSSpinlockLock(lock);
	}
	~ScopedLockSpin()
	{
		SYSSpinlockUnlock(lock);
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

	const T &LockForRead()
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
		SYSSpinlockLock(&lock);
		//ProfileBegin("Spinlock locked", nullptr, PERFORMANCEAPI_MAKE_COLOR(0xA0, 0x30, 0x10));
		return unsafe;
	}

	void Unlock()
	{
		//ProfileEnd();
		SYSSpinlockUnlock(&lock);
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
