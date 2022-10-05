struct Mutex;
void SYSMutexLock(Mutex m, u64 timeout);
void SYSMutexUnlock(Mutex m);

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

	template <typename T>
	class HandleRead
	{
	public:
		RWContainer<T> *safeContainer;

		HandleRead(RWContainer<T> *safe)
		{
			safeContainer = safe;
			safeContainer->LockForRead();
		}

		~HandleRead()
		{
			safeContainer->UnlockForRead();
		}

		T &operator*()
		{
			return safeContainer->unsafe;
		}

		T *operator->()
		{
			return &safeContainer->unsafe;
		}

		T *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	HandleRead<T> GetForRead()
	{
		return HandleRead(this);
	}

	template <typename T>
	class HandleWrite
	{
	public:
		RWContainer<T> *safeContainer;

		HandleWrite(RWContainer<T> *safe)
		{
			safe->LockForWrite();
			safeContainer = safe;
		}

		~HandleWrite()
		{
			safeContainer->UnlockForWrite();
		}

		T &operator*()
		{
			return safeContainer->unsafe;
		}

		T *operator->()
		{
			return &safeContainer->unsafe;
		}

		T *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	HandleWrite<T> GetForWrite()
	{
		return HandleWrite(this);
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

	template <typename T>
	class Handle
	{
	public:
		SLContainer<T> *safeContainer;

		Handle(SLContainer<T> *safe)
		{
			safe->Lock();
			safeContainer = safe;
		}

		~Handle()
		{
			safeContainer->Unlock();
		}

		T &operator*()
		{
			return safeContainer->unsafe;
		}

		T *operator->()
		{
			return &safeContainer->unsafe;
		}

		T *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	Handle<T> Get()
	{
		return Handle(this);
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

	template <typename T>
	class Handle
	{
	public:
		MXContainer<T> *safeContainer;

		Handle(MXContainer<T> *safe)
		{
			safe->Lock();
			safeContainer = safe;
		}

		~Handle()
		{
			safeContainer->Unlock();
		}

		T &operator*()
		{
			return safeContainer->unsafe;
		}

		T *operator->()
		{
			return &safeContainer->unsafe;
		}

		T *operator&()
		{
			return &safeContainer->unsafe;
		}
	};

	Handle<T> Get()
	{
		return Handle(this);
	}
};
