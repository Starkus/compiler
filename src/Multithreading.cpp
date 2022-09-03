struct Mutex;
void SYSMutexLock(Mutex m, u64 timeout);
void SYSMutexUnlock(Mutex m);

class [[nodiscard]] ScopedLockRead
{
public:
	SRWLOCK *lock;
	ScopedLockRead(SRWLOCK *aLock)
	{
		lock = aLock;
		AcquireSRWLockShared(lock);
	}
	~ScopedLockRead()
	{
		ReleaseSRWLockShared(lock);
	}
};

class [[nodiscard]] ScopedLockWrite
{
public:
	SRWLOCK *lock;
	ScopedLockWrite(SRWLOCK *aLock)
	{
		lock = aLock;
		AcquireSRWLockExclusive(lock);
	}
	~ScopedLockWrite()
	{
		ReleaseSRWLockExclusive(lock);
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

template <typename T>
class RWContainer
{
public:
	T content;
	SRWLOCK rwLock;

	RWContainer()
	{
		InitializeSRWLock(&rwLock);
	}

	const T &LockForRead()
	{
		AcquireSRWLockShared(&rwLock);
		return content;
	}

	void UnlockForRead()
	{
		ReleaseSRWLockShared(&rwLock);
	}

	T &LockForWrite()
	{
		AcquireSRWLockExclusive(&rwLock);
		return content;
	}

	void UnlockForWrite()
	{
		ReleaseSRWLockExclusive(&rwLock);
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

		const T &operator*()
		{
			return safeContainer->content;
		}

		const T *operator->()
		{
			return &safeContainer->content;
		}

		const T *operator&()
		{
			return &safeContainer->content;
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
			return safeContainer->content;
		}

		T *operator->()
		{
			return &safeContainer->content;
		}

		T *operator&()
		{
			return &safeContainer->content;
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
	T content;
	volatile u32 lock;

	const T &Lock()
	{
		SYSSpinlockLock(&lock);
		return content;
	}

	void Unlock()
	{
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
			return safeContainer->content;
		}

		T *operator->()
		{
			return &safeContainer->content;
		}

		T *operator&()
		{
			return &safeContainer->content;
		}
	};

	Handle<T> Get()
	{
		return Handle(this);
	}
};
