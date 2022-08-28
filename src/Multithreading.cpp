#define RWLOCK_CPP
#include "RWLock/RWLock.cpp"

struct Mutex;
void SYSMutexLock(Mutex m, u64 timeout);
void SYSMutexUnlock(Mutex m);

class ScopedLockRead
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

class ScopedLockWrite
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

template <typename T>
class SafeContainer
{
public:
	T content;
	RWLockReentrant rwLock;

	const T &LockForRead()
	{
		rwLock.StartRead();
		return content;
	}

	void UnlockForRead()
	{
		rwLock.EndRead();
	}

	T &LockForWrite()
	{
		rwLock.StartWrite();
		return content;
	}

	void UnlockForWrite()
	{
		rwLock.EndWrite();
	}

	template <typename T>
	class HandleRead
	{
	public:
		SafeContainer<T> *safeContainer;

		HandleRead(SafeContainer<T> *safe)
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
		SafeContainer<T> *safeContainer;

		HandleWrite(SafeContainer<T> *safe)
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
class SafeContainer2
{
public:
	T content;
	SRWLOCK rwLock;

	SafeContainer2()
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
		SafeContainer2<T> *safeContainer;

		HandleRead(SafeContainer2<T> *safe)
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
		SafeContainer2<T> *safeContainer;

		HandleWrite(SafeContainer2<T> *safe)
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
