inline void SpinlockLock(volatile u32 *locked);
inline bool SpinlockTryLock(volatile u32 *locked);
inline void SpinlockUnlock(volatile u32 *locked);

inline void RWSpinlockLockForRead(volatile u32 *lock);
inline void RWSpinlockUnlockForRead(volatile u32 *lock);
inline void RWSpinlockLockForWrite(volatile u32 *lock);
inline void RWSpinlockUnlockForWrite(volatile u32 *lock);

inline s32 AtomicCompareExchange(volatile s32 *destination, s32 exchange, s32 comparand);
inline s64 AtomicCompareExchange64(volatile s64 *destination, s64 exchange, s64 comparand);
inline s32 AtomicIncrementGetNew(volatile s32 *destination);
inline s32 AtomicAddGetNew(volatile s32 *destination, s32 addend);
