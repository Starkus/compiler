template <typename T>
struct ArrayView
{
	T *data;
	u64 size;

	T &operator[](s64 idx)
	{
#if DEBUG_BUILD
		ASSERT(idx >= 0 && (u64)idx < size);
#endif
		return data[idx];
	}

	const T &operator[](s64 idx) const
	{
#if DEBUG_BUILD
		ASSERT(idx >= 0 && (u64)idx < size);
#endif
		return data[idx];
	}
};

template <typename T, typename A>
struct Array
{
	T *data;
	u64 size;
#if DEBUG_BUILD
	u64 _capacity;
#endif

	T &operator[](s64 idx)
	{
#if DEBUG_BUILD
		ASSERT(idx >= 0 && (u64)idx < _capacity);
#endif
		return data[idx];
	}

	const T &operator[](s64 idx) const
	{
#if DEBUG_BUILD
		ASSERT(idx >= 0 && (u64)idx < _capacity);
#endif
		return data[idx];
	}

	operator ArrayView<T>()
	{
		return { data, size };
	}

	operator ArrayView<const T>() const
	{
		return { data, size };
	}
};

template <typename T, typename A>
void ArrayInit(Array<T, A> *array, u64 capacity)
{
	array->data = (T*)A::Alloc(sizeof(T) * capacity, alignof(T));
	array->size = 0;
#if DEBUG_BUILD
	array->_capacity = capacity;
#endif
}

template <typename T, typename A>
T *ArrayAdd(Array<T, A> *array)
{
	T *result = &array->data[array->size++];
#if DEBUG_BUILD
	ASSERT(array->size <= array->_capacity);
#endif
	return result;
}

// Good for when another thread is reading this array and we don't want to use locks.
template <typename T, typename A>
void ArrayAddMT(Array<T, A> *array, T value)
{
	array->data[array->size] = value;
	++array->size;
#if DEBUG_BUILD
	ASSERT(array->size <= array->_capacity);
#endif
}

template <typename T, typename A>
inline T *ArrayBack(Array<T, A> *array)
{
	ASSERT(array->size > 0);
	return &array->data[array->size - 1];
}

template <typename T, u64 capacity>
struct FixedArray
{
	T data[capacity];
	u64 size;

	T &operator[](s64 idx)
	{
		ASSERT(idx >= 0 && (u64)idx < capacity);
		return data[idx];
	}

	const T &operator[](s64 idx) const
	{
		ASSERT(idx >= 0 && (u64)idx < capacity);
		return data[idx];
	}

	operator ArrayView<T>()
	{
		return { data, size };
	}

	operator ArrayView<const T>() const
	{
		return { data, size };
	}
};

template <typename T, u64 capacity>
T *FixedArrayAdd(FixedArray<T, capacity> *array)
{
	ASSERT(array->size < capacity);
	T *result = &array->data[array->size++];
	return result;
}

template <typename T, u64 capacity>
inline T *FixedArrayBack(FixedArray<T, capacity> *array)
{
	ASSERT(array->size > 0);
	return &array->data[array->size - 1];
}

template <typename T, typename A>
struct DynamicArray
{
	T *data;
	u64 size;
	u64 capacity;

	T &operator[](s64 idx)
	{
		ASSERT(idx >= 0 && (u64)idx < capacity);
		return data[idx];
	}

	const T &operator[](s64 idx) const
	{
		ASSERT(idx >= 0 && (u64)idx < capacity);
		return data[idx];
	}

	operator ArrayView<T>()
	{
		return { data, size };
	}

	operator ArrayView<const T>() const
	{
		return { data, size };
	}
};

template <typename T, typename A>
void DynamicArrayInit(DynamicArray<T, A> *array, u64 initialCapacity)
{
	ASSERT(initialCapacity);
	array->data = (T*)A::Alloc(sizeof(T) * initialCapacity, alignof(T));
	array->size = 0;
	array->capacity = initialCapacity;
}

template <typename T, typename A>
T *DynamicArrayAdd(DynamicArray<T, A> *array)
{
	ASSERT(array->capacity != 0);
	if (array->size >= array->capacity) {
		u64 newCapacity = array->capacity * 2;
		array->data = (T*)A::Realloc(array->data, array->capacity * sizeof(T),
				newCapacity * sizeof(T), alignof(T));
		array->capacity = newCapacity;
	}
	return &array->data[array->size++];
}

// Good for when another thread is reading this array and we don't want to use locks.
template <typename T, typename A>
T *DynamicArrayAddMT(DynamicArray<T, A> *array, T value)
{
	ASSERT(array->capacity != 0);
	if (array->size >= array->capacity) {
		u64 newCapacity = array->capacity * 2;
		array->data = (T*)A::Realloc(array->data, array->capacity * sizeof(T),
				newCapacity * sizeof(T), alignof(T));
		array->capacity = newCapacity;
	}
	T *result = &array->data[array->size];
	*result = value;
	++array->size;
	return result;
}

template <typename T, typename A>
T *DynamicArrayAddMany(DynamicArray<T, A> *array, s64 count)
{
	u64 newSize = array->size + count;
	u64 newCapacity = array->capacity;
	while (newSize > newCapacity)
		newCapacity *= 2;
	if (newCapacity != array->capacity) {
		array->data = (T*)A::Realloc(array->data, array->capacity * sizeof(T),
				newCapacity * sizeof(T), alignof(T));
		array->capacity = newCapacity;
	}

	T *first = &array->data[array->size];
	array->size = newSize;
	return first;
}

template <typename T, typename A>
bool DynamicArrayAddUnique(DynamicArray<T, A> *array, T value)
{
	for (int i = 0; i < array->size; ++i) {
		if ((*array)[i] == value)
			return false;
	}
	if (array->size >= array->capacity) {
		u64 newCapacity = array->capacity * 2;
		array->data = (T*)A::Realloc(array->data, array->capacity * sizeof(T),
				newCapacity * sizeof(T), alignof(T));
		array->capacity = newCapacity;
	}
	array->data[array->size++] = value;
	return true;
}

template <typename T, typename A>
void DynamicArraySwapRemove(DynamicArray<T, A> *array, u64 idx)
{
	ASSERT(array->size > 0);
	(*array)[idx] = array->data[--array->size];
}

template <typename T, typename A>
void DynamicArrayRemoveOrdered(DynamicArray<T, A> *array, u64 idx)
{
	ASSERT(array->size > 0);
	ASSERT(idx >= 0 && (u64)idx < array->capacity);
	memmove(&array->data[idx], &array->data[idx + 1], (array->size - idx - 1) * sizeof(T));
	--array->size;
}

template <typename T, typename A>
T *DynamicArrayBack(DynamicArray<T, A> *array)
{
	ASSERT(array->size > 0);
	return &array->data[array->size - 1];
}

template <typename T, typename A>
void DynamicArrayCopy(DynamicArray<T, A> *dst,
		DynamicArray<T, A> *src)
{
	ASSERT(dst->capacity >= src->size);
	dst->size = src->size;
	memcpy(dst->data, src->data, src->size * sizeof(T));
}

template <typename T>
struct BucketArrayView
{
	ArrayView<T *> buckets;
	u64 count;
	u64 bucketSize;

	T &operator[](s64 idx)
	{
		s64 bucketIdx = idx / bucketSize;
		ASSERT((u64)bucketIdx < buckets.size);

		return buckets[bucketIdx][idx % bucketSize];
	}

	const T &operator[](s64 idx) const
	{
		s64 bucketIdx = idx / bucketSize;
		ASSERT((u64)bucketIdx < buckets.size);

		return buckets[bucketIdx][idx % bucketSize];
	}
};

template <typename T>
T *BucketArrayViewBack(BucketArrayView<T> *bucketArray)
{
	ASSERT(bucketArray->buckets.size > 0);
	return &(*ArrayViewBack(&bucketArray->buckets))[bucketArray->count % bucketArray->bucketSize];
}

template <typename T, typename A, u64 bucketSize>
struct BucketArray
{
	DynamicArray<T *, A> buckets;
	u64 count;

	T &operator[](s64 idx)
	{
		s64 bucketIdx = idx / bucketSize;
		ASSERT((u64)bucketIdx < buckets.size);
		ASSERT((u64)idx < count);
		return buckets[bucketIdx][idx % bucketSize];
	}

	const T &operator[](s64 idx) const
	{
		s64 bucketIdx = idx / bucketSize;
		ASSERT((u64)bucketIdx < buckets.size);
		ASSERT((u64)idx < count);
		return buckets[bucketIdx][idx % bucketSize];
	}

	operator BucketArrayView<T>()
	{
		BucketArrayView<T> result;
		result.buckets = buckets;
		result.count = count;
		result.bucketSize = bucketSize;
		return result;
	}

	operator BucketArrayView<const T>() const
	{
		BucketArrayView<T> result;
		result.buckets = buckets;
		result.count = count;
		result.bucketSize = bucketSize;
		return result;
	}
};

template <typename T, typename A, u64 bucketSize>
T *BucketArrayAllocateNewBucket(BucketArray<T, A, bucketSize> *bucketArray)
{
	T *newBucket = (T *)A::Alloc(sizeof(T) * bucketSize, alignof(T));
	// Call AddMT so we can call BucketArrayCount without locking without risking reading an
	// invalid bucket.
	DynamicArrayAddMT(&bucketArray->buckets, newBucket);
	return newBucket;
}

template <typename T, typename A, u64 bucketSize>
void BucketArrayInit(BucketArray<T, A, bucketSize> *bucketArray)
{
	bucketArray->count = 0;
	DynamicArrayInit(&bucketArray->buckets, 4);

	// Start with one bucket
	BucketArrayAllocateNewBucket(bucketArray);
}

template <typename T, typename A, u64 bucketSize>
T *BucketArrayAdd(BucketArray<T, A, bucketSize> *bucketArray)
{
	ASSERT(bucketArray->buckets.size > 0);
	u64 lastBucketCount = bucketArray->count % bucketSize;
	u64 bucketIdx = bucketArray->count / bucketSize;

	T *bucket;
	if (bucketIdx >= bucketArray->buckets.size) {
		bucket = BucketArrayAllocateNewBucket(bucketArray);
		lastBucketCount = 0;
	}
	else
		bucket = bucketArray->buckets[bucketIdx];

	++bucketArray->count;
	return &bucket[lastBucketCount];
}

// Good for when another thread is reading this array and we don't want to use locks.
template <typename T, typename A, u64 bucketSize>
void BucketArrayAddMT(BucketArray<T, A, bucketSize> *bucketArray, T value)
{
	ASSERT(bucketArray->buckets.size > 0);
	u64 lastBucketCount = bucketArray->count % bucketSize;
	u64 bucketIdx = bucketArray->count / bucketSize;

	T *bucket;
	if (lastBucketCount >= bucketSize) {
		bucket = BucketArrayAllocateNewBucket(bucketArray);
		lastBucketCount = 0;
	}
	else
		bucket = bucketArray->buckets[bucketIdx];

	bucket[lastBucketCount] = value;
	++bucketArray->count;
}

template <typename T, typename A, u64 bucketSize>
T *BucketArrayBack(BucketArray<T, A, bucketSize> *bucketArray)
{
	ASSERT(bucketArray->buckets.size > 0);
	return &(*DynamicArrayBack(&bucketArray->buckets))[bucketArray->count % bucketSize];
}

template <typename T>
inline bool BitfieldGetBit(T array, int index)
{
	ASSERT(IsPowerOf2(sizeof(array[0])));
	constexpr u8 shiftAmm = Ntz64Constexpr(sizeof(array[0]) * 8);
	constexpr u8 bitIdx = (sizeof(array[0]) * 8) - 1;
	return array[index >> shiftAmm] & (1ull << (index & bitIdx));
}

template <typename T>
inline void BitfieldSetBit(T array, int index)
{
	ASSERT(IsPowerOf2(sizeof(array[0])));
	constexpr u8 shiftAmm = Ntz64Constexpr(sizeof(array[0]) * 8);
	constexpr u8 bitIdx = (sizeof(array[0]) * 8) - 1;
	array[index >> shiftAmm] |= (1ull << (index & bitIdx));
}

template <typename T>
inline void BitfieldClearBit(T array, int index)
{
	ASSERT(IsPowerOf2(sizeof(array[0])));
	constexpr u8 shiftAmm = Ntz64Constexpr(sizeof(array[0]) * 8);
	constexpr u8 bitIdx = (sizeof(array[0]) * 8) - 1;
	array[index >> shiftAmm] &= ~(1ull << (index & bitIdx));
}

template <typename T>
inline u64 BitfieldCount(T *array, u64 size)
{
	ASSERTC(sizeof(T) % sizeof(u32) == 0);
	u64 count = 0;
	T *end = array + size;
	for (u32 *scan = (u32 *)array; scan < end; ++scan)
		count += CountOnes(*scan);
	return count;
}

// Specialization to use popcnt64
inline u64 BitfieldCount(u64 *array, u64 size)
{
	u64 count = 0;
	u64 *end = array + size;
	for (u64 *scan = array; scan < end; ++scan)
		count += CountOnes64(*scan);
	return count;
}

template <typename T, typename A>
inline u64 BitfieldCount(Array<T,A> array)
{
	ASSERT(sizeof(T) % sizeof(u32) == 0);
	u64 count = 0;
	T *end = array.data + array.size;
	for (u32 *scan = (u32 *)array.data; scan < end; ++scan)
		count += CountOnes(*scan);
	return count;
}

// Specialization to use popcnt64
template <typename A>
inline u64 BitfieldCount(Array<u64,A> array, u64 size)
{
	u64 count = 0;
	u64 *end = array.data + array.size;
	for (u64 *scan = array.data; scan < end; ++scan)
		count += CountOnes64(*scan);
	return count;
}

// From https://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key
u32 Hash(u32 value)
{
	value = ((value >> 16) ^ value) * 0x45d9f3b;
	value = ((value >> 16) ^ value) * 0x45d9f3b;
	value = ((value >> 16) ^ value);
	return value;
}

// djb2 from http://www.cse.yorku.ca/~oz/hash.html
u32 Hash(String value)
{
	u32 hash = 5381;

	const char *scan = value.data;
	for (int i = 0; i < value.size; ++i)
		hash = ((hash << 5) + hash) + *scan; // hash * 33 + *scan

	return hash;
}

template <typename K, typename A>
struct HashSet
{
	void *memory;
	u32 capacity;
};

template <typename K, typename A>
inline void HashSetClear(HashSet<K,A> hashSet)
{
	memset(hashSet.memory, 0, hashSet.capacity >> 3);
}

template <typename K, typename A>
inline void HashSetInit(HashSet<K,A> *hashSet, u32 capacity)
{
	ASSERT(IsPowerOf2(capacity) && capacity >= 32);

	u64 bookkeepSize = capacity >> 3; // divide by 8
	u64 keyMemorySize = capacity * sizeof(K);
	hashSet->memory = A::Alloc(bookkeepSize + keyMemorySize, alignof(K));
	hashSet->capacity = capacity;

	HashSetClear(*hashSet);
}

template <typename K, typename A>
inline K *HashSetKeys(HashSet<K,A> hashSet)
{
	u64 bookkeepSize = hashSet.capacity >> 3;
	return (K *)((u8 *)hashSet.memory + bookkeepSize);
}

template <typename K, typename A>
inline bool HashSetSlotOccupied(HashSet<K,A> hashSet, u32 slotIdx)
{
	u32 *bookkeep = (u32 *)hashSet.memory;
	return BitfieldGetBit(bookkeep, slotIdx);
}

template <typename K, typename A>
bool HashSetHas(HashSet<K,A> hashSet, K key)
{
	ASSERT(IsPowerOf2(hashSet.capacity));
	u32 mask = hashSet.capacity - 1;
	u32 slotIdx = Hash(key) & mask;

	K *keys = HashSetKeys(hashSet);

	K foundKey;
	for (u32 iterations = 0; iterations < hashSet.capacity; ++iterations)
	{
		if (!HashSetSlotOccupied(hashSet, slotIdx))
			return false;
		foundKey = keys[slotIdx];
		if (foundKey == key)
			return true;
		slotIdx = (slotIdx + 1) & mask;
	}
	return false;
}

template <typename K, typename A>
u64 HashSetCount(HashSet<K,A> hashSet)
{
	return BitfieldCount((u32 *)hashSet.memory, hashSet.capacity >> 5);
}

template <typename A>
void HashSetPrint(HashSet<u32,A> hashSet)
{
	Print("{ ");
	bool first = true;
	for (u32 slotIdx = 0; slotIdx < hashSet.capacity; ++slotIdx)
		if (HashSetSlotOccupied(hashSet, slotIdx))
		{
			if (!first)
				Print(", ");
			Print("%u", hashSet.keys[slotIdx]);
			first = false;
		}
	Print(" }\n");
}

template <typename K, typename A>
void HashSetRehash(HashSet<K,A> *hashSet)
{
	u32 oldCapacity = hashSet->capacity;
	u32 *oldBookkeep = (u32 *)hashSet->memory;
	K *oldKeys = HashSetKeys(*hashSet);

	HashSetInit(hashSet, oldCapacity << 1);

	for (u32 slotIdx = 0; slotIdx < oldCapacity; ++slotIdx)
		if (BitfieldGetBit(oldBookkeep, slotIdx))
			HashSetAdd(hashSet, oldKeys[slotIdx]);

	A::Free(oldBookkeep);
}

template <typename K, typename A>
bool HashSetAdd(HashSet<K,A> *hashSet, K key)
{
	ASSERT(IsPowerOf2(hashSet->capacity));
	u32 mask = hashSet->capacity - 1;
	u32 slotIdx = Hash(key) & mask;

	K *keys = HashSetKeys(*hashSet);

	K foundKey;
	u32 maxIterationsSquared = hashSet->capacity;
	for (u32 iteration = 0; iteration * iteration < maxIterationsSquared; ++iteration)
	{
		if (!HashSetSlotOccupied(*hashSet, slotIdx))
			goto add;
		foundKey = keys[slotIdx];
		if (foundKey == key)
			return false;
		slotIdx = (slotIdx + 1) & mask;
	}

	// Full!
	HashSetRehash(hashSet);
	HashSetAdd(hashSet, key);
	return true;

add:
	// Add!
	u32 *bookkeep = (u32 *)hashSet->memory;
	BitfieldSetBit(bookkeep, slotIdx);
	keys[slotIdx] = key;
	return true;
}

template <typename K, typename A>
bool HashSetRemove(HashSet<K,A> *hashSet, K key)
{
	ASSERT(IsPowerOf2(hashSet->capacity));
	u32 mask = hashSet->capacity - 1;
	u32 slotIdx = Hash(key) & mask;

	K *keys = HashSetKeys(*hashSet);

	K foundKey;
	for (u32 iterations = 0; iterations < hashSet->capacity; ++iterations)
	{
		if (!HashSetSlotOccupied(*hashSet, slotIdx))
			return false;
		foundKey = keys[slotIdx];
		if (foundKey == key)
		{
			// Remove
			u32 *bookkeep = (u32 *)hashSet->memory;
			BitfieldClearBit(bookkeep, slotIdx);
			return true;
		}
		slotIdx = (slotIdx + 1) & mask;
	}
	return false;
}

template <typename K, typename V, typename A>
struct HashMap
{
	void *memory;
	u32 capacity;
};

template <typename K, typename V, typename A>
inline void HashMapClear(HashMap<K,V,A> hashMap)
{
	memset(hashMap.memory, 0, hashMap.capacity >> 3);
}

template <typename K, typename V, typename A>
inline void HashMapInit(HashMap<K,V,A> *hashMap, u32 capacity)
{
	ASSERT(IsPowerOf2(capacity) && capacity >= 32);

	u64 bookkeepSize = capacity >> 3;
	u64 keyMemorySize = capacity * sizeof(K);
	u64 valueMemorySize = capacity * sizeof(V);
	hashMap->memory = A::Alloc(bookkeepSize + keyMemorySize + valueMemorySize, alignof(K));
	hashMap->capacity = capacity;

	HashMapClear(*hashMap);
}

template <typename K, typename V, typename A>
inline K *HashMapKeys(HashMap<K,V,A> hashMap)
{
	u64 bookkeepSize = hashMap.capacity >> 3;
	return (K *)((u8 *)hashMap.memory + bookkeepSize);
}

template <typename K, typename V, typename A>
inline V *HashMapValues(HashMap<K,V,A> hashMap)
{
	u64 bookkeepSize = hashMap.capacity >> 3;
	u64 keyMemorySize = hashMap.capacity * sizeof(K);
	return (V *)((u8 *)hashMap.memory + bookkeepSize + keyMemorySize);
}

template <typename K, typename V, typename A>
inline bool HashMapSlotOccupied(HashMap<K,V,A> hashMap, u32 slotIdx)
{
	u32 *bookkeep = (u32 *)hashMap.memory;
	return BitfieldGetBit(bookkeep, slotIdx);
}

template <typename K, typename V, typename A>
V *HashMapGet(HashMap<K,V,A> hashMap, K key)
{
	ASSERT(IsPowerOf2(hashMap.capacity));
	u32 mask = hashMap.capacity - 1;
	u32 slotIdx = Hash(key) & mask;

	K *keys = HashMapKeys(hashMap);
	V *values = HashMapValues(hashMap);

	K foundKey;
	for (u32 iterations = 0; iterations < hashMap.capacity; ++iterations)
	{
		if (!HashMapSlotOccupied(hashMap, slotIdx))
			break;
		foundKey = keys[slotIdx];
		if (foundKey == key)
			return &values[slotIdx];
		slotIdx = (slotIdx + 1) & mask;
	}
	return nullptr;
}

template <typename K, typename V, typename A>
void HashMapRehash(HashMap<K,V,A> *hashMap)
{
	u32 oldCapacity = hashMap->capacity;
	u32 *oldBookkeep = (u32 *)hashMap->memory;
	K *oldKeys = HashMapKeys(*hashMap);
	V *oldValues = HashMapValues(*hashMap);

	HashMapInit(hashMap, oldCapacity << 1);

	for (u32 i = 0; i < oldCapacity; ++i)
		if (BitfieldGetBit(oldBookkeep, i))
			*HashMapGetOrAdd(hashMap, oldKeys[i]) = oldValues[i];

	A::Free(oldBookkeep);
}

template <typename K, typename V, typename A>
V *HashMapGetOrAdd(HashMap<K,V,A> *hashMap, K key)
{
	ASSERT(IsPowerOf2(hashMap->capacity));
	u32 mask = hashMap->capacity - 1;
	u32 slotIdx = Hash(key) & mask;

	K *keys = HashMapKeys(*hashMap);
	V *values = HashMapValues(*hashMap);

	K foundKey;
	u32 maxIterationsSquared = hashMap->capacity;
	for (u32 iteration = 0; iteration * iteration < maxIterationsSquared; ++iteration)
	{
		if (!HashMapSlotOccupied(*hashMap, slotIdx))
			goto add;
		foundKey = keys[slotIdx];
		if (foundKey == key)
			return &values[slotIdx];
		slotIdx = (slotIdx + 1) & mask;
	}

	// Full!
	HashMapRehash(hashMap);
	return HashMapGetOrAdd(hashMap, key);

add:
	// Add!
	u32 *bookkeep = (u32 *)hashMap->memory;
	BitfieldSetBit(bookkeep, slotIdx);
	keys[slotIdx] = key;
	return &values[slotIdx];
}

template <typename K, typename V, typename A>
bool HashMapRemove(HashMap<K,V,A> *hashMap, K key)
{
	ASSERT(IsPowerOf2(hashMap->capacity));
	u32 mask = hashMap->capacity - 1;
	u32 slotIdx = Hash(key) & mask;

	K *keys = HashMapKeys(*hashMap);

	K foundKey;
	for (u32 iterations = 0; iterations < hashMap->capacity; ++iterations)
	{
		if (!HashMapSlotOccupied(*hashMap, slotIdx))
			return false;
		foundKey = keys[slotIdx];
		if (foundKey == key)
		{
			// Remove
			u32 *bookkeep = (u32 *)hashMap->memory;
			BitfieldClearBit(bookkeep, slotIdx);
			return true;
		}
		slotIdx = (slotIdx + 1) & mask;
	}
	return false;
}

bool PresentInBigArray(u32 *buffer, u64 count, u32 item)
{
	__m256i itemX8 = _mm256_set1_epi32(item);
	u32 currentIdx = 0;
	// Align to 32 bytes
	while (((u64)&buffer[currentIdx] & 31) && currentIdx < count)
	{
		if (buffer[currentIdx] == item)
			return true;
		++currentIdx;
	}
	while (currentIdx + 8 <= count)
	{
		__m256i res = _mm256_cmpeq_epi32(itemX8, *(__m256i *)&buffer[currentIdx]);
		u32 mask = _mm256_movemask_ps(_mm256_castsi256_ps(res));
		if (mask)
			return true;
		currentIdx += 8;
	}
	// Leftovers
	while (currentIdx < count)
	{
		if (buffer[currentIdx] == item)
			return true;
		++currentIdx;
	}
	return false;
}

u64 FindInBigArray(u32 *buffer, u64 count, u32 item)
{
	__m256i itemX8 = _mm256_set1_epi32(item);
	u32 currentIdx = 0;
	// Align to 32 bytes
	while (((u64)&buffer[currentIdx] & 31) && currentIdx < count)
	{
		if (buffer[currentIdx] == item)
			return currentIdx;
		++currentIdx;
	}
	while (currentIdx + 8 <= count)
	{
		__m256i res = _mm256_cmpeq_epi32(itemX8, *(__m256i *)&buffer[currentIdx]);
		u32 mask = _mm256_movemask_ps(_mm256_castsi256_ps(res));
		if (mask)
			return 31 - Nlz(mask) + currentIdx;
		currentIdx += 8;
	}
	// Leftovers
	while (currentIdx < count)
	{
		if (buffer[currentIdx] == item)
			return currentIdx;
		++currentIdx;
	}
	return U64_MAX;
}

template <typename T>
struct MTQueue {
	T *buffer;
	u32 capacity;
	volatile u32 head;
	volatile u32 tail;
	volatile u32 headLock;
	volatile u32 tailLock;
};

template <typename Allocator, typename T>
void MTQueueInit(MTQueue<T> *queue, u32 capacity) {
	*queue = {
		.buffer = (T *)Allocator::Alloc(sizeof(T) * capacity, alignof(T)),
		.capacity = capacity,
		.head = 0,
		.tail = 0,
		.headLock = 0,
		.tailLock = 0 };
}

template <typename T>
bool MTQueueEnqueue(MTQueue<T> *queue, T item)
{
	SpinlockLock(&queue->tailLock);
	u32 tail = queue->tail;
	if (tail + 1 == queue->head) {
		SpinlockUnlock(&queue->tailLock);
		return false;
	}
	else {
		queue->buffer[tail] = item;
		queue->tail = (tail + 1) % queue->capacity;
		SpinlockUnlock(&queue->tailLock);
		return true;
	}
}

template <typename T>
bool MTQueueDequeue(MTQueue<T> *queue, T *item)
{
	bool result = false;
	SpinlockLock(&queue->headLock);
	u32 head = queue->head;
	u32 tail = queue->tail;
	if (head != tail) {
		queue->head = (head + 1) % queue->capacity;
		*item = queue->buffer[head];
#if DEBUG_BUILD
		memset(&queue->buffer[head], 0xFE, sizeof(T));
#endif
		result = true;
	}
	SpinlockUnlock(&queue->headLock);
	return result;
}

template <typename T>
bool MTQueueIsEmpty(MTQueue<T> *queue) {
	SpinlockLock(&queue->headLock);
	SpinlockLock(&queue->tailLock);
	u32 head = queue->head;
	u32 tail = queue->tail;
	SpinlockUnlock(&queue->tailLock);
	SpinlockUnlock(&queue->headLock);
	return head == tail;
}
