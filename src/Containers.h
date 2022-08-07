template <typename T>
struct ArrayView
{
	T *data;
	u64 size;

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
};

template <typename T, typename A>
void ArrayInit(Array<T, A> *array, u64 capacity)
{
	array->data = (T*)A::Alloc(sizeof(T) * capacity);
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
};

template <typename T, u64 capacity>
T *FixedArrayAdd(FixedArray<T, capacity> *array)
{
	T *result = &array->data[array->size++];
	ASSERT(array->size < capacity);
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
};

template <typename T, typename A>
void DynamicArrayInit(DynamicArray<T, A> *array, u64 initialCapacity)
{
	ASSERT(initialCapacity);
	array->data = (T*)A::Alloc(sizeof(T) * initialCapacity);
	array->size = 0;
	array->capacity = initialCapacity;
}

template <typename T, typename A>
T *DynamicArrayAdd(DynamicArray<T, A> *array)
{
	ASSERT(array->capacity != 0);
	if (array->size >= array->capacity)
	{
		array->capacity *= 2;
		array->data = (T*)A::Realloc(array->data, array->capacity * sizeof(T));
	}
	return &array->data[array->size++];
}

template <typename T, typename A>
T *DynamicArrayAddMany(DynamicArray<T, A> *array, s64 count)
{
	bool reallocate = false;
	u64 newSize = array->size + count;
	while (newSize > array->capacity)
	{
		array->capacity *= 2;
	}
	if (reallocate)
		array->data = (T*)A::Realloc(array->data, array->capacity * sizeof(T));

	T *first = &array->data[array->size];
	array->size = newSize;
	return first;
}

template <typename T, typename A>
bool DynamicArrayAddUnique(DynamicArray<T, A> *array, T value)
{
	for (int i = 0; i < array->size; ++i)
	{
		if ((*array)[i] == value)
			return false;
	}
	if (array->size >= array->capacity)
	{
		array->capacity *= 2;
		array->data = (T*)A::Realloc(array->data, array->capacity * sizeof(T));
	}
	array->data[array->size++] = value;
	return true;
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

template <typename T, typename A, u64 bucketSize>
struct BucketArray
{
	DynamicArray<Array<T, A>, A> buckets;

	T &operator[](s64 idx)
	{
		s64 bucketIdx = idx / bucketSize;
		ASSERT((u64)bucketIdx < buckets.size);

		return buckets[bucketIdx][idx % bucketSize];
	}
};

template <typename T, typename A, u64 bucketSize>
void BucketArrayInit(BucketArray<T, A, bucketSize> *bucketArray)
{
	DynamicArrayInit(&bucketArray->buckets, 4);

	// Start with one bucket
	Array<T, A> *firstBucket = DynamicArrayAdd(&bucketArray->buckets);
	ArrayInit(firstBucket, bucketSize);
}

template <typename T, typename A, u64 bucketSize>
T *BucketArrayAdd(BucketArray<T, A, bucketSize> *bucketArray)
{
	ASSERT(bucketArray->buckets.size > 0);
	Array<T, A> *lastBucket = &bucketArray->buckets[bucketArray->buckets.size - 1];

	if (lastBucket->size >= bucketSize)
	{
		Array<T, A> *newBucket = DynamicArrayAdd(&bucketArray->buckets);
		ArrayInit(newBucket, bucketSize);
		lastBucket = newBucket;
	}

	return ArrayAdd(lastBucket);
}

template <typename T, typename A, u64 bucketSize>
T *BucketArrayBack(BucketArray<T, A, bucketSize> *bucketArray)
{
	ASSERT(bucketArray->buckets.size > 0);
	return DynamicArrayBack(&bucketArray->buckets[bucketArray->buckets.size - 1]);
}

template <typename T, typename A, u64 bucketSize>
u64 BucketArrayCount(BucketArray<T, A, bucketSize> *bucketArray)
{
	if (bucketArray->buckets.size == 0)
		return 0;
	u64 lastBucket = bucketArray->buckets.size - 1;
	u64 count = lastBucket * bucketSize;
	count += bucketArray->buckets[lastBucket].size;
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
	u8 *bookkeepBitfield;
	K *keys;
	u32 capacity;
};

template <typename K, typename A>
void HashSetInit(HashSet<K,A> *hashSet, u32 capacity)
{
	u64 bookkeepSize = capacity / 8;
	u64 keyMemorySize = capacity * sizeof(K);
	void *memory = A::Alloc(bookkeepSize + keyMemorySize);
	hashSet->bookkeepBitfield = (u8 *)memory;
	hashSet->keys = (K *)((u8 *)memory + bookkeepSize);
	hashSet->capacity = capacity;

	memset(memory, 0, bookkeepSize);
}

template <typename K, typename A>
inline bool HashSetSlotOccupied(HashSet<K,A> hashSet, u32 slotIdx)
{
	return hashSet.bookkeepBitfield[slotIdx >> 3] & (1 << (slotIdx & 7));
}

template <typename K, typename A>
bool HashSetHas(HashSet<K,A> hashSet, K key)
{
	ASSERT(IsPowerOf2(hashSet.capacity));
	u32 mask = hashSet.capacity - 1;
	u32 hash = Hash(key) & mask;
	K foundKey;
	for (u32 iterations = 0; iterations < hashSet.capacity; ++iterations)
	{
		bool occupied = HashSetSlotOccupied(hashSet, hash);
		if (!occupied)
			break;
		foundKey = hashSet.keys[hash];
		if (foundKey == key)
			return true;
		hash = (hash + 1) & mask;
	}
	return false;
}

template <typename K, typename A>
void HashSetRehash(HashSet<K,A> *hashSet)
{
	u32 oldCapacity = hashSet->capacity;
	u8 *oldBookkeep = hashSet->bookkeepBitfield;
	K *oldKeys = hashSet->keys;

	hashSet->capacity = oldCapacity << 1;

	u32 capacity = hashSet->capacity;
	u64 bookkeepSize = capacity / 8;
	u64 keyMemorySize = capacity * sizeof(K);
	void *memory = A::Alloc(bookkeepSize + keyMemorySize);
	hashSet->bookkeepBitfield = (u8 *)memory;
	hashSet->keys = (K *)((u8 *)memory + bookkeepSize);

	for (u32 i = 0; i < oldCapacity; ++i)
		if (oldBookkeep[i / 8] & (1 << (1 % 8)))
			HashSetAdd(hashSet, oldKeys[i]);
}

template <typename K, typename A>
bool HashSetAdd(HashSet<K,A> *hashSet, K key)
{
	ASSERT(IsPowerOf2(hashSet->capacity));
	u32 mask = hashSet->capacity - 1;
	u32 hash = Hash(key) & mask;
	K foundKey;
	for (u32 iterations = 0; iterations < hashSet->capacity; ++iterations)
	{
		bool occupied = HashSetSlotOccupied(*hashSet, hash);
		if (!occupied)
			goto add;
		foundKey = hashSet->keys[hash];
		if (foundKey == key)
			return false;
		hash = (hash + 1) & mask;
	}

	// Full!
	HashSetRehash(hashSet);
	HashSetAdd(hashSet, key);
	return true;

add:
	// Add!
	hashSet->bookkeepBitfield[hash / 8] |= (1 << (hash % 8));
	hashSet->keys[hash] = key;
	return true;
}

template <typename K, typename A>
bool HashSetRemove(HashSet<K,A> *hashSet, K key)
{
	ASSERT(IsPowerOf2(hashSet->capacity));
	u32 mask = hashSet->capacity - 1;
	u32 hash = Hash(key) & mask;
	K foundKey;
	for (u32 iterations = 0; iterations < hashSet->capacity; ++iterations)
	{
		bool occupied = hashSet->bookkeepBitfield[hash / 8] & (1 << (hash % 8));
		if (!occupied)
			return false;
		foundKey = hashSet->keys[hash];
		if (foundKey == key)
		{
			// Remove
			hashSet->bookkeepBitfield[hash / 8] &= (~(1 << (hash % 8)));
			return true;
		}
		hash = (hash + 1) & mask;
	}
	return false;
}

template <typename K, typename V, typename A>
struct HashMap
{
	u8 *bookkeepBitfield;
	K *keys;
	V *values;
	u32 capacity;
};

template <typename K, typename V, typename A>
void HashMapInit(HashMap<K,V,A> *hashMap, u32 capacity)
{
	u64 bookkeepSize = capacity / 8;
	u64 keyMemorySize = capacity * sizeof(K);
	u64 valueMemorySize = capacity * sizeof(V);
	void *memory = A::Alloc(bookkeepSize + keyMemorySize + valueMemorySize);
	hashMap->bookkeepBitfield = (u8 *)memory;
	hashMap->keys = (K *)((u8 *)memory + bookkeepSize);
	hashMap->values = (V *)((u8 *)memory + bookkeepSize + keyMemorySize);
	hashMap->capacity = capacity;

	memset(memory, 0, bookkeepSize);
}

template <typename K, typename V, typename A>
V *HashMapGet(HashMap<K,V,A> hashMap, K key)
{
	ASSERT(IsPowerOf2(hashMap.capacity));
	u32 mask = hashMap.capacity - 1;
	u32 hash = Hash(key) & mask;
	K foundKey;
	for (u32 iterations = 0; iterations < hashMap.capacity; ++iterations)
	{
		bool occupied = hashMap.bookkeepBitfield[hash / 8] & (1 << hash % 8);
		if (!occupied)
			break;
		foundKey = hashMap.keys[hash];
		if (foundKey == key)
			return &hashMap.values[hash];
		hash = (hash + 1) & mask;
	}
	return nullptr;
}

template <typename K, typename V, typename A>
void HashMapRehash(HashMap<K,V,A> *hashMap)
{
	u32 oldCapacity = hashMap->capacity;
	u8 *oldBookkeep = hashMap->bookkeepBitfield;
	K *oldKeys = hashMap->keys;
	V *oldValues = hashMap->values;

	hashMap->capacity = oldCapacity << 1;

	u32 capacity = hashMap->capacity;
	u64 bookkeepSize = capacity / 8;
	u64 keyMemorySize = capacity * sizeof(K);
	u64 valueMemorySize = capacity * sizeof(V);
	void *memory = A::Alloc(bookkeepSize + keyMemorySize + valueMemorySize);
	hashMap->bookkeepBitfield = (u8 *)memory;
	hashMap->keys = (K *)((u8 *)memory + bookkeepSize);
	hashMap->values = (V *)((u8 *)memory + bookkeepSize + keyMemorySize);

	for (u32 i = 0; i < oldCapacity; ++i)
		if (oldBookkeep[i / 8] & (1 << (1 % 8)))
			*HashMapGetOrAdd(hashMap, oldKeys[i]) = oldValues[i];
}

template <typename K, typename V, typename A>
V *HashMapGetOrAdd(HashMap<K,V,A> *hashMap, K key)
{
	ASSERT(IsPowerOf2(hashMap->capacity));
	u32 mask = hashMap->capacity - 1;
	u32 hash = Hash(key) & mask;
	K foundKey;
	for (u32 iterations = 0; iterations < hashMap->capacity; ++iterations)
	{
		bool occupied = hashMap->bookkeepBitfield[hash / 8] & (1 << (hash % 8));
		if (!occupied)
			goto add;
		foundKey = hashMap->keys[hash];
		if (foundKey == key)
			return &hashMap->values[hash];
		hash = (hash + 1) & mask;
	}

	// Full!
	HashMapRehash(hashMap);
	return HashMapGetOrAdd(hashMap, key);

add:
	// Add!
	hashMap->bookkeepBitfield[hash / 8] |= (1 << (hash % 8));
	hashMap->keys[hash] = key;
	return &hashMap->values[hash];
}
