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
