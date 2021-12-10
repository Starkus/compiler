template <typename T>
struct Array
{
	T *data;
	u64 size;
#if DEBUG_BUILD
	u64 _capacity;
#endif

	T &operator[](s64 idx)
	{
		ASSERT(idx >= 0 && (u64)idx < _capacity);
		return data[idx];
	}

	const T &operator[](s64 idx) const
	{
		ASSERT(idx >= 0 && (u64)idx < _capacity);
		return data[idx];
	}
};

template <typename T>
void ArrayInit(Array<T> *array, u64 capacity, void *(*allocFunc)(u64))
{
	array->data = (T*)allocFunc(sizeof(T) * capacity);
	array->size = 0;
#if DEBUG_BUILD
	array->_capacity = capacity;
#endif
}

template <typename T>
T *ArrayAdd(Array<T> *array)
{
	T *result = &array->data[array->size++];
#if DEBUG_BUILD
	ASSERT(array->size <= array->_capacity);
#endif
	return result;
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

template <typename T, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
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

template <typename T, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
void DynamicArrayInit(DynamicArray<T, allocFunc, reallocFunc> *array, u64 initialCapacity)
{
	ASSERT(initialCapacity);
	array->data = (T*)allocFunc(sizeof(T) * initialCapacity);
	array->size = 0;
	array->capacity = initialCapacity;
}

template <typename T, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
T *DynamicArrayAdd(DynamicArray<T, allocFunc, reallocFunc> *array)
{
	if (array->size >= array->capacity)
	{
		array->capacity *= 2;
		array->data = (T*)reallocFunc(array->data, array->capacity * sizeof(T));
	}
	return &array->data[array->size++];
}

template <typename T, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
T *DynamicArrayAddMany(DynamicArray<T, allocFunc, reallocFunc> *array, s64 count)
{
	bool reallocate = false;
	u64 newSize = array->size + count;
	while (newSize > array->capacity)
	{
		array->capacity *= 2;
	}
	if (reallocate)
		array->data = (T*)reallocFunc(array->data, array->capacity * sizeof(T));

	T *first = &array->data[array->size];
	array->size = newSize;
	return first;
}

template <typename T, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
bool DynamicArrayAddUnique(DynamicArray<T, allocFunc, reallocFunc> *array, T value)
{
	for (int i = 0; i < array->size; ++i)
	{
		if ((*array)[i] == value)
			return false;
	}
	if (array->size >= array->capacity)
	{
		array->capacity *= 2;
		array->data = (T*)reallocFunc(array->data, array->capacity * sizeof(T));
	}
	array->data[array->size++] = value;
	return true;
}

template <typename T, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
void DynamicArrayCopy(DynamicArray<T, allocFunc, reallocFunc> *dst,
		DynamicArray<T, allocFunc, reallocFunc> *src)
{
	ASSERT(dst->capacity >= src->size);
	dst->size = src->size;
	memcpy(dst->data, src->data, src->size * sizeof(T));
}

template <typename T, u64 bucketSize, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
struct BucketArray
{
	DynamicArray<Array<T>, allocFunc, reallocFunc> buckets;

	T &operator[](s64 idx)
	{
		s64 bucketIdx = idx / bucketSize;
		ASSERT((u64)bucketIdx < buckets.size);

		return buckets[bucketIdx][idx % bucketSize];
	}
};

template <typename T, u64 bucketSize, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
void BucketArrayInit(BucketArray<T, bucketSize, allocFunc, reallocFunc> *bucketArray)
{
	DynamicArrayInit(&bucketArray->buckets, 4);

	// Start with one bucket
	Array<T> *firstBucket = DynamicArrayAdd(&bucketArray->buckets);
	ArrayInit(firstBucket, bucketSize, allocFunc);
}

template <typename T, u64 bucketSize, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
T *BucketArrayAdd(BucketArray<T, bucketSize, allocFunc, reallocFunc> *bucketArray)
{
	ASSERT(bucketArray->buckets.size > 0);
	Array<T> *lastBucket = &bucketArray->buckets[bucketArray->buckets.size - 1];

	if (lastBucket->size >= bucketSize)
	{
		Array<T> *newBucket = DynamicArrayAdd(&bucketArray->buckets);
		ArrayInit(newBucket, bucketSize, allocFunc);
		lastBucket = newBucket;
	}

	return ArrayAdd(lastBucket);
}

template <typename T, u64 bucketSize, void *(*allocFunc)(u64), void *(*reallocFunc)(void *, u64)>
u64 BucketArrayCount(BucketArray<T, bucketSize, allocFunc, reallocFunc> *bucketArray)
{
	u64 count = (bucketArray->buckets.size - 1) * bucketSize;
	count += bucketArray->buckets[bucketArray->buckets.size - 1].size;
	return count;
}
