template <typename T>
struct Array
{
	T *data;
	u64 size;
#if DEBUG_BUILD
	u64 _capacity;
#endif

	T &operator[](int idx)
	{
		ASSERT(idx >= 0 && (u64)idx < _capacity);
		return data[idx];
	}

	const T &operator[](int idx) const
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
	return &array->data[array->size++];
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
