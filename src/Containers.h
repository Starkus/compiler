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

template <typename T>
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

template <typename T>
void DynamicArrayInit(DynamicArray<T> *array, u64 initialCapacity, void *(*allocFunc)(u64))
{
	array->data = (T*)allocFunc(sizeof(T) * initialCapacity);
	array->size = 0;
	array->capacity = initialCapacity;
}

template <typename T>
T *DynamicArrayAdd(DynamicArray<T> *array, void *(*reallocFunc)(void *, u64))
{
	if (array->size >= array->capacity)
	{
		array->capacity *= 2;
		array->data = (T*)reallocFunc(array->data, array->capacity * sizeof(T));
	}
	return &array->data[array->size++];
}

template <typename T>
T *DynamicArrayAddMany(DynamicArray<T> *array, s64 count, void *(*reallocFunc)(void *, u64))
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
