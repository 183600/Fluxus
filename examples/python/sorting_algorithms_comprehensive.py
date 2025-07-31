def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
    return arr

def quick_sort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quick_sort(left) + middle + quick_sort(right)

def merge_sort(arr):
    if len(arr) <= 1:
        return arr
    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])
    return merge(left, right)

def merge(left, right):
    result = []
    i, j = 0, 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    result.extend(left[i:])
    result.extend(right[j:])
    return result

def heap_sort(arr):
    def heapify(arr, n, i):
        largest = i
        left = 2 * i + 1
        right = 2 * i + 2
        if left < n and arr[left] > arr[largest]:
            largest = left
        if right < n and arr[right] > arr[largest]:
            largest = right
        if largest != i:
            arr[i], arr[largest] = arr[largest], arr[i]
            heapify(arr, n, largest)
    
    n = len(arr)
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i)
    for i in range(n - 1, 0, -1):
        arr[0], arr[i] = arr[i], arr[0]
        heapify(arr, i, 0)
    return arr

def insertion_sort(arr):
    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key
    return arr

def selection_sort(arr):
    n = len(arr)
    for i in range(n):
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j
        arr[i], arr[min_idx] = arr[min_idx], arr[i]
    return arr

def counting_sort(arr):
    if not arr:
        return arr
    max_val = max(arr)
    min_val = min(arr)
    range_val = max_val - min_val + 1
    count = [0] * range_val
    output = [0] * len(arr)
    
    for num in arr:
        count[num - min_val] += 1
    
    for i in range(1, len(count)):
        count[i] += count[i - 1]
    
    for i in range(len(arr) - 1, -1, -1):
        output[count[arr[i] - min_val] - 1] = arr[i]
        count[arr[i] - min_val] -= 1
    
    return output

def radix_sort(arr):
    if not arr:
        return arr
    
    def counting_sort_for_radix(arr, exp):
        n = len(arr)
        output = [0] * n
        count = [0] * 10
        
        for i in range(n):
            index = arr[i] // exp
            count[index % 10] += 1
        
        for i in range(1, 10):
            count[i] += count[i - 1]
        
        i = n - 1
        while i >= 0:
            index = arr[i] // exp
            output[count[index % 10] - 1] = arr[i]
            count[index % 10] -= 1
            i -= 1
        
        for i in range(n):
            arr[i] = output[i]
    
    max_val = max(arr)
    exp = 1
    while max_val // exp > 0:
        counting_sort_for_radix(arr, exp)
        exp *= 10
    
    return arr

def bucket_sort(arr):
    if not arr:
        return arr
    
    bucket_count = len(arr)
    max_val = max(arr)
    min_val = min(arr)
    
    buckets = [[] for _ in range(bucket_count)]
    
    for num in arr:
        index = int((num - min_val) * bucket_count / (max_val - min_val + 1))
        if index == bucket_count:
            index = bucket_count - 1
        buckets[index].append(num)
    
    for bucket in buckets:
        insertion_sort(bucket)
    
    result = []
    for bucket in buckets:
        result.extend(bucket)
    
    return result

def test_sorting_algorithms():
    test_arrays = [
        [64, 34, 25, 12, 22, 11, 90],
        [5, 2, 4, 6, 1, 3],
        [1],
        [],
        [3, 3, 3, 3],
        [9, 8, 7, 6, 5, 4, 3, 2, 1]
    ]
    
    algorithms = [
        ("Bubble Sort", bubble_sort),
        ("Quick Sort", quick_sort),
        ("Merge Sort", merge_sort),
        ("Heap Sort", heap_sort),
        ("Insertion Sort", insertion_sort),
        ("Selection Sort", selection_sort),
        ("Counting Sort", counting_sort),
        ("Radix Sort", radix_sort),
        ("Bucket Sort", bucket_sort)
    ]
    
    for name, algorithm in algorithms:
        print(f"Testing {name}:")
        for i, arr in enumerate(test_arrays):
            original = arr.copy()
            sorted_arr = algorithm(arr.copy())
            expected = sorted(original)
            status = "PASS" if sorted_arr == expected else "FAIL"
            print(f"  Test {i + 1}: {original} -> {sorted_arr} [{status}]")
        print()

if __name__ == "__main__":
    test_sorting_algorithms()