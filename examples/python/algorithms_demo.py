def bubble_sort(arr):
    n = len(arr)
    for i in range(n - 1):
        for j in range(n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
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

def binary_search(arr, target):
    left, right = 0, len(arr) - 1
    
    while left <= right:
        mid = (left + right) // 2
        
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1

def linear_search(arr, target):
    for i in range(len(arr)):
        if arr[i] == target:
            return i
    return -1

def find_max(arr):
    if not arr:
        return None
    
    max_val = arr[0]
    for val in arr:
        if val > max_val:
            max_val = val
    return max_val

def find_min(arr):
    if not arr:
        return None
    
    min_val = arr[0]
    for val in arr:
        if val < min_val:
            min_val = val
    return min_val

def calculate_average(arr):
    if not arr:
        return 0
    
    total = sum(arr)
    return total / len(arr)

def main():
    print("Python Algorithms and Data Structures Demo")
    print("=" * 45)
    
    # Test data
    test_array = [64, 34, 25, 12, 22, 11, 90, 5]
    print(f"\nOriginal array: {test_array}")
    
    # Test bubble sort
    bubble_sorted = bubble_sort(test_array.copy())
    print(f"Bubble sorted: {bubble_sorted}")
    
    # Test selection sort
    selection_sorted = selection_sort(test_array.copy())
    print(f"Selection sorted: {selection_sorted}")
    
    # Test search algorithms
    sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    target = 7
    
    print(f"\nSearching for {target} in {sorted_array}")
    
    binary_result = binary_search(sorted_array, target)
    linear_result = linear_search(sorted_array, target)
    
    print(f"Binary search result: index {binary_result}")
    print(f"Linear search result: index {linear_result}")
    
    # Test min/max/average
    numbers = [45, 23, 78, 12, 67, 34, 89, 56]
    print(f"\nArray: {numbers}")
    print(f"Maximum: {find_max(numbers)}")
    print(f"Minimum: {find_min(numbers)}")
    print(f"Average: {calculate_average(numbers):.2f}")
    
    # Test with multiple arrays
    test_arrays = [
        [5, 2, 8, 1, 9],
        [100, 50, 75, 25, 80],
        [3, 1, 4, 1, 5, 9, 2, 6]
    ]
    
    print("\nTesting with multiple arrays:")
    for i, arr in enumerate(test_arrays):
        original = arr.copy()
        sorted_arr = bubble_sort(arr)
        print(f"Array {i + 1}: {original} -> {sorted_arr}")
    
    print("\nDemo completed successfully!")

if __name__ == "__main__":
    main()