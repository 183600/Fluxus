def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def prime_check(n):
    if n < 2:
        return False
    i = 2
    while i * i <= n:
        if n % i == 0:
            return False
        i = i + 1
    return True

def gcd(a, b):
    while b != 0:
        temp = b
        b = a % b
        a = temp
    return a

def bubble_sort(arr):
    n = len(arr)
    i = 0
    while i < n:
        j = 0
        while j < n - i - 1:
            if arr[j] > arr[j + 1]:
                temp = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = temp
            j = j + 1
        i = i + 1
    return arr

def binary_search(arr, target):
    left = 0
    right = len(arr) - 1
    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return -1

print("=== Mathematical Functions ===")
print("Factorial of 5:", factorial(5))
print("Factorial of 6:", factorial(6))

print("Prime check for 17:", prime_check(17))
print("Prime check for 18:", prime_check(18))

print("GCD of 48 and 18:", gcd(48, 18))
print("GCD of 56 and 42:", gcd(56, 42))

print("\n=== Sorting and Searching ===")
test_array = [64, 34, 25, 12, 22, 11, 90]
print("Original array:", test_array)
sorted_array = bubble_sort(test_array[:])
print("Sorted array:", sorted_array)

print("Binary search for 22:", binary_search(sorted_array, 22))
print("Binary search for 50:", binary_search(sorted_array, 50))

print("\n=== All tests completed! ===")