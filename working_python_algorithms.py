def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def bubble_sort(arr):
    n = len(arr)
    for i in range(n - 1):
        for j in range(n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
    return arr

def is_prime(n):
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0:
        return False
    i = 3
    while i * i <= n:
        if n % i == 0:
            return False
        i += 2
    return True

def main():
    print("Python Algorithms Test")
    
    # Test fibonacci
    print("Fibonacci sequence:")
    for i in range(10):
        print(f"  fib({i}) = {fibonacci(i)}")
    
    # Test factorial
    print(f"Factorial(5) = {factorial(5)}")
    print(f"Factorial(7) = {factorial(7)}")
    
    # Test sorting
    arr = [64, 34, 25, 12, 22, 11, 90]
    print(f"Original array: {arr}")
    sorted_arr = bubble_sort(arr.copy())
    print(f"Sorted array: {sorted_arr}")
    
    # Test prime checking
    numbers = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    print("Prime numbers:")
    for num in numbers:
        if is_prime(num):
            print(f"  {num} is prime")
        else:
            print(f"  {num} is not prime")
    
    print("Python algorithms test completed successfully!")

if __name__ == "__main__":
    main()
