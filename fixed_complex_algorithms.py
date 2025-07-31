# Complex Python algorithms for fluxus compilation

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
    print("Complex Python Algorithms Test")
    
    # Test fibonacci
    print("Fibonacci sequence:")
    for i in range(10):
        print(f"  fib({i}) = {fibonacci(i)}")
    
    # Test factorial
    print(f"Factorial of 5: {factorial(5)}")
    print(f"Factorial of 7: {factorial(7)}")
    
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
    
    # Test mathematical operations
    a, b = 15, 25
    print(f"Mathematical operations:")
    print(f"  {a} + {b} = {a + b}")
    print(f"  {a} - {b} = {a - b}")
    print(f"  {a} * {b} = {a * b}")
    print(f"  {a} / {b} = {a / b}")
    
    # Test conditional logic
    print("Conditional logic:")
    x = 10
    if x > 5:
        print(f"  {x} is greater than 5")
    elif x < 5:
        print(f"  {x} is less than 5")
    else:
        print(f"  {x} is equal to 5")
    
    # Test loops
    print("Loop tests:")
    print("  Counting to 5:")
    for i in range(1, 6):
        print(f"    {i}")
    
    print("  While loop:")
    count = 0
    while count < 3:
        print(f"    Count: {count}")
        count += 1
    
    print("Complex algorithms test completed successfully")

if __name__ == "__main__":
    main()
