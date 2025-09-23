# Test recursion examples

# Factorial function
def factorial(n):
    if n == 0 or n == 1:
        return 1
    else:
        return n * factorial(n - 1)

# Fibonacci sequence
def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

# Power function (recursive)
def power(base, exp):
    if exp == 0:
        return 1
    elif exp == 1:
        return base
    elif exp < 0:
        return 1 / power(base, -exp)
    else:
        return base * power(base, exp - 1)

# Binary search (recursive)
def binary_search(arr, target, left=0, right=None):
    if right is None:
        right = len(arr) - 1

    if left > right:
        return -1  # Not found

    mid = (left + right) // 2

    if arr[mid] == target:
        return mid
    elif arr[mid] < target:
        return binary_search(arr, target, mid + 1, right)
    else:
        return binary_search(arr, target, left, mid - 1)

# Greatest Common Divisor (GCD)
def gcd(a, b):
    if b == 0:
        return a
    else:
        return gcd(b, a % b)

# Tower of Hanoi
def tower_of_hanoi(n, source, auxiliary, target):
    if n == 1:
        print(f"Move disk 1 from {source} to {target}")
        return
    else:
        tower_of_hanoi(n - 1, source, target, auxiliary)
        print(f"Move disk {n} from {source} to {target}")
        tower_of_hanoi(n - 1, auxiliary, source, target)

# Test the recursive functions
print("=== Factorial Tests ===")
for i in range(6):
    print(f"Factorial({i}) = {factorial(i)}")

print("\n=== Fibonacci Tests ===")
for i in range(10):
    print(f"Fibonacci({i}) = {fibonacci(i)}")

print("\n=== Power Tests ===")
print(f"2^3 = {power(2, 3)}")
print(f"3^4 = {power(3, 4)}")
print(f"2^-2 = {power(2, -2)}")

print("\n=== Binary Search Tests ===")
sorted_array = [1, 3, 5, 7, 9, 11, 13, 15]
targets = [7, 2, 13, 8]
for target in targets:
    result = binary_search(sorted_array, target)
    if result != -1:
        print(f"Found {target} at index {result}")
    else:
        print(f"{target} not found in array")

print("\n=== GCD Tests ===")
print(f"GCD(48, 18) = {gcd(48, 18)}")
print(f"GCD(56, 98) = {gcd(56, 98)}")
print(f"GCD(17, 5) = {gcd(17, 5)}")

print("\n=== Tower of Hanoi (3 disks) ===")
tower_of_hanoi(3, "A", "B", "C")