import math

def calculate_area_circle(radius):
    return math.pi * radius * radius

def calculate_area_rectangle(width, height):
    return width * height

def calculate_area_triangle(base, height):
    return 0.5 * base * height

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def is_prime(n):
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0 or n % 3 == 0:
        return False
    
    i = 5
    while i * i <= n:
        if n % i == 0 or n % (i + 2) == 0:
            return False
        i += 6
    return True

def main():
    print("Python Mathematical Functions Demo")
    print("=" * 40)
    
    # Test area calculations
    print("\n1. Area Calculations:")
    circle_area = calculate_area_circle(5.0)
    rectangle_area = calculate_area_rectangle(4.0, 6.0)
    triangle_area = calculate_area_triangle(8.0, 5.0)
    
    print(f"Circle (radius=5): {circle_area:.2f}")
    print(f"Rectangle (4x6): {rectangle_area:.2f}")
    print(f"Triangle (base=8, height=5): {triangle_area:.2f}")
    
    # Test factorial
    print("\n2. Factorial Calculations:")
    for i in range(1, 6):
        fact = factorial(i)
        print(f"Factorial of {i}: {fact}")
    
    # Test fibonacci
    print("\n3. Fibonacci Sequence:")
    for i in range(10):
        fib = fibonacci(i)
        print(f"F({i}) = {fib}")
    
    # Test prime numbers
    print("\n4. Prime Number Check:")
    test_numbers = [2, 3, 4, 5, 17, 25, 29, 37]
    for num in test_numbers:
        if is_prime(num):
            print(f"{num} is prime")
        else:
            print(f"{num} is not prime")
    
    print("\nDemo completed successfully!")

if __name__ == "__main__":
    main()