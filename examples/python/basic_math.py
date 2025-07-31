def add(a, b):
    return a + b

def subtract(a, b):
    return a - b

def multiply(a, b):
    return a * b

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def main():
    print("Simple math test")
    print(add(5, 3))
    print(subtract(10, 4))
    print(multiply(6, 7))
    print(factorial(5))

if __name__ == "__main__":
    main()