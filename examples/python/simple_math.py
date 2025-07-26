def add(a, b):
    return a + b

def multiply(a, b):
    return a * b

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def main():
    print("Simple Math Operations")
    
    x = 5
    y = 3
    
    sum_result = add(x, y)
    print("5 + 3 =", sum_result)
    
    product = multiply(x, y)  
    print("5 * 3 =", product)
    
    fact = factorial(5)
    print("5! =", fact)
    
    print("Numbers 1 to 5:")
    for i in range(1, 6):
        print(i, end=" ")
    print()

if __name__ == "__main__":
    main()