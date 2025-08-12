def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

for i in range(5):
    result = fibonacci(i)
    print(f"fib({i}) = {result}")
