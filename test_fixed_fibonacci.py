def fibonacci(n):
    """Calculate the nth Fibonacci number"""
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    for i in range(10):
        result = fibonacci(i)
        print("fib result")
