def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def fib(n):
    if n <= 1:
        return n
    a = 0
    b = 1
    i = 2
    while i <= n:
        c = a + b
        a = b
        b = c
        i = i + 1
    return b

print("Factorial of 5:", factorial(5))
print("Factorial of 6:", factorial(6))
print("Fibonacci of 7:", fib(7))
print("Fibonacci of 8:", fib(8))
print("Fibonacci of 9:", fib(9))
print("All tests completed!")