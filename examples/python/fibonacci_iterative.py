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

print(fib(5))
print(fib(6))
print(fib(7))
print(fib(8))
print(fib(9))
print(fib(10))