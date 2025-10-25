print("Hello from compiled Python!")

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print("Fibonacci sequence:")
for i in range(8):
    print(f"fib({i}) = {fibonacci(i)}")

print("Math operations:")
a = 15
b = 25
print(f"{a} + {b} = {a + b}")
print(f"{a} * {b} = {a * b}")

numbers = [1, 2, 3, 4, 5]
print(f"Sum of {numbers} = {sum(numbers)}")