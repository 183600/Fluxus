print("Hello from compiled Python!")

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print("Fibonacci sequence:")
for i in range(8):
    print("fib(" + str(i) + ") = " + str(fibonacci(i)))

print("Math operations:")
a = 15
b = 25
print(str(a) + " + " + str(b) + " = " + str(a + b))
print(str(a) + " * " + str(b) + " = " + str(a * b))

numbers = [1, 2, 3, 4, 5]
print("Sum of numbers = " + str(sum(numbers)))