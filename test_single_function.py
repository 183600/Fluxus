# Test single function
def greet(name):
    return "Hello, " + name

print(greet("World"))

def power(base, exponent):
    return base ** exponent

print(power(3, 2))
print(power(3, 3))

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))