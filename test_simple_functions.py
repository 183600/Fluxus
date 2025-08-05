# Test basic functions
def greet(name):
    return "Hello, " + name

print(greet("World"))

def power(base, exponent=2):
    return base ** exponent

print(power(3))
print(power(3, 3))

def sum_all(*args):
    total = 0
    for arg in args:
        total += arg
    return total

print(sum_all(1, 2, 3))
print(sum_all(10, 20, 30, 40, 50))

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))

def print_message(message):
    print(message)

print_message("Test message")
result = print_message("Test message")
print(result)