# Test functions
def greet(name):
    return f"Hello, {name}!"

result = greet("World")
print(result)

# Test default parameters
def power(base, exponent=2):
    return base ** exponent

print(power(5))
print(power(5, 3))

# Test lambda
multiply = lambda x, y: x * y
print(multiply(4, 5))