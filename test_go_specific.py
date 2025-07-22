# Test specific Go generator limitations

# 1. Variable declarations
a = 10
b = "hello"
c = [1, 2, 3]

# 2. Function calls
print("Hello World")
print(42)
print(a + b)

# 3. For loops
for i in range(5):
    print(i)

# 4. While loops
x = 0
while x < 5:
    print(x)
    x += 1

# 5. Complex expressions
result = (a * 2) + (b - 3)

# 6. Import statements
import fmt

# 7. Function with multiple parameters and return
def calculate(x, y, z):
    return x + y * z