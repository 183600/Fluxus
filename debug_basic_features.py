# Test basic Python features that should work
x = 10
y = 20
print(f"x = {x}")
print(f"y = {y}")
print(f"x + y = {x + y}")

# Test simple conditional
if x > y:
    print("x is greater than y")
else:
    print("y is greater than or equal to x")

# Test simple loop
print("Counting from 1 to 5:")
for i in range(1, 6):
    print(f"  {i}")

# Test list
numbers = [1, 2, 3, 4, 5]
print(f"Numbers: {numbers}")