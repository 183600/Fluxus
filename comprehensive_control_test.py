# Comprehensive Python control structure test

# While loop
count = 0
while count < 5:
    print("While loop:", count)
    count += 1
else:
    print("While loop finished")

# For loop with range
for i in range(3):
    print("For loop with range:", i)
else:
    print("For loop with range finished")

# For loop with list
items = ["apple", "banana", "cherry"]
for item in items:
    print("For loop with list:", item)
else:
    print("For loop with list finished")

# Nested loops
for i in range(2):
    for j in range(2):
        print("Nested loop:", i, j)

# Break and continue
for i in range(10):
    if i == 3:
        print("Breaking at", i)
        break
    if i % 2 == 0:
        print("Continuing at", i)
        continue
    print("Processing", i)

# While with break
x = 0
while True:
    if x >= 3:
        break
    print("While with break:", x)
    x += 1
