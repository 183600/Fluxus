result = 0
i = 0
while i < 1000000:
    result = result + (i % 1000)
    i = i + 1
print(f"Processed {result} elements")