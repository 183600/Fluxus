# Test file operations
import os

# Create a test file
test_file = "test_file.txt"

# Write to file
with open(test_file, 'w') as f:
    f.write("This is the first line.\n")
    f.write("This is the second line.\n")
    f.write("This is the third line.\n")

print(f"File '{test_file}' created and written to.")

# Read file
with open(test_file, 'r') as f:
    content = f.read()
    print("\nFile content:")
    print(content)

# Read file line by line
print("\nReading file line by line:")
with open(test_file, 'r') as f:
    for line_num, line in enumerate(f, 1):
        print(f"Line {line_num}: {line.strip()}")

# Append to file
with open(test_file, 'a') as f:
    f.write("This line was appended.\n")

print("\nAfter appending:")
with open(test_file, 'r') as f:
    print(f.read())

# Check file existence
print(f"\nFile exists: {os.path.exists(test_file)}")
print(f"File size: {os.path.getsize(test_file)} bytes")

# Read file modes
print("\nDifferent read modes:")
with open(test_file, 'r') as f:
    lines = f.readlines()
    print(f"Read {len(lines)} lines using readlines()")

with open(test_file, 'r') as f:
    first_line = f.readline()
    print(f"First line: {first_line.strip()}")

# Working with binary files
binary_file = "test_binary.bin"
with open(binary_file, 'wb') as f:
    f.write(b'\x48\x65\x6c\x6c\x6f')  # Binary data for "Hello"

print(f"\nBinary file '{binary_file}' created.")

with open(binary_file, 'rb') as f:
    binary_content = f.read()
    print(f"Binary content: {binary_content}")
    print(f"Decoded: {binary_content.decode('utf-8')}")

# Clean up
os.remove(test_file)
os.remove(binary_file)
print(f"\nTest files '{test_file}' and '{binary_file}' removed.")