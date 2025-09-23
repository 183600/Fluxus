# Test context managers and file handling
import os

# Using with statement for file operations
def read_file_example():
    filename = "test_file.txt"

    # Write to file
    with open(filename, 'w') as f:
        f.write("Hello, World!\n")
        f.write("This is a test file.\n")
        f.write("Line 3\n")
        f.write("Line 4\n")

    # Read file
    with open(filename, 'r') as f:
        content = f.read()
        print("File content:")
        print(content)

    # Read file line by line
    with open(filename, 'r') as f:
        print("\nReading line by line:")
        for i, line in enumerate(f, 1):
            print(f"Line {i}: {line.strip()}")

    # Clean up
    os.remove(filename)

# Multiple context managers
def copy_file_example():
    source = "source.txt"
    dest = "destination.txt"

    # Create source file
    with open(source, 'w') as f:
        f.write("Source content\n")
        f.write("Another line\n")

    # Copy file
    with open(source, 'r') as src, open(dest, 'w') as dst:
        dst.write(src.read())

    # Verify copy
    with open(dest, 'r') as f:
        print("\nCopied file content:")
        print(f.read())

    # Clean up
    os.remove(source)
    os.remove(dest)

# Custom context manager
class Timer:
    def __enter__(self):
        import time
        self.start_time = time.time()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        import time
        self.end_time = time.time()
        print(f"Execution time: {self.end_time - self.start_time:.4f} seconds")

# Test custom context manager
def test_timer():
    print("\nTesting custom context manager:")
    with Timer():
        # Simulate some work
        import time
        time.sleep(0.1)
        print("Doing some work...")

# Run tests
read_file_example()
copy_file_example()
test_timer()