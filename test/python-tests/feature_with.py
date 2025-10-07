# Context manager tests

class FileManager:
    def __init__(self, filename):
        self.filename = filename
        self.file = None
    
    def __enter__(self):
        print(f"Opening file: {self.filename}")
        # Simulate opening a file
        self.file = f"file_handle_{self.filename}"
        return self.file
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        print(f"Closing file: {self.filename}")
        if exc_type:
            print(f"Exception occurred: {exc_type.__name__}")
        return False

# Test with statement
with FileManager("test.txt") as file:
    print(f"Working with {file}")

print("File operation completed")