def hello():
    print("Hello, World!")

def test_basic_operations():
    # Test basic data types
    x = 42
    y = 3.14
    message = "Python compilation test"
    is_ready = True
    
    # Test arithmetic operations
    sum_result = x + 10
    product = y * 2.0
    
    # Test conditional
    if is_ready:
        print(f"Message: {message}")
        print(f"Sum: {sum_result}, Product: {product:.2f}")
    
    # Test loop
    for i in range(3):
        print(f"Loop iteration: {i}")
    
    # Test list
    numbers = [1, 2, 3, 4, 5]
    for num in numbers:
        print(f"Number: {num}")
    
    # Test dictionary
    person = {"name": "Alice", "age": 30, "city": "New York"}
    for key, value in person.items():
        print(f"{key}: {value}")
    
    # Test function call
    result = add_numbers(10, 20)
    print(f"Addition result: {result}")

def add_numbers(a, b):
    return a + b

def test_classes():
    class Calculator:
        def __init__(self):
            self.result = 0
        
        def add(self, x):
            self.result += x
            return self
        
        def multiply(self, x):
            self.result *= x
            return self
        
        def get_result(self):
            return self.result
    
    calc = Calculator()
    final_result = calc.add(5).multiply(3).get_result()
    print(f"Calculator result: {final_result}")

if __name__ == "__main__":
    hello()
    test_basic_operations()
    test_classes()