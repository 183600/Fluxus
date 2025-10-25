# Test comprehensive Python functionality
def fibonacci(n):
    """Calculate the nth Fibonacci number"""
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    # Calculate some Fibonacci numbers
    for i in range(5):
        result = fibonacci(i)
        print("Result:", result)

# Call main directly for now
main()