def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def test_arithmetic():
    a = 5
    b = 3
    result = a + b
    return result

def test_function_call():
    result = fibonacci(5)
    print("Fibonacci of 5 is:")
    print(result)
    return result

def main():
    test_function_call()
    arithmetic_result = test_arithmetic()
    print("Arithmetic result:")
    print(arithmetic_result)

main()