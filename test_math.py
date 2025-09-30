def calculate_area(length, width):
    return length * width

def main():
    # Test arithmetic
    a = 10
    b = 5
    sum_result = a + b
    product = a * b

    print(f"Sum: {sum_result}")
    print(f"Product: {product}")

    # Test function call
    area = calculate_area(6, 4)
    print(f"Area: {area}")

if __name__ == "__main__":
    main()