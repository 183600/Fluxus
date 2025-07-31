def calculate_area_rectangle(width, height):
    return width * height

def calculate_area_circle(radius):
    pi = 3.14159
    return pi * radius * radius

def calculate_area_triangle(base, height):
    return 0.5 * base * height

def add_numbers(a, b):
    return a + b

def subtract_numbers(a, b):
    return a - b

def multiply_numbers(a, b):
    return a * b

def divide_numbers(a, b):
    if b == 0:
        return None
    return a / b

def power_of(base, exponent):
    result = 1
    for i in range(exponent):
        result = result * base
    return result

def absolute_value(number):
    if number < 0:
        return -number
    return number

def min_of_two(a, b):
    if a < b:
        return a
    return b

def max_of_two(a, b):
    if a > b:
        return a
    return b

def main():
    print("Math operations test")
    
    print("Rectangle area (5x3):")
    print(calculate_area_rectangle(5, 3))
    
    print("Circle area (radius 4):")
    print(calculate_area_circle(4))
    
    print("Triangle area (base 6, height 8):")
    print(calculate_area_triangle(6, 8))
    
    print("Add 10 + 5:")
    print(add_numbers(10, 5))
    
    print("Subtract 10 - 3:")
    print(subtract_numbers(10, 3))
    
    print("Multiply 4 * 7:")
    print(multiply_numbers(4, 7))
    
    print("Divide 20 / 4:")
    print(divide_numbers(20, 4))
    
    print("2 to the power of 3:")
    print(power_of(2, 3))
    
    print("Absolute value of -5:")
    print(absolute_value(-5))
    
    print("Min of 8 and 12:")
    print(min_of_two(8, 12))
    
    print("Max of 8 and 12:")
    print(max_of_two(8, 12))

if __name__ == "__main__":
    main()