# test_optimization.py
# Simple test file to verify optimization functionality

def calculate():
    # This function has some constant expressions that could be folded
    x = 2 + 3 * 4
    y = 10 - 5
    z = x * y
    return z

def main():
    result = calculate()
    print("Result:", result)

if __name__ == "__main__":
    main()