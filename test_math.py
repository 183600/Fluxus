def add(a, b):
    return a + b

def multiply(x, y):
    return x * y

def main():
    result1 = add(5, 3)
    result2 = multiply(4, 7)
    print("5 + 3 =", result1)
    print("4 * 7 =", result2)
    print("Complex expression:", add(multiply(2, 3), add(1, 2)))

if __name__ == "__main__":
    main()
