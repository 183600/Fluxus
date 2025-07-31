# Simple Python program for fluxus compilation

def main():
    print("Hello from fixed Python")
    
    # Test basic arithmetic
    a = 10
    b = 20
    print(f"a + b = {a + b}")
    print(f"a - b = {a - b}")
    print(f"a * b = {a * b}")
    print(f"a / b = {a / b}")
    
    # Test simple function
    def square(x):
        return x * x
    
    print(f"square(5) = {square(5)}")
    
    # Test conditional
    if a > b:
        print("a is greater than b")
    else:
        print("b is greater than a")
    
    # Test loop
    print("Numbers from 1 to 5:")
    for i in range(1, 6):
        print(f"  {i}")
    
    print("Fixed Python program completed successfully")

if __name__ == "__main__":
    main()
