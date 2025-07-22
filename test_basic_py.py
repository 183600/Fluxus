def test_basic():
    x = 42
    print(x)
    return x

if __name__ == "__main__":
    result = test_basic()
    print(f"Result: {result}")