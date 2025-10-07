def add(a, b):
    return a + b

if __name__ == "__main__":
    import sys
    
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <a> <b>")
        sys.exit(1)
    
    try:
        # Try to convert to integers first
        try:
            a = int(sys.argv[1])
            b = int(sys.argv[2])
            result = add(a, b)
            print(result)
        except ValueError:
            # If not integers, try as floats
            a = float(sys.argv[1])
            b = float(sys.argv[2])
            result = add(a, b)
            print(result)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)