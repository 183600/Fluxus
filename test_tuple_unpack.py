# Test tuple unpacking

def test():
    a = 1
    b = 2
    a, b = b, a
    return a

print(test())
