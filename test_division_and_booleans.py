#!/usr/bin/env python3
"""
Test division operations
"""

print("=== Testing Division ===")

# Integer division (should be float)
result1 = 10 / 2
print(f"10 / 2 = {result1}")

# Float division (should be float)
result2 = 10 / 3
print(f"10 / 3 = {result2}")

# Mixed division
result3 = 15 / 4
print(f"15 / 4 = {result3}")

# Test boolean handling
print("\n=== Testing Booleans ===")
flag = True
print(f"True = {flag}")
print(f"False = {False}")

# Test boolean operations
print(f"True and False = {True and False}")
print(f"True or False = {True or False}")
print(f"not True = {not True}")

print("\n=== Test Complete ===")