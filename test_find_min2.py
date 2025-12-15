from typing import List

def find_min(values: List[int]) -> int:
    smallest = values[0]
    for value in values:
        if value < smallest:
            smallest = value
    return smallest

print(find_min([5, 3, 7, 2]))
