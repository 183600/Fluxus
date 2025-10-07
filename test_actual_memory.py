import sys
import array

def create_large_array(size):
    return array.array('i', [i % 1000 for i in range(size)])

def process_arrays(arr1, arr2):
    result = array.array('i', [])
    for i in range(min(len(arr1), len(arr2))):
        result.append(arr1[i] + arr2[i])
    return result

def main():
    size = 1000000
    arr1 = create_large_array(size)
    arr2 = create_large_array(size)
    result = process_arrays(arr1, arr2)
    print(f"Processed {len(result)} elements")
    return 0

if __name__ == "__main__":
    sys.exit(main())