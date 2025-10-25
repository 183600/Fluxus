def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(n - i - 1):
            if arr[j] > arr[j + 1]:
                temp = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = temp
    return arr

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def sum_list(arr):
    total = 0
    for num in arr:
        total = total + num
    return total

def find_max(arr):
    if len(arr) == 0:
        return None
    
    max_val = arr[0]
    for num in arr:
        if num > max_val:
            max_val = num
    return max_val

def is_prime(n):
    if n < 2:
        return False
    for i in range(2, n):
        if n % i == 0:
            return False
    return True

def count_chars(text):
    count = 0
    for char in text:
        count = count + 1
    return count

def reverse_string(text):
    result = ""
    for i in range(len(text) - 1, -1, -1):
        result = result + text[i]
    return result

def main():
    print("Simple Python algorithms test")
    
    numbers = [64, 34, 25, 12, 22, 11, 90]
    print("Original array:")
    print(numbers)
    
    sorted_numbers = bubble_sort(numbers)
    print("Sorted array:")
    print(sorted_numbers)
    
    print("Fibonacci of 10:")
    print(fibonacci(10))
    
    print("Factorial of 5:")
    print(factorial(5))
    
    print("Sum of array:")
    print(sum_list([1, 2, 3, 4, 5]))
    
    print("Max of array:")
    print(find_max([3, 7, 2, 9, 1]))
    
    print("Is 17 prime:")
    print(is_prime(17))
    
    text = "hello world"
    print("Text:")
    print(text)
    print("Character count:")
    print(count_chars(text))
    print("Reversed:")
    print(reverse_string(text))

if __name__ == "__main__":
    main()