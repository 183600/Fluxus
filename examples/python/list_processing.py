def process_list(numbers):
    total = 0
    count = 0
    for num in numbers:
        total = total + num
        count = count + 1
    
    if count == 0:
        return 0, 0
    
    average = total / count
    return total, average

def find_min_max(numbers):
    if len(numbers) == 0:
        return None, None
    
    min_val = numbers[0]
    max_val = numbers[0]
    
    for num in numbers:
        if num < min_val:
            min_val = num
        if num > max_val:
            max_val = num
    
    return min_val, max_val

def filter_even_numbers(numbers):
    result = []
    for num in numbers:
        if num % 2 == 0:
            result.append(num)
    return result

def filter_odd_numbers(numbers):
    result = []
    for num in numbers:
        if num % 2 == 1:
            result.append(num)
    return result

def count_positive_negative(numbers):
    positive_count = 0
    negative_count = 0
    zero_count = 0
    
    for num in numbers:
        if num > 0:
            positive_count = positive_count + 1
        elif num < 0:
            negative_count = negative_count + 1
        else:
            zero_count = zero_count + 1
    
    return positive_count, negative_count, zero_count

def square_numbers(numbers):
    result = []
    for num in numbers:
        result.append(num * num)
    return result

def main():
    print("List processing test")
    
    test_numbers = [1, -2, 3, -4, 5, 0, 6, -7, 8, 9]
    print("Test numbers:")
    print(test_numbers)
    
    total, average = process_list(test_numbers)
    print("Total and average:")
    print(total)
    print(average)
    
    min_val, max_val = find_min_max(test_numbers)
    print("Min and max:")
    print(min_val)
    print(max_val)
    
    even_nums = filter_even_numbers(test_numbers)
    print("Even numbers:")
    print(even_nums)
    
    odd_nums = filter_odd_numbers(test_numbers)
    print("Odd numbers:")
    print(odd_nums)
    
    pos_count, neg_count, zero_count = count_positive_negative(test_numbers)
    print("Positive, negative, zero counts:")
    print(pos_count)
    print(neg_count)
    print(zero_count)
    
    squared = square_numbers([1, 2, 3, 4, 5])
    print("Squared numbers (1-5):")
    print(squared)

if __name__ == "__main__":
    main()