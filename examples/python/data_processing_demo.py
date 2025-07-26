import random

def generate_random_data(size):
    return [random.randint(1, 100) for _ in range(size)]

def analyze_data(data):
    if not data:
        return {}
    
    total = sum(data)
    average = total / len(data)
    minimum = min(data)
    maximum = max(data)
    
    # Calculate median
    sorted_data = sorted(data)
    n = len(sorted_data)
    if n % 2 == 0:
        median = (sorted_data[n//2 - 1] + sorted_data[n//2]) / 2
    else:
        median = sorted_data[n//2]
    
    # Calculate mode (most frequent)
    frequency = {}
    for num in data:
        frequency[num] = frequency.get(num, 0) + 1
    
    mode = max(frequency, key=frequency.get)
    
    return {
        'count': len(data),
        'sum': total,
        'average': average,
        'median': median,
        'mode': mode,
        'minimum': minimum,
        'maximum': maximum,
        'range': maximum - minimum
    }

def filter_data(data, condition):
    result = []
    for item in data:
        if condition(item):
            result.append(item)
    return result

def map_data(data, transform):
    result = []
    for item in data:
        result.append(transform(item))
    return result

def count_occurrences(data):
    counts = {}
    for item in data:
        counts[item] = counts.get(item, 0) + 1
    return counts

def find_patterns(data):
    patterns = {
        'even_numbers': len([x for x in data if x % 2 == 0]),
        'odd_numbers': len([x for x in data if x % 2 == 1]),
        'multiples_of_5': len([x for x in data if x % 5 == 0]),
        'multiples_of_10': len([x for x in data if x % 10 == 0])
    }
    return patterns

def process_student_grades():
    print("Student Grade Processing System")
    print("-" * 35)
    
    # Sample student data
    students = {
        'Alice': [95, 87, 92, 88, 94],
        'Bob': [78, 82, 79, 85, 80],
        'Charlie': [92, 89, 94, 91, 93],
        'Diana': [85, 88, 82, 87, 86],
        'Eve': [76, 79, 81, 78, 77]
    }
    
    for name, grades in students.items():
        analysis = analyze_data(grades)
        print(f"\n{name}'s Grades: {grades}")
        print(f"  Average: {analysis['average']:.2f}")
        print(f"  Median: {analysis['median']:.2f}")
        print(f"  Range: {analysis['range']}")
        
        if analysis['average'] >= 90:
            grade_letter = 'A'
        elif analysis['average'] >= 80:
            grade_letter = 'B'
        elif analysis['average'] >= 70:
            grade_letter = 'C'
        elif analysis['average'] >= 60:
            grade_letter = 'D'
        else:
            grade_letter = 'F'
        
        print(f"  Final Grade: {grade_letter}")

def main():
    print("Python Data Processing Demo")
    print("=" * 30)
    
    # Generate random data
    random.seed(42)  # For reproducible results
    data = generate_random_data(20)
    print(f"\nGenerated data: {data}")
    
    # Analyze the data
    analysis = analyze_data(data)
    print(f"\nData Analysis:")
    for key, value in analysis.items():
        if isinstance(value, float):
            print(f"  {key.capitalize()}: {value:.2f}")
        else:
            print(f"  {key.capitalize()}: {value}")
    
    # Filter data
    high_values = filter_data(data, lambda x: x > 50)
    low_values = filter_data(data, lambda x: x <= 50)
    
    print(f"\nFiltered Data:")
    print(f"  Values > 50: {high_values} (count: {len(high_values)})")
    print(f"  Values <= 50: {low_values} (count: {len(low_values)})")
    
    # Transform data
    squared_data = map_data(data, lambda x: x * x)
    doubled_data = map_data(data, lambda x: x * 2)
    
    print(f"\nTransformed Data:")
    print(f"  Squared: {squared_data[:5]}... (showing first 5)")
    print(f"  Doubled: {doubled_data[:5]}... (showing first 5)")
    
    # Count occurrences
    counts = count_occurrences(data)
    print(f"\nFrequency Analysis:")
    sorted_counts = sorted(counts.items(), key=lambda x: x[1], reverse=True)
    for value, count in sorted_counts[:5]:
        print(f"  {value} appears {count} time(s)")
    
    # Find patterns
    patterns = find_patterns(data)
    print(f"\nPattern Analysis:")
    for pattern, count in patterns.items():
        print(f"  {pattern.replace('_', ' ').title()}: {count}")
    
    # Process student grades
    print(f"\n{'='*50}")
    process_student_grades()
    
    # Statistical summary
    print(f"\n{'='*50}")
    print("Statistical Summary:")
    print(f"  Dataset size: {len(data)}")
    print(f"  Unique values: {len(set(data))}")
    print(f"  Most common value: {analysis['mode']}")
    print(f"  Spread (max-min): {analysis['range']}")
    
    print("\nDemo completed successfully!")

if __name__ == "__main__":
    main()