#!/usr/bin/env python3
"""
Comprehensive test for Python standard library usage in compiled output.
This tests that the compiler can handle and correctly compile Python code
that uses various standard library modules.
"""

import os
import sys
import json
import math
import datetime
import random
import collections
import itertools
import functools
from typing import List, Dict, Optional, Tuple

# Test basic built-in functions
def test_builtins():
    """Test basic built-in functions"""
    numbers = [1, 2, 3, 4, 5]
    result = {
        'len': len(numbers),
        'sum': sum(numbers),
        'max': max(numbers),
        'min': min(numbers),
        'sorted': sorted(numbers, reverse=True),
        'enumerate': list(enumerate(numbers)),
        'zip': list(zip(numbers, ['a', 'b', 'c', 'd', 'e'])),
        'range': list(range(5)),
        'map': list(map(lambda x: x * 2, numbers)),
        'filter': list(filter(lambda x: x % 2 == 0, numbers))
    }
    return result

# Test string operations
def test_string_operations():
    """Test string manipulation functions"""
    text = "Hello, World!"
    return {
        'upper': text.upper(),
        'lower': text.lower(),
        'replace': text.replace('World', 'Python'),
        'split': text.split(','),
        'join': '-'.join(['a', 'b', 'c']),
        'strip': "  spaces  ".strip(),
        'format': "Hello, {}!".format("Python"),
        'f_string': f"Hello, {'Python'}!"
    }

# Test os module
def test_os_module():
    """Test os module functionality"""
    current_dir = os.getcwd()
    return {
        'getcwd': current_dir,
        'listdir_exists': len(os.listdir('.')) > 0,
        'path_join': os.path.join('path', 'to', 'file.txt'),
        'path_exists': os.path.exists('.'),
        'environ_home': os.environ.get('HOME', 'not_found')
    }

# Test sys module
def test_sys_module():
    """Test sys module functionality"""
    return {
        'platform': sys.platform,
        'version_info': f"{sys.version_info.major}.{sys.version_info.minor}",
        'argv_length': len(sys.argv),
        'maxsize': sys.maxsize > 0
    }

# Test json module
def test_json_module():
    """Test JSON serialization/deserialization"""
    data = {'name': 'test', 'value': 42, 'items': [1, 2, 3]}
    json_str = json.dumps(data)
    parsed = json.loads(json_str)
    return {
        'serialized': json_str,
        'deserialized': parsed,
        'round_trip_success': data == parsed
    }

# Test math module
def test_math_module():
    """Test mathematical functions"""
    return {
        'pi': math.pi,
        'e': math.e,
        'sqrt': math.sqrt(16),
        'pow': math.pow(2, 3),
        'sin': math.sin(math.pi / 2),
        'cos': math.cos(0),
        'log': math.log(math.e),
        'factorial': math.factorial(5),
        'gcd': math.gcd(12, 18),
        'ceil': math.ceil(4.2),
        'floor': math.floor(4.8)
    }

# Test datetime module
def test_datetime_module():
    """Test datetime functionality"""
    now = datetime.datetime.now()
    date_today = datetime.date.today()
    time_now = datetime.time(12, 30, 45)
    
    return {
        'now_year': now.year,
        'today_day': date_today.day,
        'time_hour': time_now.hour,
        'strftime': now.strftime('%Y-%m-%d'),
        'timedelta': str(datetime.timedelta(days=7))
    }

# Test random module
def test_random_module():
    """Test random number generation"""
    random.seed(42)  # For reproducible results
    return {
        'randint': random.randint(1, 100),
        'random': random.random(),
        'choice': random.choice(['a', 'b', 'c', 'd']),
        'shuffle_test': random.sample(range(10), 5)
    }

# Test collections module
def test_collections_module():
    """Test collections data structures"""
    counter = collections.Counter(['a', 'b', 'a', 'c', 'b', 'a'])
    defaultdict_test = collections.defaultdict(int)
    defaultdict_test['key'] += 1
    
    deque_test = collections.deque([1, 2, 3])
    deque_test.appendleft(0)
    deque_test.append(4)
    
    return {
        'counter': dict(counter),
        'counter_most_common': counter.most_common(2),
        'defaultdict': dict(defaultdict_test),
        'deque': list(deque_test)
    }

# Test itertools module
def test_itertools_module():
    """Test itertools functionality"""
    return {
        'chain': list(itertools.chain([1, 2], [3, 4])),
        'cycle_take_5': list(itertools.islice(itertools.cycle(['a', 'b']), 5)),
        'count_take_3': list(itertools.islice(itertools.count(10, 2), 3)),
        'combinations': list(itertools.combinations([1, 2, 3], 2)),
        'permutations': list(itertools.permutations([1, 2, 3], 2)),
        'product': list(itertools.product(['A', 'B'], [1, 2]))
    }

# Test functools module
def test_functools_module():
    """Test functools functionality"""
    @functools.lru_cache(maxsize=128)
    def cached_fibonacci(n):
        if n < 2:
            return n
        return cached_fibonacci(n-1) + cached_fibonacci(n-2)
    
    # Test reduce
    numbers = [1, 2, 3, 4, 5]
    product = functools.reduce(lambda x, y: x * y, numbers)
    
    # Test partial
    multiply_by_2 = functools.partial(lambda x, y: x * y, 2)
    
    return {
        'fibonacci_10': cached_fibonacci(10),
        'reduce_product': product,
        'partial_result': multiply_by_2(5),
        'cache_info': str(cached_fibonacci.cache_info())
    }

# Test complex data structures and algorithms
def test_complex_operations():
    """Test complex operations that combine multiple stdlib features"""
    data = [
        {'name': 'Alice', 'age': 30, 'score': 85},
        {'name': 'Bob', 'age': 25, 'score': 92},
        {'name': 'Charlie', 'age': 35, 'score': 78},
    ]
    
    # Sort by score, descending
    sorted_by_score = sorted(data, key=lambda x: x['score'], reverse=True)
    
    # Group by age range
    age_groups = collections.defaultdict(list)
    for person in data:
        age_range = "young" if person['age'] < 30 else "old"
        age_groups[age_range].append(person['name'])
    
    # Calculate statistics
    scores = [person['score'] for person in data]
    stats = {
        'mean': sum(scores) / len(scores),
        'min': min(scores),
        'max': max(scores),
        'std_dev': math.sqrt(sum((x - sum(scores)/len(scores))**2 for x in scores) / len(scores))
    }
    
    return {
        'sorted_data': sorted_by_score,
        'age_groups': dict(age_groups),
        'statistics': stats,
        'json_export': json.dumps(data, indent=2)
    }

# Test error handling with stdlib
def test_error_handling():
    """Test error handling with standard library operations"""
    results = {}
    
    # Test file operations
    try:
        with open('nonexistent_file.txt', 'r') as f:
            content = f.read()
        results['file_error'] = False
    except FileNotFoundError:
        results['file_error'] = True
    
    # Test JSON parsing error
    try:
        json.loads('invalid json')
        results['json_error'] = False
    except json.JSONDecodeError:
        results['json_error'] = True
    
    # Test math domain error
    try:
        math.sqrt(-1)
        results['math_error'] = False
    except ValueError:
        results['math_error'] = True
    
    return results

def main():
    """Main test function that runs all standard library tests"""
    print("Testing Python Standard Library Usage in Compiled Code")
    print("=" * 60)
    
    test_functions = [
        ("Built-in Functions", test_builtins),
        ("String Operations", test_string_operations),
        ("OS Module", test_os_module),
        ("Sys Module", test_sys_module),
        ("JSON Module", test_json_module),
        ("Math Module", test_math_module),
        ("DateTime Module", test_datetime_module),
        ("Random Module", test_random_module),
        ("Collections Module", test_collections_module),
        ("Itertools Module", test_itertools_module),
        ("Functools Module", test_functools_module),
        ("Complex Operations", test_complex_operations),
        ("Error Handling", test_error_handling)
    ]
    
    all_results = {}
    
    for test_name, test_func in test_functions:
        print(f"\nTesting {test_name}...")
        try:
            result = test_func()
            all_results[test_name] = result
            print(f"✓ {test_name} completed successfully")
        except Exception as e:
            print(f"✗ {test_name} failed: {e}")
            all_results[test_name] = {"error": str(e)}
    
    print("\n" + "=" * 60)
    print("Test Summary:")
    print(f"Total tests: {len(test_functions)}")
    
    successful_tests = sum(1 for result in all_results.values() if "error" not in result)
    print(f"Successful: {successful_tests}")
    print(f"Failed: {len(test_functions) - successful_tests}")
    
    # Save results to JSON file
    with open('python_stdlib_test_results.json', 'w') as f:
        json.dump(all_results, f, indent=2, default=str)
    
    print("\nDetailed results saved to 'python_stdlib_test_results.json'")
    
    return successful_tests == len(test_functions)

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)