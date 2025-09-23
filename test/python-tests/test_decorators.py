import functools
import time

def simple_decorator(func):
    def wrapper():
        print("Before function call")
        func()
        print("After function call")
    return wrapper

@simple_decorator
def greet():
    print("Hello, World!")

def decorator_with_arguments(func):
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__} with args: {args}, kwargs: {kwargs}")
        result = func(*args, **kwargs)
        print(f"{func.__name__} returned: {result}")
        return result
    return wrapper

@decorator_with_arguments
def add_numbers(a, b):
    return a + b

def repeat(num_times):
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            results = []
            for _ in range(num_times):
                result = func(*args, **kwargs)
                results.append(result)
            return results
        return wrapper
    return decorator

@repeat(3)
def multiply(a, b):
    return a * b

def timer(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        end_time = time.time()
        print(f"{func.__name__} executed in {end_time - start_time:.4f} seconds")
        return result
    return wrapper

@timer
def slow_function():
    time.sleep(0.1)
    return "Done"

def cache(func):
    cache_dict = {}

    @functools.wraps(func)
    def wrapper(*args):
        if args in cache_dict:
            print(f"Returning cached result for {args}")
            return cache_dict[args]

        result = func(*args)
        cache_dict[args] = result
        print(f"Computed and cached result for {args}")
        return result

    return wrapper

@cache
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def test_simple_decorator():
    print("Testing simple decorator:")
    greet()
    print()

def test_decorator_with_arguments():
    print("Testing decorator with arguments:")
    result = add_numbers(5, 3)
    assert result == 8
    print()

def test_repeat_decorator():
    print("Testing repeat decorator:")
    results = multiply(2, 3)
    assert results == [6, 6, 6]
    print()

def test_timer_decorator():
    print("Testing timer decorator:")
    result = slow_function()
    assert result == "Done"
    print()

def test_cache_decorator():
    print("Testing cache decorator:")
    assert fibonacci(5) == 5
    assert fibonacci(5) == 5
    assert fibonacci(6) == 8
    print()

if __name__ == "__main__":
    test_simple_decorator()
    test_decorator_with_arguments()
    test_repeat_decorator()
    test_timer_decorator()
    test_cache_decorator()
    print("All decorator tests passed!")