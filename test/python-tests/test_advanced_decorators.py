#!/usr/bin/env python3
"""
Advanced Python Decorator Patterns
Including class decorators, parameterized decorators, and decorator factories
"""

import functools
import time
import logging
import threading
from typing import Any, Callable, Type, TypeVar, cast

F = TypeVar('F', bound=Callable[..., Any])
T = TypeVar('T')

# Class Decorators

def singleton(cls: Type[T]) -> Type[T]:
    """Class decorator to implement singleton pattern"""
    instances = {}
    
    @functools.wraps(cls)
    def wrapper(*args, **kwargs):
        if cls not in instances:
            instances[cls] = cls(*args, **kwargs)
        return instances[cls]
    
    return cast(Type[T], wrapper)

def count_instances(cls: Type[T]) -> Type[T]:
    """Class decorator to count instances"""
    cls._instance_count = 0
    original_init = cls.__init__
    
    @functools.wraps(original_init)
    def new_init(self, *args, **kwargs):
        cls._instance_count += 1
        original_init(self, *args, **kwargs)
    
    cls.__init__ = new_init
    
    @classmethod
    def get_instance_count(cls):
        return cls._instance_count
    
    cls.get_instance_count = get_instance_count
    return cls

def auto_str(cls: Type[T]) -> Type[T]:
    """Class decorator to automatically implement __str__ method"""
    def __str__(self):
        attributes = []
        for attr, value in self.__dict__.items():
            attributes.append(f"{attr}={value!r}")
        return f"{cls.__name__}({', '.join(attributes)})"
    
    cls.__str__ = __str__
    return cls

# Parameterized Decorators

def retry(max_attempts: int = 3, delay: float = 1.0, backoff: float = 2.0):
    """Parameterized decorator for retry logic with exponential backoff"""
    def decorator(func: F) -> F:
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            current_delay = delay
            last_exception = None
            
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    last_exception = e
                    if attempt == max_attempts - 1:
                        raise
                    
                    logging.warning(f"Attempt {attempt + 1} failed: {e}. Retrying in {current_delay}s...")
                    time.sleep(current_delay)
                    current_delay *= backoff
            
            raise last_exception
        
        return cast(F, wrapper)
    return decorator

def timeout(seconds: float):
    """Parameterized decorator to add timeout to function execution"""
    def decorator(func: F) -> F:
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            result = [None]
            exception = [None]
            
            def target():
                try:
                    result[0] = func(*args, **kwargs)
                except Exception as e:
                    exception[0] = e
            
            thread = threading.Thread(target=target)
            thread.daemon = True
            thread.start()
            thread.join(timeout=seconds)
            
            if thread.is_alive():
                raise TimeoutError(f"Function {func.__name__} timed out after {seconds} seconds")
            
            if exception[0]:
                raise exception[0]
            
            return result[0]
        
        return cast(F, wrapper)
    return decorator

def cache_with_ttl(ttl_seconds: float = 300):
    """Parameterized decorator for caching with TTL (time to live)"""
    def decorator(func: F) -> F:
        cache = {}
        cache_timestamps = {}
        
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            # Create cache key from arguments
            cache_key = str(args) + str(sorted(kwargs.items()))
            current_time = time.time()
            
            # Check if cached result exists and is not expired
            if cache_key in cache:
                if current_time - cache_timestamps[cache_key] < ttl_seconds:
                    print(f"Cache hit for {func.__name__}{args}")
                    return cache[cache_key]
                else:
                    # Remove expired entry
                    del cache[cache_key]
                    del cache_timestamps[cache_key]
            
            # Compute and cache result
            print(f"Cache miss for {func.__name__}{args}")
            result = func(*args, **kwargs)
            cache[cache_key] = result
            cache_timestamps[cache_key] = current_time
            
            return result
        
        # Add cache management methods
        def clear_cache():
            cache.clear()
            cache_timestamps.clear()
            print(f"Cache cleared for {func.__name__}")
        
        def cache_info():
            return {
                'size': len(cache),
                'keys': list(cache.keys())
            }
        
        wrapper.clear_cache = clear_cache
        wrapper.cache_info = cache_info
        
        return cast(F, wrapper)
    return decorator

def rate_limit(max_calls: int = 10, time_window: float = 60.0):
    """Parameterized decorator for rate limiting function calls"""
    def decorator(func: F) -> F:
        calls = []
        lock = threading.Lock()
        
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            current_time = time.time()
            
            with lock:
                # Remove calls outside the time window
                calls[:] = [call_time for call_time in calls if current_time - call_time < time_window]
                
                # Check if we've exceeded the rate limit
                if len(calls) >= max_calls:
                    oldest_call = min(calls)
                    wait_time = time_window - (current_time - oldest_call)
                    raise RuntimeError(f"Rate limit exceeded. Wait {wait_time:.1f} seconds.")
                
                # Record this call
                calls.append(current_time)
            
            return func(*args, **kwargs)
        
        return cast(F, wrapper)
    return decorator

# Advanced Decorator Patterns

def conditional(condition: Callable[[], bool]):
    """Decorator that only applies the decorated function if condition is true"""
    def decorator(func: F) -> F:
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            if condition():
                return func(*args, **kwargs)
            else:
                print(f"Skipping {func.__name__} - condition not met")
                return None
        
        return cast(F, wrapper)
    return decorator

def compose(*decorators):
    """Compose multiple decorators into one"""
    def decorator(func: F) -> F:
        for dec in reversed(decorators):
            func = dec(func)
        return func
    return decorator

def logging_decorator(logger_name: str = None, level: int = logging.INFO):
    """Advanced logging decorator with configurable logger and level"""
    def decorator(func: F) -> F:
        logger = logging.getLogger(logger_name or func.__module__)
        
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            start_time = time.time()
            logger.log(level, f"Calling {func.__name__} with args: {args}, kwargs: {kwargs}")
            
            try:
                result = func(*args, **kwargs)
                execution_time = time.time() - start_time
                logger.log(level, f"{func.__name__} completed successfully in {execution_time:.4f}s")
                return result
            except Exception as e:
                execution_time = time.time() - start_time
                logger.log(logging.ERROR, f"{func.__name__} failed after {execution_time:.4f}s: {e}")
                raise
        
        return cast(F, wrapper)
    return decorator

# Test Classes and Functions

@singleton
class DatabaseConnection:
    """Singleton database connection class"""
    def __init__(self, connection_string: str):
        self.connection_string = connection_string
        self.connected = True
        print(f"Database connection established: {connection_string}")
    
    def query(self, sql: str):
        return f"Executing query: {sql}"

@count_instances
@auto_str
class Person:
    """Person class with automatic string representation and instance counting"""
    def __init__(self, name: str, age: int):
        self.name = name
        self.age = age

class Calculator:
    """Calculator class for testing decorators"""
    
    @retry(max_attempts=3, delay=0.1)
    def divide_with_retry(self, a: float, b: float) -> float:
        """Division with automatic retry on failure"""
        print(f"Attempting division: {a} / {b}")
        if b == 0:
            raise ValueError("Division by zero")
        return a / b
    
    @timeout(2.0)
    def slow_operation(self, duration: float) -> str:
        """Simulate a slow operation"""
        print(f"Starting slow operation for {duration} seconds...")
        time.sleep(duration)
        return f"Operation completed after {duration} seconds"
    
    @cache_with_ttl(ttl_seconds=1.0)
    def expensive_calculation(self, n: int) -> int:
        """Expensive calculation with caching"""
        print(f"Performing expensive calculation for n={n}")
        time.sleep(0.5)  # Simulate expensive operation
        return n * n + n
    
    @rate_limit(max_calls=3, time_window=5.0)
    def api_call(self, endpoint: str) -> str:
        """Simulated API call with rate limiting"""
        print(f"Making API call to: {endpoint}")
        return f"Response from {endpoint}"

# Conditional execution flag
enable_debug_mode = False

def debug_mode_enabled():
    """Condition function for conditional decorator"""
    return enable_debug_mode

@conditional(debug_mode_enabled)
def debug_function():
    """Function that only runs in debug mode"""
    print("Debug mode is enabled - executing debug function")

# Composed decorators
@compose(
    logging_decorator(),
    retry(max_attempts=3, delay=0.1)
)
def complex_operation(fail_count: int) -> str:
    """Complex operation with logging and retry"""
    global _attempt_count
    if '_attempt_count' not in globals():
        _attempt_count = 0
    
    _attempt_count += 1
    if _attempt_count <= fail_count:
        raise RuntimeError(f"Simulated failure #{_attempt_count}")
    return f"Operation succeeded on attempt #{_attempt_count}"

def test_class_decorators():
    """Test class decorators"""
    print("=== Testing Class Decorators ===")
    
    # Test singleton decorator
    print("\n--- Singleton Decorator ---")
    db1 = DatabaseConnection("postgresql://localhost:5432/mydb")
    db2 = DatabaseConnection("postgresql://localhost:5432/otherdb")  # Should return same instance
    print(f"db1 is db2: {db1 is db2}")  # Should be True
    print(f"Connection string: {db1.connection_string}")
    
    # Test instance counting decorator
    print("\n--- Instance Counting Decorator ---")
    person1 = Person("Alice", 25)
    person2 = Person("Bob", 30)
    person3 = Person("Charlie", 35)
    
    print(f"Person instances created: {Person.get_instance_count()}")
    print(f"Person 1: {person1}")
    print(f"Person 2: {person2}")
    print(f"Person 3: {person3}")

def test_parameterized_decorators():
    """Test parameterized decorators"""
    print("\n=== Testing Parameterized Decorators ===")
    
    calc = Calculator()
    
    # Test retry decorator
    print("\n--- Retry Decorator ---")
    try:
        result = calc.divide_with_retry(10, 2)
        print(f"Division result: {result}")
        
        result = calc.divide_with_retry(10, 0)  # This will retry and eventually fail
    except ValueError as e:
        print(f"Division failed after retries: {e}")
    
    # Test timeout decorator
    print("\n--- Timeout Decorator ---")
    try:
        result = calc.slow_operation(1.0)  # Should succeed
        print(f"Slow operation result: {result}")
        
        result = calc.slow_operation(3.0)  # Should timeout
    except TimeoutError as e:
        print(f"Operation timed out: {e}")
    
    # Test cache with TTL decorator
    print("\n--- Cache with TTL Decorator ---")
    result1 = calc.expensive_calculation(5)
    print(f"First call result: {result1}")
    
    result2 = calc.expensive_calculation(5)  # Should use cache
    print(f"Second call result (cached): {result2}")
    
    print(f"Cache info: {calc.expensive_calculation.cache_info()}")
    
    # Wait for cache to expire
    print("Waiting for cache to expire...")
    time.sleep(1.5)
    
    result3 = calc.expensive_calculation(5)  # Should recalculate
    print(f"Third call result (after cache expiry): {result3}")
    
    # Test rate limiting decorator
    print("\n--- Rate Limiting Decorator ---")
    try:
        for i in range(4):  # Should succeed for first 3, fail on 4th
            result = calc.api_call(f"/api/endpoint{i}")
            print(f"API call {i+1}: {result}")
    except RuntimeError as e:
        print(f"Rate limit exceeded: {e}")

def test_advanced_patterns():
    """Test advanced decorator patterns"""
    print("\n=== Testing Advanced Decorator Patterns ===")
    
    # Test conditional decorator
    print("\n--- Conditional Decorator ---")
    global enable_debug_mode
    
    enable_debug_mode = False
    debug_function()  # Should be skipped
    
    enable_debug_mode = True
    debug_function()  # Should execute
    
    # Test composed decorators
    print("\n--- Composed Decorators ---")
    global _attempt_count
    _attempt_count = 0
    
    result = complex_operation(2)  # Should fail twice, then succeed on 3rd attempt
    print(f"Complex operation result: {result}")

def main():
    """Main test function"""
    print("=== Advanced Python Decorator Patterns ===")
    
    test_class_decorators()
    test_parameterized_decorators()
    test_advanced_patterns()
    
    print("\n=== All decorator tests completed successfully! ===")

if __name__ == "__main__":
    # Set up basic logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
    main()