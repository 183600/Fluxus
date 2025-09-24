#!/usr/bin/env python3
"""
Advanced Python Metaclass Patterns
Including singleton, factory, registry, and descriptor patterns
"""

import weakref
import time
from typing import Any, Dict, Type, TypeVar, Optional, Callable

T = TypeVar('T')

# Advanced Metaclass Patterns

class SingletonMeta(type):
    """Metaclass that implements singleton pattern with thread safety"""
    _instances = {}
    _locks = {}
    
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            if cls not in cls._locks:
                cls._locks[cls] = threading.Lock()
            
            with cls._locks[cls]:
                if cls not in cls._instances:
                    cls._instances[cls] = super().__call__(*args, **kwargs)
        
        return cls._instances[cls]

class FactoryMeta(type):
    """Metaclass that implements factory pattern"""
    _registry = {}
    
    def __new__(mcs, name, bases, namespace):
        cls = super().__new__(mcs, name, bases, namespace)
        
        # Register the class if it has a product_type attribute
        if hasattr(cls, 'product_type'):
            mcs._registry[cls.product_type] = cls
        
        return cls
    
    @classmethod
    def create(mcs, product_type: str, *args, **kwargs):
        """Factory method to create instances by type"""
        if product_type not in mcs._registry:
            raise ValueError(f"Unknown product type: {product_type}")
        
        return mcs._registry[product_type](*args, **kwargs)

class RegistryMeta(type):
    """Metaclass that automatically registers subclasses"""
    _registry = {}
    
    def __new__(mcs, name, bases, namespace):
        cls = super().__new__(mcs, name, bases, namespace)
        
        # Don't register the base class itself
        if name != 'BaseRegistered':
            mcs._registry[name] = cls
        
        return cls
    
    @classmethod
    def get_registered_classes(mcs):
        """Get all registered classes"""
        return mcs._registry.copy()

class AutoPropertiesMeta(type):
    """Metaclass that automatically creates properties for specified attributes"""
    def __new__(mcs, name, bases, namespace):
        # Look for __auto_properties__ attribute
        if '__auto_properties__' in namespace:
            for prop_name in namespace['__auto_properties__']:
                private_name = f'_{prop_name}'
                
                def getter(self, name=private_name):
                    return getattr(self, name, None)
                
                def setter(self, value, name=private_name):
                    setattr(self, name, value)
                
                def deleter(self, name=private_name):
                    if hasattr(self, name):
                        delattr(self, name)
                
                namespace[prop_name] = property(getter, setter, deleter)
        
        return super().__new__(mcs, name, bases, namespace)

class ValidatedMeta(type):
    """Metaclass that adds validation to class attributes"""
    def __new__(mcs, name, bases, namespace):
        # Store validation rules
        validations = {}
        
        # Look for validation attributes
        for key, value in list(namespace.items()):
            if key.startswith('validate_'):
                field_name = key[9:]  # Remove 'validate_' prefix
                validations[field_name] = value
        
        # Store validations in class
        namespace['_validations'] = validations
        
        return super().__new__(mcs, name, bases, namespace)

# Advanced Descriptor Patterns

class CachedProperty:
    """Descriptor that implements cached property with TTL"""
    def __init__(self, ttl_seconds: float = 60.0):
        self.ttl_seconds = ttl_seconds
        self._cache = {}
        self._timestamps = {}
    
    def __call__(self, func):
        self.func = func
        self.name = func.__name__
        return self
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        
        current_time = time.time()
        
        # Check if cached value exists and is not expired
        if obj in self._cache:
            if current_time - self._timestamps[obj] < self.ttl_seconds:
                return self._cache[obj]
            else:
                # Remove expired entry
                del self._cache[obj]
                del self._timestamps[obj]
        
        # Compute and cache result
        result = self.func(obj)
        self._cache[obj] = result
        self._timestamps[obj] = current_time
        
        return result
    
    def __delete__(self, obj):
        """Clear cache for specific object"""
        if obj in self._cache:
            del self._cache[obj]
            del self._timestamps[obj]

class ValidatedAttribute:
    """Descriptor that validates attribute values"""
    def __init__(self, validator: Callable[[Any], bool], error_message: str = "Invalid value"):
        self.validator = validator
        self.error_message = error_message
        self.data = weakref.WeakKeyDictionary()
    
    def __set_name__(self, owner, name):
        self.name = name
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        return self.data.get(obj, None)
    
    def __set__(self, obj, value):
        if not self.validator(value):
            raise ValueError(f"{self.error_message}: {value}")
        self.data[obj] = value
    
    def __delete__(self, obj):
        if obj in self.data:
            del self.data[obj]

class ObservableAttribute:
    """Descriptor that notifies observers when attribute changes"""
    def __init__(self, initial_value=None):
        self.data = weakref.WeakKeyDictionary()
        self.observers = weakref.WeakKeyDictionary()
        self.initial_value = initial_value
    
    def __set_name__(self, owner, name):
        self.name = name
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        return self.data.get(obj, self.initial_value)
    
    def __set__(self, obj, value):
        old_value = self.data.get(obj, self.initial_value)
        self.data[obj] = value
        self._notify_observers(obj, old_value, value)
    
    def _notify_observers(self, obj, old_value, new_value):
        if obj in self.observers:
            for callback in self.observers[obj]:
                callback(self.name, old_value, new_value)
    
    def add_observer(self, obj, callback: Callable[[str, Any, Any], None]):
        """Add an observer for this attribute"""
        if obj not in self.observers:
            self.observers[obj] = []
        self.observers[obj].append(callback)
    
    def remove_observer(self, obj, callback: Callable[[str, Any, Any], None]):
        """Remove an observer for this attribute"""
        if obj in self.observers and callback in self.observers[obj]:
            self.observers[obj].remove(callback)

class ThreadSafeAttribute:
    """Descriptor that ensures thread-safe access to attributes"""
    def __init__(self, initial_value=None):
        self.data = weakref.WeakKeyDictionary()
        self.locks = weakref.WeakKeyDictionary()
        self.initial_value = initial_value
    
    def __set_name__(self, owner, name):
        self.name = name
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        
        if obj not in self.locks:
            self.locks[obj] = threading.RLock()
        
        with self.locks[obj]:
            return self.data.get(obj, self.initial_value)
    
    def __set__(self, obj, value):
        if obj not in self.locks:
            self.locks[obj] = threading.RLock()
        
        with self.locks[obj]:
            self.data[obj] = value
    
    def __delete__(self, obj):
        if obj in self.locks:
            with self.locks[obj]:
                if obj in self.data:
                    del self.data[obj]

# Classes using advanced metaclasses

class DatabaseConnection(metaclass=SingletonMeta):
    """Thread-safe singleton database connection"""
    def __init__(self, connection_string: str):
        self.connection_string = connection_string
        self.connected = True
        print(f"Database connection established: {connection_string}")
    
    def execute_query(self, query: str):
        return f"Executing query: {query}"

class Product(metaclass=FactoryMeta):
    """Base product class for factory pattern"""
    product_type = "base"
    
    def __init__(self, name: str, price: float):
        self.name = name
        self.price = price
    
    def get_info(self):
        return f"{self.name}: ${self.price}"

class ElectronicProduct(Product):
    """Electronic product"""
    product_type = "electronic"
    
    def __init__(self, name: str, price: float, warranty_months: int):
        super().__init__(name, price)
        self.warranty_months = warranty_months
    
    def get_info(self):
        return f"{self.name}: ${self.price} (Warranty: {self.warranty_months} months)"

class BookProduct(Product):
    """Book product"""
    product_type = "book"
    
    def __init__(self, name: str, price: float, author: str, pages: int):
        super().__init__(name, price)
        self.author = author
        self.pages = pages
    
    def get_info(self):
        return f"{self.name} by {self.author}: ${self.price} ({self.pages} pages)"

class BaseRegistered(metaclass=RegistryMeta):
    """Base class that automatically registers subclasses"""
    pass

class ServiceA(BaseRegistered):
    def process(self):
        return "Service A processing"

class ServiceB(BaseRegistered):
    def process(self):
        return "Service B processing"

class AutoPropertiesClass(metaclass=AutoPropertiesMeta):
    """Class with automatically generated properties"""
    __auto_properties__ = ['name', 'age', 'email']
    
    def __init__(self, name: str, age: int, email: str):
        self.name = name
        self.age = age
        self.email = email

class ValidatedClass(metaclass=ValidatedMeta):
    """Class with validated attributes"""
    
    def validate_age(self, value):
        return isinstance(value, int) and 0 <= value <= 150
    
    def validate_email(self, value):
        return isinstance(value, str) and '@' in value
    
    def __init__(self, age: int, email: str):
        if not self.validate_age(age):
            raise ValueError(f"Invalid age: {age}")
        if not self.validate_email(email):
            raise ValueError(f"Invalid email: {email}")
        
        self.age = age
        self.email = email

# Classes using advanced descriptors

class SimpleObservable:
    """Simple observable for demonstration"""
    def __init__(self, initial_value=None):
        self._value = initial_value
        self._observers = []
    
    def add_observer(self, callback):
        self._observers.append(callback)
    
    def set_value(self, new_value):
        old_value = self._value
        self._value = new_value
        for callback in self._observers:
            callback(old_value, new_value)
    
    def get_value(self):
        return self._value

class PersonWithDescriptors:
    """Person class using advanced descriptors"""
    
    def __init__(self, name: str, age: int, email: str):
        # Use simple attributes for demonstration
        self._name = SimpleObservable(name)
        self._age = age
        self._email = email
        self._balance = 0.0
        self._computation_cache = None
        self._cache_timestamp = 0
        
        # Add observer for name changes
        self._name.add_observer(self._on_name_changed)
    
    @property
    def name(self):
        return self._name.get_value()
    
    @name.setter
    def name(self, value):
        self._name.set_value(value)
    
    @property
    def age(self):
        return self._age
    
    @age.setter
    def age(self, value):
        if not (isinstance(value, int) and 0 <= value <= 150):
            raise ValueError(f"Age must be an integer between 0 and 150: {value}")
        self._age = value
    
    @property
    def email(self):
        return self._email
    
    @email.setter
    def email(self, value):
        if not (isinstance(value, str) and '@' in value):
            raise ValueError(f"Email must contain @ symbol: {value}")
        self._email = value
    
    @property
    def balance(self):
        return self._balance
    
    @balance.setter
    def balance(self, value):
        self._balance = value
    
    def _on_name_changed(self, old_value: str, new_value: str):
        """Observer callback for name changes"""
        print(f"Name changed from '{old_value}' to '{new_value}'")
    
    def deposit(self, amount: float):
        """Thread-safe deposit operation"""
        current_balance = self.balance
        self.balance = current_balance + amount
        print(f"Deposited ${amount:.2f}. New balance: ${self.balance:.2f}")
    
    def withdraw(self, amount: float):
        """Thread-safe withdrawal operation"""
        current_balance = self.balance
        if current_balance >= amount:
            self.balance = current_balance - amount
            print(f"Withdrew ${amount:.2f}. New balance: ${self.balance:.2f}")
        else:
            print(f"Insufficient funds. Current balance: ${current_balance:.2f}")
    
    @property
    def expensive_computation(self):
        """Expensive computation with caching (simplified)"""
        current_time = time.time()
        if (self._computation_cache is not None and 
            current_time - self._cache_timestamp < 2.0):  # 2 second TTL
            print("Using cached computation result")
            return self._computation_cache
        
        print("Performing expensive computation...")
        time.sleep(1.0)  # Simulate expensive operation
        result = len(self.name) * 100
        
        self._computation_cache = result
        self._cache_timestamp = current_time
        return result
    
    def clear_computation_cache(self):
        """Clear the computation cache"""
        self._computation_cache = None
        self._cache_timestamp = 0

def test_metaclass_patterns():
    """Test advanced metaclass patterns"""
    print("=== Testing Advanced Metaclass Patterns ===")
    
    # Test singleton metaclass
    print("\n--- Singleton Metaclass ---")
    db1 = DatabaseConnection("postgresql://localhost:5432/mydb")
    db2 = DatabaseConnection("postgresql://localhost:5432/otherdb")
    print(f"db1 is db2: {db1 is db2}")
    print(f"Connection string: {db1.connection_string}")
    
    # Test factory metaclass
    print("\n--- Factory Metaclass ---")
    electronic = FactoryMeta.create("electronic", "Laptop", 999.99, 24)
    book = FactoryMeta.create("book", "Python Guide", 49.99, "John Doe", 300)
    
    print(f"Electronic product: {electronic.get_info()}")
    print(f"Book product: {book.get_info()}")
    
    # Test registry metaclass
    print("\n--- Registry Metaclass ---")
    registered_classes = RegistryMeta.get_registered_classes()
    print(f"Registered classes: {list(registered_classes.keys())}")
    
    for name, cls in registered_classes.items():
        instance = cls()
        print(f"{name}: {instance.process()}")
    
    # Test auto-properties metaclass
    print("\n--- Auto-Properties Metaclass ---")
    obj = AutoPropertiesClass("Alice", 25, "alice@example.com")
    print(f"Name: {obj.name}, Age: {obj.age}, Email: {obj.email}")
    
    obj.age = 26
    obj.email = "alice.smith@example.com"
    print(f"Updated - Name: {obj.name}, Age: {obj.age}, Email: {obj.email}")
    
    # Test validated metaclass
    print("\n--- Validated Metaclass ---")
    try:
        valid_obj = ValidatedClass(25, "valid@email.com")
        print(f"Valid object created: age={valid_obj.age}, email={valid_obj.email}")
        
        invalid_obj = ValidatedClass(200, "invalid-email")  # Should raise ValueError
    except ValueError as e:
        print(f"Validation error (expected): {e}")

def test_descriptor_patterns():
    """Test advanced descriptor patterns"""
    print("\n=== Testing Advanced Descriptor Patterns ===")
    
    # Create person with descriptors
    person = PersonWithDescriptors("John Doe", 25, "john@example.com")
    print(f"Initial person: {person.name}, {person.age}, {person.email}")
    
    # Test validated attributes
    print("\n--- Validated Attributes ---")
    try:
        person.age = 30  # Valid
        print(f"Age updated to: {person.age}")
        
        person.age = 200  # Invalid - should raise ValueError
    except ValueError as e:
        print(f"Age validation error (expected): {e}")
    
    try:
        person.email = "invalid-email"  # Invalid - should raise ValueError
    except ValueError as e:
        print(f"Email validation error (expected): {e}")
    
    # Test observable attribute
    print("\n--- Observable Attribute ---")
    person.name = "Jane Doe"  # Should trigger observer
    person.name = "John Smith"  # Should trigger observer again
    
    # Test thread-safe attribute
    print("\n--- Thread-Safe Attribute ---")
    person.deposit(100.0)
    person.withdraw(30.0)
    person.withdraw(80.0)  # Should fail - insufficient funds
    
    # Test cached property
    print("\n--- Cached Property ---")
    result1 = person.expensive_computation
    print(f"First computation result: {result1}")
    
    result2 = person.expensive_computation  # Should use cache
    print(f"Second computation result (cached): {result2}")
    
    # Wait for cache to expire
    print("Waiting for cache to expire...")
    time.sleep(2.5)
    
    result3 = person.expensive_computation  # Should recalculate
    print(f"Third computation result (after expiry): {result3}")
    
    # Clear cache manually
    person.clear_computation_cache()
    result4 = person.expensive_computation  # Should recalculate
    print(f"Fourth computation result (after manual clear): {result4}")

def main():
    """Main test function"""
    print("=== Advanced Python Metaclass and Descriptor Patterns ===")
    
    test_metaclass_patterns()
    test_descriptor_patterns()
    
    print("\n=== All metaclass and descriptor tests completed successfully! ===")

if __name__ == "__main__":
    import threading
    import time
    main()