# Test Advanced Object-Oriented Programming Features

class MetaClass(type):
    """A simple metaclass that adds class attributes"""
    def __new__(cls, name, bases, namespace):
        # Add a class attribute
        namespace['meta_attribute'] = f"Created by {name}"

        # Add a class method
        def class_info(cls):
            return f"Class {name} with meta_attribute: {cls.meta_attribute}"
        namespace['class_info'] = classmethod(class_info)

        return super().__new__(cls, name, bases, namespace)

class AdvancedClass(metaclass=MetaClass):
    """Class using metaclass features"""

    def __init__(self, value):
        self.value = value

    def get_value(self):
        return self.value

class Descriptor:
    """A descriptor for managed attribute access"""
    def __init__(self, name, default=None):
        self.name = name
        self.default = default

    def __get__(self, obj, objtype):
        if obj is None:
            return self
        return obj.__dict__.get(self.name, self.default)

    def __set__(self, obj, value):
        print(f"Setting {self.name} to {value}")
        obj.__dict__[self.name] = value

    def __delete__(self, obj):
        if self.name in obj.__dict__:
            del obj.__dict__[self.name]
            print(f"Deleted {self.name}")

class PersonWithDescriptors:
    """Class using descriptors for attribute management"""
    name = Descriptor('name', 'Unknown')
    age = Descriptor('age', 0)
    email = Descriptor('email', '')

    def __init__(self, name, age, email):
        self.name = name
        self.age = age
        self.email = email

    def __str__(self):
        return f"Person(name={self.name}, age={self.age}, email={self.email})"

class PropertyExample:
    """Class demonstrating property decorators"""

    def __init__(self, temperature_celsius):
        self._temperature_celsius = temperature_celsius

    @property
    def temperature_celsius(self):
        """Getter for temperature in Celsius"""
        return self._temperature_celsius

    @temperature_celsius.setter
    def temperature_celsius(self, value):
        """Setter for temperature in Celsius"""
        if value < -273.15:
            raise ValueError("Temperature below absolute zero!")
        self._temperature_celsius = value

    @temperature_celsius.deleter
    def temperature_celsius(self):
        """Deleter for temperature"""
        del self._temperature_celsius

    @property
    def temperature_fahrenheit(self):
        """Computed property for Fahrenheit"""
        return (self._temperature_celsius * 9/5) + 32

class Singleton:
    """Singleton pattern implementation"""
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        if not self._initialized:
            self.data = []
            self._initialized = True

class BaseA:
    """Base class A for multiple inheritance"""
    def method_a(self):
        return "Method A from BaseA"

    def common_method(self):
        return "Common method from BaseA"

class BaseB:
    """Base class B for multiple inheritance"""
    def method_b(self):
        return "Method B from BaseB"

    def common_method(self):
        return "Common method from BaseB"

class DerivedMultiple(BaseA, BaseB):
    """Class demonstrating multiple inheritance"""
    def method_c(self):
        return "Method C from DerivedMultiple"

    def common_method(self):
        return "Common method from DerivedMultiple"

class AbstractShape:
    """Abstract base class using ABC"""
    def area(self):
        raise NotImplementedError("Subclasses must implement area()")

    def perimeter(self):
        raise NotImplementedError("Subclasses must implement perimeter()")

class ConcreteCircle(AbstractShape):
    """Concrete implementation of abstract class"""
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return 3.14159 * self.radius ** 2

    def perimeter(self):
        return 2 * 3.14159 * self.radius

class DunderMethodsDemo:
    """Class demonstrating magic/dunder methods"""

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return f"DunderMethodsDemo(value={self.value})"

    def __repr__(self):
        return f"DunderMethodsDemo({self.value})"

    def __len__(self):
        return len(str(self.value))

    def __getitem__(self, key):
        if isinstance(key, int):
            return str(self.value)[key]
        elif isinstance(key, slice):
            return str(self.value)[key]
        else:
            raise TypeError("Invalid key type")

    def __setitem__(self, key, value):
        # For demonstration, we'll create a new string
        current = str(self.value)
        if isinstance(key, int):
            self.value = current[:key] + str(value) + current[key+1:]
        else:
            raise TypeError("Invalid key type")

    def __call__(self, multiplier):
        return self.value * multiplier

    def __add__(self, other):
        return DunderMethodsDemo(self.value + other.value)

    def __eq__(self, other):
        return self.value == other.value

    def __lt__(self, other):
        return self.value < other.value

    def __enter__(self):
        print("Entering context")
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        print("Exiting context")
        return False  # Don't suppress exceptions

# Test functions
def test_metaclass():
    print("=== Metaclass Test ===")

    obj = AdvancedClass(42)
    print(f"Value: {obj.get_value()}")
    print(f"Meta attribute: {obj.meta_attribute}")
    print(f"Class info: {AdvancedClass.class_info()}")

def test_descriptors():
    print("\n=== Descriptors Test ===")

    person = PersonWithDescriptors("Alice", 25, "alice@example.com")
    print(person)

    # Test descriptor behavior
    person.name = "Bob"
    print(f"Updated name: {person.name}")

    # Test deletion
    del person.age
    print(f"After deletion - age: {person.age}")

def test_properties():
    print("\n=== Properties Test ===")

    temp = PropertyExample(25)
    print(f"Celsius: {temp.temperature_celsius}")
    print(f"Fahrenheit: {temp.temperature_fahrenheit}")

    temp.temperature_celsius = 30
    print(f"New Celsius: {temp.temperature_celsius}")
    print(f"New Fahrenheit: {temp.temperature_fahrenheit}")

def test_singleton():
    print("\n=== Singleton Test ===")

    s1 = Singleton()
    s2 = Singleton()

    print(f"s1 is s2: {s1 is s2}")
    s1.data.append("item1")
    print(f"s2 data: {s2.data}")

def test_multiple_inheritance():
    print("\n=== Multiple Inheritance Test ===")

    obj = DerivedMultiple()
    print(f"Method A: {obj.method_a()}")
    print(f"Method B: {obj.method_b()}")
    print(f"Method C: {obj.method_c()}")
    print(f"Common method: {obj.common_method()}")

    # MRO (Method Resolution Order)
    print(f"MRO: {DerivedMultiple.__mro__}")

def test_abstract_classes():
    print("\n=== Abstract Classes Test ===")

    circle = ConcreteCircle(5)
    print(f"Circle area: {circle.area():.2f}")
    print(f"Circle perimeter: {circle.perimeter():.2f}")

def test_dunder_methods():
    print("\n=== Dunder Methods Test ===")

    obj1 = DunderMethodsDemo("hello")
    obj2 = DunderMethodsDemo("world")

    print(f"String representation: {str(obj1)}")
    print(f"Repr: {repr(obj1)}")
    print(f"Length: {len(obj1)}")
    print(f"First character: {obj1[0]}")

    # Test arithmetic operators
    obj3 = obj1 + obj2
    print(f"Addition result: {obj3}")

    # Test comparison
    print(f"obj1 == obj2: {obj1 == obj2}")
    print(f"obj1 < obj2: {obj1 < obj2}")

    # Test callable
    result = obj1(3)
    print(f"Callable result: {result}")

    # Test context manager
    with obj1 as context_obj:
        print(f"Inside context: {context_obj}")

def test_class_methods():
    print("\n=== Class Methods and Static Methods Test ===")

    class Example:
        class_var = "I'm a class variable"

        def __init__(self, value):
            self.value = value

        @classmethod
        def from_string(cls, str_value):
            """Class method as alternative constructor"""
            return cls(int(str_value))

        @staticmethod
        def utility_function(x, y):
            """Static method - utility function"""
            return x + y

        def instance_method(self):
            """Regular instance method"""
            return self.value

    # Test class method
    obj = Example.from_string("42")
    print(f"Created from string: {obj.value}")

    # Test static method
    result = Example.utility_function(10, 20)
    print(f"Static method result: {result}")

    # Test instance method
    print(f"Instance method: {obj.instance_method()}")

if __name__ == "__main__":
    test_metaclass()
    test_descriptors()
    test_properties()
    test_singleton()
    test_multiple_inheritance()
    test_abstract_classes()
    test_dunder_methods()
    test_class_methods()

    print("\n=== All advanced OOP tests passed! ===")