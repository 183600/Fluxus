# Test Type Hints, Data Classes, and Advanced Type Features

from typing import (
    List, Dict, Set, Tuple, Optional, Union, Any, Callable,
    TypeVar, Generic, Protocol, TypedDict, Final, Literal,
    Iterator, Iterable, Sequence, Container, Collection
)
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
from enum import Enum
import collections.abc

# Type Variables
T = TypeVar('T')
K = TypeVar('K')
V = TypeVar('V')
Num = TypeVar('Num', int, float)

# Protocol definitions
class Drawable(Protocol):
    def draw(self) -> None:
        ...

class Shape(Protocol):
    def area(self) -> float:
        ...

# TypedDict
class PersonDict(TypedDict):
    name: str
    age: int
    email: str
    active: bool

# Data classes
@dataclass
class Point:
    x: float
    y: float

    def __post_init__(self):
        if self.x < 0 or self.y < 0:
            raise ValueError("Coordinates must be non-negative")

@dataclass
class Rectangle:
    width: float
    height: float
    position: Point = field(default_factory=lambda: Point(0, 0))
    color: str = field(default="blue")
    area: float = field(init=False)

    def __post_init__(self):
        self.area = self.width * self.height

@dataclass(frozen=True)
class ImmutablePerson:
    name: str
    age: int
    email: str

# Generic classes
class Box(Generic[T]):
    def __init__(self, content: T):
        self.content = content

    def get(self) -> T:
        return self.content

    def set(self, content: T) -> None:
        self.content = content

class Pair(Generic[T, V]):
    def __init__(self, first: T, second: V):
        self.first = first
        self.second = second

# Abstract Base Classes
class Animal(ABC):
    @abstractmethod
    def make_sound(self) -> str:
        ...

    @abstractmethod
    def move(self) -> str:
        ...

class Dog(Animal):
    def make_sound(self) -> str:
        return "Woof!"

    def move(self) -> str:
        return "Running on four legs"

class Cat(Animal):
    def make_sound(self) -> str:
        return "Meow!"

    def move(self) -> str:
        return "Walking gracefully"

# Enumerations
class Color(Enum):
    RED = "#FF0000"
    GREEN = "#00FF00"
    BLUE = "#0000FF"

class Status(Enum):
    PENDING = 1
    IN_PROGRESS = 2
    COMPLETED = 3
    FAILED = 4

# Union and Optional types
def process_value(value: Union[int, str, None]) -> str:
    if value is None:
        return "None"
    elif isinstance(value, int):
        return f"Integer: {value}"
    else:
        return f"String: {value}"

# Generic functions
def first_item(items: Sequence[T]) -> Optional[T]:
    if items:
        return items[0]
    return None

def filter_by_type(items: List[Any], item_type: type) -> List[Any]:
    return [item for item in items if isinstance(item, item_type)]

# Advanced type hints
class Calculator:
    def __init__(self):
        self._history: List[str] = []

    def add(self, a: Num, b: Num) -> Num:
        return a + b

    def multiply(self, a: Num, b: Num) -> Num:
        return a * b

    def get_operation_history(self) -> Tuple[str, ...]:
        return tuple(self._history)

# Callable types
def apply_function(func: Callable[[int, int], int], x: int, y: int) -> int:
    return func(x, y)

# Iterator and Iterable types
def process_iterable(data: Iterable[T], processor: Callable[[T], V]) -> Iterator[V]:
    for item in data:
        yield processor(item)

# Container and Collection types
def count_items(container: Container[T], item: T) -> int:
    return sum(1 for contained_item in container if contained_item == item)

# Literal types
def set_status(status: Literal["active", "inactive", "pending"]) -> str:
    return f"Status set to {status}"

# Final variables
MAX_RETRIES: Final = 3
DEFAULT_TIMEOUT: Final = 30.0

# Complex type combinations
type ConfigDict = Dict[str, Union[str, int, bool, List[str]]]
type ProcessorFunc = Callable[[Any], Any]
type DataProcessor = Callable[[List[Any]], List[Any]]

class DataProcessor:
    def __init__(self, config: ConfigDict):
        self.config = config

    def process_data(self, data: List[Any]) -> List[Any]:
        processors: List[ProcessorFunc] = []

        if self.config.get("filter_none", False):
            processors.append(lambda x: [item for item in x if item is not None])

        if self.config.get("uppercase_strings", False):
            processors.append(lambda x: [str(item).upper() if isinstance(item, str) else item for item in x])

        result = data
        for processor in processors:
            result = processor(result)

        return result

# Test functions
def test_type_hints():
    print("=== Type Hints Test ===")

    # Basic type hints
    name: str = "Alice"
    age: int = 25
    scores: List[int] = [90, 85, 95]
    person_data: Dict[str, Union[str, int]] = {"name": "Bob", "age": 30}

    print(f"Name: {name}, Age: {age}")
    print(f"Scores: {scores}")
    print(f"Person data: {person_data}")

    # Optional types
    optional_name: Optional[str] = None
    optional_name = "Charlie"
    print(f"Optional name: {optional_name}")

    # Union types
    mixed_data: Union[int, str, List[int]] = [1, 2, 3]
    print(f"Mixed data: {mixed_data}")

def test_generics():
    print("\n=== Generics Test ===")

    # Box generic
    int_box = Box(42)
    str_box = Box("Hello")

    print(f"Int box: {int_box.get()}")
    print(f"Str box: {str_box.get()}")

    # Pair generic
    pair = Pair("first", 2)
    print(f"Pair: ({pair.first}, {pair.second})")

    # Generic functions
    numbers = [1, 2, 3, 4, 5]
    print(f"First item: {first_item(numbers)}")

    strings = ["a", "b", "c"]
    print(f"First item: {first_item(strings)}")

def test_dataclasses():
    print("\n=== Data Classes Test ===")

    # Point dataclass
    p1 = Point(3.5, 4.2)
    p2 = Point(1.0, 2.0)
    print(f"Point 1: {p1}")
    print(f"Point 2: {p2}")

    # Rectangle dataclass
    rect = Rectangle(5.0, 3.0)
    print(f"Rectangle: {rect}")
    print(f"Rectangle area: {rect.area}")

    # Frozen dataclass
    person = ImmutablePerson("Alice", 25, "alice@example.com")
    print(f"Immutable person: {person}")

def test_enums():
    print("\n=== Enums Test ===")

    # Color enum
    print(f"Red color: {Color.RED}")
    print(f"Red value: {Color.RED.value}")

    # Status enum
    status = Status.IN_PROGRESS
    print(f"Current status: {status}")
    print(f"Status name: {status.name}")
    print(f"Status value: {status.value}")

    # Iterate over enum
    print("All statuses:")
    for s in Status:
        print(f"  {s.name}: {s.value}")

def test_protocols():
    print("\n=== Protocols Test ===")

    # Protocol implementation
    class Circle:
        def __init__(self, radius: float):
            self.radius = radius

        def area(self) -> float:
            return 3.14159 * self.radius ** 2

        def draw(self) -> None:
            print(f"Drawing circle with radius {self.radius}")

    # Protocol usage
    def process_shape(shape: Shape) -> float:
        return shape.area()

    def draw_drawable(drawable: Drawable) -> None:
        drawable.draw()

    circle = Circle(5.0)
    print(f"Circle area: {process_shape(circle)}")
    draw_drawable(circle)

def test_typeddict():
    print("\n=== TypedDict Test ===")

    # TypedDict usage
    person: PersonDict = {
        "name": "Alice",
        "age": 25,
        "email": "alice@example.com",
        "active": True
    }

    print(f"Person: {person}")
    print(f"Person name: {person['name']}")
    print(f"Person age: {person['age']}")

def test_abstract_classes():
    print("\n=== Abstract Classes Test ===")

    # Abstract class usage
    animals: List[Animal] = [Dog(), Cat()]

    for animal in animals:
        print(f"Sound: {animal.make_sound()}")
        print(f"Movement: {animal.move()}")

def test_advanced_types():
    print("\n=== Advanced Types Test ===")

    # Calculator with advanced type hints
    calc = Calculator()
    print(f"Add: {calc.add(5, 3)}")
    print(f"Multiply: {calc.multiply(4, 2.5)}")

    # Callable types
    result = apply_function(lambda x, y: x + y, 10, 20)
    print(f"Apply function result: {result}")

    # Iterator and Iterable
    numbers = [1, 2, 3, 4, 5]
    squared = process_iterable(numbers, lambda x: x ** 2)
    print(f"Squared numbers: {list(squared)}")

    # Container types
    container = [1, 2, 3, 2, 1, 2, 3]
    print(f"Count of 2: {count_items(container, 2)}")

def test_literal_types():
    print("\n=== Literal Types Test ===")

    # Literal types
    print(f"Set status: {set_status('active')}")
    print(f"Set status: {set_status('inactive')}")

    # Final variables
    print(f"Max retries: {MAX_RETRIES}")
    print(f"Default timeout: {DEFAULT_TIMEOUT}")

def test_complex_types():
    print("\n=== Complex Types Test ===")

    # ConfigDict
    config: ConfigDict = {
        "max_connections": 10,
        "timeout": 30.5,
        "debug_mode": True,
        "allowed_hosts": ["localhost", "127.0.0.1"]
    }

    print(f"Config: {config}")

    # DataProcessor
    processor = DataProcessor({
        "filter_none": True,
        "uppercase_strings": True
    })

    data = ["hello", None, "world", None, "test"]
    processed = processor.process_data(data)
    print(f"Processed data: {processed}")

def test_union_optional():
    print("\n=== Union and Optional Types Test ===")

    # Union types
    print(f"Process int: {process_value(42)}")
    print(f"Process str: {process_value('hello')}")
    print(f"Process None: {process_value(None)}")

    # Optional in function signature
    def find_value(items: List[int], target: int) -> Optional[int]:
        try:
            return items.index(target)
        except ValueError:
            return None

    items = [1, 2, 3, 4, 5]
    print(f"Find 3: {find_value(items, 3)}")
    print(f"Find 10: {find_value(items, 10)}")

if __name__ == "__main__":
    test_type_hints()
    test_generics()
    test_dataclasses()
    test_enums()
    test_protocols()
    test_typeddict()
    test_abstract_classes()
    test_advanced_types()
    test_literal_types()
    test_complex_types()
    test_union_optional()

    print("\n=== All type hints and data tests passed! ===")