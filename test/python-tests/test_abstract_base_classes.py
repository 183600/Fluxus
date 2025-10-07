#!/usr/bin/env python3
"""
测试Python的Abstract Base Classes (ABC) - 抽象基类
"""

import abc
from abc import ABC, abstractmethod, abstractproperty
import math

# 示例1: 基本抽象基类
class Shape(ABC):
    """抽象基类：形状"""
    
    @abstractmethod
    def area(self):
        """计算面积 - 必须由子类实现"""
        pass
    
    @abstractmethod
    def perimeter(self):
        """计算周长 - 必须由子类实现"""
        pass
    
    def describe(self):
        """通用描述方法 - 可以被重写"""
        return f"This is a {self.__class__.__name__}"

class Circle(Shape):
    """圆形 - 实现Shape抽象基类"""
    
    def __init__(self, radius):
        self.radius = radius
    
    def area(self):
        return math.pi * self.radius ** 2
    
    def perimeter(self):
        return 2 * math.pi * self.radius

class Rectangle(Shape):
    """矩形 - 实现Shape抽象基类"""
    
    def __init__(self, width, height):
        self.width = width
        self.height = height
    
    def area(self):
        return self.width * self.height
    
    def perimeter(self):
        return 2 * (self.width + self.height)

# 示例2: 带属性的抽象基类
class Vehicle(ABC):
    """抽象基类：交通工具"""
    
    def __init__(self, brand, model):
        self._brand = brand
        self._model = model
        self._speed = 0
    
    @property
    @abstractmethod
    def vehicle_type(self):
        """交通工具类型 - 抽象属性"""
        pass
    
    @property
    def brand(self):
        """品牌 - 只读属性"""
        return self._brand
    
    @property
    def model(self):
        """型号 - 只读属性"""
        return self._model
    
    @property
    def speed(self):
        """速度 - 可读写属性"""
        return self._speed
    
    @speed.setter
    @abstractmethod
    def speed(self, value):
        """设置速度 - 抽象setter"""
        pass
    
    @abstractmethod
    def start_engine(self):
        """启动引擎 - 抽象方法"""
        pass
    
    def stop_engine(self):
        """停止引擎 - 通用实现"""
        self._speed = 0
        return f"{self._brand} {self._model} engine stopped"

class Car(Vehicle):
    """汽车 - 实现Vehicle抽象基类"""
    
    @property
    def vehicle_type(self):
        return "Car"
    
    @property
    def speed(self):
        return super().speed
    
    @speed.setter
    def speed(self, value):
        if value < 0:
            raise ValueError("Speed cannot be negative")
        if value > 250:  # 最高速度限制
            raise ValueError("Speed cannot exceed 250 km/h")
        self._speed = value
    
    def start_engine(self):
        return f"{self.brand} {self.model} car engine started"

class Motorcycle(Vehicle):
    """摩托车 - 实现Vehicle抽象基类"""
    
    @property
    def vehicle_type(self):
        return "Motorcycle"
    
    @property
    def speed(self):
        return super().speed
    
    @speed.setter
    def speed(self, value):
        if value < 0:
            raise ValueError("Speed cannot be negative")
        if value > 300:  # 摩托车最高速度限制
            raise ValueError("Speed cannot exceed 300 km/h")
        self._speed = value
    
    def start_engine(self):
        return f"{self.brand} {self.model} motorcycle engine started"

# 示例3: 使用抽象基类作为接口
def test_vehicle_operations(vehicle: Vehicle):
    """测试任何Vehicle的子类 - 多态性演示"""
    print(f"Vehicle type: {vehicle.vehicle_type}")
    print(f"Brand: {vehicle.brand}, Model: {vehicle.model}")
    print(f"Start engine: {vehicle.start_engine()}")
    
    vehicle.speed = 50
    print(f"Current speed: {vehicle.speed} km/h")
    
    vehicle.speed = 100
    print(f"Accelerated to: {vehicle.speed} km/h")
    
    print(f"Stop engine: {vehicle.stop_engine()}")
    print(f"Speed after stopping: {vehicle.speed} km/h")
    print("-" * 50)

# 示例4: 抽象方法验证
def test_abstract_methods():
    """测试抽象方法的强制执行"""
    print("=== Testing Abstract Method Enforcement ===")
    
    try:
        # 尝试实例化抽象基类应该失败
        shape = Shape()
        print("ERROR: Should not be able to instantiate abstract class")
    except TypeError as e:
        print(f"✓ Correctly prevented abstract class instantiation: {e}")
    
    try:
        # 尝试实例化未完全实现的子类应该失败
        class IncompleteShape(Shape):
            def area(self):
                return 0
            # 缺少 perimeter 方法的实现
        
        incomplete = IncompleteShape()
        print("ERROR: Should not be able to instantiate incomplete implementation")
    except TypeError as e:
        print(f"✓ Correctly prevented incomplete implementation: {e}")

# 示例5: 注册虚拟子类
class Drawable(ABC):
    """可绘制对象的抽象基类"""
    
    @abstractmethod
    def draw(self):
        """绘制方法"""
        pass

@Drawable.register
class Circle2:
    """通过注册成为Drawable的虚拟子类"""
    
    def __init__(self, radius):
        self.radius = radius
    
    def draw(self):
        return f"Drawing circle with radius {self.radius}"

def test_virtual_subclass():
    """测试虚拟子类注册"""
    print("\n=== Testing Virtual Subclass Registration ===")
    
    circle = Circle2(5)
    print(f"Is Circle2 a subclass of Drawable? {issubclass(Circle2, Drawable)}")
    print(f"Is circle an instance of Drawable? {isinstance(circle, Drawable)}")
    print(f"Drawing: {circle.draw()}")

# 示例6: 抽象属性验证
class AbstractPropertyDemo(ABC):
    """演示抽象属性的抽象基类"""
    
    @property
    @abstractmethod
    def read_only_value(self):
        """只读抽象属性"""
        pass
    
    @property
    @abstractmethod
    def read_write_value(self):
        """可读写抽象属性"""
        pass
    
    @read_write_value.setter
    @abstractmethod
    def read_write_value(self, value):
        """抽象属性setter"""
        pass

class ConcreteDemo(AbstractPropertyDemo):
    """具体实现类"""
    
    def __init__(self):
        self._read_only = "read-only-value"
        self._read_write = "initial-value"
    
    @property
    def read_only_value(self):
        return self._read_only
    
    @property
    def read_write_value(self):
        return self._read_write
    
    @read_write_value.setter
    def read_write_value(self, value):
        if not isinstance(value, str):
            raise TypeError("Value must be a string")
        self._read_write = value

def test_abstract_properties():
    """测试抽象属性"""
    print("\n=== Testing Abstract Properties ===")
    
    demo = ConcreteDemo()
    print(f"Read-only value: {demo.read_only_value}")
    print(f"Read-write value (initial): {demo.read_write_value}")
    
    demo.read_write_value = "new-value"
    print(f"Read-write value (updated): {demo.read_write_value}")
    
    try:
        demo.read_only_value = "new-value"  # 应该失败
        print("ERROR: Should not be able to set read-only property")
    except AttributeError as e:
        print(f"✓ Correctly prevented setting read-only property: {e}")

def main():
    """主测试函数"""
    print("Python Abstract Base Classes (ABC) Demonstration")
    print("=" * 60)
    
    # 测试1: 基本抽象基类
    print("=== Testing Basic Abstract Base Classes ===")
    
    # 创建具体形状
    circle = Circle(5)
    rectangle = Rectangle(4, 6)
    
    shapes = [circle, rectangle]
    
    for shape in shapes:
        print(f"Shape: {shape.describe()}")
        print(f"Area: {shape.area():.2f}")
        print(f"Perimeter: {shape.perimeter():.2f}")
        print()
    
    # 测试2: 带属性的抽象基类
    print("=== Testing Abstract Base Classes with Properties ===")
    
    car = Car("Toyota", "Camry")
    motorcycle = Motorcycle("Harley-Davidson", "Street 750")
    
    vehicles = [car, motorcycle]
    
    for vehicle in vehicles:
        test_vehicle_operations(vehicle)
    
    # 测试3: 抽象方法验证
    test_abstract_methods()
    
    # 测试4: 虚拟子类
    test_virtual_subclass()
    
    # 测试5: 抽象属性
    test_abstract_properties()
    
    # 测试6: 类型检查和验证
    print("\n=== Type Checking and Validation ===")
    
    print(f"Circle is subclass of Shape: {issubclass(Circle, Shape)}")
    print(f"Rectangle is subclass of Shape: {issubclass(Rectangle, Shape)}")
    print(f"Circle instance is Shape: {isinstance(circle, Shape)}")
    print(f"Rectangle instance is Shape: {isinstance(rectangle, Shape)}")
    
    # 测试7: ABC模块的其他功能
    print("\n=== ABC Module Features ===")
    
    # 获取抽象方法
    print(f"Shape abstract methods: {Shape.__abstractmethods__}")
    print(f"Vehicle abstract methods: {Vehicle.__abstractmethods__}")
    
    # 检查类是否是抽象基类
    print(f"Shape is ABC: {Shape.__class__ is abc.ABCMeta}")
    print(f"Circle is ABC: {Circle.__class__ is abc.ABCMeta}")
    
    print("\n=== ABC demonstration completed ===")
    print("Key concepts covered:")
    print("- Abstract methods (@abstractmethod)")
    print("- Abstract properties (@property + @abstractmethod)")
    print("- Abstract base class instantiation prevention")
    print("- Virtual subclass registration")
    print("- Type checking with abstract base classes")

if __name__ == "__main__":
    main()
