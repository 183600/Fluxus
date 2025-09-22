#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
包含Python所有主要语法特性的代码示例
"""

# 1. 基本数据类型和操作
# 整数、浮点数、复数
int_var = 42
float_var = 3.14159
complex_var = 2 + 3j

# 字符串
str_var = "Hello, World!"
multiline_str = """这是一个
多行字符串"""

# 布尔值
bool_var = True
false_var = False

# 2. 容器类型
# 列表
list_var = [1, 2, 3, "four", 5.0]
nested_list = [[1, 2], [3, 4]]

# 元组
tuple_var = (1, 2, 3, "four")
single_tuple = (1,)  # 单元素元组

# 字典
dict_var = {"name": "Alice", "age": 30, "scores": [85, 90, 95]}
nested_dict = {"person": {"name": "Bob", "age": 25}, "active": True}

# 集合
set_var = {1, 2, 3, 3, 2, 1}  # {1, 2, 3}
frozenset_var = frozenset([4, 5, 6])

# 3. 控制流
# 条件语句
if int_var > 0:
    print("正数")
elif int_var < 0:
    print("负数")
else:
    print("零")

# 三元表达式
result = "正数" if int_var > 0 else "非正数"

# for循环
for i in range(5):
    print(f"循环索引: {i}")

# 遍历列表
for item in list_var:
    print(f"列表项: {item}")

# 遍历字典
for key, value in dict_var.items():
    print(f"字典项: {key} = {value}")

# while循环
count = 0
while count < 3:
    print(f"计数: {count}")
    count += 1

# break和continue
for i in range(10):
    if i == 5:
        break
    if i % 2 == 0:
        continue
    print(f"奇数: {i}")

# 4. 函数
# 基本函数
def greet(name):
    """简单的问候函数"""
    return f"Hello, {name}!"

# 带默认参数的函数
def power(base, exponent=2):
    return base ** exponent

# 带可变参数的函数
def sum_all(*args):
    return sum(args)

# 带关键字参数的函数
def person_info(name, **kwargs):
    info = {"name": name}
    info.update(kwargs)
    return info

# lambda函数
multiply = lambda x, y: x * y

# 5. 类和对象
class Person:
    """人类"""
    # 类变量
    species = "Homo sapiens"
    
    def __init__(self, name, age):
        # 实例变量
        self.name = name
        self.age = age
    
    def greet(self):
        return f"Hi, I'm {self.name}."
    
    def celebrate_birthday(self):
        self.age += 1
        return f"Happy {self.age}th birthday!"
    
    @classmethod
    def from_birth_year(cls, name, birth_year):
        age = 2025 - birth_year
        return cls(name, age)
    
    @staticmethod
    def is_adult(age):
        return age >= 18

# 继承
class Student(Person):
    def __init__(self, name, age, student_id):
        super().__init__(name, age)
        self.student_id = student_id
        self.courses = []
    
    def enroll(self, course):
        self.courses.append(course)
        return f"Enrolled in {course}"

# 6. 异常处理
try:
    # 尝试执行可能出错的代码
    result = 10 / 0
except ZeroDivisionError:
    # 处理特定异常
    print("除零错误")
except Exception as e:
    # 处理其他异常
    print(f"发生错误: {e}")
else:
    # 如果没有异常
    print("计算成功")
finally:
    # 无论是否有异常都会执行
    print("清理操作")

# 自定义异常
class CustomError(Exception):
    pass

try:
    raise CustomError("这是一个自定义错误")
except CustomError as e:
    print(f"捕获到自定义错误: {e}")

# 7. 文件操作
# 写入文件
with open("test_file.txt", "w") as f:
    f.write("Hello, file!\n")
    f.write("This is a test.\n")

# 读取文件
with open("test_file.txt", "r") as f:
    content = f.read()
    print(f"文件内容:\n{content}")

# 逐行读取
with open("test_file.txt", "r") as f:
    for line in f:
        print(f"行: {line.strip()}")

# 8. 模块和包
import math
import random
from datetime import datetime, date
import os.path as path

# 使用模块
print(f"圆周率: {math.pi}")
print(f"随机数: {random.randint(1, 100)}")
print(f"当前日期: {date.today()}")

# 9. 列表推导式、生成器表达式和字典推导式
# 列表推导式
squares = [x**2 for x in range(10)]
even_squares = [x**2 for x in range(10) if x % 2 == 0]

# 生成器表达式
square_gen = (x**2 for x in range(10))
for square in square_gen:
    print(f"生成器平方: {square}")

# 字典推导式
square_dict = {x: x**2 for x in range(5)}

# 集合推导式
square_set = {x**2 for x in range(5)}

# 10. 装饰器
def timer(func):
    """计时装饰器"""
    import time
    
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} 执行时间: {end - start:.5f}秒")
        return result
    return wrapper

@timer
def fibonacci(n):
    """计算斐波那契数列"""
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

# 11. 上下文管理器
class ManagedFile:
    def __init__(self, filename, mode):
        self.filename = filename
        self.mode = mode
    
    def __enter__(self):
        self.file = open(self.filename, self.mode)
        return self.file
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.file.close()

# 使用自定义上下文管理器
with ManagedFile("test_file.txt", "r") as f:
    content = f.read()
    print(f"使用上下文管理器读取: {content[:20]}...")

# 12. 迭代器和生成器
class Countdown:
    """倒计时迭代器"""
    def __init__(self, start):
        self.start = start
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.start <= 0:
            raise StopIteration
        self.start -= 1
        return self.start

# 使用迭代器
for i in Countdown(5):
    print(f"倒计时: {i}")

# 生成器函数
def simple_generator():
    yield 1
    yield 2
    yield 3

# 使用生成器
for value in simple_generator():
    print(f"生成器值: {value}")

# 13. 多线程和多进程
import threading
import time
import multiprocessing

def worker_function(name):
    """工作线程函数"""
    print(f"工作线程 {name} 开始")
    time.sleep(1)
    print(f"工作线程 {name} 结束")

# 创建并启动线程
threads = []
for i in range(3):
    thread = threading.Thread(target=worker_function, args=(f"Thread-{i}",))
    threads.append(thread)
    thread.start()

# 等待所有线程完成
for thread in threads:
    thread.join()

# 14. 正则表达式
import re

text = "我的电话号码是 123-456-7890，邮箱是 example@example.com"
phone_pattern = r'\d{3}-\d{3}-\d{4}'
email_pattern = r'\w+@\w+\.\w+'

phone_match = re.search(phone_pattern, text)
email_match = re.search(email_pattern, text)

if phone_match:
    print(f"找到电话号码: {phone_match.group()}")

if email_match:
    print(f"找到邮箱: {email_match.group()}")

# 15. JSON处理
import json

# Python对象转JSON
data = {
    "name": "Alice",
    "age": 30,
    "scores": [85, 90, 95],
    "active": True
}

json_str = json.dumps(data, indent=2)
print(f"JSON字符串:\n{json_str}")

# JSON转Python对象
parsed_data = json.loads(json_str)
print(f"解析后的数据: {parsed_data}")

# 16. 日期和时间处理
from datetime import datetime, timedelta, date

# 当前时间
now = datetime.now()
print(f"当前时间: {now}")

# 格式化时间
formatted = now.strftime("%Y-%m-%d %H:%M:%S")
print(f"格式化时间: {formatted}")

# 时间加减
tomorrow = now + timedelta(days=1)
print(f"明天: {tomorrow}")

# 17. 类型提示
from typing import List, Dict, Tuple, Optional, Union

def process_items(items: List[str]) -> Dict[str, int]:
    """处理字符串列表，返回每个字符串的长度"""
    result = {}
    for item in items:
        result[item] = len(item)
    return result

def get_value(data: Dict[str, int], key: str) -> Optional[int]:
    """从字典中获取值，可能返回None"""
    return data.get(key)

# 18. 枚举
from enum import Enum, auto

class Color(Enum):
    RED = auto()
    GREEN = auto()
    BLUE = auto()

class Status(Enum):
    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"

print(f"颜色枚举: {Color.RED}")
print(f"状态枚举: {Status.PENDING.value}")

# 19. 数据类
from dataclasses import dataclass

@dataclass
class Product:
    name: str
    price: float
    quantity: int = 1
    
    def total_value(self) -> float:
        return self.price * self.quantity

product = Product("Laptop", 999.99, 2)
print(f"产品: {product.name}, 总价: {product.total_value()}")

# 20. 异步编程
import asyncio

async def say_after(delay, what_to_say):
    """异步函数，延迟后输出内容"""
    await asyncio.sleep(delay)
    print(what_to_say)

async def main():
    """主异步函数"""
    task1 = asyncio.create_task(say_after(1, "Hello"))
    task2 = asyncio.create_task(say_after(2, "World"))
    
    await task1
    await task2

# 运行异步代码
# asyncio.run(main())  # 注释掉，避免在测试时运行

# 21. 属性和描述符
class Temperature:
    """温度类，使用属性"""
    def __init__(self, celsius=0):
        self._celsius = celsius
    
    @property
    def celsius(self):
        return self._celsius
    
    @celsius.setter
    def celsius(self, value):
        if value < -273.15:
            raise ValueError("温度不能低于绝对零度")
        self._celsius = value
    
    @property
    def fahrenheit(self):
        return self._celsius * 9/5 + 32

temp = Temperature(25)
print(f"摄氏度: {temp.celsius}°C")
print(f"华氏度: {temp.fahrenheit}°F")
temp.celsius = 30
print(f"新的摄氏度: {temp.celsius}°C")

# 22. 元类
class Meta(type):
    """自定义元类"""
    def __new__(cls, name, bases, dct):
        # 添加新属性
        dct['class_name'] = name
        # 添加新方法
        def hello(self):
            return f"Hello from {self.class_name}"
        dct['hello'] = hello
        return super().__new__(cls, name, bases, dct)

class MyClass(metaclass=Meta):
    pass

obj = MyClass()
print(f"类名: {obj.class_name}")
print(f"问候: {obj.hello()}")

# 23. 抽象基类
from abc import ABC, abstractmethod

class Shape(ABC):
    """形状抽象基类"""
    @abstractmethod
    def area(self):
        pass
    
    @abstractmethod
    def perimeter(self):
        pass

class Rectangle(Shape):
    """矩形类"""
    def __init__(self, width, height):
        self.width = width
        self.height = height
    
    def area(self):
        return self.width * self.height
    
    def perimeter(self):
        return 2 * (self.width + self.height)

rect = Rectangle(5, 3)
print(f"矩形面积: {rect.area()}")
print(f"矩形周长: {rect.perimeter()}")

# 24. 闭包和函数工厂
def make_multiplier(n):
    """创建一个乘法函数"""
    def multiplier(x):
        return x * n
    return multiplier

times3 = make_multiplier(3)
times5 = make_multiplier(5)

print(f"3倍: {times3(10)}")
print(f"5倍: {times5(10)}")

# 25. 内存管理和弱引用
import weakref

class BigObject:
    def __del__(self):
        print("BigObject 被销毁")

obj = BigObject()
weak_ref = weakref.ref(obj)

print(f"对象是否存活: {weak_ref() is not None}")
del obj
print(f"对象是否存活: {weak_ref() is not None}")

# 26. 序列化和pickle
import pickle

# 序列化对象
data = {"name": "Alice", "age": 30, "scores": [85, 90, 95]}

with open("data.pkl", "wb") as f:
    pickle.dump(data, f)

# 反序列化对象
with open("data.pkl", "rb") as f:
    loaded_data = pickle.load(f)

print(f"反序列化的数据: {loaded_data}")

# 27. 上下文变量
import contextvars

user_var = contextvars.ContextVar('user', default='Anonymous')

def process_request():
    user = user_var.get()
    print(f"处理请求，用户: {user}")

# 设置上下文变量
token = user_var.set("Alice")
process_request()

# 重置上下文变量
user_var.reset(token)
process_request()

# 28. 结构化模式匹配 (Python 3.10+)
def match_example(value):
    match value:
        case 0:
            print("零")
        case 1:
            print("一")
        case [x, y]:
            print(f"列表，元素: {x}, {y}")
        case {"name": name, "age": age}:
            print(f"字典，姓名: {name}, 年龄: {age}")
        case _:
            print("其他")

match_example(0)
match_example([1, 2])
match_example({"name": "Bob", "age": 25})
match_example("test")

# 29. 赋值表达式 (海象运算符)
numbers = [1, 2, 3, 4, 5]
while (n := len(numbers)) > 0:
    popped = numbers.pop()
    print(f"弹出: {popped}, 剩余: {n-1}")

# 30. 位置参数和关键字参数
def example_func(a, b, /, c, d, *, e, f):
    """展示不同类型的参数:
    - a, b: 仅位置参数
    - c, d: 位置或关键字参数
    - e, f: 仅关键字参数
    """
    print(f"a={a}, b={b}, c={c}, d={d}, e={e}, f={f}")

example_func(1, 2, 3, d=4, e=5, f=6)

# 31. 位置参数解包
def add(a, b, c):
    return a + b + c

values = [1, 2, 3]
result = add(*values)
print(f"解包结果: {result}")

# 32. 关键字参数解包
def greet(name, greeting):
    return f"{greeting}, {name}!"

params = {"name": "Alice", "greeting": "Hello"}
result = greet(**params)
print(f"关键字解包结果: {result}")

# 33. 链式比较
x = 10
if 0 < x < 20:
    print("x 在 0 和 20 之间")

# 34. 位运算
a = 60  # 0011 1100
b = 13  # 0000 1101

print(f"a & b = {a & b}")   # 12 (0000 1100)
print(f"a | b = {a | b}")   # 61 (0011 1101)
print(f"a ^ b = {a ^ b}")   # 49 (0011 0001)
print(f"~a = {~a}")         # -61 (1100 0011)
print(f"a << 2 = {a << 2}") # 240 (1111 0000)
print(f"a >> 2 = {a >> 2}") # 15 (0000 1111)

# 35. 运算符重载
class Vector:
    """向量类，展示运算符重载"""
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)
    
    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
    
    def __str__(self):
        return f"Vector({self.x}, {self.y})"

v1 = Vector(1, 2)
v2 = Vector(3, 4)

print(f"v1 + v2 = {v1 + v2}")
print(f"v1 - v2 = {v1 - v2}")
print(f"v1 * 3 = {v1 * 3}")
print(f"v1 == v2 = {v1 == v2}")

# 36. 单例模式
class Singleton:
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

s1 = Singleton()
s2 = Singleton()
print(f"s1 是 s2: {s1 is s2}")

# 37. 工厂模式
class Dog:
    def speak(self):
        return "Woof!"

class Cat:
    def speak(self):
        return "Meow!"

def animal_factory(animal_type):
    """动物工厂"""
    if animal_type == "dog":
        return Dog()
    elif animal_type == "cat":
        return Cat()
    else:
        raise ValueError(f"未知动物类型: {animal_type}")

dog = animal_factory("dog")
cat = animal_factory("cat")
print(f"狗说: {dog.speak()}")
print(f"猫说: {cat.speak()}")

# 38. 观察者模式
class Subject:
    """主题（被观察者）"""
    def __init__(self):
        self._observers = []
        self._state = None
    
    def attach(self, observer):
        if observer not in self._observers:
            self._observers.append(observer)
    
    def detach(self, observer):
        try:
            self._observers.remove(observer)
        except ValueError:
            pass
    
    def notify(self):
        for observer in self._observers:
            observer.update(self)
    
    @property
    def state(self):
        return self._state
    
    @state.setter
    def state(self, value):
        self._state = value
        self.notify()

class Observer:
    """观察者"""
    def update(self, subject):
        print(f"观察者收到更新: {subject.state}")

subject = Subject()
observer1 = Observer()
observer2 = Observer()

subject.attach(observer1)
subject.attach(observer2)

subject.state = "新状态"

# 39. 策略模式
class Strategy:
    """策略基类"""
    def execute(self, a, b):
        pass

class AddStrategy(Strategy):
    def execute(self, a, b):
        return a + b

class SubtractStrategy(Strategy):
    def execute(self, a, b):
        return a - b

class MultiplyStrategy(Strategy):
    def execute(self, a, b):
        return a * b

class Context:
    """上下文"""
    def __init__(self, strategy):
        self._strategy = strategy
    
    def set_strategy(self, strategy):
        self._strategy = strategy
    
    def execute_strategy(self, a, b):
        return self._strategy.execute(a, b)

context = Context(AddStrategy())
print(f"加法: {context.execute_strategy(5, 3)}")

context.set_strategy(SubtractStrategy())
print(f"减法: {context.execute_strategy(5, 3)}")

context.set_strategy(MultiplyStrategy())
print(f"乘法: {context.execute_strategy(5, 3)}")

# 40. 装饰器模式
class Coffee:
    """咖啡基类"""
    def cost(self):
        return 5
    
    def description(self):
        return "基础咖啡"

class MilkDecorator:
    """牛奶装饰器"""
    def __init__(self, coffee):
        self._coffee = coffee
    
    def cost(self):
        return self._coffee.cost() + 2
    
    def description(self):
        return self._coffee.description() + ", 牛奶"

class SugarDecorator:
    """糖装饰器"""
    def __init__(self, coffee):
        self._coffee = coffee
    
    def cost(self):
        return self._coffee.cost() + 1
    
    def description(self):
        return self._coffee.description() + ", 糖"

coffee = Coffee()
print(f"{coffee.description()}: ${coffee.cost()}")

coffee_with_milk = MilkDecorator(coffee)
print(f"{coffee_with_milk.description()}: ${coffee_with_milk.cost()}")

coffee_with_milk_and_sugar = SugarDecorator(coffee_with_milk)
print(f"{coffee_with_milk_and_sugar.description()}: ${coffee_with_milk_and_sugar.cost()}")

# 41. 上下文管理器装饰器
from contextlib import contextmanager

@contextmanager
def managed_resource(resource_name):
    """上下文管理器装饰器示例"""
    print(f"获取资源: {resource_name}")
    try:
        yield resource_name
    finally:
        print(f"释放资源: {resource_name}")

with managed_resource("数据库连接"):
    print("使用资源")

# 42. 函数注解
def calculate(
    a: float,
    b: float,
    operation: str = "add"
) -> float:
    """带详细注解的函数"""
    if operation == "add":
        return a + b
    elif operation == "subtract":
        return a - b
    elif operation == "multiply":
        return a * b
    elif operation == "divide":
        return a / b
    else:
        raise ValueError(f"未知操作: {operation}")

# 43. 类型别名
from typing import List, Dict

Vector = List[float]
Matrix = List[Vector]
UserInfo = Dict[str, Union[str, int, List[int]]]

def dot_product(v1: Vector, v2: Vector) -> float:
    """计算向量点积"""
    return sum(x * y for x, y in zip(v1, v2))

# 44. 泛型
from typing import TypeVar, Generic

T = TypeVar('T')

class Stack(Generic[T]):
    """泛型栈类"""
    def __init__(self):
        self._items = []
    
    def push(self, item: T) -> None:
        self._items.append(item)
    
    def pop(self) -> T:
        return self._items.pop()
    
    def is_empty(self) -> bool:
        return len(self._items) == 0

# 使用泛型栈
int_stack = Stack[int]()
int_stack.push(1)
int_stack.push(2)
print(f"弹出整数: {int_stack.pop()}")

str_stack = Stack[str]()
str_stack.push("Hello")
str_stack.push("World")
print(f"弹出字符串: {str_stack.pop()}")

# 45. 协议
from typing import Protocol

class Drawable(Protocol):
    def draw(self) -> str:
        ...

class Circle:
    def draw(self) -> str:
        return "绘制圆形"

class Square:
    def draw(self) -> str:
        return "绘制正方形"

def render_shape(shape: Drawable) -> None:
    print(shape.draw())

circle = Circle()
square = Square()

render_shape(circle)
render_shape(square)

# 46. 可调用对象
class Adder:
    def __init__(self, n):
        self.n = n
    
    def __call__(self, x):
        return self.n + x

add5 = Adder(5)
print(f"5 + 3 = {add5(3)}")

# 47. 描述符协议
class ValidatedAttribute:
    """验证属性描述符"""
    def __init__(self, name, type_, default=None):
        self.name = name
        self.type_ = type_
        self.default = default
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        value = obj.__dict__.get(self.name, self.default)
        if value is None:
            return None
        if not isinstance(value, self.type_):
            raise TypeError(f"{self.name} 必须是 {self.type_}")
        return value
    
    def __set__(self, obj, value):
        if value is not None and not isinstance(value, self.type_):
            raise TypeError(f"{self.name} 必须是 {self.type_}")
        obj.__dict__[self.name] = value

class Person:
    name = ValidatedAttribute("name", str)
    age = ValidatedAttribute("age", int, 0)
    height = ValidatedAttribute("height", float)

person = Person()
person.name = "Alice"
person.age = 30
person.height = 170.5

print(f"姓名: {person.name}, 年龄: {person.age}, 身高: {person.height}")

try:
    person.age = "三十"  # 这将引发TypeError
except TypeError as e:
    print(f"错误: {e}")

# 48. 上下文管理器协议
class Timer:
    """计时器上下文管理器"""
    def __init__(self, name):
        self.name = name
        self.start = None
        self.end = None
    
    def __enter__(self):
        import time
        self.start = time.time()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        import time
        self.end = time.time()
        elapsed = self.end - self.start
        print(f"{self.name} 耗时: {elapsed:.5f}秒")
        return False  # 不抑制异常

with Timer("代码块"):
    import time
    time.sleep(0.1)

# 49. 迭代器协议
class Fibonacci:
    """斐波那契数列迭代器"""
    def __init__(self, max_count):
        self.max_count = max_count
        self.count = 0
        self.a, self.b = 0, 1
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.count >= self.max_count:
            raise StopIteration
        self.count += 1
        result = self.a
        self.a, self.b = self.b, self.a + self.b
        return result

for num in Fibonacci(10):
    print(f"斐波那契数: {num}")

# 50. 可迭代对象协议
class Range:
    """自定义范围类"""
    def __init__(self, start, end, step=1):
        self.start = start
        self.end = end
        self.step = step
    
    def __iter__(self):
        return RangeIterator(self.start, self.end, self.step)

class RangeIterator:
    """范围迭代器"""
    def __init__(self, start, end, step):
        self.current = start
        self.end = end
        self.step = step
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.step > 0 and self.current >= self.end:
            raise StopIteration
        if self.step < 0 and self.current <= self.end:
            raise StopIteration
        
        result = self.current
        self.current += self.step
        return result

for i in Range(1, 5):
    print(f"范围数: {i}")

for i in Range(5, 0, -1):
    print(f"倒序范围数: {i}")

# 51. 生成器表达式和yield from
def chain(*iterables):
    """连接多个可迭代对象"""
    for it in iterables:
        yield from it

for item in chain([1, 2], ['a', 'b'], [3, 4]):
    print(f"链式项: {item}")

# 52. 协程和异步生成器
async def async_range(n):
    """异步生成器"""
    for i in range(n):
        yield i
        await asyncio.sleep(0.1)

async def async_main():
    """异步主函数"""
    async for num in async_range(5):
        print(f"异步数: {num}")

# asyncio.run(async_main())  # 注释掉，避免在测试时运行

# 53. 类型检查
import typing

def check_type(value, expected_type):
    """检查值是否符合预期类型"""
    if not isinstance(value, expected_type):
        raise TypeError(f"期望 {expected_type}, 得到 {type(value)}")
    return True

# 使用示例
check_type(42, int)
check_type("hello", str)

try:
    check_type(3.14, int)
except TypeError as e:
    print(f"类型错误: {e}")

# 54. 性能分析
import timeit

def test_performance():
    """测试性能的函数"""
    total = 0
    for i in range(1000):
        total += i
    return total

# 测量执行时间
execution_time = timeit.timeit(test_performance, number=1000)
print(f"执行时间: {execution_time:.5f}秒")

# 55. 内存分析
import sys

def get_size(obj):
    """获取对象大小"""
    return sys.getsizeof(obj)

print(f"整数大小: {get_size(42)} 字节")
print(f"字符串大小: {get_size('hello')} 字节")
print(f"列表大小: {get_size([1, 2, 3])} 字节")

# 56. 代码对象和动态执行
code_str = """
def dynamic_function(x, y):
    return x + y

result = dynamic_function(10, 20)
print(f"动态执行结果: {result}")
"""

# 编译并执行代码
code_obj = compile(code_str, "<string>", "exec")
exec(code_obj)

# 57. 函数属性
def example_function():
    """示例函数"""
    pass

# 设置函数属性
example_function.author = "Alice"
example_function.version = "1.0"

print(f"函数作者: {example_function.author}")
print(f"函数版本: {example_function.version}")

# 58. 闭包中的变量作用域
def outer_function(x):
    def inner_function(y):
        return x + y
    return inner_function

add5 = outer_function(5)
print(f"闭包结果: {add5(10)}")

# 59. nonlocal和global关键字
x = 10  # 全局变量

def modify_global():
    global x
    x = 20
    print(f"内部修改全局变量: {x}")

def outer():
    x = 5  # 外层函数变量
    
    def inner():
        nonlocal x
        x = 10
        print(f"内部修改外层变量: {x}")
    
    print(f"修改前外层变量: {x}")
    inner()
    print(f"修改后外层变量: {x}")

print(f"全局变量初始值: {x}")
modify_global()
print(f"全局变量修改后: {x}")
outer()

# 60. 元组解包
a, b, c = (1, 2, 3)
print(f"元组解包: a={a}, b={b}, c={c}")

# 交换变量
a, b = b, a
print(f"交换后: a={a}, b={b}")

# 61. 星号解包
first, *middle, last = [1, 2, 3, 4, 5]
print(f"首项: {first}, 中间项: {middle}, 末项: {last}")

# 62. 字典解包
dict1 = {"a": 1, "b": 2}
dict2 = {"c": 3, "d": 4}
merged_dict = {**dict1, **dict2}
print(f"合并字典: {merged_dict}")

# 63. 集合操作
set1 = {1, 2, 3}
set2 = {3, 4, 5}

print(f"并集: {set1 | set2}")
print(f"交集: {set1 & set2}")
print(f"差集: {set1 - set2}")
print(f"对称差集: {set1 ^ set2}")

# 64. 列表方法
numbers = [1, 2, 3, 4, 5]
numbers.append(6)
numbers.insert(0, 0)
numbers.extend([7, 8, 9])
numbers.remove(3)
popped = numbers.pop()
print(f"弹出元素: {popped}")
print(f"列表: {numbers}")
print(f"索引: {numbers.index(4)}")
print(f"计数: {numbers.count(2)}")
numbers.sort()
print(f"排序后: {numbers}")
numbers.reverse()
print(f"反转后: {numbers}")

# 65. 字典方法
student = {"name": "Alice", "age": 25, "courses": ["Math", "Science"]}
print(f"键: {student.keys()}")
print(f"值: {student.values()}")
print(f"键值对: {student.items()}")
print(f"获取姓名: {student.get('name')}")
print(f"获取成绩: {student.get('grades', 'N/A')}")
student.update({"age": 26, "grades": [85, 90, 95]})
print(f"更新后: {student}")
removed = student.pop('courses')
print(f"移除的课程: {removed}")
print(f"最终字典: {student}")

# 66. 字符串方法
text = "  Hello, World!  "
print(f"去除空白: '{text.strip()}'")
print(f"大写: '{text.upper()}'")
print(f"小写: '{text.lower()}'")
print(f"替换: '{text.replace('World', 'Python')}'")
print(f"分割: '{text.split(',')}'")
print(f"连接: {'-'.join(['a', 'b', 'c'])}")
print(f"查找: '{text.find('World')}'")
print(f"计数: '{text.count('l')}'")
print(f"是否以Hello开头: {text.strip().startswith('Hello')}")
print(f"是否以World结尾: {text.strip().endswith('World')}")

# 67. 格式化字符串
name = "Alice"
age = 30
height = 1.65

# f-strings (Python 3.6+)
print(f"姓名: {name}, 年龄: {age}, 身高: {height:.2f}米")

# str.format()
print("姓名: {}, 年龄: {}, 身高: {:.2f}米".format(name, age, height))

# %格式化
print("姓名: %s, 年龄: %d, 身高: %.2f米" % (name, age, height))

# 68. 文件和目录操作
import os

# 获取当前工作目录
print(f"当前工作目录: {os.getcwd()}")

# 列出目录内容
print(f"目录内容: {os.listdir('.')}")

# 创建目录
os.makedirs("test_dir", exist_ok=True)

# 检查路径是否存在
print(f"test_dir是否存在: {os.path.exists('test_dir')}")

# 检查是否是目录
print(f"test_dir是目录: {os.path.isdir('test_dir')}")

# 检查是否是文件
print(f"test_dir是文件: {os.path.isfile('test_dir')}")

# 获取文件大小
with open("test_file.txt", "w") as f:
    f.write("Hello, World!")
print(f"文件大小: {os.path.getsize('test_file.txt')} 字节")

# 删除文件
os.remove("test_file.txt")

# 删除目录
os.rmdir("test_dir")

# 69. 路径操作
from pathlib import Path

# 创建Path对象
current_dir = Path(".")
home_dir = Path.home()

print(f"当前目录: {current_dir.resolve()}")
print(f"家目录: {home_dir}")

# 创建路径
new_dir = current_dir / "example_dir"
new_dir.mkdir(exist_ok=True)

# 创建文件
file_path = new_dir / "example.txt"
file_path.write_text("Hello, Path!")

# 读取文件
print(f"文件内容: {file_path.read_text()}")

# 遍历目录
print("目录遍历:")
for item in new_dir.iterdir():
    print(f"  {item.name}: {'目录' if item.is_dir() else '文件'}")

# 清理
file_path.unlink()
new_dir.rmdir()

# 70. 环境变量
import os

# 获取环境变量
print(f"PATH: {os.environ.get('PATH', '未设置')}")

# 设置环境变量
os.environ['MY_VAR'] = 'my_value'
print(f"MY_VAR: {os.environ.get('MY_VAR')}")

# 删除环境变量
if 'MY_VAR' in os.environ:
    del os.environ['MY_VAR']
print(f"MY_VAR删除后: {os.environ.get('MY_VAR', '未设置')}")

# 71. 命令行参数
import sys

# 获取命令行参数
print(f"脚本名称: {sys.argv[0]}")
print(f"参数列表: {sys.argv[1:]}")

# 72. 命令行参数解析
import argparse

# 创建解析器
parser = argparse.ArgumentParser(description='示例脚本')
parser.add_argument('--name', type=str, default='World', help='姓名')
parser.add_argument('--count', type=int, default=1, help='计数')
parser.add_argument('--verbose', action='store_true', help='详细输出')

# 模拟解析命令行参数
# 在实际使用中，这些参数来自命令行
args = parser.parse_args(['--name', 'Alice', '--count', '3', '--verbose'])

print(f"姓名: {args.name}")
print(f"计数: {args.count}")
print(f"详细输出: {args.verbose}")

# 73. 日志记录
import logging

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

# 创建日志记录器
logger = logging.getLogger('my_app')

# 记录不同级别的日志
logger.debug("这是调试信息")
logger.info("这是信息")
logger.warning("这是警告")
logger.error("这是错误")
logger.critical("这是严重错误")

# 74. 单元测试
import unittest

class TestMathOperations(unittest.TestCase):
    """数学运算测试"""
    
    def test_add(self):
        self.assertEqual(1 + 1, 2)
    
    def test_subtract(self):
        self.assertEqual(5 - 3, 2)
    
    def test_multiply(self):
        self.assertEqual(3 * 4, 12)
    
    def test_divide(self):
        self.assertEqual(10 / 2, 5)
        with self.assertRaises(ZeroDivisionError):
            10 / 0

# 运行测试
# unittest.main()  # 注释掉，避免在测试时运行

# 75. doctest
def factorial(n):
    """计算阶乘
    
    >>> factorial(0)
    1
    >>> factorial(1)
    1
    >>> factorial(5)
    120
    """
    if n < 0:
        raise ValueError("负数没有阶乘")
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result

# 运行doctest
# import doctest
# doctest.testmod()  # 注释掉，避免在测试时运行

# 76. 内存视图
data = bytearray(b'Hello, World')
mv = memoryview(data)

print(f"内存视图: {mv}")
print(f"前5个字节: {mv[:5].tobytes()}")

# 修改数据
try:
    mv[7:12] = b'Python'
    print(f"修改后: {data}")
except ValueError as e:
    print(f"内存视图修改错误: {e}")
    print(f"原始数据: {data}")

# 77. 结构化数据
import struct

# 打包数据
packed_data = struct.pack('i f s', 42, 3.14, b'Hello')
print(f"打包数据: {packed_data}")

# 解包数据
unpacked_data = struct.unpack('i f s', packed_data)
print(f"解包数据: {unpacked_data}")

# 78. 数组操作
import array

# 创建数组
arr = array.array('i', [1, 2, 3, 4, 5])
print(f"数组: {arr}")

# 添加元素
arr.append(6)
print(f"添加后: {arr}")

# 扩展数组
arr.extend([7, 8, 9])
print(f"扩展后: {arr}")

# 移除元素
arr.remove(3)
print(f"移除后: {arr}")

# 79. 堆队列
import heapq

# 创建列表
heap = [3, 1, 4, 1, 5, 9, 2, 6]
print(f"原始列表: {heap}")

# 转换为堆
heapq.heapify(heap)
print(f"堆化后: {heap}")

# 推入元素
heapq.heappush(heap, 0)
print(f"推入后: {heap}")

# 弹出最小元素
min_item = heapq.heappop(heap)
print(f"弹出最小元素: {min_item}")
print(f"弹出后: {heap}")

# 获取n个最小元素
n_smallest = heapq.nsmallest(3, heap)
print(f"3个最小元素: {n_smallest}")

# 80. 双端队列
from collections import deque

# 创建双端队列
dq = deque([1, 2, 3])
print(f"双端队列: {dq}")

# 右端添加
dq.append(4)
print(f"右端添加后: {dq}")

# 左端添加
dq.appendleft(0)
print(f"左端添加后: {dq}")

# 右端移除
right_item = dq.pop()
print(f"右端移除: {right_item}")
print(f"右端移除后: {dq}")

# 左端移除
left_item = dq.popleft()
print(f"左端移除: {left_item}")
print(f"左端移除后: {dq}")

# 81. 计数器
from collections import Counter

# 创建计数器
counter = Counter(['a', 'b', 'a', 'c', 'b', 'a'])
print(f"计数器: {counter}")

# 访问计数
print(f"a的计数: {counter['a']}")

# 最常见的元素
print(f"最常见的元素: {counter.most_common(2)}")

# 更新计数
counter.update(['a', 'b', 'd'])
print(f"更新后: {counter}")

# 82. 有序字典
from collections import OrderedDict

# 创建有序字典
od = OrderedDict()
od['a'] = 1
od['b'] = 2
od['c'] = 3
print(f"有序字典: {od}")

# 移动到末尾
od.move_to_end('a')
print(f"移动a到末尾: {od}")

# 移动到开头
od.move_to_end('a', last=False)
print(f"移动a到开头: {od}")

# 弹出最后项
last_item = od.popitem()
print(f"弹出最后项: {last_item}")
print(f"弹出后: {od}")

# 83. 默认字典
from collections import defaultdict

# 创建默认字典
dd = defaultdict(int)
dd['a'] += 1
dd['b'] += 2
print(f"默认字典: {dict(dd)}")

# 使用列表作为默认工厂
dd_list = defaultdict(list)
dd_list['a'].append(1)
dd_list['a'].append(2)
dd_list['b'].append(3)
print(f"列表默认字典: {dict(dd_list)}")

# 84. 命名元组
from collections import namedtuple

# 创建命名元组类
Point = namedtuple('Point', ['x', 'y'])

# 创建实例
p1 = Point(1, 2)
p2 = Point(3, 4)

print(f"点1: {p1}")
print(f"点2: {p2}")
print(f"x坐标: {p1.x}")
print(f"y坐标: {p1.y}")

# 85. 链映射
from collections import ChainMap

# 创建字典
dict1 = {'a': 1, 'b': 2}
dict2 = {'b': 3, 'c': 4}
dict3 = {'d': 5}

# 创建链映射
chain = ChainMap(dict1, dict2, dict3)
print(f"链映射: {chain}")

# 查找键
print(f"a的值: {chain['a']}")
print(f"b的值: {chain['b']}")  # 来自dict1，因为它在前面

# 添加新映射
new_chain = chain.new_child({'e': 6})
print(f"新链映射: {new_chain}")

# 86. 可哈希对象
class HashableExample:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __hash__(self):
        return hash((self.x, self.y))
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

obj1 = HashableExample(1, 2)
obj2 = HashableExample(1, 2)
obj3 = HashableExample(3, 4)

print(f"obj1 == obj2: {obj1 == obj2}")
print(f"hash(obj1) == hash(obj2): {hash(obj1) == hash(obj2)}")

# 可以用作字典键或集合元素
d = {obj1: "value1", obj3: "value3"}
print(f"字典: {d}")

s = {obj1, obj2, obj3}
print(f"集合: {s}")

# 87. 弱引用集合
import weakref

class MyClass:
    def __init__(self, name):
        self.name = name
    
    def __repr__(self):
        return f"MyClass({self.name})"

# 创建对象
obj1 = MyClass("Object 1")
obj2 = MyClass("Object 2")

# 创建弱引用集合
weak_set = weakref.WeakSet()
weak_set.add(obj1)
weak_set.add(obj2)

print(f"弱引用集合: {list(weak_set)}")

# 删除一个引用
del obj2
import gc
gc.collect()

print(f"删除obj2后的弱引用集合: {list(weak_set)}")

# 88. 弱引用字典
weak_dict = weakref.WeakValueDictionary()
weak_dict["key1"] = MyClass("Value 1")
weak_dict["key2"] = MyClass("Value 2")

print(f"弱引用字典: {dict(weak_dict)}")

# 删除一个引用
try:
    value1 = weak_dict["key1"]
    del weak_dict["key1"]
    gc.collect()
    print(f"删除key1后的弱引用字典: {dict(weak_dict)}")
except KeyError as e:
    print(f"弱引用字典错误: {e}")
    print(f"当前弱引用字典: {dict(weak_dict)}")

# 89. 不可变集合
immutable_set = frozenset([1, 2, 3])
print(f"不可变集合: {immutable_set}")

# 尝试修改会引发错误
try:
    immutable_set.add(4)
except AttributeError as e:
    print(f"错误: {e}")

# 90. 不可变字典
from types import MappingProxyType

mutable_dict = {'a': 1, 'b': 2}
immutable_dict = MappingProxyType(mutable_dict)

print(f"不可变字典: {immutable_dict}")

# 尝试修改会引发错误
try:
    immutable_dict['c'] = 3
except TypeError as e:
    print(f"错误: {e}")

# 原始字典可以修改
mutable_dict['c'] = 3
print(f"原始字典修改后: {immutable_dict}")

# 91. 上下文变量
import contextvars

# 创建上下文变量
user_var = contextvars.ContextVar('user', default='Anonymous')
request_var = contextvars.ContextVar('request', default=None)

def process_request():
    user = user_var.get()
    request = request_var.get()
    print(f"处理请求 - 用户: {user}, 请求: {request}")

# 设置上下文变量
token1 = user_var.set("Alice")
token2 = request_var.set("GET /api/users")

process_request()

# 重置上下文变量
user_var.reset(token1)
request_var.reset(token2)

process_request()

# 92. 类型检查
from typing import get_type_hints

def example_function(x: int, y: str) -> bool:
    return str(x) in y

# 获取类型提示
hints = get_type_hints(example_function)
print(f"类型提示: {hints}")

# 93. 类型别名
from typing import List, Dict, Union

UserId = int
UserName = str
UserInfo = Dict[str, Union[str, int, List[str]]]

def process_user(user_id: UserId, user_name: UserName, user_info: UserInfo) -> None:
    print(f"处理用户: ID={user_id}, 名称={user_name}, 信息={user_info}")

# 94. 泛型类型
from typing import TypeVar, Generic, List

T = TypeVar('T')

class Stack(Generic[T]):
    def __init__(self):
        self._items: List[T] = []
    
    def push(self, item: T) -> None:
        self._items.append(item)
    
    def pop(self) -> T:
        return self._items.pop()
    
    def is_empty(self) -> bool:
        return len(self._items) == 0

# 使用泛型栈
int_stack = Stack[int]()
int_stack.push(1)
int_stack.push(2)
print(f"弹出整数: {int_stack.pop()}")

str_stack = Stack[str]()
str_stack.push("Hello")
str_stack.push("World")
print(f"弹出字符串: {str_stack.pop()}")

# 95. 协议
from typing import Protocol, runtime_checkable

@runtime_checkable
class Drawable(Protocol):
    def draw(self) -> str:
        ...

class Circle:
    def draw(self) -> str:
        return "绘制圆形"

class Square:
    def draw(self) -> str:
        return "绘制正方形"

def render_shape(shape: Drawable) -> None:
    print(shape.draw())

circle = Circle()
square = Square()

render_shape(circle)
render_shape(square)

# 检查协议实现
print(f"Circle是否实现Drawable: {isinstance(circle, Drawable)}")
print(f"Square是否实现Drawable: {isinstance(square, Drawable)}")

# 96. 可调用对象
class CallableExample:
    def __init__(self, multiplier):
        self.multiplier = multiplier
    
    def __call__(self, value):
        return self.multiplier * value

double = CallableExample(2)
triple = CallableExample(3)

print(f"双倍: {double(5)}")
print(f"三倍: {triple(5)}")

# 97. 描述符
class ValidatedAttribute:
    """验证属性描述符"""
    def __init__(self, name, type_, default=None):
        self.name = name
        self.type_ = type_
        self.default = default
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        value = obj.__dict__.get(self.name, self.default)
        if value is None:
            return None
        if not isinstance(value, self.type_):
            raise TypeError(f"{self.name} 必须是 {self.type_}")
        return value
    
    def __set__(self, obj, value):
        if value is not None and not isinstance(value, self.type_):
            raise TypeError(f"{self.name} 必须是 {self.type_}")
        obj.__dict__[self.name] = value

class Person:
    name = ValidatedAttribute("name", str)
    age = ValidatedAttribute("age", int, 0)
    height = ValidatedAttribute("height", float)

person = Person()
person.name = "Alice"
person.age = 30
person.height = 170.5

print(f"姓名: {person.name}, 年龄: {person.age}, 身高: {person.height}")

try:
    person.age = "三十"  # 这将引发TypeError
except TypeError as e:
    print(f"错误: {e}")

# 98. 抽象基类
from abc import ABC, abstractmethod

class Shape(ABC):
    @abstractmethod
    def area(self) -> float:
        pass
    
    @abstractmethod
    def perimeter(self) -> float:
        pass

class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height
    
    def area(self) -> float:
        return self.width * self.height
    
    def perimeter(self) -> float:
        return 2 * (self.width + self.height)

rect = Rectangle(5, 3)
print(f"矩形面积: {rect.area()}")
print(f"矩形周长: {rect.perimeter()}")

# 99. 元类
class Meta(type):
    def __new__(cls, name, bases, dct):
        # 添加类属性
        dct['created_by'] = 'MetaClass'
        # 添加类方法
        def get_class_name(cls):
            return cls.__name__
        dct['get_class_name'] = classmethod(get_class_name)
        return super().__new__(cls, name, bases, dct)

class MyClass(metaclass=Meta):
    pass

print(f"创建者: {MyClass.created_by}")
print(f"类名: {MyClass.get_class_name()}")

# 100. 数据类
from dataclasses import dataclass, field
from typing import List

@dataclass
class Product:
    name: str
    price: float
    quantity: int = 1
    tags: List[str] = field(default_factory=list)
    
    def total_value(self) -> float:
        return self.price * self.quantity

product = Product("Laptop", 999.99, 2, ["electronics", "computer"])
print(f"产品: {product.name}, 总价: {product.total_value()}, 标签: {product.tags}")

# 101. 枚举
from enum import Enum, auto, unique

@unique
class Color(Enum):
    RED = auto()
    GREEN = auto()
    BLUE = auto()

class Status(Enum):
    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"

print(f"颜色: {Color.RED}")
print(f"状态: {Status.PENDING.value}")

# 102. 异步编程
import asyncio

async def say_after(delay, what_to_say):
    await asyncio.sleep(delay)
    print(what_to_say)

async def main():
    task1 = asyncio.create_task(say_after(1, "Hello"))
    task2 = asyncio.create_task(say_after(2, "World"))
    
    await task1
    await task2

# asyncio.run(main())  # 注释掉，避免在测试时运行

# 103. 异步上下文管理器
class AsyncContextManager:
    async def __aenter__(self):
        print("进入异步上下文")
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        print("退出异步上下文")
        return False

async def use_async_context():
    async with AsyncContextManager():
        print("在异步上下文中")

# asyncio.run(use_async_context())  # 注释掉，避免在测试时运行

# 104. 异步迭代器
class AsyncIterator:
    def __init__(self, n):
        self.n = n
        self.i = 0
    
    def __aiter__(self):
        return self
    
    async def __anext__(self):
        if self.i >= self.n:
            raise StopAsyncIteration
        self.i += 1
        await asyncio.sleep(0.1)
        return self.i

async def use_async_iterator():
    async for i in AsyncIterator(5):
        print(f"异步迭代: {i}")

# asyncio.run(use_async_iterator())  # 注释掉，避免在测试时运行

# 105. 异步生成器
async def async_generator(n):
    for i in range(n):
        await asyncio.sleep(0.1)
        yield i

async def use_async_generator():
    async for i in async_generator(5):
        print(f"异步生成器: {i}")

# asyncio.run(use_async_generator())  # 注释掉，避免在测试时运行

# 106. 位置参数和关键字参数
def example_func(a, b, /, c, d, *, e, f):
    print(f"a={a}, b={b}, c={c}, d={d}, e={e}, f={f}")

example_func(1, 2, 3, d=4, e=5, f=6)

# 107. 赋值表达式（海象运算符）
numbers = [1, 2, 3, 4, 5]
while (n := len(numbers)) > 0:
    popped = numbers.pop()
    print(f"弹出: {popped}, 剩余: {n-1}")

# 108. 结构化模式匹配
def match_example(value):
    match value:
        case 0:
            print("零")
        case 1:
            print("一")
        case [x, y]:
            print(f"列表，元素: {x}, {y}")
        case {"name": name, "age": age}:
            print(f"字典，姓名: {name}, 年龄: {age}")
        case _:
            print("其他")

match_example(0)
match_example([1, 2])
match_example({"name": "Bob", "age": 25})
match_example("test")

# 109. 位置参数解包
def add(a, b, c):
    return a + b + c

values = [1, 2, 3]
result = add(*values)
print(f"解包结果: {result}")

# 110. 关键字参数解包
def greet(name, greeting):
    return f"{greeting}, {name}!"

params = {"name": "Alice", "greeting": "Hello"}
result = greet(**params)
print(f"关键字解包结果: {result}")

# 111. 链式比较
x = 10
if 0 < x < 20:
    print("x 在 0 和 20 之间")

# 112. 位运算
a = 60  # 0011 1100
b = 13  # 0000 1101

print(f"a & b = {a & b}")   # 12 (0000 1100)
print(f"a | b = {a | b}")   # 61 (0011 1101)
print(f"a ^ b = {a ^ b}")   # 49 (0011 0001)
print(f"~a = {~a}")         # -61 (1100 0011)
print(f"a << 2 = {a << 2}") # 240 (1111 0000)
print(f"a >> 2 = {a >> 2}") # 15 (0000 1111)

# 113. 运算符重载
class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)
    
    def __mul__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
    
    def __str__(self):
        return f"Vector({self.x}, {self.y})"

v1 = Vector(1, 2)
v2 = Vector(3, 4)

print(f"v1 + v2 = {v1 + v2}")
print(f"v1 - v2 = {v1 - v2}")
print(f"v1 * 3 = {v1 * 3}")
print(f"v1 == v2 = {v1 == v2}")

# 114. 单例模式
class Singleton:
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

s1 = Singleton()
s2 = Singleton()
print(f"s1 是 s2: {s1 is s2}")

# 115. 工厂模式
class Dog:
    def speak(self):
        return "Woof!"

class Cat:
    def speak(self):
        return "Meow!"

def animal_factory(animal_type):
    if animal_type == "dog":
        return Dog()
    elif animal_type == "cat":
        return Cat()
    else:
        raise ValueError(f"未知动物类型: {animal_type}")

dog = animal_factory("dog")
cat = animal_factory("cat")
print(f"狗说: {dog.speak()}")
print(f"猫说: {cat.speak()}")

# 116. 观察者模式
class Subject:
    def __init__(self):
        self._observers = []
        self._state = None
    
    def attach(self, observer):
        if observer not in self._observers:
            self._observers.append(observer)
    
    def detach(self, observer):
        try:
            self._observers.remove(observer)
        except ValueError:
            pass
    
    def notify(self):
        for observer in self._observers:
            observer.update(self)
    
    @property
    def state(self):
        return self._state
    
    @state.setter
    def state(self, value):
        self._state = value
        self.notify()

class Observer:
    def update(self, subject):
        print(f"观察者收到更新: {subject.state}")

subject = Subject()
observer1 = Observer()
observer2 = Observer()

subject.attach(observer1)
subject.attach(observer2)

subject.state = "新状态"

# 117. 策略模式
class Strategy:
    def execute(self, a, b):
        pass

class AddStrategy(Strategy):
    def execute(self, a, b):
        return a + b

class SubtractStrategy(Strategy):
    def execute(self, a, b):
        return a - b

class MultiplyStrategy(Strategy):
    def execute(self, a, b):
        return a * b

class Context:
    def __init__(self, strategy):
        self._strategy = strategy
    
    def set_strategy(self, strategy):
        self._strategy = strategy
    
    def execute_strategy(self, a, b):
        return self._strategy.execute(a, b)

context = Context(AddStrategy())
print(f"加法: {context.execute_strategy(5, 3)}")

context.set_strategy(SubtractStrategy())
print(f"减法: {context.execute_strategy(5, 3)}")

context.set_strategy(MultiplyStrategy())
print(f"乘法: {context.execute_strategy(5, 3)}")

# 118. 装饰器模式
class Coffee:
    def cost(self):
        return 5
    
    def description(self):
        return "基础咖啡"

class MilkDecorator:
    def __init__(self, coffee):
        self._coffee = coffee
    
    def cost(self):
        return self._coffee.cost() + 2
    
    def description(self):
        return self._coffee.description() + ", 牛奶"

class SugarDecorator:
    def __init__(self, coffee):
        self._coffee = coffee
    
    def cost(self):
        return self._coffee.cost() + 1
    
    def description(self):
        return self._coffee.description() + ", 糖"

coffee = Coffee()
print(f"{coffee.description()}: ${coffee.cost()}")

coffee_with_milk = MilkDecorator(coffee)
print(f"{coffee_with_milk.description()}: ${coffee_with_milk.cost()}")

coffee_with_milk_and_sugar = SugarDecorator(coffee_with_milk)
print(f"{coffee_with_milk_and_sugar.description()}: ${coffee_with_milk_and_sugar.cost()}")

# 119. 上下文管理器装饰器
from contextlib import contextmanager

@contextmanager
def managed_resource(resource_name):
    print(f"获取资源: {resource_name}")
    try:
        yield resource_name
    finally:
        print(f"释放资源: {resource_name}")

with managed_resource("数据库连接"):
    print("使用资源")

# 120. 函数注解
def calculate(
    a: float,
    b: float,
    operation: str = "add"
) -> float:
    if operation == "add":
        return a + b
    elif operation == "subtract":
        return a - b
    elif operation == "multiply":
        return a * b
    elif operation == "divide":
        return a / b
    else:
        raise ValueError(f"未知操作: {operation}")

# 121. 类型别名
from typing import List, Dict

Vector = List[float]
Matrix = List[Vector]
UserInfo = Dict[str, Union[str, int, List[int]]]

def dot_product(v1: Vector, v2: Vector) -> float:
    return sum(x * y for x, y in zip(v1, v2))

# 122. 泛型
from typing import TypeVar, Generic

T = TypeVar('T')

class Stack(Generic[T]):
    def __init__(self):
        self._items = []
    
    def push(self, item: T) -> None:
        self._items.append(item)
    
    def pop(self) -> T:
        return self._items.pop()
    
    def is_empty(self) -> bool:
        return len(self._items) == 0

# 使用泛型栈
int_stack = Stack[int]()
int_stack.push(1)
int_stack.push(2)
print(f"弹出整数: {int_stack.pop()}")

str_stack = Stack[str]()
str_stack.push("Hello")
str_stack.push("World")
print(f"弹出字符串: {str_stack.pop()}")

# 123. 协议
from typing import Protocol

class Drawable(Protocol):
    def draw(self) -> str:
        ...

class Circle:
    def draw(self) -> str:
        return "绘制圆形"

class Square:
    def draw(self) -> str:
        return "绘制正方形"

def render_shape(shape: Drawable) -> None:
    print(shape.draw())

circle = Circle()
square = Square()

render_shape(circle)
render_shape(square)

# 124. 可调用对象
class Adder:
    def __init__(self, n):
        self.n = n
    
    def __call__(self, x):
        return self.n + x

add5 = Adder(5)
print(f"5 + 3 = {add5(3)}")

# 125. 描述符协议
class ValidatedAttribute:
    def __init__(self, name, type_, default=None):
        self.name = name
        self.type_ = type_
        self.default = default
    
    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        value = obj.__dict__.get(self.name, self.default)
        if value is None:
            return None
        if not isinstance(value, self.type_):
            raise TypeError(f"{self.name} 必须是 {self.type_}")
        return value
    
    def __set__(self, obj, value):
        if value is not None and not isinstance(value, self.type_):
            raise TypeError(f"{self.name} 必须是 {self.type_}")
        obj.__dict__[self.name] = value

class Person:
    name = ValidatedAttribute("name", str)
    age = ValidatedAttribute("age", int, 0)
    height = ValidatedAttribute("height", float)

person = Person()
person.name = "Alice"
person.age = 30
person.height = 170.5

print(f"姓名: {person.name}, 年龄: {person.age}, 身高: {person.height}")

try:
    person.age = "三十"  # 这将引发TypeError
except TypeError as e:
    print(f"错误: {e}")

# 126. 上下文管理器协议
class Timer:
    def __init__(self, name):
        self.name = name
        self.start = None
        self.end = None
    
    def __enter__(self):
        import time
        self.start = time.time()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        import time
        self.end = time.time()
        elapsed = self.end - self.start
        print(f"{self.name} 耗时: {elapsed:.5f}秒")
        return False  # 不抑制异常

with Timer("代码块"):
    import time
    time.sleep(0.1)

# 127. 迭代器协议
class Fibonacci:
    def __init__(self, max_count):
        self.max_count = max_count
        self.count = 0
        self.a, self.b = 0, 1
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.count >= self.max_count:
            raise StopIteration
        self.count += 1
        result = self.a
        self.a, self.b = self.b, self.a + self.b
        return result

for num in Fibonacci(10):
    print(f"斐波那契数: {num}")

# 128. 可迭代对象协议
class Range:
    def __init__(self, start, end, step=1):
        self.start = start
        self.end = end
        self.step = step
    
    def __iter__(self):
        return RangeIterator(self.start, self.end, self.step)

class RangeIterator:
    def __init__(self, start, end, step):
        self.current = start
        self.end = end
        self.step = step
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.step > 0 and self.current >= self.end:
            raise StopIteration
        if self.step < 0 and self.current <= self.end:
            raise StopIteration
        
        result = self.current
        self.current += self.step
        return result

for i in Range(1, 5):
    print(f"范围数: {i}")

for i in Range(5, 0, -1):
    print(f"倒序范围数: {i}")

# 129. 生成器表达式和yield from
def chain(*iterables):
    for it in iterables:
        yield from it

for item in chain([1, 2], ['a', 'b'], [3, 4]):
    print(f"链式项: {item}")

# 130. 协程和异步生成器
async def async_range(n):
    for i in range(n):
        yield i
        await asyncio.sleep(0.1)

async def async_main():
    async for num in async_range(5):
        print(f"异步数: {num}")

# asyncio.run(async_main())  # 注释掉，避免在测试时运行

# 131. 类型检查
import typing

def check_type(value, expected_type):
    if not isinstance(value, expected_type):
        raise TypeError(f"期望 {expected_type}, 得到 {type(value)}")
    return True

# 使用示例
check_type(42, int)
check_type("hello", str)

try:
    check_type(3.14, int)
except TypeError as e:
    print(f"类型错误: {e}")

# 132. 性能分析
import timeit

def test_performance():
    total = 0
    for i in range(1000):
        total += i
    return total

# 测量执行时间
execution_time = timeit.timeit(test_performance, number=1000)
print(f"执行时间: {execution_time:.5f}秒")

# 133. 内存分析
import sys

def get_size(obj):
    return sys.getsizeof(obj)

print(f"整数大小: {get_size(42)} 字节")
print(f"字符串大小: {get_size('hello')} 字节")
print(f"列表大小: {get_size([1, 2, 3])} 字节")

# 134. 代码对象和动态执行
code_str = """
def dynamic_function(x, y):
    return x + y

result = dynamic_function(10, 20)
print(f"动态执行结果: {result}")
"""

# 编译并执行代码
code_obj = compile(code_str, "<string>", "exec")
exec(code_obj)

# 135. 函数属性
def example_function():
    pass

# 设置函数属性
example_function.author = "Alice"
example_function.version = "1.0"

print(f"函数作者: {example_function.author}")
print(f"函数版本: {example_function.version}")

# 136. 闭包中的变量作用域
def outer_function(x):
    def inner_function(y):
        return x + y
    return inner_function

add5 = outer_function(5)
print(f"闭包结果: {add5(10)}")

# 137. nonlocal和global关键字
x = 10  # 全局变量

def modify_global():
    global x
    x = 20
    print(f"内部修改全局变量: {x}")

def outer():
    x = 5  # 外层函数变量
    
    def inner():
        nonlocal x
        x = 10
        print(f"内部修改外层变量: {x}")
    
    print(f"修改前外层变量: {x}")
    inner()
    print(f"修改后外层变量: {x}")

print(f"全局变量初始值: {x}")
modify_global()
print(f"全局变量修改后: {x}")
outer()

# 138. 元组解包
a, b, c = (1, 2, 3)
print(f"元组解包: a={a}, b={b}, c={c}")

# 交换变量
a, b = b, a
print(f"交换后: a={a}, b={b}")

# 139. 星号解包
first, *middle, last = [1, 2, 3, 4, 5]
print(f"首项: {first}, 中间项: {middle}, 末项: {last}")

# 140. 字典解包
dict1 = {"a": 1, "b": 2}
dict2 = {"c": 3, "d": 4}
merged_dict = {**dict1, **dict2}
print(f"合并字典: {merged_dict}")

# 141. 集合操作
set1 = {1, 2, 3}
set2 = {3, 4, 5}

print(f"并集: {set1 | set2}")
print(f"交集: {set1 & set2}")
print(f"差集: {set1 - set2}")
print(f"对称差集: {set1 ^ set2}")

# 142. 列表方法
numbers = [1, 2, 3, 4, 5]
numbers.append(6)
numbers.insert(0, 0)
numbers.extend([7, 8, 9])
numbers.remove(3)
popped = numbers.pop()
print(f"弹出元素: {popped}")
print(f"列表: {numbers}")
print(f"索引: {numbers.index(4)}")
print(f"计数: {numbers.count(2)}")
numbers.sort()
print(f"排序后: {numbers}")
numbers.reverse()
print(f"反转后: {numbers}")

# 143. 字典方法
student = {"name": "Alice", "age": 25, "courses": ["Math", "Science"]}
print(f"键: {student.keys()}")
print(f"值: {student.values()}")
print(f"键值对: {student.items()}")
print(f"获取姓名: {student.get('name')}")
print(f"获取成绩: {student.get('grades', 'N/A')}")
student.update({"age": 26, "grades": [85, 90, 95]})
print(f"更新后: {student}")
removed = student.pop('courses')
print(f"移除的课程: {removed}")
print(f"最终字典: {student}")

# 144. 字符串方法
text = "  Hello, World!  "
print(f"去除空白: '{text.strip()}'")
print(f"大写: '{text.upper()}'")
print(f"小写: '{text.lower()}'")
print(f"替换: '{text.replace('World', 'Python')}'")
print(f"分割: '{text.split(',')}'")
print(f"连接: {'-'.join(['a', 'b', 'c'])}")
print(f"查找: '{text.find('World')}'")
print(f"计数: '{text.count('l')}'")
print(f"是否以Hello开头: {text.strip().startswith('Hello')}")
print(f"是否以World结尾: {text.strip().endswith('World')}")

# 145. 格式化字符串
name = "Alice"
age = 30
height = 1.65

# f-strings (Python 3.6+)
print(f"姓名: {name}, 年龄: {age}, 身高: {height:.2f}米")

# str.format()
print("姓名: {}, 年龄: {}, 身高: {:.2f}米".format(name, age, height))

# %格式化
print("姓名: %s, 年龄: %d, 身高: %.2f米" % (name, age, height))

# 146. 文件和目录操作
import os

# 获取当前工作目录
print(f"当前工作目录: {os.getcwd()}")

# 列出目录内容
print(f"目录内容: {os.listdir('.')}")

# 创建目录
os.makedirs("test_dir", exist_ok=True)

# 检查路径是否存在
print(f"test_dir是否存在: {os.path.exists('test_dir')}")

# 检查是否是目录
print(f"test_dir是目录: {os.path.isdir('test_dir')}")

# 检查是否是文件
print(f"test_dir是文件: {os.path.isfile('test_dir')}")

# 获取文件大小
with open("test_file.txt", "w") as f:
    f.write("Hello, World!")
print(f"文件大小: {os.path.getsize('test_file.txt')} 字节")

# 删除文件
os.remove("test_file.txt")

# 删除目录
os.rmdir("test_dir")

# 147. 路径操作
from pathlib import Path

# 创建Path对象
current_dir = Path(".")
home_dir = Path.home()

print(f"当前目录: {current_dir.resolve()}")
print(f"家目录: {home_dir}")

# 创建路径
new_dir = current_dir / "example_dir"
new_dir.mkdir(exist_ok=True)

# 创建文件
file_path = new_dir / "example.txt"
file_path.write_text("Hello, Path!")

# 读取文件
print(f"文件内容: {file_path.read_text()}")

# 遍历目录
print("目录遍历:")
for item in new_dir.iterdir():
    print(f"  {item.name}: {'目录' if item.is_dir() else '文件'}")

# 清理
file_path.unlink()
new_dir.rmdir()

# 148. 环境变量
import os

# 获取环境变量
print(f"PATH: {os.environ.get('PATH', '未设置')}")

# 设置环境变量
os.environ['MY_VAR'] = 'my_value'
print(f"MY_VAR: {os.environ.get('MY_VAR')}")

# 删除环境变量
if 'MY_VAR' in os.environ:
    del os.environ['MY_VAR']
print(f"MY_VAR删除后: {os.environ.get('MY_VAR', '未设置')}")

# 149. 命令行参数
import sys

# 获取命令行参数
print(f"脚本名称: {sys.argv[0]}")
print(f"参数列表: {sys.argv[1:]}")

# 150. 命令行参数解析
import argparse

# 创建解析器
parser = argparse.ArgumentParser(description='示例脚本')
parser.add_argument('--name', type=str, default='World', help='姓名')
parser.add_argument('--count', type=int, default=1, help='计数')
parser.add_argument('--verbose', action='store_true', help='详细输出')

# 模拟解析命令行参数
# 在实际使用中，这些参数来自命令行
args = parser.parse_args(['--name', 'Alice', '--count', '3', '--verbose'])

print(f"姓名: {args.name}")
print(f"计数: {args.count}")
print(f"详细输出: {args.verbose}")

print("Python 所有语法特性示例执行完毕！")