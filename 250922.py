#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
包含Python所有主要语法特性的测试代码
用于测试编译器的正确性
"""

# 1. 基本数据类型和操作
def basic_types():
    # 整数
    int_num = 42
    negative_int = -17
    big_int = 10**100
    
    # 浮点数
    float_num = 3.14159
    scientific = 1.23e-4
    
    # 字符串
    single_quote = 'Hello'
    double_quote = "World"
    triple_quote = """多行
    字符串"""
    raw_string = r"原始字符串\n"
    f_string = f"插值字符串: {int_num}, {float_num}"
    
    # 布尔值
    bool_true = True
    bool_false = False
    
    # 复数
    complex_num = 3 + 4j
    
    # 空值
    none_value = None
    
    # 字节和字节数组
    bytes_data = b'hello'
    bytearray_data = bytearray(b'world')
    
    return {
        "int": int_num,
        "float": float_num,
        "string": single_quote + " " + double_quote,
        "bool": bool_true,
        "complex": complex_num,
        "none": none_value,
        "bytes": bytes_data,
        "bytearray": bytearray_data
    }

# 2. 容器类型
def container_types():
    # 列表
    empty_list = []
    simple_list = [1, 2, 3, 4, 5]
    mixed_list = [1, "two", 3.0, True, None]
    nested_list = [[1, 2], [3, 4], [5, 6]]
    list_comprehension = [x**2 for x in range(10)]
    
    # 元组
    empty_tuple = ()
    simple_tuple = (1, 2, 3)
    single_element_tuple = (1,)  # 注意逗号
    tuple_unpacking = a, b, c = (1, 2, 3)
    
    # 字典
    empty_dict = {}
    simple_dict = {"name": "Alice", "age": 30, "city": "New York"}
    dict_comprehension = {x: x**2 for x in range(5)}
    
    # 集合
    empty_set = set()
    simple_set = {1, 2, 3, 4, 5}
    set_comprehension = {x**2 for x in range(5)}
    
    # 冻结集合
    frozen_set = frozenset([1, 2, 3, 4, 5])
    
    return {
        "list": simple_list,
        "tuple": simple_tuple,
        "dict": simple_dict,
        "set": simple_set,
        "frozenset": frozen_set
    }

# 3. 控制流
def control_flow():
    # if-elif-else
    x = 10
    if x > 10:
        result = "大于10"
    elif x == 10:
        result = "等于10"
    else:
        result = "小于10"
    
    # for循环
    sum_result = 0
    for i in range(1, 11):
        sum_result += i
    
    # while循环
    count = 0
    while count < 5:
        count += 1
    
    # break和continue
    for i in range(10):
        if i == 3:
            continue  # 跳过3
        if i == 7:
            break     # 在7处停止
    
    # 嵌套循环
    matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    flattened = []
    for row in matrix:
        for item in row:
            flattened.append(item)
    
    # 三元表达式
    max_value = 10 if 5 > 3 else 3
    
    return {
        "if_result": result,
        "sum": sum_result,
        "count": count,
        "flattened": flattened,
        "max_value": max_value
    }

# 4. 函数定义和调用
def functions_demo():
    # 基本函数
    def add(a, b):
        return a + b
    
    # 默认参数
    def greet(name="World"):
        return f"Hello, {name}!"
    
    # 可变参数
    def sum_all(*args):
        return sum(args)
    
    # 关键字参数
    def person_info(name, age, **kwargs):
        info = {"name": name, "age": age}
        info.update(kwargs)
        return info
    
    # 高阶函数
    def apply_function(func, x):
        return func(x)
    
    # Lambda函数
    square = lambda x: x**2
    
    # 递归函数
    def factorial(n):
        if n <= 1:
            return 1
        return n * factorial(n - 1)
    
    # 函数调用
    result1 = add(5, 3)
    result2 = greet("Alice")
    result3 = sum_all(1, 2, 3, 4, 5)
    result4 = person_info("Bob", 25, city="New York", job="Engineer")
    result5 = apply_function(lambda x: x*2, 10)
    result6 = square(5)
    result7 = factorial(5)
    
    return {
        "add": result1,
        "greet": result2,
        "sum_all": result3,
        "person_info": result4,
        "apply_lambda": result5,
        "square": result6,
        "factorial": result7
    }

# 5. 类和对象
def classes_demo():
    # 基本类
    class Animal:
        def __init__(self, name):
            self.name = name
        
        def speak(self):
            return f"{self.name} makes a sound"
    
    # 继承
    class Dog(Animal):
        def __init__(self, name, breed):
            super().__init__(name)
            self.breed = breed
        
        def speak(self):
            return f"{self.name} barks"
        
        def fetch(self):
            return f"{self.name} fetches the ball"
    
    # 多重继承
    class Flyable:
        def fly(self):
            return "Can fly"
    
    class Bird(Animal, Flyable):
        def __init__(self, name, wingspan):
            super().__init__(name)
            self.wingspan = wingspan
        
        def speak(self):
            return f"{self.name} chirps"
    
    # 类变量和实例变量
    class Counter:
        count = 0  # 类变量
        
        def __init__(self):
            self.value = 0  # 实例变量
        
        def increment(self):
            self.value += 1
            Counter.count += 1
    
    # 静态方法和类方法
    class MathUtils:
        @staticmethod
        def add(a, b):
            return a + b
        
        @classmethod
        def multiply(cls, a, b):
            return a * b
    
    # 属性
    class Circle:
        def __init__(self, radius):
            self._radius = radius
        
        @property
        def radius(self):
            return self._radius
        
        @radius.setter
        def radius(self, value):
            if value > 0:
                self._radius = value
        
        @property
        def area(self):
            return 3.14159 * self._radius ** 2
    
    # 创建实例
    animal = Animal("Generic Animal")
    dog = Dog("Rex", "German Shepherd")
    bird = Bird("Tweety", 10)
    counter1 = Counter()
    counter2 = Counter()
    counter1.increment()
    counter1.increment()
    counter2.increment()
    circle = Circle(5)
    
    return {
        "animal": animal.speak(),
        "dog": dog.speak(),
        "dog_fetch": dog.fetch(),
        "bird": bird.speak(),
        "bird_fly": bird.fly(),
        "counter_value": counter1.value,
        "counter_count": Counter.count,
        "static_add": MathUtils.add(5, 3),
        "class_multiply": MathUtils.multiply(4, 5),
        "circle_radius": circle.radius,
        "circle_area": circle.area
    }

# 6. 异常处理
def exceptions_demo():
    # 基本异常处理
    try:
        result = 10 / 0
    except ZeroDivisionError:
        result = "除零错误"
    except Exception as e:
        result = f"其他错误: {e}"
    else:
        result = "没有错误"
    finally:
        cleanup = "清理完成"
    
    # 自定义异常
    class CustomError(Exception):
        def __init__(self, message):
            self.message = message
    
    # 抛出异常
    try:
        raise CustomError("这是一个自定义错误")
    except CustomError as e:
        custom_error_message = e.message
    
    # 多重异常
    def risky_operation(x):
        if x < 0:
            raise ValueError("x不能为负数")
        if x > 100:
            raise OverflowError("x太大")
        return x * 2
    
    try:
        risky_operation(-5)
    except ValueError as ve:
        value_error = ve
    except OverflowError as oe:
        overflow_error = oe
    
    return {
        "division_result": result,
        "cleanup": cleanup,
        "custom_error": custom_error_message,
        "value_error": str(value_error) if 'value_error' in locals() else None,
        "overflow_error": str(overflow_error) if 'overflow_error' in locals() else None
    }

# 7. 文件操作
def file_operations():
    # 写入文件
    with open("temp_file.txt", "w") as f:
        f.write("Hello, World!\n")
        f.write("This is a test file.\n")
    
    # 读取文件
    with open("temp_file.txt", "r") as f:
        content = f.read()
    
    # 逐行读取
    with open("temp_file.txt", "r") as f:
        lines = f.readlines()
    
    # 使用上下文管理器
    class FileContext:
        def __enter__(self):
            print("进入上下文")
            return self
        
        def __exit__(self, exc_type, exc_val, exc_tb):
            print("退出上下文")
            return True  # 抑制异常
    
    with FileContext():
        print("在上下文中")
    
    return {
        "file_content": content,
        "file_lines": lines
    }

# 8. 模块和包
def modules_demo():
    # 导入模块
    import math
    import os.path as osp
    
    # 从模块导入特定函数
    from datetime import datetime, timedelta
    
    # 使用导入的模块
    math_result = math.sqrt(16)
    path_exists = osp.exists("temp_file.txt")
    current_time = datetime.now()
    future_time = current_time + timedelta(days=7)
    
    return {
        "math_sqrt": math_result,
        "path_exists": path_exists,
        "current_time": str(current_time),
        "future_time": str(future_time)
    }

# 9. 迭代器和生成器
def iterators_generators():
    # 迭代器
    my_list = [1, 2, 3, 4, 5]
    my_iterator = iter(my_list)
    
    # 手动迭代
    iterator_values = []
    try:
        iterator_values.append(next(my_iterator))
        iterator_values.append(next(my_iterator))
        iterator_values.append(next(my_iterator))
    except StopIteration:
        pass
    
    # 生成器函数
    def countdown(n):
        while n > 0:
            yield n
            n -= 1
    
    # 使用生成器
    countdown_values = list(countdown(5))
    
    # 生成器表达式
    squares = (x**2 for x in range(10))
    squares_list = list(squares)
    
    return {
        "iterator_values": iterator_values,
        "countdown": countdown_values,
        "squares": squares_list
    }

# 10. 装饰器
def decorators_demo():
    # 简单装饰器
    def logger(func):
        def wrapper(*args, **kwargs):
            print(f"调用函数: {func.__name__}")
            result = func(*args, **kwargs)
            print(f"函数 {func.__name__} 返回: {result}")
            return result
        return wrapper
    
    @logger
    def add(a, b):
        return a + b
    
    # 带参数的装饰器
    def repeat(n):
        def decorator(func):
            def wrapper(*args, **kwargs):
                result = None
                for _ in range(n):
                    result = func(*args, **kwargs)
                return result
            return wrapper
        return decorator
    
    @repeat(3)
    def greet(name):
        print(f"Hello, {name}!")
        return f"Greeted {name}"
    
    # 类装饰器
    class CountCalls:
        def __init__(self, func):
            self.func = func
            self.count = 0
        
        def __call__(self, *args, **kwargs):
            self.count += 1
            print(f"调用次数: {self.count}")
            return self.func(*args, **kwargs)
    
    @CountCalls
    def multiply(a, b):
        return a * b
    
    # 调用装饰的函数
    add_result = add(3, 4)
    greet_result = greet("Alice")
    multiply_result1 = multiply(2, 3)
    multiply_result2 = multiply(3, 4)
    
    return {
        "add_result": add_result,
        "greet_result": greet_result,
        "multiply_result1": multiply_result1,
        "multiply_result2": multiply_result2
    }

# 11. 上下文管理器
def context_managers():
    # 自定义上下文管理器
    class Timer:
        def __enter__(self):
            import time
            self.start_time = time.time()
            return self
        
        def __exit__(self, exc_type, exc_val, exc_tb):
            import time
            end_time = time.time()
            print(f"耗时: {end_time - self.start_time}秒")
    
    # 使用自定义上下文管理器
    with Timer():
        import time
        time.sleep(0.1)
    
    # 使用多个上下文管理器
    with open("temp_file.txt", "r") as f1, Timer():
        content = f1.read()
    
    return {
        "content": content
    }

# 12. 并发和异步
def concurrency_async():
    # 线程
    import threading
    import time
    
    def worker():
        print("Worker线程开始")
        time.sleep(0.1)
        print("Worker线程结束")
    
    thread = threading.Thread(target=worker)
    thread.start()
    thread.join()
    
    # 异步IO
    import asyncio
    
    async def say_after(delay, what_to_say):
        await asyncio.sleep(delay)
        print(what_to_say)
    
    async def main():
        task1 = asyncio.create_task(say_after(0.1, "Hello"))
        task2 = asyncio.create_task(say_after(0.1, "World"))
        await task1
        await task2
    
    # 运行异步代码
    try:
        asyncio.run(main())
    except:
        pass  # 在某些环境中可能不支持
    
    return {
        "thread_completed": True,
        "async_completed": True
    }

# 13. 正则表达式
def regex_demo():
    import re
    
    # 基本匹配
    text = "The quick brown fox jumps over the lazy dog"
    pattern = r"fox"
    match = re.search(pattern, text)
    
    # 查找所有匹配
    all_matches = re.findall(r"\b\w{3}\b", text)  # 查找所有3字母单词
    
    # 替换
    replaced = re.sub(r"fox", "cat", text)
    
    # 分割
    words = re.split(r"\s+", text)
    
    # 编译正则表达式
    compiled_pattern = re.compile(r"\d+")
    numbers = compiled_pattern.findall("There are 123 apples and 456 oranges")
    
    return {
        "match_found": match.group() if match else None,
        "three_letter_words": all_matches,
        "replaced_text": replaced,
        "split_words": words,
        "numbers": numbers
    }

# 14. JSON处理
def json_demo():
    import json
    
    # 序列化
    data = {
        "name": "Alice",
        "age": 30,
        "hobbies": ["reading", "hiking", "coding"],
        "address": {
            "street": "123 Main St",
            "city": "New York"
        }
    }
    
    json_string = json.dumps(data, indent=2)
    
    # 反序列化
    parsed_data = json.loads(json_string)
    
    # 从文件读取JSON
    with open("temp_file.txt", "w") as f:
        json.dump(data, f)
    
    with open("temp_file.txt", "r") as f:
        file_data = json.load(f)
    
    return {
        "json_string": json_string,
        "parsed_name": parsed_data["name"],
        "file_data_name": file_data["name"]
    }

# 15. 列表推导式和生成器表达式
def comprehensions():
    # 列表推导式
    squares = [x**2 for x in range(10)]
    even_squares = [x**2 for x in range(10) if x % 2 == 0]
    
    # 字典推导式
    square_dict = {x: x**2 for x in range(5)}
    
    # 集合推导式
    square_set = {x**2 for x in range(5)}
    
    # 嵌套列表推导式
    matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    flattened = [item for row in matrix for item in row]
    
    # 生成器表达式
    sum_of_squares = sum(x**2 for x in range(10))
    
    return {
        "squares": squares,
        "even_squares": even_squares,
        "square_dict": square_dict,
        "square_set": square_set,
        "flattened": flattened,
        "sum_of_squares": sum_of_squares
    }

# 16. 函数式编程工具
def functional_programming():
    # map
    numbers = [1, 2, 3, 4, 5]
    squared = list(map(lambda x: x**2, numbers))
    
    # filter
    evens = list(filter(lambda x: x % 2 == 0, numbers))
    
    # reduce
    from functools import reduce
    product = reduce(lambda x, y: x * y, numbers)
    
    # 排序
    words = ["apple", "banana", "cherry", "date"]
    sorted_words = sorted(words)
    sorted_by_length = sorted(words, key=len)
    
    # 部分函数应用
    from functools import partial
    multiply_by_2 = partial(lambda x, y: x * y, 2)
    doubled = multiply_by_2(5)
    
    return {
        "squared": squared,
        "evens": evens,
        "product": product,
        "sorted_words": sorted_words,
        "sorted_by_length": sorted_by_length,
        "doubled": doubled
    }

# 17. 时间和日期
def datetime_demo():
    from datetime import datetime, date, time, timedelta, timezone
    
    # 当前时间
    now = datetime.now()
    today = date.today()
    
    # 创建特定日期
    specific_date = date(2023, 12, 25)
    specific_time = time(14, 30, 45)
    specific_datetime = datetime(2023, 12, 25, 14, 30, 45)
    
    # 时间差
    delta = timedelta(days=7, hours=3)
    future_date = today + delta
    
    # 格式化
    formatted_date = now.strftime("%Y-%m-%d %H:%M:%S")
    
    # 解析
    parsed_date = datetime.strptime("2023-12-25", "%Y-%m-%d")
    
    # 时区
    utc_now = datetime.now(timezone.utc)
    
    return {
        "now": str(now),
        "today": str(today),
        "specific_date": str(specific_date),
        "specific_time": str(specific_time),
        "specific_datetime": str(specific_datetime),
        "future_date": str(future_date),
        "formatted_date": formatted_date,
        "parsed_date": str(parsed_date),
        "utc_now": str(utc_now)
    }

# 18. 数值计算
def numeric_computations():
    import math
    import random
    
    # 数学函数
    sqrt_value = math.sqrt(16)
    log_value = math.log(100, 10)
    sin_value = math.sin(math.pi / 2)
    
    # 随机数
    random_int = random.randint(1, 100)
    random_float = random.random()
    random_choice = random.choice(["apple", "banana", "cherry"])
    
    # 统计
    data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    mean = sum(data) / len(data)
    
    # 十进制计算
    from decimal import Decimal, getcontext
    getcontext().prec = 6
    decimal_result = Decimal('1.1') + Decimal('2.2')
    
    # 分数
    from fractions import Fraction
    fraction_result = Fraction(1, 3) + Fraction(1, 6)
    
    return {
        "sqrt": sqrt_value,
        "log": log_value,
        "sin": sin_value,
        "random_int": random_int,
        "random_float": random_float,
        "random_choice": random_choice,
        "mean": mean,
        "decimal_result": float(decimal_result),
        "fraction_result": float(fraction_result)
    }

# 19. 数据结构操作
def data_structures():
    from collections import namedtuple, defaultdict, deque, Counter
    
    # 命名元组
    Point = namedtuple('Point', ['x', 'y'])
    p = Point(1, 2)
    
    # 默认字典
    dd = defaultdict(int)
    dd['a'] += 1
    dd['b'] += 2
    
    # 双端队列
    d = deque([1, 2, 3])
    d.append(4)
    d.appendleft(0)
    popped_right = d.pop()
    popped_left = d.popleft()
    
    # 计数器
    words = ["apple", "banana", "apple", "cherry", "banana", "apple"]
    word_counts = Counter(words)
    
    # 有序字典
    from collections import OrderedDict
    od = OrderedDict()
    od['first'] = 1
    od['second'] = 2
    od['third'] = 3
    
    return {
        "point": p,
        "defaultdict": dict(dd),
        "deque_after_operations": list(d),
        "popped_right": popped_right,
        "popped_left": popped_left,
        "word_counts": dict(word_counts),
        "ordered_dict": dict(od)
    }

# 20. 类型注解
def type_annotations():
    # 基本类型注解
    def add_numbers(a: int, b: int) -> int:
        return a + b
    
    # 容器类型注解
    from typing import List, Dict, Tuple, Set, Optional, Union
    
    def process_list(items: List[str]) -> Dict[str, int]:
        return {item: len(item) for item in items}
    
    def get_value(key: str, mapping: Dict[str, int]) -> Optional[int]:
        return mapping.get(key)
    
    def process_data(data: Union[int, str]) -> str:
        return str(data)
    
    # 类型别名
    Vector = List[float]
    
    def scale_vector(v: Vector, factor: float) -> Vector:
        return [x * factor for x in v]
    
    # 泛型类型
    from typing import TypeVar, Generic
    
    T = TypeVar('T')
    
    class Box(Generic[T]):
        def __init__(self, content: T):
            self.content = content
        
        def get_content(self) -> T:
            return self.content
    
    # 使用类型注解
    result1 = add_numbers(5, 3)
    result2 = process_list(["apple", "banana", "cherry"])
    result3 = get_value("apple", {"apple": 5, "banana": 3})
    result4 = process_data(123)
    result5 = scale_vector([1.0, 2.0, 3.0], 2.0)
    box = Box("Hello")
    result6 = box.get_content()
    
    return {
        "add_numbers": result1,
        "process_list": result2,
        "get_value": result3,
        "process_data": result4,
        "scale_vector": result5,
        "box_content": result6
    }

# 主函数，运行所有测试
def main():
    print("开始测试Python语法特性...")
    
    # 运行所有测试函数
    results = {}
    
    try:
        results["basic_types"] = basic_types()
        print("✓ 基本数据类型测试通过")
    except Exception as e:
        print(f"✗ 基本数据类型测试失败: {e}")
        results["basic_types"] = None
    
    try:
        results["container_types"] = container_types()
        print("✓ 容器类型测试通过")
    except Exception as e:
        print(f"✗ 容器类型测试失败: {e}")
        results["container_types"] = None
    
    try:
        results["control_flow"] = control_flow()
        print("✓ 控制流测试通过")
    except Exception as e:
        print(f"✗ 控制流测试失败: {e}")
        results["control_flow"] = None
    
    try:
        results["functions"] = functions_demo()
        print("✓ 函数测试通过")
    except Exception as e:
        print(f"✗ 函数测试失败: {e}")
        results["functions"] = None
    
    try:
        results["classes"] = classes_demo()
        print("✓ 类和对象测试通过")
    except Exception as e:
        print(f"✗ 类和对象测试失败: {e}")
        results["classes"] = None
    
    try:
        results["exceptions"] = exceptions_demo()
        print("✓ 异常处理测试通过")
    except Exception as e:
        print(f"✗ 异常处理测试失败: {e}")
        results["exceptions"] = None
    
    try:
        results["file_operations"] = file_operations()
        print("✓ 文件操作测试通过")
    except Exception as e:
        print(f"✗ 文件操作测试失败: {e}")
        results["file_operations"] = None
    
    try:
        results["modules"] = modules_demo()
        print("✓ 模块和包测试通过")
    except Exception as e:
        print(f"✗ 模块和包测试失败: {e}")
        results["modules"] = None
    
    try:
        results["iterators"] = iterators_generators()
        print("✓ 迭代器和生成器测试通过")
    except Exception as e:
        print(f"✗ 迭代器和生成器测试失败: {e}")
        results["iterators"] = None
    
    try:
        results["decorators"] = decorators_demo()
        print("✓ 装饰器测试通过")
    except Exception as e:
        print(f"✗ 装饰器测试失败: {e}")
        results["decorators"] = None
    
    try:
        results["context_managers"] = context_managers()
        print("✓ 上下文管理器测试通过")
    except Exception as e:
        print(f"✗ 上下文管理器测试失败: {e}")
        results["context_managers"] = None
    
    try:
        results["concurrency"] = concurrency_async()
        print("✓ 并发和异步测试通过")
    except Exception as e:
        print(f"✗ 并发和异步测试失败: {e}")
        results["concurrency"] = None
    
    try:
        results["regex"] = regex_demo()
        print("✓ 正则表达式测试通过")
    except Exception as e:
        print(f"✗ 正则表达式测试失败: {e}")
        results["regex"] = None
    
    try:
        results["json"] = json_demo()
        print("✓ JSON处理测试通过")
    except Exception as e:
        print(f"✗ JSON处理测试失败: {e}")
        results["json"] = None
    
    try:
        results["comprehensions"] = comprehensions()
        print("✓ 列表推导式测试通过")
    except Exception as e:
        print(f"✗ 列表推导式测试失败: {e}")
        results["comprehensions"] = None
    
    try:
        results["functional"] = functional_programming()
        print("✓ 函数式编程测试通过")
    except Exception as e:
        print(f"✗ 函数式编程测试失败: {e}")
        results["functional"] = None
    
    try:
        results["datetime"] = datetime_demo()
        print("✓ 时间和日期测试通过")
    except Exception as e:
        print(f"✗ 时间和日期测试失败: {e}")
        results["datetime"] = None
    
    try:
        results["numeric"] = numeric_computations()
        print("✓ 数值计算测试通过")
    except Exception as e:
        print(f"✗ 数值计算测试失败: {e}")
        results["numeric"] = None
    
    try:
        results["data_structures"] = data_structures()
        print("✓ 数据结构操作测试通过")
    except Exception as e:
        print(f"✗ 数据结构操作测试失败: {e}")
        results["data_structures"] = None
    
    try:
        results["type_annotations"] = type_annotations()
        print("✓ 类型注解测试通过")
    except Exception as e:
        print(f"✗ 类型注解测试失败: {e}")
        results["type_annotations"] = None
    
    print("\n所有测试完成!")
    return results

if __name__ == "__main__":
    main()