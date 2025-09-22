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
    result4 = square(5)
    result5 = factorial(5)
    
    return {
        "add": result1,
        "greet": result2,
        "sum_all": result3,
        "square": result4,
        "factorial": result5
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
    
    # 创建实例
    animal = Animal("Generic Animal")
    dog = Dog("Rex", "German Shepherd")
    
    return {
        "animal": animal.speak(),
        "dog": dog.speak()
    }

# 6. 异常处理
def exceptions_demo():
    # 基本异常处理
    try:
        result = 10 / 0
    except ZeroDivisionError:
        result = "除零错误"
    
    return {
        "division_result": result
    }

# 7. 列表推导式和生成器表达式
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
        results["comprehensions"] = comprehensions()
        print("✓ 列表推导式测试通过")
    except Exception as e:
        print(f"✗ 列表推导式测试失败: {e}")
        results["comprehensions"] = None
    
    print("\n所有测试完成!")
    return results

if __name__ == "__main__":
    main()