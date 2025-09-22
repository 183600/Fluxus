def basic_types():
    # 整数
    int_num = 42
    # 浮点数
    float_num = 3.14
    # 字符串
    string_var = "Hello"
    # 布尔值
    bool_var = True
    
    return {
        "int": int_num,
        "float": float_num,
        "string": string_var,
        "bool": bool_var
    }

def control_flow():
    # if语句
    x = 10
    if x > 5:
        result = "大于5"
    else:
        result = "小于等于5"
    
    # for循环
    sum_result = 0
    for i in range(1, 6):
        sum_result += i
    
    return {
        "if_result": result,
        "sum": sum_result
    }

def functions_demo():
    # 基本函数
    def add(a, b):
        return a + b
    
    # 函数调用
    result = add(3, 4)
    
    return {
        "add_result": result
    }

def main():
    print("开始测试...")
    
    try:
        result1 = basic_types()
        print("✓ 基本数据类型测试通过")
    except Exception as e:
        print(f"✗ 基本数据类型测试失败: {e}")
        result1 = None
    
    try:
        result2 = control_flow()
        print("✓ 控制流测试通过")
    except Exception as e:
        print(f"✗ 控制流测试失败: {e}")
        result2 = None
    
    try:
        result3 = functions_demo()
        print("✓ 函数测试通过")
    except Exception as e:
        print(f"✗ 函数测试失败: {e}")
        result3 = None
    
    print("所有测试完成!")
    return {
        "basic_types": result1,
        "control_flow": result2,
        "functions": result3
    }

if __name__ == "__main__":
    main()