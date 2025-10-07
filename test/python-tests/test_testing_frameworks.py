#!/usr/bin/env python3
"""
测试Python的测试框架：unittest和pytest模式
"""

import unittest
from unittest.mock import Mock, patch, MagicMock
import time
import random
import statistics

# 示例函数和类用于测试
def calculate_area(radius):
    """计算圆的面积"""
    if radius < 0:
        raise ValueError("Radius cannot be negative")
    return 3.14159 * radius * radius

def fetch_data_from_api(url, timeout=5):
    """模拟API调用"""
    time.sleep(1)  # 模拟网络延迟
    if "error" in url:
        raise ConnectionError("API connection failed")
    return {"data": "sample data", "status": "success"}

class Calculator:
    """计算器类"""
    
    def add(self, a, b):
        return a + b
    
    def multiply(self, a, b):
        return a * b
    
    def divide(self, a, b):
        if b == 0:
            raise ZeroDivisionError("Cannot divide by zero")
        return a / b
    
    def get_random_number(self):
        """获取随机数（用于mock测试）"""
        return random.randint(1, 100)

def demonstrate_unittest_basics():
    """演示unittest基础用法"""
    print("=== Demonstrating unittest basics ===")
    
    # 简单的测试用例
    class TestBasicMath(unittest.TestCase):
        
        def setUp(self):
            """每个测试方法前的准备"""
            self.calc = Calculator()
            print(f"Setting up test: {self._testMethodName}")
        
        def tearDown(self):
            """每个测试方法后的清理"""
            print(f"Tearing down test: {self._testMethodName}")
        
        @classmethod
        def setUpClass(cls):
            """整个测试类前的准备"""
            print("Setting up test class")
        
        @classmethod
        def tearDownClass(cls):
            """整个测试类后的清理"""
            print("Tearing down test class")
        
        def test_addition(self):
            """测试加法"""
            result = self.calc.add(2, 3)
            self.assertEqual(result, 5)
        
        def test_multiplication(self):
            """测试乘法"""
            result = self.calc.multiply(4, 5)
            self.assertEqual(result, 20)
        
        def test_division(self):
            """测试除法"""
            result = self.calc.divide(10, 2)
            self.assertEqual(result, 5.0)
        
        def test_division_by_zero(self):
            """测试除零错误"""
            with self.assertRaises(ZeroDivisionError):
                self.calc.divide(10, 0)
        
        def test_string_assertions(self):
            """测试字符串断言"""
            text = "Hello World"
            self.assertIn("Hello", text)
            self.assertNotIn("Python", text)
            self.assertTrue(text.startswith("Hello"))
            self.assertFalse(text.islower())
    
    # 运行测试
    suite = unittest.TestLoader().loadTestsFromTestCase(TestBasicMath)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    return result.wasSuccessful()

def demonstrate_mocking():
    """演示mock技术"""
    print("\n=== Demonstrating mocking techniques ===")
    
    class TestWithMocking(unittest.TestCase):
        
        def test_mock_function_return_value(self):
            """测试mock函数返回值"""
            # 创建mock对象
            mock_calc = Mock()
            mock_calc.add.return_value = 999
            
            result = mock_calc.add(1, 2)
            self.assertEqual(result, 999)
            
            # 验证调用
            mock_calc.add.assert_called_once_with(1, 2)
        
        def test_mock_side_effect(self):
            """测试mock副作用"""
            mock_function = Mock()
            mock_function.side_effect = [1, 2, 3, Exception("Stop")]
            
            self.assertEqual(mock_function(), 1)
            self.assertEqual(mock_function(), 2)
            self.assertEqual(mock_function(), 3)
            self.assertRaises(Exception, mock_function)
        
        @patch('random.randint')
        def test_mock_random_number(self, mock_randint):
            """测试mock随机数生成"""
            mock_randint.return_value = 42
            
            calc = Calculator()
            result = calc.get_random_number()
            
            self.assertEqual(result, 42)
            mock_randint.assert_called_once_with(1, 100)
        
        @patch('__main__.time.sleep')  # 注意：这里应该根据实际模块名修改
        def test_mock_time_sleep(self, mock_sleep):
            """测试mock时间函数"""
            # 这将立即返回，不会实际等待
            time.sleep(100)
            mock_sleep.assert_called_once_with(100)
        
        def test_mock_api_call(self):
            """测试mock API调用"""
            with patch('__main__.fetch_data_from_api') as mock_fetch:
                mock_fetch.return_value = {'mocked': 'data'}
                
                result = fetch_data_from_api('http://example.com')
                
                self.assertEqual(result, {'mocked': 'data'})
                mock_fetch.assert_called_once_with('http://example.com')
    
    # 运行mock测试
    suite = unittest.TestLoader().loadTestsFromTestCase(TestWithMocking)
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    return result.wasSuccessful()

def demonstrate_parameterized_testing():
    """演示参数化测试"""
    print("\n=== Demonstrating parameterized testing ===")
    
    class TestParameterized(unittest.TestCase):
        
        def test_calculate_area_with_parameters(self):
            """测试圆面积计算（手动参数化）"""
            test_cases = [
                (0, 0),
                (1, 3.14159),
                (2, 12.56636),
                (3, 28.27431),
                (5, 78.53975),
            ]
            
            for radius, expected in test_cases:
                with self.subTest(radius=radius, expected=expected):
                    result = calculate_area(radius)
                    self.assertAlmostEqual(result, expected, places=5)
        
        def test_calculate_area_negative_radius(self):
            """测试负数半径"""
            with self.assertRaises(ValueError):
                calculate_area(-1)

def demonstrate_fixtures():
    """演示测试固件"""
    print("\n=== Demonstrating test fixtures ===")
    
    class TestWithFixtures(unittest.TestCase):
        
        def setUp(self):
            """设置测试数据"""
            self.sample_data = [1, 2, 3, 4, 5]
            self.empty_list = []
            self.calc = Calculator()
        
        def test_list_average(self):
            """测试列表平均值"""
            avg = statistics.mean(self.sample_data)
            self.assertEqual(avg, 3.0)
        
        def test_empty_list_average(self):
            """测试空列表平均值"""
            self.assertRaises(statistics.StatisticsError, statistics.mean, self.empty_list)
        
        def test_calculator_with_fixture(self):
            """使用固件测试计算器"""
            result = self.calc.add(self.sample_data[0], self.sample_data[1])
            self.assertEqual(result, 3)

def demonstrate_performance_testing():
    """演示性能测试"""
    print("\n=== Demonstrating performance testing ===")
    
    class TestPerformance(unittest.TestCase):
        
        def test_function_execution_time(self):
            """测试函数执行时间"""
            start_time = time.time()
            
            # 执行一些操作
            result = sum(range(1000000))
            
            end_time = time.time()
            execution_time = end_time - start_time
            
            print(f"Function execution time: {execution_time:.4f} seconds")
            self.assertLess(execution_time, 1.0)  # 应该在1秒内完成
            self.assertEqual(result, 499999500000)
        
        def test_memory_efficiency(self):
            """测试内存使用效率"""
            # 创建大列表
            large_list = list(range(100000))
            
            # 测试列表操作
            result = [x * 2 for x in large_list[:1000]]  # 只处理前1000个
            
            self.assertEqual(len(result), 1000)
            self.assertEqual(result[0], 0)
            self.assertEqual(result[-1], 1998)

def demonstrate_test_organization():
    """演示测试组织结构"""
    print("\n=== Demonstrating test organization ===")
    
    # 模拟不同的测试模块
    class TestMathOperations(unittest.TestCase):
        """数学运算测试组"""
        
        def test_basic_arithmetic(self):
            self.assertEqual(2 + 2, 4)
            self.assertEqual(10 - 3, 7)
            self.assertEqual(4 * 5, 20)
        
        def test_division_operations(self):
            self.assertEqual(10 / 2, 5.0)
            self.assertEqual(10 // 3, 3)
    
    class TestStringOperations(unittest.TestCase):
        """字符串操作测试组"""
        
        def test_string_concatenation(self):
            self.assertEqual("Hello" + " " + "World", "Hello World")
        
        def test_string_methods(self):
            self.assertEqual("hello".upper(), "HELLO")
            self.assertTrue("Python".startswith("Py"))
    
    # 运行所有测试
    test_classes = [TestMathOperations, TestStringOperations]
    
    for test_class in test_classes:
        print(f"\nRunning {test_class.__name__}:")
        suite = unittest.TestLoader().loadTestsFromTestCase(test_class)
        runner = unittest.TextTestRunner(verbosity=1)
        result = runner.run(suite)
        
        print(f"Tests run: {result.testsRun}")
        print(f"Failures: {len(result.failures)}")
        print(f"Errors: {len(result.errors)}")

def demonstrate_pytest_style():
    """演示pytest风格的测试（概念性）"""
    print("\n=== Demonstrating pytest-style testing concepts ===")
    
    # 以下是pytest风格的测试示例（概念性，需要实际安装pytest才能运行）
    
    print("""
PyTest style examples (conceptual):

# test_calculator.py
import pytest
from calculator import Calculator

@pytest.fixture
def calculator():
    return Calculator()

@pytest.mark.parametrize("a,b,expected", [
    (2, 3, 5),
    (10, 5, 15),
    (-1, 1, 0),
    (0, 0, 0),
])
def test_add(calculator, a, b, expected):
    assert calculator.add(a, b) == expected

@pytest.mark.slow
def test_performance():
    # 性能测试
    result = sum(range(1000000))
    assert result == 499999500000

# 运行测试
# pytest -v                    # 详细输出
# pytest -k "test_add"         # 运行特定测试
# pytest -m "not slow"         # 运行非慢速测试
# pytest --cov=myproject       # 代码覆盖率
""")

if __name__ == "__main__":
    print("Python Testing Frameworks Demonstration")
    print("=" * 50)
    
    # 运行所有演示
    success1 = demonstrate_unittest_basics()
    success2 = demonstrate_mocking()
    demonstrate_parameterized_testing()
    demonstrate_fixtures()
    demonstrate_performance_testing()
    demonstrate_test_organization()
    demonstrate_pytest_style()
    
    print(f"\n=== Testing demonstration completed ===")
    print(f"Basic tests passed: {success1}")
    print(f"Mock tests passed: {success2}")
    
    # 实际运行unittest主程序
    print("\nRunning full test suite...")
    unittest.main(verbosity=2, exit=False)