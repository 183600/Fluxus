#!/usr/bin/env python3
"""
测试Python的Property装饰器、Classmethod和Staticmethod
"""

import math
from datetime import datetime

class Circle:
    """圆形类 - 演示property装饰器的完整功能"""
    
    def __init__(self, radius):
        self._radius = radius
        self._color = "red"
    
    @property
    def radius(self):
        """半径属性 - 只读"""
        return self._radius
    
    @property
    def diameter(self):
        """直径属性 - 计算属性（只读）"""
        return self._radius * 2
    
    @property
    def area(self):
        """面积属性 - 计算属性（只读）"""
        return math.pi * self._radius ** 2
    
    @property
    def circumference(self):
        """周长属性 - 计算属性（只读）"""
        return 2 * math.pi * self._radius
    
    @property
    def color(self):
        """颜色属性 - 可读写，带验证"""
        return self._color
    
    @color.setter
    def color(self, value):
        """颜色setter - 带验证"""
        valid_colors = ["red", "blue", "green", "yellow", "black", "white"]
        if value.lower() not in valid_colors:
            raise ValueError(f"Invalid color. Must be one of: {valid_colors}")
        self._color = value.lower()
    
    @color.deleter
    def color(self):
        """颜色deleter - 删除属性"""
        print("Deleting color property")
        self._color = None

class Temperature:
    """温度类 - 演示复杂的property模式"""
    
    def __init__(self, celsius=0):
        self._celsius = celsius
    
    @property
    def celsius(self):
        """摄氏度 - 基础属性"""
        return self._celsius
    
    @celsius.setter
    def celsius(self, value):
        """摄氏度setter - 带范围验证"""
        if value < -273.15:  # 绝对零度
            raise ValueError("Temperature cannot be below absolute zero (-273.15°C)")
        self._celsius = value
    
    @property
    def fahrenheit(self):
        """华氏度 - 计算属性"""
        return (self._celsius * 9/5) + 32
    
    @fahrenheit.setter
    def fahrenheit(self, value):
        """华氏度setter - 自动转换"""
        self._celsius = (value - 32) * 5/9
    
    @property
    def kelvin(self):
        """开尔文 - 计算属性"""
        return self._celsius + 273.15
    
    @kelvin.setter
    def kelvin(self, value):
        """开尔文setter - 自动转换"""
        if value < 0:
            raise ValueError("Temperature cannot be below 0K")
        self._celsius = value - 273.15

class BankAccount:
    """银行账户类 - 演示property的业务逻辑"""
    
    _interest_rate = 0.05  # 类变量：利率
    
    def __init__(self, account_number, initial_balance=0):
        self._account_number = account_number
        self._balance = initial_balance
        self._transaction_history = []
        self._created_at = datetime.now()
    
    @property
    def account_number(self):
        """账号 - 只读属性"""
        return self._account_number
    
    @property
    def balance(self):
        """余额 - 只读属性（通过方法修改）"""
        return self._balance
    
    @property
    def formatted_balance(self):
        """格式化余额 - 计算属性"""
        return f"${self._balance:,.2f}"
    
    @property
    def transaction_count(self):
        """交易次数 - 计算属性"""
        return len(self._transaction_history)
    
    @property
    def account_age_days(self):
        """账户年龄（天） - 计算属性"""
        return (datetime.now() - self._created_at).days
    
    def deposit(self, amount):
        """存款"""
        if amount <= 0:
            raise ValueError("Deposit amount must be positive")
        
        self._balance += amount
        self._transaction_history.append({
            'type': 'deposit',
            'amount': amount,
            'timestamp': datetime.now()
        })
        return self._balance
    
    def withdraw(self, amount):
        """取款"""
        if amount <= 0:
            raise ValueError("Withdrawal amount must be positive")
        if amount > self._balance:
            raise ValueError("Insufficient funds")
        
        self._balance -= amount
        self._transaction_history.append({
            'type': 'withdrawal',
            'amount': amount,
            'timestamp': datetime.now()
        })
        return self._balance
    
    @classmethod
    def get_interest_rate(cls):
        """获取当前利率 - 类方法"""
        return cls._interest_rate
    
    @classmethod
    def set_interest_rate(cls, rate):
        """设置利率 - 类方法"""
        if rate < 0:
            raise ValueError("Interest rate cannot be negative")
        cls._interest_rate = rate
    
    @classmethod
    def create_savings_account(cls, account_number, initial_balance=0):
        """创建储蓄账户 - 工厂类方法"""
        account = cls(account_number, initial_balance)
        account.account_type = "savings"
        return account
    
    @classmethod
    def create_checking_account(cls, account_number, initial_balance=0):
        """创建支票账户 - 工厂类方法"""
        account = cls(account_number, initial_balance)
        account.account_type = "checking"
        return account
    
    @staticmethod
    def validate_account_number(account_number):
        """验证账号格式 - 静态方法"""
        return (isinstance(account_number, str) and 
                len(account_number) == 10 and 
                account_number.isdigit())
    
    @staticmethod
    def calculate_compound_interest(principal, rate, time):
        """计算复利 - 静态工具方法"""
        return principal * (1 + rate) ** time
    
    @staticmethod
    def currency_converter(amount, from_currency, to_currency):
        """货币转换器 - 静态工具方法"""
        # 简化的汇率转换
        rates = {
            ('USD', 'CNY'): 7.2,
            ('CNY', 'USD'): 1/7.2,
            ('USD', 'EUR'): 0.85,
            ('EUR', 'USD'): 1/0.85
        }
        
        if from_currency == to_currency:
            return amount
        
        rate = rates.get((from_currency, to_currency))
        if rate is None:
            raise ValueError(f"Conversion rate not available for {from_currency} to {to_currency}")
        
        return amount * rate

class MathUtils:
    """数学工具类 - 演示静态方法的实际应用"""
    
    PI = math.pi
    E = math.e
    
    @staticmethod
    def factorial(n):
        """阶乘计算 - 静态方法"""
        if n < 0:
            raise ValueError("Factorial is not defined for negative numbers")
        if n == 0 or n == 1:
            return 1
        result = 1
        for i in range(2, n + 1):
            result *= i
        return result
    
    @staticmethod
    def is_prime(n):
        """质数判断 - 静态方法"""
        if n < 2:
            return False
        if n == 2:
            return True
        if n % 2 == 0:
            return False
        
        for i in range(3, int(math.sqrt(n)) + 1, 2):
            if n % i == 0:
                return False
        return True
    
    @staticmethod
    def fibonacci(n):
        """斐波那契数列 - 静态方法"""
        if n < 0:
            raise ValueError("Fibonacci is not defined for negative numbers")
        if n == 0:
            return 0
        if n == 1:
            return 1
        
        a, b = 0, 1
        for _ in range(2, n + 1):
            a, b = b, a + b
        return b
    
    @staticmethod
    def gcd(a, b):
        """最大公约数 - 静态方法"""
        while b:
            a, b = b, a % b
        return a
    
    @staticmethod
    def lcm(a, b):
        """最小公倍数 - 静态方法"""
        return abs(a * b) // MathUtils.gcd(a, b)

def test_circle_properties():
    """测试Circle类的property装饰器"""
    print("=== Testing Circle Properties ===")
    
    circle = Circle(5)
    print(f"Circle with radius {circle.radius}")
    print(f"Diameter: {circle.diameter}")
    print(f"Area: {circle.area:.2f}")
    print(f"Circumference: {circle.circumference:.2f}")
    print(f"Color: {circle.color}")
    
    # 测试颜色setter
    circle.color = "blue"
    print(f"Color after change: {circle.color}")
    
    # 测试无效颜色
    try:
        circle.color = "purple"
        print("ERROR: Should not accept invalid color")
    except ValueError as e:
        print(f"✓ Correctly rejected invalid color: {e}")
    
    # 测试颜色deleter
    del circle.color
    print(f"Color after deletion: {circle.color}")

def test_temperature_properties():
    """测试Temperature类的复杂property模式"""
    print("\n=== Testing Temperature Properties ===")
    
    temp = Temperature(25)  # 25°C
    print(f"Temperature: {temp.celsius}°C")
    print(f"Fahrenheit: {temp.fahrenheit:.1f}°F")
    print(f"Kelvin: {temp.kelvin:.1f}K")
    
    # 设置华氏度，自动转换
    temp.fahrenheit = 77
    print(f"\nAfter setting 77°F:")
    print(f"Celsius: {temp.celsius:.1f}°C")
    print(f"Fahrenheit: {temp.fahrenheit:.1f}°F")
    print(f"Kelvin: {temp.kelvin:.1f}K")
    
    # 设置开尔文，自动转换
    temp.kelvin = 300
    print(f"\nAfter setting 300K:")
    print(f"Celsius: {temp.celsius:.1f}°C")
    print(f"Fahrenheit: {temp.fahrenheit:.1f}°F")
    print(f"Kelvin: {temp.kelvin:.1f}K")
    
    # 测试范围验证
    try:
        temp.celsius = -300  # 低于绝对零度
        print("ERROR: Should not accept temperature below absolute zero")
    except ValueError as e:
        print(f"✓ Correctly rejected invalid temperature: {e}")

def test_bank_account_classmethods():
    """测试BankAccount的类方法"""
    print("\n=== Testing BankAccount Class Methods ===")
    
    # 使用工厂方法创建账户
    savings = BankAccount.create_savings_account("1234567890", 1000)
    checking = BankAccount.create_checking_account("0987654321", 500)
    
    print(f"Savings account: {savings.account_number}, Balance: {savings.formatted_balance}")
    print(f"Checking account: {checking.account_number}, Balance: {checking.formatted_balance}")
    
    # 测试类方法获取和设置利率
    print(f"Current interest rate: {BankAccount.get_interest_rate():.2%}")
    
    BankAccount.set_interest_rate(0.06)
    print(f"New interest rate: {BankAccount.get_interest_rate():.2%}")
    
    # 测试静态方法
    print(f"Is valid account number? {BankAccount.validate_account_number('1234567890')}")
    print(f"Is valid account number? {BankAccount.validate_account_number('12345')}")
    
    # 测试静态工具方法
    compound_interest = BankAccount.calculate_compound_interest(1000, 0.05, 5)
    print(f"Compound interest (5 years): ${compound_interest:.2f}")
    
    converted = BankAccount.currency_converter(100, 'USD', 'CNY')
    print(f"100 USD to CNY: {converted:.2f}")

def test_math_utils_staticmethods():
    """测试MathUtils的静态方法"""
    print("\n=== Testing MathUtils Static Methods ===")
    
    # 测试阶乘
    print(f"Factorial of 5: {MathUtils.factorial(5)}")
    print(f"Factorial of 0: {MathUtils.factorial(0)}")
    
    # 测试质数判断
    primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    for num in primes:
        print(f"Is {num} prime? {MathUtils.is_prime(num)}")
    
    # 测试斐波那契数列
    fib_sequence = [MathUtils.fibonacci(i) for i in range(10)]
    print(f"First 10 Fibonacci numbers: {fib_sequence}")
    
    # 测试最大公约数和最小公倍数
    print(f"GCD of 48 and 18: {MathUtils.gcd(48, 18)}")
    print(f"LCM of 12 and 18: {MathUtils.lcm(12, 18)}")
    
    # 测试类常量
    print(f"MathUtils.PI: {MathUtils.PI}")
    print(f"MathUtils.E: {MathUtils.E}")

def test_property_edge_cases():
    """测试property的边缘情况"""
    print("\n=== Testing Property Edge Cases ===")
    
    # 测试属性删除
    circle = Circle(3)
    print(f"Original color: {circle.color}")
    
    # 删除属性
    del circle.color
    print(f"Color after deletion: {circle.color}")
    
    # 重新设置属性
    circle.color = "green"
    print(f"Color after reset: {circle.color}")

def test_inheritance_with_properties():
    """测试带property的继承"""
    print("\n=== Testing Inheritance with Properties ===")
    
    class ShapeWithArea:
        """基础形状类"""
        
        def __init__(self, name):
            self._name = name
        
        @property
        def name(self):
            return self._name
        
        @property
        def area(self):
            raise NotImplementedError("Subclasses must implement area property")
    
    class Square(ShapeWithArea):
        """正方形"""
        
        def __init__(self, side_length):
            super().__init__("Square")
            self._side_length = side_length
        
        @property
        def side_length(self):
            return self._side_length
        
        @property
        def area(self):
            return self._side_length ** 2
    
    class Triangle(ShapeWithArea):
        """三角形"""
        
        def __init__(self, base, height):
            super().__init__("Triangle")
            self._base = base
            self._height = height
        
        @property
        def area(self):
            return 0.5 * self._base * self._height
    
    shapes = [Square(4), Triangle(6, 3)]
    
    for shape in shapes:
        print(f"{shape.name} area: {shape.area}")

def main():
    """主测试函数"""
    print("Python Property Decorators, Classmethod and Staticmethod Demonstration")
    print("=" * 80)
    
    test_circle_properties()
    test_temperature_properties()
    test_bank_account_classmethods()
    test_math_utils_staticmethods()
    test_property_edge_cases()
    test_inheritance_with_properties()
    
    print("\n=== Summary ===")
    print("Key concepts demonstrated:")
    print("1. Property decorators with getter, setter, and deleter")
    print("2. Calculated properties (read-only)")
    print("3. Properties with validation")
    print("4. Class methods for factory patterns and class-level operations")
    print("5. Static methods for utility functions")
    print("6. Inheritance with properties")
    print("\nProperty vs Classmethod vs Staticmethod:")
    print("- Property: Instance-level computed attributes with optional validation")
    print("- Classmethod: Operations that affect the class itself or create instances")
    print("- Staticmethod: Utility functions that don't need instance or class state")

if __name__ == "__main__":
    main()