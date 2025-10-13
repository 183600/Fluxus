# Test scope, closures, and global/nonlocal keywords

# Local vs global scope
print("=== Global and Local Scope ===")
x = "global x"

def test_scope():
    x = "local x"
    print(f"Inside function: {x}")

test_scope()
print(f"Outside function: {x}")

# global keyword
print("\n=== global Keyword ===")
counter = 0

def increment():
    global counter
    counter += 1

print(f"Before: counter = {counter}")
increment()
increment()
print(f"After: counter = {counter}")

# Multiple globals
value1 = 10
value2 = 20

def modify_globals():
    global value1, value2
    value1 = 100
    value2 = 200

print(f"Before: value1={value1}, value2={value2}")
modify_globals()
print(f"After: value1={value1}, value2={value2}")

# nonlocal keyword
print("\n=== nonlocal Keyword ===")
def outer():
    x = "outer x"
    
    def inner():
        nonlocal x
        x = "modified by inner"
        print(f"Inner: {x}")
    
    print(f"Before inner: {x}")
    inner()
    print(f"After inner: {x}")

outer()

# Closures - simple
print("\n=== Simple Closures ===")
def make_multiplier(n):
    def multiplier(x):
        return x * n
    return multiplier

times_two = make_multiplier(2)
times_three = make_multiplier(3)

print(f"times_two(5) = {times_two(5)}")
print(f"times_three(5) = {times_three(5)}")

# Closures - counter
print("\n=== Closure Counter ===")
def make_counter():
    count = 0
    
    def increment():
        nonlocal count
        count += 1
        return count
    
    return increment

counter1 = make_counter()
counter2 = make_counter()

print(f"counter1: {counter1()}, {counter1()}, {counter1()}")
print(f"counter2: {counter2()}, {counter2()}")

# Closures - with multiple functions
print("\n=== Closure with Multiple Functions ===")
def make_account(initial_balance):
    balance = initial_balance
    
    def deposit(amount):
        nonlocal balance
        balance += amount
        return balance
    
    def withdraw(amount):
        nonlocal balance
        if amount <= balance:
            balance -= amount
            return balance
        else:
            return "Insufficient funds"
    
    def get_balance():
        return balance
    
    return deposit, withdraw, get_balance

deposit, withdraw, get_balance = make_account(100)

print(f"Initial balance: {get_balance()}")
print(f"After deposit(50): {deposit(50)}")
print(f"After withdraw(30): {withdraw(30)}")
print(f"Final balance: {get_balance()}")

# LEGB Rule (Local, Enclosing, Global, Built-in)
print("\n=== LEGB Rule ===")
x = "global"

def outer():
    x = "enclosing"
    
    def inner():
        x = "local"
        print(f"Local: {x}")
    
    inner()
    print(f"Enclosing: {x}")

outer()
print(f"Global: {x}")

# Closure with loop (common pitfall)
print("\n=== Closure in Loop (Fixed) ===")
def create_multipliers():
    multipliers = []
    for i in range(5):
        # Using default argument to capture current value
        multipliers.append(lambda x, i=i: x * i)
    return multipliers

mults = create_multipliers()
for i, mult in enumerate(mults):
    print(f"mult[{i}](10) = {mult(10)}")

# Nested closures
print("\n=== Nested Closures ===")
def outer(x):
    def middle(y):
        def inner(z):
            return x + y + z
        return inner
    return middle

result = outer(1)(2)(3)
print(f"outer(1)(2)(3) = {result}")

# Closure with class
print("\n=== Closure vs Class ===")
# Closure version
def make_averager():
    series = []
    
    def averager(new_value):
        series.append(new_value)
        return sum(series) / len(series)
    
    return averager

avg = make_averager()
print(f"avg(10) = {avg(10)}")
print(f"avg(20) = {avg(20)}")
print(f"avg(30) = {avg(30)}")

# Access to outer scope variables
print("\n=== Reading Outer Variables ===")
def outer():
    x = 10
    y = 20
    
    def inner():
        # Can read outer variables without nonlocal
        return x + y
    
    return inner()

result = outer()
print(f"Result: {result}")

# Modifying outer list (mutable)
print("\n=== Modifying Mutable Outer Variables ===")
def outer():
    items = []
    
    def add_item(item):
        # Can modify mutable objects without nonlocal
        items.append(item)
        return items
    
    return add_item

add = outer()
print(f"add('a') = {add('a')}")
print(f"add('b') = {add('b')}")
print(f"add('c') = {add('c')}")

# Closure preserves state
print("\n=== Closure State Preservation ===")
def power_factory(exp):
    def power(base):
        return base ** exp
    return power

square = power_factory(2)
cube = power_factory(3)

print(f"square(4) = {square(4)}")
print(f"cube(4) = {cube(4)}")

print("\n=== All scope and closure tests completed ===")
