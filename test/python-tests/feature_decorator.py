# Decorator tests

def simple_decorator(func):
    def wrapper():
        print("Before function call")
        result = func()
        print("After function call")
        return result
    return wrapper

@simple_decorator
def greet():
    print("Hello from decorated function!")
    return "greeting completed"

result = greet()
print("Result:", result)