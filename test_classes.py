# Test classes
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age
    
    def greet(self):
        return "Hello, I'm " + self.name

person = Person("Alice", 30)
greeting = person.greet()
print(greeting)
print("Name:", person.name)
print("Age:", person.age)