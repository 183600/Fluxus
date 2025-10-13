class MyClass:
    def __init__(self):
        self.items = []
    
    def add(self, x):
        self.items.append(x)

obj = MyClass()
obj.add(1)
obj.add(2)
print(len(obj.items))
