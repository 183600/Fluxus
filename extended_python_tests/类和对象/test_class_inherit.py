class Animal:
    def speak(self):
        return 'sound'
class Dog(Animal):
    def speak(self):
        return 'woof'
d = Dog()
print(d.speak())
