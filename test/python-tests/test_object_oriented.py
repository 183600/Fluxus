class Animal:
    def __init__(self, name, species):
        self.name = name
        self.species = species

    def speak(self):
        return f"{self.name} makes a sound"

    def __str__(self):
        return f"{self.name} is a {self.species}"

class Dog(Animal):
    def __init__(self, name, breed):
        super().__init__(name, "Dog")
        self.breed = breed

    def speak(self):
        return f"{self.name} barks!"

    def wag_tail(self):
        return f"{self.name} wags tail happily"

class Cat(Animal):
    def __init__(self, name, color):
        super().__init__(name, "Cat")
        self.color = color

    def speak(self):
        return f"{self.name} meows!"

    def purr(self):
        return f"{self.name} purrs contently"

def test_inheritance():
    dog = Dog("Buddy", "Golden Retriever")
    cat = Cat("Whiskers", "Orange")

    assert isinstance(dog, Animal)
    assert isinstance(cat, Animal)
    assert dog.species == "Dog"
    assert cat.species == "Cat"
    assert dog.breed == "Golden Retriever"
    assert cat.color == "Orange"

def test_polymorphism():
    dog = Dog("Buddy", "Golden Retriever")
    cat = Cat("Whiskers", "Orange")

    assert dog.speak() == "Buddy barks!"
    assert cat.speak() == "Whiskers meows!"

    animals = [dog, cat]
    sounds = [animal.speak() for animal in animals]
    assert sounds == ["Buddy barks!", "Whiskers meows!"]

def test_encapsulation():
    class BankAccount:
        def __init__(self, balance=0):
            self._balance = balance

        def deposit(self, amount):
            if amount > 0:
                self._balance += amount
                return True
            return False

        def withdraw(self, amount):
            if 0 < amount <= self._balance:
                self._balance -= amount
                return True
            return False

        def get_balance(self):
            return self._balance

    account = BankAccount(100)
    assert account.get_balance() == 100
    assert account.deposit(50) == True
    assert account.get_balance() == 150
    assert account.withdraw(30) == True
    assert account.get_balance() == 120
    assert account.withdraw(200) == False
    assert account.get_balance() == 120

if __name__ == "__main__":
    test_inheritance()
    test_polymorphism()
    test_encapsulation()
    print("All object-oriented tests passed!")