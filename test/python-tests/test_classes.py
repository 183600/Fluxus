# Test class definitions and object-oriented programming
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def introduce(self):
        return f"My name is {self.name} and I am {self.age} years old."

    def celebrate_birthday(self):
        self.age += 1
        return f"Happy birthday! I'm now {self.age} years old."

class Student(Person):
    def __init__(self, name, age, student_id):
        super().__init__(name, age)
        self.student_id = student_id
        self.grades = []

    def add_grade(self, grade):
        self.grades.append(grade)

    def get_average_grade(self):
        return sum(self.grades) / len(self.grades) if self.grades else 0

    def introduce(self):
        return f"My name is {self.name}, I'm {self.age} years old, and my student ID is {self.student_id}."

# Test classes
person1 = Person("Alice", 25)
student1 = Student("Bob", 20, "S12345")

print(person1.introduce())
print(person1.celebrate_birthday())
print(student1.introduce())

student1.add_grade(90)
student1.add_grade(85)
student1.add_grade(95)
print(f"Student's average grade: {student1.get_average_grade():.2f}")