class Student:
    def __init__(self, student_id, name, age, gpa):
        self.student_id = student_id
        self.name = name
        self.age = age
        self.gpa = gpa
    
    def display(self):
        print(f"ID: {self.student_id}, Name: {self.name}, Age: {self.age}, GPA: {self.gpa:.2f}")
    
    def is_honor_student(self):
        return self.gpa >= 3.5
    
    def get_grade_level(self):
        if self.gpa >= 3.8:
            return "Excellent"
        elif self.gpa >= 3.5:
            return "Good"
        elif self.gpa >= 3.0:
            return "Average"
        else:
            return "Below Average"

class Course:
    def __init__(self, code, name, credits):
        self.code = code
        self.name = name
        self.credits = credits
    
    def display(self):
        print(f"Course: {self.code} - {self.name} ({self.credits} credits)")

class Rectangle:
    def __init__(self, width, height):
        self.width = width
        self.height = height
    
    def area(self):
        return self.width * self.height
    
    def perimeter(self):
        return 2 * (self.width + self.height)
    
    def display(self):
        print(f"Rectangle: {self.width} x {self.height}")
        print(f"  Area: {self.area():.2f}")
        print(f"  Perimeter: {self.perimeter():.2f}")

class Circle:
    def __init__(self, radius):
        self.radius = radius
        self.pi = 3.14159
    
    def area(self):
        return self.pi * self.radius * self.radius
    
    def circumference(self):
        return 2 * self.pi * self.radius
    
    def display(self):
        print(f"Circle: radius = {self.radius}")
        print(f"  Area: {self.area():.2f}")
        print(f"  Circumference: {self.circumference():.2f}")

def process_students(students):
    print("Processing students:")
    print("-" * 20)
    
    total_gpa = 0.0
    honor_count = 0
    
    for student in students:
        student.display()
        print(f"  Grade Level: {student.get_grade_level()}")
        
        if student.is_honor_student():
            print("  -> Honor Student!")
            honor_count += 1
        
        total_gpa += student.gpa
        print()
    
    if len(students) > 0:
        avg_gpa = total_gpa / len(students)
        print(f"Statistics:")
        print(f"  Total Students: {len(students)}")
        print(f"  Average GPA: {avg_gpa:.2f}")
        print(f"  Honor Students: {honor_count} ({honor_count/len(students)*100:.1f}%)")

def main():
    print("Object-Oriented Programming Demo")
    print("=" * 35)
    
    # Create students
    students = [
        Student(1, "Alice Johnson", 20, 3.8),
        Student(2, "Bob Smith", 21, 3.2),
        Student(3, "Charlie Brown", 19, 3.9),
        Student(4, "Diana Wilson", 22, 2.8),
        Student(5, "Eve Davis", 20, 3.6)
    ]
    
    process_students(students)
    
    # Create courses
    print("\n" + "=" * 35)
    print("Available Courses:")
    print("-" * 20)
    
    courses = [
        Course("CS101", "Introduction to Programming", 3),
        Course("MATH201", "Calculus II", 4),
        Course("ENG102", "English Composition", 3),
        Course("PHYS301", "Physics III", 4)
    ]
    
    total_credits = 0
    for course in courses:
        course.display()
        total_credits += course.credits
    
    print(f"\nTotal Available Credits: {total_credits}")
    
    # Geometry calculations
    print("\n" + "=" * 35)
    print("Geometry Calculations:")
    print("-" * 20)
    
    rectangles = [
        Rectangle(5.0, 3.0),
        Rectangle(7.5, 4.2),
        Rectangle(10.0, 8.0)
    ]
    
    circles = [
        Circle(2.5),
        Circle(4.0),
        Circle(6.0)
    ]
    
    print("Rectangles:")
    for i, rect in enumerate(rectangles):
        print(f"Rectangle {i + 1}:")
        rect.display()
        print()
    
    print("Circles:")
    for i, circle in enumerate(circles):
        print(f"Circle {i + 1}:")
        circle.display()
        print()
    
    # Find best student
    print("=" * 35)
    print("Finding Best Student:")
    print("-" * 20)
    
    best_student = students[0]
    for student in students:
        if student.gpa > best_student.gpa:
            best_student = student
    
    print("Best student:")
    best_student.display()
    print(f"Grade Level: {best_student.get_grade_level()}")
    
    print("\nDemo completed successfully!")

if __name__ == "__main__":
    main()