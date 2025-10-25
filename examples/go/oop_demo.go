package main

import "fmt"

type Student struct {
    ID   int
    Name string
    Age  int
    GPA  float64
}

func (s Student) Display() {
    fmt.Printf("ID: %d, Name: %s, Age: %d, GPA: %.2f\n", s.ID, s.Name, s.Age, s.GPA)
}

func (s Student) IsHonor() bool {
    return s.GPA >= 3.5
}

type Course struct {
    Code    string
    Name    string
    Credits int
}

func (c Course) Display() {
    fmt.Printf("Course: %s - %s (%d credits)\n", c.Code, c.Name, c.Credits)
}

type Rectangle struct {
    Width  float64
    Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
    return 2 * (r.Width + r.Height)
}

func processStudents(students []Student) {
    fmt.Println("Processing students:")
    
    totalGPA := 0.0
    honorCount := 0
    
    for _, student := range students {
        student.Display()
        totalGPA += student.GPA
        
        if student.IsHonor() {
            fmt.Printf("  -> Honor student!\n")
            honorCount++
        }
    }
    
    if len(students) > 0 {
        avgGPA := totalGPA / float64(len(students))
        fmt.Printf("Average GPA: %.2f\n", avgGPA)
        fmt.Printf("Honor students: %d out of %d\n", honorCount, len(students))
    }
}

func main() {
    fmt.Println("Object-Oriented Programming Demo")
    
    // Create students
    students := []Student{
        {ID: 1, Name: "Alice", Age: 20, GPA: 3.8},
        {ID: 2, Name: "Bob", Age: 21, GPA: 3.2},
        {ID: 3, Name: "Charlie", Age: 19, GPA: 3.9},
        {ID: 4, Name: "Diana", Age: 22, GPA: 2.8},
        {ID: 5, Name: "Eve", Age: 20, GPA: 3.6},
    }
    
    processStudents(students)
    
    // Create courses
    fmt.Println("\nAvailable Courses:")
    courses := []Course{
        {Code: "CS101", Name: "Introduction to Programming", Credits: 3},
        {Code: "MATH201", Name: "Calculus II", Credits: 4},
        {Code: "ENG102", Name: "English Composition", Credits: 3},
        {Code: "PHYS301", Name: "Physics III", Credits: 4},
    }
    
    totalCredits := 0
    for _, course := range courses {
        course.Display()
        totalCredits += course.Credits
    }
    fmt.Printf("Total credits: %d\n", totalCredits)
    
    // Rectangle calculations
    fmt.Println("\nGeometry Calculations:")
    rectangles := []Rectangle{
        {Width: 5.0, Height: 3.0},
        {Width: 7.5, Height: 4.2},
        {Width: 10.0, Height: 8.0},
    }
    
    for i, rect := range rectangles {
        fmt.Printf("Rectangle %d: %.1f x %.1f\n", i+1, rect.Width, rect.Height)
        fmt.Printf("  Area: %.2f\n", rect.Area())
        fmt.Printf("  Perimeter: %.2f\n", rect.Perimeter())
    }
    
    // Find best student
    fmt.Println("\nFinding best student:")
    var bestStudent Student
    bestGPA := 0.0
    
    for _, student := range students {
        if student.GPA > bestGPA {
            bestGPA = student.GPA
            bestStudent = student
        }
    }
    
    fmt.Printf("Best student: ")
    bestStudent.Display()
}