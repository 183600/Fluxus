package main

import (
	"fmt"
	"sort"
	"strings"
)

// Student represents a student record
type Student struct {
	ID     int
	Name   string
	Age    int
	Grades []float64
}

// NewStudent creates a new student
func NewStudent(id int, name string, age int) *Student {
	return &Student{
		ID:     id,
		Name:   name,
		Age:    age,
		Grades: make([]float64, 0),
	}
}

// AddGrade adds a grade to the student's record
func (s *Student) AddGrade(grade float64) {
	if grade >= 0 && grade <= 100 {
		s.Grades = append(s.Grades, grade)
	}
}

// GetAverage calculates the average grade
func (s *Student) GetAverage() float64 {
	if len(s.Grades) == 0 {
		return 0
	}
	sum := 0.0
	for _, grade := range s.Grades {
		sum += grade
	}
	return sum / float64(len(s.Grades))
}

// GetLetterGrade returns the letter grade based on average
func (s *Student) GetLetterGrade() string {
	avg := s.GetAverage()
	switch {
	case avg >= 90:
		return "A"
	case avg >= 80:
		return "B"
	case avg >= 70:
		return "C"
	case avg >= 60:
		return "D"
	default:
		return "F"
	}
}

// String returns a string representation of the student
func (s *Student) String() string {
	return fmt.Sprintf("Student{ID: %d, Name: %s, Age: %d, Average: %.2f, Letter Grade: %s}",
		s.ID, s.Name, s.Age, s.GetAverage(), s.GetLetterGrade())
}

// Classroom represents a collection of students
type Classroom struct {
	Students []*Student
}

// NewClassroom creates a new classroom
func NewClassroom() *Classroom {
	return &Classroom{
		Students: make([]*Student, 0),
	}
}

// AddStudent adds a student to the classroom
func (c *Classroom) AddStudent(student *Student) {
	c.Students = append(c.Students, student)
}

// GetClassAverage calculates the class average
func (c *Classroom) GetClassAverage() float64 {
	if len(c.Students) == 0 {
		return 0
	}
	sum := 0.0
	for _, student := range c.Students {
		sum += student.GetAverage()
	}
	return sum / float64(len(c.Students))
}

// GetTopStudents returns the top N students by average grade
func (c *Classroom) GetTopStudents(n int) []*Student {
	if n <= 0 || len(c.Students) == 0 {
		return []*Student{}
	}
	
	// Create a copy and sort by average grade
	students := make([]*Student, len(c.Students))
	copy(students, c.Students)
	
	sort.Slice(students, func(i, j int) bool {
		return students[i].GetAverage() > students[j].GetAverage()
	})
	
	if n > len(students) {
		n = len(students)
	}
	
	return students[:n]
}

// FindStudentByName finds a student by name (case insensitive)
func (c *Classroom) FindStudentByName(name string) *Student {
	lowerName := strings.ToLower(name)
	for _, student := range c.Students {
		if strings.ToLower(student.Name) == lowerName {
			return student
		}
	}
	return nil
}

func main() {
	fmt.Println("=== Student Management System ===")
	
	// Create classroom
	classroom := NewClassroom()
	
	// Create students
	alice := NewStudent(1, "Alice Johnson", 20)
	alice.AddGrade(95)
	alice.AddGrade(87)
	alice.AddGrade(92)
	alice.AddGrade(88)
	
	bob := NewStudent(2, "Bob Smith", 19)
	bob.AddGrade(78)
	bob.AddGrade(82)
	bob.AddGrade(85)
	bob.AddGrade(80)
	
	charlie := NewStudent(3, "Charlie Brown", 21)
	charlie.AddGrade(92)
	charlie.AddGrade(94)
	charlie.AddGrade(89)
	charlie.AddGrade(96)
	
	diana := NewStudent(4, "Diana Wilson", 20)
	diana.AddGrade(88)
	diana.AddGrade(91)
	diana.AddGrade(87)
	diana.AddGrade(93)
	
	// Add students to classroom
	classroom.AddStudent(alice)
	classroom.AddStudent(bob)
	classroom.AddStudent(charlie)
	classroom.AddStudent(diana)
	
	// Display all students
	fmt.Println("\nAll Students:")
	for _, student := range classroom.Students {
		fmt.Println(student)
	}
	
	// Display class average
	fmt.Printf("\nClass Average: %.2f\n", classroom.GetClassAverage())
	
	// Display top 2 students
	fmt.Println("\nTop 2 Students:")
	topStudents := classroom.GetTopStudents(2)
	for i, student := range topStudents {
		fmt.Printf("%d. %s\n", i+1, student)
	}
	
	// Find student by name
	fmt.Println("\nSearching for 'alice':")
	found := classroom.FindStudentByName("alice")
	if found != nil {
		fmt.Println("Found:", found)
	} else {
		fmt.Println("Student not found")
	}
}