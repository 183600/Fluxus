package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

type Student struct {
	ID     int
	Name   string
	Grades []float64
	Age    int
}

type StudentManager struct {
	students []Student
	nextID   int
}

func NewStudentManager() *StudentManager {
	return &StudentManager{
		students: make([]Student, 0),
		nextID:   1,
	}
}

func (sm *StudentManager) AddStudent(name string, age int) {
	student := Student{
		ID:     sm.nextID,
		Name:   name,
		Age:    age,
		Grades: make([]float64, 0),
	}
	sm.students = append(sm.students, student)
	sm.nextID++
	fmt.Printf("Added student: %s (ID: %d)\n", name, student.ID)
}

func (sm *StudentManager) AddGrade(studentID int, grade float64) error {
	for i := range sm.students {
		if sm.students[i].ID == studentID {
			sm.students[i].Grades = append(sm.students[i].Grades, grade)
			fmt.Printf("Added grade %.2f to student %s\n", grade, sm.students[i].Name)
			return nil
		}
	}
	return fmt.Errorf("student with ID %d not found", studentID)
}

func (sm *StudentManager) GetStudentByID(id int) (*Student, error) {
	for i := range sm.students {
		if sm.students[i].ID == id {
			return &sm.students[i], nil
		}
	}
	return nil, fmt.Errorf("student with ID %d not found", id)
}

func (sm *StudentManager) CalculateAverage(studentID int) (float64, error) {
	student, err := sm.GetStudentByID(studentID)
	if err != nil {
		return 0, err
	}
	
	if len(student.Grades) == 0 {
		return 0, fmt.Errorf("no grades available for student %s", student.Name)
	}
	
	sum := 0.0
	for _, grade := range student.Grades {
		sum += grade
	}
	return sum / float64(len(student.Grades)), nil
}

func (sm *StudentManager) GetTopStudent() (*Student, float64, error) {
	if len(sm.students) == 0 {
		return nil, 0, fmt.Errorf("no students available")
	}
	
	var topStudent *Student
	var highestAverage float64 = -1
	
	for i := range sm.students {
		avg, err := sm.CalculateAverage(sm.students[i].ID)
		if err == nil && avg > highestAverage {
			topStudent = &sm.students[i]
			highestAverage = avg
		}
	}
	
	if topStudent == nil {
		return nil, 0, fmt.Errorf("no student with grades found")
	}
	
	return topStudent, highestAverage, nil
}

func (sm *StudentManager) ListStudents() {
	if len(sm.students) == 0 {
		fmt.Println("No students registered")
		return
	}
	
	fmt.Println("Student List:")
	fmt.Println("=============")
	for _, student := range sm.students {
		avg, err := sm.CalculateAverage(student.ID)
		avgStr := "No grades"
		if err == nil {
			avgStr = fmt.Sprintf("%.2f", avg)
		}
		fmt.Printf("ID: %d | Name: %s | Age: %d | Average: %s\n", 
			student.ID, student.Name, student.Age, avgStr)
	}
}

func (sm *StudentManager) SearchStudents(query string) []Student {
	var results []Student
	query = strings.ToLower(query)
	
	for _, student := range sm.students {
		if strings.Contains(strings.ToLower(student.Name), query) {
			results = append(results, student)
		}
	}
	return results
}

func (sm *StudentManager) SortStudentsByAverage() []Student {
	type StudentWithAvg struct {
		Student Student
		Average float64
	}
	
	var studentsWithAvg []StudentWithAvg
	for _, student := range sm.students {
		avg, err := sm.CalculateAverage(student.ID)
		if err == nil {
			studentsWithAvg = append(studentsWithAvg, StudentWithAvg{
				Student: student,
				Average: avg,
			})
		}
	}
	
	sort.Slice(studentsWithAvg, func(i, j int) bool {
		return studentsWithAvg[i].Average > studentsWithAvg[j].Average
	})
	
	var sorted []Student
	for _, swa := range studentsWithAvg {
		sorted = append(sorted, swa.Student)
	}
	return sorted
}

func main() {
	sm := NewStudentManager()
	
	fmt.Println("Student Management System")
	fmt.Println("========================")
	
	// Add students
	sm.AddStudent("Alice Johnson", 20)
	sm.AddStudent("Bob Smith", 19)
	sm.AddStudent("Charlie Brown", 21)
	sm.AddStudent("Diana Prince", 20)
	sm.AddStudent("Eva Martinez", 22)
	
	// Add grades
	sm.AddGrade(1, 85.5)
	sm.AddGrade(1, 92.0)
	sm.AddGrade(1, 88.5)
	
	sm.AddGrade(2, 78.0)
	sm.AddGrade(2, 82.5)
	sm.AddGrade(2, 85.0)
	
	sm.AddGrade(3, 95.0)
	sm.AddGrade(3, 98.5)
	sm.AddGrade(3, 94.0)
	
	sm.AddGrade(4, 89.0)
	sm.AddGrade(4, 91.5)
	
	sm.AddGrade(5, 76.5)
	sm.AddGrade(5, 79.0)
	sm.AddGrade(5, 82.0)
	
	fmt.Println()
	sm.ListStudents()
	
	// Find top student
	fmt.Println("\nTop Student:")
	topStudent, avg, err := sm.GetTopStudent()
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("%s with average %.2f\n", topStudent.Name, avg)
	}
	
	// Search students
	fmt.Println("\nSearch Results for 'Brown':")
	results := sm.SearchStudents("Brown")
	for _, student := range results {
		fmt.Printf("Found: %s (ID: %d)\n", student.Name, student.ID)
	}
	
	// Sort by average
	fmt.Println("\nStudents sorted by average:")
	sorted := sm.SortStudentsByAverage()
	for i, student := range sorted {
		avg, _ := sm.CalculateAverage(student.ID)
		fmt.Printf("%d. %s - %.2f\n", i+1, student.Name, avg)
	}
}