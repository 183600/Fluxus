package main

import (
	"fmt"
	"testing"
)

type Calculator struct{}

func (c *Calculator) Add(a, b int) int {
	return a + b
}

func (c *Calculator) Subtract(a, b int) int {
	return a - b
}

func (c *Calculator) Multiply(a, b int) int {
	return a * b
}

func (c *Calculator) Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

type UserRepository struct {
	users map[int]string
}

func NewUserRepository() *UserRepository {
	return &UserRepository{
		users: make(map[int]string),
	}
}

func (r *UserRepository) AddUser(id int, name string) error {
	if _, exists := r.users[id]; exists {
		return fmt.Errorf("user with ID %d already exists", id)
	}
	r.users[id] = name
	return nil
}

func (r *UserRepository) GetUser(id int) (string, error) {
	name, exists := r.users[id]
	if !exists {
		return "", fmt.Errorf("user with ID %d not found", id)
	}
	return name, nil
}

func main() {
	fmt.Println("Running main test function...")
	testCalculator()
	testUserRepository()

	fmt.Println("To run unit tests, use: go test")
	fmt.Println("Example test functions are provided below...")
}

func testCalculator() {
	fmt.Println("\n--- Testing Calculator ---")
	calc := Calculator{}

	fmt.Printf("2 + 3 = %d\n", calc.Add(2, 3))
	fmt.Printf("5 - 2 = %d\n", calc.Subtract(5, 2))
	fmt.Printf("4 * 3 = %d\n", calc.Multiply(4, 3))

	result, err := calc.Divide(10, 2)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Printf("10 / 2 = %d\n", result)
	}

	_, err = calc.Divide(10, 0)
	if err != nil {
		fmt.Printf("10 / 0 = Error: %v\n", err)
	}
}

func testUserRepository() {
	fmt.Println("\n--- Testing UserRepository ---")
	repo := NewUserRepository()

	err := repo.AddUser(1, "Alice")
	if err != nil {
		fmt.Println("Error:", err)
	}

	err = repo.AddUser(1, "Bob")
	if err != nil {
		fmt.Printf("Expected error when adding duplicate user: %v\n", err)
	}

	name, err := repo.GetUser(1)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Printf("User 1: %s\n", name)
	}

	_, err = repo.GetUser(999)
	if err != nil {
		fmt.Printf("Expected error when getting non-existent user: %v\n", err)
	}
}

func TestCalculatorAdd(t *testing.T) {
	calc := Calculator{}
	result := calc.Add(2, 3)
	expected := 5
	if result != expected {
		t.Errorf("Expected %d, got %d", expected, result)
	}
}

func TestCalculatorDivide(t *testing.T) {
	calc := Calculator{}

	result, err := calc.Divide(10, 2)
	if err != nil {
		t.Errorf("Unexpected error: %v", err)
	}
	if result != 5 {
		t.Errorf("Expected 5, got %d", result)
	}

	_, err = calc.Divide(10, 0)
	if err == nil {
		t.Error("Expected error for division by zero, got nil")
	}
}

func TestUserRepository(t *testing.T) {
	repo := NewUserRepository()

	err := repo.AddUser(1, "Alice")
	if err != nil {
		t.Errorf("Unexpected error adding user: %v", err)
	}

	name, err := repo.GetUser(1)
	if err != nil {
		t.Errorf("Unexpected error getting user: %v", err)
	}
	if name != "Alice" {
		t.Errorf("Expected Alice, got %s", name)
	}

	_, err = repo.GetUser(999)
	if err == nil {
		t.Error("Expected error getting non-existent user, got nil")
	}
}

func BenchmarkCalculatorAdd(b *testing.B) {
	calc := Calculator{}
	for i := 0; i < b.N; i++ {
		calc.Add(i, i+1)
	}
}

func BenchmarkUserRepository(b *testing.B) {
	repo := NewUserRepository()
	for i := 0; i < b.N; i++ {
		repo.AddUser(i, fmt.Sprintf("User%d", i))
		repo.GetUser(i)
	}
}