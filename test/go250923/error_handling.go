package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"time"
)

type ValidationError struct {
	Field   string
	Message string
}

func (e *ValidationError) Error() string {
	return fmt.Sprintf("validation error on field '%s': %s", e.Field, e.Message)
}

type BusinessError struct {
	Code    string
	Message string
	Cause   error
}

func (e *BusinessError) Error() string {
	if e.Cause != nil {
		return fmt.Sprintf("business error [%s]: %s (caused by: %v)", e.Code, e.Message, e.Cause)
	}
	return fmt.Sprintf("business error [%s]: %s", e.Code, e.Message)
}

func (e *BusinessError) Unwrap() error {
	return e.Cause
}

type UserService struct {
	users map[string]string
}

func NewUserService() *UserService {
	return &UserService{
		users: make(map[string]string),
	}
}

func (s *UserService) CreateUser(username, email string) error {
	if username == "" {
		return &ValidationError{Field: "username", Message: "username cannot be empty"}
	}

	if email == "" {
		return &ValidationError{Field: "email", Message: "email cannot be empty"}
	}

	if !isValidEmail(email) {
		return &ValidationError{Field: "email", Message: "invalid email format"}
	}

	if _, exists := s.users[username]; exists {
		return &BusinessError{Code: "USER_EXISTS", Message: "user already exists"}
	}

	s.users[username] = email
	return nil
}

func (s *UserService) GetUser(username string) (string, error) {
	if username == "" {
		return "", &ValidationError{Field: "username", Message: "username cannot be empty"}
	}

	email, exists := s.users[username]
	if !exists {
		return "", &BusinessError{Code: "USER_NOT_FOUND", Message: "user not found"}
	}

	return email, nil
}

type FileProcessor struct{}

func (fp *FileProcessor) ProcessFile(filename string) error {
	if filename == "" {
		return &ValidationError{Field: "filename", Message: "filename cannot be empty"}
	}

	data, err := ioutil.ReadFile(filename)
	if os.IsNotExist(err) {
		return &BusinessError{Code: "FILE_NOT_FOUND", Message: "file does not exist", Cause: err}
	}

	if err != nil {
		return &BusinessError{Code: "FILE_READ_ERROR", Message: "failed to read file", Cause: err}
	}

	if len(data) == 0 {
		return &BusinessError{Code: "EMPTY_FILE", Message: "file is empty"}
	}

	fmt.Printf("File processed successfully. Size: %d bytes\n", len(data))
	return nil
}

func (fp *FileProcessor) ProcessWithTimeout(filename string, timeout time.Duration) error {
	resultChan := make(chan error, 1)

	go func() {
		resultChan <- fp.ProcessFile(filename)
	}()

	select {
	case err := <-resultChan:
		return err
	case <-time.After(timeout):
		return &BusinessError{Code: "TIMEOUT", Message: "file processing timed out"}
	}
}

type Calculator struct{}

func (c *Calculator) Divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, &BusinessError{Code: "DIVISION_BY_ZERO", Message: "cannot divide by zero"}
	}
	return a / b, nil
}

func (c *Calculator) CalculateDiscount(price, discountPercentage float64) (float64, error) {
	if price <= 0 {
		return 0, &ValidationError{Field: "price", Message: "price must be positive"}
	}

	if discountPercentage < 0 || discountPercentage > 100 {
		return 0, &ValidationError{Field: "discountPercentage", Message: "discount must be between 0 and 100"}
	}

	discountAmount := price * (discountPercentage / 100)
	finalPrice := price - discountAmount

	return finalPrice, nil
}

func main() {
	fmt.Println("=== Error Handling Patterns ===")

	userService := NewUserService()
	fileProcessor := FileProcessor{}
	calculator := Calculator{}

	fmt.Println("\n--- User Service Examples ---")
	err := userService.CreateUser("", "john@example.com")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		handleErrorWithTypeChecking(err)
	}

	err = userService.CreateUser("john", "invalid-email")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		handleErrorWithTypeChecking(err)
	}

	err = userService.CreateUser("john", "john@example.com")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		handleErrorWithTypeChecking(err)
	}

	err = userService.CreateUser("alice", "alice@example.com")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Println("User created successfully")
	}

	fmt.Println("\n--- File Processing Examples ---")
	err = fileProcessor.ProcessFile("nonexistent.txt")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		printErrorChain(err)
	}

	fmt.Println("\n--- Calculator Examples ---")
	result, err := calculator.Divide(10, 2)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("10 / 2 = %.2f\n", result)
	}

	_, err = calculator.Divide(10, 0)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	fmt.Println("\n--- Error Recovery Examples ---")
	recoverFromPanic()
	fmt.Println("Recovered from panic")

	fmt.Println("\n--- Multiple Operations Example ---")
	processMultipleOperations()

	fmt.Println("\n--- Function Composition with Error Handling ---")
	err = processWithRetry(func() error {
		fmt.Println("Attempting operation...")
		return errors.New("temporary failure")
	}, 3)
	fmt.Printf("Retry result: %v\n", err)
}

func handleErrorWithTypeChecking(err error) {
	var validationErr *ValidationError
	var businessErr *BusinessError

	switch {
	case errors.As(err, &validationErr):
		fmt.Printf("  Validation Error - Field: %s, Message: %s\n", validationErr.Field, validationErr.Message)
	case errors.As(err, &businessErr):
		fmt.Printf("  Business Error - Code: %s, Message: %s\n", businessErr.Code, businessErr.Message)
	default:
		fmt.Printf("  Unknown Error: %v\n", err)
	}
}

func printErrorChain(err error) {
	fmt.Printf("Error chain:\n")
	fmt.Printf("  %s\n", err.Error())

	if unwrapped := errors.Unwrap(err); unwrapped != nil {
		fmt.Printf("  └─ Caused by: %v\n", unwrapped)
	}
}

func recoverFromPanic() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("Recovered from panic: %v\n", r)
		}
	}()

	panicSomething()
}

func panicSomething() {
	_, err := strconv.Atoi("not-a-number")
	if err != nil {
		panic(fmt.Sprintf("Failed to convert string: %v", err))
	}
}

func processMultipleOperations() {
	operations := []func() error{
		func() error {
			fmt.Println("Operation 1: Creating user...")
			userService := NewUserService()
			return userService.CreateUser("bob", "bob@example.com")
		},
		func() error {
			fmt.Println("Operation 2: Processing file...")
			fileProcessor := FileProcessor{}
			return fileProcessor.ProcessFile("nonexistent.txt")
		},
		func() error {
			fmt.Println("Operation 3: Calculating...")
			calculator := Calculator{}
			_, err := calculator.Divide(10, 0)
			return err
		},
	}

	for i, operation := range operations {
		fmt.Printf("\nExecuting operation %d:\n", i+1)
		err := operation()
		if err != nil {
			fmt.Printf("  Operation failed: %v\n", err)
		} else {
			fmt.Printf("  Operation succeeded\n")
		}
	}
}

func processWithRetry(operation func() error, maxAttempts int) error {
	var lastErr error

	for attempt := 1; attempt <= maxAttempts; attempt++ {
		err := operation()
		if err == nil {
			return nil
		}

		lastErr = err
		fmt.Printf("  Attempt %d failed: %v\n", attempt, err)

		if attempt < maxAttempts {
			time.Sleep(time.Duration(attempt) * 100 * time.Millisecond)
		}
	}

	return fmt.Errorf("operation failed after %d attempts, last error: %w", maxAttempts, lastErr)
}

func isValidEmail(email string) bool {
	return len(email) > 3 && len(email) < 50 && contains(email, "@")
}

func contains(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}