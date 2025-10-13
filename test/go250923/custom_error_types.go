package main

import (
	"errors"
	"fmt"
	"time"
)

// 基础自定义错误
type ValidationError struct {
	Field   string
	Message string
}

func (e *ValidationError) Error() string {
	return fmt.Sprintf("validation error in field '%s': %s", e.Field, e.Message)
}

func testBasicCustomError() {
	fmt.Println("=== Testing basic custom error ===")
	
	err := &ValidationError{
		Field:   "email",
		Message: "invalid email format",
	}
	fmt.Printf("Error: %v\n", err)
}

// 带错误代码的自定义错误
type APIError struct {
	Code       int
	Message    string
	StatusCode int
	Timestamp  time.Time
}

func (e *APIError) Error() string {
	return fmt.Sprintf("[%d] %s (HTTP %d) at %s", 
		e.Code, e.Message, e.StatusCode, e.Timestamp.Format(time.RFC3339))
}

func testErrorWithCode() {
	fmt.Println("\n=== Testing error with code ===")
	
	err := &APIError{
		Code:       1001,
		Message:    "Resource not found",
		StatusCode: 404,
		Timestamp:  time.Now(),
	}
	fmt.Printf("Error: %v\n", err)
}

// 错误包装
type DatabaseError struct {
	Operation string
	Err       error
}

func (e *DatabaseError) Error() string {
	return fmt.Sprintf("database error during %s: %v", e.Operation, e.Err)
}

func (e *DatabaseError) Unwrap() error {
	return e.Err
}

func testErrorWrapping() {
	fmt.Println("\n=== Testing error wrapping ===")
	
	baseErr := errors.New("connection timeout")
	dbErr := &DatabaseError{
		Operation: "SELECT",
		Err:       baseErr,
	}
	
	fmt.Printf("Wrapped error: %v\n", dbErr)
	fmt.Printf("Unwrapped error: %v\n", errors.Unwrap(dbErr))
	
	// 使用errors.Is检查错误
	if errors.Is(dbErr, baseErr) {
		fmt.Println("Error chain contains base error")
	}
}

// 多层错误包装
func performOperation() error {
	return errors.New("disk full")
}

func saveToDatabase() error {
	err := performOperation()
	if err != nil {
		return fmt.Errorf("database save failed: %w", err)
	}
	return nil
}

func processRequest() error {
	err := saveToDatabase()
	if err != nil {
		return fmt.Errorf("request processing failed: %w", err)
	}
	return nil
}

func testMultiLayerWrapping() {
	fmt.Println("\n=== Testing multi-layer error wrapping ===")
	
	err := processRequest()
	if err != nil {
		fmt.Printf("Top-level error: %v\n", err)
		
		// 解包错误链
		current := err
		depth := 0
		for current != nil {
			fmt.Printf("  Depth %d: %v\n", depth, current)
			current = errors.Unwrap(current)
			depth++
		}
	}
}

// 错误类型检查
type NotFoundError struct {
	Resource string
}

func (e *NotFoundError) Error() string {
	return fmt.Sprintf("resource not found: %s", e.Resource)
}

type PermissionError struct {
	User   string
	Action string
}

func (e *PermissionError) Error() string {
	return fmt.Sprintf("permission denied: user '%s' cannot perform '%s'", e.User, e.Action)
}

func testErrorTypeChecking() {
	fmt.Println("\n=== Testing error type checking ===")
	
	errorList := []error{
		&NotFoundError{Resource: "user:123"},
		&PermissionError{User: "alice", Action: "delete"},
		&ValidationError{Field: "age", Message: "must be positive"},
	}
	
	for i, err := range errorList {
		fmt.Printf("\nError %d: %v\n", i+1, err)
		
		// 使用errors.As进行类型断言
		var notFound *NotFoundError
		if errors.As(err, &notFound) {
			fmt.Printf("  -> Not found error for resource: %s\n", notFound.Resource)
		}
		
		var permErr *PermissionError
		if errors.As(err, &permErr) {
			fmt.Printf("  -> Permission error for user: %s\n", permErr.User)
		}
		
		var valErr *ValidationError
		if errors.As(err, &valErr) {
			fmt.Printf("  -> Validation error in field: %s\n", valErr.Field)
		}
	}
}

// 带重试信息的错误
type RetryableError struct {
	Err        error
	RetryAfter time.Duration
	Attempt    int
}

func (e *RetryableError) Error() string {
	return fmt.Sprintf("retryable error (attempt %d, retry after %v): %v", 
		e.Attempt, e.RetryAfter, e.Err)
}

func (e *RetryableError) Unwrap() error {
	return e.Err
}

func testRetryableError() {
	fmt.Println("\n=== Testing retryable error ===")
	
	baseErr := errors.New("service unavailable")
	retryErr := &RetryableError{
		Err:        baseErr,
		RetryAfter: 5 * time.Second,
		Attempt:    1,
	}
	
	fmt.Printf("Error: %v\n", retryErr)
	fmt.Printf("Should retry after: %v\n", retryErr.RetryAfter)
}

// 聚合错误（多个错误）
type MultiError struct {
	Errors []error
}

func (e *MultiError) Error() string {
	return fmt.Sprintf("multiple errors occurred (%d errors)", len(e.Errors))
}

func (e *MultiError) Add(err error) {
	if err != nil {
		e.Errors = append(e.Errors, err)
	}
}

func (e *MultiError) HasErrors() bool {
	return len(e.Errors) > 0
}

func testMultiError() {
	fmt.Println("\n=== Testing multi-error ===")
	
	multi := &MultiError{}
	
	multi.Add(errors.New("error 1: invalid input"))
	multi.Add(errors.New("error 2: database connection failed"))
	multi.Add(errors.New("error 3: timeout"))
	
	if multi.HasErrors() {
		fmt.Printf("Aggregate error: %v\n", multi)
		fmt.Println("Individual errors:")
		for i, err := range multi.Errors {
			fmt.Printf("  %d. %v\n", i+1, err)
		}
	}
}

// 带上下文的错误
type ContextualError struct {
	Err     error
	Context map[string]interface{}
}

func (e *ContextualError) Error() string {
	return fmt.Sprintf("%v (context: %+v)", e.Err, e.Context)
}

func (e *ContextualError) Unwrap() error {
	return e.Err
}

func testContextualError() {
	fmt.Println("\n=== Testing contextual error ===")
	
	err := &ContextualError{
		Err: errors.New("operation failed"),
		Context: map[string]interface{}{
			"user_id":    123,
			"operation":  "update_profile",
			"timestamp":  time.Now().Unix(),
			"ip_address": "192.168.1.1",
		},
	}
	
	fmt.Printf("Error with context: %v\n", err)
}

// 临时错误接口
type TemporaryError interface {
	error
	Temporary() bool
}

type NetworkError struct {
	Message    string
	IsTemporary bool
}

func (e *NetworkError) Error() string {
	return e.Message
}

func (e *NetworkError) Temporary() bool {
	return e.IsTemporary
}

func testTemporaryError() {
	fmt.Println("\n=== Testing temporary error ===")
	
	errors := []error{
		&NetworkError{Message: "connection timeout", IsTemporary: true},
		&NetworkError{Message: "host not found", IsTemporary: false},
	}
	
	for _, err := range errors {
		fmt.Printf("Error: %v\n", err)
		if tempErr, ok := err.(TemporaryError); ok {
			if tempErr.Temporary() {
				fmt.Println("  -> This is a temporary error, can retry")
			} else {
				fmt.Println("  -> This is a permanent error, do not retry")
			}
		}
	}
}

// 错误链检查
func testErrorChainChecking() {
	fmt.Println("\n=== Testing error chain checking ===")
	
	// 创建错误链
	baseErr := errors.New("base error")
	wrappedErr := fmt.Errorf("wrapped: %w", baseErr)
	doubleWrapped := fmt.Errorf("double wrapped: %w", wrappedErr)
	
	fmt.Printf("Top error: %v\n", doubleWrapped)
	
	// 检查错误链
	if errors.Is(doubleWrapped, baseErr) {
		fmt.Println("Error chain contains base error")
	}
	
	// 自定义错误的Is方法
	targetErr := &ValidationError{Field: "name", Message: "required"}
	wrappedValidation := fmt.Errorf("validation failed: %w", targetErr)
	
	var valErr *ValidationError
	if errors.As(wrappedValidation, &valErr) {
		fmt.Printf("Found validation error in chain: field=%s\n", valErr.Field)
	}
}

func main() {
	testBasicCustomError()
	testErrorWithCode()
	testErrorWrapping()
	testMultiLayerWrapping()
	testErrorTypeChecking()
	testRetryableError()
	testMultiError()
	testContextualError()
	testTemporaryError()
	testErrorChainChecking()
	fmt.Println("\n=== Custom error types tests completed ===")
}
