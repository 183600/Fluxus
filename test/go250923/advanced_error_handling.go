package main

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"
)

// Custom error types
type ValidationError struct {
	Field   string
	Value   interface{}
	Message string
}

func (e ValidationError) Error() string {
	return fmt.Sprintf("validation error in field '%s' with value '%v': %s", e.Field, e.Value, e.Message)
}

type NotFoundError struct {
	Resource string
	ID       string
}

func (e NotFoundError) Error() string {
	return fmt.Sprintf("%s with ID '%s' not found", e.Resource, e.ID)
}

type DatabaseError struct {
	Operation string
	Table     string
	Cause     error
}

func (e DatabaseError) Error() string {
	if e.Cause != nil {
		return fmt.Sprintf("database error during %s on table '%s': %v", e.Operation, e.Table, e.Cause)
	}
	return fmt.Sprintf("database error during %s on table '%s'", e.Operation, e.Table)
}

func (e DatabaseError) Unwrap() error {
	return e.Cause
}

// Error wrapping and chaining
type ErrorChain struct {
	message string
	cause   error
	context map[string]interface{}
}

func NewErrorChain(message string) *ErrorChain {
	return &ErrorChain{
		message: message,
		context: make(map[string]interface{}),
	}
}

func (e *ErrorChain) WithCause(cause error) *ErrorChain {
	e.cause = cause
	return e
}

func (e *ErrorChain) WithContext(key string, value interface{}) *ErrorChain {
	e.context[key] = value
	return e
}

func (e *ErrorChain) Error() string {
	result := e.message
	if e.cause != nil {
		result += fmt.Sprintf(" caused by: %v", e.cause)
	}
	if len(e.context) > 0 {
		result += fmt.Sprintf(" context: %v", e.context)
	}
	return result
}

func (e *ErrorChain) Unwrap() error {
	return e.cause
}

// MultiError for aggregating multiple errors
type MultiError struct {
	errors []error
}

func (me *MultiError) Add(err error) {
	if err != nil {
		me.errors = append(me.errors, err)
	}
}

func (me *MultiError) Error() string {
	if len(me.errors) == 0 {
		return "no errors"
	}
	
	result := fmt.Sprintf("%d errors occurred:\n", len(me.errors))
	for i, err := range me.errors {
		result += fmt.Sprintf("  %d: %v\n", i+1, err)
	}
	return result
}

func (me *MultiError) HasErrors() bool {
	return len(me.errors) > 0
}

// Retry pattern function
func retryOperation(attempts int, fn func() error) error {
	var err error
	for i := 0; i < attempts; i++ {
		err = fn()
		if err == nil {
			return nil
		}
		fmt.Printf("Attempt %d failed: %v\n", i+1, err)
		if i < attempts-1 {
			time.Sleep(time.Duration(i+1) * 100 * time.Millisecond)
		}
	}
	return fmt.Errorf("failed after %d attempts: %w", attempts, err)
}

// Parallel error collection function
func collectErrors(tasks []func() error) []error {
	var mu sync.Mutex
	var errors []error
	var wg sync.WaitGroup
	
	for _, task := range tasks {
		wg.Add(1)
		go func(t func() error) {
			defer wg.Done()
			if err := t(); err != nil {
				mu.Lock()
				errors = append(errors, err)
				mu.Unlock()
			}
		}(task)
	}
	
	wg.Wait()
	return errors
}

func main() {
	fmt.Println("=== Go Advanced Error Handling and Patterns ===")
	testCustomErrorTypes()
	testErrorWrapping()
	testErrorChaining()
	testErrorHandlingPatterns()
	testPanicRecoveryPatterns()
	testErrorAggregation()
	testErrorClassification()
	fmt.Println("\n=== All advanced error handling tests completed successfully! ===")
}

func testCustomErrorTypes() {
	fmt.Println("\n--- Custom Error Types ---")
	
	// Validation error
	valErr := ValidationError{
		Field:   "email",
		Value:   "invalid-email",
		Message: "must be a valid email address",
	}
	fmt.Printf("Validation error: %v\n", valErr)
	
	// Not found error
	notFoundErr := NotFoundError{
		Resource: "user",
		ID:       "12345",
	}
	fmt.Printf("Not found error: %v\n", notFoundErr)
	
	// Database error with cause
	dbErr := DatabaseError{
		Operation: "SELECT",
		Table:     "users",
		Cause:     fmt.Errorf("connection timeout"),
	}
	fmt.Printf("Database error: %v\n", dbErr)
	
	// Error unwrapping
	if cause := dbErr.Unwrap(); cause != nil {
		fmt.Printf("Database error cause: %v\n", cause)
	}
}

func testErrorWrapping() {
	fmt.Println("\n--- Error Wrapping ---")
	
	// Traditional error wrapping
	originalErr := fmt.Errorf("database connection failed")
	wrappedErr := fmt.Errorf("failed to fetch user: %w", originalErr)
	
	fmt.Printf("Wrapped error: %v\n", wrappedErr)
	
	// Unwrapping with errors.Is and errors.As
	if errors.Is(wrappedErr, originalErr) {
		fmt.Println("Successfully detected original error using errors.Is")
	}
	
	var dbErr *DatabaseError
	if errors.As(wrappedErr, &dbErr) {
		fmt.Printf("Error is a DatabaseError: %v\n", dbErr)
	}
	
	// Multiple levels of wrapping
	level1 := fmt.Errorf("connection timeout")
	level2 := fmt.Errorf("database unavailable: %w", level1)
	level3 := fmt.Errorf("service unavailable: %w", level2)
	
	fmt.Printf("Multi-level wrapped error: %v\n", level3)
	
	// Unwrap chain
	current := level3
	for current != nil {
		fmt.Printf("Error level: %v\n", current)
		if unwrapper, ok := current.(interface{ Unwrap() error }); ok {
			current = unwrapper.Unwrap()
		} else {
			break
		}
	}
}

func testErrorChaining() {
	fmt.Println("\n--- Error Chaining ---")
	
	// Create error chain
	err := NewErrorChain("failed to process request").
		WithCause(fmt.Errorf("invalid input data")).
		WithContext("userID", "12345").
		WithContext("endpoint", "/api/users").
		WithContext("method", "POST")
	
	fmt.Printf("Error chain: %v\n", err)
	
	// Build complex error chain
	baseErr := fmt.Errorf("database connection refused")
	
	chain := NewErrorChain("user registration failed").
		WithCause(baseErr).
		WithContext("timestamp", "2023-12-01T10:30:00Z").
		WithContext("requestID", "req-abc-123")
	
	fmt.Printf("Complex error chain: %v\n", chain)
	
	// Error chain with multiple causes
	multiCause := NewErrorChain("service startup failed").
		WithCause(fmt.Errorf("configuration error")).
		WithContext("service", "auth-service").
		WithContext("environment", "production")
	
	fmt.Printf("Multi-cause error chain: %v\n", multiCause)
}

func testErrorHandlingPatterns() {
	fmt.Println("\n--- Error Handling Patterns ---")
	
	// Test retry with failing operation
	attemptCount := 0
	err := retryOperation(3, func() error {
		attemptCount++
		if attemptCount < 3 {
			return fmt.Errorf("temporary failure")
		}
		return nil
	})
	
	if err != nil {
		fmt.Printf("Retry failed: %v\n", err)
	} else {
		fmt.Printf("Retry succeeded after %d attempts\n", attemptCount)
	}
	
	// Fallback pattern
	primaryOperation := func() error {
		return fmt.Errorf("primary service unavailable")
	}
	
	fallbackOperation := func() error {
		fmt.Println("Executing fallback operation")
		return nil
	}
	
	executeWithFallback := func(primary, fallback func() error) error {
		if err := primary(); err != nil {
			fmt.Printf("Primary operation failed: %v, trying fallback...\n", err)
			return fallback()
		}
		return nil
	}
	
	if err := executeWithFallback(primaryOperation, fallbackOperation); err != nil {
		fmt.Printf("Both primary and fallback failed: %v\n", err)
	}
	
	// Circuit breaker pattern (simplified)
	type CircuitBreaker struct {
		failureCount   int
		failureThreshold int
		state          string
	}
	
	cb := &CircuitBreaker{
		failureThreshold: 2,
		state:          "closed",
	}
	
	executeWithCircuitBreaker := func(cb *CircuitBreaker, fn func() error) error {
		if cb.state == "open" {
			return fmt.Errorf("circuit breaker is open")
		}
		
		err := fn()
		if err != nil {
			cb.failureCount++
			if cb.failureCount >= cb.failureThreshold {
				cb.state = "open"
				fmt.Println("Circuit breaker opened")
			}
			return err
		}
		cb.failureCount = 0
		return nil
	}
	
	// Test circuit breaker
	failCount := 0
	for i := 0; i < 5; i++ {
		err := executeWithCircuitBreaker(cb, func() error {
			failCount++
			if failCount <= 3 {
				return fmt.Errorf("service failure")
			}
			return nil
		})
		
		if err != nil {
			fmt.Printf("Circuit breaker test %d: %v\n", i+1, err)
		}
	}
}

func testPanicRecoveryPatterns() {
	fmt.Println("\n--- Panic Recovery Patterns ---")
	
	// Basic panic recovery
	safeExecute := func(fn func()) (err error) {
		defer func() {
			if r := recover(); r != nil {
				err = fmt.Errorf("panic recovered: %v", r)
			}
		}()
		fn()
		return nil
	}
	
	// Test panic recovery
	err := safeExecute(func() {
		fmt.Println("Executing function that will panic...")
		panic("something went wrong")
	})
	
	if err != nil {
		fmt.Printf("Function recovered from panic: %v\n", err)
	}
	
	// Panic recovery with context
	safeExecuteWithContext := func(ctx context.Context, fn func()) error {
		done := make(chan error, 1)
		
		go func() {
			defer func() {
				if r := recover(); r != nil {
					done <- fmt.Errorf("panic recovered: %v", r)
				} else {
					done <- nil
				}
			}()
			fn()
		}()
		
		select {
		case <-ctx.Done():
			return fmt.Errorf("context cancelled: %w", ctx.Err())
		case err := <-done:
			return err
		}
	}
	
	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()
	
	err = safeExecuteWithContext(ctx, func() {
		fmt.Println("Executing function with context...")
		// No panic this time
	})
	
	if err != nil {
		fmt.Printf("Context execution failed: %v\n", err)
	} else {
		fmt.Println("Context execution succeeded")
	}
	
	// Panic recovery with cleanup
	executeWithCleanup := func(fn func()) (cleanup func(), err error) {
		done := make(chan struct{})
		cleanup = func() {
			close(done)
			fmt.Println("Cleanup completed")
		}
		
		defer func() {
			if r := recover(); r != nil {
				err = fmt.Errorf("panic recovered: %v", r)
				cleanup()
			}
		}()
		
		fn()
		return cleanup, nil
	}
	
	cleanup, err := executeWithCleanup(func() {
		fmt.Println("Executing function with cleanup...")
		// No panic
	})
	
	if err != nil {
		fmt.Printf("Execution with cleanup failed: %v\n", err)
	} else {
		fmt.Println("Execution with cleanup succeeded")
		cleanup()
	}
}

func testErrorAggregation() {
	fmt.Println("\n--- Error Aggregation ---")
	
	// Aggregate multiple errors
	multiErr := &MultiError{}
	
	// Simulate multiple operations that might fail
	operations := []func() error{
		func() error { return fmt.Errorf("database connection failed") },
		func() error { return nil }, // Success
		func() error { return fmt.Errorf("validation error") },
		func() error { return fmt.Errorf("network timeout") },
	}
	
	for i, op := range operations {
		if err := op(); err != nil {
			multiErr.Add(fmt.Errorf("operation %d: %w", i+1, err))
		}
	}
	
	if multiErr.HasErrors() {
		fmt.Printf("Multiple errors occurred:\n%v", multiErr)
	}
	
	// Parallel error collection
	tasks := []func() error{
		func() error { return fmt.Errorf("task 1 failed") },
		func() error { return nil },
		func() error { return fmt.Errorf("task 3 failed") },
	}
	
	errors := collectErrors(tasks)
	if len(errors) > 0 {
		fmt.Printf("Parallel execution errors: %d\n", len(errors))
		for i, err := range errors {
			fmt.Printf("  Error %d: %v\n", i+1, err)
		}
	}
}

func testErrorClassification() {
	fmt.Println("\n--- Error Classification ---")
	
	// Classify errors by type
	classifyError := func(err error) string {
		switch err.(type) {
		case *ValidationError:
			return fmt.Sprintf("Validation Error - Field: %s", err.(*ValidationError).Field)
		case *NotFoundError:
			return fmt.Sprintf("Not Found Error - Resource: %s", err.(*NotFoundError).Resource)
		case *DatabaseError:
			return fmt.Sprintf("Database Error - Operation: %s", err.(*DatabaseError).Operation)
		default:
			return fmt.Sprintf("Unknown Error Type: %T", err)
		}
	}
	
	// Test classification
	errors := []error{
		&ValidationError{Field: "email", Value: "invalid", Message: "invalid format"},
		&NotFoundError{Resource: "user", ID: "123"},
		&DatabaseError{Operation: "SELECT", Table: "users"},
		fmt.Errorf("generic error"),
	}
	
	for _, err := range errors {
		classification := classifyError(err)
		fmt.Printf("Error classification: %s\n", classification)
	}
	
	// Retryable error classification
	isRetryable := func(err error) bool {
		switch err.(type) {
		case *DatabaseError:
			// Retry database errors
			return true
		case *NotFoundError:
			// Don't retry not found errors
			return false
		case *ValidationError:
			// Don't retry validation errors
			return false
		default:
			// Retry network errors, timeouts, etc.
			return true
		}
	}
	
	fmt.Println("\nRetryable error classification:")
	for _, err := range errors {
		retryable := isRetryable(err)
		fmt.Printf("Error: %v - Retryable: %t\n", err, retryable)
	}
}