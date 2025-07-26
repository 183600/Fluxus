package main

import "fmt"
import "math"

// Calculator struct with basic operations
type Calculator struct {
	memory float64
}

// NewCalculator creates a new calculator instance
func NewCalculator() *Calculator {
	return &Calculator{memory: 0}
}

// Add performs addition
func (c *Calculator) Add(a, b float64) float64 {
	result := a + b
	c.memory = result
	return result
}

// Subtract performs subtraction
func (c *Calculator) Subtract(a, b float64) float64 {
	result := a - b
	c.memory = result
	return result
}

// Multiply performs multiplication
func (c *Calculator) Multiply(a, b float64) float64 {
	result := a * b
	c.memory = result
	return result
}

// Divide performs division with error handling
func (c *Calculator) Divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	result := a / b
	c.memory = result
	return result, nil
}

// Power calculates a to the power of b
func (c *Calculator) Power(a, b float64) float64 {
	result := math.Pow(a, b)
	c.memory = result
	return result
}

// GetMemory returns the stored memory value
func (c *Calculator) GetMemory() float64 {
	return c.memory
}

// ClearMemory resets the memory
func (c *Calculator) ClearMemory() {
	c.memory = 0
}

func main() {
	calc := NewCalculator()
	
	fmt.Println("=== Calculator Demo ===")
	
	// Basic operations
	fmt.Printf("5 + 3 = %.2f\n", calc.Add(5, 3))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	fmt.Printf("10 - 4 = %.2f\n", calc.Subtract(10, 4))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	fmt.Printf("7 * 8 = %.2f\n", calc.Multiply(7, 8))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	// Division with error handling
	result, err := calc.Divide(15, 3)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("15 / 3 = %.2f\n", result)
		fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	}
	
	// Test division by zero
	_, err = calc.Divide(10, 0)
	if err != nil {
		fmt.Printf("Division by zero error: %v\n", err)
	}
	
	// Power calculation
	fmt.Printf("2^10 = %.2f\n", calc.Power(2, 10))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	// Clear memory
	calc.ClearMemory()
	fmt.Printf("Memory after clear: %.2f\n", calc.GetMemory())
}