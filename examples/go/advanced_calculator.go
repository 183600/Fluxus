package main

import (
	"fmt"
	"math"
	"strconv"
)

type Calculator struct {
	memory float64
}

func NewCalculator() *Calculator {
	return &Calculator{memory: 0}
}

func (c *Calculator) Add(a, b float64) float64 {
	result := a + b
	c.memory = result
	return result
}

func (c *Calculator) Subtract(a, b float64) float64 {
	result := a - b
	c.memory = result
	return result
}

func (c *Calculator) Multiply(a, b float64) float64 {
	result := a * b
	c.memory = result
	return result
}

func (c *Calculator) Divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	result := a / b
	c.memory = result
	return result, nil
}

func (c *Calculator) Power(base, exp float64) float64 {
	result := math.Pow(base, exp)
	c.memory = result
	return result
}

func (c *Calculator) Sqrt(x float64) (float64, error) {
	if x < 0 {
		return 0, fmt.Errorf("square root of negative number")
	}
	result := math.Sqrt(x)
	c.memory = result
	return result, nil
}

func (c *Calculator) GetMemory() float64 {
	return c.memory
}

func (c *Calculator) ClearMemory() {
	c.memory = 0
}

func main() {
	calc := NewCalculator()
	
	fmt.Println("Advanced Calculator Demo")
	fmt.Println("========================")
	
	// Basic operations
	fmt.Printf("10 + 5 = %.2f\n", calc.Add(10, 5))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	fmt.Printf("20 - 8 = %.2f\n", calc.Subtract(20, 8))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	fmt.Printf("7 * 6 = %.2f\n", calc.Multiply(7, 6))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	result, err := calc.Divide(100, 4)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("100 / 4 = %.2f\n", result)
		fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	}
	
	// Advanced operations
	fmt.Printf("2^8 = %.2f\n", calc.Power(2, 8))
	fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	
	sqrt_result, err := calc.Sqrt(144)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("√144 = %.2f\n", sqrt_result)
		fmt.Printf("Memory: %.2f\n", calc.GetMemory())
	}
	
	// Test error handling
	_, err = calc.Divide(10, 0)
	if err != nil {
		fmt.Printf("Division by zero error: %v\n", err)
	}
	
	_, err = calc.Sqrt(-25)
	if err != nil {
		fmt.Printf("Negative square root error: %v\n", err)
	}
	
	calc.ClearMemory()
	fmt.Printf("Memory after clear: %.2f\n", calc.GetMemory())
}