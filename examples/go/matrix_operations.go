package main

import (
	"fmt"
	"math/rand"
	"time"
)

type Matrix [][]float64

func NewMatrix(rows, cols int) Matrix {
	matrix := make(Matrix, rows)
	for i := range matrix {
		matrix[i] = make([]float64, cols)
	}
	return matrix
}

func (m Matrix) Fill(value float64) {
	for i := range m {
		for j := range m[i] {
			m[i][j] = value
		}
	}
}

func (m Matrix) RandomFill() {
	rand.Seed(time.Now().UnixNano())
	for i := range m {
		for j := range m[i] {
			m[i][j] = rand.Float64() * 100
		}
	}
}

func (m Matrix) Print() {
	for i := range m {
		for j := range m[i] {
			fmt.Printf("%.2f ", m[i][j])
		}
		fmt.Println()
	}
}

func (m Matrix) Add(other Matrix) Matrix {
	if len(m) != len(other) || len(m[0]) != len(other[0]) {
		panic("Matrix dimensions don't match for addition")
	}
	
	result := NewMatrix(len(m), len(m[0]))
	for i := range m {
		for j := range m[i] {
			result[i][j] = m[i][j] + other[i][j]
		}
	}
	return result
}

func (m Matrix) Multiply(other Matrix) Matrix {
	if len(m[0]) != len(other) {
		panic("Invalid matrix dimensions for multiplication")
	}
	
	result := NewMatrix(len(m), len(other[0]))
	for i := range result {
		for j := range result[i] {
			for k := range other {
				result[i][j] += m[i][k] * other[k][j]
			}
		}
	}
	return result
}

func (m Matrix) Transpose() Matrix {
	result := NewMatrix(len(m[0]), len(m))
	for i := range m {
		for j := range m[i] {
			result[j][i] = m[i][j]
		}
	}
	return result
}

func main() {
	fmt.Println("Matrix Operations Demo")
	fmt.Println("======================")
	
	// Create matrices
	a := NewMatrix(3, 3)
	b := NewMatrix(3, 3)
	
	// Fill with random values
	a.RandomFill()
	b.RandomFill()
	
	fmt.Println("Matrix A:")
	a.Print()
	
	fmt.Println("\nMatrix B:")
	b.Print()
	
	// Addition
	sum := a.Add(b)
	fmt.Println("\nA + B:")
	sum.Print()
	
	// Multiplication
	product := a.Multiply(b)
	fmt.Println("\nA * B:")
	product.Print()
	
	// Transpose
	transposed := a.Transpose()
	fmt.Println("\nTranspose of A:")
	transposed.Print()
}