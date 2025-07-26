package main

import (
	"fmt"
	"math"
)

type Matrix [][]float64

func NewMatrix(rows, cols int) Matrix {
	matrix := make(Matrix, rows)
	for i := range matrix {
		matrix[i] = make([]float64, cols)
	}
	return matrix
}

func (m Matrix) Add(other Matrix) Matrix {
	rows, cols := len(m), len(m[0])
	result := NewMatrix(rows, cols)
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			result[i][j] = m[i][j] + other[i][j]
		}
	}
	return result
}

func (m Matrix) Multiply(other Matrix) Matrix {
	rows, cols := len(m), len(other[0])
	result := NewMatrix(rows, cols)
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			for k := 0; k < len(other); k++ {
				result[i][j] += m[i][k] * other[k][j]
			}
		}
	}
	return result
}

func (m Matrix) Determinant() float64 {
	n := len(m)
	if n == 1 {
		return m[0][0]
	}
	if n == 2 {
		return m[0][0]*m[1][1] - m[0][1]*m[1][0]
	}
	
	det := 0.0
	for c := 0; c < n; c++ {
		minor := NewMatrix(n-1, n-1)
		for i := 1; i < n; i++ {
			for j := 0; j < n; j++ {
				if j < c {
					minor[i-1][j] = m[i][j]
				} else if j > c {
					minor[i-1][j-1] = m[i][j]
				}
			}
		}
		det += math.Pow(-1, float64(c)) * m[0][c] * minor.Determinant()
	}
	return det
}

func (m Matrix) Print() {
	for i := range m {
		fmt.Printf("[")
		for j := range m[i] {
			fmt.Printf("%8.2f", m[i][j])
		}
		fmt.Printf(" ]\n")
	}
}

func main() {
	fmt.Println("Matrix Calculator")
	
	m1 := NewMatrix(3, 3)
	m1[0][0], m1[0][1], m1[0][2] = 1, 2, 3
	m1[1][0], m1[1][1], m1[1][2] = 4, 5, 6
	m1[2][0], m1[2][1], m1[2][2] = 7, 8, 10
	
	m2 := NewMatrix(3, 3)
	m2[0][0], m2[0][1], m2[0][2] = 2, 1, 0
	m2[1][0], m2[1][1], m2[1][2] = 1, 3, 2
	m2[2][0], m2[2][1], m2[2][2] = 0, 1, 4
	
	fmt.Println("Matrix 1:")
	m1.Print()
	
	fmt.Println("\nMatrix 2:")
	m2.Print()
	
	fmt.Println("\nMatrix Addition:")
	result := m1.Add(m2)
	result.Print()
	
	fmt.Println("\nMatrix Multiplication:")
	product := m1.Multiply(m2)
	product.Print()
	
	fmt.Printf("\nDeterminant of Matrix 1: %.2f\n", m1.Determinant())
	fmt.Printf("Determinant of Matrix 2: %.2f\n", m2.Determinant())
}