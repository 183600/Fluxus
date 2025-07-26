package main

import (
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"time"
)

// Matrix operations
type Matrix struct {
	rows, cols int
	data       [][]float64
}

func NewMatrix(rows, cols int) *Matrix {
	data := make([][]float64, rows)
	for i := range data {
		data[i] = make([]float64, cols)
	}
	return &Matrix{rows: rows, cols: cols, data: data}
}

func (m *Matrix) Set(row, col int, value float64) error {
	if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
		return fmt.Errorf("index out of bounds")
	}
	m.data[row][col] = value
	return nil
}

func (m *Matrix) Get(row, col int) (float64, error) {
	if row < 0 || row >= m.rows || col < 0 || col >= m.cols {
		return 0, fmt.Errorf("index out of bounds")
	}
	return m.data[row][col], nil
}

func (m *Matrix) Add(other *Matrix) (*Matrix, error) {
	if m.rows != other.rows || m.cols != other.cols {
		return nil, fmt.Errorf("matrix dimensions don't match")
	}
	
	result := NewMatrix(m.rows, m.cols)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < m.cols; j++ {
			result.data[i][j] = m.data[i][j] + other.data[i][j]
		}
	}
	return result, nil
}

func (m *Matrix) Multiply(other *Matrix) (*Matrix, error) {
	if m.cols != other.rows {
		return nil, fmt.Errorf("incompatible matrix dimensions for multiplication")
	}
	
	result := NewMatrix(m.rows, other.cols)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < other.cols; j++ {
			sum := 0.0
			for k := 0; k < m.cols; k++ {
				sum += m.data[i][k] * other.data[k][j]
			}
			result.data[i][j] = sum
		}
	}
	return result, nil
}

func (m *Matrix) Transpose() *Matrix {
	result := NewMatrix(m.cols, m.rows)
	for i := 0; i < m.rows; i++ {
		for j := 0; j < m.cols; j++ {
			result.data[j][i] = m.data[i][j]
		}
	}
	return result
}

func (m *Matrix) Print() {
	for i := 0; i < m.rows; i++ {
		fmt.Print("[")
		for j := 0; j < m.cols; j++ {
			fmt.Printf("%8.2f", m.data[i][j])
			if j < m.cols-1 {
				fmt.Print(" ")
			}
		}
		fmt.Println("]")
	}
}

// Statistics functions
type Statistics struct {
	data []float64
}

func NewStatistics(data []float64) *Statistics {
	return &Statistics{data: data}
}

func (s *Statistics) Mean() float64 {
	if len(s.data) == 0 {
		return 0
	}
	sum := 0.0
	for _, v := range s.data {
		sum += v
	}
	return sum / float64(len(s.data))
}

func (s *Statistics) Median() float64 {
	if len(s.data) == 0 {
		return 0
	}
	
	// Create a copy and sort it
	sorted := make([]float64, len(s.data))
	copy(sorted, s.data)
	
	// Simple bubble sort for demonstration
	for i := 0; i < len(sorted)-1; i++ {
		for j := 0; j < len(sorted)-i-1; j++ {
			if sorted[j] > sorted[j+1] {
				sorted[j], sorted[j+1] = sorted[j+1], sorted[j]
			}
		}
	}
	
	n := len(sorted)
	if n%2 == 0 {
		return (sorted[n/2-1] + sorted[n/2]) / 2
	}
	return sorted[n/2]
}

func (s *Statistics) Mode() []float64 {
	if len(s.data) == 0 {
		return nil
	}
	
	frequency := make(map[float64]int)
	for _, v := range s.data {
		frequency[v]++
	}
	
	maxFreq := 0
	for _, freq := range frequency {
		if freq > maxFreq {
			maxFreq = freq
		}
	}
	
	var modes []float64
	for value, freq := range frequency {
		if freq == maxFreq {
			modes = append(modes, value)
		}
	}
	
	return modes
}

func (s *Statistics) StandardDeviation() float64 {
	if len(s.data) <= 1 {
		return 0
	}
	
	mean := s.Mean()
	sumSquaredDiffs := 0.0
	
	for _, v := range s.data {
		diff := v - mean
		sumSquaredDiffs += diff * diff
	}
	
	variance := sumSquaredDiffs / float64(len(s.data)-1)
	return math.Sqrt(variance)
}

func (s *Statistics) Min() float64 {
	if len(s.data) == 0 {
		return 0
	}
	
	min := s.data[0]
	for _, v := range s.data {
		if v < min {
			min = v
		}
	}
	return min
}

func (s *Statistics) Max() float64 {
	if len(s.data) == 0 {
		return 0
	}
	
	max := s.data[0]
	for _, v := range s.data {
		if v > max {
			max = v
		}
	}
	return max
}

func (s *Statistics) Range() float64 {
	return s.Max() - s.Min()
}

// Complex number operations
type Complex struct {
	real, imag float64
}

func NewComplex(real, imag float64) *Complex {
	return &Complex{real: real, imag: imag}
}

func (c *Complex) Add(other *Complex) *Complex {
	return &Complex{
		real: c.real + other.real,
		imag: c.imag + other.imag,
	}
}

func (c *Complex) Subtract(other *Complex) *Complex {
	return &Complex{
		real: c.real - other.real,
		imag: c.imag - other.imag,
	}
}

func (c *Complex) Multiply(other *Complex) *Complex {
	return &Complex{
		real: c.real*other.real - c.imag*other.imag,
		imag: c.real*other.imag + c.imag*other.real,
	}
}

func (c *Complex) Magnitude() float64 {
	return math.Sqrt(c.real*c.real + c.imag*c.imag)
}

func (c *Complex) String() string {
	if c.imag >= 0 {
		return fmt.Sprintf("%.2f + %.2fi", c.real, c.imag)
	}
	return fmt.Sprintf("%.2f - %.2fi", c.real, -c.imag)
}

// Numerical methods
func NewtonRaphson(f, df func(float64) float64, x0 float64, tolerance float64, maxIterations int) (float64, int, error) {
	x := x0
	for i := 0; i < maxIterations; i++ {
		fx := f(x)
		if math.Abs(fx) < tolerance {
			return x, i, nil
		}
		
		dfx := df(x)
		if math.Abs(dfx) < 1e-15 {
			return x, i, fmt.Errorf("derivative is zero")
		}
		
		x = x - fx/dfx
	}
	
	return x, maxIterations, fmt.Errorf("maximum iterations reached")
}

func TrapezoidalIntegration(f func(float64) float64, a, b float64, n int) float64 {
	h := (b - a) / float64(n)
	sum := (f(a) + f(b)) / 2.0
	
	for i := 1; i < n; i++ {
		x := a + float64(i)*h
		sum += f(x)
	}
	
	return sum * h
}

// Random number generation and probability
func GenerateRandomData(size int, min, max float64) []float64 {
	rand.Seed(time.Now().UnixNano())
	data := make([]float64, size)
	
	for i := 0; i < size; i++ {
		data[i] = min + rand.Float64()*(max-min)
	}
	
	return data
}

func MonteCarloPi(iterations int) float64 {
	rand.Seed(time.Now().UnixNano())
	insideCircle := 0
	
	for i := 0; i < iterations; i++ {
		x := rand.Float64()*2 - 1 // Random number between -1 and 1
		y := rand.Float64()*2 - 1
		
		if x*x+y*y <= 1 {
			insideCircle++
		}
	}
	
	return 4.0 * float64(insideCircle) / float64(iterations)
}

func main() {
	fmt.Println("Advanced Mathematical Operations Demo")
	fmt.Println("====================================")
	
	// Matrix operations
	fmt.Println("\n1. Matrix Operations:")
	m1 := NewMatrix(2, 3)
	m1.Set(0, 0, 1)
	m1.Set(0, 1, 2)
	m1.Set(0, 2, 3)
	m1.Set(1, 0, 4)
	m1.Set(1, 1, 5)
	m1.Set(1, 2, 6)
	
	fmt.Println("Matrix 1:")
	m1.Print()
	
	m2 := NewMatrix(2, 3)
	m2.Set(0, 0, 7)
	m2.Set(0, 1, 8)
	m2.Set(0, 2, 9)
	m2.Set(1, 0, 10)
	m2.Set(1, 1, 11)
	m2.Set(1, 2, 12)
	
	fmt.Println("Matrix 2:")
	m2.Print()
	
	sum, _ := m1.Add(m2)
	fmt.Println("Sum:")
	sum.Print()
	
	m3 := NewMatrix(3, 2)
	m3.Set(0, 0, 1)
	m3.Set(0, 1, 2)
	m3.Set(1, 0, 3)
	m3.Set(1, 1, 4)
	m3.Set(2, 0, 5)
	m3.Set(2, 1, 6)
	
	product, _ := m1.Multiply(m3)
	fmt.Println("Product (m1 * m3):")
	product.Print()
	
	transpose := m1.Transpose()
	fmt.Println("Transpose of m1:")
	transpose.Print()
	
	// Statistics
	fmt.Println("\n2. Statistical Analysis:")
	data := []float64{10, 15, 20, 25, 30, 35, 40, 20, 25, 30}
	stats := NewStatistics(data)
	
	fmt.Printf("Data: %v\n", data)
	fmt.Printf("Mean: %.2f\n", stats.Mean())
	fmt.Printf("Median: %.2f\n", stats.Median())
	fmt.Printf("Mode: %v\n", stats.Mode())
	fmt.Printf("Standard Deviation: %.2f\n", stats.StandardDeviation())
	fmt.Printf("Min: %.2f\n", stats.Min())
	fmt.Printf("Max: %.2f\n", stats.Max())
	fmt.Printf("Range: %.2f\n", stats.Range())
	
	// Complex numbers
	fmt.Println("\n3. Complex Number Operations:")
	c1 := NewComplex(3, 4)
	c2 := NewComplex(1, 2)
	
	fmt.Printf("c1 = %s\n", c1.String())
	fmt.Printf("c2 = %s\n", c2.String())
	fmt.Printf("c1 + c2 = %s\n", c1.Add(c2).String())
	fmt.Printf("c1 - c2 = %s\n", c1.Subtract(c2).String())
	fmt.Printf("c1 * c2 = %s\n", c1.Multiply(c2).String())
	fmt.Printf("|c1| = %.2f\n", c1.Magnitude())
	fmt.Printf("|c2| = %.2f\n", c2.Magnitude())
	
	// Numerical methods
	fmt.Println("\n4. Numerical Methods:")
	
	// Find root of x^2 - 2 = 0 (should be sqrt(2))
	f := func(x float64) float64 { return x*x - 2 }
	df := func(x float64) float64 { return 2 * x }
	
	root, iterations, err := NewtonRaphson(f, df, 1.0, 1e-10, 100)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	} else {
		fmt.Printf("Root of x² - 2 = 0: %.10f (found in %d iterations)\n", root, iterations)
		fmt.Printf("Verification: f(%.10f) = %.2e\n", root, f(root))
	}
	
	// Numerical integration of x^2 from 0 to 1 (should be 1/3)
	integrand := func(x float64) float64 { return x * x }
	integral := TrapezoidalIntegration(integrand, 0, 1, 1000)
	fmt.Printf("∫₀¹ x² dx ≈ %.6f (exact value: %.6f)\n", integral, 1.0/3.0)
	
	// Monte Carlo estimation of π
	fmt.Println("\n5. Monte Carlo Methods:")
	piEstimate := MonteCarloPi(1000000)
	fmt.Printf("π ≈ %.6f (Monte Carlo with 1M samples)\n", piEstimate)
	fmt.Printf("Error: %.6f\n", math.Abs(math.Pi-piEstimate))
	
	// Random data analysis
	fmt.Println("\n6. Random Data Analysis:")
	randomData := GenerateRandomData(1000, 0, 100)
	randomStats := NewStatistics(randomData)
	
	fmt.Printf("Generated %d random numbers between 0 and 100:\n", len(randomData))
	fmt.Printf("Mean: %.2f\n", randomStats.Mean())
	fmt.Printf("Standard Deviation: %.2f\n", randomStats.StandardDeviation())
	fmt.Printf("Min: %.2f, Max: %.2f\n", randomStats.Min(), randomStats.Max())
	
	// Mathematical constants and functions
	fmt.Println("\n7. Mathematical Constants and Functions:")
	fmt.Printf("π = %.10f\n", math.Pi)
	fmt.Printf("e = %.10f\n", math.E)
	fmt.Printf("φ (golden ratio) = %.10f\n", (1+math.Sqrt(5))/2)
	
	x := 2.5
	fmt.Printf("For x = %.1f:\n", x)
	fmt.Printf("sin(x) = %.6f\n", math.Sin(x))
	fmt.Printf("cos(x) = %.6f\n", math.Cos(x))
	fmt.Printf("tan(x) = %.6f\n", math.Tan(x))
	fmt.Printf("ln(x) = %.6f\n", math.Log(x))
	fmt.Printf("log₁₀(x) = %.6f\n", math.Log10(x))
	fmt.Printf("e^x = %.6f\n", math.Exp(x))
}