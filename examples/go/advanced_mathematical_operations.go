package main

import (
    "fmt"
    "math"
    "math/rand"
    "runtime"
    "sync"
    "time"
)

// Advanced Mathematical Operations

// Matrix operations
type Matrix [][]float64

func NewMatrix(rows, cols int) Matrix {
    matrix := make(Matrix, rows)
    for i := range matrix {
        matrix[i] = make([]float64, cols)
    }
    return matrix
}

func (m Matrix) Rows() int { return len(m) }
func (m Matrix) Cols() int { 
    if len(m) == 0 { return 0 }
    return len(m[0]) 
}

func (m Matrix) Set(row, col int, value float64) {
    if row >= 0 && row < m.Rows() && col >= 0 && col < m.Cols() {
        m[row][col] = value
    }
}

func (m Matrix) Get(row, col int) float64 {
    if row >= 0 && row < m.Rows() && col >= 0 && col < m.Cols() {
        return m[row][col]
    }
    return 0
}

func (m Matrix) Add(other Matrix) Matrix {
    if m.Rows() != other.Rows() || m.Cols() != other.Cols() {
        return nil
    }
    
    result := NewMatrix(m.Rows(), m.Cols())
    for i := 0; i < m.Rows(); i++ {
        for j := 0; j < m.Cols(); j++ {
            result[i][j] = m[i][j] + other[i][j]
        }
    }
    return result
}

func (m Matrix) Multiply(other Matrix) Matrix {
    if m.Cols() != other.Rows() {
        return nil
    }
    
    result := NewMatrix(m.Rows(), other.Cols())
    for i := 0; i < m.Rows(); i++ {
        for j := 0; j < other.Cols(); j++ {
            sum := 0.0
            for k := 0; k < m.Cols(); k++ {
                sum += m[i][k] * other[k][j]
            }
            result[i][j] = sum
        }
    }
    return result
}

func (m Matrix) Transpose() Matrix {
    result := NewMatrix(m.Cols(), m.Rows())
    for i := 0; i < m.Rows(); i++ {
        for j := 0; j < m.Cols(); j++ {
            result[j][i] = m[i][j]
        }
    }
    return result
}

func (m Matrix) Determinant() float64 {
    if m.Rows() != m.Cols() {
        return 0
    }
    
    n := m.Rows()
    if n == 1 {
        return m[0][0]
    }
    if n == 2 {
        return m[0][0]*m[1][1] - m[0][1]*m[1][0]
    }
    
    det := 0.0
    for j := 0; j < n; j++ {
        minor := m.getMinor(0, j)
        sign := 1.0
        if j%2 == 1 {
            sign = -1.0
        }
        det += sign * m[0][j] * minor.Determinant()
    }
    return det
}

func (m Matrix) getMinor(skipRow, skipCol int) Matrix {
    n := m.Rows()
    minor := NewMatrix(n-1, n-1)
    mi, mj := 0, 0
    
    for i := 0; i < n; i++ {
        if i == skipRow {
            continue
        }
        mj = 0
        for j := 0; j < n; j++ {
            if j == skipCol {
                continue
            }
            minor[mi][mj] = m[i][j]
            mj++
        }
        mi++
    }
    return minor
}

func (m Matrix) String() string {
    result := "[\n"
    for i := 0; i < m.Rows(); i++ {
        result += "  ["
        for j := 0; j < m.Cols(); j++ {
            result += fmt.Sprintf("%8.2f", m[i][j])
            if j < m.Cols()-1 {
                result += ", "
            }
        }
        result += "]\n"
    }
    result += "]"
    return result
}

// Complex number operations
type Complex struct {
    Real, Imag float64
}

func NewComplex(real, imag float64) Complex {
    return Complex{Real: real, Imag: imag}
}

func (c Complex) Add(other Complex) Complex {
    return Complex{c.Real + other.Real, c.Imag + other.Imag}
}

func (c Complex) Subtract(other Complex) Complex {
    return Complex{c.Real - other.Real, c.Imag - other.Imag}
}

func (c Complex) Multiply(other Complex) Complex {
    return Complex{
        c.Real*other.Real - c.Imag*other.Imag,
        c.Real*other.Imag + c.Imag*other.Real,
    }
}

func (c Complex) Divide(other Complex) Complex {
    denominator := other.Real*other.Real + other.Imag*other.Imag
    return Complex{
        (c.Real*other.Real + c.Imag*other.Imag) / denominator,
        (c.Imag*other.Real - c.Real*other.Imag) / denominator,
    }
}

func (c Complex) Magnitude() float64 {
    return math.Sqrt(c.Real*c.Real + c.Imag*c.Imag)
}

func (c Complex) Phase() float64 {
    return math.Atan2(c.Imag, c.Real)
}

func (c Complex) String() string {
    if c.Imag >= 0 {
        return fmt.Sprintf("%.2f + %.2fi", c.Real, c.Imag)
    }
    return fmt.Sprintf("%.2f - %.2fi", c.Real, -c.Imag)
}

// Advanced statistical functions
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
    for _, val := range s.data {
        sum += val
    }
    return sum / float64(len(s.data))
}

func (s *Statistics) Median() float64 {
    if len(s.data) == 0 {
        return 0
    }
    
    // Sort data for median calculation
    sorted := make([]float64, len(s.data))
    copy(sorted, s.data)
    
    // Simple bubble sort for demonstration
    for i := 0; i < len(sorted); i++ {
        for j := 0; j < len(sorted)-1-i; j++ {
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

func (s *Statistics) StandardDeviation() float64 {
    if len(s.data) == 0 {
        return 0
    }
    
    mean := s.Mean()
    sumSquares := 0.0
    for _, val := range s.data {
        diff := val - mean
        sumSquares += diff * diff
    }
    return math.Sqrt(sumSquares / float64(len(s.data)))
}

func (s *Statistics) Variance() float64 {
    std := s.StandardDeviation()
    return std * std
}

func (s *Statistics) Min() float64 {
    if len(s.data) == 0 {
        return 0
    }
    min := s.data[0]
    for _, val := range s.data[1:] {
        if val < min {
            min = val
        }
    }
    return min
}

func (s *Statistics) Max() float64 {
    if len(s.data) == 0 {
        return 0
    }
    max := s.data[0]
    for _, val := range s.data[1:] {
        if val > max {
            max = val
        }
    }
    return max
}

// Numerical methods
func NewtonRaphson(f, df func(float64) float64, x0, tolerance float64, maxIter int) float64 {
    x := x0
    for i := 0; i < maxIter; i++ {
        fx := f(x)
        if math.Abs(fx) < tolerance {
            return x
        }
        dfx := df(x)
        if math.Abs(dfx) < 1e-10 {
            break
        }
        x = x - fx/dfx
    }
    return x
}

func TrapezoidalRule(f func(float64) float64, a, b float64, n int) float64 {
    h := (b - a) / float64(n)
    sum := (f(a) + f(b)) / 2
    
    for i := 1; i < n; i++ {
        x := a + float64(i)*h
        sum += f(x)
    }
    
    return sum * h
}

func SimpsonsRule(f func(float64) float64, a, b float64, n int) float64 {
    if n%2 != 0 {
        n++ // Make n even
    }
    
    h := (b - a) / float64(n)
    sum := f(a) + f(b)
    
    for i := 1; i < n; i++ {
        x := a + float64(i)*h
        if i%2 == 0 {
            sum += 2 * f(x)
        } else {
            sum += 4 * f(x)
        }
    }
    
    return sum * h / 3
}

// Parallel computation examples
func ParallelSum(numbers []int) int {
    numWorkers := runtime.NumCPU()
    chunkSize := len(numbers) / numWorkers
    if chunkSize == 0 {
        chunkSize = 1
        numWorkers = len(numbers)
    }
    
    resultChan := make(chan int, numWorkers)
    var wg sync.WaitGroup
    
    for i := 0; i < numWorkers; i++ {
        wg.Add(1)
        go func(start int) {
            defer wg.Done()
            end := start + chunkSize
            if end > len(numbers) {
                end = len(numbers)
            }
            
            sum := 0
            for j := start; j < end; j++ {
                sum += numbers[j]
            }
            resultChan <- sum
        }(i * chunkSize)
    }
    
    go func() {
        wg.Wait()
        close(resultChan)
    }()
    
    totalSum := 0
    for sum := range resultChan {
        totalSum += sum
    }
    
    return totalSum
}

func ParallelMatrixMultiply(a, b Matrix) Matrix {
    if a.Cols() != b.Rows() {
        return nil
    }
    
    result := NewMatrix(a.Rows(), b.Cols())
    numWorkers := runtime.NumCPU()
    rowsPerWorker := a.Rows() / numWorkers
    if rowsPerWorker == 0 {
        rowsPerWorker = 1
        numWorkers = a.Rows()
    }
    
    var wg sync.WaitGroup
    
    for w := 0; w < numWorkers; w++ {
        wg.Add(1)
        go func(startRow int) {
            defer wg.Done()
            endRow := startRow + rowsPerWorker
            if endRow > a.Rows() {
                endRow = a.Rows()
            }
            
            for i := startRow; i < endRow; i++ {
                for j := 0; j < b.Cols(); j++ {
                    sum := 0.0
                    for k := 0; k < a.Cols(); k++ {
                        sum += a[i][k] * b[k][j]
                    }
                    result[i][j] = sum
                }
            }
        }(w * rowsPerWorker)
    }
    
    wg.Wait()
    return result
}

// Benchmark functions
func BenchmarkFunction(name string, fn func()) time.Duration {
    start := time.Now()
    fn()
    duration := time.Since(start)
    fmt.Printf("  %s took: %v\n", name, duration)
    return duration
}

func main() {
    fmt.Println("=== Advanced Mathematical Operations Demo ===")
    
    // Matrix operations
    fmt.Println("\n1. Matrix Operations:")
    m1 := NewMatrix(3, 3)
    m1.Set(0, 0, 1); m1.Set(0, 1, 2); m1.Set(0, 2, 3)
    m1.Set(1, 0, 4); m1.Set(1, 1, 5); m1.Set(1, 2, 6)
    m1.Set(2, 0, 7); m1.Set(2, 1, 8); m1.Set(2, 2, 9)
    
    m2 := NewMatrix(3, 3)
    m2.Set(0, 0, 9); m2.Set(0, 1, 8); m2.Set(0, 2, 7)
    m2.Set(1, 0, 6); m2.Set(1, 1, 5); m2.Set(1, 2, 4)
    m2.Set(2, 0, 3); m2.Set(2, 1, 2); m2.Set(2, 2, 1)
    
    fmt.Printf("  Matrix 1:%s\n", m1)
    fmt.Printf("  Matrix 2:%s\n", m2)
    
    sum := m1.Add(m2)
    fmt.Printf("  Sum:%s\n", sum)
    
    product := m1.Multiply(m2)
    fmt.Printf("  Product:%s\n", product)
    
    fmt.Printf("  Determinant of Matrix 1: %.2f\n", m1.Determinant())
    
    // Complex number operations
    fmt.Println("\n2. Complex Number Operations:")
    c1 := NewComplex(3, 4)
    c2 := NewComplex(1, -2)
    
    fmt.Printf("  c1 = %s\n", c1)
    fmt.Printf("  c2 = %s\n", c2)
    fmt.Printf("  c1 + c2 = %s\n", c1.Add(c2))
    fmt.Printf("  c1 * c2 = %s\n", c1.Multiply(c2))
    fmt.Printf("  |c1| = %.2f\n", c1.Magnitude())
    fmt.Printf("  arg(c1) = %.2f radians\n", c1.Phase())
    
    // Statistics
    fmt.Println("\n3. Statistical Analysis:")
    data := []float64{1.2, 2.3, 3.1, 4.5, 2.8, 6.2, 1.9, 3.7, 4.1, 5.3}
    stats := NewStatistics(data)
    
    fmt.Printf("  Data: %v\n", data)
    fmt.Printf("  Mean: %.2f\n", stats.Mean())
    fmt.Printf("  Median: %.2f\n", stats.Median())
    fmt.Printf("  Standard Deviation: %.2f\n", stats.StandardDeviation())
    fmt.Printf("  Variance: %.2f\n", stats.Variance())
    fmt.Printf("  Min: %.2f, Max: %.2f\n", stats.Min(), stats.Max())
    
    // Numerical methods
    fmt.Println("\n4. Numerical Methods:")
    
    // Find root of x^2 - 2 = 0 (should be sqrt(2))
    f := func(x float64) float64 { return x*x - 2 }
    df := func(x float64) float64 { return 2 * x }
    root := NewtonRaphson(f, df, 1.0, 1e-10, 100)
    fmt.Printf("  Root of x² - 2 = 0: %.10f (√2 ≈ %.10f)\n", root, math.Sqrt(2))
    
    // Numerical integration of x^2 from 0 to 2 (should be 8/3)
    integrand := func(x float64) float64 { return x * x }
    trapezoid := TrapezoidalRule(integrand, 0, 2, 1000)
    simpson := SimpsonsRule(integrand, 0, 2, 1000)
    exact := 8.0 / 3.0
    
    fmt.Printf("  ∫₀² x² dx:\n")
    fmt.Printf("    Trapezoidal rule: %.6f\n", trapezoid)
    fmt.Printf("    Simpson's rule: %.6f\n", simpson)
    fmt.Printf("    Exact value: %.6f\n", exact)
    
    // Parallel computation
    fmt.Println("\n5. Parallel Computation:")
    
    // Generate large array for parallel sum
    rand.Seed(time.Now().UnixNano())
    largeArray := make([]int, 1000000)
    for i := range largeArray {
        largeArray[i] = rand.Intn(100)
    }
    
    fmt.Printf("  Computing sum of %d random numbers...\n", len(largeArray))
    
    // Sequential sum
    seqSum := 0
    seqDuration := BenchmarkFunction("Sequential sum", func() {
        seqSum = 0
        for _, num := range largeArray {
            seqSum += num
        }
    })
    
    // Parallel sum
    var parSum int
    parDuration := BenchmarkFunction("Parallel sum", func() {
        parSum = ParallelSum(largeArray)
    })
    
    fmt.Printf("  Sequential result: %d\n", seqSum)
    fmt.Printf("  Parallel result: %d\n", parSum)
    fmt.Printf("  Speedup: %.2fx\n", float64(seqDuration)/float64(parDuration))
    
    // Parallel matrix multiplication
    fmt.Println("\n6. Parallel Matrix Multiplication:")
    size := 100
    ma := NewMatrix(size, size)
    mb := NewMatrix(size, size)
    
    // Fill matrices with random values
    for i := 0; i < size; i++ {
        for j := 0; j < size; j++ {
            ma.Set(i, j, rand.Float64())
            mb.Set(i, j, rand.Float64())
        }
    }
    
    fmt.Printf("  Multiplying %dx%d matrices...\n", size, size)
    
    var seqResult, parResult Matrix
    
    seqDuration = BenchmarkFunction("Sequential multiplication", func() {
        seqResult = ma.Multiply(mb)
    })
    
    parDuration = BenchmarkFunction("Parallel multiplication", func() {
        parResult = ParallelMatrixMultiply(ma, mb)
    })
    
    // Verify results are the same
    same := true
    for i := 0; i < size && same; i++ {
        for j := 0; j < size && same; j++ {
            if math.Abs(seqResult[i][j]-parResult[i][j]) > 1e-10 {
                same = false
            }
        }
    }
    
    fmt.Printf("  Results match: %t\n", same)
    fmt.Printf("  Speedup: %.2fx\n", float64(seqDuration)/float64(parDuration))
    
    fmt.Println("\n=== Demo completed ===")
}