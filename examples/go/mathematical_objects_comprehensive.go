package main

import (
	"fmt"
	"math"
)

type ComplexNumber struct {
	Real float64
	Imag float64
}

func NewComplexNumber(real, imag float64) *ComplexNumber {
	return &ComplexNumber{Real: real, Imag: imag}
}

func (c *ComplexNumber) String() string {
	if c.Imag >= 0 {
		return fmt.Sprintf("%.2f + %.2fi", c.Real, c.Imag)
	} else {
		return fmt.Sprintf("%.2f - %.2fi", c.Real, math.Abs(c.Imag))
	}
}

func (c *ComplexNumber) Add(other *ComplexNumber) *ComplexNumber {
	return NewComplexNumber(c.Real+other.Real, c.Imag+other.Imag)
}

func (c *ComplexNumber) Subtract(other *ComplexNumber) *ComplexNumber {
	return NewComplexNumber(c.Real-other.Real, c.Imag-other.Imag)
}

func (c *ComplexNumber) Multiply(other *ComplexNumber) *ComplexNumber {
	real := c.Real*other.Real - c.Imag*other.Imag
	imag := c.Real*other.Imag + c.Imag*other.Real
	return NewComplexNumber(real, imag)
}

func (c *ComplexNumber) Conjugate() *ComplexNumber {
	return NewComplexNumber(c.Real, -c.Imag)
}

func (c *ComplexNumber) Magnitude() float64 {
	return math.Sqrt(c.Real*c.Real + c.Imag*c.Imag)
}

type Vector struct {
	Components []float64
	Dimension  int
}

func NewVector(components []float64) *Vector {
	comp := make([]float64, len(components))
	copy(comp, components)
	return &Vector{Components: comp, Dimension: len(comp)}
}

func (v *Vector) String() string {
	return fmt.Sprintf("Vector%v", v.Components)
}

func (v *Vector) Add(other *Vector) (*Vector, error) {
	if v.Dimension != other.Dimension {
		return nil, fmt.Errorf("vectors must have same dimension")
	}
	
	result := make([]float64, v.Dimension)
	for i := 0; i < v.Dimension; i++ {
		result[i] = v.Components[i] + other.Components[i]
	}
	
	return NewVector(result), nil
}

func (v *Vector) Subtract(other *Vector) (*Vector, error) {
	if v.Dimension != other.Dimension {
		return nil, fmt.Errorf("vectors must have same dimension")
	}
	
	result := make([]float64, v.Dimension)
	for i := 0; i < v.Dimension; i++ {
		result[i] = v.Components[i] - other.Components[i]
	}
	
	return NewVector(result), nil
}

func (v *Vector) ScalarMultiply(scalar float64) *Vector {
	result := make([]float64, v.Dimension)
	for i := 0; i < v.Dimension; i++ {
		result[i] = v.Components[i] * scalar
	}
	
	return NewVector(result)
}

func (v *Vector) DotProduct(other *Vector) (float64, error) {
	if v.Dimension != other.Dimension {
		return 0, fmt.Errorf("vectors must have same dimension")
	}
	
	result := 0.0
	for i := 0; i < v.Dimension; i++ {
		result += v.Components[i] * other.Components[i]
	}
	
	return result, nil
}

func (v *Vector) Magnitude() float64 {
	sum := 0.0
	for _, component := range v.Components {
		sum += component * component
	}
	return math.Sqrt(sum)
}

func (v *Vector) Normalize() *Vector {
	mag := v.Magnitude()
	if mag == 0 {
		return NewVector(make([]float64, v.Dimension))
	}
	
	result := make([]float64, v.Dimension)
	for i := 0; i < v.Dimension; i++ {
		result[i] = v.Components[i] / mag
	}
	
	return NewVector(result)
}

func (v *Vector) CrossProduct(other *Vector) (*Vector, error) {
	if v.Dimension != 3 || other.Dimension != 3 {
		return nil, fmt.Errorf("cross product only defined for 3D vectors")
	}
	
	a, b, c := v.Components[0], v.Components[1], v.Components[2]
	x, y, z := other.Components[0], other.Components[1], other.Components[2]
	
	result := []float64{
		b*z - c*y,
		c*x - a*z,
		a*y - b*x,
	}
	
	return NewVector(result), nil
}

type Polynomial struct {
	Coefficients []float64
}

func NewPolynomial(coefficients []float64) *Polynomial {
	coeff := make([]float64, len(coefficients))
	copy(coeff, coefficients)
	
	p := &Polynomial{Coefficients: coeff}
	p.removeLeadingZeros()
	return p
}

func (p *Polynomial) removeLeadingZeros() {
	for len(p.Coefficients) > 1 && p.Coefficients[len(p.Coefficients)-1] == 0 {
		p.Coefficients = p.Coefficients[:len(p.Coefficients)-1]
	}
}

func (p *Polynomial) String() string {
	if len(p.Coefficients) == 0 {
		return "0"
	}
	
	var terms []string
	degree := len(p.Coefficients) - 1
	
	for i := degree; i >= 0; i-- {
		coeff := p.Coefficients[i]
		if coeff == 0 {
			continue
		}
		
		var term string
		if i == 0 {
			term = fmt.Sprintf("%.2f", coeff)
		} else if i == 1 {
			if coeff == 1 {
				term = "x"
			} else if coeff == -1 {
				term = "-x"
			} else {
				term = fmt.Sprintf("%.2fx", coeff)
			}
		} else {
			if coeff == 1 {
				term = fmt.Sprintf("x^%d", i)
			} else if coeff == -1 {
				term = fmt.Sprintf("-x^%d", i)
			} else {
				term = fmt.Sprintf("%.2fx^%d", coeff, i)
			}
		}
		
		terms = append(terms, term)
	}
	
	if len(terms) == 0 {
		return "0"
	}
	
	result := terms[0]
	for i := 1; i < len(terms); i++ {
		if terms[i][0] == '-' {
			result += " - " + terms[i][1:]
		} else {
			result += " + " + terms[i]
		}
	}
	
	return result
}

func (p *Polynomial) Add(other *Polynomial) *Polynomial {
	maxLen := len(p.Coefficients)
	if len(other.Coefficients) > maxLen {
		maxLen = len(other.Coefficients)
	}
	
	result := make([]float64, maxLen)
	
	for i := 0; i < maxLen; i++ {
		var a, b float64
		if i < len(p.Coefficients) {
			a = p.Coefficients[i]
		}
		if i < len(other.Coefficients) {
			b = other.Coefficients[i]
		}
		result[i] = a + b
	}
	
	return NewPolynomial(result)
}

func (p *Polynomial) Multiply(other *Polynomial) *Polynomial {
	resultDegree := len(p.Coefficients) + len(other.Coefficients) - 1
	result := make([]float64, resultDegree)
	
	for i := 0; i < len(p.Coefficients); i++ {
		for j := 0; j < len(other.Coefficients); j++ {
			result[i+j] += p.Coefficients[i] * other.Coefficients[j]
		}
	}
	
	return NewPolynomial(result)
}

func (p *Polynomial) Evaluate(x float64) float64 {
	result := 0.0
	for i, coeff := range p.Coefficients {
		result += coeff * math.Pow(x, float64(i))
	}
	return result
}

func (p *Polynomial) Derivative() *Polynomial {
	if len(p.Coefficients) <= 1 {
		return NewPolynomial([]float64{0})
	}
	
	result := make([]float64, len(p.Coefficients)-1)
	for i := 1; i < len(p.Coefficients); i++ {
		result[i-1] = float64(i) * p.Coefficients[i]
	}
	
	return NewPolynomial(result)
}

type Fraction struct {
	Numerator   int
	Denominator int
}

func NewFraction(numerator, denominator int) (*Fraction, error) {
	if denominator == 0 {
		return nil, fmt.Errorf("denominator cannot be zero")
	}
	
	f := &Fraction{Numerator: numerator, Denominator: denominator}
	f.simplify()
	return f, nil
}

func (f *Fraction) gcd(a, b int) int {
	if b == 0 {
		return a
	}
	return f.gcd(b, a%b)
}

func (f *Fraction) abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (f *Fraction) simplify() {
	gcd := f.gcd(f.abs(f.Numerator), f.abs(f.Denominator))
	f.Numerator /= gcd
	f.Denominator /= gcd
	
	if f.Denominator < 0 {
		f.Numerator = -f.Numerator
		f.Denominator = -f.Denominator
	}
}

func (f *Fraction) String() string {
	if f.Denominator == 1 {
		return fmt.Sprintf("%d", f.Numerator)
	}
	return fmt.Sprintf("%d/%d", f.Numerator, f.Denominator)
}

func (f *Fraction) Add(other *Fraction) *Fraction {
	num := f.Numerator*other.Denominator + other.Numerator*f.Denominator
	den := f.Denominator * other.Denominator
	result, _ := NewFraction(num, den)
	return result
}

func (f *Fraction) Subtract(other *Fraction) *Fraction {
	num := f.Numerator*other.Denominator - other.Numerator*f.Denominator
	den := f.Denominator * other.Denominator
	result, _ := NewFraction(num, den)
	return result
}

func (f *Fraction) Multiply(other *Fraction) *Fraction {
	num := f.Numerator * other.Numerator
	den := f.Denominator * other.Denominator
	result, _ := NewFraction(num, den)
	return result
}

func (f *Fraction) Divide(other *Fraction) (*Fraction, error) {
	if other.Numerator == 0 {
		return nil, fmt.Errorf("cannot divide by zero")
	}
	num := f.Numerator * other.Denominator
	den := f.Denominator * other.Numerator
	return NewFraction(num, den)
}

func (f *Fraction) ToDecimal() float64 {
	return float64(f.Numerator) / float64(f.Denominator)
}

type Matrix struct {
	Data [][]float64
	Rows int
	Cols int
}

func NewMatrix(data [][]float64) *Matrix {
	rows := len(data)
	cols := 0
	if rows > 0 {
		cols = len(data[0])
	}
	
	matrixData := make([][]float64, rows)
	for i := range matrixData {
		matrixData[i] = make([]float64, cols)
		copy(matrixData[i], data[i])
	}
	
	return &Matrix{Data: matrixData, Rows: rows, Cols: cols}
}

func (m *Matrix) String() string {
	result := ""
	for i, row := range m.Data {
		if i > 0 {
			result += "\n"
		}
		for j, val := range row {
			if j > 0 {
				result += " "
			}
			result += fmt.Sprintf("%.2f", val)
		}
	}
	return result
}

func (m *Matrix) Add(other *Matrix) (*Matrix, error) {
	if m.Rows != other.Rows || m.Cols != other.Cols {
		return nil, fmt.Errorf("matrices must have same dimensions")
	}
	
	result := make([][]float64, m.Rows)
	for i := range result {
		result[i] = make([]float64, m.Cols)
		for j := range result[i] {
			result[i][j] = m.Data[i][j] + other.Data[i][j]
		}
	}
	
	return NewMatrix(result), nil
}

func (m *Matrix) ScalarMultiply(scalar float64) *Matrix {
	result := make([][]float64, m.Rows)
	for i := range result {
		result[i] = make([]float64, m.Cols)
		for j := range result[i] {
			result[i][j] = m.Data[i][j] * scalar
		}
	}
	
	return NewMatrix(result)
}

func (m *Matrix) Multiply(other *Matrix) (*Matrix, error) {
	if m.Cols != other.Rows {
		return nil, fmt.Errorf("cannot multiply: incompatible dimensions")
	}
	
	result := make([][]float64, m.Rows)
	for i := range result {
		result[i] = make([]float64, other.Cols)
		for j := range result[i] {
			for k := 0; k < m.Cols; k++ {
				result[i][j] += m.Data[i][k] * other.Data[k][j]
			}
		}
	}
	
	return NewMatrix(result), nil
}

func (m *Matrix) Transpose() *Matrix {
	result := make([][]float64, m.Cols)
	for i := range result {
		result[i] = make([]float64, m.Rows)
		for j := range result[i] {
			result[i][j] = m.Data[j][i]
		}
	}
	
	return NewMatrix(result)
}

func (m *Matrix) Determinant() (float64, error) {
	if m.Rows != m.Cols {
		return 0, fmt.Errorf("determinant only defined for square matrices")
	}
	
	if m.Rows == 1 {
		return m.Data[0][0], nil
	}
	
	if m.Rows == 2 {
		return m.Data[0][0]*m.Data[1][1] - m.Data[0][1]*m.Data[1][0], nil
	}
	
	det := 0.0
	for j := 0; j < m.Cols; j++ {
		minor, err := m.getMinor(0, j)
		if err != nil {
			return 0, err
		}
		
		minorDet, err := minor.Determinant()
		if err != nil {
			return 0, err
		}
		
		cofactor := math.Pow(-1, float64(j)) * m.Data[0][j] * minorDet
		det += cofactor
	}
	
	return det, nil
}

func (m *Matrix) getMinor(row, col int) (*Matrix, error) {
	if m.Rows <= 1 || m.Cols <= 1 {
		return nil, fmt.Errorf("cannot get minor of matrix with dimension <= 1")
	}
	
	minorData := make([][]float64, 0)
	
	for i := 0; i < m.Rows; i++ {
		if i == row {
			continue
		}
		
		minorRow := make([]float64, 0)
		for j := 0; j < m.Cols; j++ {
			if j == col {
				continue
			}
			minorRow = append(minorRow, m.Data[i][j])
		}
		minorData = append(minorData, minorRow)
	}
	
	return NewMatrix(minorData), nil
}

func mathematicalDemo() {
	fmt.Println("=== Advanced Mathematical Objects Demo ===\n")
	
	fmt.Println("1. Complex Numbers:")
	c1 := NewComplexNumber(3, 4)
	c2 := NewComplexNumber(1, -2)
	fmt.Printf("   c1 = %s\n", c1.String())
	fmt.Printf("   c2 = %s\n", c2.String())
	fmt.Printf("   c1 + c2 = %s\n", c1.Add(c2).String())
	fmt.Printf("   c1 * c2 = %s\n", c1.Multiply(c2).String())
	fmt.Printf("   |c1| = %.2f\n", c1.Magnitude())
	fmt.Printf("   conjugate(c1) = %s\n", c1.Conjugate().String())
	
	fmt.Println("\n2. Vectors:")
	v1 := NewVector([]float64{1, 2, 3})
	v2 := NewVector([]float64{4, 5, 6})
	fmt.Printf("   v1 = %s\n", v1.String())
	fmt.Printf("   v2 = %s\n", v2.String())
	
	if sum, err := v1.Add(v2); err == nil {
		fmt.Printf("   v1 + v2 = %s\n", sum.String())
	}
	
	if dot, err := v1.DotProduct(v2); err == nil {
		fmt.Printf("   v1 · v2 = %.2f\n", dot)
	}
	
	fmt.Printf("   |v1| = %.2f\n", v1.Magnitude())
	
	if cross, err := v1.CrossProduct(v2); err == nil {
		fmt.Printf("   v1 × v2 = %s\n", cross.String())
	}
	
	fmt.Printf("   normalized v1 = %s\n", v1.Normalize().String())
	
	fmt.Println("\n3. Polynomials:")
	p1 := NewPolynomial([]float64{1, -2, 1}) // x^2 - 2x + 1
	p2 := NewPolynomial([]float64{2, 3})      // 3x + 2
	fmt.Printf("   p1 = %s\n", p1.String())
	fmt.Printf("   p2 = %s\n", p2.String())
	fmt.Printf("   p1 + p2 = %s\n", p1.Add(p2).String())
	fmt.Printf("   p1 * p2 = %s\n", p1.Multiply(p2).String())
	fmt.Printf("   p1(2) = %.2f\n", p1.Evaluate(2))
	fmt.Printf("   p1' = %s\n", p1.Derivative().String())
	
	fmt.Println("\n4. Fractions:")
	f1, _ := NewFraction(3, 4)
	f2, _ := NewFraction(2, 5)
	fmt.Printf("   f1 = %s\n", f1.String())
	fmt.Printf("   f2 = %s\n", f2.String())
	fmt.Printf("   f1 + f2 = %s\n", f1.Add(f2).String())
	fmt.Printf("   f1 * f2 = %s\n", f1.Multiply(f2).String())
	
	if div, err := f1.Divide(f2); err == nil {
		fmt.Printf("   f1 / f2 = %s\n", div.String())
	}
	
	fmt.Printf("   f1 as decimal = %.3f\n", f1.ToDecimal())
	
	fmt.Println("\n5. Matrices:")
	m1 := NewMatrix([][]float64{{1, 2}, {3, 4}})
	m2 := NewMatrix([][]float64{{5, 6}, {7, 8}})
	fmt.Printf("   m1 =\n%s\n", m1.String())
	fmt.Printf("   m2 =\n%s\n", m2.String())
	
	if sum, err := m1.Add(m2); err == nil {
		fmt.Printf("   m1 + m2 =\n%s\n", sum.String())
	}
	
	if product, err := m1.Multiply(m2); err == nil {
		fmt.Printf("   m1 * m2 =\n%s\n", product.String())
	}
	
	fmt.Printf("   m1 transposed =\n%s\n", m1.Transpose().String())
	
	if det, err := m1.Determinant(); err == nil {
		fmt.Printf("   det(m1) = %.2f\n", det)
	}
}

func main() {
	fmt.Println("=== Go Mathematical Objects Demo ===")
	mathematicalDemo()
}