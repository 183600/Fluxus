class ComplexNumber:
    def __init__(self, real=0, imag=0):
        self.real = real
        self.imag = imag
    
    def __str__(self):
        if self.imag >= 0:
            return f"{self.real} + {self.imag}i"
        else:
            return f"{self.real} - {abs(self.imag)}i"
    
    def __add__(self, other):
        if isinstance(other, ComplexNumber):
            return ComplexNumber(self.real + other.real, self.imag + other.imag)
        else:
            return ComplexNumber(self.real + other, self.imag)
    
    def __sub__(self, other):
        if isinstance(other, ComplexNumber):
            return ComplexNumber(self.real - other.real, self.imag - other.imag)
        else:
            return ComplexNumber(self.real - other, self.imag)
    
    def __mul__(self, other):
        if isinstance(other, ComplexNumber):
            real = self.real * other.real - self.imag * other.imag
            imag = self.real * other.imag + self.imag * other.real
            return ComplexNumber(real, imag)
        else:
            return ComplexNumber(self.real * other, self.imag * other)
    
    def conjugate(self):
        return ComplexNumber(self.real, -self.imag)
    
    def magnitude(self):
        return (self.real ** 2 + self.imag ** 2) ** 0.5

class Vector:
    def __init__(self, components):
        self.components = list(components)
        self.dimension = len(self.components)
    
    def __str__(self):
        return f"Vector({self.components})"
    
    def __add__(self, other):
        if self.dimension != other.dimension:
            raise ValueError("Vectors must have same dimension")
        return Vector([a + b for a, b in zip(self.components, other.components)])
    
    def __sub__(self, other):
        if self.dimension != other.dimension:
            raise ValueError("Vectors must have same dimension")
        return Vector([a - b for a, b in zip(self.components, other.components)])
    
    def __mul__(self, scalar):
        return Vector([component * scalar for component in self.components])
    
    def dot_product(self, other):
        if self.dimension != other.dimension:
            raise ValueError("Vectors must have same dimension")
        return sum(a * b for a, b in zip(self.components, other.components))
    
    def magnitude(self):
        return sum(component ** 2 for component in self.components) ** 0.5
    
    def normalize(self):
        mag = self.magnitude()
        if mag == 0:
            return Vector([0] * self.dimension)
        return Vector([component / mag for component in self.components])
    
    def cross_product(self, other):
        if self.dimension != 3 or other.dimension != 3:
            raise ValueError("Cross product only defined for 3D vectors")
        
        a, b, c = self.components
        x, y, z = other.components
        
        return Vector([
            b * z - c * y,
            c * x - a * z,
            a * y - b * x
        ])

class Polynomial:
    def __init__(self, coefficients):
        self.coefficients = coefficients
        self._remove_leading_zeros()
    
    def _remove_leading_zeros(self):
        while len(self.coefficients) > 1 and self.coefficients[-1] == 0:
            self.coefficients.pop()
    
    def __str__(self):
        if not self.coefficients:
            return "0"
        
        terms = []
        degree = len(self.coefficients) - 1
        
        for i, coeff in enumerate(reversed(self.coefficients)):
            if coeff == 0:
                continue
            
            power = degree - i
            
            if power == 0:
                terms.append(str(coeff))
            elif power == 1:
                if coeff == 1:
                    terms.append("x")
                elif coeff == -1:
                    terms.append("-x")
                else:
                    terms.append(f"{coeff}x")
            else:
                if coeff == 1:
                    terms.append(f"x^{power}")
                elif coeff == -1:
                    terms.append(f"-x^{power}")
                else:
                    terms.append(f"{coeff}x^{power}")
        
        if not terms:
            return "0"
        
        result = terms[0]
        for term in terms[1:]:
            if term.startswith('-'):
                result += f" - {term[1:]}"
            else:
                result += f" + {term}"
        
        return result
    
    def __add__(self, other):
        max_len = max(len(self.coefficients), len(other.coefficients))
        result = []
        
        for i in range(max_len):
            a = self.coefficients[i] if i < len(self.coefficients) else 0
            b = other.coefficients[i] if i < len(other.coefficients) else 0
            result.append(a + b)
        
        return Polynomial(result)
    
    def __mul__(self, other):
        if isinstance(other, (int, float)):
            return Polynomial([coeff * other for coeff in self.coefficients])
        
        result_degree = len(self.coefficients) + len(other.coefficients) - 1
        result = [0] * result_degree
        
        for i, a in enumerate(self.coefficients):
            for j, b in enumerate(other.coefficients):
                result[i + j] += a * b
        
        return Polynomial(result)
    
    def evaluate(self, x):
        result = 0
        for i, coeff in enumerate(self.coefficients):
            result += coeff * (x ** i)
        return result
    
    def derivative(self):
        if len(self.coefficients) <= 1:
            return Polynomial([0])
        
        result = []
        for i in range(1, len(self.coefficients)):
            result.append(i * self.coefficients[i])
        
        return Polynomial(result)

class Fraction:
    def __init__(self, numerator, denominator):
        if denominator == 0:
            raise ValueError("Denominator cannot be zero")
        
        self.numerator = numerator
        self.denominator = denominator
        self._simplify()
    
    def _gcd(self, a, b):
        while b:
            a, b = b, a % b
        return a
    
    def _simplify(self):
        gcd = self._gcd(abs(self.numerator), abs(self.denominator))
        self.numerator //= gcd
        self.denominator //= gcd
        
        if self.denominator < 0:
            self.numerator = -self.numerator
            self.denominator = -self.denominator
    
    def __str__(self):
        if self.denominator == 1:
            return str(self.numerator)
        return f"{self.numerator}/{self.denominator}"
    
    def __add__(self, other):
        if isinstance(other, Fraction):
            num = self.numerator * other.denominator + other.numerator * self.denominator
            den = self.denominator * other.denominator
            return Fraction(num, den)
        else:
            return Fraction(self.numerator + other * self.denominator, self.denominator)
    
    def __sub__(self, other):
        if isinstance(other, Fraction):
            num = self.numerator * other.denominator - other.numerator * self.denominator
            den = self.denominator * other.denominator
            return Fraction(num, den)
        else:
            return Fraction(self.numerator - other * self.denominator, self.denominator)
    
    def __mul__(self, other):
        if isinstance(other, Fraction):
            return Fraction(self.numerator * other.numerator, self.denominator * other.denominator)
        else:
            return Fraction(self.numerator * other, self.denominator)
    
    def __truediv__(self, other):
        if isinstance(other, Fraction):
            return Fraction(self.numerator * other.denominator, self.denominator * other.numerator)
        else:
            return Fraction(self.numerator, self.denominator * other)
    
    def __eq__(self, other):
        if isinstance(other, Fraction):
            return self.numerator * other.denominator == other.numerator * self.denominator
        else:
            return self.numerator == other * self.denominator
    
    def to_decimal(self):
        return self.numerator / self.denominator

class Matrix:
    def __init__(self, data):
        self.data = [row[:] for row in data]
        self.rows = len(data)
        self.cols = len(data[0]) if data else 0
    
    def __str__(self):
        return '\n'.join([' '.join(map(str, row)) for row in self.data])
    
    def __add__(self, other):
        if self.rows != other.rows or self.cols != other.cols:
            raise ValueError("Matrices must have same dimensions")
        
        result = []
        for i in range(self.rows):
            row = []
            for j in range(self.cols):
                row.append(self.data[i][j] + other.data[i][j])
            result.append(row)
        
        return Matrix(result)
    
    def __mul__(self, other):
        if isinstance(other, (int, float)):
            result = []
            for i in range(self.rows):
                row = []
                for j in range(self.cols):
                    row.append(self.data[i][j] * other)
                result.append(row)
            return Matrix(result)
        
        if isinstance(other, Matrix):
            if self.cols != other.rows:
                raise ValueError("Cannot multiply: incompatible dimensions")
            
            result = []
            for i in range(self.rows):
                row = []
                for j in range(other.cols):
                    sum_val = 0
                    for k in range(self.cols):
                        sum_val += self.data[i][k] * other.data[k][j]
                    row.append(sum_val)
                result.append(row)
            
            return Matrix(result)
    
    def transpose(self):
        result = []
        for j in range(self.cols):
            row = []
            for i in range(self.rows):
                row.append(self.data[i][j])
            result.append(row)
        
        return Matrix(result)
    
    def determinant(self):
        if self.rows != self.cols:
            raise ValueError("Determinant only defined for square matrices")
        
        if self.rows == 1:
            return self.data[0][0]
        
        if self.rows == 2:
            return self.data[0][0] * self.data[1][1] - self.data[0][1] * self.data[1][0]
        
        det = 0
        for j in range(self.cols):
            minor = self._get_minor(0, j)
            cofactor = ((-1) ** j) * self.data[0][j] * minor.determinant()
            det += cofactor
        
        return det
    
    def _get_minor(self, row, col):
        minor_data = []
        for i in range(self.rows):
            if i == row:
                continue
            minor_row = []
            for j in range(self.cols):
                if j == col:
                    continue
                minor_row.append(self.data[i][j])
            minor_data.append(minor_row)
        
        return Matrix(minor_data)

def mathematical_demo():
    print("=== Advanced Mathematical Objects Demo ===\n")
    
    print("1. Complex Numbers:")
    c1 = ComplexNumber(3, 4)
    c2 = ComplexNumber(1, -2)
    print(f"   c1 = {c1}")
    print(f"   c2 = {c2}")
    print(f"   c1 + c2 = {c1 + c2}")
    print(f"   c1 * c2 = {c1 * c2}")
    print(f"   |c1| = {c1.magnitude():.2f}")
    print(f"   conjugate(c1) = {c1.conjugate()}")
    
    print("\n2. Vectors:")
    v1 = Vector([1, 2, 3])
    v2 = Vector([4, 5, 6])
    print(f"   v1 = {v1}")
    print(f"   v2 = {v2}")
    print(f"   v1 + v2 = {v1 + v2}")
    print(f"   v1 · v2 = {v1.dot_product(v2)}")
    print(f"   |v1| = {v1.magnitude():.2f}")
    print(f"   v1 × v2 = {v1.cross_product(v2)}")
    print(f"   normalized v1 = {v1.normalize()}")
    
    print("\n3. Polynomials:")
    p1 = Polynomial([1, -2, 1])  # x^2 - 2x + 1
    p2 = Polynomial([2, 3])      # 3x + 2
    print(f"   p1 = {p1}")
    print(f"   p2 = {p2}")
    print(f"   p1 + p2 = {p1 + p2}")
    print(f"   p1 * p2 = {p1 * p2}")
    print(f"   p1(2) = {p1.evaluate(2)}")
    print(f"   p1' = {p1.derivative()}")
    
    print("\n4. Fractions:")
    f1 = Fraction(3, 4)
    f2 = Fraction(2, 5)
    print(f"   f1 = {f1}")
    print(f"   f2 = {f2}")
    print(f"   f1 + f2 = {f1 + f2}")
    print(f"   f1 * f2 = {f1 * f2}")
    print(f"   f1 / f2 = {f1 / f2}")
    print(f"   f1 as decimal = {f1.to_decimal():.3f}")
    
    print("\n5. Matrices:")
    m1 = Matrix([[1, 2], [3, 4]])
    m2 = Matrix([[5, 6], [7, 8]])
    print(f"   m1 =\n{m1}")
    print(f"   m2 =\n{m2}")
    print(f"   m1 + m2 =\n{m1 + m2}")
    print(f"   m1 * m2 =\n{m1 * m2}")
    print(f"   m1 transposed =\n{m1.transpose()}")
    print(f"   det(m1) = {m1.determinant()}")

if __name__ == "__main__":
    mathematical_demo()