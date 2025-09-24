package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math"
	"net/http"
	"net/url"
	"os"
	"regexp"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"syscall"
	"time"
)

func main() {
	fmt.Println("=== Go Basic Types and Operators Tests ===")
	testBasicTypes()
	testOperators()
	testTypeConversions()
	testConstantsAndVariables()
	testPointersBasic()
	fmt.Println("\n=== All basic types and operators tests completed successfully! ===")
}

func testBasicTypes() {
	fmt.Println("\n--- Basic Types ---")

	// Integer types
	var intVar int = 42
	var int8Var int8 = 127
	var int16Var int16 = 32767
	var int32Var int32 = 2147483647
	var int64Var int64 = 9223372036854775807

	var uintVar uint = 42
	var uint8Var uint8 = 255
	var uint16Var uint16 = 65535
	var uint32Var uint32 = 4294967295
	var uint64Var uint64 = 18446744073709551615

	// Floating point types
	var float32Var float32 = 3.1415926535
	var float64Var float64 = 3.141592653589793

	// Complex types
	var complex64Var complex64 = 3 + 4i
	var complex128Var complex128 = 1 + 2i

	// Other basic types
	var boolVar bool = true
	var stringVar string = "Hello, Go!"
	var runeVar rune = 'A'
	var byteVar byte = 'B'

	fmt.Printf("int: %d\n", intVar)
	fmt.Printf("int8: %d\n", int8Var)
	fmt.Printf("int16: %d\n", int16Var)
	fmt.Printf("int32: %d\n", int32Var)
	fmt.Printf("int64: %d\n", int64Var)
	fmt.Printf("uint: %d\n", uintVar)
	fmt.Printf("uint8: %d\n", uint8Var)
	fmt.Printf("uint16: %d\n", uint16Var)
	fmt.Printf("uint32: %d\n", uint32Var)
	fmt.Printf("uint64: %d\n", uint64Var)
	fmt.Printf("float32: %f\n", float32Var)
	fmt.Printf("float64: %f\n", float64Var)
	fmt.Printf("complex64: %v\n", complex64Var)
	fmt.Printf("complex128: %v\n", complex128Var)
	fmt.Printf("bool: %t\n", boolVar)
	fmt.Printf("string: %s\n", stringVar)
	fmt.Printf("rune: %c\n", runeVar)
	fmt.Printf("byte: %c\n", byteVar)
}

func testOperators() {
	fmt.Println("\n--- Operators ---")

	// Arithmetic operators
	a, b := 10, 3
	fmt.Printf("Arithmetic: %d + %d = %d\n", a, b, a+b)
	fmt.Printf("Arithmetic: %d - %d = %d\n", a, b, a-b)
	fmt.Printf("Arithmetic: %d * %d = %d\n", a, b, a*b)
	fmt.Printf("Arithmetic: %d / %d = %d\n", a, b, a/b)
	fmt.Printf("Arithmetic: %d %% %d = %d\n", a, b, a%b)

	// Comparison operators
	fmt.Printf("Comparison: %d == %d = %t\n", a, b, a == b)
	fmt.Printf("Comparison: %d != %d = %t\n", a, b, a != b)
	fmt.Printf("Comparison: %d < %d = %t\n", a, b, a < b)
	fmt.Printf("Comparison: %d > %d = %t\n", a, b, a > b)
	fmt.Printf("Comparison: %d <= %d = %t\n", a, b, a <= b)
	fmt.Printf("Comparison: %d >= %d = %t\n", a, b, a >= b)

	// Logical operators
	x, y := true, false
	fmt.Printf("Logical: %t && %t = %t\n", x, y, x && y)
	fmt.Printf("Logical: %t || %t = %t\n", x, y, x || y)
	fmt.Printf("Logical: !%t = %t\n", x, !x)

	// Bitwise operators
	fmt.Printf("Bitwise: %d & %d = %d\n", a, b, a&b)
	fmt.Printf("Bitwise: %d | %d = %d\n", a, b, a|b)
	fmt.Printf("Bitwise: %d ^ %d = %d\n", a, b, a^b)
	fmt.Printf("Bitwise: %d << 1 = %d\n", a, a<<1)
	fmt.Printf("Bitwise: %d >> 1 = %d\n", a, a>>1)
	fmt.Printf("Bitwise: ^%d = %d\n", a, ^a)

	// Assignment operators
	c := a
	c += b
	fmt.Printf("Assignment: += %d\n", c)
	c = a
	c -= b
	fmt.Printf("Assignment: -= %d\n", c)
	c = a
	c *= b
	fmt.Printf("Assignment: *= %d\n", c)
	c = a
	c /= b
	fmt.Printf("Assignment: /= %d\n", c)
	c = a
	c %= b
	fmt.Printf("Assignment: %%= %d\n", c)
}

func testTypeConversions() {
	fmt.Println("\n--- Type Conversions ---")

	// Numeric conversions
	var i int = 42
	var f float64 = 3.14
	var s string = "123"

	fmt.Printf("int to float64: %f\n", float64(i))
	fmt.Printf("float64 to int: %d\n", int(f))
	fmt.Printf("string to int: %d\n", strconv.Atoi(s))

	// String conversions
	fmt.Printf("int to string: %s\n", strconv.Itoa(i))
	fmt.Printf("bool to string: %s\n", strconv.FormatBool(true))
	fmt.Printf("float64 to string: %s\n", strconv.FormatFloat(f, 'f', 2, 64))

	// Type assertions
	var any interface{} = "hello"
	if str, ok := any.(string); ok {
		fmt.Printf("Type assertion: %s\n", str)
	}

	// Type switches
	var another interface{} = 42
	switch v := another.(type) {
	case int:
		fmt.Printf("Type switch: int %d\n", v)
	case string:
		fmt.Printf("Type switch: string %s\n", v)
	default:
		fmt.Printf("Type switch: unknown type %T\n", v)
	}
}

func testConstantsAndVariables() {
	fmt.Println("\n--- Constants and Variables ---")

	// Constants
	const PI = 3.141592653589793
	const MaxInt = int64(1<<63 - 1)
	const Language = "Go"

	fmt.Printf("Constants: PI=%f, MaxInt=%d, Language=%s\n", PI, MaxInt, Language)

	// Variables with different declaration styles
	var a int = 10
	var b = 20
	c := 30
	var (
		d int     = 40
		e string  = "hello"
		f float64 = 3.14
	)

	fmt.Printf("Variables: a=%d, b=%d, c=%d\n", a, b, c)
	fmt.Printf("Multiple: d=%d, e=%s, f=%f\n", d, e, f)

	// Iota for constants
	const (
		_ = iota
		Monday
		Tuesday
		Wednesday
		Thursday
		Friday
		Saturday
		Sunday
	)

	fmt.Printf("Iota: Monday=%d, Tuesday=%d, Wednesday=%d\n", Monday, Tuesday, Wednesday)
}

func testPointersBasic() {
	fmt.Println("\n--- Basic Pointers ---")

	// Pointer declaration and dereferencing
	x := 42
	p := &x

	fmt.Printf("x = %d\n", x)
	fmt.Printf("p = %p\n", p)
	fmt.Printf("*p = %d\n", *p)

	// Pointer modification
	*p = 100
	fmt.Printf("After modification: x = %d\n", x)

	// Pointer to pointer
	pp := &p
	fmt.Printf("pp = %p\n", pp)
	fmt.Printf("*pp = %p\n", *pp)
	fmt.Printf("**pp = %d\n", **pp)

	// Nil pointers
	var nilPointer *int
	fmt.Printf("nil pointer: %p\n", nilPointer)
	if nilPointer == nil {
		fmt.Println("Pointer is nil")
	}

	// Pointer with structs
	type Person struct {
		Name string
		Age  int
	}

	person := Person{"Alice", 25}
	personPtr := &person
	fmt.Printf("Person pointer: %+v\n", personPtr)
	fmt.Printf("Name via pointer: %s\n", personPtr.Name)

	// Pointer methods
	personPtr.updateAge(26)
	fmt.Printf("Updated age: %d\n", person.Age)
}

// Method with pointer receiver
func (p *Person) updateAge(newAge int) {
	p.Age = newAge
}