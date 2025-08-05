package main

import "fmt"

// Test all basic Go types and operations
func main() {
	// 1. Basic types
	var (
		intVar     int     = 42
		int8Var    int8    = 127
		int16Var   int16   = 32767
		int32Var   int32   = 2147483647
		int64Var   int64   = 9223372036854775807
		uintVar    uint    = 42
		uint8Var   uint8   = 255
		uint16Var  uint16  = 65535
		uint32Var  uint32  = 4294967295
		uint64Var  uint64  = 18446744073709551615
		float32Var float32 = 3.14159
		float64Var float64 = 3.141592653589793
		stringVar  string  = "Hello, World!"
		boolVar    bool    = true
		byteVar    byte    = 255
		runeVar    rune    = 'A'
	)

	// 2. Type inference
	inferredInt := 42
	inferredFloat := 3.14
	inferredString := "inferred"
	inferredBool := true

	// 3. Constants
	const (
		PI     = 3.14159
		MaxInt = 2147483647
	)

	// 4. Arrays and slices
	arr := [5]int{1, 2, 3, 4, 5}
	slice := []int{1, 2, 3, 4, 5}
	slice = append(slice, 6)

	// 5. Maps
	m := map[string]int{
		"one":   1,
		"two":   2,
		"three": 3,
	}

	// 6. Pointers
	x := 42
	p := &x
	*p = 43

	// Print some values to verify
	fmt.Printf("Basic types test - int: %d, string: %s, bool: %t\n", intVar, stringVar, boolVar)
	fmt.Printf("Type inference - int: %d, float: %f, string: %s, bool: %t\n", inferredInt, inferredFloat, inferredString, inferredBool)
	fmt.Printf("Arrays/slices - arr[0]: %d, slice len: %d\n", arr[0], len(slice))
	fmt.Printf("Maps - m['one']: %d\n", m["one"])
	fmt.Printf("Pointers - x: %d, *p: %d\n", x, *p)
	fmt.Printf("Constants - PI: %f, MaxInt: %d\n", PI, MaxInt)
}