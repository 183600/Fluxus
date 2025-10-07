package main

import (
	"encoding/json"
	"fmt"
	"reflect"
	"strconv"
	"strings"
	"time"
)

func main() {
	fmt.Println("=== Go Reflection and Struct Tags Tests ===")
	testBasicReflection()
	testStructTags()
	testJsonTags()
	testCustomTags()
	testDynamicTypeCreation()
	testMethodInvocation()
	testFieldManipulation()
	testTypeInspection()

	fmt.Println("\n=== All reflection and struct tags tests completed successfully! ===")
}

// Struct with various tags
type Person struct {
	Name     string `json:"name" xml:"name" db:"name" validate:"required"`
	Age      int    `json:"age" xml:"age" db:"age" validate:"gte=0"`
	Email    string `json:"email,omitempty" xml:"email" db:"email" validate:"email"`
	Password string `json:"-" xml:"-" db:"password_hash" validate:"min=8"`
	Created  time.Time `json:"created_at" db:"created_at"`
	Active   bool   `json:"active" db:"is_active" default:"true"`
}

type Product struct {
	ID          int     `json:"id" db:"id" validate:"required"`
	Name        string  `json:"name" db:"name" validate:"required,min=3,max=100"`
	Price       float64 `json:"price" db:"price" validate:"required,gte=0"`
	Description string  `json:"description,omitempty" db:"description"`
	Category    string  `json:"category" db:"category" default:"general"`
	SKU         string  `json:"sku" db:"sku" validate:"required"`
}

func testBasicReflection() {
	fmt.Println("\n--- Basic Reflection ---")

	person := Person{
		Name:     "Alice",
		Age:      25,
		Email:    "alice@example.com",
		Password: "secret123",
		Created:  time.Now(),
		Active:   true,
	}

	// Get type information
	t := reflect.TypeOf(person)
	fmt.Printf("Type: %s\n", t.Name())
	fmt.Printf("Kind: %s\n", t.Kind())
	fmt.Printf("Number of fields: %d\n", t.NumField())

	// Get value information
	v := reflect.ValueOf(person)
	fmt.Printf("Value kind: %s\n", v.Kind())

	// Iterate over fields
	for i := 0; i < t.NumField(); i++ {
		field := t.Field(i)
		value := v.Field(i)
		fmt.Printf("Field %d: %s (%s) = %v\n", i, field.Name, field.Type, value)
	}
}

func testStructTags() {
	fmt.Println("\n--- Struct Tags ---")

	t := reflect.TypeOf(Person{})

	// Get tags for each field
	for i := 0; i < t.NumField(); i++ {
		field := t.Field(i)
		tags := field.Tag

		fmt.Printf("Field: %s\n", field.Name)
		fmt.Printf("  JSON tag: %s\n", tags.Get("json"))
		fmt.Printf("  XML tag: %s\n", tags.Get("xml"))
		fmt.Printf("  DB tag: %s\n", tags.Get("db"))
		fmt.Printf("  Validate tag: %s\n", tags.Get("validate"))
		fmt.Printf("  Default tag: %s\n", tags.Get("default"))
		fmt.Println()
	}
}

func testJsonTags() {
	fmt.Println("\n--- JSON Tags ---")

	person := Person{
		Name:    "Bob",
		Age:     30,
		Email:   "bob@example.com",
		Created: time.Now(),
		Active:  true,
	}

	// Marshal to JSON
	jsonData, err := json.Marshal(person)
	if err != nil {
		fmt.Printf("Error marshaling to JSON: %v\n", err)
	} else {
		fmt.Printf("JSON: %s\n", jsonData)
	}

	// Unmarshal from JSON
	jsonStr := `{"name":"Charlie","age":35,"email":"charlie@example.com","created_at":"2023-01-01T00:00:00Z","active":false}`
	var newPerson Person
	err = json.Unmarshal([]byte(jsonStr), &newPerson)
	if err != nil {
		fmt.Printf("Error unmarshaling from JSON: %v\n", err)
	} else {
		fmt.Printf("Unmarshaled: %+v\n", newPerson)
	}
}

func testCustomTags() {
	fmt.Println("\n--- Custom Tags ---")

	product := Product{
		ID:          1,
		Name:        "Laptop",
		Price:       999.99,
		Description: "A powerful laptop",
		Category:    "Electronics",
		SKU:         "LAPTOP001",
	}

	// Parse custom validation tags
	validateStruct := func(v interface{}) []string {
		var errors []string
		val := reflect.ValueOf(v)
		typ := reflect.TypeOf(v)

		if typ.Kind() == reflect.Ptr {
			val = val.Elem()
			typ = typ.Elem()
		}

		for i := 0; i < typ.NumField(); i++ {
			field := typ.Field(i)
			fieldValue := val.Field(i)
			validateTag := field.Tag.Get("validate")

			if validateTag != "" {
				errors = append(errors, parseValidationTag(field.Name, fieldValue, validateTag)...)
			}
		}

		return errors
	}

	errors := validateStruct(product)
	if len(errors) == 0 {
		fmt.Println("Product validation passed!")
	} else {
		fmt.Println("Validation errors:")
		for _, err := range errors {
			fmt.Printf("  - %s\n", err)
		}
	}
}

func parseValidationTag(fieldName string, fieldValue reflect.Value, tag string) []string {
	var errors []string
	rules := strings.Split(tag, ",")

	for _, rule := range rules {
		if strings.Contains(rule, "=") {
			parts := strings.SplitN(rule, "=", 2)
			key := parts[0]
			value := parts[1]

			switch key {
			case "required":
				if fieldValue.IsZero() {
					errors = append(errors, fmt.Sprintf("%s is required", fieldName))
				}
			case "min":
				minValue, _ := strconv.Atoi(value)
				switch fieldValue.Kind() {
				case reflect.String:
					if fieldValue.Len() < minValue {
						errors = append(errors, fmt.Sprintf("%s must be at least %d characters", fieldName, minValue))
					}
				case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
					if fieldValue.Int() < int64(minValue) {
						errors = append(errors, fmt.Sprintf("%s must be at least %d", fieldName, minValue))
					}
				}
			case "max":
				maxValue, _ := strconv.Atoi(value)
				switch fieldValue.Kind() {
				case reflect.String:
					if fieldValue.Len() > maxValue {
						errors = append(errors, fmt.Sprintf("%s must be at most %d characters", fieldName, maxValue))
					}
				}
			case "gte":
				gteValue, _ := strconv.ParseFloat(value, 64)
				switch fieldValue.Kind() {
				case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
					if fieldValue.Int() < int64(gteValue) {
						errors = append(errors, fmt.Sprintf("%s must be greater than or equal to %d", fieldName, int(gteValue)))
					}
				case reflect.Float32, reflect.Float64:
					if fieldValue.Float() < gteValue {
						errors = append(errors, fmt.Sprintf("%s must be greater than or equal to %f", fieldName, gteValue))
					}
				}
			case "email":
				if !strings.Contains(fieldValue.String(), "@") {
					errors = append(errors, fmt.Sprintf("%s must be a valid email", fieldName))
				}
			}
		}
	}

	return errors
}

func testDynamicTypeCreation() {
	fmt.Println("\n--- Dynamic Type Creation ---")

	// Create a slice dynamically
	sliceType := reflect.SliceOf(reflect.TypeOf(""))
	sliceValue := reflect.MakeSlice(sliceType, 0, 5)

	// Append values
	sliceValue = reflect.Append(sliceValue, reflect.ValueOf("hello"))
	sliceValue = reflect.Append(sliceValue, reflect.ValueOf("world"))

	fmt.Printf("Dynamic slice: %v\n", sliceValue.Interface())

	// Create a map dynamically
	mapType := reflect.MapOf(reflect.TypeOf(""), reflect.TypeOf(int(0)))
	mapValue := reflect.MakeMap(mapType)

	// Set values
	mapValue.SetMapIndex(reflect.ValueOf("one"), reflect.ValueOf(1))
	mapValue.SetMapIndex(reflect.ValueOf("two"), reflect.ValueOf(2))

	fmt.Printf("Dynamic map: %v\n", mapValue.Interface())

	// Create a struct dynamically
	// Note: This is more complex and typically uses unsafe or code generation
	fmt.Println("Struct creation typically requires more complex reflection or code generation")
}

func testMethodInvocation() {
	fmt.Println("\n--- Method Invocation ---")

	person := Person{Name: "Dave", Age: 40}

	// Call a method using reflection
	val := reflect.ValueOf(person)
	method := val.MethodByName("String")
	if method.IsValid() {
		result := method.Call([]reflect.Value{})
		fmt.Printf("String() method result: %s\n", result[0].String())
	}

	// Call method with parameters
	add := func(a, b int) int { return a + b }
	addVal := reflect.ValueOf(add)
	if addVal.IsValid() {
		args := []reflect.Value{reflect.ValueOf(5), reflect.ValueOf(3)}
		result := addVal.Call(args)
		fmt.Printf("Add function result: %d\n", result[0].Int())
	}
}

func testFieldManipulation() {
	fmt.Println("\n--- Field Manipulation ---")

	person := Person{Name: "Eve", Age: 28}

	// Get field by name
	val := reflect.ValueOf(&person).Elem() // Use pointer to modify
	nameField := val.FieldByName("Name")
	if nameField.IsValid() && nameField.CanSet() {
		nameField.SetString("Eve Smith")
		fmt.Printf("Modified name: %s\n", person.Name)
	}

	// Get field by index
	ageField := val.Field(1) // Age is at index 1
	if ageField.IsValid() && ageField.CanSet() {
		ageField.SetInt(29)
		fmt.Printf("Modified age: %d\n", person.Age)
	}

	// Get and set slice elements
	slice := []string{"a", "b", "c"}
	sliceVal := reflect.ValueOf(&slice).Elem()

	// Modify an element
	if sliceVal.Len() > 0 {
		firstElement := sliceVal.Index(0)
		if firstElement.CanSet() {
			firstElement.SetString("x")
			fmt.Printf("Modified slice: %v\n", slice)
		}
	}
}

func testTypeInspection() {
	fmt.Println("\n--- Type Inspection ---")

	// Inspect various types
	types := []interface{}{
		42,
		"hello",
		3.14,
		[]int{1, 2, 3},
		map[string]int{"a": 1},
		Person{Name: "Frank"},
		&Person{Name: "Grace"},
	}

	for _, item := range types {
		val := reflect.ValueOf(item)
		typ := val.Type()

		fmt.Printf("Value: %v\n", item)
		fmt.Printf("  Type: %s\n", typ)
		fmt.Printf("  Kind: %s\n", typ.Kind())
		fmt.Printf("  Is pointer: %t\n", typ.Kind() == reflect.Ptr)

		if typ.Kind() == reflect.Ptr {
			fmt.Printf("  Element type: %s\n", typ.Elem())
		}

		if typ.Kind() == reflect.Slice || typ.Kind() == reflect.Array {
			fmt.Printf("  Element type: %s\n", typ.Elem())
			fmt.Printf("  Length: %d\n", val.Len())
		}

		if typ.Kind() == reflect.Map {
			fmt.Printf("  Key type: %s\n", typ.Key())
			fmt.Printf("  Value type: %s\n", typ.Elem())
			fmt.Printf("  Length: %d\n", val.Len())
		}

		fmt.Println()
	}
}

// String method for Person to test method invocation
func (p Person) String() string {
	return fmt.Sprintf("Person{Name: %s, Age: %d}", p.Name, p.Age)
}