package main

import (
	"fmt"
	"reflect"
)

type Person struct {
	Name    string
	Age     int
	Address *Address
}

type Address struct {
	Street string
	City   string
}

func main() {
	p := Person{
		Name: "John Doe",
		Age:  30,
		Address: &Address{
			Street: "123 Main St",
			City:   "New York",
		},
	}

	inspectType(p)
	inspectValue(p)

	p2 := Person{Name: "Jane Doe", Age: 25}
	if reflect.DeepEqual(p, p2) {
		fmt.Println("p and p2 are equal")
	} else {
		fmt.Println("p and p2 are not equal")
	}

	callMethodViaReflection()
}

func inspectType(v interface{}) {
	t := reflect.TypeOf(v)
	fmt.Printf("Type: %s\n", t.Name())
	fmt.Printf("Kind: %s\n", t.Kind())
	fmt.Printf("NumFields: %d\n", t.NumField())

	for i := 0; i < t.NumField(); i++ {
		field := t.Field(i)
		fmt.Printf("Field %d: %s (%s)\n", i, field.Name, field.Type)
	}
	fmt.Println()
}

func inspectValue(v interface{}) {
	val := reflect.ValueOf(v)
	fmt.Printf("Value: %v\n", val.Interface())

	if val.Kind() == reflect.Struct {
		fmt.Printf("Number of fields: %d\n", val.NumField())
		for i := 0; i < val.NumField(); i++ {
			fieldVal := val.Field(i)
			fmt.Printf("Field %d: %v (%s)\n", i, fieldVal.Interface(), fieldVal.Kind())
		}
	}
	fmt.Println()
}

func callMethodViaReflection() {
	var obj interface{} = &MyStruct{}
	val := reflect.ValueOf(obj)

	method := val.MethodByName("MyMethod")
	if method.IsValid() {
		args := []reflect.Value{reflect.ValueOf("Hello from reflection!")}
		result := method.Call(args)
		fmt.Printf("Method result: %v\n", result[0].Interface())
	}
}

type MyStruct struct{}

func (m *MyStruct) MyMethod(msg string) string {
	return "Received: " + msg
}