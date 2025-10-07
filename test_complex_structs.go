package main

// Test various struct declarations
type SimpleStruct struct {
    Name string
    Age int
}

type StructWithTags struct {
    Name string `json:"name"`
    Age int `json:"age"`
}

type StructWithAnonymousField struct {
    string
    Name string
}

type StructWithPointer struct {
    Name *string
    Age *int
}

type NestedStruct struct {
    Person struct {
        Name string
        Age int
    }
    Address struct {
        Street string
        City string
    }
}

type StructWithMultipleFields struct {
    Name, Email string
    Age, Score int
}