package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type Person struct {
	Name  string `json:"name"`
	Age   int    `json:"age"`
	Email string `json:"email"`
}

type Company struct {
	Name      string   `json:"name"`
	Employees []Person `json:"employees"`
}

func main() {
	// Create sample data
	employees := []Person{
		{Name: "Alice Johnson", Age: 30, Email: "alice@company.com"},
		{Name: "Bob Smith", Age: 25, Email: "bob@company.com"},
		{Name: "Carol Brown", Age: 35, Email: "carol@company.com"},
	}

	company := Company{
		Name:      "Tech Solutions Inc",
		Employees: employees,
	}

	// Marshal to JSON
	jsonData, err := json.MarshalIndent(company, "", "  ")
	if err != nil {
		log.Fatalf("Error marshaling JSON: %v", err)
	}

	fmt.Println("Company JSON:")
	fmt.Println(string(jsonData))

	// Unmarshal from JSON
	var parsedCompany Company
	err = json.Unmarshal(jsonData, &parsedCompany)
	if err != nil {
		log.Fatalf("Error unmarshaling JSON: %v", err)
	}

	fmt.Printf("\nParsed company: %s\n", parsedCompany.Name)
	fmt.Printf("Number of employees: %d\n", len(parsedCompany.Employees))

	for i, emp := range parsedCompany.Employees {
		fmt.Printf("Employee %d: %s (Age: %d, Email: %s)\n", 
			i+1, emp.Name, emp.Age, emp.Email)
	}
}