package main

import (
	"encoding/csv"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
)

type Product struct {
	ID       int     `json:"id"`
	Name     string  `json:"name"`
	Price    float64 `json:"price"`
	Category string  `json:"category"`
}

func main() {
	createTestDirectory()
	writeTextFile()
	writeJSONFile()
	writeCSVFile()
	readFiles()
	listFiles()
}

func createTestDirectory() {
	err := os.MkdirAll("test_data", 0755)
	if err != nil {
		fmt.Println("Error creating directory:", err)
		return
	}
	fmt.Println("Directory created successfully")
}

func writeTextFile() {
	data := "This is a test file.\nIt contains multiple lines.\nCreated by Go file operations."

	err := ioutil.WriteFile("test_data/sample.txt", []byte(data), 0644)
	if err != nil {
		fmt.Println("Error writing text file:", err)
		return
	}
	fmt.Println("Text file written successfully")
}

func writeJSONFile() {
	products := []Product{
		{ID: 1, Name: "Laptop", Price: 999.99, Category: "Electronics"},
		{ID: 2, Name: "Book", Price: 19.99, Category: "Books"},
		{ID: 3, Name: "Coffee", Price: 4.99, Category: "Food"},
	}

	jsonData, err := json.MarshalIndent(products, "", "  ")
	if err != nil {
		fmt.Println("Error marshaling JSON:", err)
		return
	}

	err = ioutil.WriteFile("test_data/products.json", jsonData, 0644)
	if err != nil {
		fmt.Println("Error writing JSON file:", err)
		return
	}
	fmt.Println("JSON file written successfully")
}

func writeCSVFile() {
	file, err := os.Create("test_data/data.csv")
	if err != nil {
		fmt.Println("Error creating CSV file:", err)
		return
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	headers := []string{"ID", "Name", "Price", "Category"}
	writer.Write(headers)

	records := [][]string{
		{"1", "Laptop", "999.99", "Electronics"},
		{"2", "Book", "19.99", "Books"},
		{"3", "Coffee", "4.99", "Food"},
	}

	for _, record := range records {
		writer.Write(record)
	}

	fmt.Println("CSV file written successfully")
}

func readFiles() {
	fmt.Println("\nReading files:")

	textContent, err := ioutil.ReadFile("test_data/sample.txt")
	if err != nil {
		fmt.Println("Error reading text file:", err)
	} else {
		fmt.Println("Text content:", string(textContent))
	}

	jsonContent, err := ioutil.ReadFile("test_data/products.json")
	if err != nil {
		fmt.Println("Error reading JSON file:", err)
	} else {
		fmt.Println("JSON content:", string(jsonContent))
	}
}

func listFiles() {
	fmt.Println("\nFiles in test_data directory:")
	files, err := ioutil.ReadDir("test_data")
	if err != nil {
		fmt.Println("Error reading directory:", err)
		return
	}

	for _, file := range files {
		fmt.Printf(" - %s (%d bytes)\n", file.Name(), file.Size())
	}

	cleanup()
}

func cleanup() {
	fmt.Println("\nCleaning up test files...")
	err := os.RemoveAll("test_data")
	if err != nil {
		fmt.Println("Error cleaning up:", err)
	} else {
		fmt.Println("Cleanup completed")
	}
}