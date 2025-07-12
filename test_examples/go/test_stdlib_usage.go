package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math"
	"math/rand"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

// TestResult represents the result of a test function
type TestResult struct {
	Name    string      `json:"name"`
	Success bool        `json:"success"`
	Data    interface{} `json:"data,omitempty"`
	Error   string      `json:"error,omitempty"`
}

// Test basic Go built-in functions and types
func testBuiltins() TestResult {
	slice := []int{1, 2, 3, 4, 5}
	
	// Test slice operations
	result := map[string]interface{}{
		"len":    len(slice),
		"cap":    cap(slice),
		"append": append(slice, 6),
		"copy": func() []int {
			dest := make([]int, len(slice))
			copy(dest, slice)
			return dest
		}(),
	}
	
	// Test map operations
	m := make(map[string]int)
	m["key1"] = 10
	m["key2"] = 20
	result["map_len"] = len(m)
	result["map_values"] = m
	
	// Test channel operations
	ch := make(chan int, 2)
	ch <- 1
	ch <- 2
	close(ch)
	
	var channelValues []int
	for v := range ch {
		channelValues = append(channelValues, v)
	}
	result["channel_values"] = channelValues
	
	return TestResult{
		Name:    "Built-ins",
		Success: true,
		Data:    result,
	}
}

// Test string operations
func testStringOperations() TestResult {
	text := "Hello, World!"
	
	result := map[string]interface{}{
		"upper":       strings.ToUpper(text),
		"lower":       strings.ToLower(text),
		"replace":     strings.Replace(text, "World", "Go", 1),
		"contains":    strings.Contains(text, "World"),
		"split":       strings.Split(text, ","),
		"join":        strings.Join([]string{"a", "b", "c"}, "-"),
		"trim":        strings.TrimSpace("  spaces  "),
		"has_prefix":  strings.HasPrefix(text, "Hello"),
		"has_suffix":  strings.HasSuffix(text, "!"),
		"repeat":      strings.Repeat("Go", 3),
	}
	
	return TestResult{
		Name:    "String Operations",
		Success: true,
		Data:    result,
	}
}

// Test fmt package
func testFmtPackage() TestResult {
	name := "Go"
	age := 15
	
	result := map[string]interface{}{
		"sprintf":  fmt.Sprintf("Hello, %s! You are %d years old.", name, age),
		"sprint":   fmt.Sprint("Hello", " ", "World"),
		"sprintln": strings.TrimSuffix(fmt.Sprintln("Line", "with", "newline"), "\n"),
	}
	
	return TestResult{
		Name:    "Fmt Package",
		Success: true,
		Data:    result,
	}
}

// Test math package
func testMathPackage() TestResult {
	result := map[string]interface{}{
		"pi":       math.Pi,
		"e":        math.E,
		"sqrt":     math.Sqrt(16),
		"pow":      math.Pow(2, 3),
		"sin":      math.Sin(math.Pi / 2),
		"cos":      math.Cos(0),
		"log":      math.Log(math.E),
		"ceil":     math.Ceil(4.2),
		"floor":    math.Floor(4.8),
		"abs":      math.Abs(-5.5),
		"max":      math.Max(10, 20),
		"min":      math.Min(10, 20),
		"mod":      math.Mod(7, 3),
		"round":    math.Round(4.7),
		"trunc":    math.Trunc(4.7),
	}
	
	return TestResult{
		Name:    "Math Package",
		Success: true,
		Data:    result,
	}
}

// Test time package
func testTimePackage() TestResult {
	now := time.Now()
	
	// Create a specific time for consistent testing
	specificTime := time.Date(2024, 12, 25, 15, 30, 45, 0, time.UTC)
	
	result := map[string]interface{}{
		"now_year":       now.Year(),
		"now_month":      int(now.Month()),
		"now_day":        now.Day(),
		"format":         specificTime.Format("2006-01-02 15:04:05"),
		"unix":           specificTime.Unix(),
		"weekday":        specificTime.Weekday().String(),
		"add_duration":   specificTime.Add(24 * time.Hour).Format("2006-01-02"),
		"since_specific": time.Since(specificTime) > 0,
		"parse_test": func() bool {
			parsed, err := time.Parse("2006-01-02", "2024-12-25")
			return err == nil && parsed.Year() == 2024
		}(),
	}
	
	return TestResult{
		Name:    "Time Package",
		Success: true,
		Data:    result,
	}
}

// Test rand package
func testRandPackage() TestResult {
	rand.Seed(42) // For reproducible results
	
	slice := []string{"apple", "banana", "cherry", "date"}
	rand.Shuffle(len(slice), func(i, j int) {
		slice[i], slice[j] = slice[j], slice[i]
	})
	
	result := map[string]interface{}{
		"intn":       rand.Intn(100),
		"float64":    rand.Float64(),
		"perm":       rand.Perm(5),
		"shuffled":   slice,
		"seed_test":  rand.Intn(1000),
	}
	
	return TestResult{
		Name:    "Rand Package",
		Success: true,
		Data:    result,
	}
}

// Test sort package
func testSortPackage() TestResult {
	ints := []int{3, 1, 4, 1, 5, 9, 2, 6}
	strings := []string{"banana", "apple", "cherry", "date"}
	
	sort.Ints(ints)
	sort.Strings(strings)
	
	// Test custom sort
	people := []struct {
		Name string
		Age  int
	}{
		{"Alice", 30},
		{"Bob", 25},
		{"Charlie", 35},
	}
	
	sort.Slice(people, func(i, j int) bool {
		return people[i].Age < people[j].Age
	})
	
	result := map[string]interface{}{
		"sorted_ints":    ints,
		"sorted_strings": strings,
		"is_sorted":      sort.IntsAreSorted(ints),
		"search_result":  sort.SearchInts(ints, 5),
		"custom_sort":    people,
	}
	
	return TestResult{
		Name:    "Sort Package",
		Success: true,
		Data:    result,
	}
}

// Test strconv package
func testStrconvPackage() TestResult {
	result := map[string]interface{}{}
	
	// String to int
	if i, err := strconv.Atoi("42"); err == nil {
		result["atoi"] = i
	}
	
	// Int to string
	result["itoa"] = strconv.Itoa(42)
	
	// String to float
	if f, err := strconv.ParseFloat("3.14", 64); err == nil {
		result["parse_float"] = f
	}
	
	// Bool operations
	if b, err := strconv.ParseBool("true"); err == nil {
		result["parse_bool"] = b
	}
	result["format_bool"] = strconv.FormatBool(true)
	
	// Base conversion
	result["format_int_base2"] = strconv.FormatInt(42, 2)
	result["format_int_base16"] = strconv.FormatInt(42, 16)
	
	if i, err := strconv.ParseInt("101010", 2, 64); err == nil {
		result["parse_int_base2"] = i
	}
	
	return TestResult{
		Name:    "Strconv Package",
		Success: true,
		Data:    result,
	}
}

// Test os package
func testOsPackage() TestResult {
	result := map[string]interface{}{}
	
	// Get current working directory
	if wd, err := os.Getwd(); err == nil {
		result["getwd"] = wd
	}
	
	// Environment variables
	result["env_home"] = os.Getenv("HOME")
	result["env_path_exists"] = os.Getenv("PATH") != ""
	
	// File operations
	result["temp_dir"] = os.TempDir()
	
	// Args
	result["args_len"] = len(os.Args)
	
	// Hostname
	if hostname, err := os.Hostname(); err == nil {
		result["hostname"] = hostname
	}
	
	return TestResult{
		Name:    "OS Package",
		Success: true,
		Data:    result,
	}
}

// Test filepath package
func testFilepathPackage() TestResult {
	path := "/home/user/documents/file.txt"
	
	dir, file := filepath.Split(path)
	
	result := map[string]interface{}{
		"base":      filepath.Base(path),
		"dir":       filepath.Dir(path),
		"ext":       filepath.Ext(path),
		"join":      filepath.Join("home", "user", "file.txt"),
		"clean":     filepath.Clean("home//user/../user/./file.txt"),
		"abs_test":  filepath.IsAbs(path),
		"split_dir": dir,
		"split_file": file,
		"match": func() bool {
			matched, err := filepath.Match("*.txt", "file.txt")
			return err == nil && matched
		}(),
	}
	
	return TestResult{
		Name:    "Filepath Package",
		Success: true,
		Data:    result,
	}
}

// Test regexp package
func testRegexpPackage() TestResult {
	text := "The year 2024 was great, but 2025 will be better!"
	
	// Simple pattern matching
	matched, _ := regexp.MatchString(`\d{4}`, text)
	
	// Compile and use regexp
	re := regexp.MustCompile(`\d{4}`)
	years := re.FindAllString(text, -1)
	
	// Replace with regexp
	replaced := re.ReplaceAllString(text, "YEAR")
	
	// Split with regexp
	words := regexp.MustCompile(`\s+`).Split(text, -1)
	
	result := map[string]interface{}{
		"match_found":    matched,
		"found_years":    years,
		"replaced_text":  replaced,
		"word_count":     len(words),
		"first_word":     words[0],
		"last_word":      words[len(words)-1],
	}
	
	return TestResult{
		Name:    "Regexp Package",
		Success: true,
		Data:    result,
	}
}

// Test JSON encoding/decoding
func testJSONPackage() TestResult {
	type Person struct {
		Name    string   `json:"name"`
		Age     int      `json:"age"`
		Hobbies []string `json:"hobbies"`
	}
	
	person := Person{
		Name:    "Alice",
		Age:     30,
		Hobbies: []string{"reading", "programming", "hiking"},
	}
	
	// Marshal to JSON
	jsonData, err := json.Marshal(person)
	if err != nil {
		return TestResult{
			Name:    "JSON Package",
			Success: false,
			Error:   err.Error(),
		}
	}
	
	// Unmarshal from JSON
	var decoded Person
	err = json.Unmarshal(jsonData, &decoded)
	if err != nil {
		return TestResult{
			Name:    "JSON Package",
			Success: false,
			Error:   err.Error(),
		}
	}
	
	result := map[string]interface{}{
		"json_string":    string(jsonData),
		"decoded_name":   decoded.Name,
		"decoded_age":    decoded.Age,
		"hobby_count":    len(decoded.Hobbies),
		"round_trip_ok":  person.Name == decoded.Name && person.Age == decoded.Age,
	}
	
	return TestResult{
		Name:    "JSON Package",
		Success: true,
		Data:    result,
	}
}

// Test file I/O operations
func testFileOperations() TestResult {
	testContent := "Hello, Go file I/O!"
	filename := "test_file.txt"
	
	// Write to file
	err := ioutil.WriteFile(filename, []byte(testContent), 0644)
	if err != nil {
		return TestResult{
			Name:    "File Operations",
			Success: false,
			Error:   "Write failed: " + err.Error(),
		}
	}
	
	// Read from file
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return TestResult{
			Name:    "File Operations",
			Success: false,
			Error:   "Read failed: " + err.Error(),
		}
	}
	
	// Clean up
	os.Remove(filename)
	
	result := map[string]interface{}{
		"write_success":  true,
		"read_content":   string(content),
		"content_match":  string(content) == testContent,
		"file_size":      len(content),
	}
	
	return TestResult{
		Name:    "File Operations",
		Success: true,
		Data:    result,
	}
}

// Test complex operations combining multiple packages
func testComplexOperations() TestResult {
	// Create sample data
	type Product struct {
		Name     string  `json:"name"`
		Price    float64 `json:"price"`
		Category string  `json:"category"`
	}
	
	products := []Product{
		{"Laptop", 999.99, "Electronics"},
		{"Book", 29.99, "Education"},
		{"Coffee", 4.99, "Food"},
		{"Mouse", 19.99, "Electronics"},
		{"Notebook", 5.99, "Education"},
	}
	
	// Sort by price
	sort.Slice(products, func(i, j int) bool {
		return products[i].Price < products[j].Price
	})
	
	// Group by category
	categories := make(map[string][]Product)
	for _, product := range products {
		categories[product.Category] = append(categories[product.Category], product)
	}
	
	// Calculate statistics
	var totalPrice float64
	for _, product := range products {
		totalPrice += product.Price
	}
	avgPrice := totalPrice / float64(len(products))
	
	// Filter expensive items (> $20)
	var expensive []Product
	for _, product := range products {
		if product.Price > 20.0 {
			expensive = append(expensive, product)
		}
	}
	
	// Generate JSON report
	report := map[string]interface{}{
		"total_products":    len(products),
		"average_price":     math.Round(avgPrice*100) / 100,
		"cheapest_product":  products[0].Name,
		"most_expensive":    products[len(products)-1].Name,
		"categories":        len(categories),
		"expensive_count":   len(expensive),
	}
	
	jsonReport, _ := json.Marshal(report)
	
	result := map[string]interface{}{
		"products_sorted":   len(products) == 5,
		"categories_found":  len(categories),
		"avg_price":         avgPrice,
		"json_report":       string(jsonReport),
		"expensive_items":   len(expensive),
	}
	
	return TestResult{
		Name:    "Complex Operations",
		Success: true,
		Data:    result,
	}
}

// Test error handling
func testErrorHandling() TestResult {
	result := map[string]interface{}{}
	
	// Test file not found error
	_, err := ioutil.ReadFile("nonexistent_file.txt")
	result["file_error_handled"] = err != nil
	
	// Test JSON parsing error
	var data interface{}
	err = json.Unmarshal([]byte("invalid json"), &data)
	result["json_error_handled"] = err != nil
	
	// Test strconv parsing error
	_, err = strconv.Atoi("not_a_number")
	result["strconv_error_handled"] = err != nil
	
	// Test regex compilation error
	_, err = regexp.Compile("[invalid")
	result["regex_error_handled"] = err != nil
	
	return TestResult{
		Name:    "Error Handling",
		Success: true,
		Data:    result,
	}
}

func main() {
	fmt.Println("Testing Go Standard Library Usage in Compiled Code")
	fmt.Println(strings.Repeat("=", 60))
	
	tests := []func() TestResult{
		testBuiltins,
		testStringOperations,
		testFmtPackage,
		testMathPackage,
		testTimePackage,
		testRandPackage,
		testSortPackage,
		testStrconvPackage,
		testOsPackage,
		testFilepathPackage,
		testRegexpPackage,
		testJSONPackage,
		testFileOperations,
		testComplexOperations,
		testErrorHandling,
	}
	
	var results []TestResult
	successful := 0
	
	for _, test := range tests {
		result := test()
		results = append(results, result)
		
		fmt.Printf("\nTesting %s...\n", result.Name)
		if result.Success {
			fmt.Printf("✓ %s completed successfully\n", result.Name)
			successful++
		} else {
			fmt.Printf("✗ %s failed: %s\n", result.Name, result.Error)
		}
	}
	
	fmt.Println("\n" + strings.Repeat("=", 60))
	fmt.Println("Test Summary:")
	fmt.Printf("Total tests: %d\n", len(tests))
	fmt.Printf("Successful: %d\n", successful)
	fmt.Printf("Failed: %d\n", len(tests)-successful)
	
	// Save results to JSON file
	jsonResults, err := json.MarshalIndent(results, "", "  ")
	if err == nil {
		err = ioutil.WriteFile("go_stdlib_test_results.json", jsonResults, 0644)
		if err == nil {
			fmt.Println("\nDetailed results saved to 'go_stdlib_test_results.json'")
		}
	}
	
	if successful == len(tests) {
		fmt.Println("\n🎉 All tests passed!")
		os.Exit(0)
	} else {
		fmt.Printf("\n❌ %d tests failed\n", len(tests)-successful)
		os.Exit(1)
	}
}