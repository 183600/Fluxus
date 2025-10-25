package main

import (
	"fmt"
	"math"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

func fibonacciRecursive(n int) int {
	if n <= 0 {
		return 0
	} else if n == 1 {
		return 1
	} else {
		return fibonacciRecursive(n-1) + fibonacciRecursive(n-2)
	}
}

func fibonacciIterative(n int) int {
	if n <= 0 {
		return 0
	} else if n == 1 {
		return 1
	}
	
	a, b := 0, 1
	for i := 2; i <= n; i++ {
		a, b = b, a+b
	}
	return b
}

func fibonacciMemoized(n int, memo map[int]int) int {
	if val, exists := memo[n]; exists {
		return val
	}
	
	if n <= 0 {
		return 0
	} else if n == 1 {
		return 1
	} else {
		memo[n] = fibonacciMemoized(n-1, memo) + fibonacciMemoized(n-2, memo)
		return memo[n]
	}
}

func factorialRecursive(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorialRecursive(n-1)
}

func factorialIterative(n int) int {
	result := 1
	for i := 2; i <= n; i++ {
		result *= i
	}
	return result
}

func gcdEuclidean(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func lcm(a, b int) int {
	return int(math.Abs(float64(a*b))) / gcdEuclidean(a, b)
}

func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	if n == 2 {
		return true
	}
	if n%2 == 0 {
		return false
	}
	
	for i := 3; i*i <= n; i += 2 {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func sieveOfEratosthenes(limit int) []int {
	sieve := make([]bool, limit+1)
	for i := 2; i <= limit; i++ {
		sieve[i] = true
	}
	
	for i := 2; i*i <= limit; i++ {
		if sieve[i] {
			for j := i * i; j <= limit; j += i {
				sieve[j] = false
			}
		}
	}
	
	var primes []int
	for i := 2; i <= limit; i++ {
		if sieve[i] {
			primes = append(primes, i)
		}
	}
	
	return primes
}

func powerIterative(base, exponent int) int {
	result := 1
	for i := 0; i < exponent; i++ {
		result *= base
	}
	return result
}

func powerRecursive(base, exponent int) int {
	if exponent == 0 {
		return 1
	} else if exponent == 1 {
		return base
	} else {
		return base * powerRecursive(base, exponent-1)
	}
}

func powerOptimized(base, exponent int) int {
	if exponent == 0 {
		return 1
	}
	if exponent%2 == 0 {
		halfPower := powerOptimized(base, exponent/2)
		return halfPower * halfPower
	} else {
		return base * powerOptimized(base, exponent-1)
	}
}

func binarySearch(arr []int, target int) int {
	left, right := 0, len(arr)-1
	
	for left <= right {
		mid := (left + right) / 2
		if arr[mid] == target {
			return mid
		} else if arr[mid] < target {
			left = mid + 1
		} else {
			right = mid - 1
		}
	}
	
	return -1
}

func linearSearch(arr []int, target int) int {
	for i, value := range arr {
		if value == target {
			return i
		}
	}
	return -1
}

func towerOfHanoi(n int, source, destination, auxiliary string) []string {
	var moves []string
	
	var hanoiRecursive func(int, string, string, string)
	hanoiRecursive = func(n int, source, destination, auxiliary string) {
		if n == 1 {
			moves = append(moves, fmt.Sprintf("Move disk 1 from %s to %s", source, destination))
		} else {
			hanoiRecursive(n-1, source, auxiliary, destination)
			moves = append(moves, fmt.Sprintf("Move disk %d from %s to %s", n, source, destination))
			hanoiRecursive(n-1, auxiliary, destination, source)
		}
	}
	
	hanoiRecursive(n, source, destination, auxiliary)
	return moves
}

func generatePascalsTriangle(rows int) [][]int {
	triangle := make([][]int, rows)
	
	for i := 0; i < rows; i++ {
		triangle[i] = make([]int, i+1)
		triangle[i][0] = 1
		triangle[i][i] = 1
		
		for j := 1; j < i; j++ {
			triangle[i][j] = triangle[i-1][j-1] + triangle[i-1][j]
		}
	}
	
	return triangle
}

func matrixMultiply(A, B [][]int) ([][]int, error) {
	if len(A[0]) != len(B) {
		return nil, fmt.Errorf("cannot multiply matrices: incompatible dimensions")
	}
	
	rowsA, colsA := len(A), len(A[0])
	rowsB, colsB := len(B), len(B[0])
	
	result := make([][]int, rowsA)
	for i := range result {
		result[i] = make([]int, colsB)
	}
	
	for i := 0; i < rowsA; i++ {
		for j := 0; j < colsB; j++ {
			for k := 0; k < colsA; k++ {
				result[i][j] += A[i][k] * B[k][j]
			}
		}
	}
	
	return result, nil
}

func matrixTranspose(matrix [][]int) [][]int {
	rows, cols := len(matrix), len(matrix[0])
	result := make([][]int, cols)
	
	for i := range result {
		result[i] = make([]int, rows)
	}
	
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			result[j][i] = matrix[i][j]
		}
	}
	
	return result
}

func determinant2x2(matrix [][]int) (int, error) {
	if len(matrix) != 2 || len(matrix[0]) != 2 {
		return 0, fmt.Errorf("matrix must be 2x2")
	}
	return matrix[0][0]*matrix[1][1] - matrix[0][1]*matrix[1][0], nil
}

func knapsack01(weights, values []int, capacity int) int {
	n := len(weights)
	dp := make([][]int, n+1)
	for i := range dp {
		dp[i] = make([]int, capacity+1)
	}
	
	for i := 1; i <= n; i++ {
		for w := 1; w <= capacity; w++ {
			if weights[i-1] <= w {
				option1 := values[i-1] + dp[i-1][w-weights[i-1]]
				option2 := dp[i-1][w]
				if option1 > option2 {
					dp[i][w] = option1
				} else {
					dp[i][w] = option2
				}
			} else {
				dp[i][w] = dp[i-1][w]
			}
		}
	}
	
	return dp[n][capacity]
}

func longestCommonSubsequence(str1, str2 string) int {
	m, n := len(str1), len(str2)
	dp := make([][]int, m+1)
	for i := range dp {
		dp[i] = make([]int, n+1)
	}
	
	for i := 1; i <= m; i++ {
		for j := 1; j <= n; j++ {
			if str1[i-1] == str2[j-1] {
				dp[i][j] = dp[i-1][j-1] + 1
			} else {
				if dp[i-1][j] > dp[i][j-1] {
					dp[i][j] = dp[i-1][j]
				} else {
					dp[i][j] = dp[i][j-1]
				}
			}
		}
	}
	
	return dp[m][n]
}

func editDistance(str1, str2 string) int {
	m, n := len(str1), len(str2)
	dp := make([][]int, m+1)
	for i := range dp {
		dp[i] = make([]int, n+1)
	}
	
	for i := 0; i <= m; i++ {
		dp[i][0] = i
	}
	for j := 0; j <= n; j++ {
		dp[0][j] = j
	}
	
	for i := 1; i <= m; i++ {
		for j := 1; j <= n; j++ {
			if str1[i-1] == str2[j-1] {
				dp[i][j] = dp[i-1][j-1]
			} else {
				min := dp[i-1][j]     // deletion
				if dp[i][j-1] < min { // insertion
					min = dp[i][j-1]
				}
				if dp[i-1][j-1] < min { // substitution
					min = dp[i-1][j-1]
				}
				dp[i][j] = 1 + min
			}
		}
	}
	
	return dp[m][n]
}

func coinChange(coins []int, amount int) int {
	dp := make([]int, amount+1)
	for i := range dp {
		dp[i] = amount + 1
	}
	dp[0] = 0
	
	for _, coin := range coins {
		for i := coin; i <= amount; i++ {
			if dp[i-coin]+1 < dp[i] {
				dp[i] = dp[i-coin] + 1
			}
		}
	}
	
	if dp[amount] > amount {
		return -1
	}
	return dp[amount]
}

func palindromeCheck(s string) bool {
	re := regexp.MustCompile(`[^a-zA-Z0-9]`)
	cleaned := strings.ToLower(re.ReplaceAllString(s, ""))
	
	for i := 0; i < len(cleaned)/2; i++ {
		if cleaned[i] != cleaned[len(cleaned)-1-i] {
			return false
		}
	}
	return true
}

func anagramCheck(str1, str2 string) bool {
	str1 = strings.ToLower(str1)
	str2 = strings.ToLower(str2)
	
	if len(str1) != len(str2) {
		return false
	}
	
	chars1 := strings.Split(str1, "")
	chars2 := strings.Split(str2, "")
	
	sort.Strings(chars1)
	sort.Strings(chars2)
	
	for i := range chars1 {
		if chars1[i] != chars2[i] {
			return false
		}
	}
	
	return true
}

type TextAnalyzer struct {
	text       string
	words      []string
	sentences  []string
	paragraphs []string
}

func NewTextAnalyzer(text string) *TextAnalyzer {
	ta := &TextAnalyzer{text: text}
	ta.extractWords()
	ta.extractSentences()
	ta.extractParagraphs()
	return ta
}

func (ta *TextAnalyzer) extractWords() {
	re := regexp.MustCompile(`\b\w+\b`)
	matches := re.FindAllString(strings.ToLower(ta.text), -1)
	ta.words = matches
}

func (ta *TextAnalyzer) extractSentences() {
	re := regexp.MustCompile(`[.!?]+`)
	sentences := re.Split(ta.text, -1)
	
	for _, sentence := range sentences {
		trimmed := strings.TrimSpace(sentence)
		if trimmed != "" {
			ta.sentences = append(ta.sentences, trimmed)
		}
	}
}

func (ta *TextAnalyzer) extractParagraphs() {
	re := regexp.MustCompile(`\n\s*\n`)
	paragraphs := re.Split(ta.text, -1)
	
	for _, paragraph := range paragraphs {
		trimmed := strings.TrimSpace(paragraph)
		if trimmed != "" {
			ta.paragraphs = append(ta.paragraphs, trimmed)
		}
	}
}

func (ta *TextAnalyzer) WordCount() int {
	return len(ta.words)
}

func (ta *TextAnalyzer) UniqueWordCount() int {
	unique := make(map[string]bool)
	for _, word := range ta.words {
		unique[word] = true
	}
	return len(unique)
}

func (ta *TextAnalyzer) SentenceCount() int {
	return len(ta.sentences)
}

func (ta *TextAnalyzer) ParagraphCount() int {
	return len(ta.paragraphs)
}

func (ta *TextAnalyzer) AverageWordsPerSentence() float64 {
	if ta.SentenceCount() == 0 {
		return 0.0
	}
	return float64(ta.WordCount()) / float64(ta.SentenceCount())
}

func (ta *TextAnalyzer) WordFrequency() map[string]int {
	freq := make(map[string]int)
	for _, word := range ta.words {
		freq[word]++
	}
	return freq
}

func (ta *TextAnalyzer) LongestWord() string {
	if len(ta.words) == 0 {
		return ""
	}
	
	longest := ta.words[0]
	for _, word := range ta.words {
		if len(word) > len(longest) {
			longest = word
		}
	}
	return longest
}

func (ta *TextAnalyzer) ShortestWord() string {
	if len(ta.words) == 0 {
		return ""
	}
	
	shortest := ta.words[0]
	for _, word := range ta.words {
		if len(word) < len(shortest) {
			shortest = word
		}
	}
	return shortest
}

func (ta *TextAnalyzer) AverageWordLength() float64 {
	if len(ta.words) == 0 {
		return 0.0
	}
	
	totalLength := 0
	for _, word := range ta.words {
		totalLength += len(word)
	}
	
	return float64(totalLength) / float64(len(ta.words))
}

func (ta *TextAnalyzer) ReadingTime(wpm int) float64 {
	return float64(ta.WordCount()) / float64(wpm)
}

func formatBytes(bytes int64) string {
	units := []string{"B", "KB", "MB", "GB", "TB"}
	value := float64(bytes)
	
	for _, unit := range units {
		if value < 1024.0 {
			return fmt.Sprintf("%.2f %s", value, unit)
		}
		value /= 1024.0
	}
	
	return fmt.Sprintf("%.2f PB", value)
}

func timeAgo(timestamp time.Time) string {
	now := time.Now()
	diff := now.Sub(timestamp)
	
	if diff.Hours() >= 24 {
		days := int(diff.Hours() / 24)
		return fmt.Sprintf("%d days ago", days)
	} else if diff.Hours() >= 1 {
		hours := int(diff.Hours())
		return fmt.Sprintf("%d hours ago", hours)
	} else if diff.Minutes() >= 1 {
		minutes := int(diff.Minutes())
		return fmt.Sprintf("%d minutes ago", minutes)
	} else {
		return "Just now"
	}
}

func chunkSlice(slice []int, chunkSize int) [][]int {
	var chunks [][]int
	
	for i := 0; i < len(slice); i += chunkSize {
		end := i + chunkSize
		if end > len(slice) {
			end = len(slice)
		}
		chunks = append(chunks, slice[i:end])
	}
	
	return chunks
}

func runAlgorithmDemonstrations() {
	fmt.Println("=== Algorithm Demonstrations ===\n")
	
	fmt.Println("1. Fibonacci Sequence:")
	n := 10
	fmt.Printf("   Recursive fibonacci(%d): %d\n", n, fibonacciRecursive(n))
	fmt.Printf("   Iterative fibonacci(%d): %d\n", n, fibonacciIterative(n))
	memo := make(map[int]int)
	fmt.Printf("   Memoized fibonacci(%d): %d\n", n, fibonacciMemoized(n, memo))
	
	fmt.Println("\n2. Factorial:")
	n = 6
	fmt.Printf("   Recursive factorial(%d): %d\n", n, factorialRecursive(n))
	fmt.Printf("   Iterative factorial(%d): %d\n", n, factorialIterative(n))
	
	fmt.Println("\n3. Number Theory:")
	a, b := 48, 18
	fmt.Printf("   GCD(%d, %d): %d\n", a, b, gcdEuclidean(a, b))
	fmt.Printf("   LCM(%d, %d): %d\n", a, b, lcm(a, b))
	fmt.Printf("   Is 17 prime: %t\n", isPrime(17))
	fmt.Printf("   Primes up to 30: %v\n", sieveOfEratosthenes(30))
	
	fmt.Println("\n4. Power Functions:")
	base, exp := 3, 4
	fmt.Printf("   %d^%d iterative: %d\n", base, exp, powerIterative(base, exp))
	fmt.Printf("   %d^%d recursive: %d\n", base, exp, powerRecursive(base, exp))
	fmt.Printf("   %d^%d optimized: %d\n", base, exp, powerOptimized(base, exp))
	
	fmt.Println("\n5. Search Algorithms:")
	arr := []int{1, 3, 5, 7, 9, 11, 13, 15}
	target := 7
	fmt.Printf("   Array: %v\n", arr)
	fmt.Printf("   Binary search for %d: %d\n", target, binarySearch(arr, target))
	fmt.Printf("   Linear search for %d: %d\n", target, linearSearch(arr, target))
	
	fmt.Println("\n6. Tower of Hanoi:")
	moves := towerOfHanoi(3, "A", "C", "B")
	fmt.Printf("   Moves for 3 disks: %d moves\n", len(moves))
	for _, move := range moves {
		fmt.Printf("   %s\n", move)
	}
	
	fmt.Println("\n7. Pascal's Triangle:")
	triangle := generatePascalsTriangle(5)
	for i, row := range triangle {
		fmt.Printf("   Row %d: %v\n", i, row)
	}
	
	fmt.Println("\n8. Matrix Operations:")
	A := [][]int{{1, 2}, {3, 4}}
	B := [][]int{{5, 6}, {7, 8}}
	fmt.Printf("   Matrix A: %v\n", A)
	fmt.Printf("   Matrix B: %v\n", B)
	if product, err := matrixMultiply(A, B); err == nil {
		fmt.Printf("   A × B: %v\n", product)
	}
	fmt.Printf("   A transposed: %v\n", matrixTranspose(A))
	if det, err := determinant2x2(A); err == nil {
		fmt.Printf("   Determinant of A: %d\n", det)
	}
	
	fmt.Println("\n9. Dynamic Programming:")
	weights := []int{10, 20, 30}
	values := []int{60, 100, 120}
	capacity := 50
	fmt.Printf("   0/1 Knapsack (weights=%v, values=%v, capacity=%d): %d\n", 
		weights, values, capacity, knapsack01(weights, values, capacity))
	
	str1, str2 := "AGGTAB", "GXTXAYB"
	fmt.Printf("   LCS of '%s' and '%s': %d\n", str1, str2, longestCommonSubsequence(str1, str2))
	
	fmt.Printf("   Edit distance between 'kitten' and 'sitting': %d\n", editDistance("kitten", "sitting"))
	
	coins := []int{1, 3, 4}
	amount := 6
	fmt.Printf("   Coin change for amount %d with coins %v: %d\n", amount, coins, coinChange(coins, amount))
	
	fmt.Println("\n10. String Algorithms:")
	testStr := "A man a plan a canal Panama"
	fmt.Printf("   Is '%s' a palindrome: %t\n", testStr, palindromeCheck(testStr))
	fmt.Printf("   Are 'listen' and 'silent' anagrams: %t\n", anagramCheck("listen", "silent"))
	
	fmt.Println("\n11. Text Analysis:")
	sampleText := `This is a great example of text analysis. The program can analyze various aspects of text,
including word frequency, reading time, and sentiment analysis. It's an amazing tool for
understanding textual data!

The text analyzer provides wonderful insights into the structure and content of documents.
It can help identify patterns and extract meaningful information from large amounts of text.`
	
	analyzer := NewTextAnalyzer(sampleText)
	fmt.Printf("   Word count: %d\n", analyzer.WordCount())
	fmt.Printf("   Unique words: %d\n", analyzer.UniqueWordCount())
	fmt.Printf("   Sentences: %d\n", analyzer.SentenceCount())
	fmt.Printf("   Paragraphs: %d\n", analyzer.ParagraphCount())
	fmt.Printf("   Avg words per sentence: %.2f\n", analyzer.AverageWordsPerSentence())
	fmt.Printf("   Avg word length: %.2f\n", analyzer.AverageWordLength())
	fmt.Printf("   Reading time (200 WPM): %.2f minutes\n", analyzer.ReadingTime(200))
	fmt.Printf("   Longest word: %s\n", analyzer.LongestWord())
	fmt.Printf("   Shortest word: %s\n", analyzer.ShortestWord())
	
	fmt.Println("\n12. Utility Functions:")
	fmt.Printf("   Format bytes: %s\n", formatBytes(1024*1024*5))
	
	testTime := time.Now().Add(-2*time.Hour - 30*time.Minute)
	fmt.Printf("   Time ago: %s\n", timeAgo(testTime))
	
	testSlice := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	chunked := chunkSlice(testSlice, 3)
	fmt.Printf("   Chunked slice: %v\n", chunked)
}

func main() {
	fmt.Println("=== Go Comprehensive Algorithms Demo ===")
	runAlgorithmDemonstrations()
}