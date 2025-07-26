package main

import (
	"fmt"
	"strings"
	"unicode"
)

// StringProcessor provides various string processing utilities
type StringProcessor struct{}

// NewStringProcessor creates a new string processor
func NewStringProcessor() *StringProcessor {
	return &StringProcessor{}
}

// Reverse reverses a string
func (sp *StringProcessor) Reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

// IsPalindrome checks if a string is a palindrome
func (sp *StringProcessor) IsPalindrome(s string) bool {
	// Normalize: lowercase and remove non-alphanumeric
	normalized := ""
	for _, r := range strings.ToLower(s) {
		if unicode.IsLetter(r) || unicode.IsDigit(r) {
			normalized += string(r)
		}
	}
	
	return normalized == sp.Reverse(normalized)
}

// CountWords counts the number of words in a string
func (sp *StringProcessor) CountWords(s string) int {
	words := strings.Fields(s)
	return len(words)
}

// CountVowels counts the number of vowels in a string
func (sp *StringProcessor) CountVowels(s string) int {
	vowels := "aeiouAEIOU"
	count := 0
	for _, r := range s {
		if strings.ContainsRune(vowels, r) {
			count++
		}
	}
	return count
}

// TitleCase converts a string to title case
func (sp *StringProcessor) TitleCase(s string) string {
	return strings.Title(strings.ToLower(s))
}

// RemoveDuplicateChars removes duplicate characters from a string
func (sp *StringProcessor) RemoveDuplicateChars(s string) string {
	seen := make(map[rune]bool)
	result := ""
	
	for _, r := range s {
		if !seen[r] {
			seen[r] = true
			result += string(r)
		}
	}
	
	return result
}

// FindLongestWord finds the longest word in a string
func (sp *StringProcessor) FindLongestWord(s string) string {
	words := strings.Fields(s)
	longest := ""
	
	for _, word := range words {
		// Remove punctuation for comparison
		cleanWord := ""
		for _, r := range word {
			if unicode.IsLetter(r) {
				cleanWord += string(r)
			}
		}
		
		if len(cleanWord) > len(longest) {
			longest = cleanWord
		}
	}
	
	return longest
}

// CapitalizeFirstLetter capitalizes the first letter of each word
func (sp *StringProcessor) CapitalizeFirstLetter(s string) string {
	words := strings.Fields(s)
	for i, word := range words {
		if len(word) > 0 {
			words[i] = strings.ToUpper(string(word[0])) + strings.ToLower(word[1:])
		}
	}
	return strings.Join(words, " ")
}

// GetCharacterFrequency returns a map of character frequencies
func (sp *StringProcessor) GetCharacterFrequency(s string) map[rune]int {
	freq := make(map[rune]int)
	for _, r := range strings.ToLower(s) {
		if unicode.IsLetter(r) {
			freq[r]++
		}
	}
	return freq
}

func main() {
	processor := NewStringProcessor()
	
	fmt.Println("=== String Processing Utilities ===")
	
	testStrings := []string{
		"Hello World",
		"A man a plan a canal Panama",
		"The quick brown fox jumps over the lazy dog",
		"Programming in Go is fun!",
		"aabbccddee",
	}
	
	for _, str := range testStrings {
		fmt.Printf("\n--- Processing: \"%s\" ---\n", str)
		
		// Basic operations
		fmt.Printf("Reversed: %s\n", processor.Reverse(str))
		fmt.Printf("Is Palindrome: %t\n", processor.IsPalindrome(str))
		fmt.Printf("Word Count: %d\n", processor.CountWords(str))
		fmt.Printf("Vowel Count: %d\n", processor.CountVowels(str))
		
		// Case operations
		fmt.Printf("Title Case: %s\n", processor.TitleCase(str))
		fmt.Printf("Capitalized: %s\n", processor.CapitalizeFirstLetter(str))
		
		// Advanced operations
		fmt.Printf("Without Duplicates: %s\n", processor.RemoveDuplicateChars(str))
		fmt.Printf("Longest Word: %s\n", processor.FindLongestWord(str))
		
		// Character frequency
		freq := processor.GetCharacterFrequency(str)
		fmt.Print("Character Frequency: ")
		for char, count := range freq {
			fmt.Printf("%c:%d ", char, count)
		}
		fmt.Println()
	}
	
	// Test specific palindromes
	fmt.Println("\n=== Palindrome Tests ===")
	palindromeTests := []string{
		"racecar",
		"A man a plan a canal Panama",
		"race a car",
		"hello",
		"Madam",
	}
	
	for _, test := range palindromeTests {
		fmt.Printf("\"%s\" is palindrome: %t\n", test, processor.IsPalindrome(test))
	}
}