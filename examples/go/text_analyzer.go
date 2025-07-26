package main

import (
	"fmt"
	"math/rand"
	"sort"
	"strings"
	"time"
)

type TextAnalyzer struct {
	text      string
	wordCount map[string]int
	processed bool
}

func NewTextAnalyzer(text string) *TextAnalyzer {
	return &TextAnalyzer{
		text:      text,
		wordCount: make(map[string]int),
		processed: false,
	}
}

func (ta *TextAnalyzer) processText() {
	if ta.processed {
		return
	}
	
	// Convert to lowercase and split into words
	words := strings.Fields(strings.ToLower(ta.text))
	
	for _, word := range words {
		// Remove punctuation
		cleanWord := strings.Trim(word, ".,!?;:\"'()[]{}~`")
		if cleanWord != "" {
			ta.wordCount[cleanWord]++
		}
	}
	ta.processed = true
}

func (ta *TextAnalyzer) GetWordCount() int {
	ta.processText()
	total := 0
	for _, count := range ta.wordCount {
		total += count
	}
	return total
}

func (ta *TextAnalyzer) GetUniqueWordCount() int {
	ta.processText()
	return len(ta.wordCount)
}

func (ta *TextAnalyzer) GetCharacterCount() int {
	return len(ta.text)
}

func (ta *TextAnalyzer) GetCharacterCountNoSpaces() int {
	return len(strings.ReplaceAll(ta.text, " ", ""))
}

func (ta *TextAnalyzer) GetSentenceCount() int {
	sentences := strings.FieldsFunc(ta.text, func(c rune) bool {
		return c == '.' || c == '!' || c == '?'
	})
	count := 0
	for _, sentence := range sentences {
		if strings.TrimSpace(sentence) != "" {
			count++
		}
	}
	return count
}

func (ta *TextAnalyzer) GetMostFrequentWords(n int) []WordFreq {
	ta.processText()
	
	type WordFreq struct {
		Word  string
		Count int
	}
	
	var wordFreqs []WordFreq
	for word, count := range ta.wordCount {
		wordFreqs = append(wordFreqs, WordFreq{Word: word, Count: count})
	}
	
	sort.Slice(wordFreqs, func(i, j int) bool {
		return wordFreqs[i].Count > wordFreqs[j].Count
	})
	
	if n > len(wordFreqs) {
		n = len(wordFreqs)
	}
	
	return wordFreqs[:n]
}

type WordFreq struct {
	Word  string
	Count int
}

func (ta *TextAnalyzer) GetAverageWordLength() float64 {
	ta.processText()
	if len(ta.wordCount) == 0 {
		return 0
	}
	
	totalLength := 0
	totalWords := 0
	for word, count := range ta.wordCount {
		totalLength += len(word) * count
		totalWords += count
	}
	
	return float64(totalLength) / float64(totalWords)
}

func (ta *TextAnalyzer) FindLongestWord() string {
	ta.processText()
	longest := ""
	for word := range ta.wordCount {
		if len(word) > len(longest) {
			longest = word
		}
	}
	return longest
}

func (ta *TextAnalyzer) ContainsWord(searchWord string) bool {
	ta.processText()
	_, exists := ta.wordCount[strings.ToLower(searchWord)]
	return exists
}

func (ta *TextAnalyzer) GetWordFrequency(word string) int {
	ta.processText()
	return ta.wordCount[strings.ToLower(word)]
}

// Additional utility functions
func generateRandomText(wordCount int) string {
	words := []string{
		"the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog",
		"hello", "world", "programming", "language", "computer", "science",
		"algorithm", "data", "structure", "function", "variable", "loop",
		"condition", "array", "string", "integer", "boolean", "class",
		"object", "method", "parameter", "return", "value", "type",
		"system", "process", "memory", "storage", "network", "internet",
		"database", "query", "table", "record", "field", "index",
	}
	
	rand.Seed(time.Now().UnixNano())
	var result []string
	
	for i := 0; i < wordCount; i++ {
		result = append(result, words[rand.Intn(len(words))])
	}
	
	return strings.Join(result, " ")
}

func main() {
	fmt.Println("Text Analyzer Demo")
	fmt.Println("==================")
	
	// Sample text
	sampleText := `
	The quick brown fox jumps over the lazy dog. This is a classic pangram sentence 
	that contains every letter of the English alphabet at least once. Programming 
	languages often use this sentence for testing purposes. The fox is quick and 
	brown, while the dog is lazy. This sentence demonstrates various programming 
	concepts like string manipulation, character counting, and text analysis.
	
	Data structures and algorithms are fundamental concepts in computer science. 
	Arrays, linked lists, stacks, and queues are common data structures. Sorting 
	algorithms like quicksort and mergesort are essential for organizing data 
	efficiently. The complexity of algorithms is measured using Big O notation.
	`
	
	analyzer := NewTextAnalyzer(sampleText)
	
	fmt.Printf("Text: %s\n", strings.TrimSpace(sampleText))
	fmt.Println("\nAnalysis Results:")
	fmt.Println("-----------------")
	
	fmt.Printf("Total characters: %d\n", analyzer.GetCharacterCount())
	fmt.Printf("Characters (no spaces): %d\n", analyzer.GetCharacterCountNoSpaces())
	fmt.Printf("Word count: %d\n", analyzer.GetWordCount())
	fmt.Printf("Unique words: %d\n", analyzer.GetUniqueWordCount())
	fmt.Printf("Sentence count: %d\n", analyzer.GetSentenceCount())
	fmt.Printf("Average word length: %.2f\n", analyzer.GetAverageWordLength())
	fmt.Printf("Longest word: %s\n", analyzer.FindLongestWord())
	
	fmt.Println("\nMost frequent words:")
	topWords := analyzer.GetMostFrequentWords(10)
	for i, wf := range topWords {
		fmt.Printf("%d. %s (%d occurrences)\n", i+1, wf.Word, wf.Count)
	}
	
	// Test word search
	testWords := []string{"the", "programming", "algorithm", "xyz"}
	fmt.Println("\nWord search results:")
	for _, word := range testWords {
		if analyzer.ContainsWord(word) {
			freq := analyzer.GetWordFrequency(word)
			fmt.Printf("'%s' found %d times\n", word, freq)
		} else {
			fmt.Printf("'%s' not found\n", word)
		}
	}
	
	// Test with random text
	fmt.Println("\n" + strings.Repeat("=", 50))
	fmt.Println("Testing with random generated text:")
	randomText := generateRandomText(100)
	randomAnalyzer := NewTextAnalyzer(randomText)
	
	fmt.Printf("Random text: %s\n", randomText)
	fmt.Printf("Word count: %d\n", randomAnalyzer.GetWordCount())
	fmt.Printf("Unique words: %d\n", randomAnalyzer.GetUniqueWordCount())
	fmt.Printf("Average word length: %.2f\n", randomAnalyzer.GetAverageWordLength())
	
	fmt.Println("Top 5 words in random text:")
	randomTopWords := randomAnalyzer.GetMostFrequentWords(5)
	for i, wf := range randomTopWords {
		fmt.Printf("%d. %s (%d occurrences)\n", i+1, wf.Word, wf.Count)
	}
}