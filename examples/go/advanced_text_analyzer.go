package main

import (
	"bufio"
	"fmt"
	"net/http"
	"net/url"
	"os"
	"regexp"
	"sort"
	"strings"
	"unicode"
)

type TextAnalyzer struct {
	text          string
	words         []string
	sentences     []string
	paragraphs    []string
	wordFreq      map[string]int
	charCount     int
	wordCount     int
	sentenceCount int
	paragraphCount int
}

func NewTextAnalyzer(text string) *TextAnalyzer {
	analyzer := &TextAnalyzer{
		text:     text,
		wordFreq: make(map[string]int),
	}
	analyzer.analyze()
	return analyzer
}

func (ta *TextAnalyzer) analyze() {
	ta.charCount = len(ta.text)
	
	// Split into paragraphs
	ta.paragraphs = strings.Split(strings.TrimSpace(ta.text), "\n\n")
	ta.paragraphCount = len(ta.paragraphs)
	
	// Split into sentences
	sentenceRegex := regexp.MustCompile(`[.!?]+`)
	ta.sentences = sentenceRegex.Split(ta.text, -1)
	ta.sentenceCount = len(ta.sentences) - 1 // Last split is usually empty
	
	// Split into words and count frequency
	wordRegex := regexp.MustCompile(`\b\w+\b`)
	matches := wordRegex.FindAllString(strings.ToLower(ta.text), -1)
	ta.words = matches
	ta.wordCount = len(matches)
	
	for _, word := range matches {
		ta.wordFreq[word]++
	}
}

func (ta *TextAnalyzer) GetStats() map[string]interface{} {
	avgWordsPerSentence := 0.0
	if ta.sentenceCount > 0 {
		avgWordsPerSentence = float64(ta.wordCount) / float64(ta.sentenceCount)
	}
	
	avgSentencesPerParagraph := 0.0
	if ta.paragraphCount > 0 {
		avgSentencesPerParagraph = float64(ta.sentenceCount) / float64(ta.paragraphCount)
	}
	
	return map[string]interface{}{
		"characters":              ta.charCount,
		"words":                   ta.wordCount,
		"sentences":               ta.sentenceCount,
		"paragraphs":              ta.paragraphCount,
		"unique_words":            len(ta.wordFreq),
		"avg_words_per_sentence":  avgWordsPerSentence,
		"avg_sentences_per_para":  avgSentencesPerParagraph,
	}
}

func (ta *TextAnalyzer) GetMostFrequentWords(count int) []struct {
	Word  string
	Count int
} {
	type wordCount struct {
		Word  string
		Count int
	}
	
	var pairs []wordCount
	for word, freq := range ta.wordFreq {
		pairs = append(pairs, wordCount{word, freq})
	}
	
	sort.Slice(pairs, func(i, j int) bool {
		return pairs[i].Count > pairs[j].Count
	})
	
	if count > len(pairs) {
		count = len(pairs)
	}
	
	result := make([]struct {
		Word  string
		Count int
	}, count)
	
	for i := 0; i < count; i++ {
		result[i].Word = pairs[i].Word
		result[i].Count = pairs[i].Count
	}
	
	return result
}

func (ta *TextAnalyzer) GetReadabilityScore() float64 {
	if ta.sentenceCount == 0 || ta.wordCount == 0 {
		return 0.0
	}
	
	// Simple readability approximation
	avgWordsPerSentence := float64(ta.wordCount) / float64(ta.sentenceCount)
	avgSyllablesPerWord := ta.estimateAvgSyllables()
	
	// Flesch Reading Ease approximation
	score := 206.835 - (1.015 * avgWordsPerSentence) - (84.6 * avgSyllablesPerWord)
	return score
}

func (ta *TextAnalyzer) estimateAvgSyllables() float64 {
	totalSyllables := 0
	vowels := "aeiouAEIOU"
	
	for _, word := range ta.words {
		syllables := 0
		prevWasVowel := false
		
		for _, char := range word {
			isVowel := strings.ContainsRune(vowels, char)
			if isVowel && !prevWasVowel {
				syllables++
			}
			prevWasVowel = isVowel
		}
		
		if syllables == 0 {
			syllables = 1
		}
		totalSyllables += syllables
	}
	
	if ta.wordCount == 0 {
		return 0.0
	}
	return float64(totalSyllables) / float64(ta.wordCount)
}

func (ta *TextAnalyzer) FindLongestWords(count int) []string {
	unique := make(map[string]bool)
	for _, word := range ta.words {
		unique[word] = true
	}
	
	var words []string
	for word := range unique {
		words = append(words, word)
	}
	
	sort.Slice(words, func(i, j int) bool {
		return len(words[i]) > len(words[j])
	})
	
	if count > len(words) {
		count = len(words)
	}
	
	return words[:count]
}

func readFromURL(urlStr string) (string, error) {
	resp, err := http.Get(urlStr)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	
	scanner := bufio.NewScanner(resp.Body)
	var content strings.Builder
	
	for scanner.Scan() {
		content.WriteString(scanner.Text())
		content.WriteString("\n")
	}
	
	return content.String(), scanner.Err()
}

func isValidURL(str string) bool {
	u, err := url.Parse(str)
	return err == nil && u.Scheme != "" && u.Host != ""
}

func main() {
	fmt.Println("Advanced Text Analyzer")
	fmt.Println("======================")
	
	var text string
	var err error
	
	if len(os.Args) > 1 {
		input := os.Args[1]
		
		if isValidURL(input) {
			fmt.Printf("Reading from URL: %s\n", input)
			text, err = readFromURL(input)
			if err != nil {
				fmt.Printf("Error reading from URL: %v\n", err)
				return
			}
		} else {
			// Try to read as file
			if _, statErr := os.Stat(input); statErr == nil {
				fmt.Printf("Reading from file: %s\n", input)
				data, readErr := os.ReadFile(input)
				if readErr != nil {
					fmt.Printf("Error reading file: %v\n", readErr)
					return
				}
				text = string(data)
			} else {
				// Treat as direct text input
				text = input
			}
		}
	} else {
		// Read from stdin
		fmt.Println("Enter text to analyze (Ctrl+D to finish):")
		scanner := bufio.NewScanner(os.Stdin)
		var lines []string
		
		for scanner.Scan() {
			lines = append(lines, scanner.Text())
		}
		
		text = strings.Join(lines, "\n")
	}
	
	if strings.TrimSpace(text) == "" {
		fmt.Println("No text to analyze")
		return
	}
	
	analyzer := NewTextAnalyzer(text)
	
	fmt.Println("\nText Statistics:")
	fmt.Println("================")
	stats := analyzer.GetStats()
	
	for key, value := range stats {
		switch v := value.(type) {
		case float64:
			fmt.Printf("%-25s: %.2f\n", strings.Title(strings.ReplaceAll(key, "_", " ")), v)
		default:
			fmt.Printf("%-25s: %v\n", strings.Title(strings.ReplaceAll(key, "_", " ")), v)
		}
	}
	
	fmt.Printf("%-25s: %.1f\n", "Readability Score", analyzer.GetReadabilityScore())
	
	fmt.Println("\nMost Frequent Words:")
	fmt.Println("====================")
	frequent := analyzer.GetMostFrequentWords(10)
	for i, wc := range frequent {
		fmt.Printf("%2d. %-15s: %d\n", i+1, wc.Word, wc.Count)
	}
	
	fmt.Println("\nLongest Words:")
	fmt.Println("==============")
	longest := analyzer.FindLongestWords(10)
	for i, word := range longest {
		fmt.Printf("%2d. %s (%d chars)\n", i+1, word, len(word))
	}
}