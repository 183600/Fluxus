package main

import "fmt"

func reverse(s string) string {
    runes := []rune(s)
    for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
        runes[i], runes[j] = runes[j], runes[i]
    }
    return string(runes)
}

func countVowels(s string) int {
    count := 0
    vowels := "aeiouAEIOU"
    for _, char := range s {
        for _, vowel := range vowels {
            if char == vowel {
                count = count + 1
                break
            }
        }
    }
    return count
}

func isPalindrome(s string) bool {
    reversed := reverse(s)
    return s == reversed
}

func main() {
    fmt.Println("String Processing Demo")
    
    text := "hello"
    fmt.Printf("Original: %s\n", text)
    fmt.Printf("Reversed: %s\n", reverse(text))
    fmt.Printf("Vowel count: %d\n", countVowels(text))
    fmt.Printf("Is palindrome: %t\n", isPalindrome(text))
    
    // Test with palindrome
    palindrome := "racecar"
    fmt.Printf("\nPalindrome test: %s\n", palindrome)
    fmt.Printf("Is palindrome: %t\n", isPalindrome(palindrome))
    
    // String array
    words := [3]string{"go", "programming", "language"}
    fmt.Println("\nWords and their vowel counts:")
    for i := 0; i < 3; i++ {
        word := words[i]
        vowels := countVowels(word)
        fmt.Printf("%s: %d vowels\n", word, vowels)
    }
}