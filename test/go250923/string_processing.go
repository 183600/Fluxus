package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Hello, Go Programming!"
    words := strings.Split(text, " ")

    fmt.Println("Original text:", text)
    fmt.Println("Words in the text:")

    for i, word := range words {
        fmt.Printf("%d: %s\n", i+1, word)
    }

    reversed := strings.Join(reverseSlice(words), " ")
    fmt.Println("Reversed:", reversed)
}

func reverseSlice(slice []string) []string {
    reversed := make([]string, len(slice))
    for i, j := 0, len(slice)-1; i < len(slice); i, j = i+1, j-1 {
        reversed[i] = slice[j]
    }
    return reversed
}