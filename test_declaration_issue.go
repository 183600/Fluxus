package main

func main() {
    // This should be a syntax error but might be silently ignored
    invalid_syntax_here xyz
    fmt.Println("This might still parse")
}
