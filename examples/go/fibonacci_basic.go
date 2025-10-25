package main

func main() {
    a := 1
    b := 1
    next := a + b
    a = b
    b = next
    next = a + b
}