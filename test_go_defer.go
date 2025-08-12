package main

func main() {
    defer func() {
        println("deferred")
    }()
    println("main")
}
