package main

func main() {
    var m map[string]int
    m = make(map[string]int)
    m["key"] = 42
    println(m["key"])
}
