package main

type Person struct {
    Name string
    Age  int
}

func main() {
    p := Person{Name: "Alice", Age: 30}
    println(p.Name)
    println(p.Age)
}
