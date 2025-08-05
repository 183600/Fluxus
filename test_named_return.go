package main

func namedReturn(x, y int) (sum int, product int) {
    sum = x + y
    product = x * y
    return
}

func main() {
    s, p := namedReturn(5, 3)
    println(s, p)
}