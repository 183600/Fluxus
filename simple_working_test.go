package main

import "fmt"

func testBasic() {
    x := 10
    y := 20
    result := x + y
    fmt.Printf("Basic arithmetic: %d + %d = %d\n", x, y, result)
}

func main() {
    fmt.Printf("=== Go语言特性测试 ===\n")
    testBasic()
    fmt.Printf("=== 测试完成 ===\n")
}