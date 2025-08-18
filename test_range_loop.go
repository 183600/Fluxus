package main

func main() {
    // Go 1.22+ integer range loop
    for range 10 {
        println("iteration")
    }
    
    // Traditional range loop
    for i := range []int{1, 2, 3} {
        println(i)
    }
}
