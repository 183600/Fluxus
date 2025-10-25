package main

import "fmt"

type Node struct {
    Value int
    Next  *Node
}

type LinkedList struct {
    Head *Node
}

func (ll *LinkedList) Insert(value int) {
    newNode := &Node{Value: value}
    if ll.Head == nil {
        ll.Head = newNode
        return
    }
    
    current := ll.Head
    for current.Next != nil {
        current = current.Next
    }
    current.Next = newNode
}

func (ll *LinkedList) Display() {
    current := ll.Head
    for current != nil {
        fmt.Printf("%d -> ", current.Value)
        current = current.Next
    }
    fmt.Println("nil")
}

func main() {
    fmt.Println("Go Data Structures Test")
    
    // Test linked list
    ll := &LinkedList{}
    values := []int{10, 20, 30, 40, 50}
    
    for _, v := range values {
        ll.Insert(v)
    }
    
    fmt.Print("Linked List: ")
    ll.Display()
    
    // Test stack simulation with slice
    stack := []int{}
    fmt.Println("Stack Operations:")
    
    // Push
    for _, v := range values[:3] {
        stack = append(stack, v)
        fmt.Printf("Pushed %d, Stack: %v\n", v, stack)
    }
    
    // Pop
    for len(stack) > 0 {
        popped := stack[len(stack)-1]
        stack = stack[:len(stack)-1]
        fmt.Printf("Popped %d, Stack: %v\n", popped, stack)
    }
    
    // Test map operations
    studentAges := map[string]int{
        "Alice":   20,
        "Bob":     22,
        "Charlie": 21,
    }
    
    fmt.Println("Map Operations:")
    for name, age := range studentAges {
        fmt.Printf("  %s: %d\n", name, age)
    }
    
    studentAges["David"] = 23
    fmt.Printf("Added David: %d\n", studentAges["David"])
    
    fmt.Println("Go data structures test completed successfully!")
}
