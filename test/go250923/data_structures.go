package main

import (
    "fmt"
    "container/list"
)

func main() {
    linkedList := list.New()

    linkedList.PushBack(10)
    linkedList.PushBack(20)
    linkedList.PushFront(5)

    linkedList.InsertAfter(15, linkedList.Front())

    fmt.Println("Linked List contents:")
    for element := linkedList.Front(); element != nil; element = element.Next() {
        fmt.Printf("%d ", element.Value.(int))
    }
    fmt.Println()

    fmt.Println("Removing elements:")
    element := linkedList.Front().Next()
    if element != nil {
        linkedList.Remove(element)
        fmt.Println("Removed second element")
    }

    fmt.Println("Remaining elements:")
    for element := linkedList.Front(); element != nil; element = element.Next() {
        fmt.Printf("%d ", element.Value.(int))
    }
    fmt.Println()
}