package main

type Shape interface {
    Area() int
}

type Rectangle struct {
    Width, Height int
}

func (r Rectangle) Area() int {
    return r.Width * r.Height
}

func main() {
    var s Shape
    s = Rectangle{Width: 4, Height: 5}
    println(s.Area())
}
