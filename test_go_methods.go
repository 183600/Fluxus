package main

type Rectangle struct {
    Width, Height int
}

func (r Rectangle) Area() int {
    return r.Width * r.Height
}

func main() {
    r := Rectangle{Width: 5, Height: 3}
    println(r.Area())
}
