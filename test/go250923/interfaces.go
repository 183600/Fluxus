package main

import (
	"fmt"
	"math"
)

type Shape interface {
	Area() float64
	Perimeter() float64
}

type Circle struct {
	Radius float64
}

func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * math.Pi * c.Radius
}

type Rectangle struct {
	Width  float64
	Height float64
}

func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

type Triangle struct {
	Base   float64
	Height float64
	SideA  float64
	SideB  float64
	SideC  float64
}

func (t Triangle) Area() float64 {
	return 0.5 * t.Base * t.Height
}

func (t Triangle) Perimeter() float64 {
	return t.SideA + t.SideB + t.SideC
}

type ShapeCalculator struct{}

func (sc ShapeCalculator) CalculateArea(s Shape) float64 {
	return s.Area()
}

func (sc ShapeCalculator) CalculatePerimeter(s Shape) float64 {
	return s.Perimeter()
}

func (sc ShapeCalculator) PrintShapeInfo(s Shape) {
	fmt.Printf("Shape: %T\n", s)
	fmt.Printf("Area: %.2f\n", s.Area())
	fmt.Printf("Perimeter: %.2f\n", s.Perimeter())
	fmt.Println("---")
}

type Writer interface {
	Write(data string) error
}

type FileReader struct {
	filename string
}

func (fr FileReader) Write(data string) error {
	fmt.Printf("Writing to file %s: %s\n", fr.filename, data)
	return nil
}

type ConsoleWriter struct{}

func (cw ConsoleWriter) Write(data string) error {
	fmt.Printf("Console output: %s\n", data)
	return nil
}

type Logger interface {
	Log(message string)
}

type FileLogger struct {
	writer Writer
}

func (fl FileLogger) Log(message string) {
	fl.writer.Write(fmt.Sprintf("LOG: %s", message))
}

type ConsoleLogger struct {
	writer Writer
}

func (cl ConsoleLogger) Log(message string) {
	cl.writer.Write(fmt.Sprintf("LOG: %s", message))
}

type Drawable interface {
	Draw()
}

type Point struct {
	X, Y int
}

func (p Point) Draw() {
	fmt.Printf("Drawing point at (%d, %d)\n", p.X, p.Y)
}

type Line struct {
	Start, End Point
}

func (l Line) Draw() {
	fmt.Printf("Drawing line from (%d, %d) to (%d, %d)\n",
		l.Start.X, l.Start.Y, l.End.X, l.End.Y)
}

func main() {
	shapes := []Shape{
		Circle{Radius: 5},
		Rectangle{Width: 4, Height: 6},
		Triangle{Base: 3, Height: 4, SideA: 3, SideB: 4, SideC: 5},
	}

	calc := ShapeCalculator{}
	fmt.Println("=== Shape Calculations ===")
	for _, shape := range shapes {
		calc.PrintShapeInfo(shape)
	}

	fmt.Println("\n=== Writer Interface Examples ===")
	fileWriter := FileReader{filename: "example.txt"}
	consoleWriter := ConsoleWriter{}

	fileLogger := FileLogger{writer: fileWriter}
	consoleLogger := ConsoleLogger{writer: consoleWriter}

	fileLogger.Log("This is a file log message")
	consoleLogger.Log("This is a console log message")

	fmt.Println("\n=== Drawable Interface Examples ===")
	drawables := []Drawable{
		Point{X: 10, Y: 20},
		Line{Start: Point{X: 0, Y: 0}, End: Point{X: 100, Y: 100}},
	}

	for _, drawable := range drawables {
		drawable.Draw()
	}

	fmt.Println("\n=== Interface Type Assertions ===")
	var s Shape = Circle{Radius: 10}

	if circle, ok := s.(Circle); ok {
		fmt.Printf("Circle with radius: %.2f\n", circle.Radius)
	}

	switch v := s.(type) {
	case Circle:
		fmt.Printf("It's a Circle with radius: %.2f\n", v.Radius)
	case Rectangle:
		fmt.Printf("It's a Rectangle with dimensions: %.2f x %.2f\n", v.Width, v.Height)
	default:
		fmt.Printf("Unknown shape type: %T\n", v)
	}
}