package main

import (
	"fmt"
	"sync"
)

type Singleton struct {
	data string
}

var (
	instance *Singleton
	once     sync.Once
)

func GetInstance() *Singleton {
	once.Do(func() {
		instance = &Singleton{data: "Singleton instance created"}
	})
	return instance
}

type ShapeFactory interface {
	CreateShape() Shape
}

type CircleFactory struct{}

func (f *CircleFactory) CreateShape() Shape {
	return Circle{radius: 5}
}

type RectangleFactory struct{}

func (f *RectangleFactory) CreateShape() Shape {
	return Rectangle{width: 4, height: 6}
}

type Shape interface {
	Draw() string
	GetArea() float64
}

type Circle struct {
	radius float64
}

func (c Circle) Draw() string {
	return fmt.Sprintf("Drawing circle with radius: %.2f", c.radius)
}

func (c Circle) GetArea() float64 {
	return 3.14159 * c.radius * c.radius
}

type Rectangle struct {
	width, height float64
}

func (r Rectangle) Draw() string {
	return fmt.Sprintf("Drawing rectangle with width: %.2f, height: %.2f", r.width, r.height)
}

func (r Rectangle) GetArea() float64 {
	return r.width * r.height
}

type Subject interface {
	RegisterObserver(observer Observer)
	RemoveObserver(observer Observer)
	NotifyObservers()
}

type Observer interface {
	Update(message string)
}

type WeatherStation struct {
	observers []Observer
	temperature float64
}

func (ws *WeatherStation) RegisterObserver(observer Observer) {
	ws.observers = append(ws.observers, observer)
}

func (ws *WeatherStation) RemoveObserver(observer Observer) {
	for i, obs := range ws.observers {
		if obs == observer {
			ws.observers = append(ws.observers[:i], ws.observers[i+1:]...)
			break
		}
	}
}

func (ws *WeatherStation) NotifyObservers() {
	for _, observer := range ws.observers {
		observer.Update(fmt.Sprintf("Temperature is %.2f°C", ws.temperature))
	}
}

func (ws *WeatherStation) SetTemperature(temp float64) {
	ws.temperature = temp
	fmt.Printf("WeatherStation: Temperature updated to %.2f°C\n", temp)
	ws.NotifyObservers()
}

type PhoneDisplay struct {
	name string
}

func (pd *PhoneDisplay) Update(message string) {
	fmt.Printf("%s Display: %s\n", pd.name, message)
}

type ComputerDisplay struct {
	name string
}

func (cd *ComputerDisplay) Update(message string) {
	fmt.Printf("%s Display: %s\n", cd.name, message)
}

type Command interface {
	Execute()
}

type Light struct {
	isOn bool
}

func (l *Light) TurnOn() {
	l.isOn = true
	fmt.Println("Light is ON")
}

func (l *Light) TurnOff() {
	l.isOn = false
	fmt.Println("Light is OFF")
}

type TurnOnCommand struct {
	light *Light
}

func (c *TurnOnCommand) Execute() {
	c.light.TurnOn()
}

type TurnOffCommand struct {
	light *Light
}

func (c *TurnOffCommand) Execute() {
	c.light.TurnOff()
}

type RemoteControl struct {
	command Command
}

func (rc *RemoteControl) SetCommand(command Command) {
	rc.command = command
}

func (rc *RemoteControl) PressButton() {
	rc.command.Execute()
}

type Notifier interface {
	Send(message string)
}

type EmailNotifier struct{}

func (n *EmailNotifier) Send(message string) {
	fmt.Printf("Email sent: %s\n", message)
}

type SMSNotifier struct {
	next Notifier
}

func (n *SMSNotifier) Send(message string) {
	fmt.Printf("SMS sent: %s\n", message)
	if n.next != nil {
		n.next.Send(message)
	}
}

type PushNotifier struct {
	next Notifier
}

func (n *PushNotifier) Send(message string) {
	fmt.Printf("Push notification sent: %s\n", message)
	if n.next != nil {
		n.next.Send(message)
	}
}

type ObjectPool struct {
	pool chan *PooledObject
}

type PooledObject struct {
	id   int
	data string
}

func NewObjectPool(size int) *ObjectPool {
	pool := make(chan *PooledObject, size)
	for i := 0; i < size; i++ {
		pool <- &PooledObject{id: i, data: fmt.Sprintf("Object%d", i)}
	}
	return &ObjectPool{pool: pool}
}

func (op *ObjectPool) Acquire() *PooledObject {
	select {
	case obj := <-op.pool:
		return obj
	default:
		return &PooledObject{id: -1, data: "New Object"}
	}
}

func (op *ObjectPool) Release(obj *PooledObject) {
	select {
	case op.pool <- obj:
		fmt.Printf("Object %d returned to pool\n", obj.id)
	default:
		fmt.Printf("Pool full, object %d discarded\n", obj.id)
	}
}

func main() {
	fmt.Println("=== Design Patterns in Go ===")

	fmt.Println("\n--- Singleton Pattern ---")
	singleton1 := GetInstance()
	singleton2 := GetInstance()
	fmt.Printf("Singleton 1: %p, Data: %s\n", singleton1, singleton1.data)
	fmt.Printf("Singleton 2: %p, Data: %s\n", singleton2, singleton2.data)
	fmt.Printf("Are they the same instance? %t\n", singleton1 == singleton2)

	fmt.Println("\n--- Factory Pattern ---")
	factories := []ShapeFactory{
		&CircleFactory{},
		&RectangleFactory{},
	}

	for i, factory := range factories {
		shape := factory.CreateShape()
		fmt.Printf("Factory %d: %s, Area: %.2f\n", i+1, shape.Draw(), shape.GetArea())
	}

	fmt.Println("\n--- Observer Pattern ---")
	weatherStation := &WeatherStation{}

	phoneDisplay := &PhoneDisplay{name: "Phone"}
	computerDisplay := &ComputerDisplay{name: "Computer"}

	weatherStation.RegisterObserver(phoneDisplay)
	weatherStation.RegisterObserver(computerDisplay)

	weatherStation.SetTemperature(25.5)
	weatherStation.SetTemperature(30.0)

	weatherStation.RemoveObserver(phoneDisplay)
	weatherStation.SetTemperature(22.0)

	fmt.Println("\n--- Command Pattern ---")
	light := &Light{}
	turnOn := &TurnOnCommand{light: light}
	turnOff := &TurnOffCommand{light: light}

	remote := &RemoteControl{}

	remote.SetCommand(turnOn)
	remote.PressButton()

	remote.SetCommand(turnOff)
	remote.PressButton()

	fmt.Println("\n--- Chain of Responsibility Pattern ---")
	emailNotifier := &EmailNotifier{}
	smsNotifier := &SMSNotifier{next: emailNotifier}
	pushNotifier := &PushNotifier{next: smsNotifier}

	fmt.Println("Sending notification through full chain:")
	pushNotifier.Send("System maintenance at 2:00 AM")

	fmt.Println("\n--- Object Pool Pattern ---")
	pool := NewObjectPool(3)

	obj1 := pool.Acquire()
	obj2 := pool.Acquire()
	obj3 := pool.Acquire()
	obj4 := pool.Acquire()

	fmt.Printf("Acquired: %s\n", obj1.data)
	fmt.Printf("Acquired: %s\n", obj2.data)
	fmt.Printf("Acquired: %s\n", obj3.data)
	fmt.Printf("Acquired: %s\n", obj4.data)

	pool.Release(obj1)
	pool.Release(obj2)

	obj5 := pool.Acquire()
	fmt.Printf("Acquired after release: %s\n", obj5.data)

	fmt.Println("\n--- Strategy Pattern ---")
	strategies := []PaymentStrategy{
		&CreditCardStrategy{cardNumber: "1234-5678-9012-3456"},
		&PayPalStrategy{email: "user@example.com"},
		&CryptoStrategy{wallet: "0x1234567890abcdef"},
	}

	context := &PaymentContext{}

	for i, strategy := range strategies {
		context.SetStrategy(strategy)
		fmt.Printf("Payment %d: ", i+1)
		context.Pay(100.0)
	}
}

type PaymentStrategy interface {
	Pay(amount float64)
}

type CreditCardStrategy struct {
	cardNumber string
}

func (s *CreditCardStrategy) Pay(amount float64) {
	fmt.Printf("Paid $%.2f using Credit Card (%s)\n", amount, s.cardNumber)
}

type PayPalStrategy struct {
	email string
}

func (s *PayPalStrategy) Pay(amount float64) {
	fmt.Printf("Paid $%.2f using PayPal (%s)\n", amount, s.email)
}

type CryptoStrategy struct {
	wallet string
}

func (s *CryptoStrategy) Pay(amount float64) {
	fmt.Printf("Paid $%.2f using Cryptocurrency (%s)\n", amount, s.wallet)
}

type PaymentContext struct {
	strategy PaymentStrategy
}

func (c *PaymentContext) SetStrategy(strategy PaymentStrategy) {
	c.strategy = strategy
}

func (c *PaymentContext) Pay(amount float64) {
	c.strategy.Pay(amount)
}