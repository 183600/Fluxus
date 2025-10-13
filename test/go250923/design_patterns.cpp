#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <functional>

using namespace std;

class Singleton {
    public:
        std::string data;
};

class CircleFactory {
    public:
        int func;
        int return;
};

class RectangleFactory {
    public:
        int func;
        int return;
};

class Circle {
    public:
        int radius;
};

class Rectangle {
    public:
        int width,;
};

class WeatherStation {
    public:
        int observers;
        int temperature;
};

class PhoneDisplay {
    public:
        std::string name;
};

class ComputerDisplay {
    public:
        std::string name;
};

class Light {
    public:
        bool isOn;
};

class TurnOnCommand {
    public:
        int light;
};

class TurnOffCommand {
    public:
        int light;
};

class RemoteControl {
    public:
        int command;
};

class EmailNotifier {
    public:
        int func;
        int fmt.Printf("Email;
};

class SMSNotifier {
    public:
        int next;
};

class PushNotifier {
    public:
        int next;
};

class ObjectPool {
    public:
        int pool;
};

class PooledObject {
    public:
        int id;
        std::string data;
};

class CreditCardStrategy {
    public:
        std::string cardNumber;
};

class PayPalStrategy {
    public:
        std::string email;
};

class CryptoStrategy {
    public:
        std::string wallet;
};

class PaymentContext {
    public:
        int strategy;
};

int GetInstance() {
    once.Do(func() {;
    instance = &Singleton{data: "Singleton instance created"};
    });
    return instance;
}


std::string Circle::Draw() {
    return fmt.Sprintf("Drawing circle with radius: %.2f", c.radius);
}


int Circle::GetArea() {
    return 3.14159 * c.radius * c.radius;
}


std::string Rectangle::Draw() {
    return fmt.Sprintf("Drawing rectangle with width: %.2f, height: %.2f", r.width, r.height);
}


int Rectangle::GetArea() {
    return r.width * r.height;
}


void WeatherStation::RegisterObserver(int observer) {
    ws.observers = append(ws.observers, observer);
}


void WeatherStation::RemoveObserver(int observer) {
    for i, obs := range ws.observers {;
    if obs == observer {;
    ws.observers = append(ws.observers[:i], ws.observers[i+1:]...);
    break;
    };
    };
}


void WeatherStation::NotifyObservers() {
    for _, observer := range ws.observers {;
    observer.Update(fmt.Sprintf("Temperature is %.2f°C", ws.temperature));
    };
}


void WeatherStation::SetTemperature(int temp) {
    ws.temperature = temp;
    fmt.Printf("WeatherStation: Temperature updated to %.2f°C\n", temp);
    ws.NotifyObservers();
}


void PhoneDisplay::Update(std::string message) {
    fmt.Printf("%s Display: %s\n", pd.name, message);
}


void ComputerDisplay::Update(std::string message) {
    fmt.Printf("%s Display: %s\n", cd.name, message);
}


void Light::TurnOn() {
    l.isOn = true;
    fmt.Println("Light is ON");
}


void Light::TurnOff() {
    l.isOn = false;
    fmt.Println("Light is OFF");
}


void TurnOnCommand::Execute() {
    c.light.TurnOn();
}


void TurnOffCommand::Execute() {
    c.light.TurnOff();
}


void RemoteControl::SetCommand(int command) {
    rc.command = command;
}


void RemoteControl::PressButton() {
    rc.command.Execute();
}


void SMSNotifier::Send(std::string message) {
    fmt.Printf("SMS sent: %s\n", message);
    if n.next != nil {;
    n.next.Send(message);
    };
}


void PushNotifier::Send(std::string message) {
    fmt.Printf("Push notification sent: %s\n", message);
    if n.next != nil {;
    n.next.Send(message);
    };
}


int NewObjectPool(int size) {
    pool := make(chan *PooledObject, size);
    for i := 0; i < size; i++ {;
    pool <- &PooledObject{id: i, data: fmt.Sprintf("Object%d", i)};
    };
    return &ObjectPool{pool: pool};
}


int ObjectPool::Acquire() {
    select {;
    case obj := <-op.pool:;
    return obj;
    default:;
    return &PooledObject{id: -1, data: "New Object"};
    };
}


void ObjectPool::Release(int obj) {
    select {;
    case op.pool <- obj:;
    fmt.Printf("Object %d returned to pool\n", obj.id);
    default:;
    fmt.Printf("Pool full, object %d discarded\n", obj.id);
    };
}


int main() {
    fmt.Println("=== Design Patterns in Go ===");
    fmt.Println("\n--- Singleton Pattern ---");
    singleton1 := GetInstance();
    singleton2 := GetInstance();
    fmt.Printf("Singleton 1: %p, Data: %s\n", singleton1, singleton1.data);
    fmt.Printf("Singleton 2: %p, Data: %s\n", singleton2, singleton2.data);
    fmt.Printf("Are they the same instance? %t\n", singleton1 == singleton2);
    fmt.Println("\n--- Factory Pattern ---");
    factories := []ShapeFactory{;
    &CircleFactory{},;
    &RectangleFactory{},;
    };
    for i, factory := range factories {;
    shape := factory.CreateShape();
    fmt.Printf("Factory %d: %s, Area: %.2f\n", i+1, shape.Draw(), shape.GetArea());
    };
    fmt.Println("\n--- Observer Pattern ---");
    weatherStation := &WeatherStation{};
    phoneDisplay := &PhoneDisplay{name: "Phone"};
    computerDisplay := &ComputerDisplay{name: "Computer"};
    weatherStation.RegisterObserver(phoneDisplay);
    weatherStation.RegisterObserver(computerDisplay);
    weatherStation.SetTemperature(25.5);
    weatherStation.SetTemperature(30.0);
    weatherStation.RemoveObserver(phoneDisplay);
    weatherStation.SetTemperature(22.0);
    fmt.Println("\n--- Command Pattern ---");
    light := &Light{};
    turnOn := &TurnOnCommand{light: light};
    turnOff := &TurnOffCommand{light: light};
    remote := &RemoteControl{};
    remote.SetCommand(turnOn);
    remote.PressButton();
    remote.SetCommand(turnOff);
    remote.PressButton();
    fmt.Println("\n--- Chain of Responsibility Pattern ---");
    emailNotifier := &EmailNotifier{};
    smsNotifier := &SMSNotifier{next: emailNotifier};
    pushNotifier := &PushNotifier{next: smsNotifier};
    fmt.Println("Sending notification through full chain:");
    pushNotifier.Send("System maintenance at 2:00 AM");
    fmt.Println("\n--- Object Pool Pattern ---");
    pool := NewObjectPool(3);
    obj1 := pool.Acquire();
    obj2 := pool.Acquire();
    obj3 := pool.Acquire();
    obj4 := pool.Acquire();
    fmt.Printf("Acquired: %s\n", obj1.data);
    fmt.Printf("Acquired: %s\n", obj2.data);
    fmt.Printf("Acquired: %s\n", obj3.data);
    fmt.Printf("Acquired: %s\n", obj4.data);
    pool.Release(obj1);
    pool.Release(obj2);
    obj5 := pool.Acquire();
    fmt.Printf("Acquired after release: %s\n", obj5.data);
    fmt.Println("\n--- Strategy Pattern ---");
    strategies := []PaymentStrategy{;
    &CreditCardStrategy{cardNumber: "1234-5678-9012-3456"},;
    &PayPalStrategy{email: "user@example.com"},;
    &CryptoStrategy{wallet: "0x1234567890abcdef"},;
    };
    context := &PaymentContext{};
    for i, strategy := range strategies {;
    context.SetStrategy(strategy);
    fmt.Printf("Payment %d: ", i+1);
    context.Pay(100.0);
    };
}


void CreditCardStrategy::Pay(int amount) {
    fmt.Printf("Paid $%.2f using Credit Card (%s)\n", amount, s.cardNumber);
}


void PayPalStrategy::Pay(int amount) {
    fmt.Printf("Paid $%.2f using PayPal (%s)\n", amount, s.email);
}


void CryptoStrategy::Pay(int amount) {
    fmt.Printf("Paid $%.2f using Cryptocurrency (%s)\n", amount, s.wallet);
}


void PaymentContext::SetStrategy(int strategy) {
    c.strategy = strategy;
}


void PaymentContext::Pay(int amount) {
    c.strategy.Pay(amount);
}


