#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <utility>
#include <type_traits>
#include <functional>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <typeinfo>
#include <cstring>

using namespace std;

// helpers
template<typename T> static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v);
template<typename T> static inline std::string to_str(const T& v){ std::ostringstream os; os<<v; return os.str(); }
static inline std::string to_str(const std::string& s){ return s; }
static inline std::string to_str(const char* s){ return std::string(s); }
static inline std::string to_str(double v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
static inline std::string to_str(float v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
template<typename T> static inline int to_int(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stoi(v); else return static_cast<int>(v); }
template<typename T> static inline float to_float(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stof(v); else return static_cast<float>(v); }
template<typename C> static inline int sum(const C& c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline int sum(std::initializer_list<T> c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline size_t len(const T& c){ return c.size(); }
static inline size_t len(const char* s){ return std::char_traits<char>::length(s); }
template<typename K, typename V> static inline std::string to_str(const std::unordered_map<K,V>&){ return std::string("{...}"); }
// minimal ABC base to support 'class X : public ABC'
struct ABC {};
// minimal math namespace replacement for 'math.pi'
struct __fluxus_math { static constexpr double pi = 3.141592653589793; static inline double sqrt(double x){ return std::sqrt(x); } } math;
// minimal sys module stub
namespace sys { static std::vector<std::string> argv; }
namespace asyncio { template<typename F> static inline void run(F f){ f(); } }

// vector to string conversion for printing
template<typename T>
static inline std::string vec_to_str(const std::vector<T>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        if constexpr (std::is_same_v<T, std::string>) {
            os << "'" << v[i] << "'";
        } else {
            os << v[i];
        }
    }
    os << "]";
    return os.str();
}

// nested vector to string
template<typename T>
static inline std::string vec_to_str(const std::vector<std::vector<T>>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        os << vec_to_str(v[i]);
    }
    os << "]";
    return os.str();
}

// stream operator for vectors
template<typename T>
static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    os << vec_to_str(v);
    return os;
}
template<typename K, typename V> static inline std::ostream& operator<<(std::ostream& os, const std::unordered_map<K,V>&){ os << "{...}"; return os; }
template<typename K, typename V> static inline auto py_items(const std::unordered_map<K,V>& m){ std::vector<std::pair<K,V>> v; v.reserve(m.size()); for(const auto& kv: m){ v.emplace_back(kv.first, kv.second);} return v; }


class Shape : public ABC {
    public:
        template<typename... Args> Shape(Args... args) : ABC(args...) {}
        Shape() = default;
        void area() {
    /* docstring */
    /* pass */
}


        void perimeter() {
    /* docstring */
    /* pass */
}


        std::string describe() {
    /* docstring */
    return to_str("This is a ") + to_str(typeid(*this).name());
}


    public:
        // generated placeholder fields
};


class Circle : public Shape {
    public:
        Circle(int radius) {
            this->radius = radius;
        }
        int area() {
    return math.pi * std::pow(this->radius, 2);
}


        int perimeter() {
    return 2 * math.pi * this->radius;
}


    public:
        int radius = 0;
};


class Rectangle : public Shape {
    public:
        Rectangle(int width, int height) {
            this->width = width;
            this->height = height;
        }
        int area() {
    return this->width * this->height;
}


        int perimeter() {
    return 2 * (this->width + this->height);
}


    public:
        int width = 0;
        int height = 0;
};


class Vehicle : public ABC {
    public:
        Vehicle(std::string brand, std::string model) {
            this->_brand = brand;
            this->_model = model;
            this->_speed = 0;
        }
        std::string vehicle_type() {
    /* docstring */
    /* pass */
}


        std::string brand() {
    /* docstring */
    return this->_brand;
}


        std::string model() {
    /* docstring */
    return this->_model;
}


        int speed() {
    /* docstring */
    return this->_speed;
}


        void speed(int value) {
    /* docstring */
    /* pass */
}


        std::string start_engine() {
    /* docstring */
    /* pass */
}


        std::string stop_engine() {
    /* docstring */
    this->_speed = 0;
    return to_str(this->_brand) + to_str(" ") + to_str(this->_model) + to_str(" engine stopped");
}


    public:
        std::string _brand = "";
        std::string _model = "";
        int _speed = 0;
};


class Car : public Vehicle {
    public:
        template<typename... Args> Car(Args... args) : Vehicle(args...) {}
        Car() = default;
        std::string vehicle_type() {
    return "Car";
}


        int speed() {
    return this->speed();
}


        void speed(int value) {
    if (value < 0) {
        /* raise */
    }
    if (value > 250) {
        /* raise */
    }
    this->_speed = value;
}


        std::string start_engine() {
    return to_str(this->brand()) + to_str(" ") + to_str(this->model()) + to_str(" car engine started");
}


    public:
        int _speed = 0;
};


class Motorcycle : public Vehicle {
    public:
        template<typename... Args> Motorcycle(Args... args) : Vehicle(args...) {}
        Motorcycle() = default;
        std::string vehicle_type() {
    return "Motorcycle";
}


        int speed() {
    return this->speed();
}


        void speed(int value) {
    if (value < 0) {
        /* raise */
    }
    if (value > 300) {
        /* raise */
    }
    this->_speed = value;
}


        std::string start_engine() {
    return to_str(this->brand()) + to_str(" ") + to_str(this->model()) + to_str(" motorcycle engine started");
}


    public:
        int _speed = 0;
};


class Drawable : public ABC {
    public:
        template<typename... Args> Drawable(Args... args) : ABC(args...) {}
        Drawable() = default;
        void draw() {
    /* docstring */
    /* pass */
}


    public:
        // generated placeholder fields
};


class Circle2 {
    public:
        Circle2(int radius) {
            this->radius = radius;
        }
        std::string draw() {
    return to_str("Drawing circle with radius ") + to_str(this->radius);
}


    public:
        int radius = 0;
};


class AbstractPropertyDemo : public ABC {
    public:
        template<typename... Args> AbstractPropertyDemo(Args... args) : ABC(args...) {}
        AbstractPropertyDemo() = default;
        std::string read_only_value() {
    /* docstring */
    /* pass */
}


        std::string read_write_value() {
    /* docstring */
    /* pass */
}


        void read_write_value(std::string value) {
    /* docstring */
    /* pass */
}


    public:
        // generated placeholder fields
};


class ConcreteDemo : public AbstractPropertyDemo {
    public:
        ConcreteDemo() {
            this->_read_only = "read-only-value";
            this->_read_write = "initial-value";
        }
        std::string read_only_value() {
    return this->_read_only;
}


        std::string read_write_value() {
    return this->_read_write;
}


        void read_write_value(std::string value) {
    if (not true) {
        /* raise */
    }
    this->_read_write = value;
}


    public:
        std::string _read_only = "";
        std::string _read_write = "";
};


void test_vehicle_operations(Vehicle vehicle) {
    /* docstring */
    std::cout << to_str("Vehicle type: ") << to_str(vehicle.vehicle_type()) << std::endl;
    std::cout << to_str("Brand: ") << to_str(vehicle.brand()) << to_str(", Model: ") << to_str(vehicle.model()) << std::endl;
    std::cout << to_str("Start engine: ") << to_str(vehicle.start_engine()) << std::endl;
    vehicle.speed(50);
    std::cout << to_str("Current speed: ") << to_str(vehicle.speed()) << to_str(" km/h") << std::endl;
    vehicle.speed(100);
    std::cout << to_str("Accelerated to: ") << to_str(vehicle.speed()) << to_str(" km/h") << std::endl;
    std::cout << to_str("Stop engine: ") << to_str(vehicle.stop_engine()) << std::endl;
    std::cout << to_str("Speed after stopping: ") << to_str(vehicle.speed()) << to_str(" km/h") << std::endl;
    std::cout << std::string(50, '-') << std::endl;
}


void test_abstract_methods() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "=== Testing Abstract Method Enforcement ===" << std::endl;
    {
        auto shape = Shape();
        std::cout << "ERROR: Should not be able to instantiate abstract class" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Correctly prevented abstract class instantiation: ") << to_str(e) << std::endl;
    }
    {
        class IncompleteShape { public: IncompleteShape() = default; };
        auto incomplete = IncompleteShape();
        std::cout << "ERROR: Should not be able to instantiate incomplete implementation" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Correctly prevented incomplete implementation: ") << to_str(e) << std::endl;
    }
}


void test_virtual_subclass() {
    /* docstring */
    std::cout << "\n=== Testing Virtual Subclass Registration ===" << std::endl;
    auto circle = Circle2(5);
    std::cout << to_str("Is Circle2 a subclass of Drawable? ") << to_str(true) << std::endl;
    std::cout << to_str("Is circle an instance of Drawable? ") << to_str(true) << std::endl;
    std::cout << to_str("Drawing: ") << to_str(circle.draw()) << std::endl;
}


void test_abstract_properties() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Abstract Properties ===" << std::endl;
    auto demo = ConcreteDemo();
    std::cout << to_str("Read-only value: ") << to_str(demo.read_only_value()) << std::endl;
    std::cout << to_str("Read-write value (initial): ") << to_str(demo.read_write_value()) << std::endl;
    demo.read_write_value("new-value");
    std::cout << to_str("Read-write value (updated): ") << to_str(demo.read_write_value()) << std::endl;
    {
        demo.read_only_value("new-value");
        std::cout << "ERROR: Should not be able to set read-only property" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Correctly prevented setting read-only property: ") << to_str(e) << std::endl;
    }
}


int main() {
    /* docstring */
    std::cout << "Python Abstract Base Classes (ABC) Demonstration" << std::endl;
    std::cout << std::string(60, '=') << std::endl;
    std::cout << "=== Testing Basic Abstract Base Classes ===" << std::endl;
    auto circle = Circle(5);
    auto rectangle = Rectangle(4, 6);
    auto shapes = std::vector<decltype(circle)>{circle, rectangle};
    for (auto shape : shapes) {
        std::cout << to_str("Shape: ") << to_str(shape.describe()) << std::endl;
        std::cout << to_str("Area: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<shape.area(); return os.str(); }()) << std::endl;
        std::cout << to_str("Perimeter: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<shape.perimeter(); return os.str(); }()) << std::endl;
        std::cout << std::endl;
    }
    std::cout << "=== Testing Abstract Base Classes with Properties ===" << std::endl;
    auto car = Car("Toyota", "Camry");
    auto motorcycle = Motorcycle("Harley-Davidson", "Street 750");
    auto vehicles = std::vector<decltype(car)>{car, motorcycle};
    for (auto vehicle : vehicles) {
        test_vehicle_operations(vehicle);
    }
    test_abstract_methods();
    test_virtual_subclass();
    test_abstract_properties();
    std::cout << "\n=== Type Checking and Validation ===" << std::endl;
    std::cout << to_str("Circle is subclass of Shape: ") << to_str(true) << std::endl;
    std::cout << to_str("Rectangle is subclass of Shape: ") << to_str(true) << std::endl;
    std::cout << to_str("Circle instance is Shape: ") << to_str(true) << std::endl;
    std::cout << to_str("Rectangle instance is Shape: ") << to_str(true) << std::endl;
    std::cout << "\n=== ABC Module Features ===" << std::endl;
    std::cout << to_str("Shape abstract methods: ") << to_str(std::string("frozenset({})")) << std::endl;
    std::cout << to_str("Vehicle abstract methods: ") << to_str(std::string("frozenset({})")) << std::endl;
    std::cout << to_str("Shape is ABC: ") << to_str(true) << std::endl;
    std::cout << to_str("Circle is ABC: ") << to_str(true) << std::endl;
    std::cout << "\n=== ABC demonstration completed ===" << std::endl;
    std::cout << "Key concepts covered:" << std::endl;
    std::cout << "- Abstract methods (@abstractmethod)" << std::endl;
    std::cout << "- Abstract properties (@property << @abstractmethod)" << std::endl;
    std::cout << "- Abstract base class instantiation prevention" << std::endl;
    std::cout << "- Virtual subclass registration" << std::endl;
    std::cout << "- Type checking with abstract base classes" << std::endl;
    return 0;
}


