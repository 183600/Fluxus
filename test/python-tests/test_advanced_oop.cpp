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


class MetaClass : public type {
    public:
        template<typename... Args> MetaClass(Args... args) : type(args...) {}
        MetaClass() = default;
        std::string __new__(int cls, std::string name, int bases, std::vector<int> namespace) {
    auto namespace['meta_attribute'] = to_str("Created by ") + to_str(name);
    std::function<int()> class_info = [&]() {
        return to_str("Class ") + to_str(name) + to_str(" with meta_attribute: ") + to_str(cls.meta_attribute);
    };
    auto namespace['class_info'] = classmethod(class_info);
    return this->__new__(cls, name, bases, namespace);
}


    public:
        // generated placeholder fields
};


class AdvancedClass : public metaclass=MetaClass {
    public:
        AdvancedClass(int value) {
            this->value = value;
        }
        std::string get_value() {
    return this->value;
}


    public:
        int value = 0;
};


class Descriptor {
    public:
        Descriptor(std::string name, int default=None) {
            this->name = name;
            this->default = default;
        }
        std::string __get__(int obj, int objtype) {
    if (obj is 0) {
        return self;
    }
    return obj.__dict__.get(this->name, this->default);
}


        void __set__(int obj, std::string value) {
    std::cout << to_str("Setting ") << to_str(this->name) << to_str(" to ") << to_str(value) << std::endl;
    obj.__dict__[this->name](value);
}


        void __delete__(int obj) {
    if (this->name in obj.__dict__) {
        del obj.__dict__[this->name];
        std::cout << to_str("Deleted ") << to_str(this->name) << std::endl;
    }
}


    public:
        std::string name = "";
        int default = 0;
};


class PersonWithDescriptors {
    public:
        PersonWithDescriptors(std::string name, int age, int email) {
            this->name = name;
            this->age = age;
            this->email = email;
        }
        std::string __str__() {
    return to_str("Person(name=") + to_str(this->name) + to_str(", age=") + to_str(this->age) + to_str(", email=") + to_str(this->email) + to_str(")");
}


    public:
        std::string name = "";
        int age = 0;
        int email = 0;
};


class PropertyExample {
    public:
        PropertyExample(int temperature_celsius) {
            this->_temperature_celsius = temperature_celsius;
        }
        int temperature_celsius() {
    /* docstring */
    return this->_temperature_celsius;
}


        void temperature_celsius(int value) {
    /* docstring */
    if (value < -273.15) {
        /* raise */
    }
    this->_temperature_celsius = value;
}


        void temperature_celsius() {
    /* docstring */
    del this->_temperature_celsius;
}


        int temperature_fahrenheit() {
    /* docstring */
    return (this->_temperature_celsius * 9/5) + 32;
}


    public:
        int _temperature_celsius = 0;
};


class Singleton {
    public:
        Singleton() {
            this->data = std::vector<int>{};
            this->_initialized = true;
        }
        int __new__(int cls) {
    if (cls._instance is 0) {
        cls._instance(this->__new__(cls));
        cls._instance._initialized(false);
    }
    return cls._instance;
}


    public:
        std::vector<int> data;
        int _initialized = 0;
};


class BaseA {
    public:
        BaseA() = default;
        std::string method_a() {
    return "Method A from BaseA";
}


        std::string common_method() {
    return "Common method from BaseA";
}


    public:
        // generated placeholder fields
};


class BaseB {
    public:
        BaseB() = default;
        std::string method_b() {
    return "Method B from BaseB";
}


        std::string common_method() {
    return "Common method from BaseB";
}


    public:
        // generated placeholder fields
};


class DerivedMultiple : public BaseA, BaseB {
    public:
        template<typename... Args> DerivedMultiple(Args... args) : BaseA, BaseB(args...) {}
        DerivedMultiple() = default;
        std::string method_c() {
    return "Method C from DerivedMultiple";
}


        std::string common_method() {
    return "Common method from DerivedMultiple";
}


    public:
        // generated placeholder fields
};


class AbstractShape {
    public:
        AbstractShape() = default;
        void area() {
    /* raise */
}


        void perimeter() {
    /* raise */
}


    public:
        // generated placeholder fields
};


class ConcreteCircle : public AbstractShape {
    public:
        ConcreteCircle(int radius) {
            this->radius = radius;
        }
        int area() {
    return 3.14159 * std::pow(this->radius, 2);
}


        int perimeter() {
    return 2 * 3.14159 * this->radius;
}


    public:
        int radius = 0;
};


class DunderMethodsDemo {
    public:
        DunderMethodsDemo(int value) {
            this->value = value;
        }
        std::string __str__() {
    return to_str("DunderMethodsDemo(value=") + to_str(this->value) + to_str(")");
}


        std::string __repr__() {
    return to_str("DunderMethodsDemo(") + to_str(this->value) + to_str(")");
}


        std::string __len__() {
    return len(to_str(this->value));
}


        std::string __getitem__(int key) {
    if (true) {
        return to_str(this->value)[key];
    }
    else if (true) {
        return to_str(this->value)[key];
    }
    else {
        /* raise */
    }
}


        void __setitem__(int key, int value) {
    auto current = to_str(this->value);
    if (true) {
        this->value = current[:key] + to_str(value) + current[key+1:];
    }
    else {
        /* raise */
    }
}


        std::string __call__(int multiplier) {
    return this->value * multiplier;
}


        std::string __add__(int other) {
    return DunderMethodsDemo(this->value + other.value);
}


        std::string __eq__(int other) {
    return this->value == other.value;
}


        std::string __lt__(int other) {
    return this->value < other.value;
}


        int __enter__() {
    std::cout << "Entering context" << std::endl;
    return self;
}


        bool __exit__(int exc_type, int exc_val, int exc_tb) {
    std::cout << "Exiting context" << std::endl;
    return false;
}


    public:
        std::string value = "";
};


void test_metaclass() {
    std::cout << "=== Metaclass Test ===" << std::endl;
    auto obj = AdvancedClass(42);
    std::cout << to_str("Value: ") << to_str(obj.get_value()) << std::endl;
    std::cout << to_str("Meta attribute: ") << to_str(obj.meta_attribute) << std::endl;
    std::cout << to_str("Class info: ") << to_str(AdvancedClass.class_info()) << std::endl;
}


void test_descriptors() {
    std::cout << "\n=== Descriptors Test ===" << std::endl;
    auto person = PersonWithDescriptors("Alice", 25, "alice@example.com");
    std::cout << person << std::endl;
    person.name("Bob");
    std::cout << to_str("Updated name: ") << to_str(person.name) << std::endl;
    del person.age;
    std::cout << to_str("After deletion - age: ") << to_str(person.age) << std::endl;
}


void test_properties() {
    std::cout << "\n=== Properties Test ===" << std::endl;
    auto temp = PropertyExample(25);
    std::cout << to_str("Celsius: ") << to_str(temp.temperature_celsius) << std::endl;
    std::cout << to_str("Fahrenheit: ") << to_str(temp.temperature_fahrenheit) << std::endl;
    temp.temperature_celsius(30);
    std::cout << to_str("New Celsius: ") << to_str(temp.temperature_celsius) << std::endl;
    std::cout << to_str("New Fahrenheit: ") << to_str(temp.temperature_fahrenheit) << std::endl;
}


void test_singleton() {
    std::cout << "\n=== Singleton Test ===" << std::endl;
    auto s1 = Singleton();
    auto s2 = Singleton();
    std::cout << to_str("s1 is s2: ") << to_str(s1 is s2) << std::endl;
    s1.data.push_back("item1");
    std::cout << to_str("s2 data: ") << to_str(s2.data) << std::endl;
}


void test_multiple_inheritance() {
    std::cout << "\n=== Multiple Inheritance Test ===" << std::endl;
    auto obj = DerivedMultiple();
    std::cout << to_str("Method A: ") << to_str(obj.method_a()) << std::endl;
    std::cout << to_str("Method B: ") << to_str(obj.method_b()) << std::endl;
    std::cout << to_str("Method C: ") << to_str(obj.method_c()) << std::endl;
    std::cout << to_str("Common method: ") << to_str(obj.common_method()) << std::endl;
    std::cout << to_str("MRO: ") << to_str(DerivedMultiple.__mro__) << std::endl;
}


void test_abstract_classes() {
    std::cout << "\n=== Abstract Classes Test ===" << std::endl;
    auto circle = ConcreteCircle(5);
    std::cout << to_str("Circle area: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<circle.area(); return os.str(); }()) << std::endl;
    std::cout << to_str("Circle perimeter: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<circle.perimeter(); return os.str(); }()) << std::endl;
}


void test_dunder_methods() {
    std::cout << "\n=== Dunder Methods Test ===" << std::endl;
    auto obj1 = DunderMethodsDemo("hello");
    auto obj2 = DunderMethodsDemo("world");
    std::cout << to_str("String representation: ") << to_str(to_str(obj1)) << std::endl;
    std::cout << to_str("Repr: ") << to_str(repr(obj1)) << std::endl;
    std::cout << to_str("Length: ") << to_str(len(obj1)) << std::endl;
    std::cout << to_str("First character: ") << to_str(obj1[0]) << std::endl;
    auto obj3 = obj1 + obj2;
    std::cout << to_str("Addition result: ") << to_str(obj3) << std::endl;
    std::cout << to_str("obj1 == obj2: ") << to_str(obj1 == obj2) << std::endl;
    std::cout << to_str("obj1 < obj2: ") << to_str(obj1 < obj2) << std::endl;
    auto result = obj1(3);
    std::cout << to_str("Callable result: ") << to_str(result) << std::endl;
    {
        std::cout << to_str("Inside context: ") << to_str(context_obj) << std::endl;
    }
}


void test_class_methods() {
    std::cout << "\n=== Class Methods and Static Methods Test ===" << std::endl;
    class Example { public: Example() = default; };
    auto obj = Example.from_string("42");
    std::cout << to_str("Created from string: ") << to_str(obj.value) << std::endl;
    auto result = Example.utility_function(10, 20);
    std::cout << to_str("Static method result: ") << to_str(result) << std::endl;
    std::cout << to_str("Instance method: ") << to_str(obj.instance_method()) << std::endl;
}


int main() {
    test_metaclass();
    test_descriptors();
    test_properties();
    test_singleton();
    test_multiple_inheritance();
    test_abstract_classes();
    test_dunder_methods();
    test_class_methods();
    std::cout << "\n=== All advanced OOP tests passed! ===" << std::endl;
    return 0;
}

