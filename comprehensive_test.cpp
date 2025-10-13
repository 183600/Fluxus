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


class Animal {
    public:
        Animal(std::string name) {
            this->name = name;
        }
        std::string speak() {
    return to_str(this->name) + to_str(" makes a sound");
}


    public:
        std::string name = "";
};


class Dog : public Animal {
    public:
        template<typename... Args> Dog(Args... args) : Animal(args...) {}
        Dog() = default;
        std::string speak() {
    return to_str(this->name) + to_str(" barks");
}


    public:
        // generated placeholder fields
};


int add(int a, int b) {
    return a + b;
}


int multiply(int a, int b) {
    return a * b;
}


void count_up_to(int n) {
    auto i = 1;
    while (i <= n) {
        /* yield */
        i += 1;
    }
}


int main() {
    bool __fluxus_exc=false;
    auto x = 10;
    auto y = 20;
    std::cout << "=== Basic Arithmetic ===" << std::endl;
    std::cout << "Sum:" << " " << x + y << std::endl;
    std::cout << "Difference:" << " " << y - x << std::endl;
    std::cout << "Product:" << " " << x * y << std::endl;
    std::cout << "Quotient:" << " " << y / x << std::endl;
    std::cout << "Remainder:" << " " << y % x << std::endl;
    std::cout << "Power:" << " " << std::pow(x, 2) << std::endl;
    std::cout << "\n=== String Operations ===" << std::endl;
    auto name = "Alice";
    auto greeting = "Hello";
    auto message = to_str(greeting) + to_str(", ") + to_str(name) + to_str("!");
    std::cout << message << std::endl;
    std::cout << "Uppercase:" << " " << message.upper() << std::endl;
    std::cout << "Length:" << " " << len(message) << std::endl;
    std::cout << "\n=== List Operations ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << "Original list:" << " " << numbers << std::endl;
    numbers.push_back(6);
    std::cout << "After append:" << " " << numbers << std::endl;
    std::cout << "First element:" << " " << numbers[0] << std::endl;
    std::cout << "Last element:" << " " << numbers[-1] << std::endl;
    std::cout << "Slice:" << " " << numbers[1:4] << std::endl;
    std::cout << "\n=== Dictionary Operations ===" << std::endl;
    auto person = std::unordered_map<std::string, std::string>{{"name", "Bob"}, {"age", to_str(30)}, {"city", "New York"}};
    std::cout << "Person:" << " " << person << std::endl;
    std::cout << "Name:" << " " << person["name"] << std::endl;
    std::cout << "Keys:" << " " << list(person.keys()) << std::endl;
    std::cout << "Values:" << " " << list(person.values()) << std::endl;
    std::cout << "\n=== Control Flow ===" << std::endl;
    for (int i = 0; i < 3; ++i) {
    }
    auto count = 0;
    while (count < 3) {
    }
    std::cout << "\n=== Functions ===" << std::endl;
    std::cout << "add(5, 3):" << " " << add(5, 3) << std::endl;
    std::cout << "multiply(4):" << " " << multiply(4) << std::endl;
    std::cout << "multiply(4, 3):" << " " << multiply(4, 3) << std::endl;
    std::cout << "\n=== Classes ===" << std::endl;
    auto animal = Animal("Generic Animal");
    auto dog = Dog("Buddy");
    std::cout << animal.speak() << std::endl;
    std::cout << dog.speak() << std::endl;
    std::cout << "\n=== Exception Handling ===" << std::endl;
    {
    }
    if (__fluxus_exc) { auto e = 0;
    }
    else {
    }
    {
    }
    std::cout << "\n=== List Comprehensions ===" << std::endl;
    auto squares = 0;
    auto even_squares = 0;
    std::cout << "Squares:" << " " << squares << std::endl;
    std::cout << "Even squares:" << " " << even_squares << std::endl;
    std::cout << "\n=== File Operations ===" << std::endl;
    {
    }
    {
    }
    std::cout << "\n=== Lambda Functions ===" << std::endl;
    auto square = [](auto x) { return  x**2; };
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto squared = list(map(square, numbers));
    std::cout << "Original numbers:" << " " << numbers << std::endl;
    std::cout << "Squared numbers:" << " " << squared << std::endl;
    std::cout << "\n=== Generator ===" << std::endl;
    std::cout << "Counting to 5:" << std::endl;
    for (auto num : count_up_to(5)) {
    }
    std::cout << "\n=== Test Completed ===" << std::endl;
    return 0;
}

