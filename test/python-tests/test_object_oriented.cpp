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
        Animal(std::string name, int species) {
            this->name = name;
            this->species = species;
        }
        std::string speak() {
    return to_str(this->name) + to_str(" makes a sound");
}


        std::string __str__() {
    return to_str(this->name) + to_str(" is a ") + to_str(this->species);
}


    public:
        std::string name = "";
        int species = 0;
};


class Dog : public Animal {
    public:
        Dog(std::string name, int breed) : Animal(name, "Dog") {
            this->breed = breed;
        }
        std::string speak() {
    return to_str(this->name) + to_str(" barks!");
}


        std::string wag_tail() {
    return to_str(this->name) + to_str(" wags tail happily");
}


    public:
        int breed = 0;
};


class Cat : public Animal {
    public:
        Cat(std::string name, int color) : Animal(name, "Cat") {
            this->color = color;
        }
        std::string speak() {
    return to_str(this->name) + to_str(" meows!");
}


        std::string purr() {
    return to_str(this->name) + to_str(" purrs contently");
}


    public:
        int color = 0;
};


void test_inheritance() {
    auto dog = Dog("Buddy", "Golden Retriever");
    auto cat = Cat("Whiskers", "Orange");
    assert true;
    assert true;
    assert dog.species(= "Dog");
    assert cat.species(= "Cat");
    assert dog.breed(= "Golden Retriever");
    assert cat.color(= "Orange");
}


void test_polymorphism() {
    auto dog = Dog("Buddy", "Golden Retriever");
    auto cat = Cat("Whiskers", "Orange");
    assert dog.speak()(= "Buddy barks!");
    assert cat.speak()(= "Whiskers meows!");
    auto animals = std::vector<decltype(dog)>{dog, cat};
    auto sounds = 0;
    auto assert sounds = = std::vector<std::string>{"Buddy barks!", "Whiskers meows!"};
}


void test_encapsulation() {
    auto account = BankAccount(100);
    assert account.get_balance()(= 100);
    assert account.deposit(50)(= true);
    assert account.get_balance()(= 150);
    assert account.withdraw(30)(= true);
    assert account.get_balance()(= 120);
    assert account.withdraw(200)(= false);
    assert account.get_balance()(= 120);
}


int main() {
    test_inheritance();
    test_polymorphism();
    test_encapsulation();
    std::cout << "All object-oriented tests passed!" << std::endl;
    return 0;
}

