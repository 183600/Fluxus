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


class Point {
    public:
        Point(int x, int y) {
            this->x = x;
            this->y = y;
        }
        std::string __str__() {
    return to_str("Poto_int((") + to_str(this->x) + to_str(", ") + to_str(this->y) + to_str(")");
}


        std::string __repr__() {
    return to_str("Poto_int((x=") + to_str(this->x) + to_str(", y=") + to_str(this->y) + to_str(")");
}


    public:
        int x = 0;
        int y = 0;
};


class Person {
    public:
        Person(std::string name, int age) {
            this->name = name;
            this->age = age;
        }
        int __eq__(int other) {
    return this->age == other.age;
}


        int __lt__(int other) {
    return this->age < other.age;
}


        int __le__(int other) {
    return this->age <= other.age;
}


        int __gt__(int other) {
    return this->age > other.age;
}


        int __ge__(int other) {
    return this->age >= other.age;
}


        int __ne__(int other) {
    return this->age != other.age;
}


    public:
        std::string name = "";
        int age = 0;
};


class Vector {
    public:
        Vector(int x, int y) {
            this->x = x;
            this->y = y;
        }
        int __add__(int other) {
    return Vector(this->x + other.x, this->y + other.y);
}


        int __sub__(int other) {
    return Vector(this->x - other.x, this->y - other.y);
}


        int __mul__(int scalar) {
    return Vector(this->x * scalar, this->y * scalar);
}


        int __truediv__(int scalar) {
    return Vector(this->x / scalar, this->y / scalar);
}


        std::string __str__() {
    return to_str("Vector(") + to_str(this->x) + to_str(", ") + to_str(this->y) + to_str(")");
}


    public:
        int x = 0;
        int y = 0;
};


class MyList {
    public:
        MyList(int items) {
            this->items = items;
        }
        int __len__() {
    return len(this->items);
}


        int __getitem__(int index) {
    return this->items[index];
}


        void __setitem__(int index, int value) {
    this->items[index] = value;
}


        int __contains__(int item) {
    return item in this->items;
}


    public:
        int items = 0;
        int items[index] = 0;
};


class Multiplier {
    public:
        Multiplier(int factor) {
            this->factor = factor;
        }
        int __call__(int x) {
    return x * this->factor;
}


    public:
        int factor = 0;
};


class FileManager {
    public:
        FileManager(std::string filename) {
            this->filename = filename;
            this->file = "";
        }
        int __enter__() {
    std::cout << to_str("  Opening ") << to_str(this->filename) << std::endl;
    return self;
}


        bool __exit__(int exc_type, int exc_val, int exc_tb) {
    std::cout << to_str("  Closing ") << to_str(this->filename) << std::endl;
    return false;
}


    public:
        std::string filename = "";
        std::string file = "";
};


class Counter {
    public:
        Counter(int start, int end) {
            this->current = start;
            this->end = end;
        }
        int __iter__() {
    return self;
}


        int __next__() {
    if (this->current >= this->end) {
        /* raise */
    }
    this->current += 1;
    return this->current - 1;
}


    public:
        int current = 0;
        int end = 0;
};


class HashablePoint {
    public:
        HashablePoint(int x, int y) {
            this->x = x;
            this->y = y;
        }
        int __hash__() {
    return hash((this->x, this->y));
}


        int __eq__(int other) {
    return this->x == other.x and this->y == other.y;
}


    public:
        int x = 0;
        int y = 0;
};


class Empty {
    public:
        Empty(int items) {
            this->items = items;
        }
        int __bool__() {
    return len(this->items) > 0;
}


    public:
        int items = 0;
};


class FormattedNumber {
    public:
        FormattedNumber(int value) {
            this->value = value;
        }
        std::string __format__(int format_spec) {
    if (format_spec == "hex") {
        return hex(this->value);
    }
    else if (format_spec == "bin") {
        return bin(this->value);
    }
    else {
        return to_str(this->value);
    }
}


    public:
        int value = 0;
};


int main() {
    std::cout << "=== __str__ and __repr__ ===" << std::endl;
    auto p = Poto_int(3, 4);
    std::cout << to_str("str(p): ") << to_str(to_str(p)) << std::endl;
    std::cout << to_str("repr(p): ") << to_str(repr(p)) << std::endl;
    std::cout << "\n=== Comparison Magic Methods ===" << std::endl;
    auto alice = Person("Alice", 25);
    auto bob = Person("Bob", 30);
    auto charlie = Person("Charlie", 25);
    std::cout << to_str("alice == charlie: ") << to_str(alice == charlie) << std::endl;
    std::cout << to_str("alice != bob: ") << to_str(alice != bob) << std::endl;
    std::cout << to_str("alice < bob: ") << to_str(alice < bob) << std::endl;
    std::cout << to_str("bob > alice: ") << to_str(bob > alice) << std::endl;
    std::cout << to_str("alice <= charlie: ") << to_str(alice <= charlie) << std::endl;
    std::cout << "\n=== Arithmetic Magic Methods ===" << std::endl;
    auto v1 = Vector(1, 2);
    auto v2 = Vector(3, 4);
    auto v3 = v1 + v2;
    std::cout << to_str("v1 << v2 = ") << to_str(v3) << std::endl;
    auto v4 = v2 - v1;
    std::cout << to_str("v2 - v1 = ") << to_str(v4) << std::endl;
    auto v5 = v1 * 3;
    std::cout << to_str("v1 * 3 = ") << to_str(v5) << std::endl;
    auto v6 = v2 / 2;
    std::cout << to_str("v2 / 2 = ") << to_str(v6) << std::endl;
    std::cout << "\n=== Container Magic Methods ===" << std::endl;
    auto my_list = MyList(std::vector<decltype(1)>{1, 2, 3, 4, 5});
    std::cout << to_str("len(my_list): ") << to_str(len(my_list)) << std::endl;
    std::cout << to_str("my_list[2]: ") << to_str(my_list[2]) << std::endl;
    std::cout << to_str("3 in my_list: ") << to_str(3 in my_list) << std::endl;
    std::cout << to_str("10 in my_list: ") << to_str(10 in my_list) << std::endl;
    auto my_list[1] = 20;
    std::cout << to_str("After setting my_list[1] = 20: ") << to_str(my_list[1]) << std::endl;
    std::cout << "\n=== __call__ Magic Method ===" << std::endl;
    auto times_three = Multiplier(3);
    auto result = times_three(5);
    std::cout << to_str("times_three(5) = ") << to_str(result) << std::endl;
    std::cout << "\n=== Context Manager Magic Methods ===" << std::endl;
    {
    }
    std::cout << "\n=== Iterator Magic Methods ===" << std::endl;
    std::cout << "Counter from 0 to 5:" << std::endl;
    for (auto num : Counter(0, 5)) {
    }
    std::cout << "\n=== __hash__ Magic Method ===" << std::endl;
    auto p1 = HashablePoto_int(1, 2);
    auto p2 = HashablePoto_int(1, 2);
    auto p3 = HashablePoto_int(3, 4);
    auto point_dict = std::unordered_map<std::string, std::string>{{"p1", "Point 1"}, {"p3", "Point 3"}};
    std::cout << to_str("Using hashable point as dict key: ") << to_str(point_dict[p2]) << std::endl;
    std::cout << "\n=== __bool__ Magic Method ===" << std::endl;
    auto empty = Empty([]);
    auto not_empty = Empty(std::vector<decltype(1)>{1, 2, 3});
    std::cout << to_str("bool(empty): ") << to_str(bool(empty)) << std::endl;
    std::cout << to_str("bool(not_empty): ") << to_str(bool(not_empty)) << std::endl;
    std::cout << "\n=== __format__ Magic Method ===" << std::endl;
    auto num = FormattedNumber(42);
    std::cout << to_str("Default: ") << to_str(num) << std::endl;
    std::cout << to_str("Hex: ") << to_str(num) << std::endl;
    std::cout << to_str("Binary: ") << to_str(num) << std::endl;
    std::cout << "\n=== All magic method tests completed ===" << std::endl;
    return 0;
}

