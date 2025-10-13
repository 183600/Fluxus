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


class BasicClass {
    public:
        BasicClass(std::string name, int age=0) {
            this->name = name;
            this->age = age;
        }
        std::string instance_method() {
    return to_str("Hello, I'm ") + to_str(this->name);
}


        std::string class_method(int cls) {
    return to_str("Class method called");
}


        std::string static_method() {
    return "Static method called";
}


    public:
        std::string name = "";
        int age = 0;
};


std::string basic_function(std::string x, std::string y) {
    return to_str(y) + to_str(": ") + to_str(x);
}


int main() {
    std::cout << "=== Simple Python Syntax Demo ===" << std::endl;
    std::cout << to_str("Basic variables: ") << to_str(integer_var) << to_str(", ") << to_str(float_var) << to_str(", ") << to_str(string_var) << std::endl;
    std::cout << to_str("Collections: ") << to_str(list_var) << to_str(", ") << to_str(dict_var) << std::endl;
    std::cout << to_str("List comprehension: ") << to_str(squares) << std::endl;
    std::cout << to_str("Function result: ") << to_str(basic_function(42, 'test')) << std::endl;
    auto obj = BasicClass("Test Object", 25);
    std::cout << to_str("Object: ") << to_str(obj.name) << to_str(", ") << to_str(obj.age) << std::endl;
    std::cout << to_str("Instance method: ") << to_str(obj.instance_method()) << std::endl;
    std::cout << to_str("Class method: ") << to_str(BasicClass.class_method()) << std::endl;
    std::cout << to_str("Static method: ") << to_str(BasicClass.static_method()) << std::endl;
    std::cout << "=== Demo completed ===" << std::endl;
    return 0;
}


