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


int main() {
    auto text = "Hello, World!";
    auto name = "Alice";
    auto number = 42;
    auto greeting = text + " How are you?";
    auto full_name = "First " + name;
    std::cout << "Concatenated:" << " " << greeting << std::endl;
    std::cout << "Full name:" << " " << full_name << std::endl;
    std::cout << "\nString methods:" << std::endl;
    std::cout << "Uppercase:" << " " << text.upper() << std::endl;
    std::cout << "Lowercase:" << " " << text.lower() << std::endl;
    std::cout << "Strip spaces:" << " " << '  spaces  '.strip() << std::endl;
    std::cout << "Replace:" << " " << text.replace("World", "Python") << std::endl;
    std::cout << "Split:" << " " << text.split(",") << std::endl;
    std::cout << "Join:" << " " << "-".join(std::vector<std::string>{"a", "b", "c"}) << std::endl;
    std::cout << "\nString formatting:" << std::endl;
    std::cout << to_str("Name: ") << to_str(name) << to_str(", Number: ") << to_str(number) << std::endl;
    std::cout << "Name: {}, Number: {}".format(name, number) << std::endl;
    std::cout << "Name: %s, Number: %d" % (name, number) << std::endl;
    std::cout << "\nString slicing:" << std::endl;
    auto sample = "Python Programming";
    std::cout << "First 6 chars:" << " " << sample[:6] << std::endl;
    std::cout << "Last 11 chars:" << " " << sample[-11:] << std::endl;
    std::cout << "Middle:" << " " << sample[7:18] << std::endl;
    std::cout << "\nString operations:" << std::endl;
    std::cout << "Length:" << " " << len(text) << std::endl;
    std::cout << "Contains 'World':" << " " << "World" in text << std::endl;
    std::cout << "Starts with 'Hello':" << " " << text.startswith("Hello") << std::endl;
    std::cout << "Ends with '!':" << " " << text.endswith("!") << std::endl;
    auto multiline = """This is a;
    multiline string;
    with multiple lines""";
    std::cout << "\nMultiline string:" << std::endl;
    std::cout << multiline << std::endl;
    return 0;
}

