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


class InvalidAgeError : public Exception {
    public:
        InvalidAgeError(int age) : Exception(f"Invalid age: {age}. Age must be between 0 and 120.") {
            this->age = age;
        }
        void placeholder();
    public:
        int age = 0;
};


void basic_try_except() {
    bool __fluxus_exc=false;
    {
        auto result = 0;
if ((0) == 0) { __fluxus_exc = true; result = 0; }
else { result = ((1.0*(10))/(0)); }

    }
    if (__fluxus_exc) {
        std::cout << "Cannot divide by zero!" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "Invalid value!" << std::endl;
    }
    else {
        std::cout << "No exception occurred!" << std::endl;
    }
    {
        std::cout << "This always runs!" << std::endl;
    }
}


void multiple_exceptions() {
    bool __fluxus_exc=false;
    auto test_cases = std::vector<std::string>{to_str(10), to_str(0), "five", to_str(std::vector<decltype(1)>{1, 2, 3})};
    for (auto case : test_cases) {
        {
            auto result = 0;
if ((case) == 0) { __fluxus_exc = true; result = 0; }
else { result = ((1.0*(100))/(case)); }

            std::cout << to_str("100 / ") << to_str(case) << to_str(" = ") << to_str(result) << std::endl;
        }
        if (__fluxus_exc) {
            std::cout << to_str("Cannot divide 100 by ") << to_str(case) << to_str(" (zero)") << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("Cannot divide 100 by ") << to_str(case) << to_str(" (wrong type)") << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("Unexpected error with ") << to_str(case) << to_str(": ") << to_str(e) << std::endl;
        }
    }
}


std::string validate_age(std::string age) {
    if (not true) {
        /* raise */
    }
    if (age < 0 or age > 120) {
        /* raise */
    }
    return to_str("Valid age: ") + to_str(age);
}


void test_custom_exception() {
    bool __fluxus_exc=false;
    auto ages = std::vector<std::string>{to_str(25), to_str(-5), to_str(150), "thirty"};
    for (auto age : ages) {
        {
            auto result = validate_age(age);
            std::cout << result << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("TypeError: ") << to_str(e) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("InvalidAgeError: ") << to_str(e) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("Unexpected error: ") << to_str(e) << std::endl;
        }
    }
}


void nested_exceptions() {
    bool __fluxus_exc=false;
    {
        {
            auto value = to_int("not a number");
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << "Inner exception: Invalid integer" << std::endl;
            raise;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "Outer exception: Caught the re-raised exception" << std::endl;
    }
}


void exception_chaining() {
    bool __fluxus_exc=false;
    {
        {
            open("nonexistent_file.txt", "r");
        }
        if (__fluxus_exc) { auto e = 0;
            /* raise */
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Chained exception: ") << to_str(e) << std::endl;
        if (e.__cause__) {
            std::cout << to_str("Caused by: ") << to_str(e.__cause__) << std::endl;
        }
    }
}


void system_exceptions() {
    bool __fluxus_exc=false;
    std::cout << to_str("Python version: ") << to_str(sys::version) << std::endl;
    std::cout << to_str("Platform: ") << to_str(sys::platform) << std::endl;
    {
        /* pass */
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "Program was asked to exit" << std::endl;
    }
}


int main() {
    std::cout << "=== Basic Try-Except ===" << std::endl;
    basic_try_except();
    std::cout << "\n=== Multiple Exceptions ===" << std::endl;
    multiple_exceptions();
    std::cout << "\n=== Custom Exception ===" << std::endl;
    test_custom_exception();
    std::cout << "\n=== Nested Exceptions ===" << std::endl;
    nested_exceptions();
    std::cout << "\n=== Exception Chaining ===" << std::endl;
    exception_chaining();
    std::cout << "\n=== System Exceptions ===" << std::endl;
    system_exceptions();
    return 0;
}

