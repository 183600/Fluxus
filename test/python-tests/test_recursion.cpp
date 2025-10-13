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


int factorial(int n) {
    if (n == 0 or n == 1) {
        return 1;
    }
    else {
        return n * factorial(n - 1);
    }
}


int fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    else {
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}


int power(int base, int exp) {
    if (exp == 0) {
        return 1;
    }
    else if (exp == 1) {
        return base;
    }
    else if (exp < 0) {
        return 1 / power(base, -exp);
    }
    else {
        return base * power(base, exp - 1);
    }
}


std::string binary_search(std::vector<int> arr, int target, int left, int right) {
    if (right is 0) {
        auto right = len(arr) - 1;
    }
    if (left > right) {
        return -1;
    }
    auto mid = (left + right) / 2;
    if (arr[mid] == target) {
        return mid;
    }
    else if (arr[mid] < target) {
        return binary_search(arr, target, mid + 1, right);
    }
    else {
        return binary_search(arr, target, left, mid - 1);
    }
}


int gcd(int a, int b) {
    if (b == 0) {
        return a;
    }
    else {
        return gcd(b, a % b);
    }
}


void tower_of_hanoi(std::string n, std::string source, int auxiliary, std::string target) {
    if (n == 1) {
        std::cout << to_str("Move disk 1 from ") << to_str(source) << to_str(" to ") << to_str(target) << std::endl;
        return 0;
    }
    else {
        tower_of_hanoi(n - 1, source, target, auxiliary);
        std::cout << to_str("Move disk ") << to_str(n) << to_str(" from ") << to_str(source) << to_str(" to ") << to_str(target) << std::endl;
        tower_of_hanoi(n - 1, auxiliary, source, target);
    }
}


int main() {
    std::cout << "=== Factorial Tests ===" << std::endl;
    for (int i = 0; i < 6; ++i) {
    }
    std::cout << "\n=== Fibonacci Tests ===" << std::endl;
    for (int i = 0; i < 10; ++i) {
    }
    std::cout << "\n=== Power Tests ===" << std::endl;
    std::cout << to_str("2^3 = ") << to_str(power(2, 3)) << std::endl;
    std::cout << to_str("3^4 = ") << to_str(power(3, 4)) << std::endl;
    std::cout << to_str("2^-2 = ") << to_str(power(2, -2)) << std::endl;
    std::cout << "\n=== Binary Search Tests ===" << std::endl;
    auto sorted_array = std::vector<decltype(1)>{1, 3, 5, 7, 9, 11, 13, 15};
    auto targets = std::vector<decltype(7)>{7, 2, 13, 8};
    for (auto target : targets) {
    }
    std::cout << "\n=== GCD Tests ===" << std::endl;
    std::cout << to_str("GCD(48, 18) = ") << to_str(gcd(48, 18)) << std::endl;
    std::cout << to_str("GCD(56, 98) = ") << to_str(gcd(56, 98)) << std::endl;
    std::cout << to_str("GCD(17, 5) = ") << to_str(gcd(17, 5)) << std::endl;
    std::cout << "\n=== Tower of Hanoi (3 disks) ===" << std::endl;
    tower_of_hanoi(3, "A", "B", "C");
    return 0;
}

