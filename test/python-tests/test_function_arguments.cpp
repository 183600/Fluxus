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


std::string greet(std::string name, std::string greeting) {
    return to_str(greeting) + to_str(", ") + to_str(name) + to_str("!");
}


int power(int base, int exponent) {
    return std::pow(base, exponent);
}


std::string describe_person(std::string name, std::string age, std::string city) {
    return to_str(name) + to_str(" is ") + to_str(age) + to_str(" years old and lives in ") + to_str(city);
}


int sum_all(std::initializer_list<int> args) {
    auto total = 0;
    for (auto num : args) {
        total += num;
    }
    return total;
}

template<typename... Ts>
auto sum_all(Ts... xs){ return sum_all(std::initializer_list<int>{ static_cast<int>(xs)... }); }


void print_args(std::initializer_list<int> args) {
    for (size_t i = 0; i < args.size(); ++i) {
        auto arg = args[i];
        std::cout << to_str("  Arg ") << to_str(i) << to_str(": ") << to_str(arg) << std::endl;
    }
}

template<typename... Ts>
auto print_args(Ts... xs){ return print_args(std::initializer_list<int>{ static_cast<int>(xs)... }); }


void print_info(std::initializer_list<int> *kwargs) {
    for (auto [key, value] : py_items(kwargs)) {
        std::cout << to_str("  ") << to_str(key) << to_str(": ") << to_str(value) << std::endl;
    }
}

template<typename... Ts>
auto print_info(Ts... xs){ return print_info(std::initializer_list<int>{ static_cast<int>(xs)... }); }


std::string build_profile(std::initializer_list<int> *kwargs) {
    auto profile = {};
    for (auto [key, value] : py_items(kwargs)) {
        auto profile[key] = value;
    }
    return profile;
}

template<typename... Ts>
auto build_profile(Ts... xs){ return build_profile(std::initializer_list<int>{ static_cast<int>(xs)... }); }


int complex_function(std::string a, std::string b, std::initializer_list<int> args, std::string c, std::initializer_list<int> *kwargs) {
    std::cout << to_str("  a=") << to_str(a) << to_str(", b=") << to_str(b) << std::endl;
    std::cout << to_str("  args=") << to_str(args) << std::endl;
    std::cout << to_str("  c=") << to_str(c) << std::endl;
    std::cout << to_str("  kwargs=") << to_str(kwargs) << std::endl;
    return a + b + sum(args) + c + sum(kwargs.values());
}

template<typename... Ts>
auto complex_function(Ts... xs){ return complex_function(std::initializer_list<int>{ static_cast<int>(xs)... }); }


int keyword_only(int a, int b, std::initializer_list<int> , int c, int d) {
    return a + b + c + d;
}

template<typename... Ts>
auto keyword_only(Ts... xs){ return keyword_only(std::initializer_list<int>{ static_cast<int>(xs)... }); }


int position_only(int a, int b, int /, int c, int d) {
    return a + b + c + d;
}


int mixed_args(int a, int b, int /, int c, int d, std::initializer_list<int> , int e, int f) {
    return a + b + c + d + e + f;
}

template<typename... Ts>
auto mixed_args(Ts... xs){ return mixed_args(std::initializer_list<int>{ static_cast<int>(xs)... }); }


int add_three(int a, int b, int c) {
    return a + b + c;
}


std::string greet_person(std::string name, std::string age, std::string city) {
    return to_str(name) + to_str(", ") + to_str(age) + to_str(", ") + to_str(city);
}


int append_to(int element, std::vector<int> target) {
    if (target is 0) {
        auto target = std::vector<int>{};
    }
    target.push_back(element);
    return target;
}


int get_coordinates() {
    return 10, 20, 30;
}


int divide_and_remainder(int a, int b) {
    return a / b, a % b;
}


int main() {
    std::cout << "=== Default Arguments ===" << std::endl;
    std::cout << greet("Alice") << std::endl;
    std::cout << greet("Bob", "Hi") << std::endl;
    std::cout << greet("Charlie", greeting="Hey") << std::endl;
    std::cout << to_str("power(3) = ") << to_str(power(3)) << std::endl;
    std::cout << to_str("power(3, 3) = ") << to_str(power(3, 3)) << std::endl;
    std::cout << "\n=== Keyword Arguments ===" << std::endl;
    std::cout << describe_person("Alice", 25, "NYC") << std::endl;
    std::cout << describe_person(name="Bob", age=30, city="LA") << std::endl;
    std::cout << describe_person("Charlie", city="SF", age=35) << std::endl;
    std::cout << "\n=== Variable Positional Arguments (*args) ===" << std::endl;
    std::cout << to_str("sum_all(1, 2, 3) = ") << to_str(sum_all(1, 2, 3)) << std::endl;
    std::cout << to_str("sum_all(1, 2, 3, 4, 5) = ") << to_str(sum_all(1, 2, 3, 4, 5)) << std::endl;
    std::cout << to_str("sum_all() = ") << to_str(sum_all()) << std::endl;
    std::cout << "print_args('a', 'b', 'c'):" << std::endl;
    print_args('a', 'b', 'c');
    std::cout << "\n=== Variable Keyword Arguments (**kwargs) ===" << std::endl;
    std::cout << "print_info(name='Alice', age=25, city='NYC'):" << std::endl;
    auto print_info(name = 'Alice', age=25, city='NYC');
    auto profile = build_profile(name="Bob", age=30, occupation="Engineer");
    std::cout << to_str("Profile: ") << to_str(profile) << std::endl;
    std::cout << "\n=== Combining Argument Types ===" << std::endl;
    std::cout << "complex_function(1, 2, 3, 4, c=5, x=6, y=7):" << std::endl;
    auto result = complex_function(1, 2, 3, 4, c=5, x=6, y=7);
    std::cout << to_str("  Result: ") << to_str(result) << std::endl;
    std::cout << "\n=== Keyword-Only Arguments ===" << std::endl;
    std::cout << to_str("keyword_only(1, 2, c=3, d=4) = ") << to_str(keyword_only(1, 2, c=3, d=4)) << std::endl;
    std::cout << "\n=== Position-Only Arguments ===" << std::endl;
    std::cout << to_str("position_only(1, 2, 3, 4) = ") << to_str(position_only(1, 2, 3, 4)) << std::endl;
    std::cout << to_str("position_only(1, 2, c=3, d=4) = ") << to_str(position_only(1, 2, c=3, d=4)) << std::endl;
    std::cout << "\n=== Position-Only and Keyword-Only ===" << std::endl;
    auto result = mixed_args(1, 2, 3, 4, e=5, f=6);
    std::cout << to_str("mixed_args(1, 2, 3, 4, e=5, f=6) = ") << to_str(result) << std::endl;
    std::cout << "\n=== Unpacking Arguments ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3};
    std::cout << to_str("add_three(*[1, 2, 3]) = ") << to_str(add_three(*numbers)) << std::endl;
    auto person = std::unordered_map<std::string, std::string>{{"name", "Alice"}, {"age", to_str(25)}, {"city", "NYC"}};
    std::cout << to_str("greet_person(**person) = ") << to_str(greet_person(**person)) << std::endl;
    std::cout << "\n=== Default Mutable Arguments ===" << std::endl;
    auto list1 = append_to(1);
    auto list2 = append_to(2);
    std::cout << to_str("list1: ") << to_str(list1) << std::endl;
    std::cout << to_str("list2: ") << to_str(list2) << std::endl;
    std::cout << "\n=== Multiple Return Values ===" << std::endl;
    auto x, y, z = get_coordinates();
    std::cout << to_str("Coordinates: x=") << to_str(x) << to_str(", y=") << to_str(y) << to_str(", z=") << to_str(z) << std::endl;
    auto quotient, remainder = divide_and_remainder(17, 5);
    std::cout << to_str("17 ÷ 5 = ") << to_str(quotient) << to_str(" remainder ") << to_str(remainder) << std::endl;
    std::cout << "\n=== All function argument tests completed ===" << std::endl;
    return 0;
}

