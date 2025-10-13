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


void test_map() {
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto squares = list(map(lambda x: x**2, numbers));
    auto doubles = list(map(lambda x: x * 2, numbers));
    std::cout << "Original numbers:" << " " << numbers << std::endl;
    std::cout << "Squares:" << " " << squares << std::endl;
    std::cout << "Doubles:" << " " << doubles << std::endl;
    auto list1 = std::vector<decltype(1)>{1, 2, 3};
    auto list2 = std::vector<decltype(4)>{4, 5, 6};
    auto sum_lists = list(map(lambda x, y: x + y, list1, list2));
    std::cout << "Sum of lists:" << " " << sum_lists << std::endl;
}


void test_filter() {
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto evens = list(filter(lambda x: x % 2 == 0, numbers));
    auto odds = list(filter(lambda x: x % 2 != 0, numbers));
    auto greater_than_5 = list(filter(lambda x: x > 5, numbers));
    std::cout << "Original numbers:" << " " << numbers << std::endl;
    std::cout << "Even numbers:" << " " << evens << std::endl;
    std::cout << "Odd numbers:" << " " << odds << std::endl;
    std::cout << "Greater than 5:" << " " << greater_than_5 << std::endl;
}


void test_reduce() {
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto sum_all = reduce(lambda x, y: x + y, numbers);
    auto product = reduce(lambda x, y: x * y, numbers);
    auto max_value = (x > y ? reduce(lambda x, y: x : y, numbers));
    std::cout << "Original numbers:" << " " << numbers << std::endl;
    std::cout << "Sum of all:" << " " << sum_all << std::endl;
    std::cout << "Product:" << " " << product << std::endl;
    std::cout << "Maximum value:" << " " << max_value << std::endl;
    auto with_initial = reduce(lambda x, y: x + y, numbers, 10);
    std::cout << "Sum with initial value 10:" << " " << with_initial << std::endl;
}


int test_partial() {
    std::function<int()> multiply = [&]() {
        return x * y;
    };
    auto double = partial(multiply, 2);
    auto triple = partial(multiply, 3);
    std::cout << "Double 5:" << " " << double(5) << std::endl;
    std::cout << "Triple 5:" << " " << triple(5) << std::endl;
    auto power = partial(pow, 2);
    std::cout << "2^3:" << " " << power(3) << std::endl;
    std::cout << "2^4:" << " " << power(4) << std::endl;
}


int compose(std::function<int()> f, std::function<int()> g) {
    return lambda x: f(g(x));
}


int test_composition() {
    std::function<int()> add_one = [&]() {
        return x + 1;
    };
    std::function<int()> multiply_by_two = [&]() {
        return x * 2;
    };
    auto add_then_multiply = compose(multiply_by_two, add_one);
    auto multiply_then_add = compose(add_one, multiply_by_two);
    std::cout << "Add then multiply (5):" << " " << add_then_multiply(5) << std::endl;
    std::cout << "Multiply then add (5):" << " " << multiply_then_add(5) << std::endl;
}


int test_higher_order() {
    std::function<int()> apply_operation = [&]() {
        return func(x, y);
    };
    std::function<int()> add = [&]() {
        return x + y;
    };
    std::function<int()> subtract = [&]() {
        return x - y;
    };
    std::function<int()> multiply = [&]() {
        return x * y;
    };
    std::cout << "Apply add:" << " " << apply_operation(add, 5, 3) << std::endl;
    std::cout << "Apply subtract:" << " " << apply_operation(subtract, 5, 3) << std::endl;
    std::cout << "Apply multiply:" << " " << apply_operation(multiply, 5, 3) << std::endl;
}


int curry_add(int x) {
    return lambda y: x + y;
}


void test_currying() {
    auto add5 = curry_add(5);
    auto add10 = curry_add(10);
    std::cout << "Add 5 to 3:" << " " << add5(3) << std::endl;
    std::cout << "Add 10 to 3:" << " " << add10(3) << std::endl;
}


int pure_function(int x, int y) {
    return x + y;
}


int impure_function(std::string x, std::string y) {
    std::cout << to_str("Adding ") << to_str(x) << to_str(" and ") << to_str(y) << std::endl;
    return x + y;
}


void test_immutability() {
    auto original = std::vector<decltype(1)>{1, 2, 3};
    auto modified = list(map(lambda x: x * 2, original));
    std::cout << "Original list:" << " " << original << std::endl;
    std::cout << "Modified list:" << " " << modified << std::endl;
}


int main() {
    std::cout << "=== Map Function ===" << std::endl;
    test_map();
    std::cout << "\n=== Filter Function ===" << std::endl;
    test_filter();
    std::cout << "\n=== Reduce Function ===" << std::endl;
    test_reduce();
    std::cout << "\n=== Partial Function Application ===" << std::endl;
    test_partial();
    std::cout << "\n=== Function Composition ===" << std::endl;
    test_composition();
    std::cout << "\n=== Higher-order Functions ===" << std::endl;
    test_higher_order();
    std::cout << "\n=== Currying ===" << std::endl;
    test_currying();
    std::cout << "\n=== Immutability ===" << std::endl;
    test_immutability();
    return 0;
}

