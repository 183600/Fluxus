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


void test_scope() {
    auto x = "local x";
    std::cout << to_str("Inside function: ") << to_str(x) << std::endl;
}


void increment() {
    global counter;
    counter += 1;
}


void modify_globals() {
    global value1, value2;
    auto value1 = 100;
    auto value2 = 200;
}


void outer() {
    auto x = "outer x";
    std::function<int()> inner = [&]() {
        nonlocal x;
        auto x = "modified by inner";
        std::cout << to_str("Inner: ") << to_str(x) << std::endl;
    };
    std::cout << to_str("Before inner: ") << to_str(x) << std::endl;
    inner();
    std::cout << to_str("After inner: ") << to_str(x) << std::endl;
}


std::function<int()> make_multiplier(int n) {
    std::function<int()> multiplier = [&]() {
        return x * n;
    };
    return multiplier;
}


std::function<int()> make_counter() {
    auto count = 0;
    std::function<int()> increment = [&]() {
        nonlocal count;
        count += 1;
        return count;
    };
    return increment;
}


std::string make_account(int initial_balance) {
    auto balance = initial_balance;
    std::function<int()> deposit = [&]() {
        nonlocal balance;
        balance += amount;
        return balance;
    };
    std::function<int()> withdraw = [&]() {
        nonlocal balance;
        if (amount <= balance) {
            balance -= amount;
            return balance;
        }
        else {
            return "Insufficient funds";
        }
    };
    std::function<int()> get_balance = [&]() {
        return balance;
    };
    return deposit, withdraw, get_balance;
}


void outer() {
    auto x = "enclosing";
    std::function<int()> inner = [&]() {
        auto x = "local";
        std::cout << to_str("Local: ") << to_str(x) << std::endl;
    };
    inner();
    std::cout << to_str("Enclosing: ") << to_str(x) << std::endl;
}


int create_multipliers() {
    auto multipliers = std::vector<int>{};
    for (int i = 0; i < 5; ++i) {
        multipliers.push_back(lambda x, i=i: x * i);
    }
    return multipliers;
}


std::function<int()> outer(int x) {
    std::function<int()> middle = [&]() {
        std::function<int()> inner = [&]() {
            return x + y + z;
        };
        return inner;
    };
    return middle;
}


std::function<int()> make_averager() {
    auto series = std::vector<int>{};
    std::function<int()> averager = [&]() {
        series.push_back(new_value);
        return sum(series) / len(series);
    };
    return averager;
}


std::function<int()> outer() {
    auto x = 10;
    auto y = 20;
    std::function<int()> inner = [&]() {
        return x + y;
    };
    return inner();
}


std::function<int()> outer() {
    auto items = std::vector<int>{};
    std::function<int()> add_item = [&]() {
        items.push_back(item);
        return items;
    };
    return add_item;
}


std::function<int()> power_factory(int exp) {
    std::function<int()> power = [&]() {
        return std::pow(base, exp);
    };
    return power;
}


int main() {
    std::cout << "=== Global and Local Scope ===" << std::endl;
    auto x = "global x";
    test_scope();
    std::cout << to_str("Outside function: ") << to_str(x) << std::endl;
    std::cout << "\n=== global Keyword ===" << std::endl;
    auto counter = 0;
    std::cout << to_str("Before: counter = ") << to_str(counter) << std::endl;
    increment();
    increment();
    std::cout << to_str("After: counter = ") << to_str(counter) << std::endl;
    auto value1 = 10;
    auto value2 = 20;
    std::cout << to_str("Before: value1=") << to_str(value1) << to_str(", value2=") << to_str(value2) << std::endl;
    modify_globals();
    std::cout << to_str("After: value1=") << to_str(value1) << to_str(", value2=") << to_str(value2) << std::endl;
    std::cout << "\n=== nonlocal Keyword ===" << std::endl;
    outer();
    std::cout << "\n=== Simple Closures ===" << std::endl;
    auto times_two = make_multiplier(2);
    auto times_three = make_multiplier(3);
    std::cout << to_str("times_two(5) = ") << to_str(times_two(5)) << std::endl;
    std::cout << to_str("times_three(5) = ") << to_str(times_three(5)) << std::endl;
    std::cout << "\n=== Closure Counter ===" << std::endl;
    auto counter1 = make_counter();
    auto counter2 = make_counter();
    std::cout << to_str("counter1: ") << to_str(counter1()) << to_str(", ") << to_str(counter1()) << to_str(", ") << to_str(counter1()) << std::endl;
    std::cout << to_str("counter2: ") << to_str(counter2()) << to_str(", ") << to_str(counter2()) << std::endl;
    std::cout << "\n=== Closure with Multiple Functions ===" << std::endl;
    auto deposit, withdraw, get_balance = make_account(100);
    std::cout << to_str("Initial balance: ") << to_str(get_balance()) << std::endl;
    std::cout << to_str("After deposit(50): ") << to_str(deposit(50)) << std::endl;
    std::cout << to_str("After withdraw(30): ") << to_str(withdraw(30)) << std::endl;
    std::cout << to_str("Final balance: ") << to_str(get_balance()) << std::endl;
    std::cout << "\n=== LEGB Rule ===" << std::endl;
    auto x = "global";
    outer();
    std::cout << to_str("Global: ") << to_str(x) << std::endl;
    std::cout << "\n=== Closure in Loop (Fixed) ===" << std::endl;
    auto mults = create_multipliers();
    for (size_t i = 0; i < mults.size(); ++i) {
        auto mult = mults[i];
    }
    std::cout << "\n=== Nested Closures ===" << std::endl;
    auto result = outer(1)(2)(3);
    std::cout << to_str("outer(1)(2)(3) = ") << to_str(result) << std::endl;
    std::cout << "\n=== Closure vs Class ===" << std::endl;
    auto avg = make_averager();
    std::cout << to_str("avg(10) = ") << to_str(avg(10)) << std::endl;
    std::cout << to_str("avg(20) = ") << to_str(avg(20)) << std::endl;
    std::cout << to_str("avg(30) = ") << to_str(avg(30)) << std::endl;
    std::cout << "\n=== Reading Outer Variables ===" << std::endl;
    auto result = outer();
    std::cout << to_str("Result: ") << to_str(result) << std::endl;
    std::cout << "\n=== Modifying Mutable Outer Variables ===" << std::endl;
    auto add = outer();
    std::cout << to_str("add('a') = ") << to_str(add('a')) << std::endl;
    std::cout << to_str("add('b') = ") << to_str(add('b')) << std::endl;
    std::cout << to_str("add('c') = ") << to_str(add('c')) << std::endl;
    std::cout << "\n=== Closure State Preservation ===" << std::endl;
    auto square = power_factory(2);
    auto cube = power_factory(3);
    std::cout << to_str("square(4) = ") << to_str(square(4)) << std::endl;
    std::cout << to_str("cube(4) = ") << to_str(cube(4)) << std::endl;
    std::cout << "\n=== All scope and closure tests completed ===" << std::endl;
    return 0;
}

