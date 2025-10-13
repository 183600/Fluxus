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


void simple_counter(int n) {
    for (int i = 0; i < n; ++i) {
        /* yield */
    }
}


void fibonacci_generator() {
    auto a, b = 0, 1;
    while (true) {
        /* yield */
        auto a, b = b, a + b;
    }
}


void even_numbers(int start, int end) {
    for (int num = start; num < end + 1; ++num) {
        if (num % 2 == 0) {
            /* yield */
        }
    }
}


void generator_with_send() {
    auto value = yield "Ready";
    while (true) {
        auto value = yield f"Received: {value}";
    }
}


void count_up_to(int n) {
    auto count = 0;
    while (count <= n) {
        /* yield */
        count += 1;
    }
}


bool prime_numbers(int limit) {
    std::function<int()> is_prime = [&]() {
        if (num < 2) {
            return false;
        }
        for (int i = 2; i < int(num ** 0.5) + 1; ++i) {
            if (num % i == 0) {
                return false;
            }
        }
        return true;
    };
    for (int num = 2; num < limit + 1; ++num) {
        if (is_prime(num)) {
            /* yield */
        }
    }
}


void test_simple_generator() {
    auto counter = simple_counter(5);
    auto assert list(counter) = = std::vector<decltype(0)>{0, 1, 2, 3, 4};
}


void test_fibonacci_generator() {
    auto fib = fibonacci_generator();
    auto result = std::vector<int>{};
    for (int _ = 0; _ < 10; ++_) {
        result.push_back(next(fib));
    }
    auto assert result = = std::vector<decltype(0)>{0, 1, 1, 2, 3, 5, 8, 13, 21, 34};
}


void test_even_numbers_generator() {
    auto evens = even_numbers(1, 10);
    auto assert list(evens) = = std::vector<decltype(2)>{2, 4, 6, 8, 10};
}


void test_generator_send() {
    auto gen = generator_with_send();
    auto initial = next(gen);
    auto assert initial = = "Ready";
    auto response1 = gen.send("Hello");
    auto assert response1 = = "Received: Hello";
    auto response2 = gen.send("World");
    auto assert response2 = = "Received: World";
}


void test_count_up_to_generator() {
    auto counter = count_up_to(3);
    auto assert list(counter) = = std::vector<decltype(0)>{0, 1, 2, 3};
}


void test_prime_numbers_generator() {
    auto primes = prime_numbers(20);
    auto assert list(primes) = = std::vector<decltype(2)>{2, 3, 5, 7, 11, 13, 17, 19};
}


void test_generator_expressions() {
    auto squares = 0;
    auto assert list(squares) = = std::vector<decltype(0)>{0, 1, 4, 9, 16};
}


void test_generator_chain() {
    std::function<int()> chain = [&]() {
        for (auto iterable : iterables) {
            /* yield */
        }
    };
    auto result = list(chain(std::vector<decltype(1)>{1, 2, 3}, std::vector<std::string>{'a', 'b'}, std::vector<decltype(4)>{4, 5}));
    auto assert result = = std::vector<std::string>{to_str(1), to_str(2), to_str(3), 'a', 'b', to_str(4), to_str(5)};
}


int main() {
    test_simple_generator();
    test_fibonacci_generator();
    test_even_numbers_generator();
    test_generator_send();
    test_count_up_to_generator();
    test_prime_numbers_generator();
    test_generator_expressions();
    test_generator_chain();
    std::cout << "All generator tests passed!" << std::endl;
    return 0;
}

