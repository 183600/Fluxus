#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <functional>
#include <sstream>

using namespace std;

// helpers
template<typename T> static inline std::string to_str(const T& v){ std::ostringstream os; os<<v; return os.str(); }
static inline std::string to_str(const std::string& s){ return s; }
static inline std::string to_str(const char* s){ return std::string(s); }
template<typename C> static inline int sum(const C& c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline int sum(std::initializer_list<T> c){ int s=0; for(const auto& e: c) s += e; return s; }

int main() {
    auto x = 10;
    auto y = 20;
    auto sum_result = x + y;
    auto diff = y - x;
    auto product = x * y;
    auto quotient = y / x;
    auto remainder = y % x;
    std::cout << "Sum: " << to_str(sum_result) << std::endl;
    std::cout << "Difference: " << to_str(diff) << std::endl;
    std::cout << "Product: " << to_str(product) << std::endl;
    std::cout << "Quotient: " << to_str(quotient) << std::endl;
    std::cout << "Remainder: " << to_str(remainder) << std::endl;
    return 0;
}

