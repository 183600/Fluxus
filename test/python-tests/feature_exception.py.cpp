#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <functional>
#include <sstream>
#include <iomanip>

using namespace std;

// helpers
template<typename T> static inline std::string to_str(const T& v){ std::ostringstream os; os<<v; return os.str(); }
static inline std::string to_str(const std::string& s){ return s; }
static inline std::string to_str(const char* s){ return std::string(s); }
template<typename C> static inline int sum(const C& c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline int sum(std::initializer_list<T> c){ int s=0; for(const auto& e: c) s += e; return s; }

int divide_numbers(int a, int b) {
    bool __fluxus_exc=false;
{
        if ((b) == 0) { __fluxus_exc = true; result = 0; }
else { result = ((1.0*(a))/(b)); }

        return result;
    }
    if (__fluxus_exc) {
        std::cout << "Error: Can!divide by zero!" << std::endl;
        return 0;
    }
    { /* except */
        std::cout << 0 << std::endl;
        return 0;
    }
}


int safe_divide(int a, int b) {
    bool __fluxus_exc=false;
{
        return a / b;
    }
    if (__fluxus_exc) {
        std::cout << "Caught division by zero" << std::endl;
        return 0;
    }
    {
        std::cout << "Finally block executed" << std::endl;
    }
}


int main() {
    auto result1 = divide_numbers(10, 2);
    std::cout << "10 / 2 =" << " " <<  result1 << std::endl;
    auto result2 = divide_numbers(10, 0);
    std::cout << "10 / 0 =" << " " <<  result2 << std::endl;
    auto result3 = safe_divide(20, 4);
    std::cout << "20 / 4 =" << " " <<  result3 << std::endl;
    return 0;
}

