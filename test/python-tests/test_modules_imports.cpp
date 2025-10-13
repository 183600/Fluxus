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


class ModuleClass {
    public:
        ModuleClass(std::string name) {
            this->name = name;
        }
        std::string greet() {
    return to_str("Hello from ") + to_str(this->name) + to_str("!");
}


    public:
        std::string name = "";
};


std::string module_function() {
    return "Function from test module";
}


int main() {
    auto MODULE_CONSTANT = "I am a constant";
    std::cout << "Math module tests:" << std::endl;
    std::cout << to_str("Pi: ") << to_str(math.pi) << std::endl;
    std::cout << to_str("Square root of 16: ") << to_str(math.sqrt(16)) << std::endl;
    std::cout << to_str("Power of 2^3: ") << to_str(math.pow(2, 3)) << std::endl;
    std::cout << to_str("Factorial of 5: ") << to_str(math.factorial(5)) << std::endl;
    std::cout << "\nRandom module tests:" << std::endl;
    std::cout << to_str("Random number between 1-100: ") << to_str(random.randto_int(1, 100)) << std::endl;
    std::cout << to_str("Random choice from list: ") << to_str(random.choice(std::vector<std::string>{'apple', 'banana', 'cherry'})) << std::endl;
    auto random_numbers = 0;
    std::cout << to_str("Random floats: ") << to_str(random_numbers) << std::endl;
    std::cout << "\nDatetime module tests:" << std::endl;
    auto current_time = datetime.datetime.now();
    std::cout << to_str("Current date and time: ") << to_str(current_time) << std::endl;
    std::cout << to_str("Current date: ") << to_str(current_time.date()) << std::endl;
    std::cout << to_str("Current time: ") << to_str(current_time.time()) << std::endl;
    std::cout << to_str("Formatted date: ") << to_str(current_time.strftime('%Y-%m-%d %H) << std::endl;
    std::cout << "\nJSON module tests:" << std::endl;
    auto data = {;
    };
    auto json_string = json.dumps(data);
    std::cout << to_str("Data as JSON: ") << to_str(json_string) << std::endl;
    auto parsed_data = json.loads(json_string);
    std::cout << to_str("Parsed name: ") << to_str(parsed_data['name']) << std::endl;
    std::cout << to_str("Parsed hobbies: ") << to_str(parsed_data['hobbies']) << std::endl;
    std::cout << to_str("\\nUsing specific imports:") << std::endl;
    std::cout << to_str("Square root of 25: ") << to_str(sqrt(25)) << std::endl;
    std::cout << to_str("Pi value: ") << to_str(pi) << std::endl;
    std::cout << to_str("Random choice: ") << to_str(choice(std::vector<std::string>{'red', 'green', 'blue'})) << std::endl;
    std::cout << to_str("\\nModule constant: ") << to_str(MODULE_CONSTANT) << std::endl;
    std::cout << to_str("Module function: ") << to_str(module_function()) << std::endl;
    std::cout << to_str("Module class: ") << to_str(ModuleClass('Test').greet()) << std::endl;
    return 0;
}

