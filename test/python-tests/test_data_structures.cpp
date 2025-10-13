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
    std::cout << "=== Lists ===" << std::endl;
    auto fruits = std::vector<std::string>{"apple", "banana", "cherry", "date"};
    std::cout << to_str("Original list: ") << to_str(fruits) << std::endl;
    fruits.push_back("elderberry");
    fruits.insert(1, "blueberry");
    fruits.remove("cherry");
    auto popped = fruits.pop();
    std::cout << to_str("After operations: ") << to_str(fruits) << std::endl;
    std::cout << to_str("Popped item: ") << to_str(popped) << std::endl;
    std::cout << to_str("First three: ") << to_str(fruits[) << std::endl;
    std::cout << to_str("Last two: ") << to_str(fruits[-2) << std::endl;
    auto numbers = std::vector<decltype(3)>{3, 1, 4, 1, 5, 9, 2, 6};
    numbers.sort();
    std::cout << to_str("Sorted numbers: ") << to_str(numbers) << std::endl;
    numbers.reverse();
    std::cout << to_str("Reversed numbers: ") << to_str(numbers) << std::endl;
    std::cout << to_str("Count of 1: ") << to_str(numbers.count(1)) << std::endl;
    std::cout << to_str("Index of 9: ") << to_str(numbers.index(9)) << std::endl;
    std::cout << "\n=== Tuples ===" << std::endl;
    auto coordinates = (10, 20);
    auto point = (3, 4, 5);
    std::cout << to_str("Coordinates: ") << to_str(coordinates) << std::endl;
    std::cout << to_str("Point: ") << to_str(point) << std::endl;
    auto x, y = coordinates;
    std::cout << to_str("Unpacked: x=") << to_str(x) << to_str(", y=") << to_str(y) << std::endl;
    std::cout << "\n=== Sets ===" << std::endl;
    auto set1 = {1, 2, 3, 4, 5};
    auto set2 = {4, 5, 6, 7, 8};
    std::cout << to_str("Set 1: ") << to_str(set1) << std::endl;
    std::cout << to_str("Set 2: ") << to_str(set2) << std::endl;
    std::cout << to_str("Union: ") << to_str(set1 | set2) << std::endl;
    std::cout << to_str("Intersection: ") << to_str(set1 & set2) << std::endl;
    std::cout << to_str("Difference: ") << to_str(set1 - set2) << std::endl;
    std::cout << to_str("Symmetric difference: ") << to_str(set1 ^ set2) << std::endl;
    set1.add(6);
    set1.remove(2);
    std::cout << to_str("Modified set 1: ") << to_str(set1) << std::endl;
    std::cout << to_str("Is 3 in set1? ") << to_str(3 in set1) << std::endl;
    std::cout << to_str("Set length: ") << to_str(len(set1)) << std::endl;
    std::cout << "\n=== Dictionaries Extended ===" << std::endl;
    auto student = {;
    };
    std::cout << to_str("Student: ") << to_str(student) << std::endl;
    std::cout << to_str("Keys: ") << to_str(list(student.keys())) << std::endl;
    std::cout << to_str("Values: ") << to_str(list(student.values())) << std::endl;
    std::cout << to_str("Math grade: ") << to_str(student['courses']['math']) << std::endl;
    student.update({"major": "Computer Science"});
    auto age = student.pop("age");
    std::cout << to_str("After update and pop: ") << to_str(student) << std::endl;
    std::cout << to_str("Popped age: ") << to_str(age) << std::endl;
    auto grades = student.get("grades", []);
    std::cout << to_str("Grades: ") << to_str(grades) << std::endl;
    auto missing = student.get("missing_key", "default_value");
    std::cout << to_str("Missing key with default: ") << to_str(missing) << std::endl;
    auto complex_data = {;
    };
    std::cout << to_str("\\nComplex data: ") << to_str(complex_data) << std::endl;
    std::cout << to_str("Active users: ") << to_str(0) << std::endl;
    return 0;
}

