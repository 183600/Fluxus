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
    std::cout << "=== Basic Dictionary Comprehension ===" << std::endl;
    auto squares = std::unordered_map<std::string, int>{{"x", 0}};
    std::cout << to_str("Squares: ") << to_str(squares) << std::endl;
    std::cout << "\n=== Dictionary Comprehension with Condition ===" << std::endl;
    auto even_squares = std::unordered_map<std::string, int>{{"x", 0}};
    std::cout << to_str("Even squares: ") << to_str(even_squares) << std::endl;
    std::cout << "\n=== Dictionary from Two Lists ===" << std::endl;
    auto keys = std::vector<std::string>{'a', 'b', 'c', 'd'};
    auto values = std::vector<decltype(1)>{1, 2, 3, 4};
    auto dict_from_lists = std::unordered_map<std::string, int>{{"k", 0}, {"v in zip(keys, values)", 0}};
    std::cout << to_str("Dict from lists: ") << to_str(dict_from_lists) << std::endl;
    std::cout << "\n=== Dictionary with String Manipulation ===" << std::endl;
    auto words = std::vector<std::string>{'hello', 'world', 'python'};
    auto word_lengths = std::unordered_map<std::string, int>{{"word", 0}};
    std::cout << to_str("Word lengths: ") << to_str(word_lengths) << std::endl;
    std::cout << "\n=== Swap Keys and Values ===" << std::endl;
    auto original = std::unordered_map<std::string, int>{{"a", 1}, {"b", 2}, {"c", 3}};
    auto swapped = std::unordered_map<std::string, int>{{"v", 0}, {"v in py_items(original)", 0}};
    std::cout << to_str("Original: ") << to_str(original) << std::endl;
    std::cout << to_str("Swapped: ") << to_str(swapped) << std::endl;
    std::cout << "\n=== Dictionary with Nested Logic ===" << std::endl;
    auto categorized = {;
    };
    std::cout << to_str("Categorized: ") << to_str(categorized) << std::endl;
    std::cout << "\n=== Character Frequency ===" << std::endl;
    auto text = "hello world";
    auto char_count = 0;
    std::cout << to_str("Character frequency: ") << to_str(char_count) << std::endl;
    std::cout << "\n=== Basic Set Comprehension ===" << std::endl;
    auto squares_set = 0;
    std::cout << to_str("Squares set: ") << to_str(squares_set) << std::endl;
    std::cout << "\n=== Set Comprehension with Condition ===" << std::endl;
    auto even_numbers = 0;
    std::cout << to_str("Even numbers: ") << to_str(even_numbers) << std::endl;
    std::cout << "\n=== Set Comprehension - Unique Values ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 2, 3, 3, 3, 4, 4, 4, 4};
    auto unique = 0;
    std::cout << to_str("Original list: ") << to_str(numbers) << std::endl;
    std::cout << to_str("Unique set: ") << to_str(unique) << std::endl;
    std::cout << "\n=== Unique Characters ===" << std::endl;
    auto text = "hello world";
    auto unique_chars = 0;
    std::cout << to_str("Unique characters: ") << to_str(unique_chars) << std::endl;
    std::cout << "\n=== Set with Transformation ===" << std::endl;
    auto words = std::vector<std::string>{'Hello', 'WORLD', 'Python', 'PROGRAMMING'};
    auto lowercase_set = 0;
    std::cout << to_str("Lowercase set: ") << to_str(lowercase_set) << std::endl;
    std::cout << "\n=== Nested Dictionary Comprehension ===" << std::endl;
    auto matrix_dict = {;
    };
    std::cout << "Matrix dictionary:" << std::endl;
    for (auto [i, row] : py_items(matrix_dict)) {
    }
    std::cout << "\n=== Dictionary with Enumerate ===" << std::endl;
    auto items = std::vector<std::string>{'apple', 'banana', 'cherry'};
    auto indexed = std::unordered_map<std::string, int>{{"i", 0}, {"item in enumerate(items)", 0}};
    std::cout << to_str("Indexed items: ") << to_str(indexed) << std::endl;
    std::cout << "\n=== Set Operations ===" << std::endl;
    auto set1 = 0;
    auto set2 = 0;
    std::cout << to_str("Even numbers: ") << to_str(set1) << std::endl;
    std::cout << to_str("Multiples of 3: ") << to_str(set2) << std::endl;
    std::cout << to_str("Union: ") << to_str(set1 | set2) << std::endl;
    std::cout << to_str("Intersection: ") << to_str(set1 & set2) << std::endl;
    std::cout << to_str("Difference: ") << to_str(set1 - set2) << std::endl;
    std::cout << to_str("Symmetric difference: ") << to_str(set1 ^ set2) << std::endl;
    std::cout << "\n=== Conditional Values in Dict ===" << std::endl;
    auto numbers = range(10);
    auto classified = {;
    };
    std::cout << to_str("Classified: ") << to_str(classified) << std::endl;
    std::cout << "\n=== Set with Multiple Conditions ===" << std::endl;
    auto filtered = {;
    };
    std::cout << to_str("Numbers divisible by 2 and 3: ") << to_str(filtered) << std::endl;
    std::cout << "\n=== Dictionary from Nested Structure ===" << std::endl;
    auto data = [;
    ];
    auto people = std::unordered_map<std::string, int>{{"name", 0}, {"age in data", 0}};
    std::cout << to_str("People: ") << to_str(people) << std::endl;
    std::cout << "\n=== Filter Dictionary Items ===" << std::endl;
    auto scores = std::unordered_map<std::string, int>{{"Alice", 85}, {"Bob", 92}, {"Charlie", 78}, {"David", 95}};
    auto high_scores = std::unordered_map<std::string, int>{{"name", 0}, {"score in py_items(scores) if score >= 90", 0}};
    std::cout << to_str("High scores (>= 90): ") << to_str(high_scores) << std::endl;
    std::cout << "\n=== Set with Function Call ===" << std::endl;
    auto words = std::vector<std::string>{'hello', 'world', 'python', 'programming'};
    auto lengths = 0;
    std::cout << to_str("Unique word lengths: ") << to_str(lengths) << std::endl;
    std::cout << "\n=== Complex Dictionary Comprehension ===" << std::endl;
    auto students = std::vector<std::string>{'Alice', 'Bob', 'Charlie', 'David', 'Eve'};
    auto grades = std::vector<decltype(85)>{85, 92, 78, 95, 88};
    auto status = {;
    };
    std::cout << "Student status:" << std::endl;
    for (auto [name, info] : py_items(status)) {
    }
    std::cout << "\n=== All dict/set comprehension tests completed ===" << std::endl;
    return 0;
}

