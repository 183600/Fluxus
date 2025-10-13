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


class CountDown {
    public:
        CountDown(int start) {
            this->current = start;
        }
        int __iter__() {
    return self;
}


        std::string __next__() {
    if (this->current < 0) {
        /* raise */
    }
    auto value = this->current;
    this->current -= 1;
    return value;
}


    public:
        int current = 0;
};


void test_custom_iterator() {
    std::cout << "Custom countdown iterator:" << std::endl;
    auto countdown = CountDown(5);
    for (auto num : countdown) {
        std::cout << num << std::endl;
    }
}


void test_list_iteration() {
    auto fruits = std::vector<std::string>{"apple", "banana", "cherry"};
    std::cout << "\nList iteration:" << std::endl;
    for (auto fruit : fruits) {
        std::cout << fruit << std::endl;
    }
    std::cout << "\nWith enumerate:" << std::endl;
    for (size_t index = 0; index < fruits.size(); ++index) {
        auto fruit = fruits[index];
        std::cout << to_str(index) << to_str(": ") << to_str(fruit) << std::endl;
    }
    std::cout << "\nReversed list:" << std::endl;
    for (auto fruit : reversed(fruits)) {
        std::cout << fruit << std::endl;
    }
}


void test_dict_iteration() {
    auto student = std::unordered_map<std::string, std::string>{{"name", "Alice"}, {"age", to_str(20)}, {"grade", "A"}};
    std::cout << "\nDictionary iteration:" << std::endl;
    std::cout << "Keys:" << std::endl;
    for (auto key : student.keys()) {
        std::cout << key << std::endl;
    }
    std::cout << "\nValues:" << std::endl;
    for (auto value : student.values()) {
        std::cout << value << std::endl;
    }
    std::cout << "\nItems:" << std::endl;
    for (auto [key, value] : py_items(student)) {
        std::cout << to_str(key) << to_str(": ") << to_str(value) << std::endl;
    }
}


void test_string_iteration() {
    auto text = "Hello";
    std::cout << "\nString iteration:" << std::endl;
    for (auto char : text) {
        std::cout << char << std::endl;
    }
    std::cout << "\nSplit string:" << std::endl;
    for (auto word : text.split()) {
        std::cout << word << std::endl;
    }
}


void test_range() {
    std::cout << "\nRange iteration:" << std::endl;
    for (int i = 0; i < 5; ++i) {
        std::cout << i << std::endl;
    }
    std::cout << "\nRange with step:" << std::endl;
    for (int i = 0; i < 10; i += 2) {
        std::cout << i << std::endl;
    }
}


void test_file_iteration() {
    auto filename = "temp_test_file.txt";
    {
        f.write("Line 1\n");
        f.write("Line 2\n");
        f.write("Line 3\n");
    }
    std::cout << "\nFile iteration:" << std::endl;
    {
        for (auto line : f) {
            std::cout << to_str("Line: ") << to_str(line.strip()) << std::endl;
        }
    }
    os.remove(filename);
}


void test_itertools() {
    std::cout << "\n=== itertools.count ===" << std::endl;
    auto counter = itertools.count(10, 2);
    for (int _ = 0; _ < 5; ++_) {
        std::cout << next(counter) << std::endl;
    }
    std::cout << "\n=== itertools.cycle ===" << std::endl;
    auto cycle_chars = itertools.cycle('ABC');
    for (int _ = 0; _ < 6; ++_) {
        std::cout << next(cycle_chars) << std::endl;
    }
    std::cout << "\n=== itertools.repeat ===" << std::endl;
    auto repeater = itertools.repeat('hello', 3);
    for (auto item : repeater) {
        std::cout << item << std::endl;
    }
    std::cout << "\n=== itertools.chain ===" << std::endl;
    auto result = list(itertools.chain(std::vector<decltype(1)>{1, 2, 3}, std::vector<std::string>{'a', 'b', 'c'}, std::vector<decltype(4)>{4, 5, 6}));
    std::cout << "Chained:" << " " << result << std::endl;
    std::cout << "\n=== itertools.accumulate ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto accumulated = list(itertools.accumulate(numbers));
    std::cout << "Accumulated:" << " " << accumulated << std::endl;
    std::cout << "\n=== itertools.permutations ===" << std::endl;
    auto perms = list(itertools.permutations(std::vector<decltype(1)>{1, 2, 3}, 2));
    std::cout << "Permutations:" << " " << perms << std::endl;
    std::cout << "\n=== itertools.combinations ===" << std::endl;
    auto combos = list(itertools.combinations(std::vector<decltype(1)>{1, 2, 3, 4}, 2));
    std::cout << "Combinations:" << " " << combos << std::endl;
    std::cout << "\n=== itertools.product ===" << std::endl;
    auto products = list(itertools.product(std::vector<decltype(1)>{1, 2}, std::vector<std::string>{'a', 'b'}));
    std::cout << "Products:" << " " << products << std::endl;
}


void countdown_generator(int n) {
    while (n >= 0) {
        /* yield */
        n -= 1;
    }
}


void test_generators() {
    std::cout << "\nGenerator function:" << std::endl;
    for (auto num : countdown_generator(3)) {
        std::cout << num << std::endl;
    }
    std::cout << "\nGenerator expression:" << std::endl;
    auto squares = 0;
    for (auto square : squares) {
        std::cout << square << std::endl;
    }
}


void test_zip() {
    auto list1 = std::vector<decltype(1)>{1, 2, 3};
    auto list2 = std::vector<std::string>{'a', 'b', 'c'};
    auto list3 = std::vector<decltype(10)>{10, 20, 30};
    std::cout << "\nZip iteration:" << std::endl;
    for (;;) {
        std::cout << item << std::endl;
    }
    std::cout << "\nZip longest:" << std::endl;
    for (auto item : zip_longest(list1, list2, std::vector<decltype(100)>{100}, fillvalue='MISSING')) {
        std::cout << item << std::endl;
    }
}


int main() {
    std::cout << "=== Custom Iterator ===" << std::endl;
    test_custom_iterator();
    test_list_iteration();
    test_dict_iteration();
    test_string_iteration();
    test_range();
    test_file_iteration();
    test_itertools();
    test_generators();
    test_zip();
    return 0;
}

