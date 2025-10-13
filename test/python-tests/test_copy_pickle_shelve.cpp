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


void test_shallow_vs_deep_copy() {
    /* docstring */
    std::cout << "=== Testing Shallow vs Deep Copy ===" << std::endl;
    auto original = {;
        'numbers': std::vector<decltype(1)>{1, 2, 3},;
        'nested': {'inner': std::vector<decltype(4)>{4, 5, 6}},;
        'simple': 'string';
    }
    };
    auto shallow = copy.copy(original);
    auto deep = copy.deepcopy(original);
    std::cout << "Original:" << " " << original << std::endl;
    std::cout << "Shallow copy:" << " " << shallow << std::endl;
    std::cout << "Deep copy:" << " " << deep << std::endl;
    original['numbers'].push_back(999);
    original['nested']['inner'].push_back(888);
    auto original['simple'] = 'modified';
    std::cout << "\nAfter modifying original:" << std::endl;
    std::cout << "Original:" << " " << original << std::endl;
    std::cout << "Shallow copy (affected by nested changes):" << " " << shallow << std::endl;
    std::cout << "Deep copy (unaffected):" << " " << deep << std::endl;
}


void test_copy_edge_cases() {
    /* docstring */
    std::cout << "\n=== Testing Copy Edge Cases ===" << std::endl;
    auto a = std::vector<decltype(1)>{1, 2, 3};
    auto b = std::vector<decltype(4)>{4, 5, 6};
    a.push_back(b);
    b.push_back(a);
    std::cout << "Circular reference before copy:" << std::endl;
    std::cout << to_str("a: ") << to_str(a) << std::endl;
    std::cout << to_str("b: ") << to_str(b) << std::endl;
    auto a_copy = copy.deepcopy(a);
    std::cout << "\nDeep copy of circular reference:" << std::endl;
    std::cout << to_str("a_copy: ") << to_str(a_copy) << std::endl;
    auto a[0] = 999;
    std::cout << to_str("\\nAfter modifying original a[0] = 999:") << std::endl;
    std::cout << to_str("original a: ") << to_str(a) << std::endl;
    std::cout << to_str("copied a_copy: ") << to_str(a_copy) << std::endl;
}


void test_pickle_serialization() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Pickle Serialization ===" << std::endl;
    auto data = {;
        'string': 'hello world',;
        'list': std::vector<decltype(1)>{1, 2.5, true, 0},;
        'dict': {'nested': {'deep': 'value'}},;
        'tuple': (1, 2, 3),;
        'set': {4, 5, 6};
    }
    };
    std::cout << "Original data:" << " " << data << std::endl;
    auto pickled = pickle.dumps(data);
    std::cout << to_str("Pickled data size: ") << to_str(len(pickled)) << to_str(" bytes") << std::endl;
    auto unpickled = pickle.loads(pickled);
    std::cout << "Unpickled data:" << " " << unpickled << std::endl;
    std::cout << "Data equality:" << " " << data == unpickled << std::endl;
    for (int protocol = 0; protocol < pickle.HIGHEST_PROTOCOL + 1; ++protocol) {
        {
            auto p = pickle.dumps(data, protocol=protocol);
            auto u = pickle.loads(p);
            std::cout << to_str("Protocol ") << to_str(protocol) << to_str(": size = ") << to_str(len(p)) << to_str(" bytes, works = ") << to_str(data == u) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("Protocol ") << to_str(protocol) << to_str(": failed - ") << to_str(e) << std::endl;
        }
    }
}


void test_shelve_persistence() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Shelve Persistence ===" << std::endl;
    auto temp_dir = tempfile.mkdtemp();
    auto db_path = os.path.join(temp_dir, 'test_shelve');
    {
        {
            auto db['user_data'] = std::unordered_map<std::string, std::string>{{"name", 'Alice'}, {"age", to_str(30)}};
            auto db['settings'] = std::unordered_map<std::string, std::string>{{"theme", 'dark'}, {"language", 'en'}};
            auto db['numbers'] = std::vector<decltype(1)>{1, 2, 3, 4, 5};
            auto db['timestamp'] = time.time();
            std::cout << "Data written to shelve database" << std::endl;
        }
        {
            std::cout << "Data read from shelve database:" << std::endl;
            for (auto [key, value] : py_items(db)) {
                std::cout << to_str("  ") << to_str(key) << to_str(": ") << to_str(value) << std::endl;
            }
        }
        {
            auto user_data = db['user_data'];
            auto user_data['age'] = 31;
            auto db['user_data'] = user_data;
            auto db['new_key'] = 'new_value';
            std::cout << "\nData modified in database" << std::endl;
        }
        {
            std::cout << "Modified data:" << std::endl;
            std::cout << to_str("  user_data: ") << to_str(db['user_data']) << std::endl;
            std::cout << to_str("  new_key: ") << to_str(db['new_key']) << std::endl;
        }
    }
    {
        for (auto file_path : glob.glob(f"{db_path}*")) {
            {
                os.unlink(file_path);
            }
            if (__fluxus_exc) { auto e = 0;
                /* pass */
            }
        }
        {
            os.rmdir(temp_dir);
        }
        if (__fluxus_exc) { auto e = 0;
            /* pass */
        }
    }
}


void test_copy_performance() {
    /* docstring */
    std::cout << "\n=== Testing Copy Performance ===" << std::endl;
    auto large_data = {;
        0;
    }
    };
    auto start = time.time();
    auto shallow_copy = copy.copy(large_data);
    auto shallow_time = time.time() - start;
    auto start = time.time();
    auto deep_copy = copy.deepcopy(large_data);
    auto deep_time = time.time() - start;
    std::cout << to_str("Shallow copy time: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(4); os<<shallow_time; return os.str(); }()) << to_str(" seconds") << std::endl;
    std::cout << to_str("Deep copy time: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(4); os<<deep_time; return os.str(); }()) << to_str(" seconds") << std::endl;
    std::cout << to_str("Deep copy is ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<deep_time/shallow_time; return os.str(); }()) << to_str("x slower") << std::endl;
    std::cout << to_str("Shallow copy first item data == original: ") << to_str(shallow_copy['items'][0]['data'] == large_data['items'][0]['data']) << std::endl;
    std::cout << to_str("Deep copy first item data == original: ") << to_str(deep_copy['items'][0]['data'] == large_data['items'][0]['data']) << std::endl;
    auto large_data['items'][0]['data'][0] = 99999;
    std::cout << to_str("After modifying original, shallow copy reflects change: ") << to_str(shallow_copy['items'][0]['data'][0] == 99999) << std::endl;
    std::cout << to_str("After modifying original, deep copy remains unchanged: ") << to_str(deep_copy['items'][0]['data'][0] == 0) << std::endl;
}


int main() {
    /* docstring */
    测试Python的深拷贝vs浅拷贝行为;
    /* docstring */
    test_shallow_vs_deep_copy();
    test_copy_edge_cases();
    test_pickle_serialization();
    test_shelve_persistence();
    test_copy_performance();
    std::cout << "\n=== All copy and serialization tests completed ===" << std::endl;
    return 0;
}

