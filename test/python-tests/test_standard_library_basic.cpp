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


int multiply(int x, int y) {
    return x * y;
}


int fibonacci(int n) {
    if (n < 2) {
        return n;
    }
    return fibonacci(n-1) + fibonacci(n-2);
}


int main() {
    std::cout << "=== math module ===" << std::endl;
    std::cout << to_str("math.pi: ") << to_str(math.pi) << std::endl;
    std::cout << to_str("math.e: ") << to_str(math.e) << std::endl;
    std::cout << to_str("math.sqrt(16): ") << to_str(math.sqrt(16)) << std::endl;
    std::cout << to_str("math.pow(2, 3): ") << to_str(math.pow(2, 3)) << std::endl;
    std::cout << to_str("math.ceil(3.2): ") << to_str(math.ceil(3.2)) << std::endl;
    std::cout << to_str("math.floor(3.8): ") << to_str(math.floor(3.8)) << std::endl;
    std::cout << to_str("math.sin(math.pi/2): ") << to_str(math.sin(math.pi/2)) << std::endl;
    std::cout << to_str("math.cos(0): ") << to_str(math.cos(0)) << std::endl;
    std::cout << to_str("math.log(math.e): ") << to_str(math.log(math.e)) << std::endl;
    std::cout << to_str("math.log10(100): ") << to_str(math.log10(100)) << std::endl;
    std::cout << to_str("math.factorial(5): ") << to_str(math.factorial(5)) << std::endl;
    std::cout << "\n=== random module ===" << std::endl;
    random.seed(42);
    std::cout << to_str("random.random(): ") << to_str(random.random()) << std::endl;
    std::cout << to_str("random.randint(1, 10): ") << to_str(random.randto_int(1, 10)) << std::endl;
    std::cout << to_str("random.uniform(1.0, 10.0): ") << to_str(random.uniform(1.0, 10.0)) << std::endl;
    auto choices = std::vector<std::string>{'apple', 'banana', 'cherry'};
    std::cout << to_str("random.choice(choices): ") << to_str(random.choice(choices)) << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    std::cout << to_str("random.sample(numbers, 3): ") << to_str(random.sample(numbers, 3)) << std::endl;
    auto deck = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    random.shuffle(deck);
    std::cout << to_str("After shuffle: ") << to_str(deck) << std::endl;
    std::cout << "\n=== os module ===" << std::endl;
    std::cout << to_str("os.name: ") << to_str(os.name) << std::endl;
    std::cout << to_str("Current PID: ") << to_str(os.getpid()) << std::endl;
    std::cout << to_str("os.cpu_count(): ") << to_str(os.cpu_count()) << std::endl;
    std::cout << "\n=== os.path module ===" << std::endl;
    auto path = "/home/user/file.txt";
    std::cout << to_str("os.path.basename(path): ") << to_str(os.path.basename(path)) << std::endl;
    std::cout << to_str("os.path.dirname(path): ") << to_str(os.path.dirname(path)) << std::endl;
    std::cout << to_str("os.path.split(path): ") << to_str(os.path.split(path)) << std::endl;
    std::cout << to_str("os.path.splitext('file.txt'): ") << to_str(os.path.splitext('file.txt')) << std::endl;
    std::cout << to_str("os.path.join('dir', 'file.txt'): ") << to_str(os.path.join('dir', 'file.txt')) << std::endl;
    std::cout << "\n=== sys module ===" << std::endl;
    std::cout << to_str("sys.version_info.major: ") << to_str(sys::version_info.major) << std::endl;
    std::cout << to_str("sys.version_info.minor: ") << to_str(sys::version_info.minor) << std::endl;
    std::cout << to_str("sys.platform: ") << to_str(sys::platform) << std::endl;
    std::cout << to_str("sys.maxsize: ") << to_str(sys::maxsize) << std::endl;
    std::cout << "\n=== itertools module ===" << std::endl;
    auto counter = itertools.count(start=10, step=2);
    std::cout << to_str("First 5 from count(10, 2): ") << to_str(0) << std::endl;
    auto colors = itertools.cycle(std::vector<std::string>{'red', 'green', 'blue'});
    std::cout << to_str("First 7 from cycle: ") << to_str(0) << std::endl;
    auto repeater = itertools.repeat('hello', 3);
    std::cout << to_str("repeat('hello', 3): ") << to_str(list(repeater)) << std::endl;
    auto result = list(itertools.chain(std::vector<decltype(1)>{1, 2}, std::vector<decltype(3)>{3, 4}, std::vector<decltype(5)>{5, 6}));
    std::cout << to_str("chain([1,2], [3,4], [5,6]): ") << to_str(result) << std::endl;
    auto combs = list(itertools.combinations(std::vector<decltype(1)>{1, 2, 3, 4}, 2));
    std::cout << to_str("combinations([1,2,3,4], 2): ") << to_str(combs) << std::endl;
    auto perms = list(itertools.permutations(std::vector<decltype(1)>{1, 2, 3}, 2));
    std::cout << to_str("permutations([1,2,3], 2): ") << to_str(perms) << std::endl;
    auto prod = list(itertools.product(std::vector<decltype(1)>{1, 2}, std::vector<std::string>{'a', 'b'}));
    std::cout << to_str("product([1,2], ['a','b']): ") << to_str(prod) << std::endl;
    std::cout << "\n=== functools module ===" << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto total = functools.reduce(lambda x, y: x + y, numbers);
    std::cout << to_str("reduce(add, [1,2,3,4,5]): ") << to_str(total) << std::endl;
    auto product = functools.reduce(lambda x, y: x * y, numbers);
    std::cout << to_str("reduce(mul, [1,2,3,4,5]): ") << to_str(product) << std::endl;
    auto double = functools.partial(multiply, 2);
    std::cout << to_str("double(5) using partial: ") << to_str(double(5)) << std::endl;
    std::cout << to_str("fibonacci(10): ") << to_str(fibonacci(10)) << std::endl;
    std::cout << to_str("Cache info: ") << to_str(fibonacci.cache_info()) << std::endl;
    std::cout << "\n=== operator module ===" << std::endl;
    std::cout << to_str("operator.add(2, 3): ") << to_str(operator.add(2, 3)) << std::endl;
    std::cout << to_str("operator.mul(4, 5): ") << to_str(operator.mul(4, 5)) << std::endl;
    std::cout << to_str("operator.sub(10, 3): ") << to_str(operator.sub(10, 3)) << std::endl;
    std::cout << to_str("operator.truediv(10, 3): ") << to_str(operator.truediv(10, 3)) << std::endl;
    auto data = std::vector<std::string>{('Alice', 25), ('Bob', 30), ('Charlie', 20)};
    auto sorted_by_age = sorted(data, key=operator.itemgetter(1));
    std::cout << to_str("Sorted by age: ") << to_str(sorted_by_age) << std::endl;
    std::cout << "\n=== statistics module ===" << std::endl;
    auto data = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6, 7, 8, 9};
    std::cout << to_str("mean: ") << to_str(statistics.mean(data)) << std::endl;
    std::cout << to_str("median: ") << to_str(statistics.median(data)) << std::endl;
    std::cout << to_str("mode: ") << to_str(statistics.mode(std::vector<decltype(1)>{1, 1, 2, 3, 3, 3, 4})) << std::endl;
    std::cout << to_str("stdev: ") << to_str(statistics.stdev(data)) << std::endl;
    std::cout << to_str("variance: ") << to_str(statistics.variance(data)) << std::endl;
    std::cout << "\n=== time module ===" << std::endl;
    auto current = time.time();
    std::cout << to_str("Current timestamp: ") << to_str(to_int(current)) << std::endl;
    auto local_time = time.localtime();
    std::cout << to_str("Local time: ") << to_str(time.strftime('%Y-%m-%d %H) << std::endl;
    std::cout << "\n=== string module ===" << std::endl;
    std::cout << to_str("ascii_lowercase: ") << to_str(string.ascii_lowercase) << std::endl;
    std::cout << to_str("ascii_uppercase: ") << to_str(string.ascii_uppercase) << std::endl;
    std::cout << to_str("digits: ") << to_str(string.digits) << std::endl;
    std::cout << to_str("punctuation: ") << to_str(string.punctuation) << std::endl;
    std::cout << to_str("whitespace: ") << to_str(repr(string.whitespace)) << std::endl;
    auto template = string.Template("Hello, $name! You are $age years old.");
    auto result = template.substitute(name="Alice", age=25);
    std::cout << to_str("Template result: ") << to_str(result) << std::endl;
    std::cout << "\n=== decimal module ===" << std::endl;
    auto d1 = Decimal('0.1');
    auto d2 = Decimal('0.2');
    std::cout << to_str("Decimal('0.1') << Decimal('0.2'): ") << to_str(d1 << d2) << std::endl;
    std::cout << to_str("Float 0.1 << 0.2: ") << to_str(0.1 << 0.2) << std::endl;
    std::cout << "\n=== fractions module ===" << std::endl;
    auto f1 = Fraction(1, 3);
    auto f2 = Fraction(1, 6);
    std::cout << to_str("Fraction(1, 3) << Fraction(1, 6): ") << to_str(f1 << f2) << std::endl;
    std::cout << to_str("Fraction(1, 2) * 4: ") << to_str(Fraction(1, 2) * 4) << std::endl;
    std::cout << "\n=== All standard library tests completed ===" << std::endl;
    return 0;
}

