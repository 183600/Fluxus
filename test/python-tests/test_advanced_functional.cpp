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


class Person {
    public:
        Person() = default;
        void placeholder();
    public:
        // generated placeholder fields
};


void test_advanced_combinatorics() {
    /* docstring */
    std::cout << "=== Advanced Combinatorics ===" << std::endl;
    auto colors = std::vector<std::string>{'red', 'blue'};
    auto sizes = std::vector<std::string>{'S', 'M', 'L'};
    auto materials = std::vector<std::string>{'cotton', 'silk'};
    auto all_combinations = list(itertools.product(colors, sizes, materials));
    std::cout << to_str("All product combinations: ") << to_str(len(all_combinations)) << std::endl;
    for (auto combo : all_combinations[:5]) {
        std::cout << to_str("  ") << to_str(combo) << std::endl;
    }
    auto items = std::vector<std::string>{'A', 'B', 'C'};
    std::cout << to_str("\\nPermutations of ") << to_str(items) << to_str(":") << std::endl;
    for (int r = 1; r < len(items) + 1; ++r) {
        auto perms = list(itertools.permutations(items, r));
        std::cout << to_str("  Length ") << to_str(r) << to_str(": ") << to_str(perms) << std::endl;
    }
    auto numbers = std::vector<decltype(1)>{1, 2, 3};
    auto combos_with_replacement = list(itertools.combinations_with_replacement(numbers, 2));
    std::cout << to_str("\\nCombinations with replacement: ") << to_str(combos_with_replacement) << std::endl;
}


void test_infinite_iterators() {
    /* docstring */
    std::cout << "\n=== Infinite Iterators ===" << std::endl;
    auto cycle_iter = itertools.cycle(std::vector<std::string>{'A', 'B', 'C'});
    std::cout << "Cycling through std::vector<std::string>{'A', 'B', 'C'}:" << std::endl;
    for (size_t i = 0; i < cycle_iter.size(); ++i) {
        auto item = cycle_iter[i];
        if (i >= 10) {
            break;
        }
        std::cout << to_str("  ") << to_str(item) << std::endl;
    }
    auto counter = itertools.count(10, 2.5);
    std::cout << to_str("\\nCounting from 10 by 2.5:") << std::endl;
    for (size_t i = 0; i < counter.size(); ++i) {
        auto num = counter[i];
        if (i >= 5) {
            break;
        }
        std::cout << to_str("  ") << to_str(num) << std::endl;
    }
    auto data = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto accumulated = list(itertools.accumulate(data, operator.mul));
    std::cout << to_str("\\nAccumulated multiplication: ") << to_str(accumulated) << std::endl;
    auto words = std::vector<std::string>{"Hello", " ", "World", "!"};
    auto concatenated = list(itertools.accumulate(words));
    std::cout << to_str("Accumulated strings: ") << to_str(concatenated) << std::endl;
}


void test_iterator_aggregation() {
    /* docstring */
    std::cout << "\n=== Iterator Aggregation ===" << std::endl;
    auto list1 = std::vector<decltype(1)>{1, 2, 3};
    auto list2 = std::vector<std::string>{'a', 'b', 'c'};
    auto list3 = std::vector<decltype(true)>{true, false};
    auto chained = list(itertools.chain(list1, list2, list3));
    std::cout << to_str("Chained: ") << to_str(chained) << std::endl;
    auto nested_lists = std::vector{std::vector<decltype(1)>{1, 2}, std::vector<decltype(3)>{3, 4}, std::vector<decltype(5)>{5, 6}};
    auto chained_from_iterable = list(itertools.chain.from_iterable(nested_lists));
    std::cout << to_str("Chained from iterable: ") << to_str(chained_from_iterable) << std::endl;
    auto data = std::vector<std::string>{'A', 'B', 'C', 'D', 'E'};
    auto selectors = std::vector<decltype(1)>{1, 0, 1, 0, 1};
    auto compressed = list(itertools.compress(data, selectors));
    std::cout << to_str("Compressed: ") << to_str(compressed) << std::endl;
    auto numbers = std::vector<decltype(1)>{1, 3, 5, 2, 4, 6, 8};
    auto dropwhile_result = list(itertools.dropwhile(lambda x: x < 5, numbers));
    auto takewhile_result = list(itertools.takewhile(lambda x: x < 5, numbers));
    std::cout << to_str("Dropwhile (<5): ") << to_str(dropwhile_result) << std::endl;
    std::cout << to_str("Takewhile (<5): ") << to_str(takewhile_result) << std::endl;
    auto mixed_numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto evens = list(filter(lambda x: x % 2 == 0, mixed_numbers));
    auto odds = list(itertools.filterfalse(lambda x: x % 2 == 0, mixed_numbers));
    std::cout << to_str("Evens: ") << to_str(evens) << std::endl;
    std::cout << to_str("Odds: ") << to_str(odds) << std::endl;
}


void test_iterator_grouping() {
    /* docstring */
    std::cout << "\n=== Iterator Grouping ===" << std::endl;
    auto words = std::vector<std::string>{'apple', 'banana', 'cherry', 'apricot', 'blueberry', 'cranberry'};
    auto grouped = {};
    for (auto [key, group] : itertools.groupby(sorted(words), key=lambda x: x[0])) {
        auto grouped[key] = list(group);
    }
    std::cout << to_str("Grouped by first letter: ") << to_str(grouped) << std::endl;
    std::function<int()> partition = [&]() {
        /* docstring */
        auto items = list(iterable);
        auto size = len(items) / n;
        if (len(items) % n != 0) {
            size += 1;
        }
        for (int i = 0; i < len(items); i += size) {
            /* yield */
        }
    };
    auto numbers = list(range(20));
    auto partitions = list(partition(numbers, 4));
    std::cout << to_str("Partitioned into 4: ") << to_str(partitions) << std::endl;
    std::function<int()> sliding_window = [&]() {
        /* docstring */
        auto it = iter(iterable);
        auto window = collections.deque(itertools.islice(it, size), maxlen=size);
        if (len(window) == size) {
            /* yield */
        }
        for (auto item : it) {
            window.push_back(item);
            /* yield */
        }
    };
    auto data = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6, 7};
    auto windows = list(sliding_window(data, 3));
    std::cout << to_str("Sliding windows (size 3): ") << to_str(windows) << std::endl;
}


std::function<int()> test_function_composition() {
    /* docstring */
    std::cout << "\n=== Function Composition ===" << std::endl;
    std::function<int()> compose = [&]() {
        /* docstring */
        std::function<int()> composed_function = [&]() {
            auto result = x;
            for (auto func : reversed(functions)) {
                auto result = func(result);
            }
            return result;
        };
        return composed_function;
    };
    std::function<int()> double = [&]() {
        return x * 2;
    };
    std::function<int()> add_one = [&]() {
        return x + 1;
    };
    std::function<int()> square = [&]() {
        return std::pow(x, 2);
    };
    auto double_add_one_square = compose(square, add_one, double);
    auto result = double_add_one_square(3);
    std::cout << to_str("Composed function result: ") << to_str(result) << std::endl;
    std::function<int()> pipeline = [&]() {
        /* docstring */
        auto result = data;
        for (auto func : functions) {
            auto result = func(result);
        }
        return result;
    };
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto result = pipeline(;
        numbers,;
        auto lambda x: filter(lambda n: n % 2 = = 0, x),;
        lambda x: map(lambda n: n * 2, x),;
        lambda x: filter(lambda n: n > 5, x),;
        list;
    }
    );
    std::cout << to_str("Pipeline result: ") << to_str(result) << std::endl;
}


int test_partial_application() {
    /* docstring */
    std::cout << "\n=== Partial Function Application ===" << std::endl;
    std::function<int()> power = [&]() {
        return std::pow(base, exponent);
    };
    auto square = functools.partial(power, exponent=2);
    auto cube = functools.partial(power, exponent=3);
    std::cout << to_str("Square of 5: ") << to_str(square(5)) << std::endl;
    std::cout << to_str("Cube of 3: ") << to_str(cube(3)) << std::endl;
    std::function<int()> calculate_price = [&]() {
        return base_price * (1 + tax_rate) * (1 - discount);
    };
    auto us_pricing = functools.partial(calculate_price, tax_rate=0.08);
    auto eu_pricing = functools.partial(calculate_price, tax_rate=0.20);
    std::cout << to_str("US price (base=100, discount=0.1): ") << to_str(us_pricing(100, discount=0.1)) << std::endl;
    std::cout << to_str("EU price (base=100, discount=0.1): ") << to_str(eu_pricing(100, discount=0.1)) << std::endl;
}


std::function<int()> test_function_memoization() {
    /* docstring */
    std::cout << "\n=== Function Memoization ===" << std::endl;
    std::function<int()> memoize = [&]() {
        auto cache = {};
        @functools.wraps(func);
        std::function<int()> wrapper = [&]() {
            if (args in cache) {
                std::cout << to_str("Cache hit for ") << to_str(args) << std::endl;
                return cache[args];
            }
            std::cout << to_str("Computing result for ") << to_str(args) << std::endl;
            auto result = func(*args);
            auto cache[args] = result;
            return result;
        };
        wrapper.cache_clear(lambda: cache.clear());
        wrapper.cache_info(lambda: f"Cache size: {len(cache)}");
        return wrapper;
    };
    @memoize;
    std::function<int()> expensive_fibonacci = [&]() {
        /* docstring */
        if (n <= 1) {
            return n;
        }
        return expensive_fibonacci(n-1) + expensive_fibonacci(n-2);
    };
    std::cout << "Computing Fibonacci numbers:" << std::endl;
    std::cout << to_str("Fibonacci(5): ") << to_str(expensive_fibonacci(5)) << std::endl;
    std::cout << to_str("Fibonacci(7): ") << to_str(expensive_fibonacci(7)) << std::endl;
    std::cout << to_str("Fibonacci(5) again: ") << to_str(expensive_fibonacci(5)) << std::endl;
    std::cout << to_str("Cache info: ") << to_str(expensive_fibonacci.cache_info()) << std::endl;
    @functools.lru_cache(maxsize(32));
    std::function<int()> cached_power = [&]() {
        /* docstring */
        std::cout << to_str("Computing ") << to_str(base) << to_str("^") << to_str(exponent) << std::endl;
        return std::pow(base, exponent);
    };
    std::cout << to_str("\\nCached power results:") << std::endl;
    std::cout << to_str("2^10: ") << to_str(cached_power(2, 10)) << std::endl;
    std::cout << to_str("3^5: ") << to_str(cached_power(3, 5)) << std::endl;
    std::cout << to_str("2^10 again: ") << to_str(cached_power(2, 10)) << std::endl;
    std::cout << to_str("Cache stats: ") << to_str(cached_power.cache_info()) << std::endl;
}


std::string test_monad_patterns() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Monad-like Patterns ===" << std::endl;
    class Maybe { public: Maybe() = default; };
    std::function<int()> safe_divide = [&]() {
        if (y == 0) {
            return 0;
        }
        return x / y;
    };
    std::function<int()> safe_sqrt = [&]() {
        if (x < 0) {
            return 0;
        }
        return std::pow(x, 0.5);
    };
    auto result = Maybe(16).bind(safe_sqrt).bind(lambda x: safe_divide(x, 2));
    std::cout << to_str("Safe computation result: ") << to_str(result) << std::endl;
    class Result { public: Result() = default; };
    std::function<int()> divide_result = [&]() {
        {
            if ((y)) == 0) { __fluxus_exc = true; return 0; }
else { return ((1.0*(Result(x))/(y))); }

        }
        if (__fluxus_exc) {
            return Result(error="Division by zero");
        }
    };
    std::function<int()> sqrt_result = [&]() {
        {
            if (x < 0) {
                return Result(error="Cannot sqrt negative number");
            }
            return std::pow(Result(x, 0.5));
        }
        if (__fluxus_exc) { auto e = 0;
            return Result(error="Square root error");
        }
    };
    auto result = Result(16).bind(lambda x: sqrt_result(x)).bind(lambda x: divide_result(x, 2));
    std::cout << to_str("Result monad: ") << to_str(result) << std::endl;
    auto error_result = Result(-4).bind(lambda x: sqrt_result(x)).bind(lambda x: divide_result(x, 2));
    std::cout << to_str("Error case: ") << to_str(error_result) << std::endl;
}


void test_advanced_data_processing() {
    /* docstring */
    std::cout << "\n=== Advanced Data Processing ===" << std::endl;
    auto people = [;
        Person("Alice", 25, "New York", 50000),;
        Person("Bob", 30, "San Francisco", 75000),;
        Person("Charlie", 35, "New York", 60000),;
        Person("Diana", 28, "Chicago", 55000),;
        Person("Eve", 32, "San Francisco", 80000),;
        Person("Frank", 29, "Chicago", 58000),;
    }
    ];
    auto people_by_city = {};
    for (auto [city, group] : itertools.groupby(sorted(people, key=lambda p: p.city), key=lambda p: p.city)) {
        auto people_by_city[city] = list(group);
    }
    std::cout << "People grouped by city:" << std::endl;
    for (auto [city, city_people] : py_items(people_by_city)) {
        auto avg_salary = sum(std::vector<int>{});
        std::cout << to_str("  ") << to_str(city) << to_str(": ") << to_str(len(city_people)) << to_str(" people, avg salary: $") << to_str(avg_salary) << std::endl;
    }
    auto high_earners = list(filter(lambda p: p.salary > 60000, people));
    auto names_and_salaries = list(map(lambda p: (p.name, p.salary), high_earners));
    std::cout << to_str("\\nHigh earners (>$60k): ") << to_str(names_and_salaries) << std::endl;
    auto sorted_people = sorted(people, key=attrgetter('city', 'salary'), reverse=true);
    std::cout << "\nPeople sorted by city and salary (descending):" << std::endl;
    for (auto person : sorted_people) {
        std::cout << to_str("  ") << to_str(person.name) << to_str(" (") << to_str(person.city) << to_str("): $") << to_str(person.salary) << std::endl;
    }
    auto salaries = list(map(attrgetter('salary'), people));
    auto total_salary = functools.reduce(operator.add, salaries);
    auto max_salary = max(salaries);
    auto min_salary = min(salaries);
    std::cout << to_str("\\nSalary statistics:") << std::endl;
    std::cout << to_str("  Total: $") << to_str(total_salary) << std::endl;
    std::cout << to_str("  Average: $") << to_str(total_salary / len(salaries)) << std::endl;
    std::cout << to_str("  Max: $") << to_str(max_salary) << std::endl;
    std::cout << to_str("  Min: $") << to_str(min_salary) << std::endl;
}


std::function<int()> curry(std::function<int()> func) {
    /* docstring */
    @functools.wraps(func);
    std::function<int()> curried = [&]() {
        if (len(args) + len(kwargs) >= func.__code__.co_argcount) {
            return func(*args, **kwargs);
        }
        return functools.partial(curried, *args, **kwargs);
    };
    return curried;
}


int test_advanced_functional_utilities() {
    /* docstring */
    std::cout << "\n=== Advanced Functional Utilities ===" << std::endl;
    std::function<int()> square = [&]() {
        return std::pow(x, 2);
    };
    std::function<int()> add_one = [&]() {
        return x + 1;
    };
    std::function<int()> double = [&]() {
        return x * 2;
    };
    @curry;
    std::function<int()> add_three_numbers = [&]() {
        return a + b + c;
    };
    auto add_five = add_three_numbers(5);
    auto add_five_and_three = add_five(3);
    auto result = add_five_and_three(2);
    std::cout << to_str("Curried function result: ") << to_str(result) << std::endl;
    std::function<int()> compose_reduce = [&]() {
        return functools.reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x);
    };
    auto composed = compose_reduce(square, add_one, double);
    auto result = composed(3);
    std::cout << to_str("Composed with reduce: ") << to_str(result) << std::endl;
    auto data = list(range(20));
    auto complex_filter = [](auto x) { return  all([; };
        auto x % 2 = = 0,;
        auto x % 3 = = 0,;
        x > 10;
    }
    ]);
    auto filtered = list(filter(complex_filter, data));
    std::cout << to_str("Complex filter result: ") << to_str(filtered) << std::endl;
    std::function<int()> partition = [&]() {
        /* docstring */
        auto trues, falses = [], [];
        for (auto item : iterable) {
            if (predicate(item)) {
                trues.push_back(item);
            }
            else {
                falses.push_back(item);
            }
        }
        return trues, falses;
    };
    auto evens, odds = partition(lambda x: x % 2 == 0, range(10));
    std::cout << to_str("Partitioned evens: ") << to_str(evens) << std::endl;
    std::cout << to_str("Partitioned odds: ") << to_str(odds) << std::endl;
}


int main() {
    /* docstring */
    std::cout << "=== Advanced Python Functional Programming Patterns ===" << std::endl;
    test_advanced_combinatorics();
    test_infinite_iterators();
    test_iterator_aggregation();
    test_iterator_grouping();
    test_function_composition();
    test_partial_application();
    test_function_memoization();
    test_monad_patterns();
    test_advanced_data_processing();
    test_advanced_functional_utilities();
    std::cout << "\n=== All functional programming tests completed successfully! ===" << std::endl;
    return 0;
}


