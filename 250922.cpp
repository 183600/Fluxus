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


int basic_types() {
    auto int_num = 42;
    auto negative_int = -17;
    auto big_int = 10**100;
    auto float_num = 3.14159;
    auto scientific = 1.23e-4;
    auto single_quote = 'Hello';
    auto double_quote = "World";
    auto raw_string = r"原始字符串\n";
    auto f_string = to_str("插值字符串: ") + to_str(int_num) + to_str(", ") + to_str(float_num);
    auto bool_true = true;
    auto bool_false = false;
    auto complex_num = 3 + 4j;
    auto none_value = 0;
    auto bytes_data = b'hello';
    auto bytearray_data = bytearray(b'world');
    return {;
        "int": int_num,;
        "float": float_num,;
        "string": single_quote + " " + double_quote,;
        "bool": bool_true,;
        "complex": complex_num,;
        "none": none_value,;
        "bytes": bytes_data,;
        "bytearray": bytearray_data;
    }
    };
}


int container_types() {
    auto empty_list = std::vector<int>{};
    auto simple_list = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto mixed_list = std::vector<std::string>{to_str(1), "two", to_str(3.0), to_str(true), to_str(0)};
    auto nested_list = std::vector{std::vector<decltype(1)>{1, 2}, std::vector<decltype(3)>{3, 4}, std::vector<decltype(5)>{5, 6}};
    auto list_comprehension = 0;
    auto empty_tuple = ();
    auto simple_tuple = (1, 2, 3);
    auto single_element_tuple = (1,);
    auto tuple_unpacking = a, b, c = (1, 2, 3);
    auto empty_dict = {};
    auto simple_dict = std::unordered_map<std::string, std::string>{{"name", "Alice"}, {"age", to_str(30)}, {"city", "New York"}};
    auto dict_comprehension = std::unordered_map<std::string, int>{{"x", 0}};
    auto empty_set = set();
    auto simple_set = {1, 2, 3, 4, 5};
    auto set_comprehension = 0;
    auto frozen_set = frozenset(std::vector<decltype(1)>{1, 2, 3, 4, 5});
    return {;
        "list": simple_list,;
        "tuple": simple_tuple,;
        "dict": simple_dict,;
        "set": simple_set,;
        "frozenset": frozen_set;
    }
    };
}


int control_flow() {
    auto x = 10;
    if (x > 10) {
        auto result = "大于10";
    }
    else if (x == 10) {
        auto result = "等于10";
    }
    else {
        auto result = "小于10";
    }
    auto sum_result = 0;
    for (int i = 1; i < 11; ++i) {
        sum_result += i;
    }
    auto count = 0;
    while (count < 5) {
        count += 1;
    }
    for (int i = 0; i < 10; ++i) {
        if (i == 3) {
            continue;
        }
        if (i == 7) {
            break;
        }
    }
    auto matrix = std::vector{std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(4)>{4, 5, 6}, std::vector<decltype(7)>{7, 8, 9}};
    auto flattened = std::vector<int>{};
    for (auto row : matrix) {
        for (auto item : row) {
            flattened.push_back(item);
        }
    }
    auto max_value = (5 > 3 ? 10 : 3);
    return {;
        "if_result": result,;
        "sum": sum_result,;
        "count": count,;
        "flattened": flattened,;
        "max_value": max_value;
    }
    };
}


std::string functions_demo() {
    std::function<int()> add = [&]() {
        return a + b;
    };
    std::function<int()> greet = [&]() {
        return to_str("Hello, ") + to_str(name) + to_str("!");
    };
    std::function<int()> sum_all = [&]() {
        return sum(args);
    };
    std::function<int()> person_info = [&]() {
        auto info = std::unordered_map<std::string, int>{{"name", name}, {"age", age}};
        info.update(kwargs);
        return info;
    };
    std::function<int()> apply_function = [&]() {
        return func(x);
    };
    auto square = [](auto x) { return  x**2; };
    std::function<int()> factorial = [&]() {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    };
    auto result1 = add(5, 3);
    auto result2 = greet("Alice");
    auto result3 = sum_all(1, 2, 3, 4, 5);
    auto result4 = person_info("Bob", 25, city="New York", job="Engineer");
    auto result5 = apply_function(lambda x: x*2, 10);
    auto result6 = square(5);
    auto result7 = factorial(5);
    return {;
        "add": result1,;
        "greet": result2,;
        "sum_all": result3,;
        "person_info": result4,;
        "apply_lambda": result5,;
        "square": result6,;
        "factorial": result7;
    }
    };
}


int classes_demo() {
    class Animal { public: Animal() = default; };
    class Dog { public: Dog() = default; };
    class Flyable { public: Flyable() = default; };
    class Bird { public: Bird() = default; };
    class Counter { public: Counter() = default; };
    class MathUtils { public: MathUtils() = default; };
    class Circle { public: Circle() = default; };
    auto animal = Animal("Generic Animal");
    auto dog = Dog("Rex", "German Shepherd");
    auto bird = Bird("Tweety", 10);
    auto counter1 = Counter();
    auto counter2 = Counter();
    counter1.increment();
    counter1.increment();
    counter2.increment();
    auto circle = Circle(5);
    return {;
        "animal": animal.speak(),;
        "dog": dog.speak(),;
        "dog_fetch": dog.fetch(),;
        "bird": bird.speak(),;
        "bird_fly": bird.fly(),;
        "counter_value": counter1.value,;
        "counter_count": Counter.count,;
        "static_add": MathUtils.add(5, 3),;
        "class_multiply": MathUtils.multiply(4, 5),;
        "circle_radius": circle.radius,;
        "circle_area": circle.area;
    }
    };
}


int exceptions_demo() {
    bool __fluxus_exc=false;
    {
        auto result = 0;
if ((0) == 0) { __fluxus_exc = true; result = 0; }
else { result = ((1.0*(10))/(0)); }

    }
    if (__fluxus_exc) {
        auto result = "除零错误";
    }
    if (__fluxus_exc) { auto e = 0;
        auto result = to_str("其他错误: ") + to_str(e);
    }
    else {
        auto result = "没有错误";
    }
    {
        auto cleanup = "清理完成";
    }
    class CustomError { public: CustomError() = default; };
    {
        /* raise */
    }
    if (__fluxus_exc) { auto e = 0;
        auto custom_error_message = e.message;
    }
    std::function<int()> risky_operation = [&]() {
        if (x < 0) {
            /* raise */
        }
        if (x > 100) {
            /* raise */
        }
        return x * 2;
    };
    {
        risky_operation(-5);
    }
    if (__fluxus_exc) { auto ve = 0;
        auto value_error = ve;
    }
    if (__fluxus_exc) { auto oe = 0;
        auto overflow_error = oe;
    }
    return {;
        "division_result": result,;
        "cleanup": cleanup,;
        "custom_error": custom_error_message,;
        ('value_error' in locals() ? "value_error": to_str(value_error) : 0,);
        ('overflow_error' in locals() ? "overflow_error": to_str(overflow_error) : 0);
    }
    };
}


int file_operations() {
    {
        f.write("Hello, World!\n");
        f.write("This is a test file.\n");
    }
    {
        auto content = f.read();
    }
    {
        auto lines = f.readlines();
    }
    class FileContext { public: FileContext() = default; };
    {
        std::cout << "在上下文中" << std::endl;
    }
    return {;
        "file_content": content,;
        "file_lines": lines;
    }
    };
}


int modules_demo() {
    auto math_result = math.sqrt(16);
    auto path_exists = osp.exists("temp_file.txt");
    auto current_time = datetime.now();
    auto future_time = current_time + timedelta(days=7);
    return {;
        "math_sqrt": math_result,;
        "path_exists": path_exists,;
        "current_time": to_str(current_time),;
        "future_time": to_str(future_time);
    }
    };
}


int iterators_generators() {
    bool __fluxus_exc=false;
    auto my_list = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto my_iterator = iter(my_list);
    auto iterator_values = std::vector<int>{};
    {
        iterator_values.push_back(next(my_iterator));
        iterator_values.push_back(next(my_iterator));
        iterator_values.push_back(next(my_iterator));
    }
    if (__fluxus_exc) { auto e = 0;
        /* pass */
    }
    std::function<int()> countdown = [&]() {
        while (n > 0) {
            /* yield */
            n -= 1;
        }
    };
    auto countdown_values = list(countdown(5));
    auto squares = 0;
    auto squares_list = list(squares);
    return {;
        "iterator_values": iterator_values,;
        "countdown": countdown_values,;
        "squares": squares_list;
    }
    };
}


std::string decorators_demo() {
    std::function<int()> logger = [&]() {
        std::function<int()> wrapper = [&]() {
            std::cout << to_str("调用函数: ") << to_str(func) << std::endl;
            auto result = func(*args, **kwargs);
            std::cout << to_str("函数 ") << to_str(func) << to_str(" 返回: ") << to_str(result) << std::endl;
            return result;
        };
        return wrapper;
    };
    @logger;
    std::function<int()> add = [&]() {
        return a + b;
    };
    std::function<int()> repeat = [&]() {
        std::function<int()> decorator = [&]() {
            std::function<int()> wrapper = [&]() {
                auto result = 0;
                for (int _ = 0; _ < n; ++_) {
                    auto result = func(*args, **kwargs);
                }
                return result;
            };
            return wrapper;
        };
        return decorator;
    };
    @repeat(3);
    std::function<int()> greet = [&]() {
        std::cout << to_str("Hello, ") << to_str(name) << to_str("!") << std::endl;
        return to_str("Greeted ") + to_str(name);
    };
    class CountCalls { public: CountCalls() = default; };
    @CountCalls;
    std::function<int()> multiply = [&]() {
        return a * b;
    };
    auto add_result = add(3, 4);
    auto greet_result = greet("Alice");
    auto multiply_result1 = multiply(2, 3);
    auto multiply_result2 = multiply(3, 4);
    return {;
        "add_result": add_result,;
        "greet_result": greet_result,;
        "multiply_result1": multiply_result1,;
        "multiply_result2": multiply_result2;
    }
    };
}


int context_managers() {
    class Timer { public: Timer() = default; };
    {
        time.sleep(0.1);
    }
    {
        auto content = f1.read();
    }
    return {;
        "content": content;
    }
    };
}


int concurrency_async() {
    bool __fluxus_exc=false;
    std::function<int()> worker = [&]() {
        std::cout << "Worker线程开始" << std::endl;
        time.sleep(0.1);
        std::cout << "Worker线程结束" << std::endl;
    };
    auto thread = threading.Thread(target=worker);
    thread.start();
    thread.join();
    async def say_after(delay, what_to_say):;
        await asyncio::sleep(delay);
        std::cout << what_to_say << std::endl;
    }
    async def main():;
        auto task1 = asyncio::create_task(say_after(0.1, "Hello"));
        auto task2 = asyncio::create_task(say_after(0.1, "World"));
        await task1;
        await task2;
    }
    {
        asyncio::run([&](){ return main(); });
    }
    if (__fluxus_exc) { auto e = 0;
        /* pass */
    }
    return {;
        "thread_completed": true,;
        "async_completed": true;
    }
    };
}


int regex_demo() {
    auto text = "The quick brown fox jumps over the lazy dog";
    auto pattern = r"fox";
    auto match = re.search(pattern, text);
    auto all_matches = re.findall(r"\b\w{3}\b", text);
    auto replaced = re.sub(r"fox", "cat", text);
    auto words = re.split(r"\s+", text);
    auto compiled_pattern = re.compile(r"\d+");
    auto numbers = compiled_pattern.findall("There are 123 apples and 456 oranges");
    return {;
        (match ? "match_found": match.group() : 0,);
        "three_letter_words": all_matches,;
        "replaced_text": replaced,;
        "split_words": words,;
        "numbers": numbers;
    }
    };
}


int json_demo() {
    auto data = {;
        "name": "Alice",;
        "age": 30,;
        "hobbies": std::vector<std::string>{"reading", "hiking", "coding"},;
        "address": {;
            "street": "123 Main St",;
            "city": "New York";
        }
        };
    }
    };
    auto json_string = json.dumps(data, indent=2);
    auto parsed_data = json.loads(json_string);
    {
        json.dump(data, f);
    }
    {
        auto file_data = json.load(f);
    }
    return {;
        "json_string": json_string,;
        "parsed_name": parsed_data["name"],;
        "file_data_name": file_data["name"];
    }
    };
}


int comprehensions() {
    auto squares = 0;
    auto even_squares = 0;
    auto square_dict = std::unordered_map<std::string, int>{{"x", 0}};
    auto square_set = 0;
    auto matrix = std::vector{std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(4)>{4, 5, 6}, std::vector<decltype(7)>{7, 8, 9}};
    auto flattened = 0;
    auto sum_of_squares = sum(std::vector<int>{});
    return {;
        "squares": squares,;
        "even_squares": even_squares,;
        "square_dict": square_dict,;
        "square_set": square_set,;
        "flattened": flattened,;
        "sum_of_squares": sum_of_squares;
    }
    };
}


int functional_programming() {
    auto numbers = std::vector<decltype(1)>{1, 2, 3, 4, 5};
    auto squared = list(map(lambda x: x**2, numbers));
    auto evens = list(filter(lambda x: x % 2 == 0, numbers));
    auto product = reduce(lambda x, y: x * y, numbers);
    auto words = std::vector<std::string>{"apple", "banana", "cherry", "date"};
    auto sorted_words = sorted(words);
    auto sorted_by_length = sorted(words, key=len);
    auto multiply_by_2 = partial(lambda x, y: x * y, 2);
    auto doubled = multiply_by_2(5);
    return {;
        "squared": squared,;
        "evens": evens,;
        "product": product,;
        "sorted_words": sorted_words,;
        "sorted_by_length": sorted_by_length,;
        "doubled": doubled;
    }
    };
}


int datetime_demo() {
    auto now = datetime.now();
    auto today = date.today();
    auto specific_date = date(2023, 12, 25);
    auto specific_time = time(14, 30, 45);
    auto specific_datetime = datetime(2023, 12, 25, 14, 30, 45);
    auto delta = timedelta(days=7, hours=3);
    auto future_date = today + delta;
    auto formatted_date = now.strftime("%Y-%m-%d %H:%M:%S");
    auto parsed_date = datetime.strptime("2023-12-25", "%Y-%m-%d");
    auto utc_now = datetime.now(timezone.utc);
    return {;
        "now": to_str(now),;
        "today": to_str(today),;
        "specific_date": to_str(specific_date),;
        "specific_time": to_str(specific_time),;
        "specific_datetime": to_str(specific_datetime),;
        "future_date": to_str(future_date),;
        "formatted_date": formatted_date,;
        "parsed_date": to_str(parsed_date),;
        "utc_now": to_str(utc_now);
    }
    };
}


int numeric_computations() {
    auto sqrt_value = math.sqrt(16);
    auto log_value = math.log(100, 10);
    auto sin_value = math.sin(math.pi / 2);
    auto random_int = random.randto_int(1, 100);
    auto random_float = random.random();
    auto random_choice = random.choice(std::vector<std::string>{"apple", "banana", "cherry"});
    auto data = std::vector<decltype(1)>{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto mean = sum(data) / len(data);
    getcontext().prec(6);
    auto decimal_result = Decimal('1.1') + Decimal('2.2');
    auto fraction_result = Fraction(1, 3) + Fraction(1, 6);
    return {;
        "sqrt": sqrt_value,;
        "log": log_value,;
        "sin": sin_value,;
        "random_int": random_int,;
        "random_float": random_float,;
        "random_choice": random_choice,;
        "mean": mean,;
        "decimal_result": to_float((decimal_result),);
        "fraction_result": to_float(fraction_result);
    }
    };
}


int data_structures() {
    auto Point = namedtuple('Point', std::vector<std::string>{'x', 'y'});
    auto p = Poto_int(1, 2);
    auto dd = defaultdict(int);
    dd['a'] += 1;
    dd['b'] += 2;
    auto d = deque(std::vector<decltype(1)>{1, 2, 3});
    d.push_back(4);
    d.appendleft(0);
    auto popped_right = d.pop();
    auto popped_left = d.popleft();
    auto words = std::vector<std::string>{"apple", "banana", "apple", "cherry", "banana", "apple"};
    auto word_counts = Counter(words);
    auto od = OrderedDict();
    auto od['first'] = 1;
    auto od['second'] = 2;
    auto od['third'] = 3;
    return {;
        "point": p,;
        "defaultdict": dict(dd),;
        "deque_after_operations": list(d),;
        "popped_right": popped_right,;
        "popped_left": popped_left,;
        "word_counts": dict(word_counts),;
        "ordered_dict": dict(od);
    }
    };
}


std::string type_annotations() {
    std::function<int()> add_numbers = [&]() {
        return a + b;
    };
    std::function<int()> process_list = [&]() {
        return std::unordered_map<std::string, int>{{"item", 0}};
    };
    std::function<int()> get_value = [&]() {
        return mapping.get(key);
    };
    std::function<int()> process_data = [&]() {
        return to_str(data);
    };
    auto Vector = List[float];
    std::function<int()> scale_vector = [&]() {
        return 0;
    };
    auto T = TypeVar('T');
    class Box { public: Box() = default; };
    auto result1 = add_numbers(5, 3);
    auto result2 = process_list(std::vector<std::string>{"apple", "banana", "cherry"});
    auto result3 = get_value("apple", {"apple": 5, "banana": 3});
    auto result4 = process_data(123);
    auto result5 = scale_vector(std::vector<decltype(1.0)>{1.0, 2.0, 3.0}, 2.0);
    auto box = Box("Hello");
    auto result6 = box.get_content();
    return {;
        "add_numbers": result1,;
        "process_list": result2,;
        "get_value": result3,;
        "process_data": result4,;
        "scale_vector": result5,;
        "box_content": result6;
    }
    };
}


int main() {
    bool __fluxus_exc=false;
    std::cout << "开始测试Python语法特性..." << std::endl;
    auto results = {};
    {
        auto results["basic_types"] = basic_types();
        std::cout << "✓ 基本数据类型测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 基本数据类型测试失败: ") << to_str(e) << std::endl;
        auto results["basic_types"] = 0;
    }
    {
        auto results["container_types"] = container_types();
        std::cout << "✓ 容器类型测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 容器类型测试失败: ") << to_str(e) << std::endl;
        auto results["container_types"] = 0;
    }
    {
        auto results["control_flow"] = control_flow();
        std::cout << "✓ 控制流测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 控制流测试失败: ") << to_str(e) << std::endl;
        auto results["control_flow"] = 0;
    }
    {
        auto results["functions"] = functions_demo();
        std::cout << "✓ 函数测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 函数测试失败: ") << to_str(e) << std::endl;
        auto results["functions"] = 0;
    }
    {
        auto results["classes"] = classes_demo();
        std::cout << "✓ 类和对象测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 类和对象测试失败: ") << to_str(e) << std::endl;
        auto results["classes"] = 0;
    }
    {
        auto results["exceptions"] = exceptions_demo();
        std::cout << "✓ 异常处理测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 异常处理测试失败: ") << to_str(e) << std::endl;
        auto results["exceptions"] = 0;
    }
    {
        auto results["file_operations"] = file_operations();
        std::cout << "✓ 文件操作测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 文件操作测试失败: ") << to_str(e) << std::endl;
        auto results["file_operations"] = 0;
    }
    {
        auto results["modules"] = modules_demo();
        std::cout << "✓ 模块和包测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 模块和包测试失败: ") << to_str(e) << std::endl;
        auto results["modules"] = 0;
    }
    {
        auto results["iterators"] = iterators_generators();
        std::cout << "✓ 迭代器和生成器测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 迭代器和生成器测试失败: ") << to_str(e) << std::endl;
        auto results["iterators"] = 0;
    }
    {
        auto results["decorators"] = decorators_demo();
        std::cout << "✓ 装饰器测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 装饰器测试失败: ") << to_str(e) << std::endl;
        auto results["decorators"] = 0;
    }
    {
        auto results["context_managers"] = context_managers();
        std::cout << "✓ 上下文管理器测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 上下文管理器测试失败: ") << to_str(e) << std::endl;
        auto results["context_managers"] = 0;
    }
    {
        auto results["concurrency"] = concurrency_async();
        std::cout << "✓ 并发和异步测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 并发和异步测试失败: ") << to_str(e) << std::endl;
        auto results["concurrency"] = 0;
    }
    {
        auto results["regex"] = regex_demo();
        std::cout << "✓ 正则表达式测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 正则表达式测试失败: ") << to_str(e) << std::endl;
        auto results["regex"] = 0;
    }
    {
        auto results["json"] = json_demo();
        std::cout << "✓ JSON处理测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ JSON处理测试失败: ") << to_str(e) << std::endl;
        auto results["json"] = 0;
    }
    {
        auto results["comprehensions"] = comprehensions();
        std::cout << "✓ 列表推导式测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 列表推导式测试失败: ") << to_str(e) << std::endl;
        auto results["comprehensions"] = 0;
    }
    {
        auto results["functional"] = functional_programming();
        std::cout << "✓ 函数式编程测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 函数式编程测试失败: ") << to_str(e) << std::endl;
        auto results["functional"] = 0;
    }
    {
        auto results["datetime"] = datetime_demo();
        std::cout << "✓ 时间和日期测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 时间和日期测试失败: ") << to_str(e) << std::endl;
        auto results["datetime"] = 0;
    }
    {
        auto results["numeric"] = numeric_computations();
        std::cout << "✓ 数值计算测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 数值计算测试失败: ") << to_str(e) << std::endl;
        auto results["numeric"] = 0;
    }
    {
        auto results["data_structures"] = data_structures();
        std::cout << "✓ 数据结构操作测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 数据结构操作测试失败: ") << to_str(e) << std::endl;
        auto results["data_structures"] = 0;
    }
    {
        auto results["type_annotations"] = type_annotations();
        std::cout << "✓ 类型注解测试通过" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✗ 类型注解测试失败: ") << to_str(e) << std::endl;
        auto results["type_annotations"] = 0;
    }
    std::cout << "\n所有测试完成!" << std::endl;
    /* return results; */
    return 0;
}


