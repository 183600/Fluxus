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


void test_counter() {
    auto words = std::vector<std::string>{'apple', 'banana', 'apple', 'cherry', 'banana', 'apple', 'date'};
    auto word_counts = Counter(words);
    std::cout << "Words:" << " " << words << std::endl;
    std::cout << "Word counts:" << " " << word_counts << std::endl;
    auto most_common = word_counts.most_common(2);
    std::cout << "Most common:" << " " << most_common << std::endl;
    auto counter1 = Counter(std::vector<std::string>{'a', 'a', 'b', 'c'});
    auto counter2 = Counter(std::vector<std::string>{'a', 'b', 'b', 'd'});
    std::cout << "\nCounter1:" << " " << counter1 << std::endl;
    std::cout << "Counter2:" << " " << counter2 << std::endl;
    std::cout << "Addition:" << " " << counter1 + counter2 << std::endl;
    std::cout << "Subtraction:" << " " << counter1 - counter2 << std::endl;
    std::cout << "Intersection:" << " " << counter1 & counter2 << std::endl;
    std::cout << "Union:" << " " << counter1 | counter2 << std::endl;
}


void test_defaultdict() {
    auto grouped_by_length = defaultdict(list);
    auto words = std::vector<std::string>{'cat', 'dog', 'elephant', 'rat', 'mouse'};
    for (auto word : words) {
        grouped_by_length[len(word)].push_back(word);
    }
    std::cout << "Words grouped by length:" << " " << dict(grouped_by_length) << std::endl;
    auto char_count = defaultdict(int);
    auto text = "hello world";
    for (auto char : text) {
        char_count[char] += 1;
    }
    std::cout << "\nCharacter counts:" << " " << dict(char_count) << std::endl;
    auto first_letters = defaultdict(set);
    auto words = std::vector<std::string>{'apple', 'ant', 'banana', 'ball', 'cherry'};
    for (auto word : words) {
        first_letters[word[0]].add(word);
    }
    std::cout << "\nWords by first letter:" << " " << dict(first_letters) << std::endl;
}


void test_ordereddict() {
    auto ordered_dict = OrderedDict();
    auto ordered_dict['first'] = 1;
    auto ordered_dict['second'] = 2;
    auto ordered_dict['third'] = 3;
    std::cout << "Ordered dict:" << " " << ordered_dict << std::endl;
    std::cout << "Keys in order:" << " " << list(ordered_dict.keys()) << std::endl;
    ordered_dict.move_to_end('first');
    std::cout << "After moving 'first' to end:" << " " << list(ordered_dict.keys()) << std::endl;
    ordered_dict.move_to_end('second', last(false));
    std::cout << "After moving 'second' to front:" << " " << list(ordered_dict.keys()) << std::endl;
    auto last_item = ordered_dict.popitem();
    std::cout << "Popped last item:" << " " << last_item << std::endl;
    std::cout << "Remaining:" << " " << list(ordered_dict.keys()) << std::endl;
}


void test_deque() {
    auto d = deque(std::vector<decltype(1)>{1, 2, 3, 4, 5});
    std::cout << "Original deque:" << " " << d << std::endl;
    d.push_back(6);
    d.appendleft(0);
    std::cout << "After append(6) and appendleft(0):" << " " << d << std::endl;
    auto right_pop = d.pop();
    auto left_pop = d.popleft();
    std::cout << to_str("Popped right: ") << to_str(right_pop) << to_str(", left: ") << to_str(left_pop) << std::endl;
    std::cout << "After pops:" << " " << d << std::endl;
    d.extend(std::vector<decltype(7)>{7, 8, 9});
    d.extendleft(std::vector<decltype(-1)>{-1, -2});
    std::cout << "After extend(std::vector<decltype(7)>{7,8,9}) and extendleft(std::vector<decltype(-1)>{-1,-2}):" << " " << d << std::endl;
    d.rotate(2);
    std::cout << "After rotate(2):" << " " << d << std::endl;
    d.rotate(-1);
    std::cout << "After rotate(-1):" << " " << d << std::endl;
    d.clear();
    std::cout << "After clear:" << " " << d << std::endl;
}


void test_namedtuple() {
    auto Person = namedtuple('Person', std::vector<std::string>{'name', 'age', 'city'});
    auto Student = namedtuple('Student', std::vector<std::string>{'name', 'student_id', 'grades'}, defaults=std::vector<decltype(0)>{0});
    auto person1 = Person('Alice', 30, 'New York');
    auto person2 = Person('Bob', 25, 'Los Angeles');
    auto student = Student('Charlie', 'S123', std::vector<decltype(90)>{90, 85, 95});
    std::cout << "Person1:" << " " << person1 << std::endl;
    std::cout << "Person2:" << " " << person2 << std::endl;
    std::cout << "Student:" << " " << student << std::endl;
    std::cout << to_str("\\nPerson1 name: ") << to_str(person1.name) << std::endl;
    std::cout << to_str("Person1 age: ") << to_str(person1.age) << std::endl;
    std::cout << to_str("Person1[0]: ") << to_str(person1[0]) << std::endl;
    std::cout << to_str("Person1[1]: ") << to_str(person1[1]) << std::endl;
    auto person_dict = person1._asdict();
    std::cout << "Person1 as dict:" << " " << person_dict << std::endl;
    auto updated_person = person1._replace(age=31);
    std::cout << "Updated person:" << " " << updated_person << std::endl;
}


void test_heap() {
    auto numbers = std::vector<decltype(3)>{3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5};
    heapq.heapify(numbers);
    std::cout << "Heapified list:" << " " << numbers << std::endl;
    heapq.heappush(numbers, 0);
    heapq.heappush(numbers, 7);
    std::cout << "After pushing 0 and 7:" << " " << numbers << std::endl;
    auto smallest = heapq.heappop(numbers);
    auto second_smallest = heapq.heappop(numbers);
    std::cout << to_str("Smallest: ") << to_str(smallest) << to_str(", Second smallest: ") << to_str(second_smallest) << std::endl;
    std::cout << "After pops:" << " " << numbers << std::endl;
    auto smallest_3 = heapq.nsmallest(3, numbers);
    std::cout << "3 smallest items:" << " " << smallest_3 << std::endl;
    auto largest_3 = heapq.nlargest(3, numbers);
    std::cout << "3 largest items:" << " " << largest_3 << std::endl;
}


void test_chainmap() {
    auto dict1 = std::unordered_map<std::string, int>{{"a", 1}, {"b", 2}};
    auto dict2 = std::unordered_map<std::string, int>{{"b", 3}, {"c", 4}};
    auto dict3 = std::unordered_map<std::string, int>{{"c", 5}, {"d", 6}};
    auto chain = ChainMap(dict1, dict2, dict3);
    std::cout << "ChainMap:" << " " << chain << std::endl;
    std::cout << "Keys:" << " " << list(chain.keys()) << std::endl;
    std::cout << "Values:" << " " << list(chain.values()) << std::endl;
    std::cout << "Value of 'a':" << " " << chain['a'] << std::endl;
    std::cout << "Value of 'b':" << " " << chain['b'] << std::endl;
    std::cout << "Value of 'c':" << " " << chain['c'] << std::endl;
    std::cout << "Value of 'd':" << " " << chain['d'] << std::endl;
    auto new_chain = chain.new_child({'e': 7, 'a': 10});
    std::cout << "\nNew chain with new mapping:" << " " << new_chain << std::endl;
    std::cout << "Value of 'a' in new chain:" << " " << new_chain['a'] << std::endl;
}


int main() {
    std::cout << "=== Counter ===" << std::endl;
    test_counter();
    std::cout << "\n=== DefaultDict ===" << std::endl;
    test_defaultdict();
    std::cout << "\n=== OrderedDict ===" << std::endl;
    test_ordereddict();
    std::cout << "\n=== Deque ===" << std::endl;
    test_deque();
    std::cout << "\n=== NamedTuple ===" << std::endl;
    test_namedtuple();
    std::cout << "\n=== Heap Operations ===" << std::endl;
    test_heap();
    std::cout << "\n=== ChainMap ===" << std::endl;
    test_chainmap();
    return 0;
}

