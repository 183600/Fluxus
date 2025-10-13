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
        Person(std::string name, int age, int city) {
            this->name = name;
            this->age = age;
            this->city = city;
        }
        int to_dict() {
    return {;
        "name": this->name,;
        "age": this->age,;
        "city": this->city;
    }
    };
}


    public:
        std::string name = "";
        int age = 0;
        int city = 0;
};


void test_json_serialization() {
    auto data = {;
        "name": "Alice",;
        "age": 30,;
        "city": "New York",;
        "hobbies": std::vector<std::string>{"reading", "swimming", "coding"},;
        "is_student": false,;
        "address": {;
            "street": "123 Main St",;
            "city": "New York",;
            "zip": "10001";
        }
        };
    }
    };
    auto json_string = json.dumps(data, indent=2);
    std::cout << "JSON serialization:" << std::endl;
    std::cout << json_string << std::endl;
    auto parsed_data = json.loads(json_string);
    std::cout << "\nParsed data:" << std::endl;
    std::cout << to_str("Name: ") << to_str(parsed_data['name']) << std::endl;
    std::cout << to_str("Age: ") << to_str(parsed_data['age']) << std::endl;
    std::cout << to_str("Hobbies: ") << to_str(parsed_data['hobbies']) << std::endl;
}


void test_json_file_operations() {
    auto data = {;
        "users": [;
            {"id": 1, "name": "Alice", "email": "alice@example.com"},;
            {"id": 2, "name": "Bob", "email": "bob@example.com"},;
            std::unordered_map<std::string, std::string>{{"id", to_str(3)}, {"name", "Charlie"}, {"email", "charlie@example.com"}};
        }
        ],;
        "settings": {;
            "theme": "dark",;
            "language": "en",;
            "notifications": true;
        }
        };
    }
    };
    auto filename = "test_data.json";
    {
        json.dump(data, f, indent(2));
    }
    std::cout << to_str("Data written to ") << to_str(filename) << std::endl;
    {
        auto loaded_data = json.load(f);
    }
    std::cout << "\nLoaded data:" << std::endl;
    std::cout << to_str("Number of users: ") << to_str(len(loaded_data['users'])) << std::endl;
    std::cout << to_str("Theme: ") << to_str(loaded_data['settings']['theme']) << std::endl;
    os.remove(filename);
}


void test_json_custom_objects() {
    auto people = [;
        Person("Alice", 30, "New York"),;
        Person("Bob", 25, "Los Angeles"),;
        Person("Charlie", 35, "Chicago");
    }
    ];
    auto people_data = 0;
    auto json_string = json.dumps(people_data, indent=2);
    std::cout << "\nCustom objects to JSON:" << std::endl;
    std::cout << json_string << std::endl;
    auto parsed_people = json.loads(json_string);
    std::cout << "\nParsed people:" << std::endl;
    for (auto person : parsed_people) {
        std::cout << to_str(person['name']) << to_str(", ") << to_str(person['age']) << to_str(", ") << to_str(person['city']) << std::endl;
    }
}


void test_json_validation() {
    bool __fluxus_exc=false;
    auto valid_json = '{"name": "Alice", "age": 30}';
    auto invalid_json = '{"name": "Alice", "age": 30,';
    std::cout << "\nJSON validation:" << std::endl;
    {
        auto parsed = json.loads(valid_json);
        std::cout << to_str("Valid JSON parsed: ") << to_str(parsed) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Valid JSON failed: ") << to_str(e) << std::endl;
    }
    {
        auto parsed = json.loads(invalid_json);
        std::cout << to_str("Invalid JSON parsed: ") << to_str(parsed) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Invalid JSON failed: ") << to_str(e) << std::endl;
    }
}


void test_json_pretty_print() {
    auto data = {;
        "students": [;
            {"name": "Alice", "grades": std::vector<decltype(90)>{90, 85, 95}},;
            {"name": "Bob", "grades": std::vector<decltype(80)>{80, 75, 85}},;
            std::unordered_map<std::string, std::string>{{"name", "Charlie"}, {"grades", to_str(std::vector<decltype(95)>{95})}, {"90", to_str(0)}, {"88]", to_str(0)}};
        }
        ],;
        "class_info": {;
            "subject": "Mathematics",;
            "teacher": "Mr. Smith",;
            "room": "101";
        }
        };
    }
    };
    std::cout << "\nJSON pretty printing:" << std::endl;
    auto compact = json.dumps(data, separators=(',', ':'));
    std::cout << "Compact:" << std::endl;
    std::cout << compact << std::endl;
    auto pretty = json.dumps(data, indent=2);
    std::cout << "\nPretty:" << std::endl;
    std::cout << pretty << std::endl;
}


void test_json_data_types() {
    auto data = {;
        "string": "Hello World",;
        "number": 42,;
        "float": 3.14,;
        "boolean": true,;
        "null": 0,;
        "array": std::vector<std::string>{to_str(1), to_str(2), to_str(3), "four", to_str(true)},;
        "nested": {;
            "level1": {;
                "level2": {;
                    "deep": "value";
                }
                };
            }
            };
        }
        };
    }
    };
    auto json_string = json.dumps(data, indent=2);
    std::cout << "\nJSON data types:" << std::endl;
    std::cout << json_string << std::endl;
    auto parsed = json.loads(json_string);
    std::cout << "\nParsed types:" << std::endl;
    std::cout << to_str("String: ") << to_str(type(parsed['string'])) << std::endl;
    std::cout << to_str("Number: ") << to_str(type(parsed['number'])) << std::endl;
    std::cout << to_str("Float: ") << to_str(type(parsed['float'])) << std::endl;
    std::cout << to_str("Boolean: ") << to_str(type(parsed['boolean'])) << std::endl;
    std::cout << to_str("Null: ") << to_str(type(parsed['null'])) << std::endl;
    std::cout << to_str("Array: ") << to_str(type(parsed['array'])) << std::endl;
}


int main() {
    std::cout << "=== JSON Serialization ===" << std::endl;
    test_json_serialization();
    std::cout << "\n=== JSON File Operations ===" << std::endl;
    test_json_file_operations();
    std::cout << "\n=== JSON Custom Objects ===" << std::endl;
    test_json_custom_objects();
    std::cout << "\n=== JSON Validation ===" << std::endl;
    test_json_validation();
    std::cout << "\n=== JSON Pretty Printing ===" << std::endl;
    test_json_pretty_prto_int();
    std::cout << "\n=== JSON Data Types ===" << std::endl;
    test_json_data_types();
    return 0;
}

