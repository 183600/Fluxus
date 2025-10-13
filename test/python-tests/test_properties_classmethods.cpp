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


class Circle {
    public:
        Circle(int radius) {
            this->_radius = radius;
            this->_color = "red";
        }
        int radius() {
    /* docstring */
    return this->_radius;
}


        int diameter() {
    /* docstring */
    return this->_radius * 2;
}


        int area() {
    /* docstring */
    return math.pi * std::pow(this->_radius, 2);
}


        int circumference() {
    /* docstring */
    return 2 * math.pi * this->_radius;
}


        int color() {
    /* docstring */
    return this->_color;
}


        void color(int value) {
    /* docstring */
    auto valid_colors = std::vector<std::string>{"red", "blue", "green", "yellow", "black", "white"};
    if (value.lower() not in valid_colors) {
        /* raise */
    }
    this->_color = value.lower();
}


        void color() {
    /* docstring */
    std::cout << "Deleting color property" << std::endl;
    this->_color = 0;
}


    public:
        int _radius = 0;
        std::string _color = "";
};


class Temperature {
    public:
        Temperature(int celsius=0) {
            this->_celsius = celsius;
        }
        int celsius() {
    /* docstring */
    return this->_celsius;
}


        void celsius(int value) {
    /* docstring */
    if (value < -273.15) {
        /* raise */
    }
    this->_celsius = value;
}


        int fahrenheit() {
    /* docstring */
    return (this->_celsius * 9/5) + 32;
}


        void fahrenheit(int value) {
    /* docstring */
    this->_celsius = (value - 32) * 5/9;
}


        int kelvin() {
    /* docstring */
    return this->_celsius + 273.15;
}


        void kelvin(int value) {
    /* docstring */
    if (value < 0) {
        /* raise */
    }
    this->_celsius = value - 273.15;
}


    public:
        int _celsius = 0;
};


class BankAccount {
    public:
        BankAccount(int account_number, int initial_balance=0) {
            this->_account_number = account_number;
            this->_balance = initial_balance;
            this->_transaction_history = std::vector<int>{};
            this->_created_at = datetime.now();
        }
        int account_number() {
    /* docstring */
    return this->_account_number;
}


        int balance() {
    /* docstring */
    return this->_balance;
}


        std::string formatted_balance() {
    /* docstring */
    return to_str("$") + to_str(this->_balance);
}


        int transaction_count() {
    /* docstring */
    return len(this->_transaction_history);
}


        int account_age_days() {
    /* docstring */
    return (datetime.now() - this->_created_at).days;
}


        int deposit(int amount) {
    /* docstring */
    if (amount <= 0) {
        /* raise */
    }
    this->_balance += amount;
    this->_transaction_history.push_back({;
        'type': 'deposit',;
        'amount': amount,;
        'timestamp': datetime.now();
    }
    });
    return this->_balance;
}


        int withdraw(int amount) {
    /* docstring */
    if (amount <= 0) {
        /* raise */
    }
    if (amount > this->_balance) {
        /* raise */
    }
    this->_balance -= amount;
    this->_transaction_history.push_back({;
        'type': 'withdrawal',;
        'amount': amount,;
        'timestamp': datetime.now();
    }
    });
    return this->_balance;
}


        int get_interest_rate(int cls) {
    /* docstring */
    return cls._interest_rate;
}


        void set_interest_rate(int cls, int rate) {
    /* docstring */
    if (rate < 0) {
        /* raise */
    }
    cls._interest_rate(rate);
}


        int create_savings_account(std::function<int()> cls, int account_number, int initial_balance) {
    /* docstring */
    auto account = cls(account_number, initial_balance);
    account.account_type("savings");
    return account;
}


        int create_checking_account(std::function<int()> cls, int account_number, int initial_balance) {
    /* docstring */
    auto account = cls(account_number, initial_balance);
    account.account_type("checking");
    return account;
}


        bool validate_account_number(std::vector<int> account_number) {
    /* docstring */
    return (true;
            auto len(account_number) = = 10 and;
            account_number.isdigit());
        }
    }
}


        int calculate_compound_interest(int principal, int rate, int time) {
    /* docstring */
    return principal * (1 + std::pow(rate), time);
}


        int currency_converter(int amount, std::string from_currency, std::string to_currency) {
    /* docstring */
    auto rates = {;
        ('USD', 'CNY'): 7.2,;
        ('CNY', 'USD'): 1/7.2,;
        ('USD', 'EUR'): 0.85,;
        ('EUR', 'USD'): 1/0.85;
    }
    };
    if (from_currency == to_currency) {
        return amount;
    }
    auto rate = rates.get((from_currency, to_currency));
    if (rate is 0) {
        /* raise */
    }
    return amount * rate;
}


    public:
        int _account_number = 0;
        int _balance = 0;
        std::vector<int> _transaction_history;
        int _created_at = 0;
};


class MathUtils {
    public:
        MathUtils() = default;
        int factorial(int n) {
    /* docstring */
    if (n < 0) {
        /* raise */
    }
    if (n == 0 or n == 1) {
        return 1;
    }
    auto result = 1;
    for (int i = 2; i < n + 1; ++i) {
        result *= i;
    }
    return result;
}


        bool is_prime(int n) {
    /* docstring */
    if (n < 2) {
        return false;
    }
    if (n == 2) {
        return true;
    }
    if (n % 2 == 0) {
        return false;
    }
    for (int i = 3; i < int(math.sqrt(n)) + 1; i += 2) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}


        int fibonacci(int n) {
    /* docstring */
    if (n < 0) {
        /* raise */
    }
    if (n == 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    }
    auto a, b = 0, 1;
    for (int _ = 2; _ < n + 1; ++_) {
        auto a, b = b, a + b;
    }
    return b;
}


        int gcd(int a, int b) {
    /* docstring */
    while (b) {
        auto a, b = b, a % b;
    }
    return a;
}


        int lcm(int a, int b) {
    /* docstring */
    return abs(a * b) / MathUtils.gcd(a, b);
}


    public:
        // generated placeholder fields
};


void test_circle_properties() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "=== Testing Circle Properties ===" << std::endl;
    auto circle = Circle(5);
    std::cout << to_str("Circle with radius ") << to_str(circle.radius) << std::endl;
    std::cout << to_str("Diameter: ") << to_str(circle.diameter) << std::endl;
    std::cout << to_str("Area: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<circle.area; return os.str(); }()) << std::endl;
    std::cout << to_str("Circumference: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<circle.circumference; return os.str(); }()) << std::endl;
    std::cout << to_str("Color: ") << to_str(circle.color) << std::endl;
    circle.color("blue");
    std::cout << to_str("Color after change: ") << to_str(circle.color) << std::endl;
    {
        circle.color("purple");
        std::cout << "ERROR: Should not accept invalid color" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Correctly rejected invalid color: ") << to_str(e) << std::endl;
    }
    del circle.color;
    std::cout << to_str("Color after deletion: ") << to_str(circle.color) << std::endl;
}


void test_temperature_properties() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Temperature Properties ===" << std::endl;
    auto temp = Temperature(25);
    std::cout << to_str("Temperature: ") << to_str(temp.celsius) << to_str("°C") << std::endl;
    std::cout << to_str("Fahrenheit: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.fahrenheit; return os.str(); }()) << to_str("°F") << std::endl;
    std::cout << to_str("Kelvin: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.kelvin; return os.str(); }()) << to_str("K") << std::endl;
    temp.fahrenheit(77);
    std::cout << to_str("\\nAfter setting 77°F:") << std::endl;
    std::cout << to_str("Celsius: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.celsius; return os.str(); }()) << to_str("°C") << std::endl;
    std::cout << to_str("Fahrenheit: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.fahrenheit; return os.str(); }()) << to_str("°F") << std::endl;
    std::cout << to_str("Kelvin: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.kelvin; return os.str(); }()) << to_str("K") << std::endl;
    temp.kelvin(300);
    std::cout << to_str("\\nAfter setting 300K:") << std::endl;
    std::cout << to_str("Celsius: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.celsius; return os.str(); }()) << to_str("°C") << std::endl;
    std::cout << to_str("Fahrenheit: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.fahrenheit; return os.str(); }()) << to_str("°F") << std::endl;
    std::cout << to_str("Kelvin: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<temp.kelvin; return os.str(); }()) << to_str("K") << std::endl;
    {
        temp.celsius(-300);
        std::cout << "ERROR: Should not accept temperature below absolute zero" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Correctly rejected invalid temperature: ") << to_str(e) << std::endl;
    }
}


void test_bank_account_classmethods() {
    /* docstring */
    std::cout << "\n=== Testing BankAccount Class Methods ===" << std::endl;
    auto savings = BankAccount.create_savings_account("1234567890", 1000);
    auto checking = BankAccount.create_checking_account("0987654321", 500);
    std::cout << to_str("Savings account: ") << to_str(savings.account_number) << to_str(", Balance: ") << to_str(savings.formatted_balance) << std::endl;
    std::cout << to_str("Checking account: ") << to_str(checking.account_number) << to_str(", Balance: ") << to_str(checking.formatted_balance) << std::endl;
    std::cout << to_str("Current interest rate: ") << to_str(BankAccount.get_interest_rate()) << std::endl;
    BankAccount.set_interest_rate(0.06);
    std::cout << to_str("New interest rate: ") << to_str(BankAccount.get_interest_rate()) << std::endl;
    std::cout << to_str("Is valid account number? ") << to_str(BankAccount.validate_account_number('1234567890')) << std::endl;
    std::cout << to_str("Is valid account number? ") << to_str(BankAccount.validate_account_number('12345')) << std::endl;
    auto compound_interest = BankAccount.calculate_compound_interest(1000, 0.05, 5);
    std::cout << to_str("Compound interest (5 years): $") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<compound_interest; return os.str(); }()) << std::endl;
    auto converted = BankAccount.currency_converter(100, 'USD', 'CNY');
    std::cout << to_str("100 USD to CNY: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<converted; return os.str(); }()) << std::endl;
}


void test_math_utils_staticmethods() {
    /* docstring */
    std::cout << "\n=== Testing MathUtils Static Methods ===" << std::endl;
    std::cout << to_str("Factorial of 5: ") << to_str(MathUtils.factorial(5)) << std::endl;
    std::cout << to_str("Factorial of 0: ") << to_str(MathUtils.factorial(0)) << std::endl;
    auto primes = std::vector<decltype(2)>{2, 3, 5, 7, 11, 13, 17, 19, 23, 29};
    for (auto num : primes) {
        std::cout << to_str("Is ") << to_str(num) << to_str(" prime? ") << to_str(MathUtils.is_prime(num)) << std::endl;
    }
    auto fib_sequence = 0;
    std::cout << to_str("First 10 Fibonacci numbers: ") << to_str(fib_sequence) << std::endl;
    std::cout << to_str("GCD of 48 and 18: ") << to_str(MathUtils.gcd(48, 18)) << std::endl;
    std::cout << to_str("LCM of 12 and 18: ") << to_str(MathUtils.lcm(12, 18)) << std::endl;
    std::cout << to_str("MathUtils.PI: ") << to_str(MathUtils.PI) << std::endl;
    std::cout << to_str("MathUtils.E: ") << to_str(MathUtils.E) << std::endl;
}


void test_property_edge_cases() {
    /* docstring */
    std::cout << "\n=== Testing Property Edge Cases ===" << std::endl;
    auto circle = Circle(3);
    std::cout << to_str("Original color: ") << to_str(circle.color) << std::endl;
    del circle.color;
    std::cout << to_str("Color after deletion: ") << to_str(circle.color) << std::endl;
    circle.color("green");
    std::cout << to_str("Color after reset: ") << to_str(circle.color) << std::endl;
}


void test_inheritance_with_properties() {
    /* docstring */
    std::cout << "\n=== Testing Inheritance with Properties ===" << std::endl;
    auto shapes = std::vector<decltype(Square(4))>{Square(4), Triangle(6, 3)};
    for (auto shape : shapes) {
        std::cout << to_str(shape.name) << to_str(" area: ") << to_str(shape.area) << std::endl;
    }
}


int main() {
    /* docstring */
    std::cout << "Python Property Decorators, Classmethod and Staticmethod Demonstration" << std::endl;
    std::cout << std::string(80, '=') << std::endl;
    test_circle_properties();
    test_temperature_properties();
    test_bank_account_classmethods();
    test_math_utils_staticmethods();
    test_property_edge_cases();
    test_inheritance_with_properties();
    std::cout << "\n=== Summary ===" << std::endl;
    std::cout << "Key concepts demonstrated:" << std::endl;
    std::cout << "1. Property decorators with getter, setter, and deleter" << std::endl;
    std::cout << "2. Calculated properties (read-only)" << std::endl;
    std::cout << "3. Properties with validation" << std::endl;
    std::cout << 0 << std::endl;
    std::cout << 0 << std::endl;
    std::cout << "6. Inheritance with properties" << std::endl;
    std::cout << "\nProperty vs Classmethod vs Staticmethod:" << std::endl;
    std::cout << "- Property: Instance-level computed attributes with optional validation" << std::endl;
    std::cout << "- Classmethod: Operations that affect the class itself or create instances" << std::endl;
    std::cout << "- Staticmethod: Utility functions that don't need instance or class state" << std::endl;
    return 0;
}


