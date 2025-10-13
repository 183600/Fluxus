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
    auto now = datetime.now();
    std::cout << "Current datetime:" << " " << now << std::endl;
    std::cout << "Current date:" << " " << now.date() << std::endl;
    std::cout << "Current time:" << " " << now.time() << std::endl;
    auto formatted = now.strftime("%Y-%m-%d %H:%M:%S");
    std::cout << "Formatted datetime:" << " " << formatted << std::endl;
    auto date_string = "2023-12-25 15:30:00";
    auto parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S");
    std::cout << "Parsed datetime:" << " " << parsed_date << std::endl;
    auto today = date.today();
    auto tomorrow = today + timedelta(days=1);
    auto yesterday = today - timedelta(days=1);
    auto next_week = today + timedelta(weeks=1);
    std::cout << "\nDate arithmetic:" << std::endl;
    std::cout << "Today:" << " " << today << std::endl;
    std::cout << "Tomorrow:" << " " << tomorrow << std::endl;
    std::cout << "Yesterday:" << " " << yesterday << std::endl;
    std::cout << "Next week:" << " " << next_week << std::endl;
    auto date1 = date(2023, 1, 1);
    auto date2 = date(2023, 12, 31);
    auto difference = date2 - date1;
    std::cout << to_str("\\nDays between ") << to_str(date1) << to_str(" and ") << to_str(date2) << to_str(": ") << to_str(difference.days) << std::endl;
    auto specific_datetime = datetime(2023, 12, 25, 15, 30, 0);
    std::cout << "Specific datetime:" << " " << specific_datetime << std::endl;
    auto current_time = time_module.time();
    std::cout << to_str("Unix timestamp: ") << to_str(current_time) << std::endl;
    std::cout << 0 << std::endl;
    time_module.sleep(0.5);
    std::cout << "Done sleeping!" << std::endl;
    std::cout << "\nDate components:" << std::endl;
    std::cout << "Year:" << " " << now.year << std::endl;
    std::cout << "Month:" << " " << now.month << std::endl;
    std::cout << "Day:" << " " << now.day << std::endl;
    std::cout << "Hour:" << " " << now.hour << std::endl;
    std::cout << "Minute:" << " " << now.minute << std::endl;
    std::cout << "Second:" << " " << now.second << std::endl;
    auto weekday_names = std::vector<std::string>{"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"};
    std::cout << to_str("Today is ") << to_str(weekday_names[now.weekday()]) << std::endl;
    std::cout << "\nTime operations:" << std::endl;
    auto noon = time(12, 0, 0);
    std::cout << "Noon time:" << " " << noon << std::endl;
    std::cout << "Time hour:" << " " << noon.hour << std::endl;
    std::cout << "Time minute:" << " " << noon.minute << std::endl;
    return 0;
}

