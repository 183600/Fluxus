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


void test_basic_patterns() {
    auto text = "Hello World 123 Python 456";
    auto digits = re.findall(r'\d+', text);
    std::cout << "Text:" << " " << text << std::endl;
    std::cout << "Digits found:" << " " << digits << std::endl;
    auto words = re.findall(r'\w+', text);
    std::cout << "Words found:" << " " << words << std::endl;
    auto capitals = re.findall(r'std::vector<decltype(A-Z)>{A-Z}', text);
    std::cout << "Capital letters:" << " " << capitals << std::endl;
}


void test_groups() {
    auto text = "John: 30 years old, Jane: 25 years old, Bob: 35 years old";
    auto pattern = r'(\w+): (\d+) years old';
    auto matches = re.findall(pattern, text);
    std::cout << "Text:" << " " << text << std::endl;
    std::cout << "Name-Age pairs:" << " " << matches << std::endl;
    for (auto match : re.finditer(pattern, text)) {
        std::cout << to_str("Match: ") << to_str(match.group(1)) << to_str(" is ") << to_str(match.group(2)) << to_str(" years old") << std::endl;
    }
}


void test_email_validation() {
    auto emails = [;
        "user@example.com",;
        "john.doe@company.co.uk",;
        "invalid-email",;
        "another@domain",;
        "valid.email123@sub.domain.com",;
        "@invalid.com",;
        "missing@dotcom";
    }
    ];
    auto email_pattern = r'^std::vector<decltype(a-zA-Z0-9._%+-)>{a-zA-Z0-9._%+-}+@std::vector<decltype(a-zA-Z0-9.-)>{a-zA-Z0-9.-}+\.std::vector<decltype(a-zA-Z)>{a-zA-Z}{2,}$';
    std::cout << "Email validation:" << std::endl;
    for (auto email : emails) {
        auto is_valid = bool(re.match(email_pattern, email));
        std::cout << to_str(email) << to_str(": ") << to_str((is_valid ? 'Valid' : 'Invalid')) << std::endl;
    }
}


void test_phone_extraction() {
    auto text = """;
    Contact us at:;
    Phone: 123-456-7890;
    Mobile: (555) 123-4567;
    International: +1-800-555-1234;
    Invalid: 123456;
    /* docstring */
    auto patterns = [;
        r'\d{3}-\d{3}-\d{4}',;
        r'\(\d{3}\) \d{3}-\d{4}',;
        r'\+\d{1,3}-\d{3}-\d{3}-\d{4}';
    }
    ];
    std::cout << "Phone number extraction:" << std::endl;
    for (auto pattern : patterns) {
        auto matches = re.findall(pattern, text);
        std::cout << to_str("Pattern '") << to_str(pattern) << to_str("': ") << to_str(matches) << std::endl;
    }
}


void test_substitution() {
    auto text = "The quick brown fox jumps over the lazy dog.";
    auto new_text = re.sub(r'fox', 'cat', text);
    std::cout << "Original:" << " " << text << std::endl;
    std::cout << "After substitution:" << " " << new_text << std::endl;
    auto messy_text = "This   has    multiple    spaces";
    auto cleaned = re.sub(r'\s+', ' ', messy_text);
    std::cout << to_str("\\nMessy: '") << to_str(messy_text) << to_str("'") << std::endl;
    std::cout << to_str("Cleaned: '") << to_str(cleaned) << to_str("'") << std::endl;
    auto text_with_punct = "Hello, world! How are you?";
    auto no_punct = re.sub(r'std::vector<decltype(^\w\s)>{^\w\s}', '', text_with_punct);
    std::cout << to_str("\\nWith punctuation: '") << to_str(text_with_punct) << to_str("'") << std::endl;
    std::cout << to_str("Without punctuation: '") << to_str(no_punct) << to_str("'") << std::endl;
}


void test_splitting() {
    auto text = "apple,banana;cherry orange|grape";
    auto fruits = re.split(r'std::vector{,;| }+', text);
    std::cout << "Text:" << " " << text << std::endl;
    std::cout << "Split fruits:" << " " << fruits << std::endl;
    auto sentences = "First sentence. Second sentence! Third sentence?";
    auto sentence_parts = re.split(r'std::vector<decltype(.!?)>{.!?}+', sentences);
    std::cout << "\nSentences:" << " " << sentence_parts << std::endl;
}


void test_word_boundaries() {
    auto text = "Python is a pythonic language. PYTHON programming is fun!";
    auto python_words = re.findall(r'\bPython\b', text);
    std::cout << "Text:" << " " << text << std::endl;
    std::cout << "Exact 'Python' matches:" << " " << python_words << std::endl;
    auto python_insensitive = re.findall(r'\bpython\b', text, re.IGNORECASE);
    std::cout << "Case insensitive 'python' matches:" << " " << python_insensitive << std::endl;
}


void test_lookahead_lookbehind() {
    auto text = "apple pie, banana split, cherry tart, apple juice";
    auto followed_by_pie = re.findall(r'\w+(?= pie)', text);
    std::cout << "Text:" << " " << text << std::endl;
    std::cout << "Words followed by 'pie':" << " " << followed_by_pie << std::endl;
    auto preceded_by_apple = re.findall(r'(?<=apple )\w+', text);
    std::cout << "Words preceded by 'apple':" << " " << preceded_by_apple << std::endl;
}


void test_url_extraction() {
    auto text = """;
    Visit https://www.example.com or http://sub.domain.com/path;
    Check out www.another-site.com and ftp://files.server.org;
    Not a URL: just.some.text;
    /* docstring */
    auto url_pattern = r'https?://std::vector<std::string>{^\s<>"{}|\\^`\[\]}+|www\.std::vector<std::string>{^\s<>"{}|\\^`\[\]}+';
    auto urls = re.findall(url_pattern, text);
    std::cout << "URL extraction:" << std::endl;
    std::cout << "Found URLs:" << " " << urls << std::endl;
}


void test_html_tags() {
    auto html_text = """;
    auto <div class = "container">;
        <h1>Title</h1>;
        <p>Paragraph text</p>;
        auto <a href = "https://example.com">Link</a>;
    }
    </div>;
    /* docstring */
    auto tag_pattern = r'<(std::vector<decltype(^>)>{^>}+)>';
    auto tags = re.findall(tag_pattern, html_text);
    std::cout << "HTML tag extraction:" << std::endl;
    std::cout << "Found tags:" << " " << tags << std::endl;
}


int main() {
    std::cout << "=== Basic Patterns ===" << std::endl;
    test_basic_patterns();
    std::cout << "\n=== Groups ===" << std::endl;
    test_groups();
    std::cout << "\n=== Email Validation ===" << std::endl;
    test_email_validation();
    std::cout << "\n=== Phone Number Extraction ===" << std::endl;
    test_phone_extraction();
    std::cout << "\n=== Substitution ===" << std::endl;
    test_substitution();
    std::cout << "\n=== Splitting ===" << std::endl;
    test_splitting();
    std::cout << "\n=== Word Boundaries ===" << std::endl;
    test_word_boundaries();
    std::cout << "\n=== Lookahead and Lookbehind ===" << std::endl;
    test_lookahead_lookbehind();
    std::cout << "\n=== URL Extraction ===" << std::endl;
    test_url_extraction();
    std::cout << "\n=== HTML Tag Extraction ===" << std::endl;
    test_html_tags();
    return 0;
}

