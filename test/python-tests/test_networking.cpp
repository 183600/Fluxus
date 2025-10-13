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


void test_url_parsing() {
    auto urls = [;
        "https://www.example.com/path/to/page?param1(value1&param2=value2#fragment",);
        "http://subdomain.domain.com:8080/api/v1/users",;
        "ftp://files.server.org/download/file.txt",;
        "https://example.com",;
        "mailto:user@example.com";
    }
    ];
    std::cout << "URL parsing:" << std::endl;
    for (auto url : urls) {
        auto parsed = urlparse(url);
        std::cout << to_str("\\nURL: ") << to_str(url) << std::endl;
        std::cout << to_str("  Scheme: ") << to_str(parsed.scheme) << std::endl;
        std::cout << to_str("  Netloc: ") << to_str(parsed.netloc) << std::endl;
        std::cout << to_str("  Path: ") << to_str(parsed.path) << std::endl;
        std::cout << to_str("  Params: ") << to_str(parsed.params) << std::endl;
        std::cout << to_str("  Query: ") << to_str(parsed.query) << std::endl;
        std::cout << to_str("  Fragment: ") << to_str(parsed.fragment) << std::endl;
    }
}


void test_http_get() {
    bool __fluxus_exc=false;
    auto url = "https://httpbin.org/get";
    auto params = std::unordered_map<std::string, std::string>{{"key1", 'value1'}, {"key2", 'value2'}};
    {
        auto encoded_params = urllib.parse.urlencode(params);
        auto full_url = to_str(url) + to_str("?") + to_str(encoded_params);
        std::cout << to_str("Making GET request to: ") << to_str(full_url) << std::endl;
        {
            auto content = response.read().decode('utf-8');
            auto data = json.loads(content);
            std::cout << "Response status:" << " " << response.status << std::endl;
            std::cout << "Response headers:" << " " << dict(response.headers) << std::endl;
            std::cout << "Response data:" << " " << json.dumps(data, indent=2) << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("URL Error: ") << to_str(e) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Error: ") << to_str(e) << std::endl;
    }
}


void test_http_post() {
    bool __fluxus_exc=false;
    auto url = "https://httpbin.org/post";
    auto data = {;
        'name': 'John Doe',;
        'email': 'john@example.com',;
        'message': 'Hello, World!';
    }
    };
    {
        auto encoded_data = urllib.parse.urlencode(data).encode('utf-8');
        auto req = urllib.request.Request(url, data=encoded_data, method='POST');
        req.add_header('Content-Type', 'application/x-www-form-urlencoded');
        std::cout << to_str("Making POST request to: ") << to_str(url) << std::endl;
        {
            auto content = response.read().decode('utf-8');
            auto response_data = json.loads(content);
            std::cout << "Response status:" << " " << response.status << std::endl;
            std::cout << "Response data:" << " " << json.dumps(response_data, indent=2) << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("URL Error: ") << to_str(e) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Error: ") << to_str(e) << std::endl;
    }
}


void test_json_api() {
    bool __fluxus_exc=false;
    auto url = "https://jsonplaceholder.typicode.com/posts/1";
    {
        std::cout << to_str("Making JSON request to: ") << to_str(url) << std::endl;
        {
            auto content = response.read().decode('utf-8');
            auto data = json.loads(content);
            std::cout << "Response status:" << " " << response.status << std::endl;
            std::cout << "Content-Type:" << " " << response.getheader('Content-Type') << std::endl;
            std::cout << "Post data:" << std::endl;
            std::cout << to_str("  ID: ") << to_str(data['id']) << std::endl;
            std::cout << to_str("  Title: ") << to_str(data['title']) << std::endl;
            std::cout << to_str("  Body: ") << to_str(data['body'][) << to_str("...") << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("URL Error: ") << to_str(e) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Error: ") << to_str(e) << std::endl;
    }
}


void test_socket_operations() {
    bool __fluxus_exc=false;
    auto host = "www.example.com";
    auto port = 80;
    std::cout << to_str("Testing socket operations for ") << to_str(host) << to_str(":") << to_str(port) << std::endl;
    {
        auto ip = socket.gethostbyname(host);
        std::cout << to_str("IP address of ") << to_str(host) << to_str(": ") << to_str(ip) << std::endl;
        auto sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
        sock.settimeout(5);
        sock.connect((host, port));
        std::cout << to_str("Connected to ") << to_str(host) << to_str(":") << to_str(port) << std::endl;
        auto request = 0;
if ((HTTP/1.1\r\nHost: {host}\r\nConnection: close\r\n\r\n") == 0) { __fluxus_exc = true; request = 0; }
else { request = ((1.0*(f"GET))/(HTTP/1.1\r\nHost: {host}\r\nConnection: close\r\n\r\n")); }

        sock.send(request.encode());
        auto response = sock.recv(4096).decode('utf-8');
        std::cout << "\nHTTP Response (first few lines):" << std::endl;
        for (auto line : response.split('\r\n')[:10]) {
            if (line.strip()) {
                std::cout << to_str("  ") << to_str(line) << std::endl;
            }
        }
        sock.close();
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << "Connection timed out" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Socket error: ") << to_str(e) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Error: ") << to_str(e) << std::endl;
    }
}


void test_url_encoding() {
    auto original_url = "https://example.com/search?q=Python programming&category=web development&page=1";
    auto parsed = urlparse(original_url);
    auto query_params = urllib.parse.parse_qs(parsed.query);
    std::cout << "Original URL:" << " " << original_url << std::endl;
    std::cout << "Parsed query parameters:" << " " << query_params << std::endl;
    auto query_params['q'] = std::vector<std::string>{'Data Science'};
    auto query_params['sort'] = std::vector<std::string>{'relevance'};
    auto new_query = urllib.parse.urlencode(query_params, doseq=true);
    auto new_url = to_str(parsed.scheme) + to_str("://") + to_str(parsed.netloc) + to_str(parsed.path) + to_str("?") + to_str(new_query);
    std::cout << "\nNew URL:" << " " << new_url << std::endl;
    auto text = "Hello World! Programming & Coding";
    auto encoded_text = urllib.parse.quote(text);
    std::cout << to_str("\\nOriginal text: ") << to_str(text) << std::endl;
    std::cout << to_str("URL encoded: ") << to_str(encoded_text) << std::endl;
    auto decoded_text = urllib.parse.unquote(encoded_text);
    std::cout << to_str("URL decoded: ") << to_str(decoded_text) << std::endl;
}


void test_http_headers() {
    bool __fluxus_exc=false;
    auto url = "https://httpbin.org/headers";
    {
        auto req = urllib.request.Request(url);
        req.add_header('User-Agent', 'Mozilla/5.0 (Test Script)');
        req.add_header('Accept', 'application/json');
        req.add_header('X-Custom-Header', 'TestValue');
        std::cout << to_str("Making request with custom headers to: ") << to_str(url) << std::endl;
        {
            auto content = response.read().decode('utf-8');
            auto data = json.loads(content);
            std::cout << "Response status:" << " " << response.status << std::endl;
            std::cout << "Response headers from server:" << " " << json.dumps(data['headers'], indent=2) << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("URL Error: ") << to_str(e) << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Error: ") << to_str(e) << std::endl;
    }
}


void test_error_handling() {
    bool __fluxus_exc=false;
    auto urls = [;
        "https://www.example.com",;
        "https://this-domain-does-not-exist.com",;
        "https://www.example.com/nonexistent-page",;
        "http://localhost:9999",;
    }
    ];
    std::cout << "Testing error handling:" << std::endl;
    for (auto url : urls) {
        {
            std::cout << to_str("\\nTrying: ") << to_str(url) << std::endl;
            auto response = urllib.request.urlopen(url, timeout=5);
            std::cout << to_str("  Success: ") << to_str(response.status) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("  HTTP Error ") << to_str(e.code) << to_str(": ") << to_str(e.reason) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("  URL Error: ") << to_str(e.reason) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("  Other error: ") << to_str(e) << std::endl;
        }
    }
}


int main() {
    std::cout << "=== URL Parsing ===" << std::endl;
    test_url_parsing();
    std::cout << "\n=== HTTP GET Request ===" << std::endl;
    test_http_get();
    std::cout << "\n=== HTTP POST Request ===" << std::endl;
    test_http_post();
    std::cout << "\n=== JSON API Request ===" << std::endl;
    test_json_api();
    std::cout << "\n=== Socket Operations ===" << std::endl;
    test_socket_operations();
    std::cout << "\n=== URL Encoding/Decoding ===" << std::endl;
    test_url_encoding();
    std::cout << "\n=== HTTP Headers ===" << std::endl;
    test_http_headers();
    std::cout << "\n=== Error Handling ===" << std::endl;
    test_error_handling();
    return 0;
}

