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


class SimpleRESTHandler : public http.server.SimpleHTTPRequestHandler {
    public:
        template<typename... Args> SimpleRESTHandler(Args... args) : http.server.SimpleHTTPRequestHandler(args...) {}
        SimpleRESTHandler() = default;
        void do_GET() {
    bool __fluxus_exc=false;
    /* docstring */
    if (this->path == '/api/users') {
        this->send_response(200);
        this->send_header('Content-type', 'application/json');
        this->end_headers();
        this->wfile.write(json.dumps(list(this->data_store.values())).encode());
    }
    else if (this->path.startswith('/api/users/')) {
        {
            auto user_id = to_int(this->path.split('/')[-1]);
            if (user_id in this->data_store) {
                this->send_response(200);
                this->send_header('Content-type', 'application/json');
                this->end_headers();
                this->wfile.write(json.dumps(this->data_store[user_id]).encode());
            }
            else {
                this->send_error(404, "User not found");
            }
        }
        if (__fluxus_exc) { auto e = 0;
            this->send_error(400, "Invalid user ID");
        }
    }
    else {
        this->send_error(404, "Endpoint not found");
    }
}


        void do_POST() {
    bool __fluxus_exc=false;
    /* docstring */
    if (this->path == '/api/users') {
        {
            auto content_length = to_int(this->headers['Content-Length']);
            auto post_data = this->rfile.read(content_length);
            auto user_data = json.loads(post_data.decode());
            auto new_user = {;
                "id": this->next_id,;
                "name": user_data.get("name", ""),;
                "age": user_data.get("age", 0);
            }
            };
            this->data_store[this->next_id] = new_user;
            this->next_id += 1;
            this->send_response(201);
            this->send_header('Content-type', 'application/json');
            this->end_headers();
            this->wfile.write(json.dumps(new_user).encode());
        }
        if (__fluxus_exc) { auto e = 0;
            this->send_error(400, "Invalid JSON data");
        }
    }
    else {
        this->send_error(404, "Endpoint not found");
    }
}


        void do_PUT() {
    bool __fluxus_exc=false;
    /* docstring */
    if (this->path.startswith('/api/users/')) {
        {
            auto user_id = to_int(this->path.split('/')[-1]);
            if (user_id in this->data_store) {
                auto content_length = to_int(this->headers['Content-Length']);
                auto put_data = this->rfile.read(content_length);
                auto user_data = json.loads(put_data.decode());
                this->data_store[user_id].update({;
                    "name": user_data.get("name", this->data_store[user_id]["name"]),;
                    "age": user_data.get("age", this->data_store[user_id]["age"]);
                }
                });
                this->send_response(200);
                this->send_header('Content-type', 'application/json');
                this->end_headers();
                this->wfile.write(json.dumps(this->data_store[user_id]).encode());
            }
            else {
                this->send_error(404, "User not found");
            }
        }
        if (__fluxus_exc) { auto e = 0;
            this->send_error(400, "Invalid request");
        }
    }
    else {
        this->send_error(404, "Endpoint not found");
    }
}


        void do_DELETE() {
    bool __fluxus_exc=false;
    /* docstring */
    if (this->path.startswith('/api/users/')) {
        {
            auto user_id = to_int(this->path.split('/')[-1]);
            if (user_id in this->data_store) {
                auto deleted_user = this->data_store.pop(user_id);
                this->send_response(200);
                this->send_header('Content-type', 'application/json');
                this->end_headers();
                this->wfile.write(json.dumps({"message": "User deleted", "user": deleted_user}).encode());
            }
            else {
                this->send_error(404, "User not found");
            }
        }
        if (__fluxus_exc) { auto e = 0;
            this->send_error(400, "Invalid user ID");
        }
    }
    else {
        this->send_error(404, "Endpoint not found");
    }
}


    public:
        int data_store[self.next_id] = 0;
        std::string next_id = "";
};


void test_simple_http_server() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "=== Testing Simple HTTP Server ===" << std::endl;
    auto html_content = """;
    <!DOCTYPE html>;
    <html>;
    <head>;
        <title>Test Page</title>;
    }
    </head>;
    <body>;
        <h1>Hello from Python HTTP Server!</h1>;
        <p>This is a test page served by Python's built-in HTTP server.</p>;
    }
    </body>;
    </html>;
    /* docstring */
    {
        auto html_path = f.name;
        f.write(html_content);
    }
    auto serve_directory = os.path.dirname(html_path);
    auto filename = os.path.basename(html_path);
    {
        auto port = 8080;
        auto handler = http.server.SimpleHTTPRequestHandler;
        std::function<int()> run_server = [&]() {
            os.chdir(serve_directory);
            {
                std::cout << to_str("HTTP server running on port ") << to_str(port) << std::endl;
                httpd.serve_forever();
            }
        };
        auto server_thread = threading.Thread(target=run_server, daemon=true);
        server_thread.start();
        time.sleep(1);
        {
            auto url = to_str("http://localhost:") + to_str(port) + to_str("/") + to_str(filename);
            std::cout << to_str("Fetching: ") << to_str(url) << std::endl;
            {
                auto content = response.read().decode('utf-8');
                std::cout << to_str("Response status: ") << to_str(response.status) << std::endl;
                std::cout << to_str("Content length: ") << to_str(len(content)) << to_str(" bytes") << std::endl;
                std::cout << "First 200 characters of content:" << std::endl;
                std::cout << content[:200] << std::endl;
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("Failed to fetch from HTTP server: ") << to_str(e) << std::endl;
        }
    }
    {
        if (os.path.exists(html_path)) {
            os.unlink(html_path);
        }
    }
}


void test_rest_api_server() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing REST API Server ===" << std::endl;
    auto port = 8081;
    std::function<int()> run_rest_server = [&]() {
        {
            std::cout << to_str("REST API server running on port ") << to_str(port) << std::endl;
            httpd.serve_forever();
        }
    };
    auto server_thread = threading.Thread(target=run_rest_server, daemon=true);
    server_thread.start();
    time.sleep(1);
    auto base_url = to_str("http://localhost:") + to_str(port) + to_str("/api");
    {
        std::cout << "\n--- Testing GET /api/users ---" << std::endl;
        {
            {
                auto users = json.loads(response.read().decode());
                std::cout << to_str("Found ") << to_str(len(users)) << to_str(" users:") << std::endl;
                for (auto user : users) {
                    std::cout << to_str("  ID: ") << to_str(user['id']) << to_str(", Name: ") << to_str(user['name']) << to_str(", Age: ") << to_str(user['age']) << std::endl;
                }
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("GET request failed: ") << to_str(e) << std::endl;
        }
        std::cout << "\n--- Testing GET /api/users/1 ---" << std::endl;
        {
            {
                auto user = json.loads(response.read().decode());
                std::cout << to_str("User 1: ") << to_str(user) << std::endl;
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("GET specific user failed: ") << to_str(e) << std::endl;
        }
        std::cout << "\n--- Testing POST /api/users ---" << std::endl;
        auto new_user_data = json.dumps({"name": "Eve", "age": 28}).encode('utf-8');
        auto request = urllib.request.Request(;
            f"{base_url}/users",;
            auto data = new_user_data,;
            auto headers = {'Content-Type': 'application/json'},;
            auto method = 'POST';
        }
        );
        {
            {
                auto created_user = json.loads(response.read().decode());
                std::cout << to_str("Created new user: ") << to_str(created_user) << std::endl;
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("POST request failed: ") << to_str(e) << std::endl;
        }
        std::cout << "\n--- Testing PUT /api/users/1 ---" << std::endl;
        auto update_data = json.dumps({"name": "Alice Updated", "age": 31}).encode('utf-8');
        auto request = urllib.request.Request(;
            f"{base_url}/users/1",;
            auto data = update_data,;
            auto headers = {'Content-Type': 'application/json'},;
            auto method = 'PUT';
        }
        );
        {
            {
                auto updated_user = json.loads(response.read().decode());
                std::cout << to_str("Updated user: ") << to_str(updated_user) << std::endl;
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("PUT request failed: ") << to_str(e) << std::endl;
        }
        std::cout << "\n--- Testing DELETE /api/users/2 ---" << std::endl;
        auto request = urllib.request.Request(;
            f"{base_url}/users/2",;
            auto method = 'DELETE';
        }
        );
        {
            {
                auto delete_result = json.loads(response.read().decode());
                std::cout << to_str("Delete result: ") << to_str(delete_result) << std::endl;
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("DELETE request failed: ") << to_str(e) << std::endl;
        }
        std::cout << "\n--- Final user list ---" << std::endl;
        {
            {
                auto final_users = json.loads(response.read().decode());
                std::cout << to_str("Final users (") << to_str(len(final_users)) << to_str(" total):") << std::endl;
                for (auto user : final_users) {
                    std::cout << to_str("  ID: ") << to_str(user['id']) << to_str(", Name: ") << to_str(user['name']) << to_str(", Age: ") << to_str(user['age']) << std::endl;
                }
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("Final GET request failed: ") << to_str(e) << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("REST API test failed: ") << to_str(e) << std::endl;
    }
}


void test_urllib_operations() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing urllib Operations ===" << std::endl;
    auto test_urls = [;
        "https://httpbin.org/get",;
        "https://jsonplaceholder.typicode.com/posts/1",;
    }
    ];
    for (auto url : test_urls) {
        std::cout << to_str("\\n--- Testing GET ") << to_str(url) << to_str(" ---") << std::endl;
        {
            {
                auto content = response.read().decode('utf-8');
                auto headers = dict(response.headers);
                std::cout << to_str("Status: ") << to_str(response.status) << std::endl;
                std::cout << to_str("Content-Type: ") << to_str(headers.get('content-type', 'unknown')) << std::endl;
                std::cout << to_str("Content length: ") << to_str(len(content)) << to_str(" bytes") << std::endl;
                {
                    auto json_data = json.loads(content);
                    std::cout << "JSON response preview:" << std::endl;
                    std::cout << (len(content) > 500 ? json.dumps(json_data, indent=2)[:500] << "..." : json.dumps(json_data, indent=2)) << std::endl;
                }
                if (__fluxus_exc) { auto e = 0;
                    std::cout << "Non-JSON response:" << std::endl;
                    std::cout << (len(content) > 200 ? content[:200] << "..." : content) << std::endl;
                }
            }
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("HTTP Error: ") << to_str(e.code) << to_str(" - ") << to_str(e.reason) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("URL Error: ") << to_str(e.reason) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << to_str("Unexpected error: ") << to_str(e) << std::endl;
        }
    }
}


void test_url_manipulation() {
    /* docstring */
    std::cout << "\n=== Testing URL Manipulation ===" << std::endl;
    auto test_url = "https://example.com:8080/path/to/resource?param1=value1&param2=value2#fragment";
    auto parsed = urllib.parse.urlparse(test_url);
    std::cout << to_str("Original URL: ") << to_str(test_url) << std::endl;
    std::cout << to_str("Scheme: ") << to_str(parsed.scheme) << std::endl;
    std::cout << to_str("Netloc: ") << to_str(parsed.netloc) << std::endl;
    std::cout << to_str("Path: ") << to_str(parsed.path) << std::endl;
    std::cout << to_str("Params: ") << to_str(parsed.params) << std::endl;
    std::cout << to_str("Query: ") << to_str(parsed.query) << std::endl;
    std::cout << to_str("Fragment: ") << to_str(parsed.fragment) << std::endl;
    auto query_params = urllib.parse.parse_qs(parsed.query);
    std::cout << to_str("Parsed query parameters: ") << to_str(query_params) << std::endl;
    auto base_url = "https://api.example.com";
    auto path = "/users/123";
    auto params = std::unordered_map<std::string, std::string>{{"format", "json"}, {"fields", "name,email"}, {"limit", "10"}};
    auto constructed_url = urllib.parse.urljoin(base_url, path) + "?" + urllib.parse.urlencode(params);
    std::cout << to_str("\\nConstructed URL: ") << to_str(constructed_url) << std::endl;
}


int main() {
    /* docstring */
    测试Python的Web开发相关特性：HTTP服务器、REST API等;
    /* docstring */
    std::cout << "Python Web Development Testing" << std::endl;
    std::cout << std::string(50, '=') << std::endl;
    test_simple_http_server();
    test_rest_api_server();
    test_urllib_operations();
    test_url_manipulation();
    std::cout << "\n=== All web development tests completed ===" << std::endl;
    std::cout << "\nNote: HTTP servers were started in daemon threads." << std::endl;
    std::cout << "Real-world applications would use proper web frameworks like Flask, Django, or FastAPI." << std::endl;
    return 0;
}

