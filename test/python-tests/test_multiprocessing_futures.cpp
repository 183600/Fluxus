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


int cpu_intensive_task(int n) {
    /* docstring */
    auto result = 0;
    for (int i = 0; i < n * 1000000; ++i) {
        result += math.sqrt(i);
    }
    return result;
}


std::string io_simulation_task(std::string duration) {
    /* docstring */
    time.sleep(duration);
    return to_str("IO task completed after ") + to_str(duration) + to_str("s");
}


std::string test_multiprocessing_basics() {
    /* docstring */
    std::cout << "=== Testing Multiprocessing Basics ===" << std::endl;
    std::function<int()> worker_function = [&]() {
        std::cout << to_str("Worker ") << to_str(name) << to_str(" started (PID: ") << to_str(os.getpid()) << to_str(")") << std::endl;
        time.sleep(sleep_time);
        std::cout << to_str("Worker ") << to_str(name) << to_str(" finished") << std::endl;
        return to_str("Result from ") + to_str(name);
    };
    auto processes = std::vector<int>{};
    for (int i = 0; i < 3; ++i) {
        auto p = multiprocessing.Process(target=worker_function, args=(f"worker_{i}", 0.5));
        processes.push_back(p);
        p.start();
    }
    for (auto p : processes) {
        p.join();
    }
    std::cout << to_str("Main process PID: ") << to_str(os.getpid()) << std::endl;
}


void test_process_pool() {
    /* docstring */
    std::cout << "\n=== Testing Process Pool ===" << std::endl;
    auto numbers = std::vector<decltype(10)>{10, 20, 30, 40, 50};
    auto start_time = time.time();
    {
        auto results = pool.map(cpu_intensive_task, numbers);
        std::cout << to_str("Pool.map results: ") << to_str(results) << std::endl;
        auto async_results = std::vector<int>{};
        for (auto num : numbers) {
            auto async_result = pool.apply_async(cpu_intensive_task, (num,));
            async_results.push_back(async_result);
        }
        auto async_values = 0;
        std::cout << to_str("Pool.apply_async results: ") << to_str(async_values) << std::endl;
    }
    auto elapsed_time = time.time() - start_time;
    std::cout << to_str("Process pool completed in ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<elapsed_time; return os.str(); }()) << to_str(" seconds") << std::endl;
}


void test_concurrent_futures_process_pool() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing ProcessPoolExecutor ===" << std::endl;
    auto numbers = std::vector<decltype(5)>{5, 10, 15, 20, 25};
    auto start_time = time.time();
    {
        auto results = list(executor.map(cpu_intensive_task, numbers));
        std::cout << to_str("ProcessPoolExecutor.map results: ") << to_str(results) << std::endl;
        auto futures = 0;
        for (auto future : as_completed(futures)) {
            {
                auto result = future.result();
                std::cout << to_str("Task completed with result: ") << to_str(result) << std::endl;
            }
            if (__fluxus_exc) { auto e = 0;
                std::cout << to_str("Task generated an exception: ") << to_str(e) << std::endl;
            }
        }
    }
    auto elapsed_time = time.time() - start_time;
    std::cout << to_str("ProcessPoolExecutor completed in ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<elapsed_time; return os.str(); }()) << to_str(" seconds") << std::endl;
}


void test_concurrent_futures_thread_pool() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing ThreadPoolExecutor ===" << std::endl;
    auto durations = std::vector<decltype(0.1)>{0.1, 0.2, 0.3, 0.4, 0.5};
    auto start_time = time.time();
    {
        auto futures = 0;
        for (auto future : as_completed(futures)) {
            {
                auto result = future.result();
                std::cout << to_str("Thread task result: ") << to_str(result) << std::endl;
            }
            if (__fluxus_exc) { auto e = 0;
                std::cout << to_str("Thread task exception: ") << to_str(e) << std::endl;
            }
        }
    }
    auto elapsed_time = time.time() - start_time;
    std::cout << to_str("ThreadPoolExecutor completed in ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<elapsed_time; return os.str(); }()) << to_str(" seconds") << std::endl;
}


std::string mixed_task(std::string task_id, std::string cpu_work, std::string io_duration) {
    /* docstring */
    std::cout << to_str("Task ") << to_str(task_id) << to_str(": Starting CPU work (") << to_str(cpu_work) << to_str(")") << std::endl;
    auto cpu_result = cpu_intensive_task(cpu_work / 10);
    std::cout << to_str("Task ") << to_str(task_id) << to_str(": Starting IO wait (") << to_str(io_duration) << to_str("s)") << std::endl;
    time.sleep(io_duration);
    return to_str("Task ") + to_str(task_id) + to_str(": CPU=") + ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(0); os<<cpu_result; return os.str(); }()) + to_str(", IO=") + to_str(io_duration) + to_str("s");
}


void test_mixed_cpu_io_tasks() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Mixed CPU/IO Tasks ===" << std::endl;
    auto tasks = [;
        (1, 50, 0.1),;
        (2, 30, 0.2),;
        (3, 40, 0.15),;
        (4, 20, 0.3),;
        (5, 35, 0.25);
    }
    ];
    auto start_time = time.time();
    {
        auto futures = 0;
        for (auto future : as_completed(futures)) {
            {
                auto result = future.result();
                std::cout << to_str("Mixed task result: ") << to_str(result) << std::endl;
            }
            if (__fluxus_exc) { auto e = 0;
                std::cout << to_str("Mixed task exception: ") << to_str(e) << std::endl;
            }
        }
    }
    auto elapsed_time = time.time() - start_time;
    std::cout << to_str("Mixed tasks completed in ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<elapsed_time; return os.str(); }()) << to_str(" seconds") << std::endl;
}


void test_process_communication() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Process Communication ===" << std::endl;
    std::function<int()> worker_with_queue = [&]() {
        /* docstring */
        while (true) {
            {
                auto data = input_queue.get(timeout=1);
                if (data is 0) {
                    break;
                }
                auto result = data * 2;
                output_queue.put(f"Worker {worker_id}: {data} -> {result}");
                time.sleep(0.1);
            }
            if (__fluxus_exc) { auto e = 0;
                break;
            }
        }
    };
    auto input_queue = multiprocessing.Queue();
    auto output_queue = multiprocessing.Queue();
    auto processes = std::vector<int>{};
    for (int i = 0; i < 3; ++i) {
        auto p = multiprocessing.Process(;
            auto target = worker_with_queue,;
            auto args = (input_queue, output_queue, i);
        }
        );
        p.start();
        processes.push_back(p);
    }
    for (int num = 0; num < 10; ++num) {
        input_queue.put(num);
    }
    for (int _ = 0; _ < 3; ++_) {
        input_queue.put(0);
    }
    for (auto p : processes) {
        p.join();
    }
    auto results = std::vector<int>{};
    while (not output_queue.empty()) {
        results.push_back(output_queue.get());
    }
    std::cout << to_str("Process communication results: ") << to_str(results) << std::endl;
}


std::string long_running_task(int duration) {
    /* docstring */
    for (int i = 0; i < int(duration * 10); ++i) {
        time.sleep(0.1);
        std::cout << to_str("Task progress: ") << to_str(i+1) << to_str("/") << to_str(to_int(duration * 10)) << std::endl;
    }
    return "Task completed";
}


void test_timeout_and_cancellation() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Timeout and Cancellation ===" << std::endl;
    {
        auto future = executor.submit(long_running_task, 2);
        {
            auto result = future.result(timeout=1);
            std::cout << to_str("Task result: ") << to_str(result) << std::endl;
        }
        if (__fluxus_exc) { auto e = 0;
            std::cout << "Task timed out after 1 second" << std::endl;
            future.cancel();
            std::cout << "Task cancelled" << std::endl;
        }
    }
}


int main() {
    /* docstring */
    测试Python的multiprocessing和concurrent.futures高级特性;
    /* docstring */
    test_multiprocessing_basics();
    test_process_pool();
    test_concurrent_futures_process_pool();
    test_concurrent_futures_thread_pool();
    test_mixed_cpu_io_tasks();
    test_process_communication();
    test_timeout_and_cancellation();
    std::cout << to_str("\\n=== All multiprocessing tests completed (PID: ") << to_str(os.getpid()) << to_str(") ===") << std::endl;
    return 0;
}

