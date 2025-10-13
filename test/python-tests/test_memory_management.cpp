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


class RegularClass {
    public:
        RegularClass(std::string name, int value) {
            this->name = name;
            this->value = value;
            this->data = list(range(100))  # 一些数据;
        }
        std::string __repr__() {
    return to_str("RegularClass(name='") + to_str(this->name) + to_str("', value=") + to_str(this->value) + to_str(")");
}


    public:
        std::string name = "";
        int value = 0;
        int data = 0;
};


class SlottedClass {
    public:
        SlottedClass(std::string name, int value) {
            this->name = name;
            this->value = value;
            this->data = list(range(100))  # 一些数据;
        }
        std::string __repr__() {
    return to_str("SlottedClass(name='") + to_str(this->name) + to_str("', value=") + to_str(this->value) + to_str(")");
}


    public:
        std::string name = "";
        int value = 0;
        int data = 0;
};


class SlottedWithInheritance : public SlottedClass {
    public:
        SlottedWithInheritance(std::string name, int value, int extra_field=None) : SlottedClass(name, value) {
            this->extra_field = extra_field;
        }
        void placeholder();
    public:
        int extra_field = 0;
};


void test_slots_memory_usage() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "=== Testing __slots__ Memory Usage ===" << std::endl;
    auto num_instances = 10000;
    std::cout << to_str("Creating ") << to_str(num_instances) << to_str(" instances of each class...") << std::endl;
    auto start_time = time.time();
    auto regular_instances = 0;
    auto regular_time = time.time() - start_time;
    auto start_time = time.time();
    auto slotted_instances = 0;
    auto slotted_time = time.time() - start_time;
    auto regular_size = sys::getsizeof(regular_instances[0]) * num_instances;
    auto slotted_size = sys::getsizeof(slotted_instances[0]) * num_instances;
    std::cout << to_str("Regular class:") << std::endl;
    std::cout << to_str("  Creation time: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(3); os<<regular_time; return os.str(); }()) << to_str("s") << std::endl;
    std::cout << to_str("  Approximate memory: ") << to_str(regular_size) << to_str(" bytes") << std::endl;
    std::cout << to_str("  Instance size: ") << to_str(sys::getsizeof(regular_instances[0])) << to_str(" bytes") << std::endl;
    std::cout << to_str("Slotted class:") << std::endl;
    std::cout << to_str("  Creation time: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(3); os<<slotted_time; return os.str(); }()) << to_str("s") << std::endl;
    std::cout << to_str("  Approximate memory: ") << to_str(slotted_size) << to_str(" bytes") << std::endl;
    std::cout << to_str("  Instance size: ") << to_str(sys::getsizeof(slotted_instances[0])) << to_str(" bytes") << std::endl;
    std::cout << to_str("Memory savings: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<((regular_size - slotted_size) / regular_size * 100); return os.str(); }()) << to_str("%") << std::endl;
    std::cout << "\n--- Testing Slot Restrictions ---" << std::endl;
    auto slotted_obj = SlottedClass("test", 42);
    std::cout << to_str("Allowed attributes: ") << to_str(slotted_obj.__slots__) << std::endl;
    std::cout << to_str("name: ") << to_str(slotted_obj.name) << std::endl;
    std::cout << to_str("value: ") << to_str(slotted_obj.value) << std::endl;
    {
        slotted_obj.new_attribute("should_fail");
        std::cout << "ERROR: Should not be able to add new attributes to slotted class" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Correctly prevented new attribute: ") << to_str(e) << std::endl;
    }
}


void test_slots_inheritance() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Slots Inheritance ===" << std::endl;
    auto inherited = SlottedWithInheritance("test", 100, "extra_value");
    std::cout << to_str("Inherited slots: ") << to_str(inherited.__slots__) << std::endl;
    std::cout << to_str("Name: ") << to_str(inherited.name) << to_str(" (from parent)") << std::endl;
    std::cout << to_str("Value: ") << to_str(inherited.value) << to_str(" (from parent)") << std::endl;
    std::cout << to_str("Extra field: ") << to_str(inherited.extra_field) << to_str(" (from child)") << std::endl;
    std::cout << to_str("Instance size: ") << to_str(sys::getsizeof(inherited)) << to_str(" bytes") << std::endl;
    {
        inherited.another_new_attr("should_fail");
        std::cout << "ERROR: Should not be able to add new attributes" << std::endl;
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Correctly prevented new attribute in inherited class: ") << to_str(e) << std::endl;
    }
}


void test_weak_references() {
    /* docstring */
    std::cout << "\n=== Testing Weak References ===" << std::endl;
    class DataObject { public: DataObject() = default; };
    auto obj1 = DataObject(100);
    auto obj2 = DataObject(200);
    auto weak_ref1 = weakref.ref(obj1);
    auto weak_ref2 = weakref.ref(obj2);
    std::cout << to_str("Original objects: ") << to_str(obj1) << to_str(", ") << to_str(obj2) << std::endl;
    std::cout << to_str("Weak references: ") << to_str(weak_ref1()) << to_str(", ") << to_str(weak_ref2()) << std::endl;
    std::cout << to_str("Weak reference is alive: ") << to_str(weak_ref1() is not 0) << std::endl;
    std::cout << to_str("Weak reference value: ") << to_str((weak_ref1() ? weak_ref1().value : 'Object deleted')) << std::endl;
    del obj1;
    gc.collect();
    std::cout << to_str("After deleting obj1:") << std::endl;
    std::cout << to_str("Weak reference 1 is alive: ") << to_str(weak_ref1() is not 0) << std::endl;
    std::cout << to_str("Weak reference 2 is alive: ") << to_str(weak_ref2() is not 0) << std::endl;
    std::cout << "\n--- Testing WeakValueDictionary ---" << std::endl;
    auto weak_dict = weakref.WeakValueDictionary();
    auto temp_obj1 = DataObject(300);
    auto temp_obj2 = DataObject(400);
    auto weak_dict['obj1'] = temp_obj1;
    auto weak_dict['obj2'] = temp_obj2;
    std::cout << to_str("Weak dict before deletion: ") << to_str(dict(weak_dict)) << std::endl;
    del temp_obj1;
    gc.collect();
    std::cout << to_str("Weak dict after deleting obj1: ") << to_str(dict(weak_dict)) << std::endl;
    std::cout << "\n--- Testing WeakKeyDictionary ---" << std::endl;
    auto weak_key_dict = weakref.WeakKeyDictionary();
    auto key1 = DataObject("key1");
    auto key2 = DataObject("key2");
    auto weak_key_dict[key1] = "value1";
    auto weak_key_dict[key2] = "value2";
    std::cout << to_str("Weak key dict before deletion: ") << to_str(dict(weak_key_dict)) << std::endl;
    del key1;
    gc.collect();
    std::cout << to_str("Weak key dict after deleting key1: ") << to_str(dict(weak_key_dict)) << std::endl;
}


void test_garbage_collection() {
    /* docstring */
    std::cout << "\n=== Testing Garbage Collection ===" << std::endl;
    std::cout << to_str("GC is enabled: ") << to_str(gc.isenabled()) << std::endl;
    std::cout << to_str("GC thresholds: ") << to_str(gc.get_threshold()) << std::endl;
    std::cout << to_str("Current GC count: ") << to_str(gc.get_count()) << std::endl;
    class Node { public: Node() = default; };
    auto node1 = Node(1);
    auto node2 = Node(2);
    auto node3 = Node(3);
    node1.next(node2);
    node2.next(node3);
    node3.next(node1);
    std::cout << to_str("\\nCreated circular reference: ") << to_str(node1) << to_str(" -> ") << to_str(node2) << to_str(" -> ") << to_str(node3) << to_str(" -> ") << to_str(node1) << std::endl;
    gc.disable();
    std::cout << to_str("GC disabled: ") << to_str(not gc.isenabled()) << std::endl;
    del node1;
    del node2;
    del node3;
    std::cout << "Deleted all references to nodes with circular reference" << std::endl;
    auto collected = gc.collect();
    std::cout << to_str("Manually collected ") << to_str(collected) << to_str(" objects") << std::endl;
    gc.enable();
    std::cout << to_str("GC re-enabled: ") << to_str(gc.isenabled()) << std::endl;
}


void test_memory_profiling() {
    /* docstring */
    std::cout << "\n=== Testing Memory Profiling ===" << std::endl;
    tracemalloc.start();
    auto snapshot1 = tracemalloc.take_snapshot();
    auto large_list = 0;
    auto snapshot2 = tracemalloc.take_snapshot();
    auto top_stats = snapshot2.compare_to(snapshot1, 'lineno');
    std::cout << "Top memory allocations:" << std::endl;
    for (auto stat : top_stats[:5]) {
        std::cout << to_str("  ") << to_str(stat) << std::endl;
    }
    auto current, peak = tracemalloc.get_traced_memory();
    std::cout << to_str("\\nCurrent memory usage: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<current / 1024 / 1024; return os.str(); }()) << to_str(" MB") << std::endl;
    std::cout << to_str("Peak memory usage: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<peak / 1024 / 1024; return os.str(); }()) << to_str(" MB") << std::endl;
    tracemalloc.stop();
}


void test_object_lifecycle() {
    /* docstring */
    std::cout << "\n=== Testing Object Lifecycle ===" << std::endl;
    class ManagedObject { public: ManagedObject() = default; };
    auto obj1 = ManagedObject("first");
    auto obj2 = ManagedObject("second");
    auto obj3 = ManagedObject("third");
    std::cout << to_str("Active instances after creation: ") << to_str(len(ManagedObject.get_active_instances())) << std::endl;
    del obj1;
    gc.collect();
    std::cout << to_str("Active instances after deletion: ") << to_str(len(ManagedObject.get_active_instances())) << std::endl;
    auto weak_set = weakref.WeakSet();
    weak_set.add(obj2);
    weak_set.add(obj3);
    std::cout << to_str("Objects in weak set: ") << to_str(len(weak_set)) << std::endl;
    del obj2;
    gc.collect();
    std::cout << to_str("Objects in weak set after deletion: ") << to_str(len(weak_set)) << std::endl;
}


std::string test_memory_optimization_patterns() {
    /* docstring */
    std::cout << "\n=== Testing Memory Optimization Patterns ===" << std::endl;
    class RegularPoint { public: RegularPoint() = default; };
    class SlottedPoint { public: SlottedPoint() = default; };
    auto num_points = 100000;
    auto regular_points = 0;
    auto slotted_points = 0;
    auto regular_size = sum(std::vector<int>{});
    auto slotted_size = sum(std::vector<int>{});
    std::cout << to_str("Average size per RegularPoint: ") << to_str(regular_size) << to_str(" bytes") << std::endl;
    std::cout << to_str("Average size per SlottedPoint: ") << to_str(slotted_size) << to_str(" bytes") << std::endl;
    std::cout << to_str("Memory savings with slots: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<((regular_size - slotted_size) / regular_size * 100); return os.str(); }()) << to_str("%") << std::endl;
    class ObjectPool { public: ObjectPool() = default; };
    std::function<int()> create_expensive_object = [&]() {
        return std::unordered_map<std::string, int>{{"data", list(range(1000))}, {"timestamp", time.time()}};
    };
    auto pool = ObjectPool(create_expensive_object, max_size=50);
    auto obj1 = pool.get();
    auto obj2 = pool.get();
    std::cout << to_str("\\nObject pool - Active: ") << to_str(pool.active_count) << to_str(", Pool size: ") << to_str(pool.pool_size) << std::endl;
    pool.put(obj1);
    pool.put(obj2);
    std::cout << to_str("After returning objects - Active: ") << to_str(pool.active_count) << to_str(", Pool size: ") << to_str(pool.pool_size) << std::endl;
}


int main() {
    /* docstring */
    std::cout << "Python Memory Management: __slots__, weakref, gc Module Demonstration" << std::endl;
    std::cout << std::string(80, '=') << std::endl;
    test_slots_memory_usage();
    test_slots_inheritance();
    test_weak_references();
    test_garbage_collection();
    test_memory_profiling();
    test_object_lifecycle();
    test_memory_optimization_patterns();
    std::cout << "\n=== Summary ===" << std::endl;
    std::cout << "Key concepts demonstrated:" << std::endl;
    std::cout << 0 << std::endl;
    std::cout << 0 << std::endl;
    std::cout << 0 << std::endl;
    std::cout << "4. Memory profiling with tracemalloc" << std::endl;
    std::cout << "5. Object lifecycle management with weak references" << std::endl;
    std::cout << "6. Memory optimization patterns including object pooling" << std::endl;
    std::cout << "\nBest practices:" << std::endl;
    std::cout << "- Use __slots__ when creating many instances with fixed attributes" << std::endl;
    std::cout << 0 << std::endl;
    std::cout << "- Monitor memory usage in production applications" << std::endl;
    std::cout << "- Implement proper cleanup in __del__ methods" << std::endl;
    return 0;
}


