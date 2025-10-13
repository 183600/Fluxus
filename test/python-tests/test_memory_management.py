#!/usr/bin/env python3
"""
测试Python的内存管理特性：__slots__、weakref、gc模块
"""

import sys
import gc
import weakref
from collections import defaultdict
import time
import tracemalloc

class RegularClass:
    """普通类 - 用于对比内存使用"""
    
    def __init__(self, name, value):
        self.name = name
        self.value = value
        self.data = list(range(100))  # 一些数据
    
    def __repr__(self):
        return f"RegularClass(name='{self.name}', value={self.value})"

class SlottedClass:
    """使用__slots__的类 - 内存优化"""
    
    __slots__ = ['name', 'value', 'data']  # 只允许这些属性
    
    def __init__(self, name, value):
        self.name = name
        self.value = value
        self.data = list(range(100))  # 一些数据
    
    def __repr__(self):
        return f"SlottedClass(name='{self.name}', value={self.value})"

class SlottedWithInheritance(SlottedClass):
    """继承自带slots的类"""
    
    __slots__ = ['extra_field']  # 添加新的slot
    
    def __init__(self, name, value, extra_field=None):
        super().__init__(name, value)
        self.extra_field = extra_field

def test_slots_memory_usage():
    """测试__slots__的内存使用优势"""
    print("=== Testing __slots__ Memory Usage ===")
    
    # 创建大量实例进行对比
    num_instances = 10000
    
    print(f"Creating {num_instances} instances of each class...")
    
    # 测试普通类
    start_time = time.time()
    regular_instances = [RegularClass(f"obj_{i}", i) for i in range(num_instances)]
    regular_time = time.time() - start_time
    
    # 测试带slots的类
    start_time = time.time()
    slotted_instances = [SlottedClass(f"obj_{i}", i) for i in range(num_instances)]
    slotted_time = time.time() - start_time
    
    # 计算内存使用（近似）
    regular_size = sys.getsizeof(regular_instances[0]) * num_instances
    slotted_size = sys.getsizeof(slotted_instances[0]) * num_instances
    
    print(f"Regular class:")
    print(f"  Creation time: {regular_time:.3f}s")
    print(f"  Approximate memory: {regular_size:,} bytes")
    print(f"  Instance size: {sys.getsizeof(regular_instances[0])} bytes")
    
    print(f"Slotted class:")
    print(f"  Creation time: {slotted_time:.3f}s")
    print(f"  Approximate memory: {slotted_size:,} bytes")
    print(f"  Instance size: {sys.getsizeof(slotted_instances[0])} bytes")
    
    print(f"Memory savings: {((regular_size - slotted_size) / regular_size * 100):.1f}%")
    
    # 测试属性访问限制
    print("\n--- Testing Slot Restrictions ---")
    
    slotted_obj = SlottedClass("test", 42)
    print(f"Allowed attributes: {slotted_obj.__slots__}")
    
    # 正常访问
    print(f"name: {slotted_obj.name}")
    print(f"value: {slotted_obj.value}")
    
    # 尝试添加未在slots中定义的属性
    try:
        slotted_obj.new_attribute = "should_fail"
        print("ERROR: Should not be able to add new attributes to slotted class")
    except AttributeError as e:
        print(f"✓ Correctly prevented new attribute: {e}")

def test_slots_inheritance():
    """测试带slots类的继承"""
    print("\n=== Testing Slots Inheritance ===")
    
    # 创建继承实例
    inherited = SlottedWithInheritance("test", 100, "extra_value")
    
    print(f"Inherited slots: {inherited.__slots__}")
    print(f"Name: {inherited.name} (from parent)")
    print(f"Value: {inherited.value} (from parent)")
    print(f"Extra field: {inherited.extra_field} (from child)")
    
    # 测试内存使用
    print(f"Instance size: {sys.getsizeof(inherited)} bytes")
    
    # 测试新属性限制
    try:
        inherited.another_new_attr = "should_fail"
        print("ERROR: Should not be able to add new attributes")
    except AttributeError as e:
        print(f"✓ Correctly prevented new attribute in inherited class: {e}")

def test_weak_references():
    """测试弱引用"""
    print("\n=== Testing Weak References ===")
    
    class DataObject:
        """测试对象"""
        def __init__(self, value):
            self.value = value
        
        def __repr__(self):
            return f"DataObject({self.value})"
    
    # 创建对象和弱引用
    obj1 = DataObject(100)
    obj2 = DataObject(200)
    
    # 创建弱引用
    weak_ref1 = weakref.ref(obj1)
    weak_ref2 = weakref.ref(obj2)
    
    print(f"Original objects: {obj1}, {obj2}")
    print(f"Weak references: {weak_ref1()}, {weak_ref2()}")
    
    # 测试弱引用在对象存在时的行为
    print(f"Weak reference is alive: {weak_ref1() is not None}")
    print(f"Weak reference value: {weak_ref1().value if weak_ref1() else 'Object deleted'}")
    
    # 删除强引用
    del obj1
    
    # 强制垃圾回收（可选）
    gc.collect()
    
    print(f"After deleting obj1:")
    print(f"Weak reference 1 is alive: {weak_ref1() is not None}")
    print(f"Weak reference 2 is alive: {weak_ref2() is not None}")
    
    # 测试WeakValueDictionary
    print("\n--- Testing WeakValueDictionary ---")
    
    weak_dict = weakref.WeakValueDictionary()
    
    # 创建临时对象
    temp_obj1 = DataObject(300)
    temp_obj2 = DataObject(400)
    
    # 存储到弱引用字典
    weak_dict['obj1'] = temp_obj1
    weak_dict['obj2'] = temp_obj2
    
    print(f"Weak dict before deletion: {dict(weak_dict)}")
    
    # 删除临时对象
    del temp_obj1
    gc.collect()
    
    print(f"Weak dict after deleting obj1: {dict(weak_dict)}")
    
    # 测试WeakKeyDictionary
    print("\n--- Testing WeakKeyDictionary ---")
    
    weak_key_dict = weakref.WeakKeyDictionary()
    
    key1 = DataObject("key1")
    key2 = DataObject("key2")
    
    weak_key_dict[key1] = "value1"
    weak_key_dict[key2] = "value2"
    
    print(f"Weak key dict before deletion: {dict(weak_key_dict)}")
    
    del key1
    gc.collect()
    
    print(f"Weak key dict after deleting key1: {dict(weak_key_dict)}")

def test_garbage_collection():
    """测试垃圾回收"""
    print("\n=== Testing Garbage Collection ===")
    
    # 获取当前GC状态
    print(f"GC is enabled: {gc.isenabled()}")
    print(f"GC thresholds: {gc.get_threshold()}")
    print(f"Current GC count: {gc.get_count()}")
    
    # 创建一些循环引用
    class Node:
        def __init__(self, value):
            self.value = value
            self.next = None
        
        def __repr__(self):
            return f"Node({self.value})"
    
    # 创建循环引用
    node1 = Node(1)
    node2 = Node(2)
    node3 = Node(3)
    
    node1.next = node2
    node2.next = node3
    node3.next = node1  # 循环引用
    
    print(f"\nCreated circular reference: {node1} -> {node2} -> {node3} -> {node1}")
    
    # 手动禁用GC来观察引用计数
    gc.disable()
    print(f"GC disabled: {not gc.isenabled()}")
    
    # 删除引用
    del node1
    del node2
    del node3
    
    print("Deleted all references to nodes with circular reference")
    
    # 强制垃圾回收
    collected = gc.collect()
    print(f"Manually collected {collected} objects")
    
    # 重新启用GC
    gc.enable()
    print(f"GC re-enabled: {gc.isenabled()}")

def test_memory_profiling():
    """测试内存分析"""
    print("\n=== Testing Memory Profiling ===")
    
    # 启动内存跟踪
    tracemalloc.start()
    
    # 记录当前内存使用
    snapshot1 = tracemalloc.take_snapshot()
    
    # 创建一些对象
    large_list = [i for i in range(100000)]
    
    # 记录内存使用变化
    snapshot2 = tracemalloc.take_snapshot()
    
    # 计算内存差异
    top_stats = snapshot2.compare_to(snapshot1, 'lineno')
    
    print("Top memory allocations:")
    for stat in top_stats[:5]:
        print(f"  {stat}")
    
    # 获取当前内存使用统计
    current, peak = tracemalloc.get_traced_memory()
    print(f"\nCurrent memory usage: {current / 1024 / 1024:.2f} MB")
    print(f"Peak memory usage: {peak / 1024 / 1024:.2f} MB")
    
    # 停止内存跟踪
    tracemalloc.stop()

def test_object_lifecycle():
    """测试对象生命周期管理"""
    print("\n=== Testing Object Lifecycle ===")
    
    class ManagedObject:
        """带生命周期管理的对象"""
        
        _instances = weakref.WeakSet()
        
        def __init__(self, name):
            self.name = name
            self._instances.add(self)
            print(f"Created: {self}")
        
        def __del__(self):
            print(f"Destroyed: {self}")
        
        def __repr__(self):
            return f"ManagedObject({self.name})"
        
        @classmethod
        def get_active_instances(cls):
            """获取当前活动的实例"""
            return list(cls._instances)
        
        @classmethod
        def cleanup_expired_instances(cls):
            """清理已失效的实例"""
            active = cls.get_active_instances()
            print(f"Active instances: {len(active)}")
            return active
    
    # 创建一些对象
    obj1 = ManagedObject("first")
    obj2 = ManagedObject("second")
    obj3 = ManagedObject("third")
    
    print(f"Active instances after creation: {len(ManagedObject.get_active_instances())}")
    
    # 删除一些对象
    del obj1
    gc.collect()  # 强制垃圾回收
    
    print(f"Active instances after deletion: {len(ManagedObject.get_active_instances())}")
    
    # 使用弱引用跟踪
    weak_set = weakref.WeakSet()
    weak_set.add(obj2)
    weak_set.add(obj3)
    
    print(f"Objects in weak set: {len(weak_set)}")
    
    del obj2
    gc.collect()
    
    print(f"Objects in weak set after deletion: {len(weak_set)}")

def test_memory_optimization_patterns():
    """测试内存优化模式"""
    print("\n=== Testing Memory Optimization Patterns ===")
    
    # 模式1: 使用slots减少内存占用
    class RegularPoint:
        def __init__(self, x, y):
            self.x = x
            self.y = y
    
    class SlottedPoint:
        __slots__ = ['x', 'y']
        
        def __init__(self, x, y):
            self.x = x
            self.y = y
    
    # 创建大量实例进行对比
    num_points = 100000
    
    regular_points = [RegularPoint(i, i) for i in range(num_points)]
    slotted_points = [SlottedPoint(i, i) for i in range(num_points)]
    
    regular_size = sum(sys.getsizeof(p) for p in regular_points[:100]) / 100
    slotted_size = sum(sys.getsizeof(p) for p in slotted_points[:100]) / 100
    
    print(f"Average size per RegularPoint: {regular_size} bytes")
    print(f"Average size per SlottedPoint: {slotted_size} bytes")
    print(f"Memory savings with slots: {((regular_size - slotted_size) / regular_size * 100):.1f}%")
    
    # 模式2: 对象池模式
    class ObjectPool:
        """简单的对象池实现"""
        
        def __init__(self, factory_func, max_size=100):
            self.factory_func = factory_func
            self.max_size = max_size
            self._pool = []
            # 用于跟踪活动对象的 id（支持不可弱引用且不可哈希的对象，如 dict）
            self._active_ids = set()
        
        def get(self):
            """从池中获取对象"""
            if self._pool:
                obj = self._pool.pop()
            else:
                obj = self.factory_func()
            
            self._active_ids.add(id(obj))
            return obj
        
        def put(self, obj):
            """将对象返回池中"""
            if len(self._pool) < self.max_size:
                self._pool.append(obj)
            self._active_ids.discard(id(obj))
        
        @property
        def pool_size(self):
            return len(self._pool)
        
        @property
        def active_count(self):
            return len(self._active_ids)
    
    # 使用对象池
    def create_expensive_object():
        return {"data": list(range(1000)), "timestamp": time.time()}
    
    pool = ObjectPool(create_expensive_object, max_size=50)
    
    # 获取和释放对象
    obj1 = pool.get()
    obj2 = pool.get()
    
    print(f"\nObject pool - Active: {pool.active_count}, Pool size: {pool.pool_size}")
    
    pool.put(obj1)
    pool.put(obj2)
    
    print(f"After returning objects - Active: {pool.active_count}, Pool size: {pool.pool_size}")

def main():
    """主测试函数"""
    print("Python Memory Management: __slots__, weakref, gc Module Demonstration")
    print("=" * 80)
    
    test_slots_memory_usage()
    test_slots_inheritance()
    test_weak_references()
    test_garbage_collection()
    test_memory_profiling()
    test_object_lifecycle()
    test_memory_optimization_patterns()
    
    print("\n=== Summary ===")
    print("Key concepts demonstrated:")
    print("1. __slots__ for memory optimization and attribute restriction")
    print("2. weakref for creating weak references that don't prevent garbage collection")
    print("3. gc module for manual garbage collection control")
    print("4. Memory profiling with tracemalloc")
    print("5. Object lifecycle management with weak references")
    print("6. Memory optimization patterns including object pooling")
    print("\nBest practices:")
    print("- Use __slots__ when creating many instances with fixed attributes")
    print("- Use weakref for caches and observer patterns to avoid circular references")
    print("- Monitor memory usage in production applications")
    print("- Implement proper cleanup in __del__ methods")

if __name__ == "__main__":
    main()