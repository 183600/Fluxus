#!/usr/bin/env python3
"""
测试Python的深拷贝vs浅拷贝行为
"""

import copy
import pickle
import shelve
import os
import tempfile

def test_shallow_vs_deep_copy():
    """测试浅拷贝和深拷贝的区别"""
    print("=== Testing Shallow vs Deep Copy ===")
    
    # 创建嵌套数据结构
    original = {
        'numbers': [1, 2, 3],
        'nested': {'inner': [4, 5, 6]},
        'simple': 'string'
    }
    
    # 浅拷贝
    shallow = copy.copy(original)
    
    # 深拷贝
    deep = copy.deepcopy(original)
    
    print("Original:", original)
    print("Shallow copy:", shallow)
    print("Deep copy:", deep)
    
    # 修改原始数据
    original['numbers'].append(999)
    original['nested']['inner'].append(888)
    original['simple'] = 'modified'
    
    print("\nAfter modifying original:")
    print("Original:", original)
    print("Shallow copy (affected by nested changes):", shallow)
    print("Deep copy (unaffected):", deep)

def test_copy_edge_cases():
    """测试拷贝的边缘情况"""
    print("\n=== Testing Copy Edge Cases ===")
    
    # 测试循环引用
    a = [1, 2, 3]
    b = [4, 5, 6]
    a.append(b)
    b.append(a)  # 创建循环引用
    
    print("Circular reference before copy:")
    print(f"a: {a}")
    print(f"b: {b}")
    
    # 深拷贝循环引用
    a_copy = copy.deepcopy(a)
    print("\nDeep copy of circular reference:")
    print(f"a_copy: {a_copy}")
    
    # 修改原始数据验证独立性
    a[0] = 999
    print(f"\nAfter modifying original a[0] = 999:")
    print(f"original a: {a}")
    print(f"copied a_copy: {a_copy}")

def test_pickle_serialization():
    """测试pickle序列化"""
    print("\n=== Testing Pickle Serialization ===")
    
    # 创建测试数据
    data = {
        'string': 'hello world',
        'list': [1, 2.5, True, None],
        'dict': {'nested': {'deep': 'value'}},
        'tuple': (1, 2, 3),
        'set': {4, 5, 6}
    }
    
    print("Original data:", data)
    
    # 序列化
    pickled = pickle.dumps(data)
    print(f"Pickled data size: {len(pickled)} bytes")
    
    # 反序列化
    unpickled = pickle.loads(pickled)
    print("Unpickled data:", unpickled)
    
    # 验证数据相等性
    print("Data equality:", data == unpickled)
    
    # 测试不同协议版本
    for protocol in range(pickle.HIGHEST_PROTOCOL + 1):
        try:
            p = pickle.dumps(data, protocol=protocol)
            u = pickle.loads(p)
            print(f"Protocol {protocol}: size = {len(p)} bytes, works = {data == u}")
        except Exception as e:
            print(f"Protocol {protocol}: failed - {e}")

def test_shelve_persistence():
    """测试shelve持久化存储"""
    print("\n=== Testing Shelve Persistence ===")
    
    # 创建临时数据库文件（使用简单的文件名，避免扩展名问题）
    temp_dir = tempfile.mkdtemp()
    db_path = os.path.join(temp_dir, 'test_shelve')
    
    try:
        # 写入数据到shelve数据库
        with shelve.open(db_path) as db:
            db['user_data'] = {'name': 'Alice', 'age': 30}
            db['settings'] = {'theme': 'dark', 'language': 'en'}
            db['numbers'] = [1, 2, 3, 4, 5]
            db['timestamp'] = time.time()
            print("Data written to shelve database")
        
        # 从shelve数据库读取数据
        with shelve.open(db_path) as db:
            print("Data read from shelve database:")
            for key, value in db.items():
                print(f"  {key}: {value}")
        
        # 修改数据
        with shelve.open(db_path) as db:
            user_data = db['user_data']
            user_data['age'] = 31  # 修改年龄
            db['user_data'] = user_data  # 写回数据库
            db['new_key'] = 'new_value'  # 添加新键
            print("\nData modified in database")
        
        # 验证修改
        with shelve.open(db_path) as db:
            print("Modified data:")
            print(f"  user_data: {db['user_data']}")
            print(f"  new_key: {db['new_key']}")
    
    finally:
        # 清理临时文件和目录
        import glob
        for file_path in glob.glob(f"{db_path}*"):
            try:
                os.unlink(file_path)
            except:
                pass
        try:
            os.rmdir(temp_dir)
        except:
            pass

import time

def test_copy_performance():
    """测试拷贝性能"""
    print("\n=== Testing Copy Performance ===")
    
    # 创建大数据结构
    large_data = {
        'items': [{'id': i, 'data': list(range(100))} for i in range(1000)]
    }
    
    # 测试浅拷贝性能
    start = time.time()
    shallow_copy = copy.copy(large_data)
    shallow_time = time.time() - start
    
    # 测试深拷贝性能
    start = time.time()
    deep_copy = copy.deepcopy(large_data)
    deep_time = time.time() - start
    
    print(f"Shallow copy time: {shallow_time:.4f} seconds")
    print(f"Deep copy time: {deep_time:.4f} seconds")
    print(f"Deep copy is {deep_time/shallow_time:.1f}x slower")
    
    # 验证拷贝的正确性
    print(f"Shallow copy first item data == original: {shallow_copy['items'][0]['data'] == large_data['items'][0]['data']}")
    print(f"Deep copy first item data == original: {deep_copy['items'][0]['data'] == large_data['items'][0]['data']}")
    
    # 修改验证独立性
    large_data['items'][0]['data'][0] = 99999
    print(f"After modifying original, shallow copy reflects change: {shallow_copy['items'][0]['data'][0] == 99999}")
    print(f"After modifying original, deep copy remains unchanged: {deep_copy['items'][0]['data'][0] == 0}")

if __name__ == "__main__":
    test_shallow_vs_deep_copy()
    test_copy_edge_cases()
    test_pickle_serialization()
    test_shelve_persistence()
    test_copy_performance()
    print("\n=== All copy and serialization tests completed ===")