#!/usr/bin/env python3
"""
测试Python的SQLite集成
"""

import sqlite3
import tempfile
import os
import datetime
from contextlib import contextmanager

@contextmanager
def get_db_connection(db_path):
    """数据库连接上下文管理器"""
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row  # 使查询结果可以像字典一样访问
    try:
        yield conn
    finally:
        conn.close()

def create_sample_database(db_path):
    """创建示例数据库"""
    print("=== Creating Sample Database ===")
    
    with get_db_connection(db_path) as conn:
        cursor = conn.cursor()
        
        # 创建用户表
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS users (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                username TEXT UNIQUE NOT NULL,
                email TEXT UNIQUE NOT NULL,
                age INTEGER,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        # 创建订单表
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS orders (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                user_id INTEGER,
                product_name TEXT NOT NULL,
                quantity INTEGER NOT NULL,
                price REAL NOT NULL,
                order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                status TEXT DEFAULT 'pending',
                FOREIGN KEY (user_id) REFERENCES users (id)
            )
        ''')
        
        # 创建索引
        cursor.execute('CREATE INDEX IF NOT EXISTS idx_users_email ON users (email)')
        cursor.execute('CREATE INDEX IF NOT EXISTS idx_orders_user_id ON orders (user_id)')
        cursor.execute('CREATE INDEX IF NOT EXISTS idx_orders_status ON orders (status)')
        
        conn.commit()
        print("✓ Database tables created successfully")

def test_basic_crud_operations(db_path):
    """测试基本的CRUD操作"""
    print("\n=== Testing Basic CRUD Operations ===")
    
    with get_db_connection(db_path) as conn:
        cursor = conn.cursor()
        
        # CREATE - 插入用户数据
        users_data = [
            ('alice', 'alice@example.com', 25),
            ('bob', 'bob@example.com', 30),
            ('charlie', 'charlie@example.com', 35),
            ('diana', 'diana@example.com', 28)
        ]
        
        cursor.executemany(
            'INSERT INTO users (username, email, age) VALUES (?, ?, ?)',
            users_data
        )
        
        # 插入订单数据
        orders_data = [
            (1, 'Laptop', 1, 999.99),
            (1, 'Mouse', 2, 29.99),
            (2, 'Keyboard', 1, 79.99),
            (3, 'Monitor', 1, 299.99),
            (4, 'Headphones', 1, 149.99),
            (2, 'USB Cable', 3, 19.99)
        ]
        
        cursor.executemany(
            'INSERT INTO orders (user_id, product_name, quantity, price) VALUES (?, ?, ?, ?)',
            orders_data
        )
        
        conn.commit()
        print(f"✓ Inserted {len(users_data)} users and {len(orders_data)} orders")
        
        # READ - 查询数据
        print("\n--- Reading Data ---")
        
        # 查询所有用户
        cursor.execute('SELECT * FROM users ORDER BY id')
        users = cursor.fetchall()
        print("All users:")
        for user in users:
            print(f"  ID: {user['id']}, Username: {user['username']}, Email: {user['email']}, Age: {user['age']}")
        
        # 查询特定用户
        cursor.execute('SELECT * FROM users WHERE username = ?', ('alice',))
        alice = cursor.fetchone()
        print(f"\nAlice's details: {dict(alice)}")
        
        # 带条件的查询
        cursor.execute('SELECT * FROM users WHERE age > ? ORDER BY age', (28,))
        older_users = cursor.fetchall()
        print(f"\nUsers older than 28:")
        for user in older_users:
            print(f"  {user['username']}: {user['age']} years old")
        
        # UPDATE - 更新数据
        print("\n--- Updating Data ---")
        cursor.execute('UPDATE users SET age = ? WHERE username = ?', (26, 'alice'))
        cursor.execute('UPDATE orders SET status = ? WHERE user_id = ?', ('completed', 1))
        conn.commit()
        
        cursor.execute('SELECT age FROM users WHERE username = ?', ('alice',))
        alice_age = cursor.fetchone()['age']
        print(f"✓ Alice's age updated to: {alice_age}")
        
        # DELETE - 删除数据
        print("\n--- Deleting Data ---")
        cursor.execute('DELETE FROM orders WHERE id = ?', (6,))  # 删除USB Cable订单
        conn.commit()
        
        cursor.execute('SELECT COUNT(*) as count FROM orders')
        order_count = cursor.fetchone()['count']
        print(f"✓ Remaining orders: {order_count}")

def test_advanced_queries(db_path):
    """测试高级查询"""
    print("\n=== Testing Advanced Queries ===")
    
    with get_db_connection(db_path) as conn:
        cursor = conn.cursor()
        
        # JOIN查询 - 获取用户及其订单
        cursor.execute('''
            SELECT u.username, u.email, o.product_name, o.quantity, o.price, o.status
            FROM users u
            JOIN orders o ON u.id = o.user_id
            ORDER BY u.username, o.order_date
        ''')
        
        user_orders = cursor.fetchall()
        print("User orders (JOIN):")
        current_user = None
        for row in user_orders:
            if row['username'] != current_user:
                current_user = row['username']
                print(f"\n  {current_user} ({row['email']}):")
            print(f"    - {row['product_name']}: {row['quantity']} x ${row['price']:.2f} [{row['status']}]")
        
        # 聚合函数 - 统计信息
        print("\n--- Aggregation Functions ---")
        
        cursor.execute('SELECT COUNT(*) as total_users FROM users')
        total_users = cursor.fetchone()['total_users']
        print(f"Total users: {total_users}")
        
        cursor.execute('SELECT AVG(age) as avg_age FROM users')
        avg_age = cursor.fetchone()['avg_age']
        print(f"Average user age: {avg_age:.1f}")
        
        cursor.execute('SELECT SUM(price * quantity) as total_revenue FROM orders')
        total_revenue = cursor.fetchone()['total_revenue']
        print(f"Total revenue: ${total_revenue:.2f}")
        
        cursor.execute('SELECT MAX(price) as max_price FROM orders')
        max_price = cursor.fetchone()['max_price']
        print(f"Most expensive order: ${max_price:.2f}")
        
        # GROUP BY - 按用户统计订单
        cursor.execute('''
            SELECT u.username, COUNT(o.id) as order_count, SUM(o.price * o.quantity) as total_spent
            FROM users u
            LEFT JOIN orders o ON u.id = o.user_id
            GROUP BY u.id, u.username
            ORDER BY total_spent DESC
        ''')
        
        user_stats = cursor.fetchall()
        print("\nUser spending statistics:")
        for stat in user_stats:
            print(f"  {stat['username']}: {stat['order_count']} orders, ${stat['total_spent'] or 0:.2f} total")

def test_transactions(db_path):
    """测试事务处理"""
    print("\n=== Testing Transactions ===")
    
    # 正常事务
    print("--- Successful Transaction ---")
    try:
        with get_db_connection(db_path) as conn:
            cursor = conn.cursor()
            
            # 开始事务
            cursor.execute('BEGIN TRANSACTION')
            
            # 执行多个操作
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)', 
                          ('eve', 'eve@example.com', 32))
            
            cursor.execute('INSERT INTO orders (user_id, product_name, quantity, price) VALUES (?, ?, ?, ?)',
                          (5, 'Tablet', 1, 399.99))
            
            # 提交事务
            conn.commit()
            print("✓ Transaction committed successfully")
            
    except Exception as e:
        print(f"Transaction failed: {e}")
    
    # 回滚事务
    print("\n--- Transaction Rollback ---")
    try:
        with get_db_connection(db_path) as conn:
            cursor = conn.cursor()
            
            # 开始事务
            cursor.execute('BEGIN TRANSACTION')
            
            # 执行一些操作
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',
                          ('frank', 'frank@example.com', 40))
            
            # 故意触发错误（重复的用户名）
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',
                          ('frank', 'duplicate@example.com', 25))
            
            # 这行不会执行，因为上一步会抛出异常
            conn.commit()
            
    except sqlite3.IntegrityError as e:
        print(f"✓ Expected integrity error: {e}")
        print("✓ Transaction was automatically rolled back")

def test_prepared_statements(db_path):
    """测试预编译语句"""
    print("\n=== Testing Prepared Statements ===")
    
    with get_db_connection(db_path) as conn:
        cursor = conn.cursor()
        
        # 预编译插入语句
        insert_user = 'INSERT INTO users (username, email, age) VALUES (?, ?, ?)'
        
        # 多次执行预编译语句
        new_users = [
            ('grace', 'grace@example.com', 27),
            ('henry', 'henry@example.com', 33),
            ('iris', 'iris@example.com', 29)
        ]
        
        cursor.executemany(insert_user, new_users)
        conn.commit()
        print(f"✓ Inserted {len(new_users)} users using prepared statement")
        
        # 预编译查询语句
        query_user = 'SELECT * FROM users WHERE age > ? AND age < ?'
        cursor.execute(query_user, (25, 35))
        
        users_in_range = cursor.fetchall()
        print(f"Users aged 25-35: {len(users_in_range)}")
        for user in users_in_range:
            print(f"  {user['username']}: {user['age']} years old")

def test_database_metadata(db_path):
    """测试数据库元数据"""
    print("\n=== Testing Database Metadata ===")
    
    with get_db_connection(db_path) as conn:
        cursor = conn.cursor()
        
        # 获取表信息
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
        tables = cursor.fetchall()
        print("Database tables:")
        for table in tables:
            print(f"  - {table['name']}")
        
        # 获取表结构
        for table_name in ['users', 'orders']:
            cursor.execute(f"PRAGMA table_info({table_name})")
            columns = cursor.fetchall()
            print(f"\n{table_name} table structure:")
            for col in columns:
                print(f"  {col['name']}: {col['type']} (NULL: {col['notnull'] == 0})")
        
        # 获取数据库版本
        cursor.execute('SELECT sqlite_version()')
        sqlite_version = cursor.fetchone()[0]
        print(f"\nSQLite version: {sqlite_version}")

def test_error_handling(db_path):
    """测试错误处理"""
    print("\n=== Testing Error Handling ===")
    
    # 测试重复键错误
    print("--- Testing Duplicate Key Error ---")
    try:
        with get_db_connection(db_path) as conn:
            cursor = conn.cursor()
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',
                          ('alice', 'duplicate@example.com', 25))
            conn.commit()
    except sqlite3.IntegrityError as e:
        print(f"✓ Caught expected integrity error: {e}")
    
    # 测试外键约束（如果启用）
    print("\n--- Testing Foreign Key Constraint ---")
    try:
        with get_db_connection(db_path) as conn:
            cursor = conn.cursor()
            # 尝试插入不存在的用户的订单
            cursor.execute('INSERT INTO orders (user_id, product_name, quantity, price) VALUES (?, ?, ?, ?)',
                          (999, 'Invalid Product', 1, 99.99))
            conn.commit()
            print("Note: Foreign key constraints may not be enforced by default in SQLite")
    except sqlite3.IntegrityError as e:
        print(f"✓ Caught foreign key constraint error: {e}")
    
    # 测试SQL注入防护
    print("\n--- Testing SQL Injection Protection ---")
    malicious_input = "'; DROP TABLE users; --"
    try:
        with get_db_connection(db_path) as conn:
            cursor = conn.cursor()
            # 使用参数化查询防止SQL注入
            cursor.execute('SELECT * FROM users WHERE username = ?', (malicious_input,))
            results = cursor.fetchall()
            print(f"✓ Parameterized query safely handled malicious input: {len(results)} results")
    except Exception as e:
        print(f"Error handling malicious input: {e}")

def main():
    """主测试函数"""
    print("Python SQLite Integration Demonstration")
    print("=" * 50)
    
    # 创建临时数据库文件
    with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as tmp:
        db_path = tmp.name
    
    try:
        # 运行所有测试
        create_sample_database(db_path)
        test_basic_crud_operations(db_path)
        test_advanced_queries(db_path)
        test_transactions(db_path)
        test_prepared_statements(db_path)
        test_database_metadata(db_path)
        test_error_handling(db_path)
        
        print(f"\n=== All SQLite tests completed successfully ===")
        print(f"Database file: {db_path}")
        print("Key concepts covered:")
        print("- Database creation and table schema")
        print("- CRUD operations (Create, Read, Update, Delete)")
        print("- SQL JOIN queries")
        print("- Aggregation functions (COUNT, SUM, AVG, MAX)")
        print("- GROUP BY and data grouping")
        print("- Transaction management (commit/rollback)")
        print("- Prepared statements for security")
        print("- Database metadata inspection")
        print("- Error handling for database operations")
        
    finally:
        # 清理临时数据库文件
        if os.path.exists(db_path):
            os.unlink(db_path)
            print(f"\nCleaned up temporary database: {db_path}")

if __name__ == "__main__":
    main()