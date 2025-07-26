#!/usr/bin/env python3
"""
Advanced Python Systems Programming Demo
Covering concurrency, networking, data processing, and system-level operations
"""

import asyncio
import concurrent.futures
import hashlib
import http.server
import json
import multiprocessing
import os
import random
import re
import socketserver
import sqlite3
import statistics
import string
import threading
import time
import urllib.parse
import xml.etree.ElementTree as ET
from collections import Counter, defaultdict, namedtuple
from contextlib import contextmanager
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from functools import lru_cache, wraps
from pathlib import Path
from typing import Any, Callable, Dict, Generator, List, Optional, Set, Tuple, Union
import tempfile
import uuid


@dataclass
class User:
    id: int
    name: str
    email: str
    age: int
    balance: float = 0.0
    created_at: datetime = field(default_factory=datetime.now)
    tags: List[str] = field(default_factory=list)


@dataclass
class Transaction:
    id: str
    user_id: int
    amount: float
    transaction_type: str
    timestamp: datetime = field(default_factory=datetime.now)


class BankingSystem:
    def __init__(self):
        self.users: Dict[int, User] = {}
        self.transactions: List[Transaction] = []
        self.lock = threading.RLock()
        self._next_user_id = 1
    
    def create_user(self, name: str, email: str, age: int, initial_balance: float = 0.0) -> User:
        with self.lock:
            user = User(
                id=self._next_user_id,
                name=name,
                email=email,
                age=age,
                balance=initial_balance
            )
            self.users[user.id] = user
            self._next_user_id += 1
            return user
    
    def deposit(self, user_id: int, amount: float) -> bool:
        with self.lock:
            if user_id not in self.users or amount <= 0:
                return False
            
            self.users[user_id].balance += amount
            transaction = Transaction(
                id=str(uuid.uuid4()),
                user_id=user_id,
                amount=amount,
                transaction_type="deposit"
            )
            self.transactions.append(transaction)
            return True
    
    def withdraw(self, user_id: int, amount: float) -> bool:
        with self.lock:
            if user_id not in self.users or amount <= 0:
                return False
            
            if self.users[user_id].balance >= amount:
                self.users[user_id].balance -= amount
                transaction = Transaction(
                    id=str(uuid.uuid4()),
                    user_id=user_id,
                    amount=-amount,
                    transaction_type="withdrawal"
                )
                self.transactions.append(transaction)
                return True
            return False
    
    def transfer(self, from_user_id: int, to_user_id: int, amount: float) -> bool:
        with self.lock:
            if (from_user_id not in self.users or 
                to_user_id not in self.users or 
                amount <= 0 or
                from_user_id == to_user_id):
                return False
            
            if self.users[from_user_id].balance >= amount:
                self.users[from_user_id].balance -= amount
                self.users[to_user_id].balance += amount
                
                # Record transactions
                withdraw_transaction = Transaction(
                    id=str(uuid.uuid4()),
                    user_id=from_user_id,
                    amount=-amount,
                    transaction_type="transfer_out"
                )
                
                deposit_transaction = Transaction(
                    id=str(uuid.uuid4()),
                    user_id=to_user_id,
                    amount=amount,
                    transaction_type="transfer_in"
                )
                
                self.transactions.extend([withdraw_transaction, deposit_transaction])
                return True
            return False
    
    def get_user_balance(self, user_id: int) -> Optional[float]:
        with self.lock:
            return self.users[user_id].balance if user_id in self.users else None
    
    def get_transaction_history(self, user_id: int) -> List[Transaction]:
        with self.lock:
            return [t for t in self.transactions if t.user_id == user_id]


class ThreadSafeCounter:
    def __init__(self):
        self._value = 0
        self._lock = threading.Lock()
    
    def increment(self):
        with self._lock:
            self._value += 1
    
    def decrement(self):
        with self._lock:
            self._value -= 1
    
    @property
    def value(self):
        with self._lock:
            return self._value


class RateLimiter:
    def __init__(self, max_requests: int, time_window: int):
        self.max_requests = max_requests
        self.time_window = time_window
        self.requests: Dict[str, List[float]] = defaultdict(list)
        self.lock = threading.RLock()
    
    def is_allowed(self, identifier: str) -> bool:
        with self.lock:
            now = time.time()
            cutoff = now - self.time_window
            
            # Remove old requests
            self.requests[identifier] = [
                req_time for req_time in self.requests[identifier] 
                if req_time > cutoff
            ]
            
            # Check if under limit
            if len(self.requests[identifier]) < self.max_requests:
                self.requests[identifier].append(now)
                return True
            
            return False


class DatabaseManager:
    def __init__(self, db_path: str = ":memory:"):
        self.db_path = db_path
        self.lock = threading.RLock()
        self._init_database()
    
    def _init_database(self):
        with self.get_connection() as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS users (
                    id INTEGER PRIMARY KEY,
                    name TEXT NOT NULL,
                    email TEXT UNIQUE NOT NULL,
                    age INTEGER,
                    balance REAL DEFAULT 0.0,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            conn.execute("""
                CREATE TABLE IF NOT EXISTS transactions (
                    id TEXT PRIMARY KEY,
                    user_id INTEGER,
                    amount REAL,
                    transaction_type TEXT,
                    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (user_id) REFERENCES users (id)
                )
            """)
    
    @contextmanager
    def get_connection(self):
        with self.lock:
            conn = sqlite3.connect(self.db_path)
            conn.row_factory = sqlite3.Row
            try:
                yield conn
            finally:
                conn.close()
    
    def insert_user(self, user: User) -> bool:
        try:
            with self.get_connection() as conn:
                conn.execute(
                    "INSERT INTO users (name, email, age, balance) VALUES (?, ?, ?, ?)",
                    (user.name, user.email, user.age, user.balance)
                )
                conn.commit()
                return True
        except sqlite3.Error:
            return False
    
    def get_user_by_email(self, email: str) -> Optional[Dict]:
        with self.get_connection() as conn:
            cursor = conn.execute("SELECT * FROM users WHERE email = ?", (email,))
            row = cursor.fetchone()
            return dict(row) if row else None


def timing_decorator(func: Callable) -> Callable:
    """Decorator to measure function execution time"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        start_time = time.time()
        result = func(*args, **kwargs)
        end_time = time.time()
        print(f"{func.__name__} executed in {end_time - start_time:.4f} seconds")
        return result
    return wrapper


def retry_decorator(max_attempts: int = 3, delay: float = 1.0):
    """Decorator to retry function execution on failure"""
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise e
                    time.sleep(delay * (2 ** attempt))  # Exponential backoff
            return None
        return wrapper
    return decorator


@lru_cache(maxsize=128)
def expensive_fibonacci(n: int) -> int:
    """Cached fibonacci calculation"""
    if n <= 1:
        return n
    return expensive_fibonacci(n - 1) + expensive_fibonacci(n - 2)


def cpu_bound_task(n: int) -> int:
    """CPU-intensive task for multiprocessing demo"""
    total = 0
    for i in range(n):
        for j in range(1000):
            total += i * j
    return total


def process_chunk(chunk: List[int]) -> int:
    """Process a chunk of data"""
    return sum(x * x for x in chunk)


class DataProcessor:
    @staticmethod
    def process_with_multiprocessing(data: List[int], num_processes: int = None) -> int:
        """Process data using multiprocessing"""
        if num_processes is None:
            num_processes = multiprocessing.cpu_count()
        
        # Split data into chunks
        chunk_size = len(data) // num_processes
        chunks = [data[i:i + chunk_size] for i in range(0, len(data), chunk_size)]
        
        with multiprocessing.Pool(num_processes) as pool:
            results = pool.map(process_chunk, chunks)
        
        return sum(results)
    
    @staticmethod
    def process_with_threading(data: List[int], num_threads: int = 4) -> int:
        """Process data using threading"""
        chunk_size = len(data) // num_threads
        chunks = [data[i:i + chunk_size] for i in range(0, len(data), chunk_size)]
        
        results = []
        
        def worker(chunk: List[int]):
            results.append(process_chunk(chunk))
        
        threads = []
        for chunk in chunks:
            thread = threading.Thread(target=worker, args=(chunk,))
            thread.start()
            threads.append(thread)
        
        for thread in threads:
            thread.join()
        
        return sum(results)


async def async_http_request(url: str, session_id: str) -> Dict[str, Any]:
    """Simulate async HTTP request"""
    await asyncio.sleep(random.uniform(0.1, 0.5))  # Simulate network delay
    
    return {
        "url": url,
        "session_id": session_id,
        "status": 200,
        "response_time": random.uniform(0.1, 0.5),
        "timestamp": datetime.now().isoformat()
    }


async def async_data_processing():
    """Demonstrate async programming with concurrent requests"""
    urls = [f"https://api.example.com/data/{i}" for i in range(10)]
    session_id = str(uuid.uuid4())
    
    # Process requests concurrently
    tasks = [async_http_request(url, session_id) for url in urls]
    results = await asyncio.gather(*tasks)
    
    return results


class TextAnalyzer:
    def __init__(self):
        self.word_patterns = {
            'email': re.compile(r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b'),
            'phone': re.compile(r'\b\d{3}-\d{3}-\d{4}\b'),
            'url': re.compile(r'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+'),
            'hashtag': re.compile(r'#\w+'),
            'mention': re.compile(r'@\w+')
        }
    
    def analyze_text(self, text: str) -> Dict[str, Any]:
        """Comprehensive text analysis"""
        words = text.lower().split()
        sentences = text.split('.')
        
        # Basic statistics
        analysis = {
            'word_count': len(words),
            'sentence_count': len([s for s in sentences if s.strip()]),
            'character_count': len(text),
            'character_count_no_spaces': len(text.replace(' ', '')),
            'average_word_length': statistics.mean(len(word.strip('.,!?;:"()[]{}')) for word in words) if words else 0,
            'unique_words': len(set(words)),
            'word_frequency': Counter(words),
            'most_common_words': Counter(words).most_common(10)
        }
        
        # Pattern matching
        for pattern_name, pattern in self.word_patterns.items():
            matches = pattern.findall(text)
            analysis[f'{pattern_name}_count'] = len(matches)
            analysis[f'{pattern_name}_matches'] = matches
        
        # Readability metrics (simplified)
        if analysis['sentence_count'] > 0:
            analysis['average_words_per_sentence'] = analysis['word_count'] / analysis['sentence_count']
        else:
            analysis['average_words_per_sentence'] = 0
        
        return analysis


class CacheSystem:
    def __init__(self, max_size: int = 1000, ttl: int = 3600):
        self.max_size = max_size
        self.ttl = ttl
        self.cache: Dict[str, Tuple[Any, float]] = {}
        self.access_times: Dict[str, float] = {}
        self.lock = threading.RLock()
    
    def get(self, key: str) -> Optional[Any]:
        with self.lock:
            if key in self.cache:
                value, timestamp = self.cache[key]
                if time.time() - timestamp < self.ttl:
                    self.access_times[key] = time.time()
                    return value
                else:
                    # Expired
                    del self.cache[key]
                    del self.access_times[key]
            return None
    
    def set(self, key: str, value: Any) -> None:
        with self.lock:
            now = time.time()
            
            # Evict if at capacity
            if len(self.cache) >= self.max_size and key not in self.cache:
                # Remove least recently used
                lru_key = min(self.access_times.keys(), key=self.access_times.get)
                del self.cache[lru_key]
                del self.access_times[lru_key]
            
            self.cache[key] = (value, now)
            self.access_times[key] = now
    
    def clear(self) -> None:
        with self.lock:
            self.cache.clear()
            self.access_times.clear()


def generate_sample_data(size: int) -> List[Dict[str, Any]]:
    """Generate sample data for testing"""
    data = []
    for i in range(size):
        record = {
            'id': i,
            'name': f"User_{i}",
            'email': f"user_{i}@example.com",
            'age': random.randint(18, 80),
            'score': random.uniform(0, 100),
            'category': random.choice(['A', 'B', 'C', 'D']),
            'timestamp': datetime.now() - timedelta(days=random.randint(0, 365)),
            'active': random.choice([True, False])
        }
        data.append(record)
    return data


def data_analysis_pipeline(data: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Comprehensive data analysis pipeline"""
    if not data:
        return {}
    
    # Basic statistics
    ages = [record['age'] for record in data]
    scores = [record['score'] for record in data]
    
    analysis = {
        'total_records': len(data),
        'age_statistics': {
            'mean': statistics.mean(ages),
            'median': statistics.median(ages),
            'mode': statistics.mode(ages) if ages else None,
            'min': min(ages),
            'max': max(ages),
            'std_dev': statistics.stdev(ages) if len(ages) > 1 else 0
        },
        'score_statistics': {
            'mean': statistics.mean(scores),
            'median': statistics.median(scores),
            'min': min(scores),
            'max': max(scores),
            'std_dev': statistics.stdev(scores) if len(scores) > 1 else 0
        },
        'category_distribution': Counter(record['category'] for record in data),
        'active_users': sum(1 for record in data if record['active']),
        'inactive_users': sum(1 for record in data if not record['active']),
        'recent_users': sum(1 for record in data 
                          if (datetime.now() - record['timestamp']).days <= 30)
    }
    
    # Advanced analytics
    analysis['high_score_users'] = sum(1 for record in data if record['score'] > 80)
    analysis['score_percentiles'] = {
        '25th': statistics.quantiles(scores, n=4)[0] if len(scores) >= 4 else None,
        '50th': statistics.median(scores),
        '75th': statistics.quantiles(scores, n=4)[2] if len(scores) >= 4 else None
    }
    
    return analysis


class SimpleHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    """Custom HTTP request handler"""
    
    def do_GET(self):
        if self.path == '/api/status':
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            
            response = {
                'status': 'healthy',
                'timestamp': datetime.now().isoformat(),
                'version': '1.0.0'
            }
            self.wfile.write(json.dumps(response).encode())
            
        elif self.path == '/api/data':
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            
            sample_data = generate_sample_data(10)
            # Convert datetime objects to strings for JSON serialization
            for record in sample_data:
                record['timestamp'] = record['timestamp'].isoformat()
            
            response = {
                'data': sample_data,
                'count': len(sample_data)
            }
            self.wfile.write(json.dumps(response).encode())
            
        else:
            super().do_GET()


def start_http_server(port: int = 8080):
    """Start a simple HTTP server"""
    handler = SimpleHTTPRequestHandler
    
    with socketserver.TCPServer(("", port), handler) as httpd:
        print(f"HTTP server starting on port {port}")
        httpd.serve_forever()


@timing_decorator
def performance_benchmark():
    """Benchmark different approaches"""
    data = list(range(100000))
    
    print("Performance Benchmark:")
    
    # Sequential processing
    start_time = time.time()
    sequential_result = sum(x * x for x in data)
    sequential_time = time.time() - start_time
    print(f"Sequential: {sequential_result} in {sequential_time:.4f}s")
    
    # Multiprocessing
    start_time = time.time()
    mp_result = DataProcessor.process_with_multiprocessing(data)
    mp_time = time.time() - start_time
    print(f"Multiprocessing: {mp_result} in {mp_time:.4f}s")
    
    # Threading
    start_time = time.time()
    thread_result = DataProcessor.process_with_threading(data)
    thread_time = time.time() - start_time
    print(f"Threading: {thread_result} in {thread_time:.4f}s")
    
    return {
        'sequential': {'result': sequential_result, 'time': sequential_time},
        'multiprocessing': {'result': mp_result, 'time': mp_time},
        'threading': {'result': thread_result, 'time': thread_time}
    }


def main():
    print("=== Advanced Python Systems Programming Demo ===")
    
    # 1. Banking System Demo
    print("\n1. Banking System Operations:")
    banking = BankingSystem()
    
    # Create users
    alice = banking.create_user("Alice Johnson", "alice@example.com", 30, 1000.0)
    bob = banking.create_user("Bob Smith", "bob@example.com", 25, 500.0)
    
    print(f"Created users: Alice (ID: {alice.id}), Bob (ID: {bob.id})")
    
    # Perform transactions
    banking.deposit(alice.id, 200.0)
    banking.withdraw(bob.id, 100.0)
    banking.transfer(alice.id, bob.id, 150.0)
    
    print(f"Alice balance: ${banking.get_user_balance(alice.id):.2f}")
    print(f"Bob balance: ${banking.get_user_balance(bob.id):.2f}")
    
    # 2. Thread-Safe Counter Demo
    print("\n2. Thread-Safe Counter:")
    counter = ThreadSafeCounter()
    
    def increment_worker():
        for _ in range(1000):
            counter.increment()
    
    def decrement_worker():
        for _ in range(500):
            counter.decrement()
    
    threads = []
    for _ in range(5):
        t1 = threading.Thread(target=increment_worker)
        t2 = threading.Thread(target=decrement_worker)
        threads.extend([t1, t2])
        t1.start()
        t2.start()
    
    for thread in threads:
        thread.join()
    
    print(f"Final counter value: {counter.value}")
    
    # 3. Rate Limiter Demo
    print("\n3. Rate Limiter (5 requests per 10 seconds):")
    rate_limiter = RateLimiter(max_requests=5, time_window=10)
    
    for i in range(8):
        allowed = rate_limiter.is_allowed("user_123")
        print(f"Request {i+1}: {'Allowed' if allowed else 'Rate limited'}")
        time.sleep(0.1)
    
    # 4. Cache System Demo
    print("\n4. Cache System:")
    cache = CacheSystem(max_size=3, ttl=5)
    
    # Add items to cache
    cache.set("key1", "value1")
    cache.set("key2", "value2")
    cache.set("key3", "value3")
    
    print(f"Get key1: {cache.get('key1')}")
    print(f"Get key2: {cache.get('key2')}")
    
    # Add another item (should evict least recently used)
    cache.set("key4", "value4")
    print(f"Get key3 after adding key4: {cache.get('key3')}")
    
    # 5. Text Analysis Demo
    print("\n5. Text Analysis:")
    analyzer = TextAnalyzer()
    
    sample_text = """
    Hello @username! Check out this article: https://example.com/article
    You can reach me at john.doe@email.com or call 555-123-4567.
    Don't forget to use #hashtag for better visibility!
    This is a comprehensive text analysis demo.
    """
    
    analysis = analyzer.analyze_text(sample_text)
    print(f"Word count: {analysis['word_count']}")
    print(f"Unique words: {analysis['unique_words']}")
    print(f"Email addresses found: {analysis['email_count']}")
    print(f"URLs found: {analysis['url_count']}")
    print(f"Most common words: {analysis['most_common_words'][:5]}")
    
    # 6. Data Analysis Pipeline
    print("\n6. Data Analysis Pipeline:")
    sample_data = generate_sample_data(1000)
    analysis_results = data_analysis_pipeline(sample_data)
    
    print(f"Total records: {analysis_results['total_records']}")
    print(f"Average age: {analysis_results['age_statistics']['mean']:.2f}")
    print(f"Average score: {analysis_results['score_statistics']['mean']:.2f}")
    print(f"Active users: {analysis_results['active_users']}")
    print(f"Category distribution: {dict(analysis_results['category_distribution'])}")
    
    # 7. Async Programming Demo
    print("\n7. Async Programming Demo:")
    
    async def run_async_demo():
        start_time = time.time()
        results = await async_data_processing()
        end_time = time.time()
        
        print(f"Processed {len(results)} async requests in {end_time - start_time:.2f} seconds")
        print(f"Average response time: {statistics.mean(r['response_time'] for r in results):.3f}s")
        return results
    
    # Run async demo
    async_results = asyncio.run(run_async_demo())
    
    # 8. Performance Benchmark
    print("\n8. Performance Benchmark:")
    benchmark_results = performance_benchmark()
    
    # 9. Database Operations
    print("\n9. Database Operations:")
    db = DatabaseManager()
    
    # Insert users
    test_user = User(id=1, name="Test User", email="test@example.com", age=25, balance=100.0)
    success = db.insert_user(test_user)
    print(f"User insertion {'successful' if success else 'failed'}")
    
    # Retrieve user
    retrieved_user = db.get_user_by_email("test@example.com")
    if retrieved_user:
        print(f"Retrieved user: {retrieved_user['name']} (Balance: ${retrieved_user['balance']})")
    
    # 10. Fibonacci with Caching
    print("\n10. Cached Fibonacci:")
    
    @timing_decorator
    def test_fibonacci(n):
        return expensive_fibonacci(n)
    
    print("First call (uncached):")
    result1 = test_fibonacci(30)
    
    print("Second call (cached):")
    result2 = test_fibonacci(30)
    
    print(f"Fibonacci(30) = {result1}")
    
    # 11. Error Handling with Retry Decorator
    print("\n11. Retry Decorator Demo:")
    
    @retry_decorator(max_attempts=3, delay=0.1)
    def unreliable_function():
        if random.random() < 0.7:  # 70% chance of failure
            raise Exception("Random failure")
        return "Success!"
    
    try:
        result = unreliable_function()
        print(f"Function result: {result}")
    except Exception as e:
        print(f"Function failed after retries: {e}")
    
    # 12. File System Operations
    print("\n12. File System Operations:")
    
    # Create temporary directory and files
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        
        # Create sample files
        for i in range(3):
            file_path = temp_path / f"sample_{i}.txt"
            file_path.write_text(f"This is sample file {i}\nWith multiple lines\nAnd some content")
        
        # Analyze files
        txt_files = list(temp_path.glob("*.txt"))
        print(f"Created {len(txt_files)} temporary files")
        
        total_size = sum(f.stat().st_size for f in txt_files)
        print(f"Total size: {total_size} bytes")
        
        # Process file contents
        all_content = ""
        for file_path in txt_files:
            all_content += file_path.read_text() + "\n"
        
        file_analysis = analyzer.analyze_text(all_content)
        print(f"Total words in all files: {file_analysis['word_count']}")
    
    # 13. HTTP Server (start in background)
    print("\n13. HTTP Server:")
    server_thread = threading.Thread(target=start_http_server, args=(8081,), daemon=True)
    server_thread.start()
    time.sleep(1)  # Give server time to start
    print("HTTP server started on port 8081 (running in background)")
    print("Available endpoints: /api/status, /api/data")
    
    print("\n=== Advanced Systems Demo Complete ===")
    print(f"Total execution time: {time.time() - time.time():.2f} seconds")


if __name__ == "__main__":
    main()