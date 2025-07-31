#!/usr/bin/env python3

import asyncio
import json
import os
import sqlite3
import threading
import time
from collections import defaultdict
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Any
import socket
import hashlib

class DatabaseManager:
    def __init__(self, db_path: str = "system_demo.db"):
        self.db_path = db_path
        self.init_database()
    
    def init_database(self):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Create users table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS users (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                username TEXT UNIQUE NOT NULL,
                email TEXT UNIQUE NOT NULL,
                password_hash TEXT NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        # Create logs table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS logs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                level TEXT NOT NULL,
                message TEXT NOT NULL,
                timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                source TEXT
            )
        ''')
        
        conn.commit()
        conn.close()
    
    def add_user(self, username: str, email: str, password: str) -> bool:
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            password_hash = hashlib.sha256(password.encode()).hexdigest()
            cursor.execute(
                "INSERT INTO users (username, email, password_hash) VALUES (?, ?, ?)",
                (username, email, password_hash)
            )
            
            conn.commit()
            conn.close()
            return True
        except sqlite3.IntegrityError:
            return False
    
    def get_user(self, username: str) -> Optional[Dict[str, Any]]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute("SELECT * FROM users WHERE username = ?", (username,))
        row = cursor.fetchone()
        conn.close()
        
        if row:
            return {
                'id': row[0],
                'username': row[1],
                'email': row[2],
                'password_hash': row[3],
                'created_at': row[4]
            }
        return None
    
    def log_message(self, level: str, message: str, source: str = "system"):
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute(
            "INSERT INTO logs (level, message, source) VALUES (?, ?, ?)",
            (level, message, source)
        )
        
        conn.commit()
        conn.close()
    
    def get_logs(self, limit: int = 100) -> List[Dict[str, Any]]:
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute(
            "SELECT * FROM logs ORDER BY timestamp DESC LIMIT ?",
            (limit,)
        )
        
        rows = cursor.fetchall()
        conn.close()
        
        return [{
            'id': row[0],
            'level': row[1],
            'message': row[2],
            'timestamp': row[3],
            'source': row[4]
        } for row in rows]

class FileSystemManager:
    def __init__(self, base_path: str = "./demo_files"):
        self.base_path = Path(base_path)
        self.base_path.mkdir(exist_ok=True)
    
    def create_file(self, filename: str, content: str) -> bool:
        try:
            file_path = self.base_path / filename
            file_path.write_text(content)
            return True
        except Exception as e:
            print(f"Error creating file: {e}")
            return False
    
    def read_file(self, filename: str) -> Optional[str]:
        try:
            file_path = self.base_path / filename
            return file_path.read_text()
        except Exception as e:
            print(f"Error reading file: {e}")
            return None
    
    def list_files(self) -> List[str]:
        try:
            return [f.name for f in self.base_path.iterdir() if f.is_file()]
        except Exception as e:
            print(f"Error listing files: {e}")
            return []
    
    def delete_file(self, filename: str) -> bool:
        try:
            file_path = self.base_path / filename
            file_path.unlink()
            return True
        except Exception as e:
            print(f"Error deleting file: {e}")
            return False
    
    def get_file_stats(self, filename: str) -> Optional[Dict[str, Any]]:
        try:
            file_path = self.base_path / filename
            stats = file_path.stat()
            return {
                'size': stats.st_size,
                'created': datetime.fromtimestamp(stats.st_ctime),
                'modified': datetime.fromtimestamp(stats.st_mtime),
                'is_file': file_path.is_file()
            }
        except Exception as e:
            print(f"Error getting file stats: {e}")
            return None

class ConfigurationManager:
    def __init__(self, config_file: str = "config.json"):
        self.config_file = config_file
        self.config = self.load_config()
    
    def load_config(self) -> Dict[str, Any]:
        try:
            with open(self.config_file, 'r') as f:
                return json.load(f)
        except FileNotFoundError:
            return {
                "server_port": 8080,
                "max_connections": 100,
                "debug_mode": False,
                "database_path": "system_demo.db",
                "log_level": "INFO"
            }
    
    def save_config(self) -> bool:
        try:
            with open(self.config_file, 'w') as f:
                json.dump(self.config, f, indent=2)
            return True
        except Exception as e:
            print(f"Error saving config: {e}")
            return False
    
    def get(self, key: str, default: Any = None) -> Any:
        return self.config.get(key, default)
    
    def set(self, key: str, value: Any) -> None:
        self.config[key] = value
        self.save_config()

class CacheManager:
    def __init__(self, max_size: int = 1000, ttl_seconds: int = 3600):
        self.cache = {}
        self.access_times = {}
        self.max_size = max_size
        self.ttl_seconds = ttl_seconds
        self._lock = threading.Lock()
    
    def get(self, key: str) -> Optional[Any]:
        with self._lock:
            if key not in self.cache:
                return None
            
            # Check if expired
            if time.time() - self.access_times[key] > self.ttl_seconds:
                del self.cache[key]
                del self.access_times[key]
                return None
            
            self.access_times[key] = time.time()
            return self.cache[key]
    
    def set(self, key: str, value: Any) -> None:
        with self._lock:
            # If cache is full, remove oldest item
            if len(self.cache) >= self.max_size and key not in self.cache:
                oldest_key = min(self.access_times.keys(), key=lambda k: self.access_times[k])
                del self.cache[oldest_key]
                del self.access_times[oldest_key]
            
            self.cache[key] = value
            self.access_times[key] = time.time()
    
    def clear(self) -> None:
        with self._lock:
            self.cache.clear()
            self.access_times.clear()
    
    def stats(self) -> Dict[str, int]:
        with self._lock:
            return {
                'size': len(self.cache),
                'max_size': self.max_size,
                'hit_rate': 0  # Would need to track hits/misses for real implementation
            }

class TaskScheduler:
    def __init__(self):
        self.tasks = []
        self.running = False
        self._lock = threading.Lock()
    
    def add_task(self, func, interval_seconds: int, *args, **kwargs):
        with self._lock:
            task = {
                'func': func,
                'interval': interval_seconds,
                'args': args,
                'kwargs': kwargs,
                'next_run': time.time() + interval_seconds
            }
            self.tasks.append(task)
    
    def start(self):
        if self.running:
            return
        
        self.running = True
        
        def scheduler_loop():
            while self.running:
                current_time = time.time()
                
                with self._lock:
                    for task in self.tasks:
                        if current_time >= task['next_run']:
                            try:
                                task['func'](*task['args'], **task['kwargs'])
                            except Exception as e:
                                print(f"Task execution error: {e}")
                            
                            task['next_run'] = current_time + task['interval']
                
                time.sleep(1)
        
        thread = threading.Thread(target=scheduler_loop, daemon=True)
        thread.start()
    
    def stop(self):
        self.running = False

class NetworkServer:
    def __init__(self, host: str = "localhost", port: int = 8080):
        self.host = host
        self.port = port
        self.clients = []
        self._lock = threading.Lock()
    
    def handle_client(self, client_socket, address):
        print(f"Client connected from {address}")
        
        try:
            while True:
                data = client_socket.recv(1024).decode('utf-8')
                if not data:
                    break
                
                # Echo the message back
                response = f"Echo: {data}"
                client_socket.send(response.encode('utf-8'))
        
        except Exception as e:
            print(f"Error handling client {address}: {e}")
        
        finally:
            with self._lock:
                if client_socket in self.clients:
                    self.clients.remove(client_socket)
            client_socket.close()
            print(f"Client {address} disconnected")
    
    def start_server(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        
        try:
            server_socket.bind((self.host, self.port))
            server_socket.listen(5)
            print(f"Server listening on {self.host}:{self.port}")
            
            while True:
                client_socket, address = server_socket.accept()
                
                with self._lock:
                    self.clients.append(client_socket)
                
                client_thread = threading.Thread(
                    target=self.handle_client,
                    args=(client_socket, address),
                    daemon=True
                )
                client_thread.start()
        
        except Exception as e:
            print(f"Server error: {e}")
        
        finally:
            server_socket.close()

class PerformanceMonitor:
    def __init__(self):
        self.metrics = defaultdict(list)
        self._lock = threading.Lock()
    
    def record_metric(self, name: str, value: float):
        with self._lock:
            self.metrics[name].append({
                'value': value,
                'timestamp': time.time()
            })
            
            # Keep only last 1000 entries
            if len(self.metrics[name]) > 1000:
                self.metrics[name] = self.metrics[name][-1000:]
    
    def get_stats(self, name: str) -> Dict[str, float]:
        with self._lock:
            if name not in self.metrics:
                return {}
            
            values = [m['value'] for m in self.metrics[name]]
            return {
                'count': len(values),
                'min': min(values),
                'max': max(values),
                'avg': sum(values) / len(values),
                'latest': values[-1] if values else 0
            }
    
    def get_all_stats(self) -> Dict[str, Dict[str, float]]:
        return {name: self.get_stats(name) for name in self.metrics.keys()}

def demo_all_systems():
    print("Python Comprehensive Systems Demo")
    print("=================================")
    
    # Database operations
    print("\n--- Database Operations ---")
    db = DatabaseManager("demo.db")
    
    # Add some users
    success = db.add_user("alice", "alice@example.com", "password123")
    print(f"Added user alice: {success}")
    
    success = db.add_user("bob", "bob@example.com", "password456")
    print(f"Added user bob: {success}")
    
    # Retrieve user
    user = db.get_user("alice")
    print(f"Retrieved user: {user['username'] if user else 'Not found'}")
    
    # Log some messages
    db.log_message("INFO", "System started")
    db.log_message("WARNING", "High memory usage")
    db.log_message("ERROR", "Database connection failed")
    
    # Get logs
    logs = db.get_logs(5)
    print(f"Recent logs: {len(logs)} entries")
    for log in logs[:3]:
        print(f"  {log['level']}: {log['message']}")
    
    # File system operations
    print("\n--- File System Operations ---")
    fs = FileSystemManager()
    
    # Create files
    fs.create_file("test1.txt", "Hello, World!")
    fs.create_file("test2.txt", "Python Systems Demo")
    fs.create_file("data.json", '{"key": "value", "number": 42}')
    
    # List files
    files = fs.list_files()
    print(f"Files created: {files}")
    
    # Read file
    content = fs.read_file("test1.txt")
    print(f"Content of test1.txt: {content}")
    
    # Get file stats
    stats = fs.get_file_stats("data.json")
    if stats:
        print(f"data.json size: {stats['size']} bytes")
    
    # Configuration management
    print("\n--- Configuration Management ---")
    config = ConfigurationManager("demo_config.json")
    
    # Set some values
    config.set("app_name", "SystemsDemo")
    config.set("version", "1.0.0")
    config.set("features", ["database", "cache", "scheduler"])
    
    print(f"App name: {config.get('app_name')}")
    print(f"Server port: {config.get('server_port')}")
    print(f"Features: {config.get('features')}")
    
    # Cache operations
    print("\n--- Cache Operations ---")
    cache = CacheManager(max_size=10, ttl_seconds=60)
    
    # Store some data
    cache.set("user:1", {"name": "Alice", "role": "admin"})
    cache.set("config:theme", "dark")
    cache.set("temp:data", [1, 2, 3, 4, 5])
    
    # Retrieve data
    user_data = cache.get("user:1")
    print(f"Cached user data: {user_data}")
    
    theme = cache.get("config:theme")
    print(f"Cached theme: {theme}")
    
    # Cache stats
    stats = cache.stats()
    print(f"Cache stats: {stats}")
    
    # Task scheduler
    print("\n--- Task Scheduler ---")
    scheduler = TaskScheduler()
    
    def periodic_task():
        print(f"Periodic task executed at {datetime.now().strftime('%H:%M:%S')}")
    
    def cleanup_task():
        print("Cleanup task executed")
    
    scheduler.add_task(periodic_task, 2)  # Every 2 seconds
    scheduler.add_task(cleanup_task, 5)   # Every 5 seconds
    
    print("Starting scheduler for 8 seconds...")
    scheduler.start()
    time.sleep(8)
    scheduler.stop()
    print("Scheduler stopped")
    
    # Performance monitoring
    print("\n--- Performance Monitoring ---")
    monitor = PerformanceMonitor()
    
    # Simulate some metrics
    import random
    for i in range(20):
        monitor.record_metric("cpu_usage", random.uniform(10, 90))
        monitor.record_metric("memory_usage", random.uniform(30, 80))
        monitor.record_metric("response_time", random.uniform(50, 500))
    
    # Get statistics
    cpu_stats = monitor.get_stats("cpu_usage")
    print(f"CPU Usage - Min: {cpu_stats['min']:.1f}%, Max: {cpu_stats['max']:.1f}%, Avg: {cpu_stats['avg']:.1f}%")
    
    memory_stats = monitor.get_stats("memory_usage")
    print(f"Memory Usage - Min: {memory_stats['min']:.1f}%, Max: {memory_stats['max']:.1f}%, Avg: {memory_stats['avg']:.1f}%")
    
    response_stats = monitor.get_stats("response_time")
    print(f"Response Time - Min: {response_stats['min']:.1f}ms, Max: {response_stats['max']:.1f}ms, Avg: {response_stats['avg']:.1f}ms")
    
    # Network server demo (commented out to avoid blocking)
    print("\n--- Network Server Demo ---")
    print("Network server configured but not started to avoid blocking the demo")
    server = NetworkServer("localhost", 9999)
    print(f"Server would listen on localhost:9999")
    
    # Cleanup
    print("\n--- Cleanup ---")
    
    # Clean up files
    for file in fs.list_files():
        fs.delete_file(file)
    
    # Clean up database
    if os.path.exists("demo.db"):
        os.remove("demo.db")
    
    # Clean up config
    if os.path.exists("demo_config.json"):
        os.remove("demo_config.json")
    
    # Remove demo files directory
    import shutil
    if os.path.exists("demo_files"):
        shutil.rmtree("demo_files")
    
    print("All systems demo completed successfully!")
    print("All temporary files and databases have been cleaned up.")

if __name__ == "__main__":
    demo_all_systems()