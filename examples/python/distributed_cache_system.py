import asyncio
import aiohttp
import json
import time
import hashlib
import sqlite3
import threading
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, asdict
from concurrent.futures import ThreadPoolExecutor
import logging

@dataclass
class CacheEntry:
    key: str
    value: Any
    timestamp: datetime
    ttl: int
    access_count: int
    last_access: datetime

class DistributedCache:
    def __init__(self, db_path: str = "cache.db", max_size: int = 10000):
        self.db_path = db_path
        self.max_size = max_size
        self.local_cache: Dict[str, CacheEntry] = {}
        self.cache_lock = threading.RLock()
        self.stats = {
            'hits': 0,
            'misses': 0,
            'evictions': 0,
            'total_requests': 0
        }
        
        # Initialize database
        self._init_database()
        
        # Background cleanup thread
        self.cleanup_thread = threading.Thread(target=self._cleanup_expired, daemon=True)
        self.cleanup_thread.start()
        
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)
    
    def _init_database(self):
        """Initialize SQLite database for persistent storage"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS cache_entries (
                key TEXT PRIMARY KEY,
                value TEXT,
                timestamp TEXT,
                ttl INTEGER,
                access_count INTEGER,
                last_access TEXT
            )
        ''')
        
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_timestamp ON cache_entries(timestamp)
        ''')
        
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_last_access ON cache_entries(last_access)
        ''')
        
        conn.commit()
        conn.close()
    
    def _hash_key(self, key: str) -> str:
        """Generate consistent hash for key distribution"""
        return hashlib.md5(key.encode()).hexdigest()
    
    def _serialize_value(self, value: Any) -> str:
        """Serialize value for storage"""
        try:
            return json.dumps(value)
        except (TypeError, ValueError):
            return str(value)
    
    def _deserialize_value(self, serialized: str) -> Any:
        """Deserialize value from storage"""
        try:
            return json.loads(serialized)
        except (json.JSONDecodeError, ValueError):
            return serialized
    
    def _is_expired(self, entry: CacheEntry) -> bool:
        """Check if cache entry is expired"""
        if entry.ttl <= 0:
            return False
        return datetime.now() > entry.timestamp + timedelta(seconds=entry.ttl)
    
    def _evict_lru(self):
        """Evict least recently used entries"""
        with self.cache_lock:
            if len(self.local_cache) <= self.max_size:
                return
            
            # Sort by last access time and remove oldest
            sorted_entries = sorted(
                self.local_cache.items(),
                key=lambda x: x[1].last_access
            )
            
            to_remove = len(self.local_cache) - self.max_size + 100  # Remove extra for buffer
            
            for i in range(min(to_remove, len(sorted_entries))):
                key = sorted_entries[i][0]
                del self.local_cache[key]
                self.stats['evictions'] += 1
    
    def set(self, key: str, value: Any, ttl: int = 3600) -> bool:
        """Set a value in the cache"""
        try:
            with self.cache_lock:
                entry = CacheEntry(
                    key=key,
                    value=value,
                    timestamp=datetime.now(),
                    ttl=ttl,
                    access_count=0,
                    last_access=datetime.now()
                )
                
                # Store in local cache
                self.local_cache[key] = entry
                
                # Evict if necessary
                if len(self.local_cache) > self.max_size:
                    self._evict_lru()
                
                # Store in database
                self._persist_entry(entry)
                
                self.logger.info(f"Set key '{key}' with TTL {ttl}s")
                return True
                
        except Exception as e:
            self.logger.error(f"Error setting key '{key}': {e}")
            return False
    
    def get(self, key: str) -> Optional[Any]:
        """Get a value from the cache"""
        self.stats['total_requests'] += 1
        
        try:
            with self.cache_lock:
                # Check local cache first
                if key in self.local_cache:
                    entry = self.local_cache[key]
                    
                    if self._is_expired(entry):
                        del self.local_cache[key]
                        self._remove_from_db(key)
                        self.stats['misses'] += 1
                        return None
                    
                    # Update access statistics
                    entry.access_count += 1
                    entry.last_access = datetime.now()
                    self.stats['hits'] += 1
                    
                    self.logger.debug(f"Cache hit for key '{key}'")
                    return entry.value
                
                # Check database
                entry = self._load_from_db(key)
                if entry and not self._is_expired(entry):
                    # Load into local cache
                    entry.access_count += 1
                    entry.last_access = datetime.now()
                    self.local_cache[key] = entry
                    
                    self.stats['hits'] += 1
                    self.logger.debug(f"Database hit for key '{key}'")
                    return entry.value
                
                self.stats['misses'] += 1
                return None
                
        except Exception as e:
            self.logger.error(f"Error getting key '{key}': {e}")
            self.stats['misses'] += 1
            return None
    
    def delete(self, key: str) -> bool:
        """Delete a key from the cache"""
        try:
            with self.cache_lock:
                deleted = False
                
                if key in self.local_cache:
                    del self.local_cache[key]
                    deleted = True
                
                if self._remove_from_db(key):
                    deleted = True
                
                if deleted:
                    self.logger.info(f"Deleted key '{key}'")
                
                return deleted
                
        except Exception as e:
            self.logger.error(f"Error deleting key '{key}': {e}")
            return False
    
    def _persist_entry(self, entry: CacheEntry):
        """Persist entry to database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            INSERT OR REPLACE INTO cache_entries 
            (key, value, timestamp, ttl, access_count, last_access)
            VALUES (?, ?, ?, ?, ?, ?)
        ''', (
            entry.key,
            self._serialize_value(entry.value),
            entry.timestamp.isoformat(),
            entry.ttl,
            entry.access_count,
            entry.last_access.isoformat()
        ))
        
        conn.commit()
        conn.close()
    
    def _load_from_db(self, key: str) -> Optional[CacheEntry]:
        """Load entry from database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            SELECT key, value, timestamp, ttl, access_count, last_access
            FROM cache_entries WHERE key = ?
        ''', (key,))
        
        row = cursor.fetchone()
        conn.close()
        
        if row:
            return CacheEntry(
                key=row[0],
                value=self._deserialize_value(row[1]),
                timestamp=datetime.fromisoformat(row[2]),
                ttl=row[3],
                access_count=row[4],
                last_access=datetime.fromisoformat(row[5])
            )
        
        return None
    
    def _remove_from_db(self, key: str) -> bool:
        """Remove entry from database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('DELETE FROM cache_entries WHERE key = ?', (key,))
        deleted = cursor.rowcount > 0
        
        conn.commit()
        conn.close()
        
        return deleted
    
    def _cleanup_expired(self):
        """Background thread to clean up expired entries"""
        while True:
            try:
                time.sleep(60)  # Run every minute
                
                with self.cache_lock:
                    # Clean local cache
                    expired_keys = []
                    for key, entry in self.local_cache.items():
                        if self._is_expired(entry):
                            expired_keys.append(key)
                    
                    for key in expired_keys:
                        del self.local_cache[key]
                        self.logger.debug(f"Expired key from local cache: {key}")
                
                # Clean database
                conn = sqlite3.connect(self.db_path)
                cursor = conn.cursor()
                
                cursor.execute('''
                    DELETE FROM cache_entries 
                    WHERE ttl > 0 AND datetime(timestamp, '+' || ttl || ' seconds') < datetime('now')
                ''')
                
                deleted_count = cursor.rowcount
                if deleted_count > 0:
                    self.logger.info(f"Cleaned {deleted_count} expired entries from database")
                
                conn.commit()
                conn.close()
                
            except Exception as e:
                self.logger.error(f"Error in cleanup thread: {e}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get cache statistics"""
        with self.cache_lock:
            hit_rate = 0.0
            if self.stats['total_requests'] > 0:
                hit_rate = self.stats['hits'] / self.stats['total_requests'] * 100
            
            return {
                'local_cache_size': len(self.local_cache),
                'hit_rate_percent': round(hit_rate, 2),
                'total_requests': self.stats['total_requests'],
                'hits': self.stats['hits'],
                'misses': self.stats['misses'],
                'evictions': self.stats['evictions']
            }
    
    def clear(self):
        """Clear all cache entries"""
        with self.cache_lock:
            self.local_cache.clear()
            
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            cursor.execute('DELETE FROM cache_entries')
            conn.commit()
            conn.close()
            
            self.logger.info("Cache cleared")
    
    async def async_get_multiple(self, keys: List[str]) -> Dict[str, Any]:
        """Asynchronously get multiple keys"""
        results = {}
        
        def get_key(key):
            return key, self.get(key)
        
        with ThreadPoolExecutor(max_workers=10) as executor:
            loop = asyncio.get_event_loop()
            tasks = []
            
            for key in keys:
                task = loop.run_in_executor(executor, get_key, key)
                tasks.append(task)
            
            completed_tasks = await asyncio.gather(*tasks)
            
            for key, value in completed_tasks:
                if value is not None:
                    results[key] = value
        
        return results
    
    async def async_set_multiple(self, data: Dict[str, Any], ttl: int = 3600) -> Dict[str, bool]:
        """Asynchronously set multiple keys"""
        results = {}
        
        def set_key(key, value):
            return key, self.set(key, value, ttl)
        
        with ThreadPoolExecutor(max_workers=10) as executor:
            loop = asyncio.get_event_loop()
            tasks = []
            
            for key, value in data.items():
                task = loop.run_in_executor(executor, set_key, key, value)
                tasks.append(task)
            
            completed_tasks = await asyncio.gather(*tasks)
            
            for key, success in completed_tasks:
                results[key] = success
        
        return results

class CacheHTTPServer:
    def __init__(self, cache: DistributedCache, host: str = 'localhost', port: int = 8080):
        self.cache = cache
        self.host = host
        self.port = port
        self.app = None
    
    async def handle_get(self, request):
        """Handle GET request to retrieve cached value"""
        key = request.query.get('key')
        if not key:
            return aiohttp.web.json_response({'error': 'Missing key parameter'}, status=400)
        
        value = self.cache.get(key)
        if value is None:
            return aiohttp.web.json_response({'error': 'Key not found'}, status=404)
        
        return aiohttp.web.json_response({'key': key, 'value': value})
    
    async def handle_set(self, request):
        """Handle POST request to set cached value"""
        try:
            data = await request.json()
            key = data.get('key')
            value = data.get('value')
            ttl = data.get('ttl', 3600)
            
            if not key or value is None:
                return aiohttp.web.json_response({'error': 'Missing key or value'}, status=400)
            
            success = self.cache.set(key, value, ttl)
            if success:
                return aiohttp.web.json_response({'message': 'Key set successfully'})
            else:
                return aiohttp.web.json_response({'error': 'Failed to set key'}, status=500)
                
        except Exception as e:
            return aiohttp.web.json_response({'error': str(e)}, status=400)
    
    async def handle_delete(self, request):
        """Handle DELETE request to remove cached value"""
        key = request.query.get('key')
        if not key:
            return aiohttp.web.json_response({'error': 'Missing key parameter'}, status=400)
        
        success = self.cache.delete(key)
        if success:
            return aiohttp.web.json_response({'message': 'Key deleted successfully'})
        else:
            return aiohttp.web.json_response({'error': 'Key not found'}, status=404)
    
    async def handle_stats(self, request):
        """Handle GET request to retrieve cache statistics"""
        stats = self.cache.get_stats()
        return aiohttp.web.json_response(stats)
    
    async def handle_clear(self, request):
        """Handle POST request to clear all cache entries"""
        self.cache.clear()
        return aiohttp.web.json_response({'message': 'Cache cleared successfully'})
    
    async def start_server(self):
        """Start the HTTP server"""
        app = aiohttp.web.Application()
        
        app.router.add_get('/get', self.handle_get)
        app.router.add_post('/set', self.handle_set)
        app.router.add_delete('/delete', self.handle_delete)
        app.router.add_get('/stats', self.handle_stats)
        app.router.add_post('/clear', self.handle_clear)
        
        runner = aiohttp.web.AppRunner(app)
        await runner.setup()
        
        site = aiohttp.web.TCPSite(runner, self.host, self.port)
        await site.start()
        
        print(f"Cache server started at http://{self.host}:{self.port}")
        return runner

async def benchmark_cache(cache: DistributedCache, num_operations: int = 10000):
    """Benchmark cache performance"""
    print(f"Running benchmark with {num_operations} operations...")
    
    # Set operations
    start_time = time.time()
    set_tasks = []
    
    for i in range(num_operations // 2):
        key = f"benchmark_key_{i}"
        value = {"data": f"benchmark_value_{i}", "timestamp": time.time(), "counter": i}
        set_tasks.append((key, value))
    
    set_results = await cache.async_set_multiple(dict(set_tasks))
    set_time = time.time() - start_time
    
    successful_sets = sum(1 for success in set_results.values() if success)
    print(f"Set operations: {successful_sets}/{len(set_tasks)} successful in {set_time:.2f}s")
    print(f"Set rate: {successful_sets/set_time:.2f} ops/sec")
    
    # Get operations
    start_time = time.time()
    get_keys = [f"benchmark_key_{i}" for i in range(num_operations // 2)]
    
    get_results = await cache.async_get_multiple(get_keys)
    get_time = time.time() - start_time
    
    successful_gets = len(get_results)
    print(f"Get operations: {successful_gets}/{len(get_keys)} successful in {get_time:.2f}s")
    print(f"Get rate: {successful_gets/get_time:.2f} ops/sec")
    
    return {
        'set_rate': successful_sets/set_time,
        'get_rate': successful_gets/get_time,
        'set_success_rate': successful_sets/len(set_tasks) * 100,
        'get_success_rate': successful_gets/len(get_keys) * 100
    }

async def demo_cache_operations():
    """Demonstrate various cache operations"""
    print("=== Distributed Cache Demo ===")
    
    # Initialize cache
    cache = DistributedCache(db_path="demo_cache.db", max_size=1000)
    
    # Basic operations
    print("\n1. Basic Operations:")
    cache.set("user:1", {"name": "Alice", "age": 30, "email": "alice@example.com"})
    cache.set("user:2", {"name": "Bob", "age": 25, "email": "bob@example.com"})
    cache.set("session:abc123", {"user_id": 1, "expires": "2024-12-31"}, ttl=1800)
    
    user1 = cache.get("user:1")
    session = cache.get("session:abc123")
    print(f"Retrieved user:1: {user1}")
    print(f"Retrieved session: {session}")
    
    # Batch operations
    print("\n2. Batch Operations:")
    batch_data = {
        "config:timeout": 30,
        "config:max_users": 1000,
        "config:debug": True,
        "temp:calc_result": 42.857
    }
    
    batch_results = await cache.async_set_multiple(batch_data, ttl=600)
    print(f"Batch set results: {batch_results}")
    
    batch_keys = list(batch_data.keys())
    batch_values = await cache.async_get_multiple(batch_keys)
    print(f"Batch get results: {batch_values}")
    
    # Statistics
    print("\n3. Cache Statistics:")
    stats = cache.get_stats()
    for key, value in stats.items():
        print(f"{key}: {value}")
    
    # Benchmark
    print("\n4. Performance Benchmark:")
    benchmark_results = await benchmark_cache(cache, 1000)
    for key, value in benchmark_results.items():
        print(f"{key}: {value:.2f}")
    
    # Test expiration
    print("\n5. Testing TTL Expiration:")
    cache.set("temp_key", "temp_value", ttl=2)
    print(f"Immediately after set: {cache.get('temp_key')}")
    
    await asyncio.sleep(3)
    print(f"After TTL expiration: {cache.get('temp_key')}")
    
    # Final statistics
    print("\n6. Final Statistics:")
    final_stats = cache.get_stats()
    for key, value in final_stats.items():
        print(f"{key}: {value}")

async def run_http_server_demo():
    """Run HTTP server demo"""
    cache = DistributedCache(db_path="server_cache.db")
    server = CacheHTTPServer(cache, port=8081)
    
    # Start server
    runner = await server.start_server()
    
    try:
        # Demo HTTP client operations
        async with aiohttp.ClientSession() as session:
            base_url = "http://localhost:8081"
            
            # Set some values via HTTP
            set_data = {"key": "http_test", "value": {"message": "Hello from HTTP!"}, "ttl": 300}
            async with session.post(f"{base_url}/set", json=set_data) as resp:
                result = await resp.json()
                print(f"HTTP Set: {result}")
            
            # Get value via HTTP
            async with session.get(f"{base_url}/get?key=http_test") as resp:
                result = await resp.json()
                print(f"HTTP Get: {result}")
            
            # Get stats via HTTP
            async with session.get(f"{base_url}/stats") as resp:
                stats = await resp.json()
                print(f"HTTP Stats: {stats}")
        
        print("HTTP server demo completed. Server will continue running...")
        
        # Keep server running
        while True:
            await asyncio.sleep(1)
            
    except KeyboardInterrupt:
        print("Shutting down server...")
    finally:
        await runner.cleanup()

def main():
    """Main function to run the cache system"""
    import sys
    
    if len(sys.argv) > 1 and sys.argv[1] == "server":
        # Run HTTP server mode
        asyncio.run(run_http_server_demo())
    else:
        # Run demo mode
        asyncio.run(demo_cache_operations())

if __name__ == "__main__":
    main()