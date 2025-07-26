import asyncio
import aiohttp
import aiofiles
import json
import time
import logging
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from typing import List, Dict, Optional, AsyncGenerator
import sqlite3
from datetime import datetime
import hashlib
import os

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class WebResource:
    url: str
    content: Optional[str] = None
    status_code: Optional[int] = None
    headers: Optional[Dict] = None
    error: Optional[str] = None
    fetch_time: Optional[float] = None
    content_hash: Optional[str] = None

class AsyncWebScraper:
    def __init__(self, max_concurrent=10, timeout=30):
        self.max_concurrent = max_concurrent
        self.timeout = aiohttp.ClientTimeout(total=timeout)
        self.session = None
        self.semaphore = asyncio.Semaphore(max_concurrent)
        
    async def __aenter__(self):
        self.session = aiohttp.ClientSession(timeout=self.timeout)
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if self.session:
            await self.session.close()
    
    async def fetch_single(self, url: str) -> WebResource:
        """Fetch a single URL asynchronously"""
        resource = WebResource(url=url)
        
        async with self.semaphore:
            try:
                start_time = time.time()
                async with self.session.get(url) as response:
                    resource.status_code = response.status
                    resource.headers = dict(response.headers)
                    resource.content = await response.text()
                    resource.fetch_time = time.time() - start_time
                    resource.content_hash = hashlib.md5(
                        resource.content.encode()
                    ).hexdigest()
                    
                logger.info(f"Fetched {url} - Status: {resource.status_code}")
                
            except Exception as e:
                resource.error = str(e)
                logger.error(f"Error fetching {url}: {e}")
        
        return resource
    
    async def fetch_multiple(self, urls: List[str]) -> List[WebResource]:
        """Fetch multiple URLs concurrently"""
        tasks = [self.fetch_single(url) for url in urls]
        return await asyncio.gather(*tasks)
    
    async def fetch_with_retries(self, url: str, max_retries: int = 3) -> WebResource:
        """Fetch URL with retry logic"""
        for attempt in range(max_retries + 1):
            resource = await self.fetch_single(url)
            
            if resource.error is None and resource.status_code == 200:
                return resource
            
            if attempt < max_retries:
                wait_time = 2 ** attempt  # Exponential backoff
                logger.info(f"Retrying {url} in {wait_time}s (attempt {attempt + 1})")
                await asyncio.sleep(wait_time)
        
        return resource

class AsyncFileProcessor:
    def __init__(self, max_workers=4):
        self.executor = ThreadPoolExecutor(max_workers=max_workers)
    
    async def read_file_async(self, file_path: str) -> str:
        """Read file asynchronously"""
        async with aiofiles.open(file_path, 'r', encoding='utf-8') as f:
            return await f.read()
    
    async def write_file_async(self, file_path: str, content: str) -> None:
        """Write file asynchronously"""
        # Ensure directory exists
        Path(file_path).parent.mkdir(parents=True, exist_ok=True)
        
        async with aiofiles.open(file_path, 'w', encoding='utf-8') as f:
            await f.write(content)
    
    async def process_large_file(self, file_path: str) -> AsyncGenerator[str, None]:
        """Process large file line by line asynchronously"""
        async with aiofiles.open(file_path, 'r', encoding='utf-8') as f:
            async for line in f:
                yield line.strip()
    
    def cpu_intensive_task(self, data: str) -> dict:
        """CPU-intensive task to be run in thread pool"""
        # Simulate CPU-intensive processing
        word_count = len(data.split())
        char_count = len(data)
        hash_value = hashlib.sha256(data.encode()).hexdigest()
        
        # Simulate more processing time
        time.sleep(0.01)
        
        return {
            'word_count': word_count,
            'char_count': char_count,
            'hash': hash_value
        }
    
    async def process_cpu_intensive_async(self, data: str) -> dict:
        """Run CPU-intensive task asynchronously using thread pool"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            self.executor, 
            self.cpu_intensive_task, 
            data
        )

class AsyncDatabaseManager:
    def __init__(self, db_path: str = "async_example.db"):
        self.db_path = db_path
        self.executor = ThreadPoolExecutor(max_workers=1)
    
    async def execute_query(self, query: str, params: tuple = ()) -> List[tuple]:
        """Execute database query asynchronously"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(
            self.executor,
            self._execute_sync,
            query,
            params
        )
    
    def _execute_sync(self, query: str, params: tuple = ()) -> List[tuple]:
        """Synchronous database operation"""
        with sqlite3.connect(self.db_path) as conn:
            cursor = conn.cursor()
            cursor.execute(query, params)
            
            if query.strip().upper().startswith('SELECT'):
                return cursor.fetchall()
            else:
                conn.commit()
                return []
    
    async def create_tables(self):
        """Create database tables"""
        create_table_query = """
        CREATE TABLE IF NOT EXISTS web_resources (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            url TEXT UNIQUE,
            content_hash TEXT,
            status_code INTEGER,
            fetch_time REAL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """
        await self.execute_query(create_table_query)
    
    async def save_resource(self, resource: WebResource):
        """Save web resource to database"""
        query = """
        INSERT OR REPLACE INTO web_resources 
        (url, content_hash, status_code, fetch_time) 
        VALUES (?, ?, ?, ?)
        """
        params = (
            resource.url,
            resource.content_hash,
            resource.status_code,
            resource.fetch_time
        )
        await self.execute_query(query, params)

class AsyncTaskQueue:
    def __init__(self):
        self.queue = asyncio.Queue()
        self.workers = []
        self.results = []
    
    async def add_task(self, coro):
        """Add a coroutine task to the queue"""
        await self.queue.put(coro)
    
    async def worker(self, worker_id: int):
        """Worker that processes tasks from the queue"""
        while True:
            try:
                # Get task from queue
                task = await self.queue.get()
                
                if task is None:  # Shutdown signal
                    break
                
                logger.info(f"Worker {worker_id} processing task")
                
                # Execute the task
                result = await task
                self.results.append(result)
                
                # Mark task as done
                self.queue.task_done()
                
            except Exception as e:
                logger.error(f"Worker {worker_id} error: {e}")
                self.queue.task_done()
    
    async def start_workers(self, num_workers: int = 3):
        """Start worker tasks"""
        self.workers = [
            asyncio.create_task(self.worker(i)) 
            for i in range(num_workers)
        ]
    
    async def stop_workers(self):
        """Stop all workers"""
        # Send shutdown signal to all workers
        for _ in self.workers:
            await self.queue.put(None)
        
        # Wait for workers to finish
        await asyncio.gather(*self.workers)

async def demonstrate_web_scraping():
    """Demonstrate async web scraping"""
    print("Async Web Scraping Demo")
    print("=" * 30)
    
    urls = [
        "https://httpbin.org/delay/1",
        "https://httpbin.org/delay/2", 
        "https://httpbin.org/status/200",
        "https://httpbin.org/json",
        "https://httpbin.org/html"
    ]
    
    # Use async context manager
    async with AsyncWebScraper(max_concurrent=3) as scraper:
        start_time = time.time()
        resources = await scraper.fetch_multiple(urls)
        total_time = time.time() - start_time
        
        print(f"Fetched {len(resources)} URLs in {total_time:.2f}s")
        
        for resource in resources:
            if resource.error:
                print(f"❌ {resource.url}: {resource.error}")
            else:
                print(f"✅ {resource.url}: {resource.status_code} "
                      f"({resource.fetch_time:.2f}s)")
    
    return resources

async def demonstrate_file_processing():
    """Demonstrate async file processing"""
    print("\nAsync File Processing Demo")
    print("=" * 30)
    
    processor = AsyncFileProcessor()
    
    # Create test files
    test_data = [
        "This is test file 1 with some content for processing.",
        "Another test file with different content and more words to analyze.",
        "Third file containing various text for comprehensive testing purposes."
    ]
    
    # Write files asynchronously
    write_tasks = []
    for i, data in enumerate(test_data):
        file_path = f"temp/test_file_{i}.txt"
        write_tasks.append(processor.write_file_async(file_path, data))
    
    await asyncio.gather(*write_tasks)
    print("Created test files")
    
    # Read and process files asynchronously
    read_tasks = []
    for i in range(len(test_data)):
        file_path = f"temp/test_file_{i}.txt"
        read_tasks.append(processor.read_file_async(file_path))
    
    file_contents = await asyncio.gather(*read_tasks)
    
    # Process content with CPU-intensive tasks
    process_tasks = []
    for content in file_contents:
        process_tasks.append(processor.process_cpu_intensive_async(content))
    
    results = await asyncio.gather(*process_tasks)
    
    for i, result in enumerate(results):
        print(f"File {i}: {result['word_count']} words, "
              f"{result['char_count']} chars")

async def demonstrate_database_operations():
    """Demonstrate async database operations"""
    print("\nAsync Database Operations Demo")
    print("=" * 30)
    
    db_manager = AsyncDatabaseManager()
    
    # Create tables
    await db_manager.create_tables()
    print("Database tables created")
    
    # Create sample resources
    sample_resources = [
        WebResource("https://example.com", status_code=200, 
                   fetch_time=1.5, content_hash="abc123"),
        WebResource("https://test.com", status_code=404, 
                   fetch_time=0.8, content_hash="def456"),
    ]
    
    # Save resources
    save_tasks = [db_manager.save_resource(res) for res in sample_resources]
    await asyncio.gather(*save_tasks)
    print("Saved resources to database")
    
    # Query database
    results = await db_manager.execute_query(
        "SELECT url, status_code, fetch_time FROM web_resources"
    )
    
    print("Database contents:")
    for row in results:
        print(f"  {row[0]}: {row[1]} ({row[2]}s)")

async def demonstrate_task_queue():
    """Demonstrate async task queue"""
    print("\nAsync Task Queue Demo")
    print("=" * 30)
    
    queue = AsyncTaskQueue()
    
    # Start workers
    await queue.start_workers(num_workers=2)
    
    # Add tasks to queue
    async def sample_task(task_id: int, duration: float):
        await asyncio.sleep(duration)
        return f"Task {task_id} completed in {duration}s"
    
    tasks = [
        sample_task(1, 1.0),
        sample_task(2, 0.5),
        sample_task(3, 1.5),
        sample_task(4, 0.8),
    ]
    
    # Add tasks to queue
    for task in tasks:
        await queue.add_task(task)
    
    # Wait for all tasks to complete
    await queue.queue.join()
    
    # Stop workers
    await queue.stop_workers()
    
    print(f"Completed {len(queue.results)} tasks:")
    for result in queue.results:
        print(f"  {result}")

async def main():
    """Main async function demonstrating various patterns"""
    print("Advanced Async Programming Demo")
    print("=" * 40)
    
    start_time = time.time()
    
    # Run demonstrations
    await demonstrate_web_scraping()
    await demonstrate_file_processing()
    await demonstrate_database_operations()
    await demonstrate_task_queue()
    
    total_time = time.time() - start_time
    print(f"\nTotal execution time: {total_time:.2f}s")
    
    # Cleanup
    import shutil
    if os.path.exists("temp"):
        shutil.rmtree("temp")
    if os.path.exists("async_example.db"):
        os.remove("async_example.db")
    
    print("Cleanup completed")

if __name__ == "__main__":
    # Run the async main function
    asyncio.run(main())