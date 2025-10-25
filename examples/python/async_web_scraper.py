import asyncio
import aiohttp
import time
import random
import json
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, asdict
import logging
from urllib.parse import urljoin

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class RequestResult:
    url: str
    status_code: Optional[int] = None
    response_time: Optional[float] = None
    content_length: Optional[int] = None
    error: Optional[str] = None
    success: bool = False

class AsyncWebScraper:
    """Advanced asynchronous web scraper with rate limiting and error handling"""
    
    def __init__(self, max_concurrent: int = 10, delay_between_requests: float = 0.1):
        self.max_concurrent = max_concurrent
        self.delay_between_requests = delay_between_requests
        self.session: Optional[aiohttp.ClientSession] = None
        self.semaphore = asyncio.Semaphore(max_concurrent)
        self.results: List[RequestResult] = []
    
    async def __aenter__(self):
        """Async context manager entry"""
        connector = aiohttp.TCPConnector(limit=100, limit_per_host=20)
        timeout = aiohttp.ClientTimeout(total=30, connect=10)
        self.session = aiohttp.ClientSession(
            connector=connector,
            timeout=timeout,
            headers={'User-Agent': 'AsyncWebScraper/1.0'}
        )
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit"""
        if self.session:
            await self.session.close()
    
    async def fetch_url(self, url: str) -> RequestResult:
        """Fetch a single URL with error handling and metrics"""
        async with self.semaphore:
            result = RequestResult(url=url)
            start_time = time.time()
            
            try:
                async with self.session.get(url) as response:
                    result.status_code = response.status
                    result.response_time = time.time() - start_time
                    result.content_length = len(await response.read())
                    result.success = 200 <= response.status < 300
                    
                    logger.info(f"Fetched {url}: {response.status} ({result.response_time:.3f}s)")
                    
            except asyncio.TimeoutError:
                result.error = "Request timeout"
                result.response_time = time.time() - start_time
                logger.warning(f"Timeout for {url}")
                
            except aiohttp.ClientError as e:
                result.error = f"Client error: {str(e)}"
                result.response_time = time.time() - start_time
                logger.error(f"Client error for {url}: {e}")
                
            except Exception as e:
                result.error = f"Unexpected error: {str(e)}"
                result.response_time = time.time() - start_time
                logger.error(f"Unexpected error for {url}: {e}")
            
            # Rate limiting
            if self.delay_between_requests > 0:
                await asyncio.sleep(self.delay_between_requests)
            
            return result
    
    async def fetch_multiple(self, urls: List[str]) -> List[RequestResult]:
        """Fetch multiple URLs concurrently"""
        logger.info(f"Starting to fetch {len(urls)} URLs with max concurrency: {self.max_concurrent}")
        
        start_time = time.time()
        tasks = [self.fetch_url(url) for url in urls]
        self.results = await asyncio.gather(*tasks, return_exceptions=False)
        total_time = time.time() - start_time
        
        successful = sum(1 for r in self.results if r.success)
        logger.info(f"Completed {len(urls)} requests in {total_time:.2f}s ({successful} successful)")
        
        return self.results
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get comprehensive statistics about the scraping session"""
        if not self.results:
            return {}
        
        successful_results = [r for r in self.results if r.success]
        failed_results = [r for r in self.results if not r.success]
        
        response_times = [r.response_time for r in self.results if r.response_time is not None]
        content_lengths = [r.content_length for r in successful_results if r.content_length is not None]
        
        status_codes = {}
        for result in self.results:
            if result.status_code:
                status_codes[result.status_code] = status_codes.get(result.status_code, 0) + 1
        
        error_types = {}
        for result in failed_results:
            if result.error:
                error_type = result.error.split(':')[0]
                error_types[error_type] = error_types.get(error_type, 0) + 1
        
        return {
            "total_requests": len(self.results),
            "successful_requests": len(successful_results),
            "failed_requests": len(failed_results),
            "success_rate": len(successful_results) / len(self.results) * 100,
            "average_response_time": sum(response_times) / len(response_times) if response_times else 0,
            "min_response_time": min(response_times) if response_times else 0,
            "max_response_time": max(response_times) if response_times else 0,
            "total_bytes_downloaded": sum(content_lengths),
            "average_content_length": sum(content_lengths) / len(content_lengths) if content_lengths else 0,
            "status_code_distribution": status_codes,
            "error_distribution": error_types
        }

class MockHTTPServer:
    """Mock HTTP server for testing purposes"""
    
    def __init__(self, host='localhost', port=8888):
        self.host = host
        self.port = port
        self.app = None
    
    async def create_app(self):
        """Create aiohttp application with test endpoints"""
        from aiohttp import web
        
        app = web.Application()
        
        # Normal endpoint
        async def hello(request):
            await asyncio.sleep(random.uniform(0.1, 0.5))  # Simulate processing time
            return web.json_response({
                'message': 'Hello, World!',
                'timestamp': time.time(),
                'random_data': [random.randint(1, 100) for _ in range(10)]
            })
        
        # Slow endpoint
        async def slow(request):
            await asyncio.sleep(random.uniform(1, 3))
            return web.json_response({'message': 'This was slow'})
        
        # Error endpoint
        async def error(request):
            if random.random() < 0.3:  # 30% chance of error
                raise web.HTTPInternalServerError(text="Random server error")
            return web.json_response({'message': 'Success after potential error'})
        
        # Large response endpoint
        async def large(request):
            large_data = {
                'data': [{'id': i, 'value': f'item_{i}', 'random': random.randint(1, 1000)} 
                        for i in range(1000)]
            }
            return web.json_response(large_data)
        
        app.router.add_get('/', hello)
        app.router.add_get('/hello', hello)
        app.router.add_get('/slow', slow)
        app.router.add_get('/error', error)
        app.router.add_get('/large', large)
        
        return app
    
    async def start_server(self):
        """Start the mock server"""
        from aiohttp import web
        
        app = await self.create_app()
        runner = web.AppRunner(app)
        await runner.setup()
        
        site = web.TCPSite(runner, self.host, self.port)
        await site.start()
        
        logger.info(f"Mock server started at http://{self.host}:{self.port}")
        return runner

async def run_scraping_demo():
    """Demonstrate advanced web scraping capabilities"""
    print("Advanced Asynchronous Web Scraping Demo")
    print("======================================")
    
    # Start mock server for testing
    server = MockHTTPServer()
    runner = await server.start_server()
    
    try:
        # Generate test URLs
        base_url = f"http://{server.host}:{server.port}"
        test_urls = []
        
        # Add various endpoint types
        endpoints = ['/', '/hello', '/slow', '/error', '/large']
        for _ in range(50):
            endpoint = random.choice(endpoints)
            test_urls.append(urljoin(base_url, endpoint))
        
        print(f"Generated {len(test_urls)} URLs for testing")
        
        # Test different concurrency levels
        concurrency_levels = [5, 10, 20]
        
        for max_concurrent in concurrency_levels:
            print(f"\n--- Testing with max_concurrent = {max_concurrent} ---")
            
            async with AsyncWebScraper(
                max_concurrent=max_concurrent,
                delay_between_requests=0.05
            ) as scraper:
                start_time = time.time()
                results = await scraper.fetch_multiple(test_urls)
                total_time = time.time() - start_time
                
                stats = scraper.get_statistics()
                
                print(f"Total time: {total_time:.2f}s")
                print(f"Requests per second: {len(test_urls) / total_time:.2f}")
                print(f"Success rate: {stats['success_rate']:.1f}%")
                print(f"Average response time: {stats['average_response_time']:.3f}s")
                print(f"Total bytes downloaded: {stats['total_bytes_downloaded']:,}")
                
                if stats['error_distribution']:
                    print(f"Error distribution: {stats['error_distribution']}")
        
        # Demonstrate error handling with invalid URLs
        print(f"\n--- Testing Error Handling ---")
        
        error_urls = [
            "http://nonexistent-domain-12345.com",
            "http://localhost:9999/notfound",
            f"{base_url}/nonexistent",
        ] + test_urls[:5]  # Mix of bad and good URLs
        
        async with AsyncWebScraper(max_concurrent=5) as scraper:
            results = await scraper.fetch_multiple(error_urls)
            stats = scraper.get_statistics()
            
            print(f"Mixed URL test results:")
            print(f"Total requests: {stats['total_requests']}")
            print(f"Successful: {stats['successful_requests']}")
            print(f"Failed: {stats['failed_requests']}")
            print(f"Success rate: {stats['success_rate']:.1f}%")
            
            if stats['error_distribution']:
                print(f"Error types: {stats['error_distribution']}")
        
        # Performance comparison: sync vs async simulation
        print(f"\n--- Performance Comparison ---")
        
        # Simulate synchronous approach time
        sample_response_times = [r.response_time for r in results if r.response_time][:10]
        simulated_sync_time = sum(sample_response_times)
        actual_async_time = max(sample_response_times)  # Concurrent execution
        
        print(f"Simulated synchronous time: {simulated_sync_time:.2f}s")
        print(f"Actual asynchronous time: {actual_async_time:.2f}s")
        print(f"Speedup factor: {simulated_sync_time / actual_async_time:.1f}x")
        
    finally:
        # Clean up server
        await runner.cleanup()
        logger.info("Mock server stopped")

class RealWorldScrapingExample:
    """Example of scraping real-world APIs (for educational purposes)"""
    
    @staticmethod
    async def scrape_public_apis():
        """Demonstrate scraping public APIs with proper headers and error handling"""
        print("\nReal-world API Scraping Example")
        print("===============================")
        
        # List of public APIs that allow programmatic access
        public_apis = [
            "https://httpbin.org/json",
            "https://httpbin.org/uuid",
            "https://httpbin.org/ip",
            "https://httpbin.org/user-agent",
            "https://httpbin.org/headers",
        ]
        
        async with AsyncWebScraper(max_concurrent=3, delay_between_requests=0.5) as scraper:
            # Add respectful headers
            scraper.session._default_headers.update({
                'Accept': 'application/json',
                'Accept-Language': 'en-US,en;q=0.9',
            })
            
            results = await scraper.fetch_multiple(public_apis)
            stats = scraper.get_statistics()
            
            print(f"API scraping results:")
            print(f"Success rate: {stats['success_rate']:.1f}%")
            print(f"Average response time: {stats['average_response_time']:.3f}s")
            
            # Show sample successful responses
            successful_results = [r for r in results if r.success][:3]
            for result in successful_results:
                print(f"✓ {result.url}: {result.status_code} ({result.response_time:.3f}s)")

async def main():
    """Main function to run all demonstrations"""
    await run_scraping_demo()
    await RealWorldScrapingExample.scrape_public_apis()
    
    print("\nAdvanced web scraping demonstration completed!")

if __name__ == "__main__":
    # Run the async main function
    asyncio.run(main())