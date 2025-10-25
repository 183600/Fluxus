import asyncio
import aiohttp
import aiofiles
import time
from datetime import datetime
import json
import random
from typing import List, Dict, Any
from dataclasses import dataclass
import logging

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class ScrapingResult:
    url: str
    status_code: int
    content_length: int
    response_time: float
    title: str
    links_count: int
    images_count: int
    timestamp: datetime
    error: str = None

class AsyncWebScraper:
    def __init__(self, max_concurrent_requests: int = 10, request_delay: float = 1.0):
        self.max_concurrent_requests = max_concurrent_requests
        self.request_delay = request_delay
        self.session = None
        self.semaphore = asyncio.Semaphore(max_concurrent_requests)
        self.results: List[ScrapingResult] = []
        
    async def __aenter__(self):
        connector = aiohttp.TCPConnector(limit=100, limit_per_host=10)
        timeout = aiohttp.ClientTimeout(total=30, connect=10)
        self.session = aiohttp.ClientSession(
            connector=connector,
            timeout=timeout,
            headers={
                'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
            }
        )
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if self.session:
            await self.session.close()
    
    async def fetch_url(self, url: str) -> ScrapingResult:
        """Fetch a single URL and extract basic information"""
        async with self.semaphore:
            start_time = time.time()
            
            try:
                await asyncio.sleep(self.request_delay)
                
                async with self.session.get(url) as response:
                    content = await response.text()
                    response_time = time.time() - start_time
                    
                    # Extract basic information
                    title = self.extract_title(content)
                    links_count = content.lower().count('<a href')
                    images_count = content.lower().count('<img')
                    
                    result = ScrapingResult(
                        url=url,
                        status_code=response.status,
                        content_length=len(content),
                        response_time=response_time,
                        title=title,
                        links_count=links_count,
                        images_count=images_count,
                        timestamp=datetime.now()
                    )
                    
                    logger.info(f"✓ Scraped {url} - Status: {response.status}, Time: {response_time:.2f}s")
                    return result
                    
            except Exception as e:
                error_result = ScrapingResult(
                    url=url,
                    status_code=0,
                    content_length=0,
                    response_time=time.time() - start_time,
                    title="",
                    links_count=0,
                    images_count=0,
                    timestamp=datetime.now(),
                    error=str(e)
                )
                
                logger.error(f"✗ Failed to scrape {url}: {str(e)}")
                return error_result
    
    def extract_title(self, html_content: str) -> str:
        """Extract title from HTML content"""
        try:
            start = html_content.lower().find('<title>')
            if start != -1:
                start += 7
                end = html_content.lower().find('</title>', start)
                if end != -1:
                    return html_content[start:end].strip()
        except:
            pass
        return "No title found"
    
    async def scrape_urls(self, urls: List[str]) -> List[ScrapingResult]:
        """Scrape multiple URLs concurrently"""
        logger.info(f"Starting to scrape {len(urls)} URLs with {self.max_concurrent_requests} concurrent requests")
        
        start_time = time.time()
        
        # Create tasks for all URLs
        tasks = [self.fetch_url(url) for url in urls]
        
        # Execute all tasks concurrently
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Filter out exceptions and collect valid results
        valid_results = []
        for result in results:
            if isinstance(result, ScrapingResult):
                valid_results.append(result)
                self.results.append(result)
        
        total_time = time.time() - start_time
        logger.info(f"Completed scraping {len(valid_results)} URLs in {total_time:.2f} seconds")
        
        return valid_results
    
    async def save_results_to_file(self, filename: str):
        """Save scraping results to JSON file"""
        results_data = []
        for result in self.results:
            results_data.append({
                'url': result.url,
                'status_code': result.status_code,
                'content_length': result.content_length,
                'response_time': result.response_time,
                'title': result.title,
                'links_count': result.links_count,
                'images_count': result.images_count,
                'timestamp': result.timestamp.isoformat(),
                'error': result.error
            })
        
        async with aiofiles.open(filename, 'w') as f:
            await f.write(json.dumps(results_data, indent=2))
        
        logger.info(f"Results saved to {filename}")
    
    def generate_statistics(self) -> Dict[str, Any]:
        """Generate statistics from scraping results"""
        if not self.results:
            return {}
        
        successful_results = [r for r in self.results if r.error is None]
        failed_results = [r for r in self.results if r.error is not None]
        
        stats = {
            'total_urls': len(self.results),
            'successful_requests': len(successful_results),
            'failed_requests': len(failed_results),
            'success_rate': len(successful_results) / len(self.results) * 100,
            'average_response_time': sum(r.response_time for r in successful_results) / len(successful_results) if successful_results else 0,
            'total_content_length': sum(r.content_length for r in successful_results),
            'average_content_length': sum(r.content_length for r in successful_results) / len(successful_results) if successful_results else 0,
            'total_links_found': sum(r.links_count for r in successful_results),
            'total_images_found': sum(r.images_count for r in successful_results),
            'status_code_distribution': {}
        }
        
        # Status code distribution
        for result in self.results:
            status = result.status_code
            stats['status_code_distribution'][status] = stats['status_code_distribution'].get(status, 0) + 1
        
        return stats

class URLGenerator:
    """Generate URLs for testing purposes"""
    
    @staticmethod
    def generate_test_urls(count: int = 20) -> List[str]:
        """Generate a list of test URLs"""
        # Note: These are example URLs - in real usage, replace with actual URLs you want to scrape
        base_urls = [
            "https://httpbin.org/delay/1",
            "https://httpbin.org/delay/2", 
            "https://httpbin.org/delay/3",
            "https://httpbin.org/status/200",
            "https://httpbin.org/status/404",
            "https://httpbin.org/status/500",
            "https://httpbin.org/html",
            "https://httpbin.org/json",
            "https://httpbin.org/xml",
            "https://httpbin.org/robots.txt"
        ]
        
        urls = []
        for i in range(count):
            base_url = random.choice(base_urls)
            # Add some variation to avoid too much duplication
            if 'delay' in base_url:
                delay = random.randint(1, 3)
                urls.append(f"https://httpbin.org/delay/{delay}")
            elif 'status' in base_url:
                status = random.choice([200, 404, 500, 301, 302])
                urls.append(f"https://httpbin.org/status/{status}")
            else:
                urls.append(base_url)
        
        return urls

async def run_scraping_demo():
    """Run a demonstration of the async web scraper"""
    print("Async Web Scraper Demo")
    print("=" * 50)
    
    # Generate test URLs
    urls = URLGenerator.generate_test_urls(25)
    print(f"Generated {len(urls)} URLs for testing")
    
    # Configure scraper
    async with AsyncWebScraper(max_concurrent_requests=5, request_delay=0.5) as scraper:
        # Scrape URLs
        results = await scraper.scrape_urls(urls)
        
        # Generate and display statistics
        stats = scraper.generate_statistics()
        
        print("\nScraping Statistics:")
        print("=" * 30)
        print(f"Total URLs: {stats['total_urls']}")
        print(f"Successful: {stats['successful_requests']}")
        print(f"Failed: {stats['failed_requests']}")
        print(f"Success Rate: {stats['success_rate']:.1f}%")
        print(f"Average Response Time: {stats['average_response_time']:.2f}s")
        print(f"Total Content Length: {stats['total_content_length']:,} bytes")
        print(f"Average Content Length: {stats['average_content_length']:.0f} bytes")
        print(f"Total Links Found: {stats['total_links_found']}")
        print(f"Total Images Found: {stats['total_images_found']}")
        
        print("\nStatus Code Distribution:")
        for status_code, count in stats['status_code_distribution'].items():
            print(f"  {status_code}: {count} requests")
        
        # Save results to file
        await scraper.save_results_to_file('scraping_results.json')
        
        # Show sample results
        print("\nSample Results:")
        print("-" * 20)
        for i, result in enumerate(results[:5]):
            if result.error is None:
                print(f"{i+1}. {result.url}")
                print(f"   Status: {result.status_code}, Time: {result.response_time:.2f}s")
                print(f"   Title: {result.title[:50]}...")
                print(f"   Links: {result.links_count}, Images: {result.images_count}")
            else:
                print(f"{i+1}. {result.url} - ERROR: {result.error}")
            print()

async def run_batch_processing_demo():
    """Demonstrate batch processing capabilities"""
    print("\nBatch Processing Demo")
    print("=" * 30)
    
    # Simulate processing multiple batches
    batch_size = 10
    total_urls = 50
    all_urls = URLGenerator.generate_test_urls(total_urls)
    
    async with AsyncWebScraper(max_concurrent_requests=3, request_delay=0.2) as scraper:
        for i in range(0, len(all_urls), batch_size):
            batch = all_urls[i:i + batch_size]
            batch_num = i // batch_size + 1
            
            print(f"Processing batch {batch_num} ({len(batch)} URLs)...")
            
            start_time = time.time()
            await scraper.scrape_urls(batch)
            batch_time = time.time() - start_time
            
            print(f"Batch {batch_num} completed in {batch_time:.2f}s")
            
            # Small delay between batches
            await asyncio.sleep(1)
        
        # Final statistics
        final_stats = scraper.generate_statistics()
        print(f"\nFinal Results: {final_stats['successful_requests']}/{final_stats['total_urls']} successful")

async def main():
    """Main execution function"""
    try:
        await run_scraping_demo()
        await run_batch_processing_demo()
        
    except KeyboardInterrupt:
        print("\nScraping interrupted by user")
    except Exception as e:
        logger.error(f"Unexpected error: {e}")

if __name__ == "__main__":
    asyncio.run(main())