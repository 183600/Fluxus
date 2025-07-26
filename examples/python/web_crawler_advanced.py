import asyncio
import aiohttp
import aiofiles
import json
import time
import hashlib
import urllib.parse
import re
from datetime import datetime, timedelta
from typing import Dict, List, Set, Optional, Tuple, Any
from dataclasses import dataclass, asdict
from collections import defaultdict, deque
import sqlite3
import threading
from concurrent.futures import ThreadPoolExecutor
import logging
from bs4 import BeautifulSoup
import csv
import os

@dataclass
class WebPage:
    url: str
    title: str
    content: str
    links: List[str]
    timestamp: datetime
    response_time: float
    status_code: int
    content_hash: str
    headers: Dict[str, str]
    meta_description: str = ""
    keywords: List[str] = None
    images: List[str] = None
    
    def __post_init__(self):
        if self.keywords is None:
            self.keywords = []
        if self.images is None:
            self.images = []

@dataclass
class CrawlStats:
    total_pages: int = 0
    successful_crawls: int = 0
    failed_crawls: int = 0
    total_links_found: int = 0
    unique_domains: int = 0
    average_response_time: float = 0.0
    start_time: datetime = None
    end_time: datetime = None
    
    def __post_init__(self):
        if self.start_time is None:
            self.start_time = datetime.now()

class AdvancedWebCrawler:
    def __init__(self, 
                 max_concurrent: int = 10,
                 delay_between_requests: float = 1.0,
                 max_pages: int = 100,
                 max_depth: int = 3,
                 db_path: str = "web_crawler.db",
                 respect_robots_txt: bool = True):
        
        self.max_concurrent = max_concurrent
        self.delay_between_requests = delay_between_requests
        self.max_pages = max_pages
        self.max_depth = max_depth
        self.db_path = db_path
        self.respect_robots_txt = respect_robots_txt
        
        # Crawling state
        self.crawled_urls: Set[str] = set()
        self.failed_urls: Set[str] = set()
        self.url_queue: deque = deque()
        self.domain_delays: Dict[str, float] = defaultdict(float)
        self.robots_cache: Dict[str, Dict] = {}
        self.stats = CrawlStats()
        
        # Thread safety
        self.lock = threading.RLock()
        self.semaphore = asyncio.Semaphore(max_concurrent)
        
        # Session and headers
        self.session = None
        self.headers = {
            'User-Agent': 'Advanced Web Crawler 1.0 (Educational Purpose)',
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.5',
            'Accept-Encoding': 'gzip, deflate',
            'Connection': 'keep-alive',
            'Upgrade-Insecure-Requests': '1',
        }
        
        # Initialize database
        self._init_database()
        
        # Setup logging
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger(__name__)
    
    def _init_database(self):
        """Initialize SQLite database for storing crawled data"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS crawled_pages (
                url TEXT PRIMARY KEY,
                title TEXT,
                content TEXT,
                content_hash TEXT,
                timestamp TEXT,
                response_time REAL,
                status_code INTEGER,
                depth INTEGER,
                domain TEXT,
                links_count INTEGER,
                meta_description TEXT,
                keywords TEXT
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS page_links (
                source_url TEXT,
                target_url TEXT,
                link_text TEXT,
                FOREIGN KEY (source_url) REFERENCES crawled_pages (url)
            )
        ''')
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS crawl_sessions (
                session_id TEXT PRIMARY KEY,
                start_time TEXT,
                end_time TEXT,
                total_pages INTEGER,
                successful_crawls INTEGER,
                failed_crawls INTEGER
            )
        ''')
        
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_domain ON crawled_pages(domain)
        ''')
        
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_timestamp ON crawled_pages(timestamp)
        ''')
        
        conn.commit()
        conn.close()
        
        self.logger.info(f"Database initialized: {self.db_path}")
    
    def _normalize_url(self, url: str, base_url: str = None) -> str:
        """Normalize and resolve relative URLs"""
        if base_url:
            url = urllib.parse.urljoin(base_url, url)
        
        parsed = urllib.parse.urlparse(url)
        
        # Remove fragment
        normalized = urllib.parse.urlunparse((
            parsed.scheme.lower(),
            parsed.netloc.lower(),
            parsed.path,
            parsed.params,
            parsed.query,
            ''  # Remove fragment
        ))
        
        return normalized
    
    def _extract_domain(self, url: str) -> str:
        """Extract domain from URL"""
        return urllib.parse.urlparse(url).netloc.lower()
    
    def _is_valid_url(self, url: str) -> bool:
        """Check if URL is valid and should be crawled"""
        if not url or not url.startswith(('http://', 'https://')):
            return False
        
        # Skip common file extensions
        skip_extensions = {'.pdf', '.doc', '.docx', '.xls', '.xlsx', '.ppt', '.pptx',
                          '.zip', '.rar', '.tar', '.gz', '.jpg', '.jpeg', '.png',
                          '.gif', '.bmp', '.svg', '.mp3', '.mp4', '.avi', '.mov',
                          '.css', '.js', '.ico', '.xml', '.rss'}
        
        parsed_url = urllib.parse.urlparse(url)
        path = parsed_url.path.lower()
        
        for ext in skip_extensions:
            if path.endswith(ext):
                return False
        
        return True
    
    async def _check_robots_txt(self, domain: str) -> Dict[str, Any]:
        """Check and parse robots.txt for the domain"""
        if domain in self.robots_cache:
            return self.robots_cache[domain]
        
        robots_url = f"https://{domain}/robots.txt"
        robots_info = {
            'allowed': True,
            'crawl_delay': 0,
            'disallowed_paths': []
        }
        
        try:
            async with self.session.get(robots_url, timeout=10) as response:
                if response.status == 200:
                    content = await response.text()
                    
                    # Simple robots.txt parser
                    user_agent_section = False
                    for line in content.split('\n'):
                        line = line.strip().lower()
                        
                        if line.startswith('user-agent:'):
                            agent = line.split(':', 1)[1].strip()
                            user_agent_section = agent in ['*', 'advanced web crawler']
                        
                        elif user_agent_section:
                            if line.startswith('disallow:'):
                                path = line.split(':', 1)[1].strip()
                                if path:
                                    robots_info['disallowed_paths'].append(path)
                            elif line.startswith('crawl-delay:'):
                                try:
                                    delay = float(line.split(':', 1)[1].strip())
                                    robots_info['crawl_delay'] = delay
                                except ValueError:
                                    pass
        
        except Exception as e:
            self.logger.debug(f"Could not fetch robots.txt for {domain}: {e}")
        
        self.robots_cache[domain] = robots_info
        return robots_info
    
    def _can_crawl_url(self, url: str, robots_info: Dict[str, Any]) -> bool:
        """Check if URL can be crawled according to robots.txt"""
        if not self.respect_robots_txt:
            return True
        
        parsed_url = urllib.parse.urlparse(url)
        path = parsed_url.path
        
        for disallowed_path in robots_info['disallowed_paths']:
            if path.startswith(disallowed_path):
                return False
        
        return True
    
    async def _extract_content(self, html: str, url: str) -> Tuple[str, str, List[str], List[str], List[str]]:
        """Extract content, title, links, and metadata from HTML"""
        soup = BeautifulSoup(html, 'html.parser')
        
        # Remove script and style elements
        for script in soup(["script", "style"]):
            script.decompose()
        
        # Extract title
        title_tag = soup.find('title')
        title = title_tag.get_text().strip() if title_tag else "No Title"
        
        # Extract meta description
        meta_desc = ""
        meta_desc_tag = soup.find('meta', attrs={'name': 'description'})
        if meta_desc_tag:
            meta_desc = meta_desc_tag.get('content', '').strip()
        
        # Extract keywords
        keywords = []
        meta_keywords_tag = soup.find('meta', attrs={'name': 'keywords'})
        if meta_keywords_tag:
            keywords_content = meta_keywords_tag.get('content', '')
            keywords = [k.strip() for k in keywords_content.split(',') if k.strip()]
        
        # Extract text content
        content = soup.get_text()
        content = re.sub(r'\s+', ' ', content).strip()
        
        # Extract links
        links = []
        for link in soup.find_all('a', href=True):
            href = link['href']
            normalized_href = self._normalize_url(href, url)
            if self._is_valid_url(normalized_href):
                links.append(normalized_href)
        
        # Extract images
        images = []
        for img in soup.find_all('img', src=True):
            src = img['src']
            normalized_src = self._normalize_url(src, url)
            images.append(normalized_src)
        
        return title, content, links, images, keywords, meta_desc
    
    async def _crawl_page(self, url: str, depth: int) -> Optional[WebPage]:
        """Crawl a single page"""
        async with self.semaphore:
            domain = self._extract_domain(url)
            
            # Check robots.txt
            if self.respect_robots_txt:
                robots_info = await self._check_robots_txt(domain)
                if not self._can_crawl_url(url, robots_info):
                    self.logger.info(f"Skipping {url} due to robots.txt")
                    return None
                
                # Respect crawl delay
                crawl_delay = max(robots_info['crawl_delay'], self.delay_between_requests)
            else:
                crawl_delay = self.delay_between_requests
            
            # Implement per-domain delay
            current_time = time.time()
            if domain in self.domain_delays:
                time_since_last = current_time - self.domain_delays[domain]
                if time_since_last < crawl_delay:
                    await asyncio.sleep(crawl_delay - time_since_last)
            
            self.domain_delays[domain] = time.time()
            
            try:
                start_time = time.time()
                
                async with self.session.get(url, timeout=30, headers=self.headers) as response:
                    response_time = time.time() - start_time
                    
                    if response.status != 200:
                        self.logger.warning(f"HTTP {response.status} for {url}")
                        with self.lock:
                            self.failed_urls.add(url)
                            self.stats.failed_crawls += 1
                        return None
                    
                    content_type = response.headers.get('content-type', '').lower()
                    if 'text/html' not in content_type:
                        self.logger.info(f"Skipping non-HTML content: {url}")
                        return None
                    
                    html = await response.text()
                    
                    # Extract content and metadata
                    title, text_content, links, images, keywords, meta_desc = await self._extract_content(html, url)
                    
                    # Create content hash for duplicate detection
                    content_hash = hashlib.md5(text_content.encode()).hexdigest()
                    
                    # Create WebPage object
                    page = WebPage(
                        url=url,
                        title=title,
                        content=text_content[:5000],  # Limit content length
                        links=links,
                        timestamp=datetime.now(),
                        response_time=response_time,
                        status_code=response.status,
                        content_hash=content_hash,
                        headers=dict(response.headers),
                        meta_description=meta_desc,
                        keywords=keywords[:10],  # Limit keywords
                        images=images[:20]  # Limit images
                    )
                    
                    # Store in database
                    await self._store_page(page, depth)
                    
                    # Add new links to queue
                    if depth < self.max_depth:
                        with self.lock:
                            for link in links[:50]:  # Limit links per page
                                if (link not in self.crawled_urls and 
                                    link not in self.failed_urls and
                                    len(self.crawled_urls) < self.max_pages):
                                    self.url_queue.append((link, depth + 1))
                    
                    with self.lock:
                        self.crawled_urls.add(url)
                        self.stats.successful_crawls += 1
                        self.stats.total_links_found += len(links)
                    
                    self.logger.info(f"Crawled: {url} ({len(text_content)} chars, {len(links)} links)")
                    return page
            
            except asyncio.TimeoutError:
                self.logger.error(f"Timeout crawling {url}")
            except Exception as e:
                self.logger.error(f"Error crawling {url}: {e}")
            
            with self.lock:
                self.failed_urls.add(url)
                self.stats.failed_crawls += 1
            
            return None
    
    async def _store_page(self, page: WebPage, depth: int):
        """Store crawled page in database"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        try:
            # Store main page data
            cursor.execute('''
                INSERT OR REPLACE INTO crawled_pages 
                (url, title, content, content_hash, timestamp, response_time, 
                 status_code, depth, domain, links_count, meta_description, keywords)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ''', (
                page.url,
                page.title,
                page.content,
                page.content_hash,
                page.timestamp.isoformat(),
                page.response_time,
                page.status_code,
                depth,
                self._extract_domain(page.url),
                len(page.links),
                page.meta_description,
                json.dumps(page.keywords)
            ))
            
            # Store links
            cursor.execute('DELETE FROM page_links WHERE source_url = ?', (page.url,))
            for link in page.links:
                cursor.execute('''
                    INSERT INTO page_links (source_url, target_url, link_text)
                    VALUES (?, ?, ?)
                ''', (page.url, link, ""))
            
            conn.commit()
            
        except Exception as e:
            self.logger.error(f"Database error storing {page.url}: {e}")
        finally:
            conn.close()
    
    async def crawl(self, start_urls: List[str]) -> CrawlStats:
        """Main crawling method"""
        self.stats.start_time = datetime.now()
        
        # Initialize session
        connector = aiohttp.TCPConnector(limit=100, limit_per_host=10)
        timeout = aiohttp.ClientTimeout(total=60)
        self.session = aiohttp.ClientSession(
            connector=connector,
            timeout=timeout,
            headers=self.headers
        )
        
        try:
            # Add start URLs to queue
            for url in start_urls:
                normalized_url = self._normalize_url(url)
                if self._is_valid_url(normalized_url):
                    self.url_queue.append((normalized_url, 0))
            
            self.logger.info(f"Starting crawl with {len(self.url_queue)} seed URLs")
            
            # Crawling loop
            tasks = []
            
            while (self.url_queue and 
                   len(self.crawled_urls) < self.max_pages):
                
                # Start new crawling tasks
                while (len(tasks) < self.max_concurrent and 
                       self.url_queue and 
                       len(self.crawled_urls) < self.max_pages):
                    
                    url, depth = self.url_queue.popleft()
                    if url not in self.crawled_urls and url not in self.failed_urls:
                        task = asyncio.create_task(self._crawl_page(url, depth))
                        tasks.append(task)
                
                # Wait for some tasks to complete
                if tasks:
                    done, tasks = await asyncio.wait(
                        tasks, 
                        timeout=1.0,
                        return_when=asyncio.FIRST_COMPLETED
                    )
                    
                    for task in done:
                        try:
                            await task
                        except Exception as e:
                            self.logger.error(f"Task error: {e}")
                
                # Progress update
                if len(self.crawled_urls) % 10 == 0:
                    self.logger.info(f"Progress: {len(self.crawled_urls)} pages crawled, "
                                   f"{len(self.url_queue)} URLs in queue")
            
            # Wait for remaining tasks
            if tasks:
                await asyncio.gather(*tasks, return_exceptions=True)
            
        finally:
            await self.session.close()
        
        self.stats.end_time = datetime.now()
        self.stats.total_pages = len(self.crawled_urls)
        self.stats.unique_domains = len(set(self._extract_domain(url) for url in self.crawled_urls))
        
        # Calculate average response time
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        cursor.execute('SELECT AVG(response_time) FROM crawled_pages')
        avg_time = cursor.fetchone()[0]
        self.stats.average_response_time = avg_time if avg_time else 0.0
        conn.close()
        
        self.logger.info(f"Crawling completed: {self.stats.successful_crawls} successful, "
                        f"{self.stats.failed_crawls} failed")
        
        return self.stats
    
    def analyze_crawled_data(self) -> Dict[str, Any]:
        """Analyze the crawled data and generate insights"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        analysis = {}
        
        # Basic statistics
        cursor.execute('SELECT COUNT(*) FROM crawled_pages')
        total_pages = cursor.fetchone()[0]
        analysis['total_pages'] = total_pages
        
        # Domain distribution
        cursor.execute('''
            SELECT domain, COUNT(*) as count 
            FROM crawled_pages 
            GROUP BY domain 
            ORDER BY count DESC 
            LIMIT 10
        ''')
        analysis['top_domains'] = cursor.fetchall()
        
        # Content length distribution
        cursor.execute('''
            SELECT AVG(LENGTH(content)), MIN(LENGTH(content)), MAX(LENGTH(content))
            FROM crawled_pages
        ''')
        avg_len, min_len, max_len = cursor.fetchone()
        analysis['content_length'] = {
            'average': avg_len or 0,
            'minimum': min_len or 0,
            'maximum': max_len or 0
        }
        
        # Response time statistics
        cursor.execute('''
            SELECT AVG(response_time), MIN(response_time), MAX(response_time)
            FROM crawled_pages
        ''')
        avg_time, min_time, max_time = cursor.fetchone()
        analysis['response_times'] = {
            'average': avg_time or 0,
            'minimum': min_time or 0,
            'maximum': max_time or 0
        }
        
        # Link analysis
        cursor.execute('''
            SELECT AVG(links_count), MAX(links_count)
            FROM crawled_pages
        ''')
        avg_links, max_links = cursor.fetchone()
        analysis['links'] = {
            'average_per_page': avg_links or 0,
            'maximum_per_page': max_links or 0,
            'total_found': self.stats.total_links_found
        }
        
        # Most common keywords
        cursor.execute('SELECT keywords FROM crawled_pages WHERE keywords IS NOT NULL')
        all_keywords = []
        for (keywords_json,) in cursor.fetchall():
            try:
                keywords = json.loads(keywords_json)
                all_keywords.extend(keywords)
            except:
                continue
        
        from collections import Counter
        keyword_counter = Counter(all_keywords)
        analysis['top_keywords'] = keyword_counter.most_common(20)
        
        # Duplicate content detection
        cursor.execute('''
            SELECT content_hash, COUNT(*) as count
            FROM crawled_pages
            GROUP BY content_hash
            HAVING count > 1
            ORDER BY count DESC
            LIMIT 10
        ''')
        analysis['duplicate_content'] = cursor.fetchall()
        
        conn.close()
        return analysis
    
    def export_data(self, format_type: str = 'json', filename: str = None) -> str:
        """Export crawled data to various formats"""
        if filename is None:
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            filename = f"crawled_data_{timestamp}.{format_type}"
        
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('''
            SELECT url, title, content, timestamp, response_time, 
                   status_code, domain, links_count, meta_description, keywords
            FROM crawled_pages
            ORDER BY timestamp
        ''')
        
        data = cursor.fetchall()
        columns = ['url', 'title', 'content', 'timestamp', 'response_time',
                  'status_code', 'domain', 'links_count', 'meta_description', 'keywords']
        
        if format_type.lower() == 'json':
            export_data = []
            for row in data:
                row_dict = dict(zip(columns, row))
                try:
                    row_dict['keywords'] = json.loads(row_dict['keywords']) if row_dict['keywords'] else []
                except:
                    row_dict['keywords'] = []
                export_data.append(row_dict)
            
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(export_data, f, indent=2, ensure_ascii=False, default=str)
        
        elif format_type.lower() == 'csv':
            with open(filename, 'w', newline='', encoding='utf-8') as f:
                writer = csv.writer(f)
                writer.writerow(columns)
                writer.writerows(data)
        
        else:
            raise ValueError("Unsupported format. Use 'json' or 'csv'.")
        
        conn.close()
        
        self.logger.info(f"Data exported to {filename}")
        return filename
    
    def generate_report(self, analysis: Dict[str, Any]) -> str:
        """Generate a comprehensive crawling report"""
        report = []
        report.append("="*80)
        report.append("WEB CRAWLER ANALYSIS REPORT")
        report.append("="*80)
        report.append("")
        
        # Crawl statistics
        report.append("CRAWL STATISTICS")
        report.append("-" * 40)
        duration = self.stats.end_time - self.stats.start_time if self.stats.end_time else timedelta(0)
        report.append(f"Total Pages Crawled: {analysis['total_pages']}")
        report.append(f"Successful Crawls: {self.stats.successful_crawls}")
        report.append(f"Failed Crawls: {self.stats.failed_crawls}")
        report.append(f"Unique Domains: {len(analysis['top_domains'])}")
        report.append(f"Total Links Found: {analysis['links']['total_found']}")
        report.append(f"Crawl Duration: {duration}")
        report.append(f"Average Response Time: {analysis['response_times']['average']:.3f}s")
        report.append("")
        
        # Top domains
        report.append("TOP DOMAINS")
        report.append("-" * 40)
        for domain, count in analysis['top_domains'][:10]:
            report.append(f"{domain:<40} {count:>6} pages")
        report.append("")
        
        # Content statistics
        report.append("CONTENT STATISTICS")
        report.append("-" * 40)
        report.append(f"Average Content Length: {analysis['content_length']['average']:.0f} chars")
        report.append(f"Minimum Content Length: {analysis['content_length']['minimum']} chars")
        report.append(f"Maximum Content Length: {analysis['content_length']['maximum']} chars")
        report.append(f"Average Links per Page: {analysis['links']['average_per_page']:.1f}")
        report.append(f"Maximum Links per Page: {analysis['links']['maximum_per_page']}")
        report.append("")
        
        # Top keywords
        if analysis['top_keywords']:
            report.append("TOP KEYWORDS")
            report.append("-" * 40)
            for keyword, count in analysis['top_keywords'][:15]:
                report.append(f"{keyword:<30} {count:>6} occurrences")
            report.append("")
        
        # Duplicate content
        if analysis['duplicate_content']:
            report.append("DUPLICATE CONTENT DETECTED")
            report.append("-" * 40)
            for content_hash, count in analysis['duplicate_content'][:10]:
                report.append(f"Hash {content_hash[:16]}... found in {count} pages")
            report.append("")
        
        report_text = "\n".join(report)
        
        # Save report to file
        report_filename = f"crawl_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        with open(report_filename, 'w', encoding='utf-8') as f:
            f.write(report_text)
        
        print(report_text)
        self.logger.info(f"Report saved to {report_filename}")
        
        return report_filename

async def demo_web_crawler():
    """Demonstrate the web crawler with sample websites"""
    print("Advanced Web Crawler Demo")
    print("=" * 40)
    
    # Initialize crawler
    crawler = AdvancedWebCrawler(
        max_concurrent=5,
        delay_between_requests=1.0,
        max_pages=20,
        max_depth=2,
        respect_robots_txt=True
    )
    
    # Sample URLs for demonstration (using well-known, crawl-friendly sites)
    start_urls = [
        "https://httpbin.org/",
        "https://jsonplaceholder.typicode.com/",
        "https://example.com/",
    ]
    
    # Start crawling
    print(f"Starting crawl with {len(start_urls)} seed URLs...")
    stats = await crawler.crawl(start_urls)
    
    # Analyze results
    print("\nAnalyzing crawled data...")
    analysis = crawler.analyze_crawled_data()
    
    # Generate report
    print("\nGenerating report...")
    report_file = crawler.generate_report(analysis)
    
    # Export data
    print("\nExporting data...")
    json_file = crawler.export_data('json')
    csv_file = crawler.export_data('csv')
    
    print(f"\nCrawling completed successfully!")
    print(f"Files generated:")
    print(f"- {report_file} (analysis report)")
    print(f"- {json_file} (JSON export)")
    print(f"- {csv_file} (CSV export)")
    print(f"- {crawler.db_path} (SQLite database)")
    
    return crawler, analysis

def main():
    """Main function to run the web crawler"""
    asyncio.run(demo_web_crawler())

if __name__ == "__main__":
    main()