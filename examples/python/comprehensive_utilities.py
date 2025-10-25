import re
import hashlib
import json
from datetime import datetime, timedelta
from collections import defaultdict, Counter
from typing import List, Dict, Any, Optional, Tuple

class TextAnalyzer:
    def __init__(self, text: str = ""):
        self.text = text
        self.words = self._extract_words()
        self.sentences = self._extract_sentences()
        self.paragraphs = self._extract_paragraphs()
    
    def _extract_words(self) -> List[str]:
        return re.findall(r'\b\w+\b', self.text.lower())
    
    def _extract_sentences(self) -> List[str]:
        sentences = re.split(r'[.!?]+', self.text)
        return [s.strip() for s in sentences if s.strip()]
    
    def _extract_paragraphs(self) -> List[str]:
        paragraphs = re.split(r'\n\s*\n', self.text)
        return [p.strip() for p in paragraphs if p.strip()]
    
    def word_count(self) -> int:
        return len(self.words)
    
    def unique_word_count(self) -> int:
        return len(set(self.words))
    
    def sentence_count(self) -> int:
        return len(self.sentences)
    
    def paragraph_count(self) -> int:
        return len(self.paragraphs)
    
    def average_words_per_sentence(self) -> float:
        if self.sentence_count() == 0:
            return 0.0
        return self.word_count() / self.sentence_count()
    
    def word_frequency(self) -> Dict[str, int]:
        return dict(Counter(self.words))
    
    def most_common_words(self, n: int = 10) -> List[Tuple[str, int]]:
        return Counter(self.words).most_common(n)
    
    def longest_word(self) -> str:
        return max(self.words, key=len) if self.words else ""
    
    def shortest_word(self) -> str:
        return min(self.words, key=len) if self.words else ""
    
    def average_word_length(self) -> float:
        if not self.words:
            return 0.0
        return sum(len(word) for word in self.words) / len(self.words)
    
    def reading_time(self, wpm: int = 200) -> float:
        return self.word_count() / wpm
    
    def lexical_diversity(self) -> float:
        if self.word_count() == 0:
            return 0.0
        return self.unique_word_count() / self.word_count()
    
    def sentiment_score(self) -> float:
        positive_words = {'good', 'great', 'excellent', 'amazing', 'wonderful', 'fantastic', 'awesome', 'brilliant', 'perfect', 'love', 'happy', 'joy', 'success', 'win', 'victory'}
        negative_words = {'bad', 'terrible', 'awful', 'horrible', 'hate', 'sad', 'angry', 'failure', 'lose', 'defeat', 'wrong', 'error', 'problem', 'issue', 'difficult'}
        
        positive_count = sum(1 for word in self.words if word in positive_words)
        negative_count = sum(1 for word in self.words if word in negative_words)
        
        total_sentiment_words = positive_count + negative_count
        if total_sentiment_words == 0:
            return 0.0
        
        return (positive_count - negative_count) / total_sentiment_words
    
    def generate_summary(self) -> Dict[str, Any]:
        return {
            'word_count': self.word_count(),
            'unique_words': self.unique_word_count(),
            'sentences': self.sentence_count(),
            'paragraphs': self.paragraph_count(),
            'avg_words_per_sentence': round(self.average_words_per_sentence(), 2),
            'avg_word_length': round(self.average_word_length(), 2),
            'reading_time_minutes': round(self.reading_time(), 2),
            'lexical_diversity': round(self.lexical_diversity(), 3),
            'sentiment_score': round(self.sentiment_score(), 3),
            'longest_word': self.longest_word(),
            'shortest_word': self.shortest_word(),
            'most_common_words': self.most_common_words(5)
        }

class DataProcessor:
    def __init__(self):
        self.data = []
        self.cache = {}
    
    def add_data(self, item: Any) -> None:
        self.data.append(item)
        self.cache.clear()
    
    def filter_data(self, condition) -> List[Any]:
        cache_key = f"filter_{id(condition)}"
        if cache_key not in self.cache:
            self.cache[cache_key] = [item for item in self.data if condition(item)]
        return self.cache[cache_key]
    
    def map_data(self, transform) -> List[Any]:
        cache_key = f"map_{id(transform)}"
        if cache_key not in self.cache:
            self.cache[cache_key] = [transform(item) for item in self.data]
        return self.cache[cache_key]
    
    def reduce_data(self, operation, initial=None):
        result = initial
        for item in self.data:
            if result is None:
                result = item
            else:
                result = operation(result, item)
        return result
    
    def group_by(self, key_func) -> Dict[Any, List[Any]]:
        cache_key = f"group_{id(key_func)}"
        if cache_key not in self.cache:
            groups = defaultdict(list)
            for item in self.data:
                groups[key_func(item)].append(item)
            self.cache[cache_key] = dict(groups)
        return self.cache[cache_key]
    
    def sort_data(self, key_func=None, reverse=False) -> List[Any]:
        cache_key = f"sort_{id(key_func)}_{reverse}"
        if cache_key not in self.cache:
            self.cache[cache_key] = sorted(self.data, key=key_func, reverse=reverse)
        return self.cache[cache_key]
    
    def statistics(self) -> Dict[str, Any]:
        if not self.data:
            return {}
        
        numeric_data = [item for item in self.data if isinstance(item, (int, float))]
        
        if not numeric_data:
            return {'count': len(self.data), 'type': 'non_numeric'}
        
        return {
            'count': len(numeric_data),
            'sum': sum(numeric_data),
            'mean': sum(numeric_data) / len(numeric_data),
            'min': min(numeric_data),
            'max': max(numeric_data),
            'median': self._median(numeric_data),
            'std_dev': self._std_deviation(numeric_data)
        }
    
    def _median(self, data: List[float]) -> float:
        sorted_data = sorted(data)
        n = len(sorted_data)
        if n % 2 == 0:
            return (sorted_data[n // 2 - 1] + sorted_data[n // 2]) / 2
        return sorted_data[n // 2]
    
    def _std_deviation(self, data: List[float]) -> float:
        mean = sum(data) / len(data)
        variance = sum((x - mean) ** 2 for x in data) / len(data)
        return variance ** 0.5

class Utilities:
    @staticmethod
    def generate_hash(data: str, algorithm: str = 'md5') -> str:
        hash_obj = hashlib.new(algorithm)
        hash_obj.update(data.encode('utf-8'))
        return hash_obj.hexdigest()
    
    @staticmethod
    def generate_password(length: int = 12, include_symbols: bool = True) -> str:
        import string
        import random
        
        chars = string.ascii_letters + string.digits
        if include_symbols:
            chars += "!@#$%^&*"
        
        return ''.join(random.choice(chars) for _ in range(length))
    
    @staticmethod
    def validate_email(email: str) -> bool:
        pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
        return re.match(pattern, email) is not None
    
    @staticmethod
    def format_bytes(bytes_value: int) -> str:
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if bytes_value < 1024.0:
                return f"{bytes_value:.2f} {unit}"
            bytes_value /= 1024.0
        return f"{bytes_value:.2f} PB"
    
    @staticmethod
    def time_ago(timestamp: datetime) -> str:
        now = datetime.now()
        diff = now - timestamp
        
        if diff.days > 0:
            return f"{diff.days} days ago"
        elif diff.seconds > 3600:
            hours = diff.seconds // 3600
            return f"{hours} hours ago"
        elif diff.seconds > 60:
            minutes = diff.seconds // 60
            return f"{minutes} minutes ago"
        else:
            return "Just now"
    
    @staticmethod
    def chunk_list(lst: List[Any], chunk_size: int) -> List[List[Any]]:
        return [lst[i:i + chunk_size] for i in range(0, len(lst), chunk_size)]
    
    @staticmethod
    def flatten_list(nested_list: List[Any]) -> List[Any]:
        result = []
        for item in nested_list:
            if isinstance(item, list):
                result.extend(Utilities.flatten_list(item))
            else:
                result.append(item)
        return result
    
    @staticmethod
    def deep_merge_dicts(dict1: Dict[str, Any], dict2: Dict[str, Any]) -> Dict[str, Any]:
        result = dict1.copy()
        for key, value in dict2.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = Utilities.deep_merge_dicts(result[key], value)
            else:
                result[key] = value
        return result

class Logger:
    def __init__(self, name: str = "default"):
        self.name = name
        self.logs = []
        self.levels = {'DEBUG': 0, 'INFO': 1, 'WARNING': 2, 'ERROR': 3, 'CRITICAL': 4}
        self.min_level = 1
    
    def _log(self, level: str, message: str) -> None:
        if self.levels.get(level, 0) >= self.min_level:
            timestamp = datetime.now().isoformat()
            log_entry = {
                'timestamp': timestamp,
                'level': level,
                'logger': self.name,
                'message': message
            }
            self.logs.append(log_entry)
            print(f"[{timestamp}] {level}: {message}")
    
    def debug(self, message: str) -> None:
        self._log('DEBUG', message)
    
    def info(self, message: str) -> None:
        self._log('INFO', message)
    
    def warning(self, message: str) -> None:
        self._log('WARNING', message)
    
    def error(self, message: str) -> None:
        self._log('ERROR', message)
    
    def critical(self, message: str) -> None:
        self._log('CRITICAL', message)
    
    def set_level(self, level: str) -> None:
        if level in self.levels:
            self.min_level = self.levels[level]
    
    def get_logs(self, level: Optional[str] = None) -> List[Dict[str, Any]]:
        if level:
            return [log for log in self.logs if log['level'] == level]
        return self.logs.copy()
    
    def clear_logs(self) -> None:
        self.logs.clear()
    
    def export_logs(self, format_type: str = 'json') -> str:
        if format_type == 'json':
            return json.dumps(self.logs, indent=2)
        elif format_type == 'csv':
            if not self.logs:
                return ""
            header = "timestamp,level,logger,message\n"
            rows = []
            for log in self.logs:
                row = f"{log['timestamp']},{log['level']},{log['logger']},\"{log['message']}\""
                rows.append(row)
            return header + "\n".join(rows)
        else:
            return "\n".join([f"[{log['timestamp']}] {log['level']}: {log['message']}" for log in self.logs])

def comprehensive_demo():
    print("=== Comprehensive Python Utilities Demo ===\n")
    
    sample_text = """
    This is a great example of text analysis. The program can analyze various aspects of text,
    including word frequency, reading time, and sentiment analysis. It's an amazing tool for
    understanding textual data!
    
    The text analyzer provides wonderful insights into the structure and content of documents.
    It can help identify patterns and extract meaningful information from large amounts of text.
    """
    
    print("1. Text Analysis:")
    analyzer = TextAnalyzer(sample_text)
    summary = analyzer.generate_summary()
    for key, value in summary.items():
        print(f"   {key}: {value}")
    
    print("\n2. Data Processing:")
    processor = DataProcessor()
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    for num in numbers:
        processor.add_data(num)
    
    even_numbers = processor.filter_data(lambda x: x % 2 == 0)
    squared_numbers = processor.map_data(lambda x: x ** 2)
    sum_result = processor.reduce_data(lambda a, b: a + b)
    stats = processor.statistics()
    
    print(f"   Original: {numbers}")
    print(f"   Even numbers: {even_numbers}")
    print(f"   Squared: {squared_numbers}")
    print(f"   Sum: {sum_result}")
    print(f"   Statistics: {stats}")
    
    print("\n3. Utility Functions:")
    print(f"   MD5 hash of 'hello': {Utilities.generate_hash('hello')}")
    print(f"   Random password: {Utilities.generate_password()}")
    print(f"   Email validation: {Utilities.validate_email('test@example.com')}")
    print(f"   Format bytes: {Utilities.format_bytes(1024 * 1024 * 5)}")
    
    test_time = datetime.now() - timedelta(hours=2, minutes=30)
    print(f"   Time ago: {Utilities.time_ago(test_time)}")
    
    test_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    chunked = Utilities.chunk_list(test_list, 3)
    print(f"   Chunked list: {chunked}")
    
    nested = [[1, 2], [3, [4, 5]], 6]
    flattened = Utilities.flatten_list(nested)
    print(f"   Flattened: {flattened}")
    
    print("\n4. Logging System:")
    logger = Logger("demo_logger")
    logger.info("Application started")
    logger.warning("This is a warning message")
    logger.error("An error occurred")
    logger.debug("Debug information (may not show)")
    
    print(f"   Total logs: {len(logger.get_logs())}")
    print(f"   Error logs: {len(logger.get_logs('ERROR'))}")

if __name__ == "__main__":
    comprehensive_demo()