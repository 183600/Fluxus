#!/usr/bin/env python3
"""
Advanced Text Analysis and Processing Tools
"""

import re
import collections
from typing import Dict, List, Tuple, Set
import string


class TextAnalyzer:
    """Comprehensive text analysis and processing toolkit."""
    
    def __init__(self):
        self.stop_words = {
            'the', 'a', 'an', 'and', 'or', 'but', 'in', 'on', 'at', 'to', 'for',
            'of', 'with', 'by', 'from', 'up', 'about', 'into', 'through', 'during',
            'before', 'after', 'above', 'below', 'between', 'among', 'is', 'are',
            'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'do', 'does',
            'did', 'will', 'would', 'should', 'could', 'can', 'may', 'might', 'must',
            'this', 'that', 'these', 'those', 'i', 'you', 'he', 'she', 'it', 'we',
            'they', 'them', 'their', 'what', 'which', 'who', 'when', 'where', 'why',
            'how', 'not', 'no', 'yes'
        }
    
    def clean_text(self, text: str) -> str:
        """Clean text by removing extra whitespace and normalizing."""
        # Remove extra whitespace
        text = ' '.join(text.split())
        # Remove leading/trailing whitespace
        return text.strip()
    
    def word_count(self, text: str) -> int:
        """Count words in text."""
        words = text.split()
        return len(words)
    
    def character_count(self, text: str, include_spaces: bool = True) -> int:
        """Count characters in text."""
        if include_spaces:
            return len(text)
        else:
            return len(text.replace(' ', ''))
    
    def sentence_count(self, text: str) -> int:
        """Count sentences in text."""
        # Simple sentence detection based on punctuation
        sentences = re.split(r'[.!?]+', text)
        # Filter out empty strings
        sentences = [s for s in sentences if s.strip()]
        return len(sentences)
    
    def paragraph_count(self, text: str) -> int:
        """Count paragraphs in text."""
        paragraphs = text.split('\n\n')
        # Filter out empty paragraphs
        paragraphs = [p for p in paragraphs if p.strip()]
        return len(paragraphs)
    
    def get_word_frequency(self, text: str, case_sensitive: bool = False) -> Dict[str, int]:
        """Get word frequency distribution."""
        if not case_sensitive:
            text = text.lower()
        
        # Remove punctuation and split into words
        words = re.findall(r'\b\w+\b', text)
        return collections.Counter(words)
    
    def get_character_frequency(self, text: str, case_sensitive: bool = False) -> Dict[str, int]:
        """Get character frequency distribution."""
        if not case_sensitive:
            text = text.lower()
        
        # Only count letters
        chars = [c for c in text if c.isalpha()]
        return collections.Counter(chars)
    
    def find_most_common_words(self, text: str, n: int = 10, exclude_stop_words: bool = True) -> List[Tuple[str, int]]:
        """Find the most common words in text."""
        word_freq = self.get_word_frequency(text, case_sensitive=False)
        
        if exclude_stop_words:
            # Remove stop words
            word_freq = {word: count for word, count in word_freq.items() 
                        if word.lower() not in self.stop_words}
        
        return word_freq.most_common(n)
    
    def find_longest_words(self, text: str, n: int = 5) -> List[str]:
        """Find the longest words in text."""
        words = re.findall(r'\b\w+\b', text.lower())
        # Remove duplicates while preserving order
        unique_words = list(dict.fromkeys(words))
        # Sort by length (descending) then alphabetically
        sorted_words = sorted(unique_words, key=lambda x: (-len(x), x))
        return sorted_words[:n]
    
    def reading_level_metrics(self, text: str) -> Dict[str, float]:
        """Calculate basic reading level metrics."""
        word_count = self.word_count(text)
        sentence_count = self.sentence_count(text)
        char_count = self.character_count(text, include_spaces=False)
        
        if sentence_count == 0 or word_count == 0:
            return {
                'avg_words_per_sentence': 0,
                'avg_chars_per_word': 0,
                'flesch_reading_ease': 0
            }
        
        avg_words_per_sentence = word_count / sentence_count
        avg_chars_per_word = char_count / word_count
        
        # Simplified Flesch Reading Ease score
        # Formula: 206.835 - (1.015 × ASL) - (84.6 × ASW)
        # ASL = average sentence length, ASW = average syllables per word
        # Using character count as syllable approximation
        avg_syllables_per_word = avg_chars_per_word / 2  # Rough approximation
        flesch_score = 206.835 - (1.015 * avg_words_per_sentence) - (84.6 * avg_syllables_per_word)
        
        return {
            'avg_words_per_sentence': round(avg_words_per_sentence, 2),
            'avg_chars_per_word': round(avg_chars_per_word, 2),
            'flesch_reading_ease': round(max(0, min(100, flesch_score)), 2)
        }
    
    def find_repeated_phrases(self, text: str, min_length: int = 2, max_length: int = 5) -> Dict[str, int]:
        """Find repeated phrases in text."""
        words = re.findall(r'\b\w+\b', text.lower())
        phrases = {}
        
        for length in range(min_length, max_length + 1):
            for i in range(len(words) - length + 1):
                phrase = ' '.join(words[i:i + length])
                phrases[phrase] = phrases.get(phrase, 0) + 1
        
        # Filter out phrases that appear only once
        repeated_phrases = {phrase: count for phrase, count in phrases.items() if count > 1}
        return dict(sorted(repeated_phrases.items(), key=lambda x: x[1], reverse=True))
    
    def sentiment_analysis_simple(self, text: str) -> Dict[str, any]:
        """Simple sentiment analysis based on word lists."""
        positive_words = {
            'good', 'great', 'excellent', 'amazing', 'wonderful', 'fantastic',
            'awesome', 'brilliant', 'outstanding', 'superb', 'perfect', 'love',
            'like', 'enjoy', 'happy', 'pleased', 'satisfied', 'delighted',
            'thrilled', 'excited', 'beautiful', 'nice', 'pleasant'
        }
        
        negative_words = {
            'bad', 'terrible', 'awful', 'horrible', 'disgusting', 'hate',
            'dislike', 'angry', 'frustrated', 'disappointed', 'sad', 'depressed',
            'ugly', 'nasty', 'annoying', 'boring', 'stupid', 'wrong', 'fail',
            'failure', 'problem', 'issue', 'difficult', 'hard'
        }
        
        words = re.findall(r'\b\w+\b', text.lower())
        
        positive_count = sum(1 for word in words if word in positive_words)
        negative_count = sum(1 for word in words if word in negative_words)
        total_sentiment_words = positive_count + negative_count
        
        if total_sentiment_words == 0:
            sentiment = 'neutral'
            confidence = 0.0
        else:
            sentiment_score = (positive_count - negative_count) / len(words)
            if sentiment_score > 0.05:
                sentiment = 'positive'
            elif sentiment_score < -0.05:
                sentiment = 'negative'
            else:
                sentiment = 'neutral'
            
            confidence = total_sentiment_words / len(words) if words else 0
        
        return {
            'sentiment': sentiment,
            'positive_words': positive_count,
            'negative_words': negative_count,
            'confidence': round(confidence, 3),
            'total_words': len(words)
        }
    
    def extract_keywords(self, text: str, n: int = 10) -> List[str]:
        """Extract keywords using simple frequency-based approach."""
        # Get word frequency excluding stop words
        common_words = self.find_most_common_words(text, n * 2, exclude_stop_words=True)
        
        # Filter by word length and frequency
        keywords = []
        for word, freq in common_words:
            if len(word) >= 3 and freq >= 2:  # Minimum length and frequency
                keywords.append(word)
            if len(keywords) >= n:
                break
        
        return keywords


def demonstrate_text_analyzer():
    """Demonstrate text analyzer functionality."""
    analyzer = TextAnalyzer()
    
    sample_text = """
    The quick brown fox jumps over the lazy dog. This is a sample text for demonstration purposes.
    Text analysis is a fascinating field that combines linguistics, computer science, and statistics.
    Natural language processing has many applications in modern technology.
    
    We can analyze various aspects of text including word frequency, sentiment, readability, and more.
    The quick brown fox appears again in this text to demonstrate repeated phrase detection.
    This technology is used in search engines, social media analysis, and content recommendation systems.
    """
    
    print("=== Text Analysis Demo ===")
    print(f"Sample text: {sample_text[:100]}...")
    
    # Basic statistics
    print("\n--- Basic Statistics ---")
    print(f"Word count: {analyzer.word_count(sample_text)}")
    print(f"Character count (with spaces): {analyzer.character_count(sample_text, True)}")
    print(f"Character count (without spaces): {analyzer.character_count(sample_text, False)}")
    print(f"Sentence count: {analyzer.sentence_count(sample_text)}")
    print(f"Paragraph count: {analyzer.paragraph_count(sample_text)}")
    
    # Word frequency
    print("\n--- Most Common Words ---")
    common_words = analyzer.find_most_common_words(sample_text, 5, exclude_stop_words=True)
    for i, (word, count) in enumerate(common_words, 1):
        print(f"{i}. {word}: {count}")
    
    # Longest words
    print("\n--- Longest Words ---")
    longest_words = analyzer.find_longest_words(sample_text, 5)
    for i, word in enumerate(longest_words, 1):
        print(f"{i}. {word} ({len(word)} characters)")
    
    # Reading metrics
    print("\n--- Reading Level Metrics ---")
    metrics = analyzer.reading_level_metrics(sample_text)
    for metric, value in metrics.items():
        print(f"{metric.replace('_', ' ').title()}: {value}")
    
    # Repeated phrases
    print("\n--- Repeated Phrases ---")
    repeated = analyzer.find_repeated_phrases(sample_text, 2, 4)
    for phrase, count in list(repeated.items())[:5]:
        print(f"'{phrase}': {count} times")
    
    # Sentiment analysis
    print("\n--- Sentiment Analysis ---")
    sentiment = analyzer.sentiment_analysis_simple(sample_text)
    print(f"Overall sentiment: {sentiment['sentiment']}")
    print(f"Positive words: {sentiment['positive_words']}")
    print(f"Negative words: {sentiment['negative_words']}")
    print(f"Confidence: {sentiment['confidence']}")
    
    # Keywords
    print("\n--- Keywords ---")
    keywords = analyzer.extract_keywords(sample_text, 8)
    print(f"Extracted keywords: {', '.join(keywords)}")
    
    # Character frequency
    print("\n--- Character Frequency (Top 10) ---")
    char_freq = analyzer.get_character_frequency(sample_text, case_sensitive=False)
    for char, count in char_freq.most_common(10):
        print(f"'{char}': {count}")


if __name__ == "__main__":
    demonstrate_text_analyzer()