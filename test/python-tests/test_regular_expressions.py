# Test regular expressions
import re

# Basic pattern matching
def test_basic_patterns():
    text = "Hello World 123 Python 456"

    # Find all digits
    digits = re.findall(r'\d+', text)
    print("Text:", text)
    print("Digits found:", digits)

    # Find all words
    words = re.findall(r'\w+', text)
    print("Words found:", words)

    # Find capital letters
    capitals = re.findall(r'[A-Z]', text)
    print("Capital letters:", capitals)

# Pattern matching with groups
def test_groups():
    text = "John: 30 years old, Jane: 25 years old, Bob: 35 years old"

    # Extract names and ages
    pattern = r'(\w+): (\d+) years old'
    matches = re.findall(pattern, text)

    print("Text:", text)
    print("Name-Age pairs:", matches)

    # Using finditer for more detailed info
    for match in re.finditer(pattern, text):
        print(f"Match: {match.group(1)} is {match.group(2)} years old")

# Email validation
def test_email_validation():
    emails = [
        "user@example.com",
        "john.doe@company.co.uk",
        "invalid-email",
        "another@domain",
        "valid.email123@sub.domain.com",
        "@invalid.com",
        "missing@dotcom"
    ]

    email_pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'

    print("Email validation:")
    for email in emails:
        is_valid = bool(re.match(email_pattern, email))
        print(f"{email}: {'Valid' if is_valid else 'Invalid'}")

# Phone number extraction
def test_phone_extraction():
    text = """
    Contact us at:
    Phone: 123-456-7890
    Mobile: (555) 123-4567
    International: +1-800-555-1234
    Invalid: 123456
    """

    # Different phone number patterns
    patterns = [
        r'\d{3}-\d{3}-\d{4}',  # 123-456-7890
        r'\(\d{3}\) \d{3}-\d{4}',  # (555) 123-4567
        r'\+\d{1,3}-\d{3}-\d{3}-\d{4}'  # +1-800-555-1234
    ]

    print("Phone number extraction:")
    for pattern in patterns:
        matches = re.findall(pattern, text)
        print(f"Pattern '{pattern}': {matches}")

# Text substitution
def test_substitution():
    text = "The quick brown fox jumps over the lazy dog."

    # Replace specific words
    new_text = re.sub(r'fox', 'cat', text)
    print("Original:", text)
    print("After substitution:", new_text)

    # Replace multiple spaces with single space
    messy_text = "This   has    multiple    spaces"
    cleaned = re.sub(r'\s+', ' ', messy_text)
    print(f"\nMessy: '{messy_text}'")
    print(f"Cleaned: '{cleaned}'")

    # Remove punctuation
    text_with_punct = "Hello, world! How are you?"
    no_punct = re.sub(r'[^\w\s]', '', text_with_punct)
    print(f"\nWith punctuation: '{text_with_punct}'")
    print(f"Without punctuation: '{no_punct}'")

# Pattern splitting
def test_splitting():
    text = "apple,banana;cherry orange|grape"

    # Split by multiple delimiters
    fruits = re.split(r'[,;| ]+', text)
    print("Text:", text)
    print("Split fruits:", fruits)

    # Split sentences
    sentences = "First sentence. Second sentence! Third sentence?"
    sentence_parts = re.split(r'[.!?]+', sentences)
    print("\nSentences:", sentence_parts)

# Word boundaries
def test_word_boundaries():
    text = "Python is a pythonic language. PYTHON programming is fun!"

    # Find exact word matches (case sensitive)
    python_words = re.findall(r'\bPython\b', text)
    print("Text:", text)
    print("Exact 'Python' matches:", python_words)

    # Case insensitive
    python_insensitive = re.findall(r'\bpython\b', text, re.IGNORECASE)
    print("Case insensitive 'python' matches:", python_insensitive)

# Lookahead and lookbehind
def test_lookahead_lookbehind():
    text = "apple pie, banana split, cherry tart, apple juice"

    # Find words followed by 'pie'
    followed_by_pie = re.findall(r'\w+(?= pie)', text)
    print("Text:", text)
    print("Words followed by 'pie':", followed_by_pie)

    # Find words preceded by 'apple'
    preceded_by_apple = re.findall(r'(?<=apple )\w+', text)
    print("Words preceded by 'apple':", preceded_by_apple)

# URL extraction
def test_url_extraction():
    text = """
    Visit https://www.example.com or http://sub.domain.com/path
    Check out www.another-site.com and ftp://files.server.org
    Not a URL: just.some.text
    """

    url_pattern = r'https?://[^\s<>"{}|\\^`\[\]]+|www\.[^\s<>"{}|\\^`\[\]]+'

    urls = re.findall(url_pattern, text)
    print("URL extraction:")
    print("Found URLs:", urls)

# Complex pattern - HTML tag extraction
def test_html_tags():
    html_text = """
    <div class="container">
        <h1>Title</h1>
        <p>Paragraph text</p>
        <a href="https://example.com">Link</a>
    </div>
    """

    # Extract tags
    tag_pattern = r'<([^>]+)>'
    tags = re.findall(tag_pattern, html_text)

    print("HTML tag extraction:")
    print("Found tags:", tags)

# Run all tests
print("=== Basic Patterns ===")
test_basic_patterns()

print("\n=== Groups ===")
test_groups()

print("\n=== Email Validation ===")
test_email_validation()

print("\n=== Phone Number Extraction ===")
test_phone_extraction()

print("\n=== Substitution ===")
test_substitution()

print("\n=== Splitting ===")
test_splitting()

print("\n=== Word Boundaries ===")
test_word_boundaries()

print("\n=== Lookahead and Lookbehind ===")
test_lookahead_lookbehind()

print("\n=== URL Extraction ===")
test_url_extraction()

print("\n=== HTML Tag Extraction ===")
test_html_tags()