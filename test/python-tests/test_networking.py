# Test basic networking and web operations
import urllib.request
import urllib.parse
import urllib.error
import json
import socket
import http.client
import time
from urllib.request import urlopen
from urllib.parse import urlparse

# URL parsing
def test_url_parsing():
    urls = [
        "https://www.example.com/path/to/page?param1=value1&param2=value2#fragment",
        "http://subdomain.domain.com:8080/api/v1/users",
        "ftp://files.server.org/download/file.txt",
        "https://example.com",
        "mailto:user@example.com"
    ]

    print("URL parsing:")
    for url in urls:
        parsed = urlparse(url)
        print(f"\nURL: {url}")
        print(f"  Scheme: {parsed.scheme}")
        print(f"  Netloc: {parsed.netloc}")
        print(f"  Path: {parsed.path}")
        print(f"  Params: {parsed.params}")
        print(f"  Query: {parsed.query}")
        print(f"  Fragment: {parsed.fragment}")

# HTTP GET request
def test_http_get():
    url = "https://httpbin.org/get"
    params = {'key1': 'value1', 'key2': 'value2'}

    try:
        # Add parameters to URL
        encoded_params = urllib.parse.urlencode(params)
        full_url = f"{url}?{encoded_params}"

        print(f"Making GET request to: {full_url}")

        # Make request
        with urllib.request.urlopen(full_url, timeout=10) as response:
            content = response.read().decode('utf-8')
            data = json.loads(content)

            print("Response status:", response.status)
            print("Response headers:", dict(response.headers))
            print("Response data:", json.dumps(data, indent=2))

    except urllib.error.URLError as e:
        print(f"URL Error: {e}")
    except Exception as e:
        print(f"Error: {e}")

# HTTP POST request
def test_http_post():
    url = "https://httpbin.org/post"
    data = {
        'name': 'John Doe',
        'email': 'john@example.com',
        'message': 'Hello, World!'
    }

    try:
        # Encode data
        encoded_data = urllib.parse.urlencode(data).encode('utf-8')

        # Create request
        req = urllib.request.Request(url, data=encoded_data, method='POST')
        req.add_header('Content-Type', 'application/x-www-form-urlencoded')

        print(f"Making POST request to: {url}")

        # Make request
        with urllib.request.urlopen(req, timeout=10) as response:
            content = response.read().decode('utf-8')
            response_data = json.loads(content)

            print("Response status:", response.status)
            print("Response data:", json.dumps(response_data, indent=2))

    except urllib.error.URLError as e:
        print(f"URL Error: {e}")
    except Exception as e:
        print(f"Error: {e}")

# JSON API request
def test_json_api():
    url = "https://jsonplaceholder.typicode.com/posts/1"

    try:
        print(f"Making JSON request to: {url}")

        # Make request
        with urllib.request.urlopen(url, timeout=10) as response:
            content = response.read().decode('utf-8')
            data = json.loads(content)

            print("Response status:", response.status)
            print("Content-Type:", response.getheader('Content-Type'))
            print("Post data:")
            print(f"  ID: {data['id']}")
            print(f"  Title: {data['title']}")
            print(f"  Body: {data['body'][:50]}...")

    except urllib.error.URLError as e:
        print(f"URL Error: {e}")
    except Exception as e:
        print(f"Error: {e}")

# Basic socket operations
def test_socket_operations():
    host = "www.example.com"
    port = 80

    print(f"Testing socket operations for {host}:{port}")

    try:
        # Get IP address
        ip = socket.gethostbyname(host)
        print(f"IP address of {host}: {ip}")

        # Create socket
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5)

        # Connect
        sock.connect((host, port))
        print(f"Connected to {host}:{port}")

        # Send HTTP request
        request = f"GET / HTTP/1.1\r\nHost: {host}\r\nConnection: close\r\n\r\n"
        sock.send(request.encode())

        # Receive response
        response = sock.recv(4096).decode('utf-8')
        print("\nHTTP Response (first few lines):")
        for line in response.split('\r\n')[:10]:
            if line.strip():
                print(f"  {line}")

        sock.close()

    except socket.timeout:
        print("Connection timed out")
    except socket.error as e:
        print(f"Socket error: {e}")
    except Exception as e:
        print(f"Error: {e}")

# URL encoding and decoding
def test_url_encoding():
    original_url = "https://example.com/search?q=Python programming&category=web development&page=1"

    # Parse URL
    parsed = urlparse(original_url)
    query_params = urllib.parse.parse_qs(parsed.query)

    print("Original URL:", original_url)
    print("Parsed query parameters:", query_params)

    # Modify parameters
    query_params['q'] = ['Data Science']
    query_params['sort'] = ['relevance']

    # Rebuild URL
    new_query = urllib.parse.urlencode(query_params, doseq=True)
    new_url = f"{parsed.scheme}://{parsed.netloc}{parsed.path}?{new_query}"

    print("\nNew URL:", new_url)

    # URL encode individual components
    text = "Hello World! Programming & Coding"
    encoded_text = urllib.parse.quote(text)
    print(f"\nOriginal text: {text}")
    print(f"URL encoded: {encoded_text}")

    # URL decode
    decoded_text = urllib.parse.unquote(encoded_text)
    print(f"URL decoded: {decoded_text}")

# HTTP headers manipulation
def test_http_headers():
    url = "https://httpbin.org/headers"

    try:
        # Create request with custom headers
        req = urllib.request.Request(url)
        req.add_header('User-Agent', 'Mozilla/5.0 (Test Script)')
        req.add_header('Accept', 'application/json')
        req.add_header('X-Custom-Header', 'TestValue')

        print(f"Making request with custom headers to: {url}")

        with urllib.request.urlopen(req, timeout=10) as response:
            content = response.read().decode('utf-8')
            data = json.loads(content)

            print("Response status:", response.status)
            print("Response headers from server:", json.dumps(data['headers'], indent=2))

    except urllib.error.URLError as e:
        print(f"URL Error: {e}")
    except Exception as e:
        print(f"Error: {e}")

# Error handling in networking
def test_error_handling():
    urls = [
        "https://www.example.com",  # Valid
        "https://this-domain-does-not-exist.com",  # Invalid domain
        "https://www.example.com/nonexistent-page",  # 404
        "http://localhost:9999",  # Connection refused
    ]

    print("Testing error handling:")

    for url in urls:
        try:
            print(f"\nTrying: {url}")
            response = urllib.request.urlopen(url, timeout=5)
            print(f"  Success: {response.status}")

        except urllib.error.HTTPError as e:
            print(f"  HTTP Error {e.code}: {e.reason}")
        except urllib.error.URLError as e:
            print(f"  URL Error: {e.reason}")
        except Exception as e:
            print(f"  Other error: {e}")

# Run all tests
print("=== URL Parsing ===")
test_url_parsing()

print("\n=== HTTP GET Request ===")
test_http_get()

print("\n=== HTTP POST Request ===")
test_http_post()

print("\n=== JSON API Request ===")
test_json_api()

print("\n=== Socket Operations ===")
test_socket_operations()

print("\n=== URL Encoding/Decoding ===")
test_url_encoding()

print("\n=== HTTP Headers ===")
test_http_headers()

print("\n=== Error Handling ===")
test_error_handling()