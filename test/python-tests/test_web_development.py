#!/usr/bin/env python3
"""
测试Python的Web开发相关特性：HTTP服务器、REST API等
"""

import http.server
import socketserver
import json
import urllib.request
import urllib.parse
import urllib.error
import threading
import time
import tempfile
import os
from urllib.error import URLError, HTTPError

class SimpleRESTHandler(http.server.SimpleHTTPRequestHandler):
    """简单的REST API处理器"""
    
    # 内存中的数据存储
    data_store = {
        1: {"id": 1, "name": "Alice", "age": 30},
        2: {"id": 2, "name": "Bob", "age": 25},
        3: {"id": 3, "name": "Charlie", "age": 35}
    }
    next_id = 4
    
    def do_GET(self):
        """处理GET请求"""
        if self.path == '/api/users':
            # 获取所有用户
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps(list(self.data_store.values())).encode())
        
        elif self.path.startswith('/api/users/'):
            # 获取特定用户
            try:
                user_id = int(self.path.split('/')[-1])
                if user_id in self.data_store:
                    self.send_response(200)
                    self.send_header('Content-type', 'application/json')
                    self.end_headers()
                    self.wfile.write(json.dumps(self.data_store[user_id]).encode())
                else:
                    self.send_error(404, "User not found")
            except ValueError:
                self.send_error(400, "Invalid user ID")
        
        else:
            self.send_error(404, "Endpoint not found")
    
    def do_POST(self):
        """处理POST请求"""
        if self.path == '/api/users':
            try:
                content_length = int(self.headers['Content-Length'])
                post_data = self.rfile.read(content_length)
                user_data = json.loads(post_data.decode())
                
                # 创建新用户
                new_user = {
                    "id": self.next_id,
                    "name": user_data.get("name", ""),
                    "age": user_data.get("age", 0)
                }
                
                self.data_store[self.next_id] = new_user
                self.next_id += 1
                
                self.send_response(201)
                self.send_header('Content-type', 'application/json')
                self.end_headers()
                self.wfile.write(json.dumps(new_user).encode())
                
            except (json.JSONDecodeError, KeyError):
                self.send_error(400, "Invalid JSON data")
        else:
            self.send_error(404, "Endpoint not found")
    
    def do_PUT(self):
        """处理PUT请求"""
        if self.path.startswith('/api/users/'):
            try:
                user_id = int(self.path.split('/')[-1])
                if user_id in self.data_store:
                    content_length = int(self.headers['Content-Length'])
                    put_data = self.rfile.read(content_length)
                    user_data = json.loads(put_data.decode())
                    
                    # 更新用户数据
                    self.data_store[user_id].update({
                        "name": user_data.get("name", self.data_store[user_id]["name"]),
                        "age": user_data.get("age", self.data_store[user_id]["age"])
                    })
                    
                    self.send_response(200)
                    self.send_header('Content-type', 'application/json')
                    self.end_headers()
                    self.wfile.write(json.dumps(self.data_store[user_id]).encode())
                else:
                    self.send_error(404, "User not found")
            except (ValueError, json.JSONDecodeError, KeyError):
                self.send_error(400, "Invalid request")
        else:
            self.send_error(404, "Endpoint not found")
    
    def do_DELETE(self):
        """处理DELETE请求"""
        if self.path.startswith('/api/users/'):
            try:
                user_id = int(self.path.split('/')[-1])
                if user_id in self.data_store:
                    deleted_user = self.data_store.pop(user_id)
                    
                    self.send_response(200)
                    self.send_header('Content-type', 'application/json')
                    self.end_headers()
                    self.wfile.write(json.dumps({"message": "User deleted", "user": deleted_user}).encode())
                else:
                    self.send_error(404, "User not found")
            except ValueError:
                self.send_error(400, "Invalid user ID")
        else:
            self.send_error(404, "Endpoint not found")

def test_simple_http_server():
    """测试简单的HTTP服务器"""
    print("=== Testing Simple HTTP Server ===")
    
    # 创建临时HTML文件
    html_content = """
    <!DOCTYPE html>
    <html>
    <head>
        <title>Test Page</title>
    </head>
    <body>
        <h1>Hello from Python HTTP Server!</h1>
        <p>This is a test page served by Python's built-in HTTP server.</p>
    </body>
    </html>
    """
    
    with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.html') as f:
        html_path = f.name
        f.write(html_content)
    
    # 获取文件所在目录
    serve_directory = os.path.dirname(html_path)
    filename = os.path.basename(html_path)
    
    try:
        # 启动HTTP服务器（在单独的线程中）
        port = 8080
        handler = http.server.SimpleHTTPRequestHandler
        
        def run_server():
            os.chdir(serve_directory)  # 切换到服务目录
            with socketserver.TCPServer(("", port), handler) as httpd:
                print(f"HTTP server running on port {port}")
                httpd.serve_forever()
        
        # 在后台线程启动服务器
        server_thread = threading.Thread(target=run_server, daemon=True)
        server_thread.start()
        
        # 等待服务器启动
        time.sleep(1)
        
        # 测试HTTP请求
        try:
            url = f"http://localhost:{port}/{filename}"
            print(f"Fetching: {url}")
            
            with urllib.request.urlopen(url) as response:
                content = response.read().decode('utf-8')
                print(f"Response status: {response.status}")
                print(f"Content length: {len(content)} bytes")
                print("First 200 characters of content:")
                print(content[:200])
                
        except URLError as e:
            print(f"Failed to fetch from HTTP server: {e}")
    
    finally:
        # 清理临时文件
        if os.path.exists(html_path):
            os.unlink(html_path)

def test_rest_api_server():
    """测试REST API服务器"""
    print("\n=== Testing REST API Server ===")
    
    # 启动REST API服务器
    port = 8081
    
    def run_rest_server():
        with socketserver.TCPServer(("", port), SimpleRESTHandler) as httpd:
            print(f"REST API server running on port {port}")
            httpd.serve_forever()
    
    # 在后台线程启动服务器
    server_thread = threading.Thread(target=run_rest_server, daemon=True)
    server_thread.start()
    
    # 等待服务器启动
    time.sleep(1)
    
    base_url = f"http://localhost:{port}/api"
    
    try:
        # 测试GET所有用户
        print("\n--- Testing GET /api/users ---")
        try:
            with urllib.request.urlopen(f"{base_url}/users") as response:
                users = json.loads(response.read().decode())
                print(f"Found {len(users)} users:")
                for user in users:
                    print(f"  ID: {user['id']}, Name: {user['name']}, Age: {user['age']}")
        except URLError as e:
            print(f"GET request failed: {e}")
        
        # 测试GET特定用户
        print("\n--- Testing GET /api/users/1 ---")
        try:
            with urllib.request.urlopen(f"{base_url}/users/1") as response:
                user = json.loads(response.read().decode())
                print(f"User 1: {user}")
        except URLError as e:
            print(f"GET specific user failed: {e}")
        
        # 测试POST创建新用户
        print("\n--- Testing POST /api/users ---")
        new_user_data = json.dumps({"name": "Eve", "age": 28}).encode('utf-8')
        request = urllib.request.Request(
            f"{base_url}/users",
            data=new_user_data,
            headers={'Content-Type': 'application/json'},
            method='POST'
        )
        
        try:
            with urllib.request.urlopen(request) as response:
                created_user = json.loads(response.read().decode())
                print(f"Created new user: {created_user}")
        except URLError as e:
            print(f"POST request failed: {e}")
        
        # 测试PUT更新用户
        print("\n--- Testing PUT /api/users/1 ---")
        update_data = json.dumps({"name": "Alice Updated", "age": 31}).encode('utf-8')
        request = urllib.request.Request(
            f"{base_url}/users/1",
            data=update_data,
            headers={'Content-Type': 'application/json'},
            method='PUT'
        )
        
        try:
            with urllib.request.urlopen(request) as response:
                updated_user = json.loads(response.read().decode())
                print(f"Updated user: {updated_user}")
        except URLError as e:
            print(f"PUT request failed: {e}")
        
        # 测试DELETE删除用户
        print("\n--- Testing DELETE /api/users/2 ---")
        request = urllib.request.Request(
            f"{base_url}/users/2",
            method='DELETE'
        )
        
        try:
            with urllib.request.urlopen(request) as response:
                delete_result = json.loads(response.read().decode())
                print(f"Delete result: {delete_result}")
        except URLError as e:
            print(f"DELETE request failed: {e}")
        
        # 验证最终状态
        print("\n--- Final user list ---")
        try:
            with urllib.request.urlopen(f"{base_url}/users") as response:
                final_users = json.loads(response.read().decode())
                print(f"Final users ({len(final_users)} total):")
                for user in final_users:
                    print(f"  ID: {user['id']}, Name: {user['name']}, Age: {user['age']}")
        except URLError as e:
            print(f"Final GET request failed: {e}")
    
    except Exception as e:
        print(f"REST API test failed: {e}")

def test_urllib_operations():
    """测试urllib的各种操作"""
    print("\n=== Testing urllib Operations ===")
    
    # 测试GET请求到公共API
    test_urls = [
        "https://httpbin.org/get",
        "https://jsonplaceholder.typicode.com/posts/1",
    ]
    
    for url in test_urls:
        print(f"\n--- Testing GET {url} ---")
        try:
            with urllib.request.urlopen(url) as response:
                content = response.read().decode('utf-8')
                headers = dict(response.headers)
                
                print(f"Status: {response.status}")
                print(f"Content-Type: {headers.get('content-type', 'unknown')}")
                print(f"Content length: {len(content)} bytes")
                
                # 尝试解析JSON
                try:
                    json_data = json.loads(content)
                    print("JSON response preview:")
                    print(json.dumps(json_data, indent=2)[:500] + "..." if len(content) > 500 else json.dumps(json_data, indent=2))
                except json.JSONDecodeError:
                    print("Non-JSON response:")
                    print(content[:200] + "..." if len(content) > 200 else content)
        
        except HTTPError as e:
            print(f"HTTP Error: {e.code} - {e.reason}")
        except URLError as e:
            print(f"URL Error: {e.reason}")
        except Exception as e:
            print(f"Unexpected error: {e}")

def test_url_manipulation():
    """测试URL解析和构造"""
    print("\n=== Testing URL Manipulation ===")
    
    # 解析URL
    test_url = "https://example.com:8080/path/to/resource?param1=value1&param2=value2#fragment"
    parsed = urllib.parse.urlparse(test_url)
    
    print(f"Original URL: {test_url}")
    print(f"Scheme: {parsed.scheme}")
    print(f"Netloc: {parsed.netloc}")
    print(f"Path: {parsed.path}")
    print(f"Params: {parsed.params}")
    print(f"Query: {parsed.query}")
    print(f"Fragment: {parsed.fragment}")
    
    # 解析查询参数
    query_params = urllib.parse.parse_qs(parsed.query)
    print(f"Parsed query parameters: {query_params}")
    
    # 构造URL
    base_url = "https://api.example.com"
    path = "/users/123"
    params = {"format": "json", "fields": "name,email", "limit": "10"}
    
    constructed_url = urllib.parse.urljoin(base_url, path) + "?" + urllib.parse.urlencode(params)
    print(f"\nConstructed URL: {constructed_url}")

if __name__ == "__main__":
    print("Python Web Development Testing")
    print("=" * 50)
    
    test_simple_http_server()
    test_rest_api_server()
    test_urllib_operations()
    test_url_manipulation()
    
    print("\n=== All web development tests completed ===")
    print("\nNote: HTTP servers were started in daemon threads.")
    print("Real-world applications would use proper web frameworks like Flask, Django, or FastAPI.")