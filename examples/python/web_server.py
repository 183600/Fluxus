#!/usr/bin/env python3
"""
Advanced Web Server with FastAPI-like functionality
Implements a comprehensive web framework with routing, middleware, and templating
"""

import json
import socket
import threading
import urllib.parse
from datetime import datetime
from typing import Dict, List, Callable, Any, Optional
import re
import html


class HTTPRequest:
    def __init__(self, raw_request: str):
        lines = raw_request.split('\r\n')
        request_line = lines[0].split()
        
        self.method = request_line[0]
        self.path = request_line[1]
        self.version = request_line[2]
        
        # Parse headers
        self.headers = {}
        i = 1
        while i < len(lines) and lines[i]:
            if ':' in lines[i]:
                key, value = lines[i].split(':', 1)
                self.headers[key.strip().lower()] = value.strip()
            i += 1
        
        # Parse body
        self.body = '\r\n'.join(lines[i+1:]) if i+1 < len(lines) else ''
        
        # Parse query parameters
        if '?' in self.path:
            self.path, query_string = self.path.split('?', 1)
            self.query_params = urllib.parse.parse_qs(query_string)
        else:
            self.query_params = {}
            
        # Parse form data
        self.form_data = {}
        if self.method == 'POST' and 'application/x-www-form-urlencoded' in self.headers.get('content-type', ''):
            self.form_data = urllib.parse.parse_qs(self.body)


class HTTPResponse:
    def __init__(self, status_code: int = 200, content: str = '', headers: Dict[str, str] = None):
        self.status_code = status_code
        self.content = content
        self.headers = headers or {}
        
        # Set default headers
        if 'content-type' not in self.headers:
            self.headers['content-type'] = 'text/html; charset=utf-8'
        if 'server' not in self.headers:
            self.headers['server'] = 'Python-WebServer/1.0'
        if 'date' not in self.headers:
            self.headers['date'] = datetime.utcnow().strftime('%a, %d %b %Y %H:%M:%S GMT')

    def to_bytes(self) -> bytes:
        status_messages = {
            200: 'OK',
            201: 'Created',
            400: 'Bad Request',
            404: 'Not Found',
            405: 'Method Not Allowed',
            500: 'Internal Server Error'
        }
        
        status_line = f"HTTP/1.1 {self.status_code} {status_messages.get(self.status_code, 'Unknown')}\r\n"
        headers_str = '\r\n'.join(f"{key}: {value}" for key, value in self.headers.items())
        
        response = f"{status_line}{headers_str}\r\n\r\n{self.content}"
        return response.encode('utf-8')


class Route:
    def __init__(self, pattern: str, handler: Callable, methods: List[str] = None):
        self.pattern = pattern
        self.handler = handler
        self.methods = methods or ['GET']
        self.regex = self._compile_pattern(pattern)
    
    def _compile_pattern(self, pattern: str) -> re.Pattern:
        # Convert /users/<id> to /users/(?P<id>[^/]+)
        regex_pattern = re.sub(r'<(\w+)>', r'(?P<\1>[^/]+)', pattern)
        regex_pattern = f"^{regex_pattern}$"
        return re.compile(regex_pattern)
    
    def match(self, path: str, method: str) -> Optional[Dict[str, str]]:
        if method not in self.methods:
            return None
        
        match = self.regex.match(path)
        if match:
            return match.groupdict()
        return None


class WebServer:
    def __init__(self, host: str = 'localhost', port: int = 8080):
        self.host = host
        self.port = port
        self.routes: List[Route] = []
        self.middleware: List[Callable] = []
        self.static_files: Dict[str, str] = {}
        
    def route(self, path: str, methods: List[str] = None):
        def decorator(handler):
            self.routes.append(Route(path, handler, methods))
            return handler
        return decorator
    
    def add_middleware(self, middleware: Callable):
        self.middleware.append(middleware)
    
    def serve_static(self, url_path: str, file_path: str):
        self.static_files[url_path] = file_path
    
    def handle_request(self, request: HTTPRequest) -> HTTPResponse:
        try:
            # Apply middleware
            for middleware in self.middleware:
                result = middleware(request)
                if isinstance(result, HTTPResponse):
                    return result
            
            # Check static files
            if request.path in self.static_files:
                try:
                    with open(self.static_files[request.path], 'r') as f:
                        content = f.read()
                    return HTTPResponse(200, content)
                except FileNotFoundError:
                    return HTTPResponse(404, "File not found")
            
            # Find matching route
            for route in self.routes:
                path_params = route.match(request.path, request.method)
                if path_params is not None:
                    try:
                        result = route.handler(request, **path_params)
                        if isinstance(result, HTTPResponse):
                            return result
                        elif isinstance(result, str):
                            return HTTPResponse(200, result)
                        elif isinstance(result, dict):
                            return HTTPResponse(200, json.dumps(result), 
                                              {'content-type': 'application/json'})
                    except Exception as e:
                        return HTTPResponse(500, f"Internal Server Error: {str(e)}")
            
            return HTTPResponse(404, "Not Found")
            
        except Exception as e:
            return HTTPResponse(500, f"Server Error: {str(e)}")
    
    def handle_client(self, client_socket: socket.socket):
        try:
            raw_request = client_socket.recv(4096).decode('utf-8')
            if not raw_request:
                return
                
            request = HTTPRequest(raw_request)
            response = self.handle_request(request)
            
            client_socket.send(response.to_bytes())
            
        except Exception as e:
            error_response = HTTPResponse(500, f"Error: {str(e)}")
            client_socket.send(error_response.to_bytes())
        finally:
            client_socket.close()
    
    def run(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        
        try:
            server_socket.bind((self.host, self.port))
            server_socket.listen(5)
            
            print(f"Server running on http://{self.host}:{self.port}")
            print("Press Ctrl+C to stop the server")
            
            while True:
                client_socket, address = server_socket.accept()
                print(f"Connection from {address}")
                
                # Handle each request in a separate thread
                client_thread = threading.Thread(
                    target=self.handle_client,
                    args=(client_socket,)
                )
                client_thread.daemon = True
                client_thread.start()
                
        except KeyboardInterrupt:
            print("\nShutting down server...")
        finally:
            server_socket.close()


# Demo application
def create_demo_app():
    app = WebServer('localhost', 8080)
    
    # In-memory data store
    users = [
        {'id': 1, 'name': 'John Doe', 'email': 'john@example.com'},
        {'id': 2, 'name': 'Jane Smith', 'email': 'jane@example.com'}
    ]
    next_id = 3
    
    # Logging middleware
    def logging_middleware(request):
        timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        print(f"[{timestamp}] {request.method} {request.path}")
        return None  # Continue processing
    
    app.add_middleware(logging_middleware)
    
    @app.route('/')
    def home(request):
        return """
        <html>
        <head><title>Python Web Server</title></head>
        <body>
            <h1>Welcome to Python Web Server</h1>
            <ul>
                <li><a href="/users">View Users</a></li>
                <li><a href="/users/1">View User 1</a></li>
                <li><a href="/health">Health Check</a></li>
                <li><a href="/form">Test Form</a></li>
            </ul>
        </body>
        </html>
        """
    
    @app.route('/users')
    def get_users(request):
        return {
            'users': users,
            'count': len(users)
        }
    
    @app.route('/users/<user_id>')
    def get_user(request, user_id):
        user_id = int(user_id)
        user = next((u for u in users if u['id'] == user_id), None)
        if user:
            return user
        else:
            return HTTPResponse(404, json.dumps({'error': 'User not found'}),
                              {'content-type': 'application/json'})
    
    @app.route('/users', ['POST'])
    def create_user(request):
        nonlocal next_id
        try:
            if request.form_data:
                name = request.form_data.get('name', [''])[0]
                email = request.form_data.get('email', [''])[0]
            else:
                data = json.loads(request.body)
                name = data.get('name', '')
                email = data.get('email', '')
            
            if not name or not email:
                return HTTPResponse(400, json.dumps({'error': 'Name and email required'}),
                                  {'content-type': 'application/json'})
            
            user = {
                'id': next_id,
                'name': name,
                'email': email
            }
            users.append(user)
            next_id += 1
            
            return HTTPResponse(201, json.dumps(user),
                              {'content-type': 'application/json'})
        except Exception as e:
            return HTTPResponse(400, json.dumps({'error': str(e)}),
                              {'content-type': 'application/json'})
    
    @app.route('/health')
    def health_check(request):
        return {
            'status': 'healthy',
            'timestamp': datetime.now().isoformat(),
            'users_count': len(users)
        }
    
    @app.route('/form')
    def show_form(request):
        return """
        <html>
        <head><title>User Form</title></head>
        <body>
            <h1>Create New User</h1>
            <form method="POST" action="/users">
                <p>
                    <label>Name:</label><br>
                    <input type="text" name="name" required>
                </p>
                <p>
                    <label>Email:</label><br>
                    <input type="email" name="email" required>
                </p>
                <p>
                    <input type="submit" value="Create User">
                </p>
            </form>
        </body>
        </html>
        """
    
    return app


def main():
    print("=== Advanced Python Web Server ===")
    print("Features:")
    print("- HTTP request/response handling")
    print("- URL routing with parameters")
    print("- Middleware support")
    print("- JSON API endpoints")
    print("- Form handling")
    print("- Multi-threaded request handling")
    print()
    
    app = create_demo_app()
    
    try:
        app.run()
    except KeyboardInterrupt:
        print("Server stopped.")


if __name__ == "__main__":
    main()