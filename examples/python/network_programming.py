#!/usr/bin/env python3
"""
Advanced Network Programming and Protocol Implementation
Implements various network protocols and communication patterns
"""

import socket
import threading
import json
import time
import hashlib
import struct
from typing import Dict, List, Tuple, Optional, Callable
from datetime import datetime
import urllib.parse
import ssl
import base64


class Protocol:
    """Base protocol class"""
    
    def __init__(self):
        self.handlers: Dict[str, Callable] = {}
    
    def register_handler(self, message_type: str, handler: Callable):
        self.handlers[message_type] = handler
    
    def handle_message(self, message: dict, client_socket: socket.socket = None):
        message_type = message.get('type')
        if message_type in self.handlers:
            return self.handlers[message_type](message, client_socket)
        else:
            return {'type': 'error', 'message': f'Unknown message type: {message_type}'}


class ChatProtocol(Protocol):
    """Simple chat protocol implementation"""
    
    def __init__(self):
        super().__init__()
        self.clients: Dict[str, socket.socket] = {}
        self.rooms: Dict[str, List[str]] = {}
        
        # Register message handlers
        self.register_handler('join', self.handle_join)
        self.register_handler('leave', self.handle_leave)
        self.register_handler('message', self.handle_message_send)
        self.register_handler('list_users', self.handle_list_users)
        self.register_handler('create_room', self.handle_create_room)
        self.register_handler('join_room', self.handle_join_room)
    
    def handle_join(self, message: dict, client_socket: socket.socket):
        username = message.get('username')
        if username and username not in self.clients:
            self.clients[username] = client_socket
            return {'type': 'join_success', 'username': username}
        else:
            return {'type': 'join_error', 'message': 'Username already taken or invalid'}
    
    def handle_leave(self, message: dict, client_socket: socket.socket):
        username = message.get('username')
        if username in self.clients:
            del self.clients[username]
            # Remove from all rooms
            for room_users in self.rooms.values():
                if username in room_users:
                    room_users.remove(username)
            return {'type': 'leave_success'}
        return {'type': 'leave_error', 'message': 'User not found'}
    
    def handle_message_send(self, message: dict, client_socket: socket.socket):
        sender = message.get('sender')
        content = message.get('content')
        target = message.get('target', 'all')  # 'all', username, or room name
        
        if not sender or not content:
            return {'type': 'message_error', 'message': 'Missing sender or content'}
        
        broadcast_message = {
            'type': 'message_received',
            'sender': sender,
            'content': content,
            'timestamp': datetime.now().isoformat(),
            'target': target
        }
        
        if target == 'all':
            # Broadcast to all users
            for username, sock in self.clients.items():
                if username != sender:
                    self.send_message(sock, broadcast_message)
        elif target in self.clients:
            # Send to specific user
            self.send_message(self.clients[target], broadcast_message)
        elif target in self.rooms:
            # Send to room members
            for username in self.rooms[target]:
                if username != sender and username in self.clients:
                    self.send_message(self.clients[username], broadcast_message)
        
        return {'type': 'message_sent'}
    
    def handle_list_users(self, message: dict, client_socket: socket.socket):
        return {
            'type': 'user_list',
            'users': list(self.clients.keys()),
            'rooms': list(self.rooms.keys())
        }
    
    def handle_create_room(self, message: dict, client_socket: socket.socket):
        room_name = message.get('room_name')
        creator = message.get('creator')
        
        if room_name and creator and room_name not in self.rooms:
            self.rooms[room_name] = [creator]
            return {'type': 'room_created', 'room_name': room_name}
        else:
            return {'type': 'room_error', 'message': 'Room already exists or invalid data'}
    
    def handle_join_room(self, message: dict, client_socket: socket.socket):
        room_name = message.get('room_name')
        username = message.get('username')
        
        if room_name in self.rooms and username not in self.rooms[room_name]:
            self.rooms[room_name].append(username)
            return {'type': 'room_joined', 'room_name': room_name}
        else:
            return {'type': 'room_error', 'message': 'Room not found or already joined'}
    
    def send_message(self, client_socket: socket.socket, message: dict):
        try:
            data = json.dumps(message).encode('utf-8')
            length = len(data)
            client_socket.send(struct.pack('!I', length) + data)
        except Exception as e:
            print(f"Error sending message: {e}")


class HTTPServer:
    """Simple HTTP server implementation"""
    
    def __init__(self, host: str = 'localhost', port: int = 8080):
        self.host = host
        self.port = port
        self.routes: Dict[str, Callable] = {}
    
    def route(self, path: str, method: str = 'GET'):
        def decorator(handler):
            self.routes[f"{method} {path}"] = handler
            return handler
        return decorator
    
    def parse_request(self, request: str) -> Tuple[str, str, Dict[str, str], str]:
        lines = request.split('\r\n')
        request_line = lines[0].split()
        method, path, version = request_line[0], request_line[1], request_line[2]
        
        headers = {}
        i = 1
        while i < len(lines) and lines[i]:
            if ':' in lines[i]:
                key, value = lines[i].split(':', 1)
                headers[key.strip().lower()] = value.strip()
            i += 1
        
        body = '\r\n'.join(lines[i+1:]) if i+1 < len(lines) else ''
        
        return method, path, headers, body
    
    def create_response(self, status_code: int, content: str, headers: Dict[str, str] = None) -> str:
        status_messages = {
            200: 'OK',
            404: 'Not Found',
            500: 'Internal Server Error'
        }
        
        if headers is None:
            headers = {}
        
        headers.setdefault('Content-Type', 'text/html')
        headers.setdefault('Content-Length', str(len(content)))
        headers.setdefault('Server', 'Python-HTTP/1.0')
        
        response = f"HTTP/1.1 {status_code} {status_messages.get(status_code, 'Unknown')}\r\n"
        for key, value in headers.items():
            response += f"{key}: {value}\r\n"
        response += f"\r\n{content}"
        
        return response
    
    def handle_request(self, client_socket: socket.socket):
        try:
            request = client_socket.recv(4096).decode('utf-8')
            if not request:
                return
            
            method, path, headers, body = self.parse_request(request)
            route_key = f"{method} {path}"
            
            if route_key in self.routes:
                try:
                    response_content = self.routes[route_key](headers, body)
                    response = self.create_response(200, response_content)
                except Exception as e:
                    response = self.create_response(500, f"Internal Server Error: {str(e)}")
            else:
                response = self.create_response(404, "404 Not Found")
            
            client_socket.send(response.encode('utf-8'))
            
        except Exception as e:
            error_response = self.create_response(500, f"Server Error: {str(e)}")
            client_socket.send(error_response.encode('utf-8'))
        finally:
            client_socket.close()
    
    def run(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        
        try:
            server_socket.bind((self.host, self.port))
            server_socket.listen(5)
            
            print(f"HTTP Server running on http://{self.host}:{self.port}")
            
            while True:
                client_socket, address = server_socket.accept()
                print(f"HTTP connection from {address}")
                
                client_thread = threading.Thread(
                    target=self.handle_request,
                    args=(client_socket,)
                )
                client_thread.daemon = True
                client_thread.start()
                
        except KeyboardInterrupt:
            print("\nShutting down HTTP server...")
        finally:
            server_socket.close()


class FTPServer:
    """Simple FTP-like file transfer server"""
    
    def __init__(self, host: str = 'localhost', port: int = 2121):
        self.host = host
        self.port = port
        self.files: Dict[str, bytes] = {}  # In-memory file storage
        
    def handle_client(self, client_socket: socket.socket, address: Tuple[str, int]):
        print(f"FTP client connected from {address}")
        
        # Send welcome message
        welcome = "220 Welcome to Python FTP Server\r\n"
        client_socket.send(welcome.encode('utf-8'))
        
        authenticated = False
        current_dir = "/"
        
        try:
            while True:
                data = client_socket.recv(1024).decode('utf-8').strip()
                if not data:
                    break
                
                command_parts = data.split()
                command = command_parts[0].upper()
                
                if command == 'USER':
                    client_socket.send(b"331 Username OK, need password\r\n")
                
                elif command == 'PASS':
                    authenticated = True
                    client_socket.send(b"230 Login successful\r\n")
                
                elif command == 'PWD':
                    if authenticated:
                        response = f"257 \"{current_dir}\" is current directory\r\n"
                        client_socket.send(response.encode('utf-8'))
                    else:
                        client_socket.send(b"530 Not logged in\r\n")
                
                elif command == 'LIST':
                    if authenticated:
                        file_list = '\r\n'.join(self.files.keys())
                        response = f"150 File status okay\r\n{file_list}\r\n226 Transfer complete\r\n"
                        client_socket.send(response.encode('utf-8'))
                    else:
                        client_socket.send(b"530 Not logged in\r\n")
                
                elif command == 'STOR':
                    if authenticated and len(command_parts) > 1:
                        filename = command_parts[1]
                        client_socket.send(b"150 Ready to receive file\r\n")
                        
                        # Receive file data (simplified)
                        file_data = client_socket.recv(8192)
                        self.files[filename] = file_data
                        
                        client_socket.send(b"226 Transfer complete\r\n")
                    else:
                        client_socket.send(b"530 Not logged in or missing filename\r\n")
                
                elif command == 'RETR':
                    if authenticated and len(command_parts) > 1:
                        filename = command_parts[1]
                        if filename in self.files:
                            client_socket.send(b"150 File status okay\r\n")
                            client_socket.send(self.files[filename])
                            client_socket.send(b"\r\n226 Transfer complete\r\n")
                        else:
                            client_socket.send(b"550 File not found\r\n")
                    else:
                        client_socket.send(b"530 Not logged in or missing filename\r\n")
                
                elif command == 'QUIT':
                    client_socket.send(b"221 Goodbye\r\n")
                    break
                
                else:
                    client_socket.send(b"502 Command not implemented\r\n")
                    
        except Exception as e:
            print(f"FTP client error: {e}")
        finally:
            client_socket.close()
    
    def run(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        
        try:
            server_socket.bind((self.host, self.port))
            server_socket.listen(5)
            
            print(f"FTP Server running on {self.host}:{self.port}")
            
            while True:
                client_socket, address = server_socket.accept()
                
                client_thread = threading.Thread(
                    target=self.handle_client,
                    args=(client_socket, address)
                )
                client_thread.daemon = True
                client_thread.start()
                
        except KeyboardInterrupt:
            print("\nShutting down FTP server...")
        finally:
            server_socket.close()


class NetworkScanner:
    """Network scanning and discovery tools"""
    
    @staticmethod
    def ping_host(host: str, port: int, timeout: int = 3) -> bool:
        """Check if a host:port is reachable"""
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                sock.settimeout(timeout)
                result = sock.connect_ex((host, port))
                return result == 0
        except Exception:
            return False
    
    @staticmethod
    def scan_ports(host: str, start_port: int = 1, end_port: int = 1024) -> List[int]:
        """Scan for open ports on a host"""
        open_ports = []
        
        for port in range(start_port, end_port + 1):
            if NetworkScanner.ping_host(host, port, timeout=1):
                open_ports.append(port)
                print(f"Port {port} is open on {host}")
        
        return open_ports
    
    @staticmethod
    def get_service_banner(host: str, port: int) -> Optional[str]:
        """Get service banner from a port"""
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                sock.settimeout(5)
                sock.connect((host, port))
                
                # Send a simple HTTP request for web servers
                if port in [80, 8080, 8000]:
                    sock.send(b"GET / HTTP/1.0\r\n\r\n")
                
                banner = sock.recv(1024).decode('utf-8', errors='ignore')
                return banner.strip()
        except Exception:
            return None


class PacketAnalyzer:
    """Simple packet analysis tools"""
    
    @staticmethod
    def create_tcp_packet(src_ip: str, dst_ip: str, src_port: int, dst_port: int, data: bytes) -> bytes:
        """Create a simplified TCP packet structure"""
        # This is a simplified representation - real TCP packets are more complex
        packet = {
            'src_ip': src_ip,
            'dst_ip': dst_ip,
            'src_port': src_port,
            'dst_port': dst_port,
            'data': base64.b64encode(data).decode('utf-8'),
            'timestamp': time.time()
        }
        return json.dumps(packet).encode('utf-8')
    
    @staticmethod
    def parse_packet(packet_data: bytes) -> dict:
        """Parse a simplified packet"""
        try:
            packet = json.loads(packet_data.decode('utf-8'))
            packet['data'] = base64.b64decode(packet['data'])
            return packet
        except Exception as e:
            return {'error': str(e)}


def demo_chat_server():
    """Demo chat server"""
    print("Starting Chat Server on port 9999...")
    
    protocol = ChatProtocol()
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    
    def handle_client(client_socket, address):
        print(f"Chat client connected from {address}")
        
        try:
            while True:
                # Receive message length
                length_data = client_socket.recv(4)
                if not length_data:
                    break
                
                message_length = struct.unpack('!I', length_data)[0]
                
                # Receive message
                message_data = client_socket.recv(message_length)
                message = json.loads(message_data.decode('utf-8'))
                
                # Handle message
                response = protocol.handle_message(message, client_socket)
                
                # Send response
                response_data = json.dumps(response).encode('utf-8')
                client_socket.send(struct.pack('!I', len(response_data)) + response_data)
                
        except Exception as e:
            print(f"Chat client error: {e}")
        finally:
            client_socket.close()
    
    try:
        server_socket.bind(('localhost', 9999))
        server_socket.listen(5)
        
        print("Chat server listening on localhost:9999")
        print("Use telnet or a custom client to connect")
        
        while True:
            client_socket, address = server_socket.accept()
            client_thread = threading.Thread(target=handle_client, args=(client_socket, address))
            client_thread.daemon = True
            client_thread.start()
            
    except KeyboardInterrupt:
        print("\nShutting down chat server...")
    finally:
        server_socket.close()


def main():
    print("=== Advanced Network Programming Suite ===\n")
    
    # Network Scanner Demo
    print("1. Network Scanner Demo:")
    localhost_ports = NetworkScanner.scan_ports('127.0.0.1', 1, 100)
    print(f"   Open ports on localhost (1-100): {localhost_ports}")
    
    # Banner grabbing
    if 22 in localhost_ports:  # SSH typically on port 22
        banner = NetworkScanner.get_service_banner('127.0.0.1', 22)
        if banner:
            print(f"   SSH Banner: {banner[:100]}...")
    
    # Packet Analysis Demo
    print("\n2. Packet Analysis Demo:")
    test_data = b"Hello, Network!"
    packet = PacketAnalyzer.create_tcp_packet('192.168.1.1', '192.168.1.2', 12345, 80, test_data)
    parsed = PacketAnalyzer.parse_packet(packet)
    
    print(f"   Created packet: {len(packet)} bytes")
    print(f"   Parsed packet: {parsed['src_ip']}:{parsed['src_port']} -> {parsed['dst_ip']}:{parsed['dst_port']}")
    print(f"   Data: {parsed['data']}")
    
    # HTTP Server Demo
    print("\n3. HTTP Server Demo:")
    http_server = HTTPServer('localhost', 8081)
    
    @http_server.route('/')
    def home(headers, body):
        return """
        <html>
        <head><title>Python Network Server</title></head>
        <body>
            <h1>Advanced Network Programming Demo</h1>
            <p>This server demonstrates HTTP protocol implementation.</p>
            <ul>
                <li><a href="/time">Current Time</a></li>
                <li><a href="/info">Server Info</a></li>
            </ul>
        </body>
        </html>
        """
    
    @http_server.route('/time')
    def current_time(headers, body):
        return f"<html><body><h1>Current Time</h1><p>{datetime.now()}</p></body></html>"
    
    @http_server.route('/info')
    def server_info(headers, body):
        return f"""
        <html>
        <body>
            <h1>Server Information</h1>
            <p>Headers received: {len(headers)}</p>
            <p>User Agent: {headers.get('user-agent', 'Unknown')}</p>
            <p>Host: {headers.get('host', 'Unknown')}</p>
        </body>
        </html>
        """
    
    print("   HTTP Server configured. Run demo_servers() to start.")
    
    # Network Protocol Analysis
    print("\n4. Protocol Analysis:")
    
    # Analyze different protocols
    protocols = {
        'HTTP': {'port': 80, 'description': 'HyperText Transfer Protocol'},
        'HTTPS': {'port': 443, 'description': 'HTTP Secure'},
        'FTP': {'port': 21, 'description': 'File Transfer Protocol'},
        'SSH': {'port': 22, 'description': 'Secure Shell'},
        'SMTP': {'port': 25, 'description': 'Simple Mail Transfer Protocol'},
        'DNS': {'port': 53, 'description': 'Domain Name System'},
        'POP3': {'port': 110, 'description': 'Post Office Protocol 3'},
        'IMAP': {'port': 143, 'description': 'Internet Message Access Protocol'},
    }
    
    print("   Common Network Protocols:")
    for name, info in protocols.items():
        print(f"     {name}: Port {info['port']} - {info['description']}")
    
    print("\nNetwork programming demo completed!")
    print("\nTo run interactive demos:")
    print("  - Call demo_chat_server() for chat server")
    print("  - Call http_server.run() for HTTP server")
    print("  - Call FTPServer().run() for FTP server")


def demo_servers():
    """Run all server demos in separate threads"""
    print("Starting all network servers...")
    
    # HTTP Server
    http_server = HTTPServer('localhost', 8081)
    http_thread = threading.Thread(target=http_server.run)
    http_thread.daemon = True
    http_thread.start()
    
    # FTP Server
    ftp_server = FTPServer('localhost', 2121)
    ftp_thread = threading.Thread(target=ftp_server.run)
    ftp_thread.daemon = True
    ftp_thread.start()
    
    # Chat Server
    chat_thread = threading.Thread(target=demo_chat_server)
    chat_thread.daemon = True
    chat_thread.start()
    
    print("All servers started!")
    print("HTTP: http://localhost:8081")
    print("FTP: localhost:2121")
    print("Chat: localhost:9999")
    print("Press Ctrl+C to stop all servers")
    
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print("\nStopping all servers...")


if __name__ == "__main__":
    main()