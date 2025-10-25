import socket
import threading
import select
import time
import json
import struct
import hashlib
import logging
from datetime import datetime
from typing import Dict, List, Optional, Callable
from dataclasses import dataclass
from concurrent.futures import ThreadPoolExecutor
import queue
import ssl
import http.server
import socketserver
from urllib.parse import urlparse, parse_qs

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

@dataclass
class Message:
    sender: str
    recipient: str
    content: str
    timestamp: datetime
    message_type: str = "text"
    data: Optional[bytes] = None

class Protocol:
    """Simple message protocol for network communication"""
    
    @staticmethod
    def encode_message(message: Message) -> bytes:
        """Encode message to bytes"""
        data = {
            'sender': message.sender,
            'recipient': message.recipient,
            'content': message.content,
            'timestamp': message.timestamp.isoformat(),
            'message_type': message.message_type,
            'data': message.data.hex() if message.data else None
        }
        
        json_data = json.dumps(data).encode('utf-8')
        length = struct.pack('!I', len(json_data))
        return length + json_data
    
    @staticmethod
    def decode_message(data: bytes) -> Optional[Message]:
        """Decode bytes to message"""
        try:
            if len(data) < 4:
                return None
            
            length = struct.unpack('!I', data[:4])[0]
            if len(data) < 4 + length:
                return None
            
            json_data = data[4:4+length].decode('utf-8')
            msg_dict = json.loads(json_data)
            
            return Message(
                sender=msg_dict['sender'],
                recipient=msg_dict['recipient'],
                content=msg_dict['content'],
                timestamp=datetime.fromisoformat(msg_dict['timestamp']),
                message_type=msg_dict['message_type'],
                data=bytes.fromhex(msg_dict['data']) if msg_dict['data'] else None
            )
        except Exception as e:
            logger.error(f"Error decoding message: {e}")
            return None

class TCPServer:
    """Multi-threaded TCP server"""
    
    def __init__(self, host='localhost', port=8888):
        self.host = host
        self.port = port
        self.socket = None
        self.clients = {}
        self.running = False
        self.message_handlers = {}
        self.executor = ThreadPoolExecutor(max_workers=10)
    
    def add_message_handler(self, message_type: str, handler: Callable):
        """Add handler for specific message type"""
        self.message_handlers[message_type] = handler
    
    def start(self):
        """Start the server"""
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind((self.host, self.port))
        self.socket.listen(5)
        self.running = True
        
        logger.info(f"Server started on {self.host}:{self.port}")
        
        while self.running:
            try:
                client_socket, address = self.socket.accept()
                logger.info(f"New client connected: {address}")
                
                # Handle client in separate thread
                self.executor.submit(self.handle_client, client_socket, address)
                
            except OSError:
                if self.running:
                    logger.error("Server socket error")
                break
    
    def handle_client(self, client_socket, address):
        """Handle individual client connection"""
        client_id = f"{address[0]}:{address[1]}"
        self.clients[client_id] = client_socket
        
        buffer = b''
        
        try:
            while self.running:
                data = client_socket.recv(4096)
                if not data:
                    break
                
                buffer += data
                
                # Process complete messages
                while len(buffer) >= 4:
                    length = struct.unpack('!I', buffer[:4])[0]
                    if len(buffer) < 4 + length:
                        break
                    
                    message_data = buffer[:4+length]
                    buffer = buffer[4+length:]
                    
                    message = Protocol.decode_message(message_data)
                    if message:
                        self.process_message(message, client_id)
        
        except Exception as e:
            logger.error(f"Error handling client {client_id}: {e}")
        
        finally:
            client_socket.close()
            if client_id in self.clients:
                del self.clients[client_id]
            logger.info(f"Client {client_id} disconnected")
    
    def process_message(self, message: Message, sender_id: str):
        """Process received message"""
        logger.info(f"Received {message.message_type} from {sender_id}: {message.content}")
        
        # Call appropriate handler
        handler = self.message_handlers.get(message.message_type)
        if handler:
            response = handler(message, sender_id)
            if response:
                self.send_message_to_client(sender_id, response)
        else:
            # Default echo behavior
            echo_message = Message(
                sender="server",
                recipient=message.sender,
                content=f"Echo: {message.content}",
                timestamp=datetime.now()
            )
            self.send_message_to_client(sender_id, echo_message)
    
    def send_message_to_client(self, client_id: str, message: Message):
        """Send message to specific client"""
        if client_id in self.clients:
            try:
                data = Protocol.encode_message(message)
                self.clients[client_id].send(data)
            except Exception as e:
                logger.error(f"Error sending message to {client_id}: {e}")
    
    def broadcast_message(self, message: Message):
        """Broadcast message to all clients"""
        for client_id in list(self.clients.keys()):
            self.send_message_to_client(client_id, message)
    
    def stop(self):
        """Stop the server"""
        self.running = False
        if self.socket:
            self.socket.close()

class TCPClient:
    """TCP client for connecting to server"""
    
    def __init__(self, host='localhost', port=8888):
        self.host = host
        self.port = port
        self.socket = None
        self.connected = False
        self.receive_thread = None
        self.message_queue = queue.Queue()
    
    def connect(self):
        """Connect to server"""
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect((self.host, self.port))
            self.connected = True
            
            # Start receive thread
            self.receive_thread = threading.Thread(target=self.receive_messages)
            self.receive_thread.daemon = True
            self.receive_thread.start()
            
            logger.info(f"Connected to server {self.host}:{self.port}")
            return True
            
        except Exception as e:
            logger.error(f"Connection failed: {e}")
            return False
    
    def receive_messages(self):
        """Receive messages from server"""
        buffer = b''
        
        while self.connected:
            try:
                data = self.socket.recv(4096)
                if not data:
                    break
                
                buffer += data
                
                # Process complete messages
                while len(buffer) >= 4:
                    length = struct.unpack('!I', buffer[:4])[0]
                    if len(buffer) < 4 + length:
                        break
                    
                    message_data = buffer[:4+length]
                    buffer = buffer[4+length:]
                    
                    message = Protocol.decode_message(message_data)
                    if message:
                        self.message_queue.put(message)
            
            except Exception as e:
                if self.connected:
                    logger.error(f"Receive error: {e}")
                break
    
    def send_message(self, message: Message):
        """Send message to server"""
        if self.connected:
            try:
                data = Protocol.encode_message(message)
                self.socket.send(data)
                return True
            except Exception as e:
                logger.error(f"Send error: {e}")
                return False
        return False
    
    def get_message(self, timeout=1.0):
        """Get received message from queue"""
        try:
            return self.message_queue.get(timeout=timeout)
        except queue.Empty:
            return None
    
    def disconnect(self):
        """Disconnect from server"""
        self.connected = False
        if self.socket:
            self.socket.close()

class UDPServer:
    """UDP server for connectionless communication"""
    
    def __init__(self, host='localhost', port=9999):
        self.host = host
        self.port = port  
        self.socket = None
        self.running = False
        self.clients = set()
    
    def start(self):
        """Start UDP server"""
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.socket.bind((self.host, self.port))
        self.running = True
        
        logger.info(f"UDP Server started on {self.host}:{self.port}")
        
        while self.running:
            try:
                data, address = self.socket.recvfrom(4096)
                self.clients.add(address)
                
                # Process UDP message
                message_str = data.decode('utf-8')
                logger.info(f"UDP message from {address}: {message_str}")
                
                # Echo back
                response = f"UDP Echo: {message_str}"
                self.socket.sendto(response.encode('utf-8'), address)
                
            except OSError:
                if self.running:
                    logger.error("UDP server error")
                break
    
    def stop(self):
        """Stop UDP server"""
        self.running = False
        if self.socket:
            self.socket.close()

class HTTPServerWithCustomHandlers(http.server.BaseHTTPRequestHandler):
    """Custom HTTP server with API endpoints"""
    
    def do_GET(self):
        """Handle GET requests"""
        parsed_path = urlparse(self.path)
        path = parsed_path.path
        params = parse_qs(parsed_path.query)
        
        if path == '/api/status':
            self.send_json_response({
                'status': 'ok',
                'timestamp': datetime.now().isoformat(),
                'server': 'Custom HTTP Server'
            })
        
        elif path == '/api/echo':
            message = params.get('message', [''])[0]
            self.send_json_response({
                'echo': message,
                'length': len(message)
            })
        
        elif path == '/':
            self.send_html_response("""
            <html>
            <head><title>Custom HTTP Server</title></head>
            <body>
                <h1>Custom HTTP Server</h1>
                <p>Available endpoints:</p>
                <ul>
                    <li><a href="/api/status">/api/status</a> - Server status</li>
                    <li><a href="/api/echo?message=hello">/api/echo?message=hello</a> - Echo service</li>
                </ul>
            </body>
            </html>
            """)
        
        else:
            self.send_error(404, "Endpoint not found")
    
    def do_POST(self):
        """Handle POST requests"""
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)
        
        try:
            data = json.loads(post_data.decode('utf-8'))
            
            if self.path == '/api/process':
                result = {
                    'received': data,
                    'processed_at': datetime.now().isoformat(),
                    'hash': hashlib.md5(post_data).hexdigest()
                }
                self.send_json_response(result)
            
            else:
                self.send_error(404, "Endpoint not found")
        
        except json.JSONDecodeError:
            self.send_error(400, "Invalid JSON")
    
    def send_json_response(self, data):
        """Send JSON response"""
        response = json.dumps(data, indent=2)
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.send_header('Content-length', len(response))
        self.end_headers()
        self.wfile.write(response.encode('utf-8'))
    
    def send_html_response(self, html):
        """Send HTML response"""
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.send_header('Content-length', len(html))
        self.end_headers()
        self.wfile.write(html.encode('utf-8'))
    
    def log_message(self, format, *args):
        """Override logging to use our logger"""
        logger.info(f"HTTP {format % args}")

class NetworkMonitor:
    """Monitor network connections and traffic"""
    
    def __init__(self):
        self.connections = {}
        self.traffic_stats = {
            'bytes_sent': 0,
            'bytes_received': 0,
            'messages_sent': 0,
            'messages_received': 0
        }
    
    def log_connection(self, connection_type: str, address: str):
        """Log new connection"""
        self.connections[address] = {
            'type': connection_type,
            'connected_at': datetime.now(),
            'last_activity': datetime.now()
        }
        logger.info(f"New {connection_type} connection: {address}")
    
    def log_traffic(self, direction: str, bytes_count: int):
        """Log network traffic"""
        if direction == 'sent':
            self.traffic_stats['bytes_sent'] += bytes_count
            self.traffic_stats['messages_sent'] += 1
        elif direction == 'received':
            self.traffic_stats['bytes_received'] += bytes_count
            self.traffic_stats['messages_received'] += 1
    
    def get_stats(self):
        """Get network statistics"""
        return {
            'active_connections': len(self.connections),
            'traffic_stats': self.traffic_stats,
            'connections': self.connections
        }

def message_handler_chat(message: Message, sender_id: str):
    """Handler for chat messages"""
    if message.content.startswith('/time'):
        return Message(
            sender="server",
            recipient=message.sender,
            content=f"Current time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            timestamp=datetime.now(),
            message_type="system"
        )
    return None

def message_handler_file(message: Message, sender_id: str):
    """Handler for file messages"""
    if message.data:
        filename = f"received_file_{datetime.now().strftime('%Y%m%d_%H%M%S')}.dat"
        with open(filename, 'wb') as f:
            f.write(message.data)
        
        return Message(
            sender="server",
            recipient=message.sender,
            content=f"File saved as {filename} ({len(message.data)} bytes)",
            timestamp=datetime.now(),
            message_type="system"
        )
    return None

def demonstrate_tcp_communication():
    """Demonstrate TCP client-server communication"""
    print("TCP Communication Demo")
    print("=" * 30)
    
    # Start server in separate thread
    server = TCPServer(port=8888)
    server.add_message_handler("chat", message_handler_chat)
    server.add_message_handler("file", message_handler_file)
    
    server_thread = threading.Thread(target=server.start)
    server_thread.daemon = True
    server_thread.start()
    
    time.sleep(1)  # Let server start
    
    # Create client and connect
    client = TCPClient(port=8888)
    if client.connect():
        # Send various types of messages
        messages = [
            Message("client1", "server", "Hello Server!", datetime.now(), "chat"),
            Message("client1", "server", "/time", datetime.now(), "chat"),
            Message("client1", "server", "File data", datetime.now(), "file", 
                   data=b"This is test file content"),
        ]
        
        for msg in messages:
            client.send_message(msg)
            time.sleep(0.5)
            
            # Check for response
            response = client.get_message(timeout=2.0)
            if response:
                print(f"Response: {response.content}")
        
        client.disconnect()
    
    server.stop()

def demonstrate_udp_communication():
    """Demonstrate UDP communication"""
    print("\nUDP Communication Demo")
    print("=" * 30)
    
    # Start UDP server
    udp_server = UDPServer(port=9999)
    server_thread = threading.Thread(target=udp_server.start)
    server_thread.daemon = True
    server_thread.start()
    
    time.sleep(1)  # Let server start
    
    # Create UDP client
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    
    # Send messages
    messages = ["Hello UDP Server!", "How are you?", "Goodbye!"]
    
    for msg in messages:
        client_socket.sendto(msg.encode('utf-8'), ('localhost', 9999))
        
        # Receive response
        response, address = client_socket.recvfrom(4096)
        print(f"UDP Response: {response.decode('utf-8')}")
        
        time.sleep(0.5)
    
    client_socket.close()
    udp_server.stop()

def demonstrate_http_server():
    """Demonstrate HTTP server"""
    print("\nHTTP Server Demo")
    print("=" * 30)
    
    port = 8080
    server = socketserver.TCPServer(("", port), HTTPServerWithCustomHandlers)
    
    print(f"HTTP Server running on http://localhost:{port}")
    print("Available endpoints:")
    print(f"  - http://localhost:{port}/")
    print(f"  - http://localhost:{port}/api/status")
    print(f"  - http://localhost:{port}/api/echo?message=hello")
    
    # Run server in separate thread for demo
    server_thread = threading.Thread(target=server.serve_forever)
    server_thread.daemon = True
    server_thread.start()
    
    # Make some test requests
    import urllib.request
    import urllib.parse
    
    try:
        # Test GET requests
        with urllib.request.urlopen(f'http://localhost:{port}/api/status') as response:
            data = response.read().decode('utf-8')
            print(f"Status response: {json.loads(data)}")
        
        # Test POST request
        post_data = json.dumps({'test': 'data', 'number': 42}).encode('utf-8')
        req = urllib.request.Request(
            f'http://localhost:{port}/api/process',
            data=post_data,
            headers={'Content-Type': 'application/json'}
        )
        
        with urllib.request.urlopen(req) as response:
            data = response.read().decode('utf-8')
            print(f"Process response: {json.loads(data)}")
    
    except Exception as e:
        print(f"HTTP request error: {e}")
    
    server.shutdown()

def main():
    """Main function demonstrating network programming"""
    print("Advanced Network Programming Demo")
    print("=" * 40)
    
    try:
        demonstrate_tcp_communication()
        demonstrate_udp_communication()
        demonstrate_http_server()
        
        print("\nNetwork programming demonstration completed!")
        
    except KeyboardInterrupt:
        print("\nDemo interrupted by user")
    except Exception as e:
        print(f"Demo error: {e}")

if __name__ == "__main__":
    main()