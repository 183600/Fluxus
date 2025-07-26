#!/usr/bin/env python3
"""
Advanced Cryptography and Security Tools
Implements various encryption algorithms and security utilities
"""

import hashlib
import hmac
import secrets
import base64
from typing import List, Tuple, Dict, Optional
import string
import random
from collections import Counter


class Caesar:
    """Caesar cipher implementation"""
    
    @staticmethod
    def encrypt(plaintext: str, shift: int) -> str:
        result = ""
        for char in plaintext:
            if char.isalpha():
                ascii_offset = ord('A') if char.isupper() else ord('a')
                shifted = (ord(char) - ascii_offset + shift) % 26
                result += chr(shifted + ascii_offset)
            else:
                result += char
        return result
    
    @staticmethod
    def decrypt(ciphertext: str, shift: int) -> str:
        return Caesar.encrypt(ciphertext, -shift)


class Vigenere:
    """Vigenère cipher implementation"""
    
    @staticmethod
    def _extend_key(plaintext: str, key: str) -> str:
        key = key.upper()
        extended_key = ""
        key_index = 0
        
        for char in plaintext:
            if char.isalpha():
                extended_key += key[key_index % len(key)]
                key_index += 1
            else:
                extended_key += char
        
        return extended_key
    
    @staticmethod
    def encrypt(plaintext: str, key: str) -> str:
        extended_key = Vigenere._extend_key(plaintext, key)
        result = ""
        
        for i, char in enumerate(plaintext):
            if char.isalpha():
                ascii_offset = ord('A') if char.isupper() else ord('a')
                key_char = extended_key[i]
                shift = ord(key_char) - ord('A')
                shifted = (ord(char) - ascii_offset + shift) % 26
                result += chr(shifted + ascii_offset)
            else:
                result += char
        
        return result
    
    @staticmethod
    def decrypt(ciphertext: str, key: str) -> str:
        extended_key = Vigenere._extend_key(ciphertext, key)
        result = ""
        
        for i, char in enumerate(ciphertext):
            if char.isalpha():
                ascii_offset = ord('A') if char.isupper() else ord('a')
                key_char = extended_key[i]
                shift = ord(key_char) - ord('A')
                shifted = (ord(char) - ascii_offset - shift) % 26
                result += chr(shifted + ascii_offset)
            else:
                result += char
        
        return result


class RSA:
    """Simplified RSA implementation (for educational purposes only)"""
    
    @staticmethod
    def _gcd(a: int, b: int) -> int:
        while b:
            a, b = b, a % b
        return a
    
    @staticmethod
    def _mod_inverse(a: int, m: int) -> Optional[int]:
        """Extended Euclidean Algorithm to find modular inverse"""
        if RSA._gcd(a, m) != 1:
            return None
        
        # Find x such that (a * x) % m = 1
        def extended_gcd(a, b):
            if a == 0:
                return b, 0, 1
            gcd, x1, y1 = extended_gcd(b % a, a)
            x = y1 - (b // a) * x1
            y = x1
            return gcd, x, y
        
        _, x, _ = extended_gcd(a, m)
        return (x % m + m) % m
    
    @staticmethod
    def _is_prime(n: int) -> bool:
        if n < 2:
            return False
        if n == 2:
            return True
        if n % 2 == 0:
            return False
        
        for i in range(3, int(n ** 0.5) + 1, 2):
            if n % i == 0:
                return False
        return True
    
    @staticmethod
    def _generate_prime(bits: int) -> int:
        """Generate a random prime number with specified bit length"""
        while True:
            num = random.getrandbits(bits)
            num |= (1 << bits - 1) | 1  # Set MSB and LSB to 1
            if RSA._is_prime(num):
                return num
    
    @staticmethod
    def generate_keypair(bits: int = 8) -> Tuple[Tuple[int, int], Tuple[int, int]]:
        """Generate RSA key pair (for small examples only)"""
        # Generate two distinct prime numbers
        p = RSA._generate_prime(bits // 2)
        q = RSA._generate_prime(bits // 2)
        while p == q:
            q = RSA._generate_prime(bits // 2)
        
        n = p * q
        phi = (p - 1) * (q - 1)
        
        # Choose e (commonly 65537, but using smaller values for demo)
        e = 65537
        while RSA._gcd(e, phi) != 1:
            e += 2
        
        # Calculate d (private exponent)
        d = RSA._mod_inverse(e, phi)
        if d is None:
            return RSA.generate_keypair(bits)  # Retry
        
        public_key = (n, e)
        private_key = (n, d)
        
        return public_key, private_key
    
    @staticmethod
    def encrypt(plaintext: int, public_key: Tuple[int, int]) -> int:
        n, e = public_key
        return pow(plaintext, e, n)
    
    @staticmethod
    def decrypt(ciphertext: int, private_key: Tuple[int, int]) -> int:
        n, d = private_key
        return pow(ciphertext, d, n)


class HashFunctions:
    """Various hash functions and utilities"""
    
    @staticmethod
    def md5_hash(data: str) -> str:
        return hashlib.md5(data.encode()).hexdigest()
    
    @staticmethod
    def sha256_hash(data: str) -> str:
        return hashlib.sha256(data.encode()).hexdigest()
    
    @staticmethod
    def sha512_hash(data: str) -> str:
        return hashlib.sha512(data.encode()).hexdigest()
    
    @staticmethod
    def hmac_sha256(data: str, key: str) -> str:
        return hmac.new(key.encode(), data.encode(), hashlib.sha256).hexdigest()
    
    @staticmethod
    def simple_hash(data: str) -> int:
        """Simple hash function for demonstration"""
        hash_value = 0
        for char in data:
            hash_value = (hash_value * 31 + ord(char)) % (2**32)
        return hash_value


class PasswordSecurity:
    """Password generation and strength analysis"""
    
    @staticmethod
    def generate_password(length: int = 12, use_symbols: bool = True) -> str:
        """Generate a cryptographically secure password"""
        chars = string.ascii_letters + string.digits
        if use_symbols:
            chars += "!@#$%^&*()-_=+[]{}|;:,.<>?"
        
        return ''.join(secrets.choice(chars) for _ in range(length))
    
    @staticmethod
    def analyze_strength(password: str) -> Dict[str, any]:
        """Analyze password strength"""
        length = len(password)
        has_lower = any(c.islower() for c in password)
        has_upper = any(c.isupper() for c in password)
        has_digit = any(c.isdigit() for c in password)
        has_symbol = any(c in "!@#$%^&*()-_=+[]{}|;:,.<>?" for c in password)
        
        # Calculate character set size
        charset_size = 0
        if has_lower:
            charset_size += 26
        if has_upper:
            charset_size += 26
        if has_digit:
            charset_size += 10
        if has_symbol:
            charset_size += 32
        
        # Calculate entropy
        entropy = length * (charset_size.bit_length() - 1) if charset_size > 0 else 0
        
        # Determine strength
        score = 0
        if length >= 8:
            score += 1
        if length >= 12:
            score += 1
        if has_lower:
            score += 1
        if has_upper:
            score += 1
        if has_digit:
            score += 1
        if has_symbol:
            score += 1
        
        strength_levels = ["Very Weak", "Weak", "Fair", "Good", "Strong", "Very Strong"]
        strength = strength_levels[min(score, len(strength_levels) - 1)]
        
        return {
            'length': length,
            'has_lowercase': has_lower,
            'has_uppercase': has_upper,
            'has_digits': has_digit,
            'has_symbols': has_symbol,
            'charset_size': charset_size,
            'entropy_bits': entropy,
            'strength': strength,
            'score': score
        }
    
    @staticmethod
    def check_common_passwords(password: str) -> bool:
        """Check against common passwords (simplified list)"""
        common_passwords = {
            'password', '123456', '123456789', 'qwerty', 'abc123',
            'password123', 'admin', 'letmein', 'welcome', '123123',
            'password1', 'iloveyou', '1234567890', 'monkey'
        }
        return password.lower() in common_passwords


class FrequencyAnalysis:
    """Frequency analysis for cryptanalysis"""
    
    @staticmethod
    def letter_frequency(text: str) -> Dict[str, float]:
        """Calculate letter frequency in text"""
        text = ''.join(c.upper() for c in text if c.isalpha())
        if not text:
            return {}
        
        frequency = Counter(text)
        total = len(text)
        
        return {letter: count / total for letter, count in frequency.items()}
    
    @staticmethod
    def index_of_coincidence(text: str) -> float:
        """Calculate index of coincidence"""
        text = ''.join(c.upper() for c in text if c.isalpha())
        n = len(text)
        
        if n <= 1:
            return 0.0
        
        frequency = Counter(text)
        ic = sum(f * (f - 1) for f in frequency.values()) / (n * (n - 1))
        
        return ic
    
    @staticmethod
    def chi_squared_test(observed: Dict[str, float], expected: Dict[str, float]) -> float:
        """Chi-squared test for frequency analysis"""
        chi_squared = 0.0
        
        for letter in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ':
            obs = observed.get(letter, 0)
            exp = expected.get(letter, 1/26)  # Uniform distribution if not specified
            
            if exp > 0:
                chi_squared += ((obs - exp) ** 2) / exp
        
        return chi_squared


class Base64Encoder:
    """Custom Base64 encoder/decoder implementation"""
    
    ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    PADDING = "="
    
    @staticmethod
    def encode(data: bytes) -> str:
        """Encode bytes to base64 string"""
        result = ""
        padding = 0
        
        # Process input in 3-byte chunks
        for i in range(0, len(data), 3):
            chunk = data[i:i+3]
            
            # Pad chunk to 3 bytes if necessary
            while len(chunk) < 3:
                chunk += b'\x00'
                padding += 1
            
            # Convert 3 bytes to 4 base64 characters
            n = (chunk[0] << 16) | (chunk[1] << 8) | chunk[2]
            
            result += Base64Encoder.ALPHABET[(n >> 18) & 63]
            result += Base64Encoder.ALPHABET[(n >> 12) & 63]
            result += Base64Encoder.ALPHABET[(n >> 6) & 63]
            result += Base64Encoder.ALPHABET[n & 63]
        
        # Add padding
        if padding:
            result = result[:-padding] + Base64Encoder.PADDING * padding
        
        return result
    
    @staticmethod
    def decode(data: str) -> bytes:
        """Decode base64 string to bytes"""
        # Remove padding
        padding = data.count(Base64Encoder.PADDING)
        data = data.rstrip(Base64Encoder.PADDING)
        
        result = bytearray()
        
        # Process input in 4-character chunks
        for i in range(0, len(data), 4):
            chunk = data[i:i+4]
            
            # Convert characters to indices
            try:
                indices = [Base64Encoder.ALPHABET.index(c) for c in chunk]
            except ValueError:
                raise ValueError("Invalid base64 character")
            
            # Pad chunk to 4 characters if necessary
            while len(indices) < 4:
                indices.append(0)
            
            # Convert 4 base64 characters to 3 bytes
            n = (indices[0] << 18) | (indices[1] << 12) | (indices[2] << 6) | indices[3]
            
            result.append((n >> 16) & 255)
            result.append((n >> 8) & 255)
            result.append(n & 255)
        
        # Remove padding bytes
        if padding:
            result = result[:-padding]
        
        return bytes(result)


def main():
    print("=== Advanced Cryptography and Security Tools ===\n")
    
    # 1. Caesar Cipher
    print("1. Caesar Cipher:")
    plaintext = "Hello, World!"
    shift = 3
    encrypted = Caesar.encrypt(plaintext, shift)
    decrypted = Caesar.decrypt(encrypted, shift)
    print(f"   Plaintext: {plaintext}")
    print(f"   Encrypted: {encrypted}")
    print(f"   Decrypted: {decrypted}")
    
    # 2. Vigenère Cipher
    print("\n2. Vigenère Cipher:")
    key = "KEY"
    encrypted = Vigenere.encrypt(plaintext, key)
    decrypted = Vigenere.decrypt(encrypted, key)
    print(f"   Plaintext: {plaintext}")
    print(f"   Key: {key}")
    print(f"   Encrypted: {encrypted}")
    print(f"   Decrypted: {decrypted}")
    
    # 3. RSA (Small Example)
    print("\n3. RSA Encryption (Small Example):")
    try:
        public_key, private_key = RSA.generate_keypair(16)  # Small key for demo
        message = 123  # Small integer message
        
        encrypted_msg = RSA.encrypt(message, public_key)
        decrypted_msg = RSA.decrypt(encrypted_msg, private_key)
        
        print(f"   Public Key (n, e): {public_key}")
        print(f"   Private Key (n, d): {private_key}")
        print(f"   Original Message: {message}")
        print(f"   Encrypted: {encrypted_msg}")
        print(f"   Decrypted: {decrypted_msg}")
    except Exception as e:
        print(f"   RSA Demo Error: {e}")
    
    # 4. Hash Functions
    print("\n4. Hash Functions:")
    data = "Sensitive Data"
    print(f"   Data: {data}")
    print(f"   MD5: {HashFunctions.md5_hash(data)}")
    print(f"   SHA-256: {HashFunctions.sha256_hash(data)}")
    print(f"   HMAC-SHA256: {HashFunctions.hmac_sha256(data, 'secret_key')}")
    print(f"   Simple Hash: {HashFunctions.simple_hash(data)}")
    
    # 5. Password Security
    print("\n5. Password Security:")
    secure_password = PasswordSecurity.generate_password(16, True)
    weak_password = "password123"
    
    print(f"   Generated Password: {secure_password}")
    
    secure_analysis = PasswordSecurity.analyze_strength(secure_password)
    weak_analysis = PasswordSecurity.analyze_strength(weak_password)
    
    print(f"   Secure Password Analysis:")
    print(f"     Strength: {secure_analysis['strength']}")
    print(f"     Entropy: {secure_analysis['entropy_bits']} bits")
    
    print(f"   Weak Password Analysis:")
    print(f"     Strength: {weak_analysis['strength']}")
    print(f"     Entropy: {weak_analysis['entropy_bits']} bits")
    print(f"     Is Common: {PasswordSecurity.check_common_passwords(weak_password)}")
    
    # 6. Frequency Analysis
    print("\n6. Frequency Analysis:")
    sample_text = "The quick brown fox jumps over the lazy dog. This pangram contains every letter."
    
    freq = FrequencyAnalysis.letter_frequency(sample_text)
    ic = FrequencyAnalysis.index_of_coincidence(sample_text)
    
    print(f"   Sample Text: {sample_text}")
    print(f"   Most Common Letters: {sorted(freq.items(), key=lambda x: x[1], reverse=True)[:5]}")
    print(f"   Index of Coincidence: {ic:.4f}")
    
    # 7. Base64 Encoding
    print("\n7. Base64 Encoding:")
    original_data = b"This is secret data!"
    encoded = Base64Encoder.encode(original_data)
    decoded = Base64Encoder.decode(encoded)
    
    print(f"   Original: {original_data}")
    print(f"   Encoded: {encoded}")
    print(f"   Decoded: {decoded}")
    print(f"   Built-in Base64: {base64.b64encode(original_data).decode()}")
    
    # 8. Cryptanalysis Demo
    print("\n8. Cryptanalysis Demo - Caesar Cipher Breaking:")
    mystery_cipher = "WKH TXLFN EURZQ IRA MXPSV RYHU WKH ODCB GRJ"
    
    print(f"   Mystery Cipher: {mystery_cipher}")
    print("   Trying all possible shifts:")
    
    for shift in range(26):
        decrypted = Caesar.decrypt(mystery_cipher, shift)
        if "THE" in decrypted and "QUICK" in decrypted:
            print(f"     Shift {shift}: {decrypted} ✓")
            break
        elif shift < 5:  # Show first few attempts
            print(f"     Shift {shift}: {decrypted}")
    
    print("\nCryptography demo completed!")


if __name__ == "__main__":
    main()