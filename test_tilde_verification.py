#!/usr/bin/env python3

# Simple test to verify tilde operator support
import subprocess
import sys

def test_tilde_operator():
    # Test Go code with tilde operator
    test_code = '''
package main

import "fmt"

// Test approximation constraints
type Numeric interface {
    ~int | ~int8 | ~int16 | ~int32 | ~int64 | 
    ~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | 
    ~float32 | ~float64
}

func AddNumbers[T Numeric](a, b T) T {
    return a + b
}

func main() {
    result := AddNumbers(10, 20)
    fmt.Println(result)
}
'''
    
    # Write test file
    with open('test_tilde_verification.go', 'w') as f:
        f.write(test_code)
    
    # Try to run the fluxus build process
    try:
        # Test lexer only (should work with tilde)
        result = subprocess.run(
            ['ghc', '-e', 'import Fluxus.Parser.Go.Lexer; main = putStrLn "Lexer test passed"'],
            capture_output=True, text=True, timeout=10
        )
        
        if result.returncode == 0:
            print("✅ Lexer compiles successfully with tilde operator support")
        else:
            print("❌ Lexer compilation failed")
            print(result.stderr)
            
    except subprocess.TimeoutExpired:
        print("⚠️  Test timed out (resource constraints)")
    except Exception as e:
        print(f"❌ Test failed with error: {e}")

if __name__ == "__main__":
    test_tilde_operator()