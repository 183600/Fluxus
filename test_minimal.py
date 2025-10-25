#!/usr/bin/env python3
import sys
import os

# Add src to path
sys.path.insert(0, 'src')

def test_python_lexer():
    try:
        from Fluxus.Parser.Python.Lexer import runPythonLexer
        from Fluxus.Parser.Python.Parser import runPythonParser
        
        # Read the file
        with open('examples/python/fibonacci.py', 'r') as f:
            content = f.read()
        
        print("=== Python Fibonacci.py Content ===")
        print(repr(content))
        print("\n=== Testing Lexer ===")
        
        # Test lexer
        result = runPythonLexer("fibonacci.py", content)
        if hasattr(result, 'is_left'):
            if result.is_left():
                print("Lexer failed:", result.value)
                return False
            else:
                tokens = result.value
                print(f"Lexer success: {len(tokens)} tokens")
                
                # Test parser
                print("\n=== Testing Parser ===")
                parse_result = runPythonParser("fibonacci.py", tokens)
                if parse_result.is_left():
                    print("Parser failed:", parse_result.value)
                    return False
                else:
                    print("Parser success!")
                    return True
        else:
            print("Unexpected lexer result:", type(result), result)
            return False
            
    except ImportError as e:
        print(f"Import error: {e}")
        return False
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_go_lexer():
    try:
        from Fluxus.Parser.Go.Lexer import runGoLexer
        from Fluxus.Parser.Go.Parser import runGoParser
        
        # Read the file
        with open('examples/go/fibonacci.go', 'r') as f:
            content = f.read()
        
        print("\n=== Go Fibonacci.go Content ===")
        print(repr(content[:200]) + "...")
        print("\n=== Testing Lexer ===")
        
        # Test lexer
        result = runGoLexer("fibonacci.go", content)
        if hasattr(result, 'is_left'):
            if result.is_left():
                print("Go lexer failed:", result.value)
                return False
            else:
                tokens = result.value
                print(f"Go lexer success: {len(tokens)} tokens")
                
                # Test parser
                print("\n=== Testing Parser ===")
                parse_result = runGoParser("fibonacci.go", tokens)
                if parse_result.is_left():
                    print("Go parser failed:", parse_result.value)
                    return False
                else:
                    print("Go parser success!")
                    return True
        else:
            print("Unexpected Go lexer result:", type(result), result)
            return False
            
    except ImportError as e:
        print(f"Import error: {e}")
        return False
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    print("Testing Python parser...")
    python_success = test_python_lexer()
    
    print("\n" + "="*50)
    print("Testing Go parser...")
    go_success = test_go_lexer()
    
    print("\n" + "="*50)
    print("Summary:")
    print(f"Python parser: {'✓' if python_success else '✗'}")
    print(f"Go parser: {'✓' if go_success else '✗'}")