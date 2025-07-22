-- Debug script to test Python parser issues
import System.Process
import System.Exit
import System.IO

main :: IO ()
main = do
    putStrLn "=== Testing Python Parser with Real Code ==="
    
    -- Test 1: Simple function definition
    putStrLn "\n1. Testing simple function definition..."
    writeFile "temp_test1.py" "def test():\n    pass"
    exitCode <- system "cabal exec -- runghc -e 'import Fluxus.Parser.Python.Parser; import Fluxus.Parser.Python.Lexer; import qualified Data.Text as T; import qualified Data.Text.IO as TIO; main = do { content <- TIO.readFile \"temp_test1.py\"; case runPythonLexer \"temp_test1.py\" content of { Left err >> putStrLn (\"Lexer error: \" ++ show err); Right tokens >> case runPythonParser \"temp_test1.py\" tokens of { Left err >> putStrLn (\"Parser error: \" ++ show err); Right ast >> putStrLn \"Success!\" } } }'"
    print exitCode
    
    -- Test 2: Function with parameters
    putStrLn "\n2. Testing function with parameters..."
    writeFile "temp_test2.py" "def add(a, b):\n    return a + b"
    exitCode <- system "cabal exec -- runghc -e 'import Fluxus.Parser.Python.Parser; import Fluxus.Parser.Python.Lexer; import qualified Data.Text as T; import qualified Data.Text.IO as TIO; main = do { content <- TIO.readFile \"temp_test2.py\"; case runPythonLexer \"temp_test2.py\" content of { Left err >> putStrLn (\"Lexer error: \" ++ show err); Right tokens >> case runPythonParser \"temp_test2.py\" tokens of { Left err >> putStrLn (\"Parser error: \" ++ show err); Right ast >> putStrLn \"Success!\" } } }'"
    print exitCode
    
    -- Test 3: For loop
    putStrLn "\n3. Testing for loop..."
    writeFile "temp_test3.py" "for i in range(10):\n    print(i)"
    exitCode <- system "cabal exec -- runghc -e 'import Fluxus.Parser.Python.Parser; import Fluxus.Parser.Python.Lexer; import qualified Data.Text as T; import qualified Data.Text.IO as TIO; main = do { content <- TIO.readFile \"temp_test3.py\"; case runPythonLexer \"temp_test3.py\" content of { Left err >> putStrLn (\"Lexer error: \" ++ show err); Right tokens >> case runPythonParser \"temp_test3.py\" tokens of { Left err >> putStrLn (\"Parser error: \" ++ show err); Right ast >> putStrLn \"Success!\" } } }'"
    print exitCode