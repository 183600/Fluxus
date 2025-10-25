{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.CodeGen.Go
import Fluxus.AST.Common (locatedValue)

main :: IO ()
main = do
    putStrLn "Testing Python parser with real Python code..."
    
    -- Read the test file
    content <- TIO.readFile "test_comprehensive.py"
    putStrLn $ "File content:\n" ++ T.unpack content
    
    putStrLn "\n=== Testing Python lexer ==="
    case runPythonLexer "test_comprehensive.py" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Successfully tokenized " ++ show (length tokens) ++ " tokens"
            mapM_ (\t -> putStrLn $ "  " ++ show (locatedValue t)) tokens
            
            putStrLn "\n=== Testing Python parser ==="
            case runPythonParser "test_comprehensive.py" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right (PythonAST module_) -> do
                    putStrLn $ "Successfully parsed module with " ++ show (length (pyModuleBody module_)) ++ " statements"
                    mapM_ (\stmt -> putStrLn $ "  " ++ show (locatedValue stmt)) (pyModuleBody module_)
                    
                    -- Try to generate Go code
                    putStrLn "\n=== Testing Go code generation ==="
                    let goCode = T.unpack $ generateGoCode (PythonAST module_) (defaultGoConfig "main")
                    putStrLn goCode