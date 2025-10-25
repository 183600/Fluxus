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
    putStrLn "Testing Python to Go code generation..."
    
    -- Simple test case
    let testCode = T.unlines [
            "a = 10",
            "b = 20", 
            "def add(x, y):",
            "    return x + y",
            "result = add(a, b)",
            "print(result)"
            ]
    
    putStrLn "\n=== Python Source ==="
    TIO.putStrLn testCode
    
    -- Lex the code
    case runPythonLexer "test.py" testCode of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn "\n=== Tokens ==="
            mapM_ (\t -> putStrLn $ "  " ++ show (locatedValue t)) tokens
            
            -- Parse the code
            case runPythonParser "test.py" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right (PythonAST module_) -> do
                    putStrLn "\n=== AST ==="
                    mapM_ (\stmt -> putStrLn $ "  " ++ show (locatedValue stmt)) (pyModuleBody module_)
                    
                    -- Generate Go code
                    putStrLn "\n=== Generated Go Code ==="
                    let goCode = generateGoCode (PythonAST module_) (defaultGoConfig "main")
                    TIO.putStrLn goCode