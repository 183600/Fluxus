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
    putStrLn "Testing Python loops to Go code generation..."
    
    -- Test for loop
    let testCode = T.unlines [
            "total = 0",
            "for i in range(5):",
            "    total += i",
            "    print(i, total)"
            ]
    
    putStrLn "\n=== Python Source ==="
    TIO.putStrLn testCode
    
    -- Lex the code
    case runPythonLexer "test.py" testCode of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            -- Parse the code
            case runPythonParser "test.py" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right (PythonAST module_) -> do
                    -- Generate Go code
                    putStrLn "\n=== Generated Go Code ==="
                    let goCode = generateGoCode (PythonAST module_) (defaultGoConfig "main")
                    TIO.putStrLn goCode