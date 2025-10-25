#!/usr/bin/env runhaskell

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Python.Lexer

main :: IO ()
main = do
    content <- TIO.readFile "test_lexer.py"
    putStrLn "=== Lexer Test ==="
    putStrLn "File content:"
    TIO.putStrLn content
    
    case runPythonLexer "test_lexer.py" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Tokens: " ++ show (length tokens)
            mapM_ (putStrLn . show) tokens