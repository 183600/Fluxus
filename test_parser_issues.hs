#!/usr/bin/env stack
-- stack --resolver lts-20.3 script --package fluxus

import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    content <- TIO.readFile "test_real_python.py"
    putStrLn "=== Testing Python Parser ==="
    putStrLn "Content:"
    TIO.putStrLn content
    putStrLn "\n=== Lexer Test ==="
    case runPythonLexer "test_real_python.py" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Tokens: " ++ show (length tokens)
            mapM_ print (take 20 tokens)
            putStrLn "\n=== Parser Test ==="
            case runPythonParser "test_real_python.py" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> do
                    putStrLn "Parser succeeded!"
                    print ast