{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

main :: IO ()
main = do
    let goCode = "package main\n\nfunc main() {\n}\n"
    putStrLn "Go code:"
    putStrLn (T.unpack goCode)
    putStrLn "\nTokens:"
    case runGoLexer "test.go" goCode of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            mapM_ (putStrLn . show) tokens
            putStrLn "\nParsing:"
            case runGoParser "test.go" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right ast -> putStrLn $ "Success: " ++ show ast