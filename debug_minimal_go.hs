{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
    content <- TIO.readFile "minimal_debug_test.go"
    putStrLn "=== File Content ==="
    TIO.putStrLn content
    putStrLn "\n=== Lexing ==="
    case runGoLexer "minimal_debug_test.go" content of
        Left err -> putStrLn $ "Lexing error: " ++ show err
        Right tokens -> do
            putStrLn $ "Found " ++ show (length tokens) ++ " tokens:"
            mapM_ (putStrLn . show) (take 15 tokens)
            putStrLn "\n=== Parsing ==="
            case runGoParser "minimal_debug_test.go" tokens of
                Left err -> putStrLn $ "Parsing error: " ++ show err
                Right (GoAST package_) -> do
                    putStrLn $ "Package: " ++ show (goPackageName package_)
                    let files = goPackageFiles package_
                    putStrLn $ "Files: " ++ show (length files)
                    mapM_ (\file -> do
                        let decls = goFileDecls file
                        putStrLn $ "  File has " ++ show (length decls) ++ " declarations:"
                        mapM_ (putStrLn . ("    " ++) . show) (take 5 decls)
                      ) files