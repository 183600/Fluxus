{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Common
import Fluxus.AST.Go
import Control.Monad (void, when)
import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Data.Void

main :: IO ()
main = do
    content <- TIO.readFile "simple_test.go"
    putStrLn "=== Original Go Content ==="
    TIO.putStrLn content
    putStrLn ""
    
    case runGoLexer "simple_test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Total tokens: " ++ show (length tokens)
            
            -- Try to parse the entire file normally
            putStrLn "=== Testing Full File Parse ==="
            case runGoParser "simple_test.go" tokens of
                Left err -> putStrLn $ "Full parse error: " ++ T.unpack (peMessage err) ++ " at " ++ show (peLocation err)
                Right ast -> do
                    putStrLn $ "Full parse success!"
                    putStrLn $ "Package: " ++ show (goPackageName (goPackage ast))
                    case goPackageFiles (goPackage ast) of
                        [] -> putStrLn "No files in package"
                        (file:_) -> do
                            putStrLn $ "File: " ++ T.unpack (goFileName file)
                            putStrLn $ "Declarations: " ++ show (length (goFileDecls file))
                            putStrLn $ "Imports: " ++ show (length (goFileImports file))
                            
                            -- Show what tokens are at each position
                            putStrLn "\n=== Token Positions ==="
                            putStrLn "All tokens:"
                            mapM_ (\(i, token) -> putStrLn $ show i ++ ": " ++ show token) (zip [0..] tokens)
                            
                            -- Try to find where the func declaration is
                            let funcIndex = findIndex (\(Located _ t) -> t == GoTokenKeyword GoKwFunc) tokens
                            case funcIndex of
                                Nothing -> putStrLn "No 'func' keyword found in tokens!"
                                Just idx -> do
                                    putStrLn $ "'func' keyword found at position " ++ show idx
                                    
                                    -- Try to parse just the function declaration from that position
                                    let funcTokens = drop idx tokens
                                    putStrLn $ "Tokens from func position: " ++ show (length funcTokens)
                                    case runParser parseFuncDeclDirect "test" funcTokens of
                                        Left err -> putStrLn $ "Direct func parse error: " ++ show err
                                        Right result -> putStrLn $ "Direct func parse success: " ++ show result
  where
    parseFuncDeclDirect :: Parsec Void [Located GoToken] GoDecl
    parseFuncDeclDirect = Fluxus.Parser.Go.Parser.parseFuncDecl
    
    findIndex _ [] = Nothing
    findIndex p (x:xs) = if p x then Just 0 else fmap (+1) (findIndex p xs)