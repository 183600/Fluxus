#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)
import Control.Applicative ((<|>), optional)

import Fluxus.Parser.Go.Lexer
import Fluxus.AST.Go
import Fluxus.AST.Common

-- Simple token-by-token debugging
main :: IO ()
main = do
  content <- TIO.readFile "test_minimal.go"
  putStrLn $ "=== File content: ==="
  TIO.putStrLn content
  putStrLn $ "=== Tokenizing ==="
  
  case runGoLexer "test_minimal.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Found " ++ show (length tokens) ++ " tokens:"
      mapM_ (putStrLn . ("  " ++) . show) tokens
      
      putStrLn "\n=== Testing individual token parsing after imports ==="
      testTokenByToken tokens

testTokenByToken :: [Located GoToken] -> IO ()
testTokenByToken tokens = do
  putStrLn "Looking for package declaration..."
  case findPackageEnd tokens of
    Nothing -> putStrLn "No package found"
    Just (packageTokens, afterPackage) -> do
      putStrLn $ "Package tokens: " ++ show (length packageTokens)
      
      putStrLn "Looking for imports..."
      case skipImports afterPackage of
        afterImports -> do
          putStrLn $ "After skipping imports, remaining tokens: " ++ show (length afterImports)
          mapM_ (putStrLn . ("  " ++) . show) (take 10 afterImports)
          
          putStrLn "\nTrying to parse 'func' keyword:"
          case afterImports of
            [] -> putStrLn "No tokens left!"
            (Located _ (GoTokenKeyword GoKwFunc) : rest) -> do
              putStrLn "Found 'func' keyword!"
              case rest of
                [] -> putStrLn "No tokens after 'func'!"
                (Located _ (GoTokenIdent name) : rest2) -> do
                  putStrLn $ "Found identifier: " ++ T.unpack name
                  case rest2 of
                    [] -> putStrLn "No tokens after identifier!"
                    (Located _ (GoTokenDelimiter GoDelimLeftParen) : rest3) -> do
                      putStrLn "Found left paren!"
                      case rest3 of
                        [] -> putStrLn "No tokens after left paren!"
                        (Located _ (GoTokenDelimiter GoDelimRightParen) : rest4) -> do
                          putStrLn "Found right paren!"
                          case rest4 of
                            [] -> putStrLn "No tokens after right paren!"
                            (Located _ (GoTokenDelimiter GoDelimLeftBrace) : rest5) -> do
                              putStrLn "Found left brace!"
                              case rest5 of
                                [] -> putStrLn "No tokens after left brace!"
                                (Located _ (GoTokenDelimiter GoDelimRightBrace) : _) -> do
                                  putStrLn "Found right brace! Function structure is correct!"
                                  putStrLn "This means the tokens are fine, the parser logic has an issue."
                                other -> putStrLn $ "Expected right brace, found: " ++ show (head other)
                            other -> putStrLn $ "Expected left brace, found: " ++ show (head other)
                        other -> putStrLn $ "Expected right paren, found: " ++ show (head other)
                    other -> putStrLn $ "Expected left paren, found: " ++ show (head other)
                other -> putStrLn $ "Expected identifier, found: " ++ show (head other)
            other -> putStrLn $ "Expected 'func' keyword, found: " ++ show (head other)

findPackageEnd :: [Located GoToken] -> Maybe ([Located GoToken], [Located GoToken])
findPackageEnd tokens = go tokens []
  where
    go [] acc = Nothing
    go (t@(Located _ (GoTokenKeyword GoKwPackage)) : rest) acc = 
      case rest of
        (Located _ (GoTokenIdent _) : afterName) -> 
          case skipNewlinesAndComments afterName of
            remaining -> Just (reverse (t : acc), remaining)
        _ -> go rest (t : acc)
    go (t : rest) acc = go rest (t : acc)

skipImports :: [Located GoToken] -> [Located GoToken]
skipImports tokens = go (skipNewlinesAndComments tokens)
  where
    go (Located _ (GoTokenKeyword GoKwImport) : rest) = 
      go (skipImportsImpl rest)
    go other = other
    
    skipImportsImpl (Located _ (GoTokenDelimiter GoDelimLeftParen) : rest) =
      skipNewlinesAndComments (skipUntilRightParen rest)
    skipImportsImpl (Located _ (GoTokenString _) : rest) = 
      skipNewlinesAndComments rest
    skipImportsImpl (Located _ (GoTokenRawString _) : rest) = 
      skipNewlinesAndComments rest
    skipImportsImpl other = skipNewlinesAndComments other

skipUntilRightParen :: [Located GoToken] -> [Located GoToken]
skipUntilRightParen [] = []
skipUntilRightParen (Located _ (GoTokenDelimiter GoDelimRightParen) : rest) = rest
skipUntilRightParen (_ : rest) = skipUntilRightParen rest

skipNewlinesAndComments :: [Located GoToken] -> [Located GoToken]
skipNewlinesAndComments [] = []
skipNewlinesAndComments (Located _ GoTokenNewline : rest) = skipNewlinesAndComments rest
skipNewlinesAndComments (Located _ (GoTokenComment _) : rest) = skipNewlinesAndComments rest
skipNewlinesAndComments other = other