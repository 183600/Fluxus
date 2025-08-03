{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go
import Fluxus.AST.Common
import Text.Megaparsec (parse, parseTest, getInput, many)
import Control.Monad (void)
import Control.Applicative ((<*))

main :: IO ()
main = do
    content <- TIO.readFile "test_go_function_issue.go"
    putStrLn "=== Debug parseDeclaration Problem ==="
    TIO.putStrLn content
    putStrLn ""
    
    case runGoLexer "test.go" content of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "=== Tokenization successful: " ++ show (length tokens) ++ " tokens ==="
            
            -- First, parse package and imports successfully
            case parse parsePackageAndImports "test" tokens of
                Left err -> putStrLn $ "Package/Import parse error: " ++ show err
                Right (remainingTokens, packageName, imports) -> do
                    putStrLn $ "✅ Package: " ++ show packageName
                    putStrLn $ "✅ Imports: " ++ show (length imports) ++ " imports"
                    putStrLn $ "Remaining tokens for declarations: " ++ show (length remainingTokens)
                    putStrLn ""
                    
                    -- Now debug declaration parsing
                    putStrLn "=== First 10 remaining tokens ==="
                    mapM_ (putStrLn . ("  " ++) . show) (take 10 remainingTokens)
                    putStrLn ""
                    
                    putStrLn "=== Attempting to parse first declaration ==="
                    case parse (parseDeclaration <* skipCommentsAndNewlines) "test" remainingTokens of
                        Left err -> do
                            putStrLn $ "❌ Declaration parse error: " ++ show err
                            putStrLn ""
                            
                            -- Try each individual declaration parser
                            putStrLn "=== Testing individual parsers ==="
                            testParser "parseFuncDecl" parseFuncDecl remainingTokens
                            testParser "parseTypeDecl" parseTypeDecl remainingTokens
                            testParser "parseVarDecl" parseVarDecl remainingTokens
                            testParser "parseConstDecl" parseConstDecl remainingTokens
                            
                        Right decl -> do
                            putStrLn $ "✅ First declaration parsed: " ++ show decl
                            
                            -- Try to parse remaining declarations
                            let tokensAfterFirst = dropWhile (not . isFunc) remainingTokens
                            if null tokensAfterFirst
                                then putStrLn "No more tokens after first declaration"
                                else do
                                    putStrLn "=== Attempting to parse second declaration ==="
                                    putStrLn $ "Tokens for second declaration: " ++ show (length tokensAfterFirst)
                                    case parse parseDeclaration "test" tokensAfterFirst of
                                        Left err2 -> putStrLn $ "❌ Second declaration error: " ++ show err2
                                        Right decl2 -> putStrLn $ "✅ Second declaration: " ++ show decl2

  where
    -- Custom parser to get package, imports, and remaining tokens
    parsePackageAndImports = do
        skipCommentsAndNewlines
        void $ goKeywordP GoKwPackage
        packageName <- parseGoIdentifier
        skipCommentsAndNewlines
        
        imports <- many (parseImportDecl <* skipCommentsAndNewlines)
        skipCommentsAndNewlines
        
        remainingTokens <- getInput
        return (remainingTokens, packageName, concat imports)
    
    testParser name parser tokens = do
        putStrLn $ "  Testing " ++ name ++ ":"
        case parse parser "test" tokens of
            Left _ -> putStrLn $ "    ❌ " ++ name ++ " failed"
            Right result -> putStrLn $ "    ✅ " ++ name ++ " succeeded: " ++ take 100 (show result) ++ "..."
    
    isFunc (Located _ (GoTokenKeyword GoKwFunc)) = True
    isFunc _ = False