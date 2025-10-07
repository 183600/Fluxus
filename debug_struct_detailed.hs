{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Parser.Go.Parser
import Fluxus.Parser.Go.Lexer
import Fluxus.AST.Go
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  -- 测试1：原始测试用例（带分号）
  putStrLn "=== Test 1: Original test case with semicolons ==="
  testWithTokens mockTokensOriginal
  
  -- 测试2：修改后的测试用例（不带分号，更符合实际Go语法）
  putStrLn "\n=== Test 2: Modified test case without semicolons ==="
  testWithTokens mockTokensModified
  
  -- 测试3：实际Go代码解析
  putStrLn "\n=== Test 3: Real Go code parsing ==="
  let goCode = T.unlines
        [ "package main"
        , ""
        , "type Person struct {"
        , "    Name string"
        , "    Age int"
        , "}"
        ]
  case runGoLexer "test.go" goCode of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens -> testWithTokens tokens

mockTokensOriginal :: [Located GoToken]
mockTokensOriginal = 
  [ Located (NodeAnn (Just (Span (Position 1 1) (Position 1 8))) [] []) (GoTokenKeyword GoKwPackage)
  , Located (NodeAnn (Just (Span (Position 1 9) (Position 1 13))) [] []) (GoTokenIdent "main")
  , Located (NodeAnn (Just (Span (Position 1 13) (Position 2 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 2 1) (Position 3 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 3 1) (Position 3 5))) [] []) (GoTokenKeyword GoKwType)
  , Located (NodeAnn (Just (Span (Position 3 6) (Position 3 12))) [] []) (GoTokenIdent "Person")
  , Located (NodeAnn (Just (Span (Position 3 13) (Position 3 19))) [] []) (GoTokenKeyword GoKwStruct)
  , Located (NodeAnn (Just (Span (Position 3 20) (Position 3 21))) [] []) (GoTokenDelimiter GoDelimLeftBrace)
  , Located (NodeAnn (Just (Span (Position 3 21) (Position 4 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 4 5) (Position 4 9))) [] []) (GoTokenIdent "Name")
  , Located (NodeAnn (Just (Span (Position 4 10) (Position 4 16))) [] []) (GoTokenIdent "string")
  , Located (NodeAnn (Just (Span (Position 4 16) (Position 5 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 5 5) (Position 5 8))) [] []) (GoTokenIdent "Age")
  , Located (NodeAnn (Just (Span (Position 5 9) (Position 5 12))) [] []) (GoTokenIdent "int")
  , Located (NodeAnn (Just (Span (Position 5 12) (Position 6 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 6 1) (Position 6 2))) [] []) (GoTokenDelimiter GoDelimRightBrace)
  , Located (NodeAnn (Just (Span (Position 6 2) (Position 7 1))) [] []) GoTokenNewline
  ]

mockTokensModified :: [Located GoToken]
mockTokensModified = 
  [ Located (NodeAnn (Just (Span (Position 1 1) (Position 1 8))) [] []) (GoTokenKeyword GoKwPackage)
  , Located (NodeAnn (Just (Span (Position 1 9) (Position 1 13))) [] []) (GoTokenIdent "main")
  , Located (NodeAnn (Just (Span (Position 1 13) (Position 2 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 2 1) (Position 3 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 3 1) (Position 3 5))) [] []) (GoTokenKeyword GoKwType)
  , Located (NodeAnn (Just (Span (Position 3 6) (Position 3 12))) [] []) (GoTokenIdent "Person")
  , Located (NodeAnn (Just (Span (Position 3 13) (Position 3 19))) [] []) (GoTokenKeyword GoKwStruct)
  , Located (NodeAnn (Just (Span (Position 3 20) (Position 3 21))) [] []) (GoTokenDelimiter GoDelimLeftBrace)
  , Located (NodeAnn (Just (Span (Position 3 21) (Position 4 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 4 5) (Position 4 9))) [] []) (GoTokenIdent "Name")
  , Located (NodeAnn (Just (Span (Position 4 10) (Position 4 16))) [] []) (GoTokenIdent "string")
  , Located (NodeAnn (Just (Span (Position 4 16) (Position 5 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 5 5) (Position 5 8))) [] []) (GoTokenIdent "Age")
  , Located (NodeAnn (Just (Span (Position 5 9) (Position 5 12))) [] []) (GoTokenIdent "int")
  , Located (NodeAnn (Just (Span (Position 5 12) (Position 6 1))) [] []) GoTokenNewline
  , Located (NodeAnn (Just (Span (Position 6 1) (Position 6 2))) [] []) (GoTokenDelimiter GoDelimRightBrace)
  , Located (NodeAnn (Just (Span (Position 6 2) (Position 7 1))) [] []) GoTokenNewline
  ]

testWithTokens :: [Located GoToken] -> IO ()
testWithTokens tokens = do
  putStrLn "Tokens:"
  mapM_ (putStrLn . show . locValue) tokens
  
  case runGoParser "test.go" tokens of
    Left err -> putStrLn $ "Parser failed: " ++ show err
    Right ast -> do
      putStrLn "Parser succeeded!"
      print ast