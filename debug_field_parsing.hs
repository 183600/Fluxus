{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Parser.Go.Parser
import Fluxus.Parser.Go.Lexer
import Fluxus.AST.Go
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

main :: IO ()
main = do
  -- 测试最简单的struct解析
  putStrLn "=== Test: Simple struct field parsing ==="
  
  -- 只测试struct内部的字段解析
  let fieldTokens = 
        [ Located (NodeAnn (Just (Span (Position 1 5) (Position 1 9))) [] []) (GoTokenIdent "Name")
        , Located (NodeAnn (Just (Span (Position 1 10) (Position 1 16))) [] []) (GoTokenIdent "string")
        , Located (NodeAnn (Just (Span (Position 1 16) (Position 2 1))) [] []) GoTokenNewline
        ]
  
  putStrLn "Field tokens:"
  mapM_ (putStrLn . show . locValue) fieldTokens
  
  -- 尝试直接解析字段
  case MP.runParser parseFieldDecl "test" fieldTokens of
    Left err -> putStrLn $ "Field parsing failed: " ++ show err
    Right fields -> do
      putStrLn "Field parsing succeeded!"
      print fields
  
  -- 测试标识符列表解析
  putStrLn "\n=== Test: Identifier list parsing ==="
  let identTokens = 
        [ Located (NodeAnn (Just (Span (Position 1 1) (Position 1 5))) [] []) (GoTokenIdent "Name")
        ]
  
  case MP.runParser parseIdentifierList "test" identTokens of
    Left err -> putStrLn $ "Identifier list parsing failed: " ++ show err
    Right idents -> do
      putStrLn "Identifier list parsing succeeded!"
      print idents