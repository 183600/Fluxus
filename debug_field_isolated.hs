{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Parser.Go.Parser
import Fluxus.Parser.Go.Lexer
import Fluxus.AST.Go
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

main :: IO ()
main = do
  -- 测试1：只解析struct内部的字段，不包含package和type声明
  putStrLn "=== Test 1: Parse struct fields only ==="
  
  let fieldTokens = 
        [ Located (NodeAnn (Just (Span (Position 1 1) (Position 1 5))) [] []) (GoTokenIdent "Name")
        , Located (NodeAnn (Just (Span (Position 1 6) (Position 1 12))) [] []) (GoTokenIdent "string")
        , Located (NodeAnn (Just (Span (Position 1 12) (Position 2 1))) [] []) GoTokenNewline
        , Located (NodeAnn (Just (Span (Position 2 1) (Position 2 2))) [] []) (GoTokenDelimiter GoDelimRightBrace)
        ]
  
  putStrLn "Field tokens:"
  mapM_ (putStrLn . show . locValue) fieldTokens
  
  -- 我们需要创建一个简化的解析器来测试字段解析
  let result = MP.runParser (many parseFieldDecl <* goDelimiterP GoDelimRightBrace) "test" fieldTokens
  case result of
    Left err -> putStrLn $ "Field parsing failed: " ++ show err
    Right fields -> do
      putStrLn "Field parsing succeeded!"
      print fields
  
  -- 测试2：测试标识符列表解析
  putStrLn "\n=== Test 2: Parse identifier list only ==="
  
  let identTokens = 
        [ Located (NodeAnn (Just (Span (Position 1 1) (Position 1 5))) [] []) (GoTokenIdent "Name")
        , Located (NodeAnn (Just (Span (Position 1 6) (Position 1 12))) [] []) (GoTokenIdent "string")
        ]
  
  putStrLn "Identifier tokens:"
  mapM_ (putStrLn . show . locValue) identTokens
  
  let result2 = MP.runParser parseIdentifierList "test" identTokens
  case result2 of
    Left err -> putStrLn $ "Identifier list parsing failed: " ++ show err
    Right idents -> do
      putStrLn "Identifier list parsing succeeded!"
      print idents