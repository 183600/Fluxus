{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Fluxus.AST.Go (Located(..), NodeAnn(..), Position(..), Span(..))

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser

mockGoTokens :: [GoToken] -> [Located GoToken]
mockGoTokens = map mockGoToken
mockGoToken :: GoToken -> Located GoToken
mockGoToken token = Located mockNodeAnn token
  where
    mockPosition = Position { posLine = 1, posColumn = 1 }
    mockSpan = Just $ Span { spanStart = mockPosition, spanEnd = mockPosition { posColumn = 10 } }
    mockNodeAnn = NodeAnn { annSpan = mockSpan, annLeading = [], annTrailing = [] }

main :: IO ()
main = do
  let tokens = mockGoTokens
        [ GoTokenKeyword GoKwPackage
        , GoTokenIdent "main"
        , GoTokenNewline
        , GoTokenKeyword GoKwType
        , GoTokenIdent "Person"
        , GoTokenKeyword GoKwStruct
        , GoTokenDelimiter GoDelimLeftBrace
        , GoTokenIdent "Name"
        , GoTokenIdent "string"
        , GoTokenDelimiter GoDelimSemicolon
        , GoTokenIdent "Age"
        , GoTokenIdent "int"
        , GoTokenDelimiter GoDelimSemicolon
        , GoTokenDelimiter GoDelimRightBrace
        ]

  putStrLn "=== Testing Go Parser ==="
  putStrLn $ "Input tokens: " ++ show (map (goTokenValue . locatedValue) tokens)

  case runGoParser "test.go" tokens of
    Left err -> do
      putStrLn "Parser failed with error:"
      print err
    Right ast -> do
      putStrLn "Parser succeeded!"
      print ast