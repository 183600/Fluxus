{-# LANGUAGE OverloadedStrings #-}
module TestListParsing where

import Test.Hspec
import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer as Lexer
import Fluxus.Parser.Python.Parser as Parser
import Fluxus.AST.Python

spec :: Spec
spec = describe "List Parsing Debug" $ do
  it "parses simple list [1] with mock tokens" $ do
    let tokens = mockTokens 
          [ Lexer.TokenDelimiter Lexer.DelimLeftBracket
          , Lexer.TokenNumber "1" False
          , Lexer.TokenDelimiter Lexer.DelimRightBracket
          , Lexer.TokenNewline
          ]
    case Parser.runPythonParser "test.py" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        
mockTokens :: [Lexer.PythonToken] -> [Located Lexer.PythonToken]
mockTokens = map mockToken

mockToken :: Lexer.PythonToken -> Located Lexer.PythonToken
mockToken token = Located mockSpan token
  where
    mockSpan = SourceSpan "test.py" (SourcePos 1 1) (SourcePos 1 10)