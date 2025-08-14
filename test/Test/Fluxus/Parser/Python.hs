{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Fluxus.Parser.Python (spec) where

import Test.Hspec
import Data.Text (Text)
-- import qualified Data.Text as T

import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.AST.Common

spec :: Spec
spec = describe "Python Parser" $ do
  lexerSpec
  parserSpec

lexerSpec :: Spec
lexerSpec = describe "Python Lexer" $ do
  it "tokenizes simple expressions" $ do
    let input = "x + 42"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let tokenValues = map (locatedValue . fmap tokenValue) tokens
        tokenValues `shouldBe` ["x", "+", "42"]
  
  it "tokenizes Python keywords" $ do
    let input = "def if else for while"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 5
        let isKeywordToken (Located _ (TokenKeyword _)) = True
            isKeywordToken _ = False
        all (isKeywordToken) tokens `shouldBe` True
  
  it "tokenizes string literals" $ do
    let input = "\"hello world\""
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case tokens of
          (token:_) -> case locatedValue token of
            TokenString content -> content `shouldBe` "hello world"
            _ -> expectationFailure "Expected string token"
          [] -> expectationFailure "No tokens produced"
  
  it "tokenizes number literals" $ do
    let input = "42 3.14 1e10"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let isNumber (Located _ (TokenNumber _ _)) = True
            isNumber _ = False
        all (isNumber) tokens `shouldBe` True

parserSpec :: Spec
parserSpec = describe "Python Parser" $ do
  it "parses simple expressions" $ do
    let tokens = mockTokens [TokenIdent "x", TokenOperator OpPlus, TokenNumber "42" False, TokenNewline]
    case runPythonParser "test.py" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let PythonAST module_ = ast
        length (pyModuleBody module_) `shouldBe` 1
  
  it "parses function definitions" $ do
    let tokens = mockTokens 
          [ TokenKeyword KwDef
          , TokenIdent "test_func"
          , TokenDelimiter DelimLeftParen
          , TokenDelimiter DelimRightParen
          , TokenDelimiter DelimColon
          , TokenNewline
          , TokenIndent 1
          , TokenKeyword KwPass
          , TokenNewline
          , TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyFuncDef funcDef -> pyFuncName funcDef `shouldBe` Identifier "test_func"
            _ -> expectationFailure "Expected function definition"
          [] -> expectationFailure "Module body is empty"
  
  it "parses class definitions" $ do
    let tokens = mockTokens 
          [ TokenKeyword KwClass
          , TokenIdent "TestClass"
          , TokenDelimiter DelimColon
          , TokenNewline
          , TokenIndent 1
          , TokenKeyword KwPass
          , TokenNewline
          , TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyClassDef classDef -> pyClassName classDef `shouldBe` Identifier "TestClass"
            _ -> expectationFailure "Expected class definition"
          [] -> expectationFailure "Module body is empty"
  
  it "parses if statements" $ do
    let tokens = mockTokens 
          [ TokenKeyword KwIf
          , TokenKeyword KwTrue
          , TokenDelimiter DelimColon
          , TokenNewline
          , TokenIndent 1
          , TokenKeyword KwPass
          , TokenNewline
          , TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyIf _ _ _ -> return ()
            _ -> expectationFailure "Expected if statement"
          [] -> expectationFailure "Module body is empty"

-- Helper functions
mockTokens :: [PythonToken] -> [Located PythonToken]
mockTokens = map mockToken

mockToken :: PythonToken -> Located PythonToken
mockToken token = Located mockSpan token
  where
    mockSpan = SourceSpan "test.py" (SourcePos 1 1) (SourcePos 1 10)

tokenValue :: PythonToken -> Text
tokenValue = \case
  TokenKeyword kw -> keywordToText kw
  TokenIdent name -> name
  TokenString str -> str
  TokenNumber num _ -> num
  TokenOperator op -> case op of
    Fluxus.Parser.Python.Lexer.OpPlus -> "+"
    Fluxus.Parser.Python.Lexer.OpMinus -> "-"
    Fluxus.Parser.Python.Lexer.OpMult -> "*"
    Fluxus.Parser.Python.Lexer.OpDiv -> "/"
    Fluxus.Parser.Python.Lexer.OpAssign -> "="
    _ -> "op"
  TokenDelimiter delim -> case delim of
    DelimLeftParen -> "("
    DelimRightParen -> ")"
    DelimColon -> ":"
    _ -> "delim"
  _ -> "token"