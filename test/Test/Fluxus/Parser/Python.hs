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

safeHead :: [a] -> a
safeHead [] = error "safeHead: empty list (test invariant violated)"
safeHead (x:_) = x

spec :: Spec
spec = describe "Python Parser" $ do
  lexerSpec
  lexerTokensSpec
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
        case locatedValue (safeHead tokens) of
          TokenString content -> content `shouldBe` "hello world"
          _ -> expectationFailure "Expected string token"
  
  it "tokenizes number literals" $ do
    let input = "42 3.14 1e10"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let isNumber (Located _ (TokenNumber _ _)) = True
            isNumber _ = False
        all (isNumber) tokens `shouldBe` True

lexerTokensSpec :: Spec
lexerTokensSpec = describe "Python Lexer - tokens coverage" $ do
  it "tokenizes identifiers" $ do
    let input = "x _tmp Alpha123"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map (locatedValue) toks `shouldBe`
          [ TokenIdent "x"
          , TokenIdent "_tmp"
          , TokenIdent "Alpha123"
          ]

  it "tokenizes common keywords" $ do
    let input = "def class if else for while return import from"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        let isKw (TokenKeyword _) = True
            isKw _ = False
        all (isKw . locatedValue) toks `shouldBe` True

  it "tokenizes arithmetic operators" $ do
    let input = "+ - * / % ** //"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenOperator Fluxus.Parser.Python.Lexer.OpPlus
          , TokenOperator Fluxus.Parser.Python.Lexer.OpMinus
          , TokenOperator Fluxus.Parser.Python.Lexer.OpMult
          , TokenOperator Fluxus.Parser.Python.Lexer.OpDiv
          , TokenOperator Fluxus.Parser.Python.Lexer.OpMod
          , TokenOperator Fluxus.Parser.Python.Lexer.OpPower
          , TokenOperator Fluxus.Parser.Python.Lexer.OpFloorDiv
          ]

  it "tokenizes comparison operators" $ do
    let input = "== != < > <= >="
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenOperator Fluxus.Parser.Python.Lexer.OpEq
          , TokenOperator Fluxus.Parser.Python.Lexer.OpNe
          , TokenOperator Fluxus.Parser.Python.Lexer.OpLt
          , TokenOperator Fluxus.Parser.Python.Lexer.OpGt
          , TokenOperator Fluxus.Parser.Python.Lexer.OpLe
          , TokenOperator Fluxus.Parser.Python.Lexer.OpGe
          ]

  it "tokenizes logical operators" $ do
    let input = "and or not"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        map locatedValue toks `shouldBe`
          [ TokenKeyword KwAnd
          , TokenKeyword KwOr
          , TokenKeyword KwNot
          ]
        -- ensure identifiers that include keywords are not mis-lexed
        let input2 = "andx oran noty"
        case runPythonLexer "test.py" input2 of
          Left _ -> expectationFailure "Lexer failed"
          Right toks2 ->
            map locatedValue toks2 `shouldBe`
              [ TokenIdent "andx"
              , TokenIdent "oran"
              , TokenIdent "noty"
              ]

  it "tokenizes assignment/walrus/arrow/ellipsis" $ do
    let input = "= := -> ..."
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenOperator Fluxus.Parser.Python.Lexer.OpAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpWalrus
          , TokenOperator Fluxus.Parser.Python.Lexer.OpArrow
          , TokenOperator Fluxus.Parser.Python.Lexer.OpEllipsis
          ]

  it "tokenizes @ delimiter and decorators" $ do
    let input = "@decorator\n@decorator.with.args"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        -- Expect: '@' delimiter tokens and identifiers/dots
        map locatedValue toks `shouldBe`
          [ TokenDelimiter DelimAt
          , TokenIdent "decorator"
          , TokenNewline
          , TokenDelimiter DelimAt
          , TokenIdent "decorator"
          , TokenDelimiter DelimDot
          , TokenKeyword KwWith
          , TokenDelimiter DelimDot
          , TokenIdent "args"
          ]

  it "tokenizes compound assignments" $ do
    let input = "+= -= *= /= %= **= //= <<= >>="
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenOperator Fluxus.Parser.Python.Lexer.OpPlusAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpMinusAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpMultAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpDivAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpModAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpPowerAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpFloorDivAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpLeftShiftAssign
          , TokenOperator Fluxus.Parser.Python.Lexer.OpRightShiftAssign
          ]

  it "tokenizes number literals (dec/hex/oct/bin/float/exp)" $ do
    let input = "123 0xFF 0o77 0b1010 3.14 1e10 2.5e-3"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenNumber "123" False
          , TokenNumber "0xFF" False
          , TokenNumber "0o77" False
          , TokenNumber "0b1010" False
          , TokenNumber "3.14" True
          , TokenNumber "1e10" True
          , TokenNumber "2.5e-3" True
          ]

  it "tokenizes bytes literals (b/B prefixes)" $ do
    let input = "b'abc' B\"def\""
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenBytes "abc"
          , TokenBytes "def"
          ]

  it "tokenizes strings and f-strings" $ do
    let input = "\"hello\" f\"world\""
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        map locatedValue toks `shouldBe`
          [ TokenString "hello"
          , TokenFString "world" []
          ]
        -- triple-quoted strings
        let input2 = "\"\"\"multi\nline\"\"\""
        case runPythonLexer "test.py" input2 of
          Left _ -> expectationFailure "Lexer failed"
          Right toks2 ->
            map locatedValue toks2 `shouldBe`
              [ TokenString "multi\nline" ]

  it "tokenizes bytes literals" $ do
    let input = "b'abc' B\"def\""
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenBytes "abc"
          , TokenBytes "def"
          ]

  it "tokenizes delimiters" $ do
    let input = "( ) [ ] { } , : ; . @"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenDelimiter DelimLeftParen
          , TokenDelimiter DelimRightParen
          , TokenDelimiter DelimLeftBracket
          , TokenDelimiter DelimRightBracket
          , TokenDelimiter DelimLeftBrace
          , TokenDelimiter DelimRightBrace
          , TokenDelimiter DelimComma
          , TokenDelimiter DelimColon
          , TokenDelimiter DelimSemicolon
          , TokenDelimiter DelimDot
          , TokenDelimiter DelimAt
          ]

  it "tokenizes parentheses, brackets and braces around identifiers" $ do
    let input = "(x) [y] {z}"
    case runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ TokenDelimiter DelimLeftParen
          , TokenIdent "x"
          , TokenDelimiter DelimRightParen
          , TokenDelimiter DelimLeftBracket
          , TokenIdent "y"
          , TokenDelimiter DelimRightBracket
          , TokenDelimiter DelimLeftBrace
          , TokenIdent "z"
          , TokenDelimiter DelimRightBrace
          ]

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
        length (pyModuleBody module_) `shouldBe` 1
        case locatedValue (safeHead (pyModuleBody module_)) of
          PyFuncDef funcDef -> pyFuncName funcDef `shouldBe` Identifier "test_func"
          _ -> expectationFailure "Expected function definition"
  
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
        length (pyModuleBody module_) `shouldBe` 1
        case locatedValue (safeHead (pyModuleBody module_)) of
          PyClassDef classDef -> pyClassName classDef `shouldBe` Identifier "TestClass"
          _ -> expectationFailure "Expected class definition"
  
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
        length (pyModuleBody module_) `shouldBe` 1
        case locatedValue (safeHead (pyModuleBody module_)) of
          PyIf _ _ _ -> return ()
          _ -> expectationFailure "Expected if statement"

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