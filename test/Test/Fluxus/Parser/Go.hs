{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Parser.Go (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go
import Fluxus.AST.Common

spec :: Spec
spec = describe "Go Parser" $ do
  lexerSpec
  parserSpec

lexerSpec :: Spec
lexerSpec = describe "Go Lexer" $ do
  it "tokenizes simple expressions" $ do
    let input = "x + 42"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let tokenValues = map (locatedValue . fmap goTokenValue) tokens
        tokenValues `shouldBe` ["x", "+", "42"]
  
  it "tokenizes Go keywords" $ do
    let input = "package func if else for"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 5
        let isKeyword (Located _ (GoTokenKeyword _)) = True
            isKeyword _ = False
        all (isKeyword . locatedValue) tokens `shouldBe` True
  
  it "tokenizes string literals" $ do
    let input = "\"hello world\""
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case locatedValue (head tokens) of
          GoTokenString content -> content `shouldBe` "hello world"
          _ -> expectationFailure "Expected string token"
  
  it "tokenizes raw string literals" $ do
    let input = "`hello\nworld`"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case locatedValue (head tokens) of
          GoTokenRawString content -> content `shouldBe` "hello\nworld"
          _ -> expectationFailure "Expected raw string token"
  
  it "tokenizes number literals" $ do
    let input = "42 3.14 1e10"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let isNumber (Located _ (GoTokenInt _)) = True
            isNumber (Located _ (GoTokenFloat _)) = True
            isNumber _ = False
        all (isNumber . locatedValue) tokens `shouldBe` True

parserSpec :: Spec
parserSpec = describe "Go Parser" $ do
  it "parses package declaration" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "main"
  
  it "parses function declarations" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "test_func"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (head files)
        length decls `shouldBe` 1
        case locatedValue (head decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "test_func"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses variable declarations" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenKeyword GoKwVar
          , GoTokenIdent "x"
          , GoTokenIdent "int"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (head files)
        length decls `shouldBe` 1
        case locatedValue (head decls) of
          GoVarDecl vars -> length vars `shouldBe` 1
          _ -> expectationFailure "Expected variable declaration"
  
  it "parses type declarations" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenKeyword GoKwType
          , GoTokenIdent "MyInt"
          , GoTokenIdent "int"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (head files)
        length decls `shouldBe` 1
        case locatedValue (head decls) of
          GoTypeDecl (Identifier name) _ -> name `shouldBe` "MyInt"
          _ -> expectationFailure "Expected type declaration"

-- Helper functions
mockGoTokens :: [GoToken] -> [Located GoToken]
mockGoTokens = map mockGoToken

mockGoToken :: GoToken -> Located GoToken
mockGoToken token = Located mockSpan token
  where
    mockSpan = SourceSpan "test.go" (SourcePos 1 1) (SourcePos 1 10)

goTokenValue :: GoToken -> Text
goTokenValue = \case
  GoTokenKeyword kw -> goKeywordToText kw
  GoTokenIdent name -> name
  GoTokenString str -> str
  GoTokenInt num -> num
  GoTokenFloat num -> num
  GoTokenOperator op -> case op of
    GoOpPlus -> "+"
    GoOpMinus -> "-"
    GoOpMult -> "*"
    GoOpDiv -> "/"
    GoOpAssign -> "="
    _ -> "op"
  GoTokenDelimiter delim -> case delim of
    GoDelimLeftParen -> "("
    GoDelimRightParen -> ")"
    GoDelimLeftBrace -> "{"
    GoDelimRightBrace -> "}"
    _ -> "delim"
  _ -> "token"