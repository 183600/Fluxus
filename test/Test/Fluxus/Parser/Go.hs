{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Fluxus.Parser.Go (spec) where

import Test.Hspec
import Data.Text (Text)
-- import qualified Data.Text as T

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
        let isKeywordToken (Located _ (GoTokenKeyword _)) = True
            isKeywordToken _ = False
        all (isKeywordToken) tokens `shouldBe` True
  
  it "tokenizes string literals" $ do
    let input = "\"hello world\""
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case tokens of
          (token:_) -> case locatedValue token of
            GoTokenString content -> content `shouldBe` "hello world"
            _ -> expectationFailure "Expected string token"
          [] -> expectationFailure "No tokens produced"
  
  it "tokenizes raw string literals" $ do
    let input = "`hello\nworld`"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case tokens of
          (token:_) -> case locatedValue token of
            GoTokenRawString content -> content `shouldBe` "hello\nworld"
            _ -> expectationFailure "Expected raw string token"
          [] -> expectationFailure "No tokens produced"
  
  it "tokenizes number literals" $ do
    let input = "42 3.14 1e10"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let isNumber (Located _ (GoTokenInt _)) = True
            isNumber (Located _ (GoTokenFloat _)) = True
            isNumber _ = False
        all (isNumber) tokens `shouldBe` True

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
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "test_func"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        case files of
          (file:_) -> do
            let decls = goFileDecls file
            length decls `shouldBe` 1
            case decls of
              (decl:_) -> case locatedValue decl of
                GoFuncDecl func -> case goFuncName func of
                  Just (Identifier name) -> name `shouldBe` "test_func"
                  Nothing -> expectationFailure "Function should have a name"
                _ -> expectationFailure "Expected function declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
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
        case files of
          (file:_) -> do
            let decls = goFileDecls file
            length decls `shouldBe` 1
            case decls of
              (decl:_) -> case locatedValue decl of
                GoVarDecl vars -> length vars `shouldBe` 1
                _ -> expectationFailure "Expected variable declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
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
        case files of
          (file:_) -> do
            let decls = goFileDecls file
            length decls `shouldBe` 1
            case decls of
              (decl:_) -> case locatedValue decl of
                GoTypeDecl (Identifier name) _ -> name `shouldBe` "MyInt"
                _ -> expectationFailure "Expected type declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"

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