{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Fluxus.Parser.Go (spec) where

import Test.Hspec
import Data.Text (Text)

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go

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
        let tokenValues = map (goTokenValue . locatedValue) tokens
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

  it "tokenizes Go operators" $ do
    let input = "+ - * / % == != < > <= >= && || ! & | ^ << >>"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        let isOperator (Located _ (GoTokenOperator _)) = True
            isOperator _ = False
        all (isOperator) tokens `shouldBe` True
  
  it "tokenizes Go delimiters" $ do
    let input = "( ) { } [ ] , ; . :"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        let isDelimiter (Located _ (GoTokenDelimiter _)) = True
            isDelimiter _ = False
        all (isDelimiter) tokens `shouldBe` True
  
  it "tokenizes Go comments" $ do
    let input = "// This is a line comment\n/* This is a block comment */"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        let hasLineComment = any (\case
              Located _ (GoTokenComment _) -> True
              _ -> False) tokens
        let hasBlockComment = any (\case
              Located _ (GoTokenComment _) -> True
              _ -> False) tokens
        hasLineComment `shouldBe` True
        hasBlockComment `shouldBe` True
  
  it "tokenizes complex expressions" $ do
    let input = "result := (a + b) * c / 2.0"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 11
        let tokenValues = map (goTokenValue . locatedValue) tokens
        tokenValues `shouldBe` ["result", ":=", "(", "a", "+", "b", ")", "*", "c", "/", "2.0"]

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
  
  it "parses function declarations with parameters" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "add"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "x"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "y"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenKeyword GoKwReturn
          , GoTokenIdent "x"
          , GoTokenOperator GoOpPlus
          , GoTokenIdent "y"
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
                GoFuncDecl func -> do
                  case goFuncName func of
                    Just (Identifier name) -> name `shouldBe` "add"
                    Nothing -> expectationFailure "Function should have a name"
                  let params = goFuncParams func
                  length params `shouldBe` 2
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
                GoBindDecl bindings -> length bindings `shouldBe` 1
                _ -> expectationFailure "Expected variable declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
  it "parses variable declarations with initialization" $ do
    let tokens = mockGoTokens
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenKeyword GoKwVar
          , GoTokenIdent "x"
          , GoTokenIdent "int"
          , GoTokenOperator GoOpAssign
          , GoTokenInt "42"
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
                GoBindDecl bindings -> length bindings `shouldBe` 1
                _ -> expectationFailure "Expected variable declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
  it "parses variable declarations" $ do
    let tokens = mockGoTokens
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenKeyword GoKwVar
          , GoTokenIdent "x"
          , GoTokenOperator GoOpAssign
          , GoTokenInt "42"
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
                GoBindDecl [binding] -> case bindKind binding of
                  BindVar -> return ()
                  _ -> expectationFailure "Expected variable declaration"
                _ -> expectationFailure "Expected single binding"
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
                GoTypeDeclStmt (GoTypeDecl name _ _ _) -> name `shouldBe` Identifier "MyInt"
                _ -> expectationFailure "Expected type declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
  it "parses struct declarations" $ do
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
                GoTypeDeclStmt (GoTypeDecl name _ _ _) -> name `shouldBe` Identifier "Person"
                _ -> expectationFailure "Expected struct declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
  it "parses interface declarations" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwType
          , GoTokenIdent "Writer"
          , GoTokenKeyword GoKwInterface
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "Write"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "p"
          , GoTokenIdent "string"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenIdent "error"
          , GoTokenDelimiter GoDelimSemicolon
          , GoTokenNewline
          , GoTokenDelimiter GoDelimRightBrace
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
                GoTypeDeclStmt (GoTypeDecl name _ _ _) -> name `shouldBe` Identifier "Writer"
                _ -> expectationFailure "Expected interface declaration"
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
  it "parses if statements" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "main"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenKeyword GoKwIf
          , GoTokenIdent "x"
          , GoTokenOperator GoOpGt
          , GoTokenInt "0"
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          , GoTokenDelimiter GoDelimRightBrace
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
              (_:_) -> return ()  -- If statement parsed successfully
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"
  
  it "parses for loops" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "main"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenKeyword GoKwFor
          , GoTokenIdent "i"
          , GoTokenOperator GoOpAssign
          , GoTokenInt "0"
          , GoTokenDelimiter GoDelimSemicolon
          , GoTokenIdent "i"
          , GoTokenOperator GoOpLt
          , GoTokenInt "10"
          , GoTokenDelimiter GoDelimSemicolon
          , GoTokenIdent "i"
          , GoTokenOperator GoOpIncrement
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          , GoTokenDelimiter GoDelimRightBrace
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
              (_:_) -> return ()  -- For loop parsed successfully
              [] -> expectationFailure "No declarations in file"
          [] -> expectationFailure "No files in package"

-- Helper functions
mockGoTokens :: [GoToken] -> [Located GoToken]
mockGoTokens = map mockGoToken

mockGoToken :: GoToken -> Located GoToken
mockGoToken token = Located mockNodeAnn token
  where
    mockPosition = Position { posLine = 1, posColumn = 1 }
    mockSpan = Just $ Span { spanStart = mockPosition, spanEnd = mockPosition { posColumn = 10 } }
    mockNodeAnn = NodeAnn { annSpan = mockSpan, annLeading = [], annTrailing = [] }

-- Extract the value from a Located node
locatedValue :: Located a -> a
locatedValue = locValue

goTokenValue :: GoToken -> Text
goTokenValue = \case
  GoTokenKeyword kw -> goKeywordToText kw
  GoTokenIdent name -> name
  GoTokenString str -> str
  GoTokenInt num -> num
  GoTokenFloat num -> num
  GoTokenOperator op -> goOperatorToText op
  GoTokenDelimiter delim -> goDelimiterToText delim
  _ -> "token"

goOperatorToText :: GoOperator -> Text
goOperatorToText = \case
  GoOpPlus -> "+"
  GoOpMinus -> "-"
  GoOpMult -> "*"
  GoOpDiv -> "/"
  GoOpMod -> "%"
  GoOpBitClear -> "&^"
  GoOpAssign -> "="
  GoOpDefine -> ":="
  GoOpPlusAssign -> "+="
  GoOpMinusAssign -> "-="
  GoOpMultAssign -> "*="
  GoOpDivAssign -> "/="
  GoOpModAssign -> "%="
  GoOpEq -> "=="
  GoOpNe -> "!="
  GoOpLt -> "<"
  GoOpLe -> "<="
  GoOpGt -> ">"
  GoOpGe -> ">="
  GoOpAnd -> "&&"
  GoOpOr -> "||"
  GoOpNot -> "!"
  GoOpIncrement -> "++"
  GoOpDecrement -> "--"
  GoOpBitAnd -> "&"
  GoOpBitOr -> "|"
  GoOpBitXor -> "^"
  GoOpTilde -> "~"
  GoOpLeftShift -> "<<"
  GoOpRightShift -> ">>"
  GoOpBitAndAssign -> "&="
  GoOpBitOrAssign -> "|="
  GoOpBitXorAssign -> "^="
  GoOpBitClearAssign -> "&^="
  GoOpLeftShiftAssign -> "<<="
  GoOpRightShiftAssign -> ">>="
  GoOpArrow -> "<-"
  GoOpAddress -> "&"
  GoOpDeref -> "*"
  GoOpEllipsis -> "..."

goDelimiterToText :: GoDelimiter -> Text
goDelimiterToText = \case
  GoDelimLeftParen -> "("
  GoDelimRightParen -> ")"
  GoDelimLeftBrace -> "{"
  GoDelimRightBrace -> "}"
  GoDelimLeftBracket -> "["
  GoDelimRightBracket -> "]"
  GoDelimComma -> ","
  GoDelimSemicolon -> ";"
  GoDelimDot -> "."
  GoDelimColon -> ":"