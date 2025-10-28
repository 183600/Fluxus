{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Fluxus.Parser.Go (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go
import Fluxus.AST.Common

-- Safe safeHead function for tests
safeHead :: [a] -> a
safeHead [] = error "safeHead: empty list (test invariant violated)"
safeHead (x:_) = x

spec :: Spec
spec = describe "Go Parser" $ do
  lexerSpec
  lexerTokensSpec
  basicTypesSpec
  importDeclSpec
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
        case locatedValue (safeHead tokens) of
          GoTokenString content -> content `shouldBe` "hello world"
          _ -> expectationFailure "Expected string token"
  
  it "tokenizes raw string literals" $ do
    let input = "`hello\nworld`"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case locatedValue (safeHead tokens) of
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
        all (isNumber) tokens `shouldBe` True

lexerTokensSpec :: Spec
lexerTokensSpec = describe "Go Lexer - tokens coverage" $ do
  it "tokenizes identifiers" $ do
    let input = "x _tmp Alpha123 rangeVar"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        let ts = map locatedValue toks
        ts `shouldBe`
          [ GoTokenIdent "x"
          , GoTokenIdent "_tmp"
          , GoTokenIdent "Alpha123"
          , GoTokenIdent "rangeVar"
          ]

  it "tokenizes common keywords" $ do
    let input = "package import var type func if for range return"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        length toks `shouldBe` 9
        let isKw (GoTokenKeyword _) = True
            isKw _ = False
        all (isKw . locatedValue) toks `shouldBe` True

  it "tokenizes arithmetic operators" $ do
    let input = "+ - * / %"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenOperator GoOpPlus
          , GoTokenOperator GoOpMinus
          , GoTokenOperator GoOpMult
          , GoTokenOperator GoOpDiv
          , GoTokenOperator GoOpMod
          ]

  it "tokenizes comparison operators" $ do
    let input = "== != < > <= >="
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenOperator GoOpEq
          , GoTokenOperator GoOpNe
          , GoTokenOperator GoOpLt
          , GoTokenOperator GoOpGt
          , GoTokenOperator GoOpLe
          , GoTokenOperator GoOpGe
          ]

  it "tokenizes logical operators" $ do
    let input = "&& || !"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenOperator GoOpAnd
          , GoTokenOperator GoOpOr
          , GoTokenOperator GoOpNot
          ]

  it "tokenizes assignment and define" $ do
    let input = "= :="
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenOperator GoOpAssign
          , GoTokenOperator GoOpDefine
          ]

  it "tokenizes inc/dec, shifts and bitwise" $ do
    let input = "++ -- << >> & | ^ &^"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenOperator GoOpIncrement
          , GoTokenOperator GoOpDecrement
          , GoTokenOperator GoOpLeftShift
          , GoTokenOperator GoOpRightShift
          , GoTokenOperator GoOpAddress
          , GoTokenOperator GoOpBitOr
          , GoTokenOperator GoOpBitXor
          , GoTokenOperator GoOpBitClear
          ]

  it "tokenizes compound assignments" $ do
    let input = "+= -= *= /= %= &= |= ^= &^= <<= >>="
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenOperator GoOpPlusAssign
          , GoTokenOperator GoOpMinusAssign
          , GoTokenOperator GoOpMultAssign
          , GoTokenOperator GoOpDivAssign
          , GoTokenOperator GoOpModAssign
          , GoTokenOperator GoOpBitAndAssign
          , GoTokenOperator GoOpBitOrAssign
          , GoTokenOperator GoOpBitXorAssign
          , GoTokenOperator GoOpBitClearAssign
          , GoTokenOperator GoOpLeftShiftAssign
          , GoTokenOperator GoOpRightShiftAssign
          ]

  it "tokenizes special operators" $ do
    let input = "<- ..."
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenOperator GoOpArrow
          , GoTokenOperator GoOpEllipsis
          ]

  it "tokenizes number literals (dec/float/exp/imag)" $ do
    let input = "123 3.14 1e10 2.5e-3 1i 3.14i"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenInt "123"
          , GoTokenFloat "3.14"
          , GoTokenFloat "1e10"
          , GoTokenFloat "2.5e-3"
          , GoTokenImag "1i"
          , GoTokenImag "3.14i"
          ]

  it "tokenizes string, raw string and rune literals" $ do
    let input = "\"hello\" `world` 'a' '\\n'"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        let ts = map locatedValue toks
        ts `shouldBe`
          [ GoTokenString "hello"
          , GoTokenRawString "world"
          , GoTokenRune 'a'
          , GoTokenRune '\n'
          ]

  it "tokenizes delimiters" $ do
    let input = "( ) [ ] { } , ; . :"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBracket
          , GoTokenDelimiter GoDelimRightBracket
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          , GoTokenDelimiter GoDelimComma
          , GoTokenDelimiter GoDelimSemicolon
          , GoTokenDelimiter GoDelimDot
          , GoTokenDelimiter GoDelimColon
          ]

  it "tokenizes function literal tokens" $ do
    let input = "func(){}"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenKeyword GoKwFunc
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          ]

  it "tokenizes hex, octal and binary integers" $ do
    let input = "0xFF 0o77 0b1010"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenInt "0xFF"
          , GoTokenInt "0o77"
          , GoTokenInt "0b1010"
          ]

  it "tokenizes string escape sequences" $ do
    let input = "\"a\\n\\t\\r\\\"\\'\\\\\""
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks -> do
        length toks `shouldBe` 1
        case locatedValue (safeHead toks) of
          GoTokenString content -> content `shouldBe` "a\n\t\r\"'\\"
          _ -> expectationFailure "Expected string token"

  it "produces newline tokens" $ do
    let input = "x\ny"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenIdent "x"
          , GoTokenNewline
          , GoTokenIdent "y"
          ]

-- New basic data types tests
basicTypesSpec :: Spec
basicTypesSpec = describe "Go Lexer - basic data types" $ do
  it "tokenizes boolean literals true/false as identifiers" $ do
    let input = "true false"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenIdent "true"
          , GoTokenIdent "false"
          ]

  it "tokenizes basic type identifiers" $ do
    let input = "bool int int8 int16 int32 int64 uint uint8 byte uint16 uint32 uint64 uintptr float32 float64 complex64 complex128 string"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenIdent "bool"
          , GoTokenIdent "int"
          , GoTokenIdent "int8"
          , GoTokenIdent "int16"
          , GoTokenIdent "int32"
          , GoTokenIdent "int64"
          , GoTokenIdent "uint"
          , GoTokenIdent "uint8"
          , GoTokenIdent "byte"
          , GoTokenIdent "uint16"
          , GoTokenIdent "uint32"
          , GoTokenIdent "uint64"
          , GoTokenIdent "uintptr"
          , GoTokenIdent "float32"
          , GoTokenIdent "float64"
          , GoTokenIdent "complex64"
          , GoTokenIdent "complex128"
          , GoTokenIdent "string"
          ]

  it "tokenizes UTF-8 string literals" $ do
    let input = "\"你好，世界\" `你好，世界`"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenString "你好，世界"
          , GoTokenRawString "你好，世界"
          ]

  it "tokenizes imaginary number literals for complex numbers" $ do
    let input = "1i 2.5i 0i"
    case runGoLexer "test.go" input of
      Left _ -> expectationFailure "Lexer failed"
      Right toks ->
        map locatedValue toks `shouldBe`
          [ GoTokenImag "1i"
          , GoTokenImag "2.5i"
          , GoTokenImag "0i"
          ]

-- Import declaration tests
importDeclSpec :: Spec
importDeclSpec = describe "Go Parser - Import Declaration" $ do
  it "parses single import statement" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenString "fmt"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 1
        case locatedValue (safeHead imports) of
          GoImportNormal Nothing path -> path `shouldBe` "fmt"
          _ -> expectationFailure "Expected simple import"
  
  it "parses grouped import statements" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenString "fmt"
          , GoTokenNewline
          , GoTokenString "math/rand"
          , GoTokenNewline
          , GoTokenDelimiter GoDelimRightParen
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 2
  
  it "parses import with alias" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenIdent "f"
          , GoTokenString "fmt"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 1
        case locatedValue (safeHead imports) of
          GoImportNormal (Just (Identifier alias)) path -> do
            alias `shouldBe` "f"
            path `shouldBe` "fmt"
          _ -> expectationFailure "Expected aliased import"
  
  it "parses import with dot notation" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenDelimiter GoDelimDot
          , GoTokenString "fmt"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 1
        case locatedValue (safeHead imports) of
          GoImportDot path -> path `shouldBe` "fmt"
          _ -> expectationFailure "Expected dot import"
  
  it "parses import with blank identifier" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenIdent "_"
          , GoTokenString "database/sql/driver"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 1
        case locatedValue (safeHead imports) of
          GoImportBlank path -> path `shouldBe` "database/sql/driver"
          _ -> expectationFailure "Expected blank import"
  
  it "parses multiple imports with different styles" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenString "fmt"
          , GoTokenNewline
          , GoTokenIdent "m"
          , GoTokenString "math"
          , GoTokenNewline
          , GoTokenDelimiter GoDelimDot
          , GoTokenString "strings"
          , GoTokenNewline
          , GoTokenIdent "_"
          , GoTokenString "net/http/pprof"
          , GoTokenNewline
          , GoTokenDelimiter GoDelimRightParen
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 4
  
  it "parses import with nested package path" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenString "encoding/json"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 1
        case locatedValue (safeHead imports) of
          GoImportNormal Nothing path -> path `shouldBe` "encoding/json"
          _ -> expectationFailure "Expected nested path import"
  
  it "parses import with deeply nested package path" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenString "github.com/user/project/pkg/module"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 1
        case locatedValue (safeHead imports) of
          GoImportNormal Nothing path -> path `shouldBe` "github.com/user/project/pkg/module"
          _ -> expectationFailure "Expected deeply nested path import"
  
  it "parses standard library imports" $ do
    let stdLibs = ["fmt", "os", "io", "net/http", "database/sql", "encoding/json", "time", "sync"]
    mapM_ (\lib -> do
      let tokens = mockGoTokens 
            [ GoTokenKeyword GoKwPackage
            , GoTokenIdent "main"
            , GoTokenNewline
            , GoTokenKeyword GoKwImport
            , GoTokenString lib
            ]
      case runGoParser "test.go" tokens of
        Left _ -> expectationFailure $ "Parser failed for import " ++ T.unpack lib
        Right ast -> do
          let GoAST package_ = ast
          let files = goPackageFiles package_
          let imports = goFileImports (safeHead files)
          length imports `shouldBe` 1
      ) stdLibs
  
  it "parses package with both imports and declarations" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenString "fmt"
          , GoTokenNewline
          , GoTokenString "os"
          , GoTokenNewline
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "main"
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
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 2
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1

-- Package declaration tests
packageDeclSpec :: Spec
packageDeclSpec = describe "Go Parser - Package Declaration" $ do
  it "parses package main declaration" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "main"
  
  it "parses package with custom name" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "mypackage"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "mypackage"
  
  it "parses package with underscore in name" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "my_package"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "my_package"
  
  it "parses package with numbers in name" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "package123"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "package123"
  
  it "parses package declaration followed by imports" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwImport
          , GoTokenString "fmt"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "main"
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let imports = goFileImports (safeHead files)
        length imports `shouldBe` 1
  
  it "parses package declaration followed by function" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "main"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "main"
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
  
  it "parses package declaration with common library names" $ do
    let packageNames = ["fmt", "os", "io", "net", "http", "strings", "bytes", "time"]
    mapM_ (\pkgName -> do
      let tokens = mockGoTokens 
            [ GoTokenKeyword GoKwPackage
            , GoTokenIdent pkgName
            ]
      case runGoParser "test.go" tokens of
        Left _ -> expectationFailure $ "Parser failed for package " ++ T.unpack pkgName
        Right ast -> do
          let GoAST package_ = ast
          goPackageName package_ `shouldBe` Identifier pkgName
      ) packageNames
  
  it "parses package declaration at start of file" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwVar
          , GoTokenIdent "x"
          , GoTokenIdent "int"
          ]
    case runGoParser "test.go" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let GoAST package_ = ast
        goPackageName package_ `shouldBe` Identifier "main"
  
  it "rejects file without package declaration" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwFunc
          , GoTokenIdent "main"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left _ -> return () -- Expected to fail
      Right _ -> expectationFailure "Parser should fail without package declaration"

parserSpec :: Spec
parserSpec = describe "Go Parser" $ do
  packageDeclSpec
  
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
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
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
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
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
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoTypeDecl (Identifier name) _ -> name `shouldBe` "MyInt"
          _ -> expectationFailure "Expected type declaration"
  
  -- 新增函数测试用例
  it "parses function with parameters and return type" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "add"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "a"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "b"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenKeyword GoKwReturn
          , GoTokenIdent "a"
          , GoTokenOperator GoOpPlus
          , GoTokenIdent "b"
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "add"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses function with multiple return values" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "swap"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "x"
          , GoTokenIdent "string"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "y"
          , GoTokenIdent "string"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "string"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "string"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenKeyword GoKwReturn
          , GoTokenIdent "y"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "x"
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "swap"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses function with no parameters and no return value" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "hello"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenIdent "fmt"
          , GoTokenDelimiter GoDelimDot
          , GoTokenIdent "Println"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenString "Hello, World!"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "hello"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses main function as program entry point" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "main"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenIdent "fmt"
          , GoTokenDelimiter GoDelimDot
          , GoTokenIdent "Println"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenString "Program started"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "main"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses function with variadic parameters" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "sum"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "nums"
          , GoTokenOperator GoOpEllipsis
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenIdent "total"
          , GoTokenOperator GoOpDefine
          , GoTokenFloat "0"
          , GoTokenNewline
          , GoTokenKeyword GoKwFor
          , GoTokenIdent "_"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "num"
          , GoTokenOperator GoOpDefine
          , GoTokenKeyword GoKwRange
          , GoTokenIdent "nums"
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenIdent "total"
          , GoTokenOperator GoOpPlusAssign
          , GoTokenIdent "num"
          , GoTokenDelimiter GoDelimRightBrace
          , GoTokenNewline
          , GoTokenKeyword GoKwReturn
          , GoTokenIdent "total"
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "sum"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses function with named return values" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "divide"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "a"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "b"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "quotient"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimComma
          , GoTokenIdent "remainder"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenIdent "quotient"
          , GoTokenOperator GoOpAssign
          , GoTokenIdent "a"
          , GoTokenOperator GoOpDiv
          , GoTokenIdent "b"
          , GoTokenNewline
          , GoTokenIdent "remainder"
          , GoTokenOperator GoOpAssign
          , GoTokenIdent "a"
          , GoTokenOperator GoOpMod
          , GoTokenIdent "b"
          , GoTokenNewline
          , GoTokenKeyword GoKwReturn
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "divide"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses recursive function" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "factorial"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "n"
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenKeyword GoKwIf
          , GoTokenIdent "n"
          , GoTokenOperator GoOpLe
          , GoTokenFloat "1"
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenKeyword GoKwReturn
          , GoTokenFloat "1"
          , GoTokenDelimiter GoDelimRightBrace
          , GoTokenNewline
          , GoTokenKeyword GoKwReturn
          , GoTokenIdent "n"
          , GoTokenOperator GoOpMult
          , GoTokenIdent "factorial"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "n"
          , GoTokenOperator GoOpMinus
          , GoTokenFloat "1"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "factorial"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"
  
  it "parses function with pointer parameters and return type" $ do
    let tokens = mockGoTokens 
          [ GoTokenKeyword GoKwPackage
          , GoTokenIdent "main"
          , GoTokenNewline
          , GoTokenKeyword GoKwFunc
          , GoTokenIdent "increment"
          , GoTokenDelimiter GoDelimLeftParen
          , GoTokenIdent "x"
          , GoTokenOperator GoOpMult
          , GoTokenIdent "int"
          , GoTokenDelimiter GoDelimRightParen
          , GoTokenDelimiter GoDelimLeftBrace
          , GoTokenOperator GoOpMult
          , GoTokenIdent "x"
          , GoTokenOperator GoOpIncrement
          , GoTokenDelimiter GoDelimRightBrace
          ]
    case runGoParser "test.go" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let GoAST package_ = ast
        let files = goPackageFiles package_
        length files `shouldBe` 1
        let decls = goFileDecls (safeHead files)
        length decls `shouldBe` 1
        case locatedValue (safeHead decls) of
          GoFuncDecl func -> case goFuncName func of
            Just (Identifier name) -> name `shouldBe` "increment"
            Nothing -> expectationFailure "Function should have a name"
          _ -> expectationFailure "Expected function declaration"

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
