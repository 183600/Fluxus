{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Fluxus.Parser.Python (spec) where

import Test.Hspec
import Data.Text (Text)

import qualified Fluxus.Parser.Python.Lexer as Lexer
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
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let tokenValues = map (locatedValue . fmap tokenValue) tokens
        tokenValues `shouldBe` ["x", "+", "42"]
  
  it "tokenizes Python keywords" $ do
    let input = "def if else for while"
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 5
        let isKeywordToken (Located _ (Lexer.TokenKeyword _)) = True
            isKeywordToken _ = False
        all (isKeywordToken) tokens `shouldBe` True
  
  it "tokenizes string literals" $ do
    let input = "\"hello world\""
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case tokens of
          (token:_) -> case locatedValue token of
            Lexer.TokenString content -> content `shouldBe` "hello world"
            _ -> expectationFailure "Expected string token"
          [] -> expectationFailure "No tokens produced"
  
  it "tokenizes number literals" $ do
    let input = "42 3.14 1e10"
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 3
        let isNumber (Located _ (Lexer.TokenNumber _ _)) = True
            isNumber _ = False
        all (isNumber) tokens `shouldBe` True

  it "tokenizes complex expressions" $ do
    let input = "result = (a + b) * c / 2.0"
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 11
        let tokenValues = map (locatedValue . fmap tokenValue) tokens
        tokenValues `shouldBe` ["result", "=", "(", "a", "+", "b", ")", "*", "c", "/", "2.0"]
  
  it "tokenizes indentation and newlines" $ do
    let input = "def func():\n    x = 1\n    return x"
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        let hasIndent = any (\case
              Located _ (Lexer.TokenIndent _) -> True
              _ -> False) tokens
        let hasNewline = any (\case
              Located _ Lexer.TokenNewline -> True
              _ -> False) tokens
        hasIndent `shouldBe` True
        hasNewline `shouldBe` True
  
  it "tokenizes escape sequences in strings" $ do
    let input = "\"Hello\\nWorld\\tTab\\\"Quote\""
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 1
        case tokens of
          (token:_) -> case locatedValue token of
            Lexer.TokenString content -> content `shouldBe` "Hello\nWorld\tTab\"Quote"
            _ -> expectationFailure "Expected string token"
          [] -> expectationFailure "No tokens produced"
  
  it "tokenizes boolean literals" $ do
    let input = "True False true false"
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        length tokens `shouldBe` 4
        let isBool (Located _ (Lexer.TokenKeyword kw)) = kw == Lexer.KwTrue || kw == Lexer.KwFalse
            isBool _ = False
            isIdent (Located _ (Lexer.TokenIdent _)) = True
            isIdent _ = False
        -- Only True and False should be boolean keywords, true and false should be identifiers
        (isBool (tokens !! 0)) `shouldBe` True  -- True
        (isBool (tokens !! 1)) `shouldBe` True  -- False  
        (isIdent (tokens !! 2)) `shouldBe` True -- true (identifier)
        (isIdent (tokens !! 3)) `shouldBe` True -- false (identifier)
  
  it "tokenizes comments" $ do
    let input = "x = 1  # This is a comment\ny = 2"
    case Lexer.runPythonLexer "test.py" input of
      Left _ -> expectationFailure "Lexer failed"
      Right tokens -> do
        let hasComment = any (\case
              Located _ (Lexer.TokenComment _) -> True
              _ -> False) tokens
        hasComment `shouldBe` True

parserSpec :: Spec
parserSpec = describe "Python Parser" $ do
  it "parses simple expressions" $ do
    let tokens = mockTokens [Lexer.TokenIdent "x", Lexer.TokenOperator Lexer.OpPlus, Lexer.TokenNumber "42" False, Lexer.TokenNewline]
    case runPythonParser "test.py" tokens of
      Left err -> expectationFailure $ "Parser failed: " ++ show err
      Right ast -> do
        let PythonAST module_ = ast
        length (pyModuleBody module_) `shouldBe` 1
  
  it "parses function definitions" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwDef
          , Lexer.TokenIdent "test_func"
          , Lexer.TokenDelimiter Lexer.DelimLeftParen
          , Lexer.TokenDelimiter Lexer.DelimRightParen
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
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
  
  it "parses function definitions with parameters" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwDef
          , Lexer.TokenIdent "func_with_params"
          , Lexer.TokenDelimiter Lexer.DelimLeftParen
          , Lexer.TokenIdent "x"
          , Lexer.TokenDelimiter Lexer.DelimComma
          , Lexer.TokenIdent "y"
          , Lexer.TokenDelimiter Lexer.DelimRightParen
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwReturn
          , Lexer.TokenIdent "x"
          , Lexer.TokenOperator Lexer.OpPlus
          , Lexer.TokenIdent "y"
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyFuncDef funcDef -> do
              pyFuncName funcDef `shouldBe` Identifier "func_with_params"
              length (pyFuncParams funcDef) `shouldBe` 2
            _ -> expectationFailure "Expected function definition"
          [] -> expectationFailure "Module body is empty"
  
  it "parses class definitions" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwClass
          , Lexer.TokenIdent "TestClass"
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
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
  
  it "parses class definitions with inheritance" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwClass
          , Lexer.TokenIdent "DerivedClass"
          , Lexer.TokenDelimiter Lexer.DelimLeftParen
          , Lexer.TokenIdent "BaseClass"
          , Lexer.TokenDelimiter Lexer.DelimRightParen
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyClassDef classDef -> do
              pyClassName classDef `shouldBe` Identifier "DerivedClass"
              length (pyClassBases classDef) `shouldBe` 1
            _ -> expectationFailure "Expected class definition"
          [] -> expectationFailure "Module body is empty"
  
  it "parses if statements" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwIf
          , Lexer.TokenKeyword Lexer.KwTrue
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
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
  
  it "parses if-else statements" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwIf
          , Lexer.TokenKeyword Lexer.KwTrue
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          , Lexer.TokenKeyword Lexer.KwElse
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyIf _ _ elseBody -> do
              length elseBody `shouldBe` 1
            _ -> expectationFailure "Expected if-else statement"
          [] -> expectationFailure "Module body is empty"
  
  it "parses while loops" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwWhile
          , Lexer.TokenKeyword Lexer.KwTrue
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyWhile _ _ _ -> return ()
            _ -> expectationFailure "Expected while loop"
          [] -> expectationFailure "Module body is empty"
  
  it "parses for loops" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwFor
          , Lexer.TokenIdent "i"
          , Lexer.TokenKeyword Lexer.KwIn
          , Lexer.TokenIdent "range"
          , Lexer.TokenDelimiter Lexer.DelimLeftParen
          , Lexer.TokenNumber "10" False
          , Lexer.TokenDelimiter Lexer.DelimRightParen
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyFor _ _ _ _ _ -> return ()
            _ -> expectationFailure "Expected for loop"
          [] -> expectationFailure "Module body is empty"
  
  it "parses try-except blocks" $ do
    let tokens = mockTokens 
          [ Lexer.TokenKeyword Lexer.KwTry
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          , Lexer.TokenKeyword Lexer.KwExcept
          , Lexer.TokenDelimiter Lexer.DelimColon
          , Lexer.TokenNewline
          , Lexer.TokenIndent 1
          , Lexer.TokenKeyword Lexer.KwPass
          , Lexer.TokenNewline
          , Lexer.TokenDedent 0
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyTry _ _ _ _ -> return ()
            _ -> expectationFailure "Expected try-except block"
          [] -> expectationFailure "Module body is empty"
  
  it "parses list comprehensions" $ do
    let tokens = mockTokens 
          [ Lexer.TokenDelimiter Lexer.DelimLeftBracket
          , Lexer.TokenIdent "x"
          , Lexer.TokenOperator Lexer.OpMult
          , Lexer.TokenNumber "2" False
          , Lexer.TokenKeyword Lexer.KwFor
          , Lexer.TokenIdent "x"
          , Lexer.TokenKeyword Lexer.KwIn
          , Lexer.TokenIdent "range"
          , Lexer.TokenDelimiter Lexer.DelimLeftParen
          , Lexer.TokenNumber "10" False
          , Lexer.TokenDelimiter Lexer.DelimRightParen
          , Lexer.TokenDelimiter Lexer.DelimRightBracket
          , Lexer.TokenNewline
          ]
    case runPythonParser "test.py" tokens of
      Left _ -> expectationFailure "Parser failed"
      Right ast -> do
        let PythonAST module_ = ast
        let body = pyModuleBody module_
        length body `shouldBe` 1
        case body of
          (item:_) -> case locatedValue item of
            PyExprStmt expr -> case locatedValue expr of
              PyListComp _ _ -> return ()
              _ -> expectationFailure "Expected list comprehension expression"
            _ -> expectationFailure "Expected expression statement"
          [] -> expectationFailure "Module body is empty"

-- Helper functions
mockTokens :: [Lexer.PythonToken] -> [Located Lexer.PythonToken]
mockTokens = map mockToken

mockToken :: Lexer.PythonToken -> Located Lexer.PythonToken
mockToken token = Located mockSpan token
  where
    mockSpan = SourceSpan "test.py" (SourcePos 1 1) (SourcePos 1 10)

tokenValue :: Lexer.PythonToken -> Text
tokenValue = \case
  Lexer.TokenKeyword kw -> Lexer.keywordToText kw
  Lexer.TokenIdent name -> name
  Lexer.TokenString str -> str
  Lexer.TokenNumber num _ -> num
  Lexer.TokenOperator op -> operatorToText op
  Lexer.TokenDelimiter delim -> delimiterToText delim
  _ -> "token"

operatorToText :: Lexer.Operator -> Text
operatorToText = \case
  Lexer.OpPlus -> "+"
  Lexer.OpMinus -> "-"
  Lexer.OpMult -> "*"
  Lexer.OpDiv -> "/"
  Lexer.OpMod -> "%"
  Lexer.OpPower -> "**"
  Lexer.OpFloorDiv -> "//"
  Lexer.OpBitAnd -> "&"
  Lexer.OpBitOr -> "|"
  Lexer.OpBitXor -> "^"
  Lexer.OpBitNot -> "~"
  Lexer.OpLeftShift -> "<<"
  Lexer.OpRightShift -> ">>"
  Lexer.OpEq -> "=="
  Lexer.OpNe -> "!="
  Lexer.OpLt -> "<"
  Lexer.OpLe -> "<="
  Lexer.OpGt -> ">"
  Lexer.OpGe -> ">="
  Lexer.OpAnd -> "and"
  Lexer.OpOr -> "or"
  Lexer.OpNot -> "not"
  Lexer.OpAssign -> "="
  Lexer.OpPlusAssign -> "+="
  Lexer.OpMinusAssign -> "-="
  Lexer.OpMultAssign -> "*="
  Lexer.OpDivAssign -> "/="
  Lexer.OpModAssign -> "%="
  Lexer.OpPowerAssign -> "**="
  Lexer.OpFloorDivAssign -> "//="
  Lexer.OpBitAndAssign -> "&="
  Lexer.OpBitOrAssign -> "|="
  Lexer.OpBitXorAssign -> "^="
  Lexer.OpLeftShiftAssign -> "<<="
  Lexer.OpRightShiftAssign -> ">>="
  Lexer.OpWalrus -> ":="
  Lexer.OpArrow -> "->"
  Lexer.OpEllipsis -> "..."

delimiterToText :: Lexer.Delimiter -> Text
delimiterToText = \case
  Lexer.DelimLeftParen -> "("
  Lexer.DelimRightParen -> ")"
  Lexer.DelimLeftBracket -> "["
  Lexer.DelimRightBracket -> "]"
  Lexer.DelimLeftBrace -> "{"
  Lexer.DelimRightBrace -> "}"
  Lexer.DelimComma -> ","
  Lexer.DelimColon -> ":"
  Lexer.DelimSemicolon -> ";"
  Lexer.DelimDot -> "."
  Lexer.DelimAt -> "@"