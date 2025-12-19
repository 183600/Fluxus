{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Fluxus.Parser.Python (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.AST.Common

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

parseModuleFrom :: Text -> Either String PythonModule
parseModuleFrom source =
  case runPythonLexer (T.pack "test.py") source of
    Left err -> Left (show err)
    Right tokens ->
      case runPythonParser (T.pack "test.py") tokens of
        Left perr -> Left (show perr)
        Right (PythonAST modu) -> Right modu

withParsedModule :: Text -> (PythonModule -> Expectation) -> Expectation
withParsedModule source action =
  case parseModuleFrom source of
    Left err -> expectationFailure err
    Right modu -> action modu

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
        case safeHead tokens of
          Nothing -> expectationFailure "Expected at least one token"
          Just token -> case locatedValue token of
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
    let input = "def class if else for while return import from match case"
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
        case map locatedValue toks of
          [TokenString "hello", TokenFString segments] -> do
            length segments `shouldBe` 1
            case segments of
              (firstSeg:_) -> case firstSeg of
                FStringLiteralSegment txt _ -> txt `shouldBe` "world"
                _ -> expectationFailure "Expected literal segment"
              [] -> expectationFailure "segments list is empty"
          other -> expectationFailure $ "Unexpected tokens: " <> show other
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
        case safeHead (pyModuleBody module_) of
          Nothing -> expectationFailure "Expected at least one statement"
          Just stmt -> case locatedValue stmt of
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
        case safeHead (pyModuleBody module_) of
          Nothing -> expectationFailure "Expected at least one statement"
          Just stmt -> case locatedValue stmt of
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
        case safeHead (pyModuleBody module_) of
          Nothing -> expectationFailure "Expected at least one statement"
          Just stmt -> case locatedValue stmt of
            PyIf _ _ _ -> return ()
            _ -> expectationFailure "Expected if statement"

  it "parses if/elif/else chains" $
    withParsedModule (T.unlines
      [ "if value > 0:"
      , "    pass"
      , "elif value == 0:"
      , "    pass"
      , "else:"
      , "    pass"
      ]) $ \module_ ->
      case pyModuleBody module_ of
        [stmt] ->
          case locatedValue stmt of
            PyIf _ _ elifBranch -> do
              length elifBranch `shouldBe` 1
              case elifBranch of
                (firstElif:_) -> case locatedValue firstElif of
                  PyIf _ _ finalElse -> finalElse `shouldSatisfy` (not . null)
                  other -> expectationFailure $ "Expected nested if for elif branch, found " <> show other
                [] -> expectationFailure "elifBranch list is empty"
            other -> expectationFailure $ "Expected top-level if statement, found " <> show other
        _ -> expectationFailure "Expected single statement"
  
  describe "enhanced constructs" $ do
    it "parses list literals with elements" $
      withParsedModule "values = [1, 2, 3]\n" $ \module_ -> do
        case pyModuleBody module_ of
          [assignStmt] -> case locatedValue assignStmt of
            PyAssign targets value -> do
              length targets `shouldBe` 1
              let SourceSpan { spanStart = SourcePos startLine _ } = locSpan value
              startLine `shouldSatisfy` (> 0)
              case locValue value of
                PyList elems -> do
                  length elems `shouldBe` 3
                  all (isLiteral . locValue) elems `shouldBe` True
                other -> expectationFailure $ "Expected list literal, found " <> show other
            other -> expectationFailure $ "Expected assignment, found " <> show other
          _ -> expectationFailure "Expected single assignment statement"
    
    it "parses dictionary literals" $
      withParsedModule "mapping = {\"a\": 1, \"b\": 2}\n" $ \module_ -> do
        case pyModuleBody module_ of
          [assignStmt] -> case locatedValue assignStmt of
            PyAssign _ value -> case locValue value of
              PyDict pairs -> do
                length pairs `shouldBe` 2
                case pairs of
                  (firstPair:_) -> isLiteral (locValue (fst firstPair)) `shouldBe` True
                  [] -> expectationFailure "pairs list is empty"
              other -> expectationFailure $ "Expected dict literal, found " <> show other
            other -> expectationFailure $ "Expected assignment, found " <> show other
          _ -> expectationFailure "Expected single assignment"
    
    it "parses set literals" $
      withParsedModule "names = {a, b, c}\n" $ \module_ -> do
        case pyModuleBody module_ of
          [assignStmt] -> case locatedValue assignStmt of
            PyAssign _ value -> case locValue value of
              PySet elems -> length elems `shouldBe` 3
              other -> expectationFailure $ "Expected set literal, found " <> show other
            other -> expectationFailure $ "Expected assignment, found " <> show other
          _ -> expectationFailure "Expected single assignment"
    
    it "parses destructuring assignments with starred patterns" $
      withParsedModule "first, *rest = values\n" $ \module_ -> do
        case pyModuleBody module_ of
          [assignStmt] -> case locatedValue assignStmt of
            PyAssign targets _ -> case map locValue targets of
              [PatTuple [firstPat, starredPat]] -> do
                locValue firstPat `shouldBe` PatVar (Identifier "first")
                case locValue starredPat of
                  PatStarred inner -> locValue inner `shouldBe` PatVar (Identifier "rest")
                  other -> expectationFailure $ "Expected starred pattern, found " <> show other
              other -> expectationFailure $ "Expected tuple pattern, found " <> show other
            other -> expectationFailure $ "Expected assignment, found " <> show other
          _ -> expectationFailure "Expected single assignment"
    
    it "captures decorators and starred parameters for async functions" $
      withParsedModule (T.unlines
        [ "@logged"
        , "async def func(a, *args, **kwargs):"
        , "    pass"
        ]) $ \module_ -> do
        case pyModuleBody module_ of
          [funcStmt] -> case locatedValue funcStmt of
            PyAsyncFuncDef funcDef -> do
              length (pyFuncDecorators funcDef) `shouldBe` 1
              case pyFuncDecorators funcDef of
                [Located _ decorator] -> locValue (pyDecoratorName decorator) `shouldBe` PyVar (Identifier "logged")
                _ -> expectationFailure "Expected single decorator"
              let params = map locValue (pyFuncParams funcDef)
              params `shouldSatisfy` any isVarParam
              params `shouldSatisfy` any isVarArgsParam
              params `shouldSatisfy` any isKwArgsParam
            other -> expectationFailure $ "Expected async function, found " <> show other
          _ -> expectationFailure "Expected single function"
    
    it "parses decorated classes with bases and keyword arguments" $
      withParsedModule (T.unlines
        [ "@decorator"
        , "class Derived(Base, metaclass=Meta):"
        , "    pass"
        ]) $ \module_ -> do
        case pyModuleBody module_ of
          [classStmt] -> case locatedValue classStmt of
            PyClassDef classDef -> do
              length (pyClassDecorators classDef) `shouldBe` 1
              map locValue (pyClassBases classDef) `shouldBe` [PyVar (Identifier "Base")]
              case pyClassKeywords classDef of
                [(Identifier "metaclass", metaExpr)] -> locValue metaExpr `shouldBe` PyVar (Identifier "Meta")
                other -> expectationFailure $ "Unexpected class keywords: " <> show other
            other -> expectationFailure $ "Expected class definition, found " <> show other
          _ -> expectationFailure "Expected single class"
    
    it "parses from-import statements with aliases" $
      withParsedModule "from package.sub import name as alias, other\n" $ \module_ -> do
        case pyModuleBody module_ of
          [importStmt] -> case locatedValue importStmt of
            PyImport [Located _ (ImportFrom moduleName items)] -> do
              moduleName `shouldBe` ModuleName "package.sub"
              items `shouldBe` [(Identifier "name", Just (Identifier "alias")), (Identifier "other", Nothing)]
            other -> expectationFailure $ "Expected from-import, found " <> show other
          _ -> expectationFailure "Expected single import"

    describe "advanced statements and expressions" $ do
      it "parses annotated assignments with type hints" $
        withParsedModule "value: int = 42\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAnnAssign target typeExpr mValue -> do
                locValue target `shouldBe` PatVar (Identifier "value")
                locValue typeExpr `shouldBe` TypeName (QualifiedName [] (Identifier "int"))
                case mValue of
                  Just expr -> locValue expr `shouldBe` PyLiteral (PyInt 42)
                  Nothing -> expectationFailure "Expected initializer for annotated assignment"
              other -> expectationFailure $ "Expected annotated assignment, found " <> show other
            _ -> expectationFailure "Expected single annotated assignment"

    describe "type annotation parsing" $ do
      it "parses generic annotations on parameters and returns" $
        withParsedModule (T.unlines
          [ "def summarize(values: list[int]) -> tuple[int, str]:"
          , "    return values[0], \"\""
          ]) $ \module_ ->
            case pyModuleBody module_ of
              [funcStmt] -> case locatedValue funcStmt of
                PyFuncDef funcDef -> do
                  case pyFuncParams funcDef of
                    [param] -> case locValue param of
                      ParamNormal _ (Just ann) _ ->
                        case locValue ann of
                          TypeSubscript base [inner] -> do
                            expectSimpleTypeName base [] "list"
                            expectSimpleTypeName inner [] "int"
                          other -> expectationFailure $ "Expected list subscript, found " <> show other
                      other -> expectationFailure $ "Expected normal parameter annotation, found " <> show other
                    _ -> expectationFailure "Expected single function parameter"
                  case pyFuncReturns funcDef of
                    Just retAnn -> case locValue retAnn of
                      TypeSubscript base elems -> do
                        expectSimpleTypeName base [] "tuple"
                        length elems `shouldBe` 2
                        case elems of
                          (firstElem:_) -> do
                            expectSimpleTypeName firstElem [] "int"
                            expectSimpleTypeName (elems !! 1) [] "str"
                          [] -> expectationFailure "elems list is empty"
                      other -> expectationFailure $ "Expected tuple return annotation, found " <> show other
                    Nothing -> expectationFailure "Expected return annotation"
                other -> expectationFailure $ "Expected function definition, found " <> show other
              _ -> expectationFailure "Expected single function definition"

      it "supports typing-qualified dictionary annotations" $
        withParsedModule (T.unlines
          [ "import typing"
          , "mapping: typing.Dict[str, int] = {}"
          ]) $ \module_ ->
            case pyModuleBody module_ of
              [_importStmt, annStmt] -> case locatedValue annStmt of
                PyAnnAssign _ annType _ ->
                  case locValue annType of
                    TypeSubscript base [keyType, valueType] -> do
                      expectSimpleTypeName base ["typing"] "Dict"
                      expectSimpleTypeName keyType [] "str"
                      expectSimpleTypeName valueType [] "int"
                    other -> expectationFailure $ "Expected Dict annotation, found " <> show other
                other -> expectationFailure $ "Expected annotated assignment, found " <> show other
              other -> expectationFailure $ "Unexpected module body: " <> show (length other)

      it "parses union annotations using the | operator" $
        withParsedModule "maybe_value: int | None = 0\n" $ \module_ ->
          case pyModuleBody module_ of
            [annStmt] -> case locatedValue annStmt of
              PyAnnAssign _ annType _ ->
                case locValue annType of
                  TypeUnion members -> do
                    length members `shouldBe` 2
                    case members of
                      (firstMember:_) -> do
                        expectSimpleTypeName firstMember [] "int"
                        expectSimpleTypeName (members !! 1) [] "None"
                      [] -> expectationFailure "members list is empty"
                  other -> expectationFailure $ "Expected TypeUnion, found " <> show other
              other -> expectationFailure $ "Expected annotated assignment, found " <> show other
            _ -> expectationFailure "Expected single annotated assignment"

      it "parses Callable annotations with tuple arguments" $
        withParsedModule (T.unlines
          [ "from typing import Callable"
          , "callback: Callable[[int, str], bool] = lambda a, b: True"
          ]) $ \module_ ->
            case pyModuleBody module_ of
              [_importStmt, annStmt] -> case locatedValue annStmt of
                PyAnnAssign _ annType _ ->
                  case locValue annType of
                    TypeSubscript base [argsNode, retNode] -> do
                      expectSimpleTypeName base [] "Callable"
                      case locValue argsNode of
                        TypeTuple elems -> do
                          length elems `shouldBe` 2
                          case elems of
                            (firstElem:secondElem:_) -> do
                              expectSimpleTypeName firstElem [] "int"
                              expectSimpleTypeName secondElem [] "str"
                            _ -> expectationFailure "Expected at least 2 elements in tuple"
                        other -> expectationFailure $ "Expected tuple argument list, found " <> show other
                      expectSimpleTypeName retNode [] "bool"
                    other -> expectationFailure $ "Expected Callable annotation, found " <> show other
                other -> expectationFailure $ "Expected annotated assignment, found " <> show other
              other -> expectationFailure $ "Unexpected module body: " <> show (length other)

      it "parses lambda expressions inline" $
        withParsedModule "square = lambda x: x * x\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAssign _ valueExpr -> case locValue valueExpr of
                PyLambda params bodyExpr -> do
                  length params `shouldBe` 1
                  case map locValue params of
                    [ParamNormal (Identifier name) _ _] -> name `shouldBe` "x"
                    other -> expectationFailure $ "Unexpected lambda params: " <> show other
                  case locValue bodyExpr of
                    PyBinaryOp OpMul left right -> do
                      locValue left `shouldBe` PyVar (Identifier "x")
                      locValue right `shouldBe` PyVar (Identifier "x")
                    other -> expectationFailure $ "Expected multiplication body, found " <> show other
                other -> expectationFailure $ "Expected lambda expression, found " <> show other
              other -> expectationFailure $ "Expected assignment, found " <> show other
            _ -> expectationFailure "Expected single lambda assignment"

      it "parses assignment expressions (walrus operator)" $
        withParsedModule "result = (n := 5)\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAssign _ valueExpr -> case locValue valueExpr of
                PyNamedExpr pat expr -> do
                  locValue pat `shouldBe` PatVar (Identifier "n")
                  locValue expr `shouldBe` PyLiteral (PyInt 5)
                other -> expectationFailure $ "Expected named expression, found " <> show other
              other -> expectationFailure $ "Expected assignment, found " <> show other
            _ -> expectationFailure "Expected single walrus assignment"

      it "parses chained assignments" $
        withParsedModule "alpha = beta = 42\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAssign targets valueExpr -> do
                length targets `shouldBe` 2
                case map locValue targets of
                  [PatVar (Identifier "alpha"), PatVar (Identifier "beta")] -> pure ()
                  other -> expectationFailure $ "Unexpected targets: " <> show other
                locValue valueExpr `shouldBe` PyLiteral (PyInt 42)
              other -> expectationFailure $ "Expected assignment, found " <> show other
            _ -> expectationFailure "Expected single chained assignment"

      it "respects bitwise operator precedence" $
        withParsedModule "mask = a | b & c\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAssign _ valueExpr -> case locValue valueExpr of
                PyBinaryOp Fluxus.AST.Common.OpBitOr left right -> do
                  locValue left `shouldBe` PyVar (Identifier "a")
                  case locValue right of
                    PyBinaryOp Fluxus.AST.Common.OpBitAnd innerLeft innerRight -> do
                      locValue innerLeft `shouldBe` PyVar (Identifier "b")
                      locValue innerRight `shouldBe` PyVar (Identifier "c")
                    other -> expectationFailure $ "Expected bitwise and on RHS, found " <> show other
                other -> expectationFailure $ "Expected bitwise or expression, found " <> show other
              other -> expectationFailure $ "Expected assignment, found " <> show other
            _ -> expectationFailure "Expected single bitwise assignment"

      it "parses raise statements with causes" $
        withParsedModule "raise ValueError('boom') from err\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyRaise mExc mCause -> do
                case mExc of
                  Just excExpr -> case locValue excExpr of
                    PyCall (Located _ (PyVar (Identifier "ValueError"))) _ -> pure ()
                    other -> expectationFailure $ "Unexpected raise target: " <> show other
                  Nothing -> expectationFailure "Expected exception expression"
                case mCause of
                  Just causeExpr -> locValue causeExpr `shouldBe` PyVar (Identifier "err")
                  Nothing -> expectationFailure "Expected raise cause"
              other -> expectationFailure $ "Expected raise statement, found " <> show other
            _ -> expectationFailure "Expected single raise statement"

      it "parses async for loops" $
        withParsedModule (T.unlines
          [ "async for item in items:"
          , "    pass"
          ]) $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAsyncFor target iter body elseBody -> do
                case locValue target of
                  PatVar (Identifier name) -> name `shouldBe` "item"
                  other -> expectationFailure $ "Unexpected async for target: " <> show other
                locValue iter `shouldBe` PyVar (Identifier "items")
                length body `shouldBe` 1
                case body of
                  [loopStmt] -> locValue loopStmt `shouldBe` PyPass
                  _ -> expectationFailure "Expected single pass in async for body"
                elseBody `shouldBe` []
              other -> expectationFailure $ "Expected async for statement, found " <> show other
            _ -> expectationFailure "Expected single async for statement"

      it "parses slicing expressions with explicit step" $
        withParsedModule "result = values[1:5:2]\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAssign _ valueExpr -> case locValue valueExpr of
                PySubscript _ sliceNode -> case locValue sliceNode of
                  SliceSlice (Just start) (Just stop) (Just stepExpr) -> do
                    locValue start `shouldBe` PyLiteral (PyInt 1)
                    locValue stop `shouldBe` PyLiteral (PyInt 5)
                    locValue stepExpr `shouldBe` PyLiteral (PyInt 2)
                  other -> expectationFailure $ "Expected slice with step, found " <> show other
                other -> expectationFailure $ "Expected subscript expression, found " <> show other
              other -> expectationFailure $ "Expected assignment, found " <> show other
            _ -> expectationFailure "Expected single assignment"
      
      it "parses multi-dimensional slicing with ellipsis" $
        withParsedModule "element = matrix[:, index, ...]\n" $ \module_ ->
          case pyModuleBody module_ of
            [stmt] -> case locatedValue stmt of
              PyAssign _ valueExpr -> case locValue valueExpr of
                PySubscript _ sliceNode -> case locValue sliceNode of
                  SliceExtSlice slices -> do
                    length slices `shouldBe` 3
                    case map locValue slices of
                      [SliceSlice Nothing Nothing Nothing, SliceIndex idxExpr, SliceIndex ellipsisExpr] -> do
                        locValue idxExpr `shouldBe` PyVar (Identifier "index")
                        locValue ellipsisExpr `shouldBe` PyLiteral PyEllipsis
                      other -> expectationFailure $ "Unexpected slice components: " <> show other
                  other -> expectationFailure $ "Expected extended slice, found " <> show other
                other -> expectationFailure $ "Expected subscript expression, found " <> show other
              other -> expectationFailure $ "Expected assignment, found " <> show other
            _ -> expectationFailure "Expected single assignment"

  describe "match statements" $ do
    it "parses match statements with literal and capture patterns" $
      withParsedModule (T.unlines
        [ "match value:"
        , "    case 0:"
        , "        pass"
        , "    case other:"
        , "        pass"
        ]) $ \module_ ->
        case pyModuleBody module_ of
          [matchStmt] -> case locatedValue matchStmt of
            PyMatch subject cases -> do
              locValue subject `shouldBe` PyVar (Identifier "value")
              length cases `shouldBe` 2
              case cases of
                (firstCase:secondCase:_) -> do
                  let firstClause = locatedValue firstCase
                      secondClause = locatedValue secondCase
                  case locValue (pyCasePattern firstClause) of
                    PatLiteral (PyInt 0) -> pure ()
                    other -> expectationFailure $ "Expected literal pattern, found " <> show other
                  case locValue (pyCasePattern secondClause) of
                    PatVar (Identifier name) -> name `shouldBe` "other"
                    other -> expectationFailure $ "Expected capture pattern, found " <> show other
                  map (length . pyCaseBody . locatedValue) cases `shouldSatisfy` all (>= 1)
                _ -> expectationFailure "Expected at least 2 cases"
            other -> expectationFailure $ "Expected match statement, found " <> show other
          _ -> expectationFailure "Expected single statement"

    it "parses match statements with OR patterns" $
      withParsedModule (T.unlines
        [ "match status:"
        , "    case \"ready\" | \"ok\":"
        , "        pass"
        ]) $ \module_ ->
        case pyModuleBody module_ of
          [matchStmt] -> case locatedValue matchStmt of
            PyMatch _ [caseClause] ->
              case locValue (pyCasePattern (locatedValue caseClause)) of
                PatOr alts -> do
                  let literalPatterns = map locValue (NE.toList alts)
                  literalPatterns `shouldSatisfy` all (\p -> case p of
                    PatLiteral (PyString _) -> True
                    _ -> False)
                other -> expectationFailure $ "Expected OR pattern, found " <> show other
            other -> expectationFailure $ "Expected single case match, found " <> show other
          _ -> expectationFailure "Expected single match statement"

    it "supports guards and sequence patterns with starred targets" $
      withParsedModule (T.unlines
        [ "match data:"
        , "    case [head, *tail] if head > 0:"
        , "        pass"
        ]) $ \module_ ->
        case pyModuleBody module_ of
          [matchStmt] -> case locatedValue matchStmt of
            PyMatch _ [caseClause] -> do
              case locValue (pyCasePattern (locatedValue caseClause)) of
                PatList patterns -> do
                  length patterns `shouldBe` 2
                  case map locValue patterns of
                    [PatVar (Identifier "head"), PatStarred inner] ->
                      locValue inner `shouldBe` PatVar (Identifier "tail")
                    other -> expectationFailure $ "Unexpected sequence layout: " <> show other
                other -> expectationFailure $ "Expected list pattern, found " <> show other
              case pyCaseGuard (locatedValue caseClause) of
                Just guardExpr -> case locValue guardExpr of
                  PyComparison [Fluxus.AST.Common.OpGt] [lhs, rhs] -> do
                    locValue lhs `shouldBe` PyVar (Identifier "head")
                    locValue rhs `shouldBe` PyLiteral (PyInt 0)
                  other -> expectationFailure $ "Unexpected guard expression: " <> show other
                Nothing -> expectationFailure "Expected guard expression"
            other -> expectationFailure $ "Expected single case match, found " <> show other
          _ -> expectationFailure "Expected single match statement"

    it "parses class patterns with positional and keyword subpatterns" $
      withParsedModule (T.unlines
        [ "match point:"
        , "    case Point(x, y=y_val):"
        , "        pass"
        ]) $ \module_ ->
        case pyModuleBody module_ of
          [matchStmt] -> case locatedValue matchStmt of
            PyMatch _ [caseClause] ->
              case locValue (pyCasePattern (locatedValue caseClause)) of
                PatClass classExpr posArgs kwArgs -> do
                  locValue classExpr `shouldBe` PyVar (Identifier "Point")
                  map locValue posArgs `shouldBe` [PatVar (Identifier "x")]
                  length kwArgs `shouldBe` 1
                  case kwArgs of
                    ((kwName, kwPattern):_) -> do
                      kwName `shouldBe` Identifier "y"
                      locValue kwPattern `shouldBe` PatVar (Identifier "y_val")
                    _ -> expectationFailure "Expected at least 1 keyword argument"
                other -> expectationFailure $ "Expected class pattern, found " <> show other
            other -> expectationFailure $ "Expected single case match, found " <> show other
          _ -> expectationFailure "Expected single match statement"

    it "parses value patterns with dotted names" $
      withParsedModule (T.unlines
        [ "match color:"
        , "    case Palette.PRIMARY:"
        , "        pass"
        ]) $ \module_ ->
        case pyModuleBody module_ of
          [matchStmt] -> case locatedValue matchStmt of
            PyMatch _ [caseClause] ->
              case locValue (pyCasePattern (locatedValue caseClause)) of
                PatValue expr ->
                  case locValue expr of
                    PyAttribute base attrName -> do
                      locValue base `shouldBe` PyVar (Identifier "Palette")
                      attrName `shouldBe` Identifier "PRIMARY"
                    other -> expectationFailure $ "Unexpected value expression: " <> show other
                other -> expectationFailure $ "Expected value pattern, found " <> show other
            other -> expectationFailure $ "Expected match statement, found " <> show other
          _ -> expectationFailure "Expected single match statement"

    -- Helper functions
mockTokens :: [PythonToken] -> [Located PythonToken]
mockTokens tokens = map mockToken tokens


mockToken :: PythonToken -> Located PythonToken
mockToken token = Located mockSpan token
  where
    mockSpan = SourceSpan "test.py" (SourcePos 1 1) (SourcePos 1 10)

isLiteral :: PythonExpr -> Bool
isLiteral (PyLiteral _) = True
isLiteral _ = False

isVarParam :: PythonParameter -> Bool
isVarParam (ParamNormal (Identifier name) _ _) = name == "a"
isVarParam _ = False

isVarArgsParam :: PythonParameter -> Bool
isVarArgsParam (ParamVarArgs (Identifier name) _) = name == "args"
isVarArgsParam _ = False

isKwArgsParam :: PythonParameter -> Bool
isKwArgsParam (ParamKwArgs (Identifier name) _) = name == "kwargs"
isKwArgsParam _ = False

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
expectSimpleTypeName :: Located PythonTypeExpr -> [Text] -> Text -> Expectation
expectSimpleTypeName located expectedModules expectedName =
  case locValue located of
    TypeName qn -> do
      let modules = map moduleNameTextTest (qnModule qn)
      modules `shouldBe` expectedModules
      identifierTextTest (qnName qn) `shouldBe` expectedName
    other ->
      expectationFailure $ "Expected simple type name, found " <> show other

moduleNameTextTest :: ModuleName -> Text
moduleNameTextTest (ModuleName txt) = txt

identifierTextTest :: Identifier -> Text
identifierTextTest (Identifier txt) = txt
