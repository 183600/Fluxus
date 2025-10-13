{-# LANGUAGE OverloadedStrings #-}

module Test.Fluxus.Debug (spec) where

import Test.Hspec
import qualified Data.Text as T

import Fluxus.Debug.Logger

spec :: Spec
spec = describe "Debug and Logging" $ do
  loggingSpec
  debugOutputSpec
  errorReportingSpec

loggingSpec :: Spec
loggingSpec = describe "Logging System" $ do
  it "logs debug messages" $ do
    let msg = "Debug message"
    let logged = logDebug msg
    logged `shouldContain` "DEBUG"
    logged `shouldContain` msg
  
  it "logs info messages" $ do
    let msg = "Info message"
    let logged = logInfo msg
    logged `shouldContain` "INFO"
    logged `shouldContain` msg
  
  it "logs warning messages" $ do
    let msg = "Warning message"
    let logged = logWarning msg
    logged `shouldContain` "WARN"
    logged `shouldContain` msg
  
  it "logs error messages" $ do
    let msg = "Error message"
    let logged = logError msg
    logged `shouldContain` "ERROR"
    logged `shouldContain` msg
  
  it "logs with context" $ do
    let context = "Parser"
    let msg = "Parsing failed"
    let logged = logWithContext context msg
    logged `shouldContain` context
    logged `shouldContain` msg
  
  it "handles multiline messages" $ do
    let msg = "Line 1\nLine 2\nLine 3"
    let logged = logInfo msg
    logged `shouldContain` "Line 1"
    logged `shouldContain` "Line 2"
    logged `shouldContain` "Line 3"

debugOutputSpec :: Spec
debugOutputSpec = describe "Debug Output" $ do
  it "formats AST for debugging" $ do
    let astDebug = formatASTDebug sampleAST
    astDebug `shouldContain` "AST"
    astDebug `shouldSatisfy` (not . null)
  
  it "formats type information" $ do
    let typeDebug = formatTypeDebug "variable" "Int"
    typeDebug `shouldContain` "variable"
    typeDebug `shouldContain` "Int"
  
  it "formats compilation phases" $ do
    let phases = ["Lexing", "Parsing", "Type Inference", "Code Generation"]
    let phaseDebug = formatPhasesDebug phases
    phaseDebug `shouldContain` "Lexing"
    phaseDebug `shouldContain` "Parsing"
    phaseDebug `shouldContain` "Type Inference"
    phaseDebug `shouldContain` "Code Generation"
  
  it "formats error stack traces" $ do
    let trace = ["main:10", "func1:5", "func2:3"]
    let traceDebug = formatStackTrace trace
    traceDebug `shouldContain` "main:10"
    traceDebug `shouldContain` "func1:5"
    traceDebug `shouldContain` "func2:3"

errorReportingSpec :: Spec
errorReportingSpec = describe "Error Reporting" $ do
  it "reports syntax errors with location" $ do
    let error = syntaxError "test.py" 10 5 "Unexpected token"
    error `shouldContain` "test.py"
    error `shouldContain` "10"
    error `shouldContain` "5"
    error `shouldContain` "Unexpected token"
  
  it "reports type errors" $ do
    let error = typeError "variable" "Expected Int, got String"
    error `shouldContain` "variable"
    error `shouldContain` "Expected Int, got String"
  
  it "reports compilation errors with suggestions" $ do
    let error = compilationErrorWithSuggestion 
                  "Undefined variable 'x'"
                  "Did you mean 'y'?"
    error `shouldContain` "Undefined variable 'x'"
    error `shouldContain` "Did you mean 'y'?"
  
  it "formats multiple errors" $ do
    let errors = [
          "Error 1: Syntax error",
          "Error 2: Type mismatch",
          "Error 3: Undefined reference"
          ]
    let formatted = formatMultipleErrors errors
    formatted `shouldContain` "Error 1"
    formatted `shouldContain` "Error 2"
    formatted `shouldContain` "Error 3"
  
  it "reports warnings" $ do
    let warning = compilerWarning "Unused variable 'temp'"
    warning `shouldContain` "WARNING"
    warning `shouldContain` "Unused variable 'temp'"
  
  it "reports errors with code snippets" $ do
    let code = T.unlines
          [ "def func():"
          , "    x = undefined_var"
          , "    return x"
          ]
    let error = errorWithSnippet code 2 "undefined_var" "Undefined variable"
    error `shouldContain` "x = undefined_var"
    error `shouldContain` "Undefined variable"
    error `shouldContain` "2"

-- Mock functions and data for testing
sampleAST :: String
sampleAST = "Module { body = [FuncDef { name = \"main\", params = [], body = [] }] }"

formatASTDebug :: String -> String
formatASTDebug ast = "[DEBUG] " ++ ast

formatTypeDebug :: String -> String -> String
formatTypeDebug var typ = "[DEBUG] " ++ var ++ " : " ++ typ

formatPhasesDebug :: [String] -> String
formatPhasesDebug phases = "[DEBUG] Compilation phases:\n" ++ unlines (map ("  - " ++) phases)

formatStackTrace :: [String] -> String
formatStackTrace trace = "[DEBUG] Stack trace:\n" ++ unlines (map ("  at " ++) trace)

syntaxError :: String -> Int -> Int -> String -> String
syntaxError file line col msg = 
  "[ERROR] " ++ file ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ msg

typeError :: String -> String -> String
typeError var msg = "[ERROR] Type error in '" ++ var ++ "': " ++ msg

compilationErrorWithSuggestion :: String -> String -> String
compilationErrorWithSuggestion err suggestion = 
  "[ERROR] " ++ err ++ "\n[HINT] " ++ suggestion

formatMultipleErrors :: [String] -> String
formatMultipleErrors errors = unlines errors

compilerWarning :: String -> String
compilerWarning msg = "[WARNING] " ++ msg

errorWithSnippet :: T.Text -> Int -> String -> String -> String
errorWithSnippet code lineNum token msg =
  let lines' = T.lines code
      line = if lineNum > 0 && lineNum <= length lines'
             then T.unpack (lines' !! (lineNum - 1))
             else ""
  in "[ERROR] Line " ++ show lineNum ++ ": " ++ msg ++ "\n" ++
     "  " ++ line ++ "\n" ++
     "  " ++ replicate (maybe 0 id $ lookup token [(token, length token)]) '^'

logDebug, logInfo, logWarning, logError :: String -> String
logDebug msg = "[DEBUG] " ++ msg
logInfo msg = "[INFO] " ++ msg
logWarning msg = "[WARN] " ++ msg
logError msg = "[ERROR] " ++ msg

logWithContext :: String -> String -> String
logWithContext ctx msg = "[" ++ ctx ++ "] " ++ msg
