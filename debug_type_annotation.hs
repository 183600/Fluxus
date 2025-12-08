#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.Megaparsec (parse, eof)
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.AST.Common (Located(..), locatedValue, locValue)

main :: IO ()
main = do
  let source = T.unlines
        [ "def summarize(values: list[int]) -> tuple[int, str]:"
        , "    return values[0], \"\""]
  
  putStrLn "Testing source code:"
  putStrLn $ T.unpack source
  putStrLn "\n=== Lexer output ==="
  
  case runPythonLexer "test.py" source of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (length tokens)
      mapM_ print tokens
      
      putStrLn "\n=== Parser output ==="
      case runPythonParser "test.py" tokens of
        Left perr -> do
          putStrLn $ "Parser error: " ++ show perr
          -- Try to parse just the first statement
          case parse (parseStatement <* eof) "test.py" tokens of
            Left err2 -> putStrLn $ "Statement parse error: " ++ show err2
            Right stmt -> putStrLn $ "Parsed statement: " ++ show stmt
        Right (PythonAST module_) -> do
          putStrLn $ "Parsed module with " ++ show (length (pyModuleBody module_)) ++ " statements"
          case pyModuleBody module_ of
            [funcStmt] -> case locatedValue funcStmt of
              PyFuncDef funcDef -> do
                putStrLn $ "Function name: " ++ show (pyFuncName funcDef)
                putStrLn $ "Parameters: " ++ show (length (pyFuncParams funcDef))
                
                case pyFuncParams funcDef of
                  [param] -> case locValue param of
                    ParamNormal _ (Just ann) _ -> do
                      putStrLn $ "Parameter annotation: " ++ show ann
                      case locValue ann of
                        TypeSubscript base [inner] -> do
                          putStrLn $ "Base: " ++ show base
                          putStrLn $ "Inner: " ++ show inner
                        other -> putStrLn $ "Expected TypeSubscript, found: " ++ show other
                    other -> putStrLn $ "Expected normal parameter with annotation, found: " ++ show other
                  _ -> putStrLn $ "Expected single parameter, found: " ++ show (length (pyFuncParams funcDef))
                
                case pyFuncReturns funcDef of
                  Just retAnn -> putStrLn $ "Return annotation: " ++ show retAnn
                  Nothing -> putStrLn "No return annotation"
                  
              other -> putStrLn $ "Expected function definition, found: " ++ show other
            other -> putStrLn $ "Expected single statement, found: " ++ show (length (pyModuleBody module_))