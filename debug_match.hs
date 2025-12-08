#!/usr/bin/env stack
-- stack script --resolver lts-21.25

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser
import Fluxus.AST.Python
import Fluxus.AST.Common (Located(..), locatedValue)

main :: IO ()
main = do
  let input = T.unlines
        [ "match data:"
        , "    case [head, *tail] if head > 0:"
        , "        pass"
        ]
  
  putStrLn "Testing match statement parsing..."
  putStrLn $ "Input:\n" ++ T.unpack input
  
  case runPythonLexer "test.py" input of
    Left err -> do
      putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens: " ++ show (map locatedValue tokens)
      case runPythonParser "test.py" tokens of
        Left perr -> do
          putStrLn $ "Parser error: " ++ show perr
        Right (PythonAST module_) -> do
          putStrLn $ "Parsed successfully! Module has " ++ show (length (pyModuleBody module_)) ++ " statements"
          case pyModuleBody module_ of
            [stmt] -> do
              putStrLn $ "Statement type: " ++ show (locatedValue stmt)
              case locatedValue stmt of
                PyMatch subject cases -> do
                  putStrLn $ "Match subject: " ++ show subject
                  putStrLn $ "Number of cases: " ++ show (length cases)
                  mapM_ printCase cases
                _ -> putStrLn "Not a match statement"
            _ -> putStrLn "Expected single statement"
  where
    printCase caseClause = do
      putStrLn $ "Case pattern: " ++ show (locValue (pyCasePattern (locatedValue caseClause)))
      putStrLn $ "Case guard: " ++ show (pyCaseGuard (locatedValue caseClause))
      putStrLn $ "Case body length: " ++ show (length (pyCaseBody (locatedValue caseClause)))