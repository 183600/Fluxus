#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Fluxus.Analysis.ShapeAnalysis
import Fluxus.AST.Common

main :: IO ()
main = do
  putStrLn "Debugging Shape Analysis..."
  let code = T.unlines
        [ "def func():"
        , "    x = 42"
        , "    y = [1, 2, 3]"
        , "    return x"
        ]
  
  putStrLn $ "Code to analyze: " ++ show code
  
  case analyzeShapeFromText code of
    Left err -> putStrLn $ "Error: " ++ show err
    Right state -> do
      putStrLn $ "Analysis state: " ++ show state
      
      -- Test getVariableShape
      let result = runShapeAnalysis $ getVariableShape state "x"
      case result of
        Left err -> putStrLn $ "getVariableShape error: " ++ show err
        Right (shape, _) -> putStrLn $ "Shape of x: " ++ show shape
  
  putStrLn "Debug completed."