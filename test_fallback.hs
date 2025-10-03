#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

import Test.Fluxus.Analysis.SmartFallback
import qualified Data.Text as T

main :: IO ()
main = do
  let code = T.unlines
        [ "def func():"
        , "    try:"
        , "        # Complex optimization that might fail"
        , "        result = complex_operation()"
        , "    except Exception:"
        , "        result = fallback_operation()"
        , "    return result"
        ]

  case analyzeWithFallback code of
    Right strategy -> do
      putStrLn $ "Has optimization fallback: " ++ show (hasOptimizationFallback strategy)
      putStrLn $ "Number of fallback points: " ++ show (length (getFallbackPoints strategy))
    Left err -> do
      putStrLn $ "Error: " ++ show err