{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Fluxus.Parser.Go.Lexer

main :: IO ()
main = do
  let input = "( ) { } [ ] , ; . : ..."
  case runGoLexer "test.go" input of
    Left err -> putStrLn $ "Lexer failed: " ++ show err
    Right tokens -> do
      putStrLn $ "Number of tokens: " ++ show (length tokens)
      mapM_ print tokens