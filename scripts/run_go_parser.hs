{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.FilePath (takeFileName)

import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> do
      src <- TIO.readFile fp
      case runGoLexer (T.pack (takeFileName fp)) src of
        Left lerr -> putStrLn $ "LEXER ERROR: " ++ show lerr
        Right toks -> case runGoParser (T.pack (takeFileName fp)) toks of
          Left perr -> putStrLn $ "PARSER ERROR: " ++ show perr
          Right _ -> putStrLn "OK"
    _ -> putStrLn "Usage: run_go_parser.hs <file.go>"
