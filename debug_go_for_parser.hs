{-# LANGUAGE OverloadedStrings #-}

import Fluxus.Parser.Go.Parser
import Fluxus.Lexer.Go.Lexer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let source = T.unlines
        [ "package main"
        , ""
        , "import \"fmt\""
        , ""
        , "func main() {"
        , "    for i := 1; i <= 3; i++ {"
        , "        fmt.Println(i)"
        , "    }"
        , "}"
        ]
  
  putStrLn "=== PARSING DEBUG ==="
  putStrLn "Source code:"
  TIO.putStrLn source
  putStrLn ""
  
  case runGoLexer source of
    Left lexErr -> do
      putStrLn "Lexer error:"
      print lexErr
    Right tokens -> do
      putStrLn "Tokens:"
      mapM_ print (take 50 tokens)
      putStrLn ""
      
      case runGoParser parseFile tokens of
        Left parseErr -> do
          putStrLn "Parser error:"
          print parseErr
        Right ast -> do
          putStrLn "AST:"
          print ast