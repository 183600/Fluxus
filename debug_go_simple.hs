{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer (runGoLexer)
import Fluxus.Parser.Go.Parser (runGoParser)

main :: IO ()
main = do
  content <- TIO.readFile "test_go_function_issue.go"
  putStrLn "=== GO FILE CONTENT ==="
  TIO.putStrLn content
  putStrLn ""
  
  putStrLn "=== TOKENIZATION ==="
  case runGoLexer "test_go_function_issue.go" content of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ "Found " ++ show (length tokens) ++ " tokens"
      mapM_ print (take 20 tokens)  -- Show first 20 tokens
      putStrLn ""
      
      putStrLn "=== PARSING ==="
      case runGoParser "test_go_function_issue.go" tokens of
        Left err -> putStrLn $ "Parser error: " ++ show err
        Right ast -> do
          putStrLn "Parse successful!"
          print ast