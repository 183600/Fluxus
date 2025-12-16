{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Fluxus.Parser.Python.Lexer
import Fluxus.Parser.Python.Parser

main :: IO ()
main = do
  let code = "result = result + f\"{i}\"\n"
  case runPythonLexer "test.py" code of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right tokens -> do
      putStrLn "Tokens:"
      mapM_ print tokens
      case runPythonParser "test.py" tokens of
        Left err -> putStrLn $ "\nParser error: " ++ show err
        Right ast -> do
          putStrLn "\nAST:"
          print ast
