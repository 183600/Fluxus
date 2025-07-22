{-# LANGUAGE OverloadedStrings #-}
import qualified Fluxus.Parser.Python.Lexer as PyLexer
import qualified Fluxus.Parser.Python.Parser as PyParser
import qualified Fluxus.Parser.Go.Lexer as GoLexer
import qualified Fluxus.Parser.Go.Parser as GoParser
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  putStrLn "Testing Python parser..."
  pyCode <- TIO.readFile "examples/python/fibonacci.py"
  case PyLexer.runPythonLexer "fibonacci.py" pyCode of
    Left err -> putStrLn $ "Python lexer: FAILED - " ++ show err
    Right tokens -> do
      putStrLn $ "Python lexer: SUCCESS - " ++ show (length tokens) ++ " tokens"
      case PyParser.runPythonParser "fibonacci.py" tokens of
        Right ast -> putStrLn "Python parser: SUCCESS"
        Left err -> putStrLn $ "Python parser: FAILED - " ++ show err
  
  putStrLn "\nTesting Go parser..."
  goCode <- TIO.readFile "examples/go/fibonacci.go"
  case GoLexer.runGoLexer "fibonacci.go" goCode of
    Left err -> putStrLn $ "Go lexer: FAILED - " ++ show err
    Right tokens -> do
      putStrLn $ "Go lexer: SUCCESS - " ++ show (length tokens) ++ " tokens"
      case GoParser.runGoParser "fibonacci.go" tokens of
        Right ast -> putStrLn "Go parser: SUCCESS"
        Left err -> putStrLn $ "Go parser: FAILED - " ++ show err