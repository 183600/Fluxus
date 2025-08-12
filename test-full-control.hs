module Main where

import Fluxus.Parser.Python.Parser
import Fluxus.Parser.Python.Lexer
import Fluxus.AST.Common
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  let code = "# Comprehensive Python control structure test\n\n# While loop\ncount = 0\nwhile count < 5:\n    print(\"While loop:\", count)\n    count += 1\nelse:\n    print(\"While loop finished\")\n\n# For loop with range\nfor i in range(3):\n    print(\"For loop with range:\", i)\nelse:\n    print(\"For loop with range finished\")\n\n# For loop with list\nitems = [\"apple\", \"banana\", \"cherry\"]\nfor item in items:\n    print(\"For loop with list:\", item)\nelse:\n    print(\"For loop with list finished\")\n\n# Nested loops\nfor i in range(2):\n    for j in range(2):\n        print(\"Nested loop:\", i, j)\n\n# Break and continue\nfor i in range(10):\n    if i == 3:\n        print(\"Breaking at\", i)\n        break\n    if i % 2 == 0:\n        print(\"Continuing at\", i)\n        continue\n    print(\"Processing\", i)\n\n# While with break\nx = 0\nwhile True:\n    if x >= 3:\n        break\n    print(\"While with break:\", x)\n    x += 1\n\n"
  case runPythonLexer (T.pack "<input>") (T.pack code) of
    Left err -> putStrLn ("Lexer error: " ++ show err)
    Right tokens -> do
      putStrLn "Lexer success!"
      -- mapM_ printToken tokens
      case runPythonParser (T.pack "<input>") tokens of
        Left err -> putStrLn ("Parser error: " ++ show err)
        Right ast -> putStrLn "Parser success!"
  where
    printToken (Located _ token) = putStrLn $ "  " ++ show token