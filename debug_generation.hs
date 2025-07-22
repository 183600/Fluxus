{-# LANGUAGE OverloadedStrings #-}
import Fluxus.Parser.Go.Parser
import Fluxus.CodeGen.CPP
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Fluxus.AST.Go

main = do
  content <- TIO.readFile "debug_function_parsing.go"
  case runGoParser "debug_function_parsing.go" content of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> do
      putStrLn "=== PARSED AST ==="
      print ast
      putStrLn "\n=== GENERATED C++ ==="
      let cppUnit = generateCpp defaultCppGenConfig (Right ast)
      putStrLn "Includes:"
      mapM_ (putStrLn . ("  " ++) . T.unpack) (cppIncludes cppUnit)
      putStrLn "Declarations:"
      mapM_ (\decl -> putStrLn $ "  " ++ show decl) (cppDeclarations cppUnit)