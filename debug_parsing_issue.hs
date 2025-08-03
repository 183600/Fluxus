{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.Parser.Go.Lexer
import Fluxus.Parser.Go.Parser
import Fluxus.AST.Go
import Fluxus.AST.Common
import System.IO

main :: IO ()
main = do
    let code = "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n    x := 42\n    fmt.Println(x)\n    return\n}\n"
    
    putStrLn "=== Input Code ==="
    TIO.putStrLn code
    
    putStrLn "\n=== Lexing ==="
    case runGoLexer "<test>" code of
        Left err -> putStrLn $ "Lexer error: " ++ show err
        Right tokens -> do
            putStrLn $ "Tokens found: " ++ show (length tokens)
            mapM_ (\(i, token) -> putStrLn $ show i ++ ": " ++ show token) (zip [0..] tokens)
            
            putStrLn "\n=== Parsing ==="
            case runGoParser "<test>" tokens of
                Left err -> putStrLn $ "Parser error: " ++ show err
                Right (GoAST goPackage) -> do
                    putStrLn $ "Package name: " ++ show (goPackageName goPackage)
                    putStrLn $ "Files: " ++ show (length $ goPackageFiles goPackage)
                    let firstFile = head $ goPackageFiles goPackage
                    putStrLn $ "Declarations in first file: " ++ show (length $ goFileDecls firstFile)
                    mapM_ (putStrLn . ("  " ++) . showDecl) (goFileDecls firstFile)
  where
    showDecl (Located _ (GoFuncDecl func)) = 
        "Function: " ++ maybe "anonymous" (\(Identifier n) -> T.unpack n) (goFuncName func) ++
        " (body: " ++ show (isJust $ goFuncBody func) ++ ")"
    showDecl (Located _ decl) = "Other: " ++ show decl
    
    isJust Nothing = False
    isJust (Just _) = True