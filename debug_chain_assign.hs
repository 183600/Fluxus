#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.CodeGen.CPP
import Fluxus.CodeGen.CPP.AST
import Fluxus.CodeGen.CPP.Python
import Fluxus.Utils.Pretty

main :: IO ()
main = do
    let assignStmt = noLoc (PyAssign [noLoc (PatVar (Identifier "outer")), noLoc (PatVar (Identifier "inner"))] (noLoc (PyLiteral (PyInt 99))))
        funcDef = PythonFuncDef
          { pyFuncName = Identifier "assign_chain"
          , pyFuncDecorators = []
          , pyFuncParams = []
          , pyFuncReturns = Nothing
          , pyFuncBody = [assignStmt]
          , pyFuncDoc = Nothing
          , pyFuncIsAsync = False
          }
        pythonAst = PythonAST PythonModule { pyModuleName = Nothing, pyModuleDoc = Nothing, pyModuleImports = [], pyModuleBody = [noLoc (PyFuncDef funcDef)] }
        testConfig = CppGenConfig
            { cgcOptimizationLevel = 0
            , cgcEnableInterop = False
            , cgcTargetCppStd = "c++20"
            , cgcUseSmartPointers = False
            , cgcEnableParallel = False
            , cgcEnableCoroutines = False
            , cgcNamespace = "test"
            , cgcHeaderGuard = "TEST"
            , cgcStrictMode = False
            }
    
    putStrLn "=== Python AST ==="
    print pythonAst
    
    case generateCpp testConfig (Left pythonAst) of
        Right res -> do
            putStrLn "\n=== Generated C++ ==="
            TIO.putStrLn $ renderCppUnit (cgrUnit res)
            putStrLn "\n=== Declarations ==="
            mapM_ print (cppDeclarations (cgrUnit res))
            putStrLn "\n=== Function Body ==="
            let funcs = [body | CppFunction "assign_chain" _ _ body <- cppDeclarations (cgrUnit res)]
            case funcs of
                [body] -> do
                    putStrLn "Found function body:"
                    mapM_ print body
                    let chainDecls = [decl | CppDecl decl <- body]
                    putStrLn $ "\nChain declarations (" ++ show (length chainDecls) ++ "):"
                    mapM_ print chainDecls
                _ -> putStrLn "Function not found or multiple found"
        Left failure -> do
            putStrLn "\n=== Generation Failed ==="
            print failure