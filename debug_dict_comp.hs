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
    let numbersLiteral = noLoc (PyList (map (noLoc . PyLiteral . PyInt) [1, 2]))
        numbersAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "numbers"))] numbersLiteral)
        comprehension = PythonComprehension
          { pyCompTarget = noLoc (PatVar (Identifier "n"))
          , pyCompIter = noLoc (PyVar (Identifier "numbers"))
          , pyCompFilters = []
          , pyCompAsync = False
          }
        keyExpr = noLoc (PyVar (Identifier "n"))
        valueExpr = noLoc (PyBinaryOp OpMul (noLoc (PyVar (Identifier "n"))) (noLoc (PyVar (Identifier "n"))))
        dictCompExpr = noLoc (PyDictComp keyExpr valueExpr [comprehension])
        squaresAssign = noLoc (PyAssign [noLoc (PatVar (Identifier "squares"))] dictCompExpr)
        pythonAst = PythonAST PythonModule { pyModuleName = Nothing, pyModuleDoc = Nothing, pyModuleImports = [], pyModuleBody = [numbersAssign, squaresAssign] }
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
            putStrLn "\n=== Squares Variable ==="
            let squaresVar = [decl | decl@(CppVariable "squares" _ _) <- cppDeclarations (cgrUnit res)]
            case squaresVar of
                [var] -> print var
                _ -> putStrLn "Squares variable not found or multiple found"
        Left failure -> do
            putStrLn "\n=== Generation Failed ==="
            print failure