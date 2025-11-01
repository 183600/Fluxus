{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Fluxus.Analysis.TypeInference
import Fluxus.Analysis.EscapeAnalysis
import Fluxus.Analysis.OwnershipInference
import Fluxus.Analysis.SmartFallback
import Fluxus.CodeGen.CPP

main :: IO ()
main = defaultMain
  [ bgroup "analysis"
      [ bench "type inference" $ nf typeInferenceWorkload sampleInferenceExpr
      , bench "escape optimization" $ nf escapeWorkload sampleEscapeExpr
      , bench "ownership optimization" $ nf ownershipWorkload sampleOwnershipExprs
      , bench "smart fallback" $ nf fallbackWorkload sampleFallbackExpr
      ]
  , bgroup "codegen"
      [ bench "python module" $ nf pythonCodegenWorkload pythonSampleModule
      , bench "go module" $ nf goCodegenWorkload goSampleAST
      ]
  ]

-------------------------------------------------------------------------------
-- Analysis workloads ---------------------------------------------------------
-------------------------------------------------------------------------------

typeInferenceWorkload :: CommonExpr -> Bool
typeInferenceWorkload expr =
  either (const False) (const True) $
    runTypeInference sampleTypeEnv (inferType expr)

escapeWorkload :: CommonExpr -> ((CommonExpr, [T.Text]), EscapeAnalysisState)
escapeWorkload expr = runEscapeAnalysis (optimizeMemoryAllocation expr)

ownershipWorkload :: [CommonExpr] -> Bool
ownershipWorkload exprs =
  either (const False) (const True) $
    runOwnershipInference (optimizeOwnership exprs)

fallbackWorkload :: CommonExpr -> CommonExpr
fallbackWorkload expr = fst (runSmartFallback (optimizeWithFallback expr))

sampleTypeEnv :: HashMap.HashMap Identifier Type
sampleTypeEnv = HashMap.fromList
  [ (Identifier "sum_three", TFunction [TInt 32, TInt 32, TInt 32] (TInt 32))
  , (Identifier "scale", TFunction [TFloat 64, TFloat 64] (TFloat 64))
  , (Identifier "flag", TBool)
  ]

sampleInferenceExpr :: CommonExpr
sampleInferenceExpr =
  CECall
    (noLoc (CEVar (Identifier "sum_three")))
    [ noLoc (CELiteral (LInt 10))
    , noLoc (CELiteral (LInt 20))
    , noLoc (CECall (noLoc (CEVar (Identifier "scale")))
        [ noLoc (CELiteral (LFloat 1.5))
        , noLoc (CELiteral (LFloat 2.0))
        ])
    ]

sampleEscapeExpr :: CommonExpr
sampleEscapeExpr =
  CEBinaryOp OpAdd
    (noLoc (CECall (noLoc (CEVar (Identifier "builder"))) []))
    (noLoc (CECall (noLoc (CEVar (Identifier "combine")))
      [ noLoc (CEVar (Identifier "state"))
      , noLoc (CELiteral (LInt 42))
      ]))

sampleOwnershipExprs :: [CommonExpr]
sampleOwnershipExprs =
  [ CECall (noLoc (CEVar (Identifier "allocate")))
      [ noLoc (CELiteral (LInt 128)) ]
  , CEBinaryOp OpConcat
      (noLoc (CELiteral (LString "flux")))
      (noLoc (CELiteral (LString "us")))
  , CEIndex (noLoc (CEVar (Identifier "buffer"))) (noLoc (CELiteral (LInt 0)))
  ]

sampleFallbackExpr :: CommonExpr
sampleFallbackExpr =
  CEBinaryOp OpAdd
    (noLoc (CECall (noLoc (CEVar (Identifier "dynamic_lookup")))
      [ noLoc (CEVar (Identifier "value")) ]))
    (noLoc (CELiteral (LInt 1)))

-------------------------------------------------------------------------------
-- Code generation workloads --------------------------------------------------
-------------------------------------------------------------------------------

pythonCodegenWorkload :: PythonAST -> CppUnit
pythonCodegenWorkload ast = generateCpp benchCppConfig (Left ast)

goCodegenWorkload :: GoAST -> CppUnit
goCodegenWorkload ast = generateCpp benchCppConfig (Right ast)

benchCppConfig :: CppGenConfig
benchCppConfig = CppGenConfig
  { cgcOptimizationLevel = 2
  , cgcEnableInterop = False
  , cgcTargetCppStd = "c++20"
  , cgcUseSmartPointers = True
  , cgcEnableParallel = False
  , cgcEnableCoroutines = False
  , cgcNamespace = "fluxus"
  , cgcHeaderGuard = "FLUXUS_BENCH"
  , cgcStrictMode = False
  }

pythonSampleModule :: PythonAST
pythonSampleModule = PythonAST moduleDef
  where
    moduleDef = PythonModule
      { pyModuleName = Just (ModuleName "bench")
      , pyModuleDoc = Nothing
      , pyModuleImports = []
      , pyModuleBody =
          [ noLoc (PyAssign [noLoc (PatVar (Identifier "scale"))] (noLoc (PyLiteral (PyFloat 1.5))))
          , noLoc (PyFuncDef adderDef)
          , noLoc (PyExprStmt (noLoc callExpr))
          ]
      }
    adderDef = PythonFuncDef
      { pyFuncName = Identifier "adder"
      , pyFuncDecorators = []
      , pyFuncParams =
          [ noLoc (ParamNormal (Identifier "a") Nothing Nothing)
          , noLoc (ParamNormal (Identifier "b") Nothing Nothing)
          ]
      , pyFuncReturns = Nothing
      , pyFuncBody =
          [ noLoc (PyReturn (Just (noLoc (PyBinaryOp OpAdd
              (noLoc (PyVar (Identifier "a")))
              (noLoc (PyVar (Identifier "b")))))))
          ]
      , pyFuncDoc = Nothing
      , pyFuncIsAsync = False
      }
    callExpr = PyCall (noLoc (PyVar (Identifier "print")))
      [ noLoc (ArgPositional (noLoc (PyCall (noLoc (PyVar (Identifier "adder")))
            [ noLoc (ArgPositional (noLoc (PyLiteral (PyInt 10))))
            , noLoc (ArgPositional (noLoc (PyLiteral (PyInt 32))))
            ])))
      ]

goSampleAST :: GoAST
goSampleAST = GoAST packageDef
  where
    packageDef = GoPackage
      { goPackageName = Identifier "bench"
      , goPackageFiles = [goFile]
      }
    goFile = GoFile
      { goFileName = "bench.go"
      , goFilePackage = Identifier "bench"
      , goFileImports = []
      , goFileDecls =
          [ noLoc (GoFuncDecl helperDef)
          , noLoc (GoFuncDecl mainDef)
          ]
      }
    helperDef = GoFunction
      { goFuncName = Just (Identifier "double")
      , goFuncParams = [GoParam (Identifier "x") (Just (GoTypeName (Identifier "int"))) Nothing]
      , goFuncResults = [GoParamAnonymous (GoTypeName (Identifier "int"))]
      , goFuncBody = Just (noLoc (GoBlock
          [ noLoc (GoReturn (Just (noLoc (GoBinaryOp "*"
              (noLoc (GoIdent (Identifier "x")))
              (noLoc (GoLiteral (GoInt 2)))
            ))))
          ]))
      }
    mainDef = GoFunction
      { goFuncName = Just (Identifier "main")
      , goFuncParams = []
      , goFuncResults = []
      , goFuncBody = Just (noLoc (GoBlock
          [ noLoc (GoAssign False [noLoc (GoIdent (Identifier "value"))]
              [noLoc (GoCall (noLoc (GoIdent (Identifier "double")))
                [noLoc (GoLiteral (GoInt 21))])])
          , noLoc (GoExprStmt (noLoc (GoCall (noLoc (GoSelector (noLoc (GoIdent (Identifier "fmt"))) (Identifier "Println")))
              [ noLoc (GoIdent (Identifier "value"))
              ])))
          ]))
      }
