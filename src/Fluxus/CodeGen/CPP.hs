{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Fluxus.CodeGen.CPP
  ( -- * C++ AST Types
    CppUnit(..)
  , CppDecl(..)
  , CppStmt(..)
  , CppExpr(..)
  , CppType(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppCatch(..)
  , CppCase(..)
  , CppGenConfig(..)
    -- * Code Generation
  , generateCpp
  , generateCppMain
    -- * Type Mapping
  , mapCommonTypeToCpp
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go as Go

-- | C++ compilation unit
data CppUnit = CppUnit
  { cppIncludes :: [Text]
  , cppNamespaces :: [Text]
  , cppDeclarations :: [CppDecl]
  } deriving (Show, Eq)

-- | C++ declarations
data CppDecl
  = CppFunction Text CppType [CppParam] [CppStmt]
  | CppVariable Text CppType (Maybe CppExpr)
  | CppNamespace Text [CppDecl]
  | CppClass Text [Text] [CppDecl]
  | CppPreprocessor Text
  | CppUsing Text CppType
  | CppStruct Text [CppDecl]
  | CppMethod Text CppType [CppParam] [CppStmt] Bool
  | CppConstructor Text [CppParam] [CppStmt]
  | CppDestructor Text [CppStmt] Bool
  | CppTypedef Text CppType
  | CppTemplate [Text] CppDecl
  | CppExternC [CppDecl]
  | CppCommentDecl Text
  deriving (Show, Eq)

-- | C++ statements
data CppStmt
  = CppReturn (Maybe CppExpr)
  | CppExprStmt CppExpr
  | CppIf CppExpr [CppStmt] [CppStmt]
  | CppWhile CppExpr [CppStmt]
  | CppFor (Maybe CppStmt) (Maybe CppExpr) (Maybe CppExpr) [CppStmt]
  | CppForRange Text CppExpr [CppStmt]
  | CppSwitch CppExpr [CppCase]
  | CppBlock [CppStmt]
  | CppDecl CppDecl
  | CppBreak
  | CppContinue
  | CppThrow (Maybe CppExpr)
  | CppTry [CppStmt] [CppCatch] [CppStmt]
  deriving (Show, Eq)

-- | C++ switch cases
data CppCase
  = CppCase CppExpr [CppStmt]
  | CppDefault [CppStmt]
  deriving (Show, Eq)

-- | C++ expressions
data CppExpr
  = CppVar Text
  | CppLiteral CppLiteral
  | CppBinary Text CppExpr CppExpr
  | CppCall CppExpr [CppExpr]
  | CppUnary Text CppExpr
  | CppMember CppExpr Text
  | CppPointerMember CppExpr Text
  | CppIndex CppExpr CppExpr
  | CppCast CppType CppExpr
  | CppStaticCast CppType CppExpr
  | CppDynamicCast CppType CppExpr
  | CppReinterpretCast CppType CppExpr
  | CppConstCast CppType CppExpr
  | CppSizeOf CppType
  | CppNew CppType [CppExpr]
  | CppDelete CppExpr
  | CppThis
  | CppMove CppExpr
  | CppTernary CppExpr CppExpr CppExpr
  | CppComma [CppExpr]
  | CppLambda [CppParam] [CppStmt] (Maybe CppType)
  | CppForward CppExpr
  | CppMakeUnique CppType [CppExpr]
  | CppMakeShared CppType [CppExpr]
  | CppInitList CppType [CppExpr]
  deriving (Show, Eq)

-- | C++ types
data CppType
  = CppVoid
  | CppBool
  | CppInt
  | CppUInt
  | CppFloat
  | CppDouble
  | CppChar
  | CppString
  | CppAuto
  | CppPointer CppType
  | CppReference CppType
  | CppConst CppType
  | CppVolatile CppType
  | CppSizeT
  | CppFunctionType [CppType] CppType
  | CppClassType Text [CppType]
  | CppTemplateType Text [CppType]
  | CppUniquePtr CppType
  | CppSharedPtr CppType
  | CppOptional CppType
  | CppVariant [CppType]
  | CppPair CppType CppType
  | CppTuple [CppType]
  | CppMap CppType CppType
  | CppUnorderedMap CppType CppType
  | CppVector CppType
  | CppTypeVar Text
  | CppDecltype CppExpr
  deriving (Show, Eq)

-- | C++ literals
data CppLiteral
  = CppIntLit Integer
  | CppFloatLit Double
  | CppCharLit Char
  | CppBoolLit Bool
  | CppStringLit Text
  | CppNullPtr
  deriving (Show, Eq)

-- | C++ function parameters
data CppParam = CppParam Text CppType (Maybe CppExpr)
  deriving (Show, Eq)

-- | C++ catch blocks
data CppCatch = CppCatch CppType Text [CppStmt]
  deriving (Show, Eq)

-- | C++ code generation configuration
data CppGenConfig = CppGenConfig
  { cgcOptimizationLevel :: Int
  , cgcEnableInterop :: Bool
  , cgcTargetCppStd :: Text
  , cgcUseSmartPointers :: Bool
  , cgcEnableParallel :: Bool
  , cgcEnableCoroutines :: Bool
  , cgcNamespace :: Text
  , cgcHeaderGuard :: Text
  } deriving (Show, Eq)

-- | Generate C++ code from AST (without main function)
generateCpp :: CppGenConfig -> Either PythonAST GoAST -> (CppUnit, [Text])
generateCpp config ast = 
  let (decls, warnings) = case ast of
        Left pyAst -> generateFromPython config pyAst
        Right goAst -> generateFromGo config goAst
      includes = standardIncludes config
      namespaces = [cgcNamespace config]
  in (CppUnit includes namespaces decls, warnings)

-- | Generate C++ code with main function
generateCppMain :: CppGenConfig -> Either PythonAST GoAST -> (CppUnit, [Text])
generateCppMain config ast = 
  let (CppUnit includes namespaces decls, warnings) = generateCpp config ast
      mainFunc = generateMainFunction config ast
      allDecls = decls ++ [mainFunc]
  in (CppUnit includes namespaces allDecls, warnings)

-- | Generate standard includes
standardIncludes :: CppGenConfig -> [Text]
standardIncludes _config = 
  [ "<iostream>"
  , "<string>"
  , "<vector>"
  , "<memory>"
  , "<functional>"
  , "<cmath>"  -- for std::pow
  ]

-- | Generate main function
-- Improved logic: if a Python function named 'main' exists (renamed to 'main_func'),
-- generate a wrapper that calls it. Otherwise fall back to flattening top-level stmts.
generateMainFunction :: CppGenConfig -> Either PythonAST GoAST -> CppDecl
generateMainFunction _config ast = 
  case ast of
    Left (PythonAST (PythonModule _ _ _ stmts)) ->
      let hasUserMain = any (\(Fluxus.AST.Common.Located _ s) -> case s of
                                   PyFuncDef PythonFuncDef{ pyFuncName = Identifier n } -> n == "main"
                                   _ -> False) stmts
          body | hasUserMain = [CppExprStmt (CppCall (CppVar "main_func") []), CppReturn (Just (CppLiteral (CppIntLit 0)))]
               | otherwise   = generateMainBodyFromPython (PythonAST (PythonModule Nothing Nothing [] stmts)) ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
      in CppFunction "main" CppInt [] body
    Right _goAst -> CppFunction "main" CppInt []
        [ CppExprStmt (CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit "Go main not implemented")))
        , CppReturn (Just (CppLiteral (CppIntLit 0)))
        ]

-- | Generate main function body from Python AST
generateMainBodyFromPython :: PythonAST -> [CppStmt]
generateMainBodyFromPython (PythonAST (PythonModule _ _ _ stmts)) = 
  concatMap convertPythonStmtToStmt stmts

-- | Generate C++ from Python AST
generateFromPython :: CppGenConfig -> PythonAST -> ([CppDecl], [Text])
generateFromPython _config (PythonAST (PythonModule _ _ _ stmts)) = 
  let decls = concatMap convertPythonStmt stmts
      warnings = []
  in (decls, warnings)

-- | Generate C++ from Go AST  
generateFromGo :: CppGenConfig -> GoAST -> ([CppDecl], [Text])
generateFromGo _config _goAst = 
  let decls = [CppCommentDecl "Go code generation not yet implemented"]
      warnings = ["Go code generation is not yet implemented"]
  in (decls, warnings)

-- | Convert Python statement to C++ declarations
convertPythonStmt :: Fluxus.AST.Common.Located PythonStmt -> [CppDecl]
convertPythonStmt (Fluxus.AST.Common.Located _ stmt) = case stmt of
  PyFuncDef funcDef -> [convertPythonFunction funcDef]
  PyClassDef classDef -> [convertPythonClass classDef]
  PyAssign targets value -> convertPythonAssignment targets value
  PyExprStmt expr -> [CppCommentDecl $ "Expression statement: " <> T.pack (show expr)]
  PyReturn (Just expr) -> [CppCommentDecl $ "Return statement: " <> T.pack (show expr)]
  PyReturn Nothing -> [CppCommentDecl "Return statement"]
  PyIf condition thenStmts elseStmts -> [convertPythonIf condition thenStmts elseStmts]
  PyWhile condition body _elseClause -> [convertPythonWhile condition body]
  PyFor{..} -> [convertPythonFor pyForTarget pyForIter pyForBody]
  _ -> [CppCommentDecl $ "Unsupported statement: " <> T.pack (show stmt)]

-- | Convert Python function to C++ function
convertPythonFunction :: PythonFuncDef -> CppDecl
convertPythonFunction PythonFuncDef{..} = 
  let funcName = case pyFuncName of
        Identifier name -> 
          if name == "main" 
          then "main_func"  -- Avoid conflict with generated main
          else name
      params = map convertPythonParam pyFuncParams
      body = concatMap convertPythonStmtToStmt pyFuncBody
      -- Use auto return type for flexibility (simple heuristic)
      returnType = CppAuto
  in CppFunction funcName returnType params body

-- | Convert Python parameter to C++ parameter
convertPythonParam :: Fluxus.AST.Common.Located PythonParameter -> CppParam
convertPythonParam (Fluxus.AST.Common.Located _ param) = case param of
  ParamNormal (Identifier name) _ _ -> CppParam name CppAuto Nothing
  ParamVarArgs (Identifier name) _ -> CppParam name CppAuto Nothing
  ParamKwArgs (Identifier name) _ -> CppParam name CppAuto Nothing
  ParamKwOnly (Identifier name) _ _ -> CppParam name CppAuto Nothing
  ParamPosOnly (Identifier name) _ _ -> CppParam name CppAuto Nothing

-- | Convert Python class to C++ class
convertPythonClass :: PythonClassDef -> CppDecl
convertPythonClass PythonClassDef{..} = 
  let className = case pyClassName of
        Identifier name -> name
      members = concatMap convertPythonStmt pyClassBody
  in CppClass className [] members

-- | Convert Python assignment to C++ variable declarations
convertPythonAssignment :: [Fluxus.AST.Common.Located PythonPattern] -> Fluxus.AST.Common.Located PythonExpr -> [CppDecl]
convertPythonAssignment targets _value = 
  map (\target -> case target of
    Fluxus.AST.Common.Located _ (PatVar (Identifier name)) -> CppVariable name CppAuto Nothing
    _ -> CppCommentDecl "Complex assignment target"
  ) targets

-- | Convert Python if statement to C++ function (as a workaround)
convertPythonIf :: Fluxus.AST.Common.Located PythonExpr -> [Fluxus.AST.Common.Located PythonStmt] -> [Fluxus.AST.Common.Located PythonStmt] -> CppDecl
convertPythonIf _condition _thenStmts _elseStmts = 
  CppCommentDecl "If statement converted to comment"

-- | Convert Python while loop to C++ function (as a workaround)
convertPythonWhile :: Fluxus.AST.Common.Located PythonExpr -> [Fluxus.AST.Common.Located PythonStmt] -> CppDecl
convertPythonWhile _condition _body = 
  CppCommentDecl "While loop converted to comment"

-- | Convert Python for loop to C++ function (as a workaround)
convertPythonFor :: Fluxus.AST.Common.Located PythonPattern -> Fluxus.AST.Common.Located PythonExpr -> [Fluxus.AST.Common.Located PythonStmt] -> CppDecl
convertPythonFor _target _iter _body = 
  CppCommentDecl "For loop converted to comment"

-- | Convert Python statement to C++ statement
convertPythonStmtToStmt :: Fluxus.AST.Common.Located PythonStmt -> [CppStmt]
convertPythonStmtToStmt (Fluxus.AST.Common.Located _ stmt) = case stmt of
  PyReturn (Just expr) -> [CppReturn (Just (convertPythonExpr expr))]
  PyReturn Nothing -> [CppReturn Nothing]
  PyExprStmt expr -> [wrapPrint (convertPythonExpr expr)]
  PyAssign (t:_) value -> case t of
    Fluxus.AST.Common.Located _ (PatVar (Identifier name)) -> [CppDecl (CppVariable name CppAuto (Just (convertPythonExpr value)))]
    _ -> []
  PyAssign [] _ -> []
  _ -> []
  where
    -- Ensure expressions printing via std::cout append newline
    wrapPrint e@(CppBinary op (CppVar "std::cout") rhs)
      | op == "<<" = CppExprStmt (CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") rhs) (CppLiteral (CppStringLit "\n")))
      | otherwise = CppExprStmt e
    wrapPrint e = CppExprStmt e

-- | Convert Python expression to C++ expression
convertPythonExpr :: Fluxus.AST.Common.Located PythonExpr -> CppExpr
convertPythonExpr (Fluxus.AST.Common.Located _ expr) = case expr of
  PyLiteral lit -> CppLiteral (convertPythonLiteral lit)
  PyVar (Identifier name) -> CppVar name
  PyBinaryOp op left right -> 
    let cppOp = convertBinaryOp op
        leftExpr = convertPythonExpr left
        rightExpr = convertPythonExpr right
    in if cppOp == "pow"
         then CppCall (CppVar "std::pow") [leftExpr, rightExpr]
         else CppBinary cppOp leftExpr rightExpr
  PyCall func args -> 
    let funcExpr = convertPythonExpr func
        argExprs = map convertPythonArg args
    in case funcExpr of
      CppVar "print" -> 
        -- Convert print to std::cout
        case argExprs of
          [arg] -> CppBinary "<<" (CppVar "std::cout") arg
          _ -> CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit ""))
      -- Avoid generating invalid calls like 0(...)
      CppLiteral (CppIntLit 0) -> CppLiteral (CppIntLit 0)
      _ -> CppCall funcExpr argExprs
  -- Fallback literal for unsupported expressions
  _ -> CppLiteral (CppIntLit 0)

-- | Convert Python literal to C++ literal
convertPythonLiteral :: PythonLiteral -> CppLiteral
convertPythonLiteral = \case
  PyInt i -> CppIntLit i
  PyFloat f -> CppFloatLit f
  PyString s -> CppStringLit s
  PyBool b -> CppBoolLit b
  PyNone -> CppNullPtr
  PyComplex r _i -> CppFloatLit r  -- Simplified: just use real part
  PyBytes b -> CppStringLit b  -- Simplified: treat as string
  PyEllipsis -> CppIntLit 0  -- Placeholder

-- | Convert binary operator to C++ operator
convertBinaryOp :: Fluxus.AST.Common.BinaryOp -> Text
convertBinaryOp = \case
  Fluxus.AST.Common.OpAdd -> "+"
  Fluxus.AST.Common.OpSub -> "-"
  Fluxus.AST.Common.OpMul -> "*"
  Fluxus.AST.Common.OpDiv -> "/"
  Fluxus.AST.Common.OpMod -> "%"
  Fluxus.AST.Common.OpPow -> "pow"  -- Will need special handling
  Fluxus.AST.Common.OpShiftL -> "<<"
  Fluxus.AST.Common.OpShiftR -> ">>"
  Fluxus.AST.Common.OpBitOr -> "|"
  Fluxus.AST.Common.OpBitXor -> "^"
  Fluxus.AST.Common.OpBitAnd -> "&"
  Fluxus.AST.Common.OpFloorDiv -> "/"  -- Approximate
  Fluxus.AST.Common.OpAnd -> "&&"
  Fluxus.AST.Common.OpOr -> "||"
  Fluxus.AST.Common.OpXor -> "^"
  Fluxus.AST.Common.OpConcat -> "+"
  Fluxus.AST.Common.OpIn -> "in"  -- Will need special handling
  Fluxus.AST.Common.OpNotIn -> "not_in"  -- Will need special handling

-- | Convert Python argument to C++ expression
convertPythonArg :: Fluxus.AST.Common.Located PythonArgument -> CppExpr
convertPythonArg (Fluxus.AST.Common.Located _ arg) = case arg of
  ArgPositional expr -> convertPythonExpr expr
  ArgKeyword _ expr -> convertPythonExpr expr
  ArgStarred expr -> convertPythonExpr expr
  ArgKwStarred expr -> convertPythonExpr expr

-- | Map common AST types to C++ types
mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp = \case
  TInt _ -> CppInt
  TUInt _ -> CppUInt
  TFloat 32 -> CppFloat
  TFloat _ -> CppDouble
  TBool -> CppBool
  TString -> CppString
  TVoid -> CppVoid
  TList elemType -> CppVector (mapCommonTypeToCpp elemType)
  TDict keyType valueType -> CppUnorderedMap (mapCommonTypeToCpp keyType) (mapCommonTypeToCpp valueType)
  TOptional innerType -> CppOptional (mapCommonTypeToCpp innerType)
  TOwned innerType -> CppUniquePtr (mapCommonTypeToCpp innerType)
  TShared innerType -> CppSharedPtr (mapCommonTypeToCpp innerType)
  TTuple types -> CppTuple (map mapCommonTypeToCpp types)
  TFunction paramTypes retType -> CppFunctionType (map mapCommonTypeToCpp paramTypes) (mapCommonTypeToCpp retType)
  _ -> CppAuto  -- Fallback for complex/unsupported types