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
import qualified Fluxus.AST.Common as Common
import Fluxus.AST.Common (Type(..))
import Fluxus.AST.Python
import Fluxus.AST.Go as Go
-- removed GoC import
-- removed GoC import

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
  , "<sstream>"  -- for future string stream composition
  ]

-- | Generate main function
-- Improved logic: if a Python function named 'main' exists (renamed to 'main_func'),
-- generate a wrapper that calls it. Otherwise fall back to flattening top-level stmts.
generateMainFunction :: CppGenConfig -> Either PythonAST GoAST -> CppDecl
generateMainFunction config ast =
  case ast of
    Left (PythonAST (PythonModule _ _ _ stmts)) ->
      let hasUserMain = any (\(Common.Located _ s) -> case s of
                                   PyFuncDef PythonFuncDef{ pyFuncName = Identifier n } -> n == "main"
                                   _ -> False) stmts
          namespace = cgcNamespace config
          body | hasUserMain = [CppExprStmt (CppCall (CppVar (namespace <> "::main_func")) []), CppReturn (Just (CppLiteral (CppIntLit 0)))]
               | otherwise   = generateMainBodyFromPython (PythonAST (PythonModule Nothing Nothing [] stmts)) ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
      in CppFunction "main" CppInt [] body
    Right (GoAST (GoPackage _ files)) ->
      let hasMain = any (\(GoFile _ _ _ decls) -> any isMain decls) files
          isMain (Go.Located _ (GoFuncDecl GoFunction{ goFuncName = Just (Identifier n) })) = n == "main"
          isMain _ = False
          namespace = cgcNamespace config
          body | hasMain = [CppExprStmt (CppCall (CppVar (namespace <> "::main_impl")) []), CppReturn (Just (CppLiteral (CppIntLit 0)))]
               | otherwise = [ CppExprStmt (CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit "Go main not implemented")))
                             , CppReturn (Just (CppLiteral (CppIntLit 0))) ]
      in CppFunction "main" CppInt [] body
    
-- | Generate main function body from Python AST
generateMainBodyFromPython :: PythonAST -> [CppStmt]
generateMainBodyFromPython (PythonAST (PythonModule _ _ _ stmts)) = 
  convertPythonStmtsToStmts stmts []

-- | Convert Python statements to C++ statements with variable tracking
convertPythonStmtsToStmts :: [Common.Located PythonStmt] -> [Text] -> [CppStmt]
convertPythonStmtsToStmts [] _ = []
convertPythonStmtsToStmts (stmt:stmts) declaredVars = 
  let (newStmts, newDeclaredVars) = convertPythonStmtToStmtWithTracking stmt declaredVars
  in newStmts ++ convertPythonStmtsToStmts stmts newDeclaredVars

-- | Convert Python statement to C++ statement with variable tracking
convertPythonStmtToStmtWithTracking :: Common.Located PythonStmt -> [Text] -> ([CppStmt], [Text])
convertPythonStmtToStmtWithTracking (Common.Located _ stmt) declaredVars = case stmt of
  PyReturn (Just expr) -> ([CppReturn (Just (convertPythonExpr expr))], declaredVars)
  PyReturn Nothing -> ([CppReturn Nothing], declaredVars)
  PyExprStmt expr -> ([wrapPrint (convertPythonExpr expr)], declaredVars)
  PyAssign (t:_) value -> case t of
    Common.Located _ (PatVar (Identifier name)) -> 
      if name `elem` declaredVars
      then -- Variable already declared, generate assignment
           ([CppExprStmt (CppBinary "=" (CppVar name) (convertPythonExpr value))], declaredVars)
      else -- First declaration, generate variable declaration
           ([CppDecl (CppVariable name CppAuto (Just (convertPythonExpr value)))], name : declaredVars)
    _ -> ([], declaredVars)
  PyAssign [] _ -> ([], declaredVars)
  PyWhile condition body _elseClause -> 
    let bodyStmts = convertPythonStmtsToStmts body declaredVars
    in ([CppWhile (convertPythonExpr condition) bodyStmts], declaredVars)
  _ -> ([], declaredVars)

-- | Generate C++ from Python AST
generateFromPython :: CppGenConfig -> PythonAST -> ([CppDecl], [Text])
generateFromPython _config (PythonAST (PythonModule _ _ _ stmts)) = 
  let decls = concatMap convertPythonStmt stmts
      warnings = []
  in (decls, warnings)

-- | Generate C++ from Go AST  
-- Minimal mapping: struct -> class with member vars, functions -> stub returning 0.
generateFromGo :: CppGenConfig -> GoAST -> ([CppDecl], [Text])
generateFromGo _config goAst = 
  let GoAST (GoPackage _ files) = goAst
      fileDeclPairs = concatMap extractFileDecls files
      decls = concatMap fst fileDeclPairs
      warnings = concatMap snd fileDeclPairs
  in (decls, warnings)
  where
    extractFileDecls :: GoFile -> [([CppDecl],[Text])]
    extractFileDecls (GoFile _ _ _ decls) = map handleDecl decls

    handleDecl :: Go.Located GoDecl -> ([CppDecl],[Text])
    handleDecl (Go.Located _ d) = case d of
      GoTypeDeclStmt td -> case goTypeDeclType td of
        Go.Located _ (GoStructType fields) ->
          let name = identText (goTypeDeclName td)
              members = concatMap fieldDecls fields
          in ([CppClass name [] members], [])
        _ -> ([CppCommentDecl "Unsupported Go type (only struct->class mapping)"],["unsupported go type"])
      GoFuncDecl fn -> ([convertFunc fn], [])
      _ -> ([CppCommentDecl "Unsupported Go declaration"],["unsupported go decl skipped"])

    -- Simple Go type mapping for primitive identifiers
    mapGoType :: GoType -> CppType
    mapGoType (GoBasicType (Identifier "int")) = CppInt
    mapGoType (GoBasicType (Identifier "string")) = CppString
    mapGoType _ = CppAuto


    fieldDecls :: GoField -> [CppDecl]
    fieldDecls (GoField names (Go.Located _ gty) _) = [ CppVariable (identText n) (mapGoType gty) Nothing | n <- names ]

    convertFunc :: GoFunction -> CppDecl
    convertFunc GoFunction{ goFuncName = Just name } =
      let nm = identText name
          outName = if nm == "main" then "main_impl" else nm
      in CppFunction outName CppAuto [] [CppReturn (Just (CppLiteral (CppIntLit 0)))]
    convertFunc _ = CppCommentDecl "Anonymous Go function not emitted"

    identText :: Identifier -> Text
    identText (Identifier t) = t
-- | Convert Python statement to C++ declarations
convertPythonStmt :: Common.Located PythonStmt -> [CppDecl]
convertPythonStmt (Common.Located _ stmt) = case stmt of
  PyFuncDef funcDef -> [convertPythonFunction funcDef]
  PyClassDef classDef -> [convertPythonClass classDef]
  -- Skip simple assignments - they will be handled in main function
  PyAssign _ _ -> []
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
      bodyRaw = concatMap convertPythonStmtToStmt pyFuncBody
      body = if null (filter hasReturn bodyRaw)
               then bodyRaw ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
               else bodyRaw
      -- Use auto return type for flexibility (simple heuristic)
      returnType = CppAuto
  in CppFunction funcName returnType params body
  where
    hasReturn (CppReturn _) = True
    hasReturn _ = False

-- | Convert Python parameter to C++ parameter
convertPythonParam :: Common.Located PythonParameter -> CppParam
convertPythonParam (Common.Located _ param) = case param of
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


-- | Convert Python if statement to C++ function (as a workaround)
convertPythonIf :: Common.Located PythonExpr -> [Common.Located PythonStmt] -> [Common.Located PythonStmt] -> CppDecl
convertPythonIf _condition _thenStmts _elseStmts = 
  CppCommentDecl "If statement converted to comment"

-- | Convert Python while loop to C++ function (as a workaround)
convertPythonWhile :: Common.Located PythonExpr -> [Common.Located PythonStmt] -> CppDecl
convertPythonWhile _condition _body = 
  CppCommentDecl "While loop converted to comment"

-- | Convert Python for loop to C++ function (as a workaround)
convertPythonFor :: Common.Located PythonPattern -> Common.Located PythonExpr -> [Common.Located PythonStmt] -> CppDecl
convertPythonFor _target _iter _body = 
  CppCommentDecl "For loop converted to comment"

-- | Convert Python statement to C++ statement
convertPythonStmtToStmt :: Common.Located PythonStmt -> [CppStmt]
convertPythonStmtToStmt (Common.Located _ stmt) = case stmt of
  PyReturn (Just expr) -> [CppReturn (Just (convertPythonExpr expr))]
  PyReturn Nothing -> [CppReturn Nothing]
  PyExprStmt expr -> [wrapPrint (convertPythonExpr expr)]
  PyAssign (t:_) value -> case t of
    Common.Located _ (PatVar (Identifier name)) -> 
      -- Generate variable declaration with initialization for main function
      [CppDecl (CppVariable name CppAuto (Just (convertPythonExpr value)))]
    _ -> []
  PyAssign [] _ -> []
  PyWhile condition body _elseClause -> [CppWhile (convertPythonExpr condition) (concatMap convertPythonStmtToStmt body)]
  _ -> []

-- | Ensure expressions printing via std::cout append newline
wrapPrint :: CppExpr -> CppStmt
wrapPrint e@(CppBinary op (CppVar "std::cout") rhs)
  | op == "<<" = CppExprStmt (CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") rhs) (CppLiteral (CppStringLit "\n")))
  | otherwise = CppExprStmt e
wrapPrint e = CppExprStmt e

-- | Convert Python expression to C++ expression
convertPythonExpr :: Common.Located PythonExpr -> CppExpr
convertPythonExpr (Common.Located _ expr) = case expr of
  PyLiteral lit -> CppLiteral (convertPythonLiteral lit)
  PyVar (Identifier name) -> CppVar name
  PyBinaryOp op left right -> 
    let cppOp = convertBinaryOp op
        leftExpr = convertPythonExpr left
        rightExpr = convertPythonExpr right
        isStringLiteral e = case e of
          CppLiteral (CppStringLit _) -> True
          _ -> False
        ensureString e = case e of
          CppLiteral (CppStringLit _) -> CppCall (CppVar "std::string") [e]
          _ -> e
        toStringIfNeeded e = case e of
          CppLiteral (CppStringLit _) -> e
          CppCall (CppVar "std::to_string") _ -> e
          _ -> CppCall (CppVar "std::to_string") [e]
    in if cppOp == "pow"
         then CppCall (CppVar "std::pow") [leftExpr, rightExpr]
         else if cppOp == "+" && (isStringLiteral leftExpr || isStringLiteral rightExpr)
                then let l' = ensureString leftExpr
                         r' = toStringIfNeeded rightExpr
                     in CppBinary "+" l' r'
                else CppBinary cppOp leftExpr rightExpr
  PyComparison [op] [left, right] ->
    -- Handle simple binary comparisons
    let cppOp = convertComparisonOp op
        leftExpr = convertPythonExpr left
        rightExpr = convertPythonExpr right
    in CppBinary cppOp leftExpr rightExpr
  PyComparison _ _ ->
    -- For chained comparisons, fall back to 0 for now
    CppLiteral (CppIntLit 0)
  PyCall func args -> 
    let funcExpr = convertPythonExpr func
        argExprs = map convertPythonArg args
    in case funcExpr of
      CppVar "print" -> 
        -- Convert print to std::cout; if the argument is a + chain of strings/numbers, just stream it
        case argExprs of
          [arg] -> case arg of
                     CppBinary "+" _ _ -> CppBinary "<<" (CppVar "std::cout") arg
                     _ -> CppBinary "<<" (CppVar "std::cout") arg
          _ -> CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit ""))
      CppVar "str" -> 
        -- Map Python str() to std::to_string (best-effort for ints/floats)
        case argExprs of
          [arg] -> CppCall (CppVar "std::to_string") [arg]
          _ -> CppCall (CppVar "std::string") []
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
convertBinaryOp :: Common.BinaryOp -> Text
convertBinaryOp = \case
  Common.OpAdd -> "+"
  Common.OpSub -> "-"
  Common.OpMul -> "*"
  Common.OpDiv -> "/"
  Common.OpMod -> "%"
  Common.OpPow -> "pow"  -- Will need special handling
  Common.OpShiftL -> "<<"
  Common.OpShiftR -> ">>"
  Common.OpBitOr -> "|"
  Common.OpBitXor -> "^"
  Common.OpBitAnd -> "&"
  Common.OpFloorDiv -> "/"  -- Approximate
  Common.OpAnd -> "&&"
  Common.OpOr -> "||"
  Common.OpXor -> "^"
  Common.OpConcat -> "+"
  Common.OpIn -> "in"  -- Will need special handling
  Common.OpNotIn -> "not_in"  -- Will need special handling

-- | Convert comparison operator to C++ operator
convertComparisonOp :: Common.ComparisonOp -> Text
convertComparisonOp = \case
  Common.OpEq -> "=="
  Common.OpNe -> "!="
  Common.OpLt -> "<"
  Common.OpLe -> "<="
  Common.OpGt -> ">"
  Common.OpGe -> ">="
  Common.OpIs -> "=="  -- Approximate with ==
  Common.OpIsNot -> "!="  -- Approximate with !=

-- | Convert Python argument to C++ expression
convertPythonArg :: Common.Located PythonArgument -> CppExpr
convertPythonArg (Common.Located _ arg) = case arg of
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
