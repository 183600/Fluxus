{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | C++ code generation module
module Fluxus.CodeGen.CPP
  ( -- * Code generation types
    CppCodeGen
  , CppGenState(..)
  , CppGenConfig(..)
    -- * Main code generation functions
  , generateCpp
  , generateCppFromPython
  , generateCppFromGo
    -- * C++ AST types
  , CppUnit(..)
  , CppDecl(..)
  , CppStmt(..)
  , CppExpr(..)
  , CppType(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppCase(..)
    -- * Code generation utilities
  , runCppCodeGen
    -- * Type mapping
  , mapPythonTypeToCpp
  , mapGoTypeToCpp
  , mapCommonTypeToCpp
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (when, unless, foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, partition, nub)
import Data.Maybe (catMaybes)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Fluxus.Utils.Pretty

-- | C++ code generation configuration
data CppGenConfig = CppGenConfig
  { cgcOptimizationLevel :: !Int                        -- 0-3, like GCC/Clang
  , cgcEnableInterop     :: !Bool                       -- Enable runtime interop
  , cgcTargetCppStd      :: !Text                       -- "c++20", "c++23", etc.
  , cgcUseSmartPointers  :: !Bool                       -- Use smart pointers aggressively
  , cgcEnableParallel    :: !Bool                       -- Enable parallel execution
  , cgcEnableCoroutines  :: !Bool                       -- Enable C++20 coroutines
  , cgcNamespace         :: !Text                       -- Target namespace
  , cgcHeaderGuard       :: !Text                       -- Header guard prefix
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Code generation state
data CppGenState = CppGenState
  { cgsIncludes     :: ![Text]                          -- Required includes
  , cgsDeclarations :: ![CppDecl]                       -- Generated declarations
  , cgsNamespaces   :: ![Text]                          -- Current namespace stack
  , cgsTempVarCount :: !Int                             -- Temporary variable counter
  , cgsSymbolTable  :: !(HashMap Text CppType)         -- Symbol to type mapping
  , cgsConfig       :: !CppGenConfig                    -- Configuration
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Code generation monad
type CppCodeGen = StateT CppGenState (Writer [Text])

-- | C++ compilation unit
data CppUnit = CppUnit
  { cppIncludes     :: ![Text]
  , cppNamespaces   :: ![Text]
  , cppDeclarations :: ![CppDecl]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | C++ declarations
data CppDecl
  = CppClass !Text ![Text] ![CppDecl]                   -- name, base classes, members
  | CppStruct !Text ![CppDecl]                          -- name, members
  | CppFunction !Text !CppType ![CppParam] ![CppStmt]   -- name, return type, params, body
  | CppMethod !Text !CppType ![CppParam] ![CppStmt] !Bool -- name, return type, params, body, isVirtual
  | CppConstructor !Text ![CppParam] ![CppStmt]         -- class name, params, body
  | CppDestructor !Text ![CppStmt] !Bool                -- class name, body, isVirtual
  | CppVariable !Text !CppType !(Maybe CppExpr)        -- name, type, initializer
  | CppTypedef !Text !CppType                           -- alias, type
  | CppUsing !Text !CppType                             -- alias, type
  | CppTemplate ![Text] !CppDecl                        -- template params, declaration
  | CppNamespace !Text ![CppDecl]                       -- name, declarations
  | CppExternC ![CppDecl]                               -- C linkage declarations
  | CppCommentDecl !Text                                -- comment at declaration level
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | C++ statements
data CppStmt
  = CppExprStmt !CppExpr
  | CppReturn !(Maybe CppExpr)
  | CppIf !CppExpr ![CppStmt] ![CppStmt]
  | CppWhile !CppExpr ![CppStmt]
  | CppFor !(Maybe CppStmt) !(Maybe CppExpr) !(Maybe CppExpr) ![CppStmt]
  | CppForRange !Text !CppExpr ![CppStmt]               -- variable, range, body
  | CppSwitch !CppExpr ![CppCase]
  | CppTry ![CppStmt] ![CppCatch] ![CppStmt]
  | CppThrow !(Maybe CppExpr)
  | CppBreak
  | CppContinue
  | CppBlock ![CppStmt]
  | CppDecl !CppDecl
  | CppComment !Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | C++ expressions
data CppExpr
  = CppVar !Text
  | CppLiteral !CppLiteral
  | CppBinary !Text !CppExpr !CppExpr                   -- operator, left, right
  | CppUnary !Text !CppExpr                             -- operator, operand
  | CppCall !CppExpr ![CppExpr]                         -- function, arguments
  | CppMember !CppExpr !Text                            -- object, member
  | CppPointerMember !CppExpr !Text                     -- pointer, member
  | CppIndex !CppExpr !CppExpr                          -- array, index
  | CppCast !CppType !CppExpr                           -- type, expression
  | CppSizeOf !CppType
  | CppNew !CppType ![CppExpr]                          -- type, constructor args
  | CppDelete !CppExpr
  | CppThis
  | CppLambda ![CppParam] ![CppStmt]                    -- parameters, body
  | CppMove !CppExpr                                    -- std::move
  | CppForward !CppExpr                                 -- std::forward
  | CppMakeUnique !CppType ![CppExpr]                   -- std::make_unique
  | CppMakeShared !CppType ![CppExpr]                   -- std::make_shared
  | CppBracedInit !CppType ![CppExpr]                   -- Type{args...}
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | C++ types
data CppType
  = CppVoid
  | CppBool
  | CppChar | CppUChar
  | CppShort | CppUShort
  | CppInt | CppUInt
  | CppLong | CppULong
  | CppLongLong | CppULongLong
  | CppFloat | CppDouble | CppLongDouble
  | CppAuto                                             -- C++11 auto
  | CppString                                           -- std::string
  | CppVector !CppType                                  -- std::vector<T>
  | CppArray !CppType !Int                              -- T[N]
  | CppPointer !CppType                                 -- T*
  | CppReference !CppType                               -- T&
  | CppRvalueRef !CppType                               -- T&&
  | CppConst !CppType                                   -- const T
  | CppVolatile !CppType                                -- volatile T
  | CppSizeT                                            -- std::size_t
  | CppFunctionType ![CppType] !CppType                     -- function type
  | CppClassType !Text ![CppType]                           -- class name, template args
  | CppTemplateType !Text ![CppType]                        -- template name, args
  | CppUniquePtr !CppType                               -- std::unique_ptr<T>
  | CppSharedPtr !CppType                               -- std::shared_ptr<T>
  | CppOptional !CppType                                -- std::optional<T>
  | CppVariant ![CppType]                               -- std::variant<Ts...>
  | CppPair !CppType !CppType                           -- std::pair<T, U>
  | CppTuple ![CppType]                                 -- std::tuple<Ts...>
  | CppMap !CppType !CppType                            -- std::map<K, V>
  | CppUnorderedMap !CppType !CppType                   -- std::unordered_map<K, V>
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | C++ literals
data CppLiteral
  = CppIntLit !Integer
  | CppFloatLit !Double
  | CppStringLit !Text
  | CppCharLit !Char
  | CppBoolLit !Bool
  | CppNullPtr
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Function parameters
data CppParam = CppParam !Text !CppType !(Maybe CppExpr)  -- name, type, default
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Switch cases
data CppCase = CppCase !CppExpr ![CppStmt] | CppDefault ![CppStmt]
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Catch blocks
data CppCatch = CppCatch !CppType !Text ![CppStmt]      -- exception type, variable, body
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Default configuration
defaultCppGenConfig :: CppGenConfig
defaultCppGenConfig = CppGenConfig
  { cgcOptimizationLevel = 2
  , cgcEnableInterop = True
  , cgcTargetCppStd = "c++20"
  , cgcUseSmartPointers = True
  , cgcEnableParallel = True
  , cgcEnableCoroutines = True
  , cgcNamespace = "hyperstatic"
  , cgcHeaderGuard = "HYPERSTATIC_GENERATED"
  }

-- | Initial state
initialCppGenState :: CppGenConfig -> CppGenState
initialCppGenState config = CppGenState
  { cgsIncludes = []  -- Start with no includes, add as needed
  , cgsDeclarations = []
  , cgsNamespaces = []
  , cgsTempVarCount = 0
  , cgsSymbolTable = HM.empty
  , cgsConfig = config
  }

-- | Run code generation
runCppCodeGen :: CppGenConfig -> CppCodeGen a -> (a, CppGenState)
runCppCodeGen config action = 
  let ((result, finalState), output) = runWriter (runStateT action (initialCppGenState config))
  in (result, finalState)

-- | Main entry point for C++ code generation
generateCpp :: CppGenConfig -> Either PythonAST GoAST -> CppUnit
generateCpp config ast = 
  let (unit, _) = runCppCodeGen config $ case ast of
        Left pyAst -> generateCppFromPython pyAst
        Right goAst -> generateCppFromGo goAst
  in unit

-- | Generate C++ from Python AST
generateCppFromPython :: PythonAST -> CppCodeGen CppUnit
generateCppFromPython (PythonAST pyModule) = do
  -- Add basic C++ includes
  addInclude "<iostream>"
  addInclude "<string>"
  
  -- Generate module namespace
  let moduleName = maybe "main" (\(ModuleName n) -> n) (pyModuleName pyModule)
  
  -- Process module body - separate function definitions from module-level statements
  let (funcDefs, moduleStmts) = partitionStmts (pyModuleBody pyModule)
  
  -- Process function definitions first
  mapM_ generatePythonStmt funcDefs
  
  -- Process module-level statements
  moduleStmtsCpp <- mapM generatePythonStmt moduleStmts
  -- Filter out comment statements that don't generate actual code
  let isActualStatement (CppComment _) = False
      isActualStatement _ = True
      actualStmts = filter isActualStatement moduleStmtsCpp
  
  -- Ensure we have a main function for standalone execution
  hasMain <- gets (any isMainFunction . cgsDeclarations)
  unless hasMain $ do
    -- If we have module-level statements, wrap them in main function
    let mainBody = if null actualStmts 
                   then [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                   else actualStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
    addDeclaration $ CppFunction "main" CppInt [] mainBody
  
  -- Build final unit
  includes <- gets cgsIncludes
  decls <- gets cgsDeclarations
  
  return $ CppUnit includes [] (reverse decls)  -- Reverse to maintain declaration order
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False
    
    partitionStmts stmts = 
      let (funcs, others) = partition isFuncDef stmts
      in (funcs, others)
    
    isFuncDef (Located _ (PyFuncDef _)) = True
    isFuncDef _ = False

-- | Generate C++ from Go AST
generateCppFromGo :: GoAST -> CppCodeGen CppUnit
generateCppFromGo (GoAST goPackage) = do
  -- Add basic C++ includes
  addInclude "<iostream>"
  addInclude "<string>"
  addInclude "<thread>"
  addInclude "<mutex>"
  addInclude "<condition_variable>"
  addInclude "<queue>"
  addInclude "<vector>"
  addInclude "<functional>"
  addInclude "<atomic>"
  addInclude "<chrono>"
  
  -- Generate package namespace
  let packageName = (\(Identifier n) -> n) (goPackageName goPackage)
  
  -- Debug: Add comment showing package info
  addComment $ "Generating C++ for Go package: " <> packageName
  
  -- Generate channel class implementation
  generateChannelClass
  
  -- Process all files in package
  let files = goPackageFiles goPackage
  addComment $ "Found " <> T.pack (show (length files)) <> " files in package"
  when (null files) $ do
    addComment "No files found in package"
  
  mapM_ generateGoFile files
  
  -- Ensure we have a main function if processing main package
  when (packageName == "main") $ do
    hasMain <- gets (any isMainFunction . cgsDeclarations)
    unless hasMain $ do
      addComment "Generating fallback main function - Go parser not working properly"
      addInclude "<iostream>"
      addDeclaration $ CppFunction "main" CppInt [] [CppReturn (Just (CppLiteral (CppIntLit 0)))]
  
  -- Build final unit
  includes <- gets cgsIncludes
  namespaces <- gets cgsNamespaces
  decls <- gets cgsDeclarations
  
  return $ CppUnit includes namespaces (reverse decls)  -- Reverse to maintain declaration order
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False

-- | Generate C++ from Python statements
generatePythonStmt :: Located PythonStmt -> CppCodeGen CppStmt
generatePythonStmt (Located _ stmt) = case stmt of
  PyFuncDef funcDef -> do
    generatePythonFunction funcDef
    return $ CppComment "Function definition processed"
  PyClassDef classDef -> do
    generatePythonClass classDef
    return $ CppComment "Class definition processed"
  PyExprStmt expr -> do
    cppExpr <- generatePythonExpr expr
    return $ CppExprStmt cppExpr
  PyAssign patterns expr@(Located _ exprVal) -> do
    -- For now, handle simple single-target assignment
    case patterns of
      [Located _ (PatVar (Identifier varName))] -> do
        symtab <- gets cgsSymbolTable
        case exprVal of
          -- Special handling for list literals: materialize a std::vector with the appropriate element type
          PyList elems -> do
            (vectorType, vectorExpr) <- generatePythonListLiteral elems
            let updateSymbolTable = modify $ \s -> s { cgsSymbolTable = HM.insert varName vectorType (cgsSymbolTable s) }
            if HM.member varName symtab
              then do
                updateSymbolTable
                return $ CppExprStmt (CppBinary "=" (CppVar varName) vectorExpr)
              else do
                updateSymbolTable
                return $ CppDecl $ CppVariable varName vectorType (Just vectorExpr)
          -- Default: evaluate RHS and either declare or assign
          _ -> do
            cppExpr <- generatePythonExpr expr
            if HM.member varName symtab
              then return $ CppExprStmt (CppBinary "=" (CppVar varName) cppExpr)
              else do
                let varType = CppAuto
                -- Update symbol table for new variable
                modify $ \s -> s { cgsSymbolTable = HM.insert varName varType (cgsSymbolTable s) }
                return $ CppDecl $ CppVariable varName varType (Just cppExpr)
      _ -> do
        -- Multiple assignment - not fully implemented
        return $ CppComment "Multiple assignment not implemented"
  PyReturn mexpr -> do
    mcppExpr <- mapM generatePythonExpr mexpr
    return $ CppReturn mcppExpr
  PyIf condition thenStmts elseStmts -> do
    cppCond <- generatePythonExpr condition
    cppThen <- mapM generatePythonStmt thenStmts
    cppElse <- mapM generatePythonStmt elseStmts
    return $ CppIf cppCond cppThen cppElse
  PyWhile condition bodyStmts _ -> do
    cppCond <- generatePythonExpr condition
    cppBody <- mapM generatePythonStmt bodyStmts
    return $ CppWhile cppCond cppBody
  PyFor (Located _ (PatVar (Identifier varName))) iterExpr bodyStmts _ -> do
    cppIter <- generatePythonExpr iterExpr
    cppBody <- mapM generatePythonStmt bodyStmts
    -- Handle range() function calls with 1-3 arguments
    case cppIter of
      -- range(end)
      CppCall (CppVar "range") [end] -> do
        let initDecl = CppDecl (CppVariable varName CppAuto (Just (CppLiteral (CppIntLit 0))))
        let condition = CppBinary "<" (CppVar varName) end
        let increment = CppUnary "++" (CppVar varName)
        return $ CppFor (Just initDecl) (Just condition) (Just increment) cppBody
      -- range(start, end)
      CppCall (CppVar "range") [start, end] -> do
        let initDecl = CppDecl (CppVariable varName CppAuto (Just start))
        let condition = CppBinary "<" (CppVar varName) end
        let increment = CppUnary "++" (CppVar varName)
        return $ CppFor (Just initDecl) (Just condition) (Just increment) cppBody
      -- range(start, end, step) - simplified handling
      CppCall (CppVar "range") [start, end, step] -> do
        let initDecl = CppDecl (CppVariable varName CppAuto (Just start))
        let condition = CppBinary "<" (CppVar varName) end
        let increment = CppBinary "+=" (CppVar varName) step
        return $ CppFor (Just initDecl) (Just condition) (Just increment) cppBody
      _ -> return $ CppComment "For loop not fully implemented"
  _ -> return $ CppComment $ "TODO: Implement Python statement: " <> T.pack (show stmt)

-- | Generate C++ from Python expressions
-- | Generate C++ expression from Python argument
generatePythonArgument :: Located PythonArgument -> CppCodeGen CppExpr
generatePythonArgument (Located _ arg) = case arg of
  ArgPositional expr -> generatePythonExpr expr
  ArgKeyword _ expr -> generatePythonExpr expr  -- Simplified: ignore keyword
  ArgStarred expr -> generatePythonExpr expr  -- Simplified
  ArgKwStarred expr -> generatePythonExpr expr  -- Simplified

generatePythonExpr :: Located PythonExpr -> CppCodeGen CppExpr
generatePythonExpr (Located _ expr) = case expr of
  PyLiteral lit -> return $ CppLiteral $ mapPythonLiteral lit
  PyConst (QualifiedName _ (Identifier name)) -> case name of
    "True"  -> return $ CppLiteral (CppBoolLit True)
    "False" -> return $ CppLiteral (CppBoolLit False)
    "None"  -> return $ CppLiteral CppNullPtr
    _        -> return $ CppVar name
  PyVar (Identifier name) -> return $ CppVar name
  PyBinaryOp op left right -> do
    cppLeft <- generatePythonExpr left
    cppRight <- generatePythonExpr right
    let cppOp = mapPythonBinaryOp op
    return $ CppBinary cppOp cppLeft cppRight
  PyUnaryOp op inner -> do
    cppInner <- generatePythonExpr inner
    let uop = case op of
          OpNot      -> "!"
          OpNegate   -> "-"
          OpBitNot   -> "~"
          OpPositive -> "+"
    return $ CppUnary uop cppInner
  PyBoolOp op exprs -> do
    cppExprs <- mapM generatePythonExpr exprs
    let cppOp = case op of
          OpAnd -> "&&"
          OpOr  -> "||"
          _     -> "&&"
    return $ foldl1 (\acc e -> CppBinary cppOp acc e) cppExprs
  PyComparison ops exprs -> do
    -- Handle chained comparisons: a < b < c becomes (a < b) && (b < c)
    case (ops, exprs) of
      ([op], [left, right]) -> do
        -- Simple comparison
        cppLeft <- generatePythonExpr left
        cppRight <- generatePythonExpr right
        let cppOp = mapComparisonOp op
        return $ CppBinary cppOp cppLeft cppRight
      (ops', exprs') | length ops' + 1 == length exprs' -> do
        -- Chained comparison
        cppExprs <- mapM generatePythonExpr exprs'
        let pairs = zip3 (init cppExprs) (map mapComparisonOp ops') (tail cppExprs)
        let comparisons = map (\(l, op, r) -> CppBinary op l r) pairs
        -- Chain with && operators
        return $ foldl1 (\acc comp -> CppBinary "&&" acc comp) comparisons
      _ -> do
        addComment $ "Invalid comparison expression"
        return $ CppLiteral $ CppBoolLit False
  PySubscript obj sliceExpr -> do
    cppObj <- generatePythonExpr obj
    case sliceExpr of
      Located _ (SliceIndex idx) -> do
        cppIdx <- generatePythonExpr idx
        return $ CppIndex cppObj cppIdx
      _ -> do
        addComment "Unsupported slice expression"
        return $ CppLiteral (CppIntLit 0)
  PyCall func args -> do
    cppFunc <- generatePythonExpr func
    cppArgs <- mapM generatePythonArgument args
    -- Handle special functions
    case func of
      Located _ (PyVar (Identifier "print")) -> do
        -- Convert print to std::cout
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [arg] -> return $ CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") arg) (CppVar "std::endl")
          args -> do
            -- Chain multiple << operators for multiple arguments
            let chainedOutput = foldl (\acc arg -> CppBinary "<<" (CppBinary "<<" acc arg) (CppLiteral (CppStringLit " "))) (CppVar "std::cout") (init args)
            return $ CppBinary "<<" (CppBinary "<<" chainedOutput (last args)) (CppVar "std::endl")
      Located _ (PyVar (Identifier "len")) -> do
        case cppArgs of
          [arg0] -> return $ CppCall (CppMember arg0 "size") []
          _      -> return $ CppLiteral (CppIntLit 0)
      Located _ (PyVar (Identifier "range")) -> do
        -- Handle range() function calls
        case cppArgs of
          [CppLiteral (CppIntLit n)] -> return $ CppCall (CppVar "range") [CppLiteral (CppIntLit n)]
          _ -> return $ CppCall (CppVar "range") cppArgs
      _ -> return $ CppCall cppFunc cppArgs
  PyList exprs -> do
    (_, vectorExpr) <- generatePythonListLiteral exprs
    return vectorExpr
  _ -> do
    addComment $ "TODO: Implement Python expression: " <> T.pack (show expr)
    return $ CppLiteral $ CppIntLit 0

-- | Materialize a Python list literal into a C++ vector expression
generatePythonListLiteral :: [Located PythonExpr] -> CppCodeGen (CppType, CppExpr)
generatePythonListLiteral elems = do
  addInclude "<vector>"
  cppElems <- mapM generatePythonExpr elems
  let inferredTypes = map inferPythonExprCppTypeLocated elems
      knownTypes = catMaybes inferredTypes
      hasUnknown = length knownTypes /= length elems
      combinedType = case knownTypes of
        [] -> Nothing
        (t:ts) -> foldM unifyElementType t ts
      uniqueTypes = nub knownTypes
      fallbackAny = do
        addInclude "<any>"
        return $ CppClassType "std::any" []
  elementType <-
    if null elems
      then return CppLongLong
      else case combinedType of
        Just t | not hasUnknown -> return t
        Just _ -> fallbackAny
        Nothing | not hasUnknown && not (null uniqueTypes) -> do
          addInclude "<variant>"
          return $ CppVariant uniqueTypes
        _ -> fallbackAny
  let vectorType = CppVector elementType
  return (vectorType, CppBracedInit vectorType cppElems)

inferPythonExprCppTypeLocated :: Located PythonExpr -> Maybe CppType
inferPythonExprCppTypeLocated (Located _ e) = inferPythonExprCppType e

inferPythonExprCppType :: PythonExpr -> Maybe CppType
inferPythonExprCppType = \case
  PyLiteral lit -> inferPythonLiteralType lit
  PyConst (QualifiedName _ (Identifier name)) -> case name of
    "True" -> Just CppBool
    "False" -> Just CppBool
    _ -> Nothing
  PyUnaryOp op inner -> case op of
    OpNot -> Just CppBool
    _ -> inferPythonExprCppTypeLocated inner
  PyBinaryOp _ left right -> do
    t1 <- inferPythonExprCppTypeLocated left
    t2 <- inferPythonExprCppTypeLocated right
    unifyElementType t1 t2
  PyBoolOp _ _ -> Just CppBool
  PyComparison _ _ -> Just CppBool
  _ -> Nothing

inferPythonLiteralType :: PythonLiteral -> Maybe CppType
inferPythonLiteralType = \case
  PyInt _ -> Just CppLongLong
  PyFloat _ -> Just CppDouble
  PyBool _ -> Just CppBool
  PyString _ -> Just CppString
  PyFString _ _ -> Just CppString
  PyBytes _ -> Just CppString
  _ -> Nothing

unifyElementType :: CppType -> CppType -> Maybe CppType
unifyElementType t1 t2
  | t1 == t2 = Just t1
  | isNumericType t1 && isNumericType t2 = Just (promoteNumericType t1 t2)
  | t1 == CppBool && t2 == CppBool = Just CppBool
  | t1 == CppBool && isNumericType t2 = Just (promoteNumericType CppLongLong t2)
  | isNumericType t1 && t2 == CppBool = Just (promoteNumericType t1 CppLongLong)
  | otherwise = Nothing

isNumericType :: CppType -> Bool
isNumericType = \case
  CppShort -> True
  CppUShort -> True
  CppInt -> True
  CppUInt -> True
  CppLong -> True
  CppULong -> True
  CppLongLong -> True
  CppULongLong -> True
  CppFloat -> True
  CppDouble -> True
  CppLongDouble -> True
  _ -> False

promoteNumericType :: CppType -> CppType -> CppType
promoteNumericType t1 t2
  | t1 `elem` floatingTypes || t2 `elem` floatingTypes = CppDouble
  | otherwise = CppLongLong
  where
    floatingTypes = [CppFloat, CppDouble, CppLongDouble]

-- | Generate C++ from Go files
generateGoFile :: GoFile -> CppCodeGen ()
generateGoFile goFile = do
  let decls = goFileDecls goFile
  addComment $ "Processing Go file with " <> T.pack (show (length decls)) <> " declarations"
  
  -- Fallback: if no declarations found, add a comment
  when (null decls) $ do
    addComment "No declarations found in Go file - parser may need to be fixed"
  
  mapM_ generateGoDecl decls

-- | Generate C++ from Go declarations
generateGoDecl :: Located GoDecl -> CppCodeGen ()
generateGoDecl (Located _ decl) = case decl of
  GoFuncDecl func -> do
    addComment $ "Generating function: " <> maybe "anonymous" (\(Identifier n) -> n) (goFuncName func)
    generateGoFunction func
  GoTypeDecl name typeExpr -> do
    addComment $ "Generating type declaration: " <> (\(Identifier n) -> n) name
    cppType <- generateGoType typeExpr
    addDeclaration $ CppTypedef ((\(Identifier n) -> n) name) cppType
  GoVarDecl vars -> do
    addComment $ "Generating variable declaration(s)"
    mapM_ generateGoVariable vars
  _ -> addComment $ "TODO: Implement Go declaration: " <> T.pack (show decl)

-- | Generate C++ functions from Python
generatePythonFunction :: PythonFuncDef -> CppCodeGen ()
generatePythonFunction funcDef = do
  let funcName = (\(Identifier n) -> n) (pyFuncName funcDef)
  
  -- Map parameters
  cppParams <- mapM mapPythonParameter (pyFuncParams funcDef)
  
  -- Determine return type
  returnType <- case pyFuncReturns funcDef of
    Just typeExpr -> mapPythonType typeExpr
    Nothing -> if funcName == "main" 
               then return CppInt 
               else do
                 -- Infer return type from function body
                 let hasReturn = any hasReturnValue (pyFuncBody funcDef)
                 -- Default to int for functions that appear to return a value to ensure
                 -- valid C++ in presence of recursion; otherwise use void
                 return $ if hasReturn then CppInt else CppVoid
  
  -- Generate function body
  bodyStmts <- mapM generatePythonStmt (pyFuncBody funcDef)
  
  -- Add return statement for main function if needed
  let finalBodyStmts = if funcName == "main" && returnType == CppInt
                      then bodyStmts ++ [CppReturn (Just (CppLiteral $ CppIntLit 0))]
                      else bodyStmts
  
  addDeclaration $ CppFunction funcName returnType cppParams finalBodyStmts

-- | Generate C++ classes from Python
generatePythonClass :: PythonClassDef -> CppCodeGen ()
generatePythonClass classDef = do
  let className = (\(Identifier n) -> n) (pyClassName classDef)
  
  -- Map base classes
  baseClasses <- mapM extractBaseClassName (pyClassBases classDef)
  
  -- Generate class members
  members <- mapM generatePythonClassMember (pyClassBody classDef)
  let memberDecls = members  -- members are already CppDecl
  
  addDeclaration $ CppClass className baseClasses memberDecls
  where
    extractBaseClassName expr = return "BaseClass"  -- Simplified

-- | Generate C++ from Go functions
generateGoFunction :: GoFunction -> CppCodeGen ()
generateGoFunction func = do
  case goFuncName func of
    Nothing -> return ()  -- Function literal, handle differently
    Just (Identifier name) -> do
      -- Map parameters and return types
      cppParams <- mapM mapGoParameter (goFuncParams func)
      returnType <- mapGoResultsForMain name (goFuncResults func)
      
      -- Generate function body
      case goFuncBody func of
        Nothing -> addDeclaration $ CppFunction name returnType cppParams []
        Just bodyStmt -> do
          bodyStmts <- generateGoBlockStmt bodyStmt
          -- Add return 0 for main function if no explicit return
          let finalStmts = if name == "main" && not (hasReturnStmt bodyStmts)
                          then bodyStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                          else bodyStmts
          addDeclaration $ CppFunction name returnType cppParams finalStmts
          
          -- If this is the main function, also ensure we have #include <iostream>
          when (name == "main") $ do
            addInclude "<iostream>"

-- | Check if statement list contains a return statement with a value
hasReturnStmt :: [CppStmt] -> Bool
hasReturnStmt = any isReturnStmt
  where
    isReturnStmt (CppReturn _) = True
    isReturnStmt (CppBlock stmts) = hasReturnStmt stmts
    isReturnStmt _ = False

-- | Check if Python statement has a return with value
hasReturnValue :: Located PythonStmt -> Bool
hasReturnValue (Located _ stmt) = case stmt of
  PyReturn (Just _) -> True
  PyReturn Nothing -> False
  PyIf _ thenStmts elseStmts -> any hasReturnValue thenStmts || any hasReturnValue elseStmts
  PyWhile _ bodyStmts _ -> any hasReturnValue bodyStmts
  PyFor _ _ bodyStmts _ -> any hasReturnValue bodyStmts
  _ -> False

-- | Generate block statement from Go (handling compound statements)
generateGoBlockStmt :: Located GoStmt -> CppCodeGen [CppStmt]
generateGoBlockStmt (Located _ stmt) = case stmt of
  GoBlock stmts -> mapM generateGoStmt stmts
  _ -> do
    singleStmt <- generateGoStmt (Located undefined stmt)
    return [singleStmt]

-- | Generate statements from Go
generateGoStmt :: Located GoStmt -> CppCodeGen CppStmt
generateGoStmt (Located _ stmt) = case stmt of
  GoReturn exprs -> do
    case exprs of
      [] -> return $ CppReturn Nothing
      [expr] -> do
        cppExpr <- generateGoExpr expr
        return $ CppReturn (Just cppExpr)
      _ -> do
        -- Multiple return values - use std::tuple
        addInclude "<tuple>"
        cppExprs <- mapM generateGoExpr exprs
        let tupleExpr = CppCall (CppVar "std::make_tuple") cppExprs
        return $ CppReturn (Just tupleExpr)
  GoExprStmt expr -> do
    cppExpr <- generateGoExpr expr
    return $ CppExprStmt cppExpr
  GoIf _ cond thenStmt elseStmt -> do
    cppCond <- generateGoExpr cond
    cppThen <- generateGoStmt thenStmt
    cppElse <- case elseStmt of
      Nothing -> return []
      Just stmt -> do
        stmt' <- generateGoStmt stmt
        return [stmt']
    return $ CppIf cppCond [cppThen] cppElse
  GoFor mforClause bodyStmt -> do
    -- Handle for loop (simplified as while for now)
    case mforClause of
      Nothing -> do
        -- Infinite loop - convert to while(true)
        bodyStmts <- generateGoBlockStmt bodyStmt
        return $ CppWhile (CppLiteral $ CppBoolLit True) bodyStmts
      Just forClause -> do
        -- TODO: Handle proper for clause with init, condition, post
        -- For now, just convert to while(true)
        bodyStmts <- generateGoBlockStmt bodyStmt
        return $ CppWhile (CppLiteral $ CppBoolLit True) bodyStmts
  GoBlock stmts -> do
    cppStmts <- mapM generateGoStmt stmts
    return $ CppBlock cppStmts
  GoGo expr -> do
    -- Handle goroutines: convert to detached std::thread invocations
    addInclude "<thread>"
    addInclude "<functional>"
    cppExpr <- generateGoExpr expr
    tempName <- generateTempVar
    let threadVar = tempName <> "_thread"
        threadType = CppClassType "std::thread" []
        threadInit = case cppExpr of
          CppCall func args -> CppCall (CppVar "std::thread") (func : args)
          _ -> CppCall (CppVar "std::thread")
                 [CppLambda [] [CppExprStmt cppExpr]]
        declStmt = CppDecl (CppVariable threadVar threadType (Just threadInit))
        detachStmt = CppExprStmt (CppCall (CppMember (CppVar threadVar) "detach") [])
    return $ CppBlock [declStmt, detachStmt]
  GoSend channel value -> do
    -- Handle channel send: channel <- value
    cppChannel <- generateGoExpr channel
    cppValue <- generateGoExpr value
    return $ CppExprStmt $ CppCall (CppMember cppChannel "send") [cppValue]
  GoDefine identifiers exprs -> do
    -- Handle variable definition: x, y := a, b
    case (identifiers, exprs) of
      ([Identifier varName], [expr]) -> do
        -- Single variable assignment
        cppExpr <- generateGoExpr expr
        return $ CppDecl $ CppVariable varName CppAuto (Just cppExpr)
      _ -> do
        -- Multiple variable assignment - simplified for now
        addComment $ "Multiple variable definition not fully implemented"
        return $ CppComment $ "Multiple variable definition"
  GoAssign leftExprs rightExprs -> do
    -- Handle assignment: x, y = a, b
    case (leftExprs, rightExprs) of
      ([leftExpr], [rightExpr]) -> do
        -- Single assignment
        cppLeft <- generateGoExpr leftExpr
        cppRight <- generateGoExpr rightExpr
        return $ CppExprStmt $ CppBinary "=" cppLeft cppRight
      _ -> do
        -- Multiple assignment - simplified for now
        addComment $ "Multiple assignment not fully implemented"
        return $ CppComment $ "Multiple assignment"
  _ -> do
    addComment $ "TODO: Implement Go statement: " <> T.pack (show stmt)
    return $ CppComment $ "Unimplemented Go statement"

-- | Generate expressions from Go
generateGoExpr :: Located GoExpr -> CppCodeGen CppExpr
generateGoExpr (Located _ expr) = case expr of
  GoLiteral lit -> return $ CppLiteral $ mapGoLiteral lit
  GoIdent (Identifier name) -> return $ CppVar name
  GoBinaryOp op left right -> do
    cppLeft <- generateGoExpr left
    cppRight <- generateGoExpr right
    let cppOp = mapGoBinaryOp op
    return $ CppBinary cppOp cppLeft cppRight
  GoCall func args -> do
    cppFunc <- generateGoExpr func
    cppArgs <- mapM generateGoExpr args
    
    -- Special handling for fmt.Printf and similar functions
    case cppFunc of
      CppMember (CppVar "fmt") "Printf" -> do
        addInclude "<cstdio>"
        return $ CppCall (CppVar "std::printf") cppArgs
      CppMember (CppVar "fmt") "Println" -> do
        addInclude "<iostream>"
        return $ buildPrintlnExpr cppArgs
      CppMember (CppVar "fmt") "Print" -> do
        addInclude "<iostream>"
        return $ buildPrintExpr cppArgs
      _ -> return $ CppCall cppFunc cppArgs
  GoSelector obj (Identifier member) -> do
    cppObj <- generateGoExpr obj
    return $ CppMember cppObj member
  GoReceive expr -> do
    -- Handle <-channel
    cppExpr <- generateGoExpr expr
    return $ CppCall (CppMember cppExpr "receive") []
  _ -> do
    addComment $ "TODO: Implement Go expression: " <> T.pack (show expr)
    return $ CppLiteral $ CppIntLit 0

-- | Convert Go format string to C++ output
convertGoFormatToCpp :: Text -> [CppExpr] -> Text
convertGoFormatToCpp formatStr args =
  -- Simplified: just replace %d with the actual values
  -- In a real implementation, this would be much more sophisticated
  let result = T.replace "%d" "\" << " formatStr
      result2 = T.replace "%s" "\" << " result
      result3 = T.replace "\\n" "\" << std::endl" result2
  in "\"" <> result3

-- | Helper utilities for fmt-style printing
buildPrintExpr :: [CppExpr] -> CppExpr
buildPrintExpr args =
  let components = if null args
                   then [CppLiteral (CppStringLit "")]
                   else spaceSeparate args
  in streamChain (CppVar "std::cout") components

buildPrintlnExpr :: [CppExpr] -> CppExpr
buildPrintlnExpr args =
  let components =
        if null args
        then [CppVar "std::endl"]
        else spaceSeparate args ++ [CppVar "std::endl"]
  in streamChain (CppVar "std::cout") components

streamChain :: CppExpr -> [CppExpr] -> CppExpr
streamChain = foldl (\acc expr -> CppBinary "<<" acc expr)

spaceSeparate :: [CppExpr] -> [CppExpr]
spaceSeparate [] = []
spaceSeparate (x:xs) = x : concatMap (\arg -> [CppLiteral (CppStringLit " "), arg]) xs

-- | Type mapping functions
mapPythonTypeToCpp :: Type -> CppType

  TInt _ -> CppInt
  TFloat _ -> CppDouble
  TBool -> CppBool
  TString -> CppString
  TList t -> CppVector (mapPythonTypeToCpp t)
  TDict k v -> CppUnorderedMap (mapPythonTypeToCpp k) (mapPythonTypeToCpp v)
  TOptional t -> CppOptional (mapPythonTypeToCpp t)
  TOwned t -> CppUniquePtr (mapPythonTypeToCpp t)
  TShared t -> CppSharedPtr (mapPythonTypeToCpp t)
  TVoid -> CppVoid
  _ -> CppAuto  -- Fallback to auto

mapGoTypeToCpp :: GoType -> CppType
mapGoTypeToCpp = \case
  GoBasicType (Identifier "int") -> CppInt
  GoBasicType (Identifier "float64") -> CppDouble
  GoBasicType (Identifier "bool") -> CppBool
  GoBasicType (Identifier "string") -> CppString
  GoSliceType (Located _ elemType) -> CppVector (mapGoTypeToCpp elemType)
  GoMapType (Located _ keyType) (Located _ valueType) -> 
    CppUnorderedMap (mapGoTypeToCpp keyType) (mapGoTypeToCpp valueType)
  GoPointerType (Located _ baseType) -> CppPointer (mapGoTypeToCpp baseType)
  GoChanType _ (Located _ elemType) -> CppTemplateType "Channel" [mapGoTypeToCpp elemType]
  _ -> CppAuto

mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp = mapPythonTypeToCpp  -- Reuse Python mapping

-- | Literal mapping
mapPythonLiteral :: PythonLiteral -> CppLiteral
mapPythonLiteral = \case
  PyInt i -> CppIntLit i
  PyFloat f -> CppFloatLit f
  PyBool b -> CppBoolLit b
  PyString s -> CppStringLit s
  PyFString s _ -> CppStringLit s  -- For now, treat f-strings as regular strings
  PyNone -> CppNullPtr
  _ -> CppIntLit 0

mapGoLiteral :: GoLiteral -> CppLiteral
mapGoLiteral = \case
  GoInt i -> CppIntLit i
  GoFloat f -> CppFloatLit f
  GoBool b -> CppBoolLit b
  GoString s -> CppStringLit s
  GoNil -> CppNullPtr
  _ -> CppIntLit 0

-- | Operator mapping
mapPythonBinaryOp :: BinaryOp -> Text
mapPythonBinaryOp = \case
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpAnd -> "&&"
  OpOr -> "||"
  _ -> "+"  -- Fallback

mapGoBinaryOp :: BinaryOp -> Text
mapGoBinaryOp = mapPythonBinaryOp  -- Same mapping

-- | Comparison operator mapping
mapComparisonOp :: ComparisonOp -> Text
mapComparisonOp = \case
  OpEq -> "=="
  OpNe -> "!="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpIs -> "=="      -- Simplified: treat 'is' as '=='
  OpIsNot -> "!="   -- Simplified: treat 'is not' as '!='

-- | Generate Channel class for Go channel operations
-- This implements a generic channel that can handle any type via templates
generateChannelClass :: CppCodeGen ()
generateChannelClass = do
  let templateParam = CppTemplateType "T" []
  
  let channelMembers = 
        [ CppVariable "queue_" (CppTemplateType "std::queue" [templateParam]) Nothing
        , CppVariable "mutex_" (CppClassType "std::mutex" []) Nothing
        , CppVariable "cv_" (CppClassType "std::condition_variable" []) Nothing
        , CppVariable "capacity_" CppSizeT Nothing
        ]
  
  let channelMethods =
        [ CppConstructor "Channel" [CppParam "capacity" CppSizeT Nothing] 
            [ CppExprStmt $ CppBinary "=" (CppMember CppThis "capacity_") (CppVar "capacity")
            ]
        , CppMethod "send" CppVoid [CppParam "value" templateParam Nothing]
            [ CppDecl $ CppVariable "lock" (CppTemplateType "std::unique_lock" [CppClassType "std::mutex" []]) 
                (Just $ CppCall (CppVar "std::unique_lock<std::mutex>") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "wait") 
                [ CppVar "lock"
                , CppLambda [] 
                    [ CppReturn $ Just $ CppBinary "<" 
                        (CppCall (CppMember (CppMember CppThis "queue_") "size") []) 
                        (CppMember CppThis "capacity_")
                    ]
                ]
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "push") [CppVar "value"]
            , CppExprStmt $ CppCall (CppMember (CppVar "cv_") "notify_one") []
            ] False
        , CppMethod "receive" templateParam []
            [ CppDecl $ CppVariable "value" templateParam Nothing
            , CppDecl $ CppVariable "lock" (CppTemplateType "std::unique_lock" [CppClassType "std::mutex" []]) 
                (Just $ CppCall (CppVar "std::unique_lock<std::mutex>") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "wait") 
                [ CppVar "lock"
                , CppLambda [] 
                    [ CppReturn $ Just $ CppUnary "!" 
                        (CppCall (CppMember (CppMember CppThis "queue_") "empty") [])
                    ]
                ]
            , CppExprStmt $ CppBinary "=" (CppVar "value") (CppCall (CppMember (CppMember CppThis "queue_") "front") [])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "pop") []
            , CppExprStmt $ CppCall (CppMember (CppVar "cv_") "notify_one") []
            , CppReturn $ Just $ CppVar "value"
            ] False
        ]
  
  addDeclaration $ CppTemplate ["T"] (CppClass "Channel" [] (channelMembers ++ channelMethods))


-- | Helper functions
addInclude :: Text -> CppCodeGen ()
addInclude inc = do
  currentIncludes <- gets cgsIncludes
  unless (inc `elem` currentIncludes) $ 
    modify $ \s -> s { cgsIncludes = inc : cgsIncludes s }

addDeclaration :: CppDecl -> CppCodeGen ()
addDeclaration decl = modify $ \s -> s { cgsDeclarations = decl : cgsDeclarations s }

addStatement :: CppStmt -> CppCodeGen ()
addStatement stmt = do
  -- For now, we'll ignore isolated statements since we don't have a proper context
  -- In a real implementation, statements would be collected within function bodies
  return ()

addComment :: Text -> CppCodeGen ()
addComment comment = addDeclaration $ CppCommentDecl comment

enterNamespace :: Text -> CppCodeGen ()
enterNamespace ns = modify $ \s -> s { cgsNamespaces = ns : cgsNamespaces s }

exitNamespace :: CppCodeGen ()
exitNamespace = modify $ \s -> s { cgsNamespaces = drop 1 (cgsNamespaces s) }

generateTempVar :: CppCodeGen Text
generateTempVar = do
  count <- gets cgsTempVarCount
  modify $ \s -> s { cgsTempVarCount = count + 1 }
  return $ "temp_" <> T.pack (show count)

-- | Placeholder implementations for complex functions
generatePythonInteropBindings :: Text -> CppCodeGen ()
generatePythonInteropBindings moduleName = 
  addComment $ "Python interop bindings for module: " <> moduleName

generateGoInteropBindings :: Text -> CppCodeGen ()
generateGoInteropBindings packageName = 
  addComment $ "Go interop bindings for package: " <> packageName

mapPythonParameter :: Located PythonParameter -> CppCodeGen CppParam
mapPythonParameter (Located _ param) = case param of
  ParamNormal (Identifier name) mtype mdefault -> do
    cppType <- case mtype of
      Just typeExpr -> mapPythonType typeExpr
      Nothing -> return CppAuto
    cppDefault <- mapM generatePythonExpr mdefault
    return $ CppParam name cppType cppDefault
  _ -> return $ CppParam "param" CppAuto Nothing

mapGoParameter :: GoField -> CppCodeGen CppParam
mapGoParameter field = do
  let name = case goFieldNames field of
        [] -> "param"
        (Identifier n : _) -> n
  cppType <- generateGoType (goFieldType field)
  return $ CppParam name cppType Nothing

mapPythonType :: Located PythonTypeExpr -> CppCodeGen CppType
mapPythonType (Located _ typeExpr) = case typeExpr of
  TypeName (QualifiedName _ (Identifier name)) -> 
    return $ case name of
      "int" -> CppInt
      "float" -> CppDouble
      "str" -> CppString
      "bool" -> CppBool
      _ -> CppAuto
  _ -> return CppAuto

generateGoType :: Located GoType -> CppCodeGen CppType
generateGoType (Located _ goType) = return $ mapGoTypeToCpp goType

mapGoResults :: [GoField] -> CppCodeGen CppType
mapGoResults [] = return CppVoid
mapGoResults [field] = generateGoType (goFieldType field)
mapGoResults fields = do
  addInclude "<tuple>"
  types <- mapM (generateGoType . goFieldType) fields
  return $ CppTuple types

-- | Special handling for main function
mapGoResultsForMain :: Text -> [GoField] -> CppCodeGen CppType
mapGoResultsForMain "main" [] = return CppInt  -- main() should return int in C++
mapGoResultsForMain _ results = mapGoResults results

generatePythonAssignment :: Located PythonPattern -> CppExpr -> CppCodeGen ()
generatePythonAssignment (Located _ pattern) cppExpr = case pattern of
  PatVar (Identifier name) -> do
    addDeclaration $ CppVariable name CppAuto (Just cppExpr)
  _ -> addComment "TODO: Complex pattern assignment"

generatePythonClassMember :: Located PythonStmt -> CppCodeGen CppDecl
generatePythonClassMember _ = return $ CppVariable "member" CppInt Nothing  -- Simplified

generateGoVariable :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> CppCodeGen ()
generateGoVariable (Identifier name, mtype, mexpr) = do
  cppType <- case mtype of
    Just typeExpr -> generateGoType typeExpr
    Nothing -> return CppAuto
  cppExpr <- case mexpr of
    Just expr -> do
      e <- generateGoExpr expr
      return $ Just e
    Nothing -> return Nothing
  addDeclaration $ CppVariable name cppType cppExpr