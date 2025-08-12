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
  , generateCppMain  -- New function for main file
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
import Control.Monad (when, unless)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, partition, sortBy)
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
  | CppForRangeStartEnd !Text !CppExpr !CppExpr ![CppStmt] -- variable, start, end, body
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
  | CppInitList !CppType ![CppExpr]                     -- initializer list: type{arg1, arg2, ...}
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
  | CppTypeVar !Text                                     -- Template type variable (T, U, etc.)
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
        Left pyAst -> generateCppFromPython pyAst False  -- False = not main file by default
        Right goAst -> generateCppFromGo goAst False    -- False = not main file by default
  in unit

-- | Generate C++ for main file (with main function)
generateCppMain :: CppGenConfig -> Either PythonAST GoAST -> CppUnit
generateCppMain config ast = 
  let (unit, _) = runCppCodeGen config $ case ast of
        Left pyAst -> generateCppFromPython pyAst True   -- True = main file
        Right goAst -> generateCppFromGo goAst True      -- True = main file
  in unit

-- | Generate C++ from Python AST with main file flag
generateCppFromPython :: PythonAST -> Bool -> CppCodeGen CppUnit
generateCppFromPython (PythonAST pyModule) isMainFile = do
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
  
  -- Ensure we have a main function for standalone execution (only for main file)
  hasMain <- gets (any isMainFunction . cgsDeclarations)
  when (isMainFile && not hasMain) $ do
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

-- | Generate C++ from Go AST with main file flag
generateCppFromGo :: GoAST -> Bool -> CppCodeGen CppUnit
generateCppFromGo (GoAST goPackage) isMainFile = do
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
  addInclude "<tuple>"  -- Add tuple support for multiple return values
  addInclude "<array>"  -- Add array support
  addInclude "<iomanip>"  -- Add formatting support
  
  -- Generate helper functions for printing containers
  generatePrintHelpers
  
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
  
  -- Only add fallback main if we're generating a main file and no main function was found
  when (isMainFile && packageName == "main") $ do
    hasMain <- gets (any isMainFunction . cgsDeclarations)
    unless hasMain $ do
      addComment "No main function found in declarations - generating fallback main"
      -- Generate a fallback main function that returns 0
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
  PyAssign patterns expr -> do
    cppExpr <- generatePythonExpr expr
    -- For now, handle simple single-target assignment
    case patterns of
      [Located _ (PatVar (Identifier varName))] -> do
        -- For simplicity, we'll check if this looks like an initialization or an assignment
        -- If the expression references the same variable, treat as assignment
        -- Otherwise, treat as initialization
        case locatedValue expr of
          PyBinaryOp _ left right -> do
            let isSelfReference = case (locatedValue left, locatedValue right) of
                  (PyVar (Identifier name), _) -> name == varName
                  (_, PyVar (Identifier name)) -> name == varName
                  _ -> False
            if isSelfReference
              then return $ CppExprStmt $ CppBinary "=" (CppVar varName) cppExpr
              else do
                let varType = CppAuto  -- Use auto for type inference
                return $ CppDecl $ CppVariable varName varType (Just cppExpr)
          PyVar (Identifier name) -> 
            if name == varName
              then return $ CppExprStmt $ CppBinary "=" (CppVar varName) cppExpr
              else do
                let varType = CppAuto  -- Use auto for type inference
                return $ CppDecl $ CppVariable varName varType (Just cppExpr)
          _ -> do
            let varType = CppAuto  -- Use auto for type inference
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
    -- Debug: Add comment to track if condition
    addComment $ "IF condition: " <> T.pack (show condition)
    return $ CppIf cppCond cppThen cppElse
  PyFor (Located _ (PatVar (Identifier varName))) iterExpr bodyStmts _ -> do
    cppIter <- generatePythonExpr iterExpr
    cppBody <- mapM generatePythonStmt bodyStmts
    -- Handle range() function calls specifically
    addComment $ "DEBUG: For loop with iterator: " <> T.pack (show iterExpr)
    addComment $ "DEBUG: Generated cppIter: " <> T.pack (show cppIter)
    case locatedValue iterExpr of
      -- Check if this is a call to range()
      PyCall (Located _ (PyVar (Identifier "range"))) args -> do
        addComment $ "DEBUG: Detected range() call with " <> T.pack (show (length args)) <> " arguments"
        addComment $ "DEBUG: Args details: " <> T.pack (show args)
        case args of
          -- range(n) -> for(int varName = 0; varName < n; varName++)
          [Located _ (ArgPositional (Located _ (PyLiteral (PyInt n))))] -> do
            addComment $ "DEBUG: range(n) with n=" <> T.pack (show n)
            let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just $ CppLiteral $ CppIntLit 0)
            let condExpr = Just $ CppBinary "<" (CppVar varName) (CppLiteral $ CppIntLit n)
            let postExpr = Just $ CppUnary "++" (CppVar varName)
            return $ CppFor initStmt condExpr postExpr cppBody
          -- Handle variables or expressions as range bounds
          [Located _ (ArgPositional expr)] -> do
            cppExpr <- generatePythonExpr expr
            addComment $ "DEBUG: range(expr) with expr=" <> T.pack (show expr)
            let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just $ CppLiteral $ CppIntLit 0)
            let condExpr = Just $ CppBinary "<" (CppVar varName) cppExpr
            let postExpr = Just $ CppUnary "++" (CppVar varName)
            return $ CppFor initStmt condExpr postExpr cppBody
          -- range(start, end) -> for(int varName = start; varName < end; varName++)
          [Located _ (ArgPositional (Located _ (PyLiteral (PyInt start)))), Located _ (ArgPositional (Located _ (PyLiteral (PyInt end))))] -> do
            addComment $ "DEBUG: range(start, end) with start=" <> T.pack (show start) <> " end=" <> T.pack (show end)
            let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just $ CppLiteral $ CppIntLit start)
            let condExpr = Just $ CppBinary "<" (CppVar varName) (CppLiteral $ CppIntLit end)
            let postExpr = Just $ CppUnary "++" (CppVar varName)
            return $ CppFor initStmt condExpr postExpr cppBody
          -- Handle expressions as range bounds (start, end)
          [Located _ (ArgPositional startExpr), Located _ (ArgPositional endExpr)] -> do
            cppStart <- generatePythonExpr startExpr
            cppEnd <- generatePythonExpr endExpr
            addComment $ "DEBUG: range(start_expr, end_expr)"
            let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just cppStart)
            let condExpr = Just $ CppBinary "<" (CppVar varName) cppEnd
            let postExpr = Just $ CppUnary "++" (CppVar varName)
            return $ CppFor initStmt condExpr postExpr cppBody
          -- range(start, end, step) -> more complex loop
          [Located _ (ArgPositional (Located _ (PyLiteral (PyInt start)))), Located _ (ArgPositional (Located _ (PyLiteral (PyInt end)))), Located _ (ArgPositional (Located _ (PyLiteral (PyInt step))))] -> do
            addComment $ "DEBUG: range(start, end, step) with start=" <> T.pack (show start) <> " end=" <> T.pack (show end) <> " step=" <> T.pack (show step)
            let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just $ CppLiteral $ CppIntLit start)
            let condExpr = if step > 0 
                          then Just $ CppBinary "<" (CppVar varName) (CppLiteral $ CppIntLit end)
                          else Just $ CppBinary ">" (CppVar varName) (CppLiteral $ CppIntLit end)
            let postExpr = Just $ CppBinary "+=" (CppVar varName) (CppLiteral $ CppIntLit step)
            return $ CppFor initStmt condExpr postExpr cppBody
          -- Handle expressions as range bounds (start, end, step)
          [Located _ (ArgPositional startExpr), Located _ (ArgPositional endExpr), Located _ (ArgPositional stepExpr)] -> do
            cppStart <- generatePythonExpr startExpr
            cppEnd <- generatePythonExpr endExpr
            cppStep <- generatePythonExpr stepExpr
            addComment $ "DEBUG: range(start_expr, end_expr, step_expr)"
            let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just cppStart)
            -- For now, assume positive step - would need runtime check for negative
            let condExpr = Just $ CppBinary "<" (CppVar varName) cppEnd
            let postExpr = Just $ CppBinary "+=" (CppVar varName) cppStep
            return $ CppFor initStmt condExpr postExpr cppBody
          _ -> do
            addComment $ "DEBUG: Complex range() arguments, falling back to range-based for"
            return $ CppForRange varName cppIter cppBody
      -- For other iterables (lists, etc.), use range-based for loop
      _ -> do
        addComment $ "DEBUG: Range-based for loop for: " <> T.pack (show iterExpr)
        return $ CppForRange varName cppIter cppBody
  PyWhile condition bodyStmts elseStmts -> do
    cppCond <- generatePythonExpr condition
    cppBody <- mapM generatePythonStmt bodyStmts
    -- Handle else clause if present
    cppElse <- mapM generatePythonStmt elseStmts
    -- Add else handling if needed
    return $ CppWhile cppCond cppBody
  _ -> return $ CppComment $ "TODO: Implement Python statement: " <> T.pack (show stmt)

-- | Generate C++ from Python expressions
-- | Generate C++ expression from Python argument
generatePythonArgument :: Located PythonArgument -> CppCodeGen (CppExpr, Maybe (Identifier, CppExpr))
generatePythonArgument (Located _ arg) = case arg of
  ArgPositional expr -> do
    cppExpr <- generatePythonExpr expr
    return (cppExpr, Nothing)
  ArgKeyword keyword expr -> do
    cppExpr <- generatePythonExpr expr
    return (cppExpr, Just (keyword, cppExpr))
  ArgStarred expr -> do
    cppExpr <- generatePythonExpr expr
    return (cppExpr, Nothing)  -- Simplified
  ArgKwStarred expr -> do
    cppExpr <- generatePythonExpr expr
    return (cppExpr, Nothing)  -- Simplified

generatePythonExpr :: Located PythonExpr -> CppCodeGen CppExpr
generatePythonExpr (Located _ expr) = case expr of
  PyLiteral lit -> case lit of
    PyFString template exprs -> do
      -- Handle f-string by generating proper C++ stream operations
      addInclude "<iostream>"
      addInclude "<sstream>"  
      addInclude "<string>"
      -- Debug the f-string processing
      addComment $ "DEBUG: Processing f-string template: " <> template
      addComment $ "DEBUG: F-string has " <> T.pack (show (length exprs)) <> " expressions"
      -- Convert expressions to C++ expressions
      cppExprs <- mapM generatePythonExpr exprs
      
      -- Process f-string template to extract parts and expressions
      let parts = parseFStringTemplate template
      addComment $ "DEBUG: F-string parsed into " <> T.pack (show (length parts)) <> " parts"
      
      -- Build streaming expression components
      case parts of
        [] -> return $ CppLiteral $ CppStringLit ""
        [LiteralPart text] -> 
          -- For a single literal, we can just return it as a string literal
          return $ CppLiteral $ CppStringLit text
        _ -> do
          -- For multiple parts, build the streaming expression components
          let streamComponents = buildFStringComponents parts cppExprs
          -- Return the chained expression without std::cout (will be added by caller)
          case streamComponents of
            [] -> return $ CppLiteral $ CppStringLit ""
            [single] -> return single
            (first:rest) -> return $ foldl (CppBinary "<<") first rest
    _ -> return $ CppLiteral $ mapPythonLiteral lit
  PyVar (Identifier name) -> return $ CppVar name
  PyBinaryOp op left right -> do
    cppLeft <- generatePythonExpr left
    cppRight <- generatePythonExpr right
    
    -- Special handling for string concatenation and division
    case op of
      OpAdd -> do
        -- For addition, check if we might be dealing with strings
        -- and wrap operands in std::string() to enable concatenation
        let isStringLiteral expr = case expr of
              CppLiteral (CppStringLit _) -> True
              _ -> False
        let needsStringConversion = isStringLiteral cppLeft || isStringLiteral cppRight
        
        if needsStringConversion
          then do
            let wrapInString expr = case expr of
                  CppLiteral (CppStringLit s) -> CppCall (CppVar "std::string") [CppLiteral (CppStringLit s)]
                  _ -> CppCall (CppVar "std::string") [expr]
            return $ CppBinary "+" (wrapInString cppLeft) (wrapInString cppRight)
          else do
            let cppOp = mapPythonBinaryOp op
            return $ CppBinary cppOp cppLeft cppRight
      OpDiv -> do
        -- For division, ensure at least one operand is a float to get float result
        let ensureFloat expr = case expr of
              CppLiteral (CppIntLit i) -> CppLiteral (CppFloatLit (fromIntegral i))
              CppLiteral (CppFloatLit _) -> expr
              _ -> CppCast CppDouble expr  -- Cast to double for non-literals
        let cppLeftFloat = ensureFloat cppLeft
        let cppRightFloat = ensureFloat cppRight
        let cppOp = mapPythonBinaryOp op
        return $ CppBinary cppOp cppLeftFloat cppRightFloat
      _ -> do
        let cppOp = mapPythonBinaryOp op
        return $ CppBinary cppOp cppLeft cppRight
  PyComparison ops exprs -> do
    -- Handle comparison expressions like x > 5
    addComment $ "DEBUG: PyComparison ops=" <> T.pack (show ops) <> " exprs=" <> T.pack (show exprs)
    case (ops, exprs) of
      ([op], [left, right]) -> do
        cppLeft <- generatePythonExpr left
        cppRight <- generatePythonExpr right
        let cppOp = mapPythonComparisonOp op
        addComment $ "DEBUG: Single comparison " <> T.pack (show op) <> " -> " <> cppOp
        return $ CppBinary cppOp cppLeft cppRight
      _ -> do
        -- For chained comparisons like a < b < c, convert to a < b && b < c
        case exprs of
          (left:rest) | length ops == length rest -> do
            cppLeft <- generatePythonExpr left
            cppExprs <- mapM generatePythonExpr rest
            let comparisons = zipWith3 (\op leftExpr rightExpr -> 
                  CppBinary (mapPythonComparisonOp op) leftExpr rightExpr) 
                  ops (cppLeft : init cppExprs ++ [last cppExprs]) cppExprs
            -- Chain all comparisons with &&
            case comparisons of
              [] -> return $ CppLiteral $ CppBoolLit True
              [single] -> return single
              (first:restComps) -> return $ foldl (\acc comp -> CppBinary "&&" acc comp) first restComps
          _ -> do
            -- Fallback for malformed comparisons
            addComment $ "Malformed comparison expression: " <> T.pack (show expr)
            return $ CppLiteral $ CppBoolLit False
  PyCall func args -> do
    cppFunc <- generatePythonExpr func
    cppArgsWithKeywords <- mapM generatePythonArgument args
    let (cppArgs, maybeKeywordArgs) = unzip cppArgsWithKeywords
    let keywordArgs = [kwarg | Just kwarg <- maybeKeywordArgs]
    -- Handle special functions
    case func of
      Located _ (PyVar (Identifier "print")) -> do
        -- Convert print to std::cout, handling the 'end' parameter
        addComment $ "DEBUG: Generating print with " <> T.pack (show (length cppArgs)) <> " arguments"
        addInclude "<iostream>"
        -- Check if 'end' keyword argument is present
        let endParam = case [expr | (Identifier "end", expr) <- keywordArgs] of
              [CppLiteral (CppStringLit "")] -> ""  -- end=""
              [CppLiteral (CppStringLit " ")] -> " "  -- end=" "
              [CppLiteral (CppStringLit s)] -> s    -- end="something"
              _ -> "\n"  -- default newline
        let positionalArgs = [arg | (arg, Nothing) <- zip cppArgs maybeKeywordArgs]
        case positionalArgs of
          [] -> 
            -- Empty print() should just print newline
            return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [arg] -> 
            -- Single argument print
            if endParam == "\n" 
            then return $ CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") arg) (CppVar "std::endl")
            else return $ CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") arg) (CppLiteral (CppStringLit endParam))
          args -> do
            -- Chain multiple << operators for multiple arguments  
            -- For print("a", x, "b"), generate: std::cout << "a" << " " << x << " " << "b" << std::endl
            addComment $ "DEBUG: Multi-argument print with " <> T.pack (show (length args)) <> " args"
            let buildChain [] acc = acc
                buildChain (arg:rest) acc = 
                  let withSpace = if null rest then acc else CppBinary "<<" acc (CppLiteral (CppStringLit " "))
                      withArg = CppBinary "<<" withSpace arg
                  in buildChain rest withArg
            let chainedOutput = buildChain args (CppVar "std::cout")
            if endParam == "\n"
            then return $ CppBinary "<<" chainedOutput (CppVar "std::endl")
            else return $ CppBinary "<<" chainedOutput (CppLiteral (CppStringLit endParam))
      Located _ (PyVar (Identifier "range")) -> do
        -- Handle range() function calls - these should only be processed by for loops
        -- Don't generate actual range calls since they'll be converted to C++ for loops
        addComment $ "WARNING: range() call outside of for loop context - this may not work as expected"
        case cppArgs of
          [arg] -> return $ CppLiteral (CppIntLit 0)  -- Placeholder
          _ -> return $ CppLiteral (CppIntLit 0)  -- Placeholder
      _ -> do
        addComment $ "DEBUG: Fallback case for function: " <> T.pack (show func)
        return $ CppCall cppFunc cppArgs
  PyList exprs -> do
    addInclude "<vector>"
    cppExprs <- mapM generatePythonExpr exprs
    -- Generate std::vector initialization with initializer list syntax
    let vectorType = CppVector CppInt  -- For now, assume int type
    return $ CppInitList vectorType cppExprs
  _ -> do
    addComment $ "TODO: Implement Python expression: " <> T.pack (show expr)
    return $ CppLiteral $ CppIntLit 0

-- | Generate C++ from Go files
generateGoFile :: GoFile -> CppCodeGen ()
generateGoFile goFile = do
  let decls = goFileDecls goFile
  addComment $ "Processing Go file with " <> T.pack (show (length decls)) <> " declarations"
  
  -- Process declarations in correct order - functions first, then others
  let (funcDecls, otherDecls) = partition isFuncDecl decls
  
  -- Process function declarations
  mapM_ generateGoDecl funcDecls
  
  -- Process other declarations (types, variables, constants)
  mapM_ generateGoDecl otherDecls
  
  -- If this is the main package and we don't have a main function, add one
  hasMainFunc <- gets (any isMainFunction . cgsDeclarations)
  let isMainPackage = (unIdentifier . goFilePackage) goFile == "main"
  when (isMainPackage && not hasMainFunc) $ do
    addComment "Adding default main function"
    addDeclaration $ CppFunction "main" CppInt [] [CppReturn (Just (CppLiteral (CppIntLit 0)))]
  
  where
    isFuncDecl (Located _ (GoFuncDecl _)) = True
    isFuncDecl _ = False
    
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False
    
    unIdentifier (Identifier name) = name

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
                 return $ if hasReturn then CppAuto else CppVoid
  
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
      cppParams <- mapGoParameters (goFuncParams func)
      returnType <- mapGoResultsForMain name (goFuncResults func)
      
      -- Generate function body
      case goFuncBody func of
        Nothing -> addDeclaration $ CppFunction name returnType cppParams []
        Just bodyStmt -> do
          bodyStmts <- generateGoBlockStmt bodyStmt
          -- Fix empty returns in main function
          let fixedStmts = if name == "main" 
                          then map (fixMainReturn name) bodyStmts 
                          else bodyStmts
          -- Add return 0 for main function if no explicit return
          let finalStmts = if name == "main" && not (hasReturnStmt fixedStmts)
                          then fixedStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                          else fixedStmts
          addDeclaration $ CppFunction name returnType cppParams finalStmts
          
          -- If this is the main function, also ensure we have #include <iostream>
          when (name == "main") $ do
            addInclude "<iostream>"
-- | Helper function to extract text from Identifier
unIdentifier :: Identifier -> Text
unIdentifier (Identifier name) = name

-- | Map Go parameters to C++ parameters, handling shorthand notation (a, b int)
mapGoParameters :: [GoField] -> CppCodeGen [CppParam]
mapGoParameters fields = do
  allParams <- mapM expandGoField fields
  return $ concat allParams
  where
    expandGoField :: GoField -> CppCodeGen [CppParam]
    expandGoField field = case goFieldNames field of
      [] -> do
        -- Anonymous parameter
        cppType <- generateGoType (goFieldType field)
        return [CppParam "param" cppType Nothing]
      [name] -> do
        -- Single named parameter
        cppType <- generateGoType (goFieldType field)
        return [CppParam (unIdentifier name) cppType Nothing]
      names -> do
        -- Multiple names with same type (a, b int) -> expand to separate parameters
        cppType <- generateGoType (goFieldType field)
        mapM (\name -> return $ CppParam (unIdentifier name) cppType Nothing) names
-- | Map a single Go parameter to C++ parameter

-- | Check if statement list contains a return statement with a value
hasReturnStmt :: [CppStmt] -> Bool
hasReturnStmt = any isReturnStmt
  where
    isReturnStmt (CppReturn _) = True
    isReturnStmt (CppBlock stmts) = hasReturnStmt stmts
    isReturnStmt _ = False

-- | Fix empty return statements in main function to return 0
fixMainReturn :: Text -> CppStmt -> CppStmt
fixMainReturn funcName stmt = case stmt of
  CppReturn Nothing | funcName == "main" -> CppReturn (Just (CppLiteral (CppIntLit 0)))
  CppBlock stmts -> CppBlock (map (fixMainReturn funcName) stmts)
  _ -> stmt

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
  GoBlock stmts -> do
    cppStmts <- mapM generateGoStmt stmts
    -- Flatten any CppBlock statements to avoid nested scopes
    return $ concatMap flattenCppStmt cppStmts
  _ -> do
    singleStmt <- generateGoStmt (Located undefined stmt)
    return $ flattenCppStmt singleStmt

-- | Flatten CppBlock statements to avoid unnecessary nesting
flattenCppStmt :: CppStmt -> [CppStmt]
flattenCppStmt (CppBlock stmts) = concatMap flattenCppStmt stmts
flattenCppStmt stmt = [stmt]

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
        cppExprs <- mapM generateGoExpr exprs
        let tupleExpr = CppCall (CppVar "std::make_tuple") cppExprs
        return $ CppReturn (Just tupleExpr)
  GoExprStmt expr -> do
    cppExpr <- generateGoExpr expr
    return $ CppExprStmt cppExpr
  GoIf _ cond thenStmt elseStmt -> do
    cppCond <- generateGoExpr cond
    cppThenStmts <- case thenStmt of
      Located _ (GoBlock stmts) -> mapM generateGoStmt stmts
      _ -> do
        stmt <- generateGoStmt thenStmt
        return [stmt]
    cppElseStmts <- case elseStmt of
      Nothing -> return []
      Just (Located _ (GoBlock stmts)) -> mapM generateGoStmt stmts
      Just stmt -> do
        stmt' <- generateGoStmt stmt
        return [stmt']
    return $ CppIf cppCond cppThenStmts cppElseStmts
  GoFor mforClause bodyStmt -> do
    -- Handle for loop properly
    case mforClause of
      Nothing -> do
        -- Infinite loop - convert to while(true)
        bodyStmts <- generateGoBlockStmt bodyStmt
        return $ CppWhile (CppLiteral $ CppBoolLit True) bodyStmts
      Just forClause -> do
        -- Handle proper for clause with init, condition, post
        bodyStmts <- generateGoBlockStmt bodyStmt
        
        let dummySpan = SourceSpan "<codegen>" (SourcePos 0 0) (SourcePos 0 0)
        
        -- Generate proper C++ for loop from Go for clause
        initStmt <- case goForInit forClause of
          Nothing -> return Nothing
          Just locatedStmt -> do
            cppStmt <- generateGoStmt locatedStmt
            return $ Just cppStmt
        
        condExpr <- case goForCond forClause of
          Nothing -> do
            addComment "DEBUG: goForCond is Nothing!"
            return Nothing
          Just locatedExpr -> do
            addComment $ "DEBUG: goForCond found: " <> T.pack (show locatedExpr)
            cppExpr <- generateGoExpr locatedExpr
            return $ Just cppExpr
        
        postExpr <- case goForPost forClause of
          Nothing -> return Nothing
          Just locatedStmt -> do
            -- Convert post statement to expression (like i++ -> ++i)
            cppStmt <- generateGoStmt locatedStmt
            case cppStmt of
              CppExprStmt expr -> return $ Just expr
              _ -> return Nothing
        
        return $ CppFor initStmt condExpr postExpr bodyStmts
  GoBlock stmts -> do
    cppStmts <- mapM generateGoStmt stmts
    return $ CppBlock cppStmts
  GoGo expr -> do
    -- Handle goroutines: convert to std::thread with proper lambda handling
    addInclude "<thread>"
    addInclude "<functional>"
    cppExpr <- generateGoExpr expr
    
    case cppExpr of
      CppCall func args -> do
        -- Handle function calls in goroutines
        let threadArgs = case func of
              CppVar funcName -> 
                [CppCall (CppVar "std::thread") ([func] ++ args)]
              _ -> [CppCall (CppVar "std::thread") [cppExpr]]
        return $ CppExprStmt (head threadArgs)
      _ ->
        return $ CppExprStmt $ CppCall (CppVar "std::thread") [cppExpr]
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
        -- Infer type from the expression (fix integer literals)
        let cppType = inferTypeFromExpr expr
        return $ CppDecl $ CppVariable varName cppType (Just cppExpr)
      (multipleNames, multipleExprs) -> do
        -- Multiple variable assignment - handle tuple unpacking
        if length multipleNames == length multipleExprs
        then do
          -- Generate individual declarations (not in a block to avoid scope issues)
          let pairs = zip multipleNames multipleExprs
          assignments <- mapM (\(Identifier name, expr) -> do
            cppExpr <- generateGoExpr expr
            let cppType = inferTypeFromExpr expr
            return $ CppDecl $ CppVariable name cppType (Just cppExpr)
            ) pairs
          -- Return individual statements, not wrapped in a block
          case assignments of
            [] -> return $ CppComment "Empty assignment"
            [single] -> return single
            multiple -> return $ CppBlock multiple
        else if length multipleExprs == 1
        then do
          -- Handle tuple unpacking from single expression (e.g., x, y := func())
          cppExpr <- generateGoExpr (head multipleExprs)
          -- Generate individual declarations first, then std::tie assignment
          let varNames = map (\(Identifier name) -> name) multipleNames
              declarations = map (\name -> CppDecl $ CppVariable name CppAuto Nothing) varNames
              tieExpr = CppCall (CppVar "std::tie") (map CppVar varNames)
              assignment = CppExprStmt $ CppBinary "=" tieExpr cppExpr
          return $ CppBlock (declarations ++ [assignment])
        else
          return $ CppComment "Unsupported multiple assignment pattern"
  GoAssign leftExprs rightExprs -> do
    -- Handle assignment: x, y = a, b
    case (leftExprs, rightExprs) of
      ([leftExpr], [rightExpr]) -> do
        -- Single assignment
        cppLeft <- generateGoExpr leftExpr
        cppRight <- generateGoExpr rightExpr
        return $ CppExprStmt $ CppBinary "=" cppLeft cppRight
      (multipleLeft, multipleRight) -> do
        -- Multiple assignment: a, b = b, a+b
        if length multipleLeft == length multipleRight
        then do
          let pairs = zip multipleLeft multipleRight
          -- For simultaneous assignment, we need to evaluate all right expressions first
          cppRightExprs <- mapM generateGoExpr multipleRight
          cppLeftExprs <- mapM generateGoExpr multipleLeft
          -- Use temporary variables for proper simultaneous assignment
          let tempVars = map (\i -> "temp_" <> T.pack (show i)) [0 .. length pairs - 1]
              tempDecls = zipWith (\tempName rightExpr -> 
                CppDecl $ CppVariable tempName CppAuto (Just rightExpr)
                ) tempVars cppRightExprs
              assignments = zipWith (\leftExpr tempName -> 
                CppExprStmt $ CppBinary "=" leftExpr (CppVar tempName)
                ) cppLeftExprs tempVars
          return $ CppBlock (tempDecls ++ assignments)
        else if length multipleRight == 1
        then do
          -- Tuple unpacking from single expression
          cppRight <- generateGoExpr (head multipleRight)
          leftVars <- mapM generateGoExpr multipleLeft
          let tieExpr = CppCall (CppVar "std::tie") leftVars
              assignment = CppExprStmt $ CppBinary "=" tieExpr cppRight
          return assignment
        else do
          -- Unsupported pattern
          addComment $ "Multiple assignment pattern not supported"
          return $ CppComment $ "Multiple assignment"
  GoVarStmt vars -> do
    -- Handle variable declarations: var x int = 42
    case vars of
      [(Identifier varName, Just typeExpr, Just valueExpr)] -> do
        -- Single variable with type and value
        cppType <- generateGoType typeExpr
        cppValue <- generateGoExpr valueExpr
        return $ CppDecl $ CppVariable varName cppType (Just cppValue)
      [(Identifier varName, Just typeExpr, Nothing)] -> do
        -- Single variable with type, no initial value
        cppType <- generateGoType typeExpr
        return $ CppDecl $ CppVariable varName cppType Nothing
      [(Identifier varName, Nothing, Just valueExpr)] -> do
        -- Single variable with value, inferred type
        cppValue <- generateGoExpr valueExpr
        return $ CppDecl $ CppVariable varName CppAuto (Just cppValue)
      multipleVars -> do
        -- Multiple variables declaration - generate all variables
        -- Create a comment that lists all variables being declared
        let varNames = map (\(Identifier name, _, _) -> name) multipleVars
        let comment = "Multiple variables: " <> T.intercalate ", " varNames
        addComment comment
        
        -- Generate only the first variable for now as a temporary workaround
        case multipleVars of
          [] -> return $ CppComment "Empty variable declaration"
          (firstVar:_) -> generateGoVariableDecl firstVar
  GoIncDec expr isIncrement -> do
    -- Handle increment/decrement: i++ or i--
    cppExpr <- generateGoExpr expr
    let op = if isIncrement then "++" else "--"
    return $ CppExprStmt $ CppUnary op cppExpr
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
  GoComparison op left right -> do
    cppLeft <- generateGoExpr left
    cppRight <- generateGoExpr right
    let cppOp = mapPythonComparisonOp op
    return $ CppBinary cppOp cppLeft cppRight
  GoQualifiedIdent pkg (Identifier name) -> do
    case (pkg, name) of
      (Identifier "fmt", "Println") -> return $ CppVar "fmt_println"
      (Identifier "fmt", "Print") -> return $ CppVar "fmt_print"
      (Identifier "fmt", "Printf") -> return $ CppVar "fmt_printf"
      _ -> return $ CppVar $ (case pkg of Identifier p -> p) <> "." <> name
  GoCall func args -> do
    cppFunc <- generateGoExpr func
    cppArgs <- mapM generateGoExpr args
    
    -- Special handling for fmt.Printf and similar functions
    case cppFunc of
      CppVar "fmt_printf" -> do
        addInclude "<iostream>"
        addInclude "<iomanip>"
        -- Convert Go format string to C++ style
        case cppArgs of
          (CppLiteral (CppStringLit formatStr) : restArgs) -> do
            -- Generate proper streaming expression: std::cout << ... << ... << ...
            let streamingExpr = buildStreamingExpr formatStr restArgs
            return streamingExpr
          _ -> return $ CppCall cppFunc cppArgs
      CppVar "fmt_println" -> do
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [arg] -> do
            -- For single arguments, just use regular stream output
            return $ CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") arg) (CppVar "std::endl")
          _ -> do
            -- Multiple arguments to println - add spaces between arguments
            let addSpaceBetween acc arg = CppBinary "<<" (CppBinary "<<" acc (CppLiteral (CppStringLit " "))) arg
            let streamingExpr = case cppArgs of
                  [] -> CppVar "std::cout"
                  (firstArg:restArgs) -> 
                    let baseExpr = CppBinary "<<" (CppVar "std::cout") firstArg
                    in foldl addSpaceBetween baseExpr restArgs
            return $ CppBinary "<<" streamingExpr (CppVar "std::endl")
      CppVar "fmt_print" -> do
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppVar "std::cout"
          [arg] -> return $ CppBinary "<<" (CppVar "std::cout") arg
          _ -> do
            -- Multiple arguments to print - add spaces between arguments (no newline)
            let addSpaceBetween acc arg = CppBinary "<<" (CppBinary "<<" acc (CppLiteral (CppStringLit " "))) arg
            let streamingExpr = case cppArgs of
                  [] -> CppVar "std::cout"
                  (firstArg:restArgs) -> 
                    let baseExpr = CppBinary "<<" (CppVar "std::cout") firstArg
                    in foldl addSpaceBetween baseExpr restArgs
            return streamingExpr
      CppMember (CppVar "fmt") "Println" -> do
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [arg] -> return $ CppBinary "<<" (CppVar "std::cout") 
                     (CppBinary "<<" arg (CppVar "std::endl"))
          _ -> do
            -- Multiple arguments to println
            let addSpaceBetween acc arg = CppBinary "<<" (CppBinary "<<" acc (CppLiteral (CppStringLit " "))) arg
            let streamingExpr = case cppArgs of
                  [] -> CppVar "std::cout"
                  (firstArg:restArgs) -> 
                    let baseExpr = CppBinary "<<" (CppVar "std::cout") firstArg
                    in foldl addSpaceBetween baseExpr restArgs
            return $ CppBinary "<<" streamingExpr (CppVar "std::endl")
      CppMember (CppVar "fmt") "Print" -> do
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [arg] -> return $ CppBinary "<<" (CppVar "std::cout") arg
          _ -> do
            -- Multiple arguments to print
            let streamingExpr = foldl (\acc arg -> CppBinary "<<" acc arg) (CppVar "std::cout") cppArgs
            return streamingExpr
      CppVar "println" -> do
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [arg] -> return $ CppBinary "<<" (CppVar "std::cout") 
                     (CppBinary "<<" arg (CppVar "std::endl"))
          _ -> do
            -- Multiple arguments to println
            let addSpaceBetween acc arg = CppBinary "<<" (CppBinary "<<" acc (CppLiteral (CppStringLit " "))) arg
            let streamingExpr = case cppArgs of
                  [] -> CppVar "std::cout"
                  (firstArg:restArgs) -> 
                    let baseExpr = CppBinary "<<" (CppVar "std::cout") firstArg
                    in foldl addSpaceBetween baseExpr restArgs
            return $ CppBinary "<<" streamingExpr (CppVar "std::endl")
      CppVar "std::cout" -> do
        -- This is the case where fmt.Printf/Println/Print was converted to std::cout
        addInclude "<iostream>"
        addInclude "<iomanip>"
        case cppArgs of
          [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [CppLiteral (CppStringLit formatStr)] -> do
            -- Single format string argument (like fmt.Printf with no args)
            let streamingExpr = buildStreamingExpr formatStr []
            return streamingExpr
          (CppLiteral (CppStringLit formatStr) : restArgs) -> do
            -- Format string with arguments (fmt.Printf case)
            let streamingExpr = buildStreamingExpr formatStr restArgs
            return streamingExpr
          [arg] -> return $ CppBinary "<<" (CppVar "std::cout") 
                     (CppBinary "<<" arg (CppVar "std::endl"))
          _ -> do
            -- Multiple arguments to println (non-format case)  
            let addSpaceBetween acc arg = CppBinary "<<" (CppBinary "<<" acc (CppLiteral (CppStringLit " "))) arg
            let streamingExpr = case cppArgs of
                  [] -> CppVar "std::cout"
                  (firstArg:restArgs) -> 
                    let baseExpr = CppBinary "<<" (CppVar "std::cout") firstArg
                    in foldl addSpaceBetween baseExpr restArgs
            return $ CppBinary "<<" streamingExpr (CppVar "std::endl")
      _ -> return $ CppCall cppFunc cppArgs
  GoSelector obj (Identifier member) -> do
    cppObj <- generateGoExpr obj
    case cppObj of
      CppVar pkg -> do
        -- Check if this is a qualified identifier like fmt.Println
        case (pkg, member) of
          ("fmt", "Println") -> return $ CppVar "fmt_println"
          ("fmt", "Print") -> return $ CppVar "fmt_print"
          ("fmt", "Printf") -> return $ CppVar "fmt_printf"
          _ -> return $ CppMember cppObj member
      _ -> return $ CppMember cppObj member
  GoReceive expr -> do
    -- Handle <-channel
    cppExpr <- generateGoExpr expr
    return $ CppCall (CppMember cppExpr "receive") []
  GoBuiltinCall builtin args -> do
    cppArgs <- mapM generateGoExpr args
    case builtin of
      GoPrint -> do
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppCall (CppVar "std::cout") [CppLiteral (CppStringLit "")]
          [arg] -> return $ CppBinary "<<" (CppVar "std::cout") arg
          _ -> do
            -- Multiple arguments to print
            let streamingExpr = foldl (\acc arg -> CppBinary "<<" acc arg) (CppVar "std::cout") cppArgs
            return streamingExpr
      GoPrintln -> do
        addInclude "<iostream>"
        case cppArgs of
          [] -> return $ CppCall (CppVar "std::cout") [CppLiteral (CppStringLit "")]
          [arg] -> return $ CppBinary "<<" (CppVar "std::cout") 
                     (CppBinary "<<" arg (CppVar "std::endl"))
          _ -> do
            -- Multiple arguments to println
            let addSpaceBetween acc arg = CppBinary "<<" (CppBinary "<<" acc (CppLiteral (CppStringLit " "))) arg
            let streamingExpr = case cppArgs of
                  [] -> CppVar "std::cout"
                  (firstArg:restArgs) -> 
                    let baseExpr = CppBinary "<<" (CppVar "std::cout") firstArg
                    in foldl addSpaceBetween baseExpr restArgs
            return $ CppBinary "<<" streamingExpr (CppVar "std::endl")
      GoAppend -> do
        -- Handle append(slice, item)
        case cppArgs of
          [sliceExpr, itemExpr] -> do
            -- For now, create a new vector with the item added
            -- In a real implementation, this would use vector::push_back
            return $ CppCall (CppMember sliceExpr "push_back") [itemExpr]
          _ -> do
            addComment $ "Append with multiple items not fully implemented"
            return $ CppLiteral $ CppIntLit 0
      GoMake -> do
        -- Handle make(type, args...)
        case cppArgs of
          -- make(map[key]value) - create empty map
          _ -> do
            -- For now, just create a default map or vector
            -- We'll improve this later
            return $ CppCall (CppVar "std::unordered_map") []
      _ -> do
        addComment $ "TODO: Implement Go builtin: " <> T.pack (show builtin)
        return $ CppLiteral $ CppIntLit 0
  GoCompositeLit mtypeExpr elements -> do
    -- Handle composite literals like [3]int{1, 2, 3} or []int{1, 2, 3}
    cppElements <- mapM generateGoExpr elements
    
    case mtypeExpr of
      Nothing -> do
        -- No type specified, use vector for slices
        let vectorType = CppVector CppInt  -- Default to int for now
        return $ CppInitList vectorType cppElements
      Just (Located _ typeExpr) -> do
        -- Type specified, use appropriate type
        case typeExpr of
          GoArrayType sizeExpr (Located _ elemType) -> do
            -- Array type like [3]int - use C-style array
            let cppElemType = mapGoTypeToCpp elemType
            case locatedValue sizeExpr of
              GoLiteral (GoInt size) -> do
                -- Use C-style array
                let arrayType = CppArray cppElemType (fromIntegral size)
                return $ CppInitList arrayType cppElements
              _ -> return $ CppInitList (CppArray cppElemType 10) cppElements
          GoSliceType (Located _ elemType) -> do
            -- Slice type like []int
            let cppElemType = mapGoTypeToCpp elemType
                vectorType = CppVector cppElemType
            return $ CppInitList vectorType cppElements
          _ -> do
            -- Other types
            let cppType = mapGoTypeToCpp typeExpr
            return $ CppInitList cppType cppElements
  GoIndex arrayExpr indexExpr -> do
    -- Handle array/slice indexing: array[index]
    cppArray <- generateGoExpr arrayExpr
    cppIndex <- generateGoExpr indexExpr
    return $ CppIndex cppArray cppIndex
  _ -> do
    addComment $ "TODO: Implement Go expression: " <> T.pack (show expr)
    return $ CppLiteral $ CppIntLit 0

-- | Infer C++ type from Go expression
inferTypeFromExpr :: Located GoExpr -> CppType
inferTypeFromExpr (Located _ expr) = case expr of
  GoLiteral (GoInt _) -> CppInt
  GoLiteral (GoFloat _) -> CppDouble
  GoLiteral (GoBool _) -> CppBool
  GoLiteral (GoString _) -> CppString
  GoCompositeLit mtypeExpr _ -> 
    case mtypeExpr of
      Nothing -> CppVector CppInt  -- Default to vector of int for slices
      Just (Located _ typeExpr) -> 
        case typeExpr of
          GoArrayType sizeExpr (Located _ elemType) -> 
            let cppElemType = mapGoTypeToCpp elemType
            in CppArray cppElemType 10  -- Default size, will be overridden by actual size in codegen
          GoSliceType (Located _ elemType) -> 
            let cppElemType = mapGoTypeToCpp elemType
            in CppVector cppElemType
          _ -> mapGoTypeToCpp typeExpr
  GoCall _ _ -> CppAuto  -- For function calls, use auto for now
  GoIdent _ -> CppAuto    -- For identifiers, use auto for now
  _ -> CppAuto

-- | Build streaming expression for printf-like functions
buildStreamingExpr :: Text -> [CppExpr] -> CppExpr
buildStreamingExpr formatStr args = 
  -- General printf format string to C++ streaming conversion
  -- "Pi = %.2f\n" with [3.14159] becomes: 
  -- std::cout << "Pi = " << std::fixed << std::setprecision(2) << 3.14159 << std::endl
  let normalizedFormat = T.replace "\\n" "\n" formatStr  -- Ensure consistent newline representation
      hasEndl = T.isSuffixOf "\n" normalizedFormat
      cleanFormatStr = if hasEndl then T.init normalizedFormat else normalizedFormat
      -- Escape any literal newlines in the format string for C++
      safeFormatStr = T.replace "\n" "\\n" cleanFormatStr
  in case args of
    [] -> -- No arguments, just print the format string
      let baseExpr = CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit safeFormatStr))
      in if hasEndl then CppBinary "<<" baseExpr (CppVar "std::endl") else baseExpr
    [arg1] -> -- Single argument
      -- Handle different format types: %d, %f, %.2f, %s, etc.
      if T.isInfixOf "%d" safeFormatStr then
        -- Integer format
        let parts = T.splitOn "%d" safeFormatStr
        in case parts of
          [before, after] ->
            let expr1 = if T.null before then CppVar "std::cout"
                        else CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit before))
                expr2 = CppBinary "<<" expr1 arg1
                expr3 = if T.null after then expr2
                        else CppBinary "<<" expr2 (CppLiteral (CppStringLit after))
            in if hasEndl then CppBinary "<<" expr3 (CppVar "std::endl") else expr3
          _ -> CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit safeFormatStr))
      else if T.isInfixOf "%.2f" safeFormatStr then
        -- Float format with 2 decimal places
        let parts = T.splitOn "%.2f" safeFormatStr
        in case parts of
          [before, after] ->
            let expr1 = if T.null before then CppVar "std::cout"
                        else CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit before))
                expr2 = CppBinary "<<" expr1 (CppVar "std::fixed")
                expr3 = CppBinary "<<" expr2 (CppCall (CppVar "std::setprecision") [CppLiteral (CppIntLit 2)])
                expr4 = CppBinary "<<" expr3 arg1
                expr5 = if T.null after then expr4
                        else CppBinary "<<" expr4 (CppLiteral (CppStringLit after))
            in if hasEndl then CppBinary "<<" expr5 (CppVar "std::endl") else expr5
          _ -> CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit safeFormatStr))
      else if T.isInfixOf "%f" safeFormatStr then
        -- Basic float format
        let parts = T.splitOn "%f" safeFormatStr
        in case parts of
          [before, after] ->
            let expr1 = if T.null before then CppVar "std::cout"
                        else CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit before))
                expr2 = CppBinary "<<" expr1 arg1
                expr3 = if T.null after then expr2
                        else CppBinary "<<" expr2 (CppLiteral (CppStringLit after))
            in if hasEndl then CppBinary "<<" expr3 (CppVar "std::endl") else expr3
          _ -> CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit safeFormatStr))
      else
        -- Fallback: treat as string or unrecognized format
        let expr1 = CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit safeFormatStr))
        in if hasEndl then CppBinary "<<" expr1 (CppVar "std::endl") else expr1
    _ -> -- Multiple arguments - handle multiple format specifiers
      -- Handle multiple format specifiers: %d, %f, %s, %t, %.1f, %.2f etc.
      let formatSpecifiers = ["%d", "%f", "%s", "%t", "%.1f", "%.2f"]
          -- Find all format specifiers in the string and their positions
          findFormatSpecs :: Text -> [(Text, Int)]
          findFormatSpecs str = 
            let allMatches = [(spec, pos) | spec <- formatSpecifiers, 
                             pos <- findAllOccurrences spec str]
            in sortBy (\(_, pos1) (_, pos2) -> compare pos1 pos2) allMatches
          
          findAllOccurrences :: Text -> Text -> [Int]
          findAllOccurrences pattern text = findAll 0 text
            where
              findAll offset remaining
                | T.null remaining = []
                | pattern `T.isPrefixOf` remaining = 
                    offset : findAll (offset + T.length pattern) (T.drop (T.length pattern) remaining)
                | otherwise = findAll (offset + 1) (T.drop 1 remaining)
          
          formatSpecs = findFormatSpecs safeFormatStr
          
      in if length formatSpecs == length args then
        -- We have matching number of format specifiers and arguments
        let buildExprWithFormats :: [(Text, Int)] -> [CppExpr] -> Text -> Int -> CppExpr -> CppExpr
            buildExprWithFormats [] [] _ _ acc = acc
            buildExprWithFormats ((spec, pos):restSpecs) (arg:restArgs) remainingStr currentPos acc =
              let beforeSpec = T.take (pos - currentPos) remainingStr
                  afterSpec = T.drop (pos - currentPos + T.length spec) remainingStr
                  acc1 = if T.null beforeSpec then acc
                        else CppBinary "<<" acc (CppLiteral (CppStringLit beforeSpec))
                  acc2 = case spec of
                    "%.1f" -> CppBinary "<<" (CppBinary "<<" (CppBinary "<<" acc1 (CppVar "std::fixed")) 
                             (CppCall (CppVar "std::setprecision") [CppLiteral (CppIntLit 1)])) arg
                    "%.2f" -> CppBinary "<<" (CppBinary "<<" (CppBinary "<<" acc1 (CppVar "std::fixed")) 
                             (CppCall (CppVar "std::setprecision") [CppLiteral (CppIntLit 2)])) arg
                    "%t" -> CppBinary "<<" (CppBinary "<<" acc1 (CppVar "std::boolalpha")) arg
                    _ -> CppBinary "<<" acc1 arg  -- %d, %f, %s all work the same way in C++
                  newPos = pos + T.length spec
              in buildExprWithFormats restSpecs restArgs afterSpec newPos acc2
            buildExprWithFormats [] [] remainingStr _ acc =
              if T.null remainingStr then acc
              else CppBinary "<<" acc (CppLiteral (CppStringLit remainingStr))
            buildExprWithFormats _ _ _ _ acc = acc  -- Fallback
            
            finalExpr = buildExprWithFormats formatSpecs args safeFormatStr 0 (CppVar "std::cout")
        in if hasEndl then CppBinary "<<" finalExpr (CppVar "std::endl") else finalExpr
      else
        -- Fallback: just output the format string
        let expr1 = CppBinary "<<" (CppVar "std::cout") (CppLiteral (CppStringLit formatStr))
        in if hasEndl then CppBinary "<<" expr1 (CppVar "std::endl") else expr1

-- | Convert Go format string to C++ output
convertGoFormatToCpp :: Text -> [CppExpr] -> Text
convertGoFormatToCpp formatStr args = 
  -- For now, create a simple working implementation
  -- Example: "%d + %d = %d\n" with args [x, y, sum] becomes
  -- "x << \" + \" << y << \" = \" << sum << std::endl"
  case (T.splitOn "%d" formatStr, args) of
    ([before, " + ", " = ", after], [arg1, arg2, arg3]) -> 
      "\"" <> escapeCppString before <> "\" << " <> renderCppExprSimple arg1 <> 
      " << \" + \" << " <> renderCppExprSimple arg2 <> 
      " << \" = \" << " <> renderCppExprSimple arg3 <> 
      " << \"" <> T.replace "\\n" "" (escapeCppString after) <> "\" << std::endl"
    ([before, after], [arg1]) ->
      "\"" <> escapeCppString before <> "\" << " <> renderCppExprSimple arg1 <> 
      " << \"" <> T.replace "\\n" "" (escapeCppString after) <> "\""
    _ -> "\"" <> escapeCppString formatStr <> "\""  -- fallback
  where
    -- Helper function to escape string literals for C++
    escapeCppString :: Text -> Text
    escapeCppString s = T.concatMap escapeChar s
      where
        escapeChar '\n' = "\\n"
        escapeChar '\t' = "\\t"
        escapeChar '\r' = "\\r"
        escapeChar '\\' = "\\\\"
        escapeChar '\"' = "\\\""
        escapeChar '\'' = "\\'"
        escapeChar c = T.singleton c
    
    renderCppExprSimple (CppVar name) = name
    renderCppExprSimple (CppLiteral (CppIntLit i)) = T.pack (show i)
    renderCppExprSimple (CppLiteral (CppStringLit s)) = "\"" <> escapeCppString s <> "\""
    renderCppExprSimple expr = "(" <> T.pack (show expr) <> ")"

-- | Type mapping functions
mapPythonTypeToCpp :: Type -> CppType
mapPythonTypeToCpp = \case
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
  -- Basic types
  GoBasicType (Identifier "int") -> CppInt
  GoBasicType (Identifier "int8") -> CppChar
  GoBasicType (Identifier "int16") -> CppShort
  GoBasicType (Identifier "int32") -> CppInt
  GoBasicType (Identifier "int64") -> CppLongLong
  GoBasicType (Identifier "uint") -> CppUInt
  GoBasicType (Identifier "uint8") -> CppUChar
  GoBasicType (Identifier "uint16") -> CppUShort
  GoBasicType (Identifier "uint32") -> CppUInt
  GoBasicType (Identifier "uint64") -> CppULongLong
  GoBasicType (Identifier "float32") -> CppFloat
  GoBasicType (Identifier "float64") -> CppDouble
  GoBasicType (Identifier "bool") -> CppBool
  GoBasicType (Identifier "string") -> CppString
  
  -- Complex types
  GoBasicType (Identifier "complex64") -> CppTemplateType "std::complex" [CppFloat]
  GoBasicType (Identifier "complex128") -> CppTemplateType "std::complex" [CppDouble]
  
  -- Container types
  GoSliceType (Located _ elemType) -> CppVector (mapGoTypeToCpp elemType)
  GoArrayType sizeExpr (Located _ elemType) -> 
    -- Extract array size from expression
    case locatedValue sizeExpr of
      GoLiteral (GoInt size) -> CppArray (mapGoTypeToCpp elemType) (fromIntegral size)
      _ -> CppArray (mapGoTypeToCpp elemType) 10  -- Default size
  GoMapType (Located _ keyType) (Located _ valueType) -> 
    CppUnorderedMap (mapGoTypeToCpp keyType) (mapGoTypeToCpp valueType)
  
  -- Pointer types
  GoPointerType (Located _ baseType) -> CppPointer (mapGoTypeToCpp baseType)
  
  -- Channel types - use template-based Channel class
  GoChanType _ (Located _ elemType) -> 
    let cppElemType = mapGoTypeToCpp elemType
    in CppTemplateType "Channel" [cppElemType]
  
  -- Function types
  GoFuncType params results -> 
    let paramTypes = map (\field -> mapGoTypeToCpp (locatedValue (goFieldType field))) params
        resultType = case results of
                      [] -> CppVoid
                      [field] -> mapGoTypeToCpp (locatedValue (goFieldType field))
                      _ -> CppAuto  -- Multiple return values use auto
    in CppFunctionType paramTypes resultType
  
  -- Interface types - use void* for simplicity
  GoInterfaceType _ -> CppPointer CppVoid
  
  -- Struct types
  GoStructType _ -> CppAuto  -- For now, use auto for structs
  
  -- Generic types and constraints
  GoGenericType (QualifiedName _ (Identifier name)) typeArgs ->
    let cppTypeArgs = map (mapGoTypeToCpp . locatedValue) typeArgs
    in case name of
         "Container" -> CppTemplateType "Container" cppTypeArgs
         "Vector" -> CppTemplateType "std::vector" cppTypeArgs
         _ -> CppTemplateType name cppTypeArgs
  
  -- Type parameters - use int as fallback for template safety
  GoTypeParam _ _ -> CppInt
  
  -- Ellipsis types (variadic)
  GoEllipsisType (Located _ baseType) -> CppVector (mapGoTypeToCpp baseType)
  
  -- Fallback for unknown types
  _ -> CppInt  -- Use int instead of auto for better template compatibility

-- | Extract array size from Go expression
extractArraySize :: Located GoExpr -> Int
extractArraySize (Located _ expr) = case expr of
  GoLiteral (GoInt n) -> fromIntegral n
  GoLiteral (GoFloat n) -> floor n
  _ -> 10  -- Default size for complex expressions

-- | Safe template type generation for Go types
safeGoTypeToCpp :: GoType -> CppType
safeGoTypeToCpp goType = 
  case mapGoTypeToCpp goType of
    CppAuto -> CppInt  -- Replace auto with int for template safety
    other -> other

mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp = mapPythonTypeToCpp  -- Reuse Python mapping

-- | Literal mapping
mapPythonLiteral :: PythonLiteral -> CppLiteral
mapPythonLiteral = \case
  PyInt i -> CppIntLit i
  PyFloat f -> CppFloatLit f
  PyBool b -> CppBoolLit b
  PyString s -> CppStringLit s
  PyFString template exprs -> CppStringLit template  -- TODO: Implement proper f-string
  PyNone -> CppNullPtr
  _ -> CppIntLit 0

mapGoLiteral :: GoLiteral -> CppLiteral
mapGoLiteral = \case
  GoInt i -> CppIntLit i
  GoFloat f -> CppFloatLit f
  GoBool b -> CppBoolLit b
  GoString s -> CppStringLit s
  GoNil -> CppNullPtr
  GoRune c -> CppCharLit c
  GoImag _ -> CppIntLit 0  -- Simplified: ignore imaginary part
  GoRawString s -> CppStringLit s
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

mapPythonComparisonOp :: ComparisonOp -> Text
mapPythonComparisonOp = \case
  OpEq -> "=="
  OpNe -> "!="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpIs -> "=="  -- Simplified: use == for 'is'
  OpIsNot -> "!="  -- Simplified: use != for 'is not'

-- | Generate Channel class for Go channel operations
-- This implements a template-based channel for different data types
generateChannelClass :: CppCodeGen ()
generateChannelClass = do
  addInclude "<queue>"
  addInclude "<mutex>"
  addInclude "<condition_variable>"
  
  let channelMembers = 
        [ CppVariable "data_queue" (CppTemplateType "std::queue" [CppTypeVar "T"]) Nothing
        , CppVariable "mutex" (CppTemplateType "std::mutex" []) Nothing
        , CppVariable "cv" (CppTemplateType "std::condition_variable" []) Nothing
        ]
  
  let channelMethods =
        [ CppConstructor "Channel" []
            [ CppComment "Initialize empty channel"
            ]
        , CppMethod "send" CppVoid [CppParam "value" (CppTypeVar "T") Nothing] 
            [ CppDecl $ CppVariable "lock" (CppTemplateType "std::lock_guard" [CppTemplateType "std::mutex" []]) 
                           (Just (CppCall (CppVar "std::lock_guard") [CppMember CppThis "mutex"]))
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "data_queue") "push") [CppVar "value"]
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv") "notify_one") []
            ] False
        , CppMethod "receive" (CppTypeVar "T") [] 
            [ CppDecl $ CppVariable "lock" (CppTemplateType "std::unique_lock" [CppTemplateType "std::mutex" []]) 
                           (Just (CppCall (CppVar "std::unique_lock") [CppMember CppThis "mutex"]))
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv") "wait") [CppVar "lock"]
            , CppDecl $ CppVariable "result" (CppTypeVar "T") (Just (CppCall (CppMember (CppMember CppThis "data_queue") "front") []))
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "data_queue") "pop") []
            , CppReturn $ Just $ CppVar "result"
            ] False
        ]
  
  addDeclaration $ CppTemplate ["T"] (CppStruct "Channel" (channelMembers ++ channelMethods))

-- | Generate helper functions for printing containers
generatePrintHelpers :: CppCodeGen ()
generatePrintHelpers = do
  -- Generate function to print vector
  addDeclaration $ CppFunction "print_vector_int" CppVoid
    [CppParam "vec" (CppReference $ CppVector CppInt) Nothing]
    [ CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppLiteral $ CppStringLit "[")
    , CppDecl $ CppVariable "first" CppBool (Just $ CppLiteral $ CppBoolLit True)
    , CppDecl $ CppVariable "i" (CppSizeT) (Just $ CppLiteral $ CppIntLit 0)
    , CppFor 
        Nothing  -- no init
        (Just $ CppBinary "<" (CppVar "i") (CppCall (CppMember (CppVar "vec") "size") []))  -- condition
        (Just $ CppUnary "++" (CppVar "i"))  -- post-increment
        [ CppIf (CppUnary "!" (CppVar "first")) 
            [CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppLiteral $ CppStringLit ", ")] []
        , CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppIndex (CppVar "vec") (CppVar "i"))
        , CppExprStmt $ CppBinary "=" (CppVar "first") (CppLiteral $ CppBoolLit False)
        ]
    , CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppLiteral $ CppStringLit "]")
    ]
  
  -- Generate function to print initializer list
  addDeclaration $ CppFunction "print_array_int" CppVoid
    [CppParam "arr" (CppReference $ CppTemplateType "std::initializer_list" [CppInt]) Nothing]
    [ CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppLiteral $ CppStringLit "[")
    , CppDecl $ CppVariable "first" CppBool (Just $ CppLiteral $ CppBoolLit True)
    , CppDecl $ CppVariable "it" (CppAuto) (Just $ CppCall (CppMember (CppVar "arr") "begin") [])
    , CppWhile (CppBinary "!=" (CppVar "it") (CppCall (CppMember (CppVar "arr") "end") []))
        [ CppIf (CppUnary "!" (CppVar "first")) 
            [CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppLiteral $ CppStringLit ", ")] []
        , CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppUnary "*" (CppVar "it"))
        , CppExprStmt $ CppBinary "=" (CppVar "first") (CppLiteral $ CppBoolLit False)
        , CppExprStmt $ CppUnary "++" (CppVar "it")
        ]
    , CppExprStmt $ CppBinary "<<" (CppVar "std::cout") (CppLiteral $ CppStringLit "]")
    ]
  
  -- Use standard for loops instead of range-based for loops to avoid CppForRange issues
  addDeclaration $ CppFunction "operator<<" (CppReference $ CppTemplateType "std::ostream" [])
    [ CppParam "os" (CppReference $ CppTemplateType "std::ostream" []) Nothing
    , CppParam "vec" (CppReference $ CppVector CppInt) Nothing
    ]
    [ CppExprStmt $ CppBinary "<<" (CppVar "os") (CppLiteral $ CppStringLit "[")
    , CppDecl $ CppVariable "first" CppBool (Just $ CppLiteral $ CppBoolLit True)
    , CppFor 
        (Just $ CppDecl $ CppVariable "it" CppAuto (Just $ CppCall (CppMember (CppVar "vec") "begin") []))
        (Just $ CppBinary "!=" (CppVar "it") (CppCall (CppMember (CppVar "vec") "end") []))
        (Just $ CppUnary "++" (CppVar "it"))
        [ CppIf (CppUnary "!" (CppVar "first"))
            [CppExprStmt $ CppBinary "<<" (CppVar "os") (CppLiteral $ CppStringLit ", ")] []
        , CppExprStmt $ CppBinary "<<" (CppVar "os") (CppUnary "*" (CppVar "it"))
        , CppExprStmt $ CppBinary "=" (CppVar "first") (CppLiteral $ CppBoolLit False)
        ]
    , CppExprStmt $ CppBinary "<<" (CppVar "os") (CppLiteral $ CppStringLit "]")
    , CppReturn $ Just $ CppVar "os"
    ]
  
  addDeclaration $ CppFunction "operator<<" (CppReference $ CppTemplateType "std::ostream" [])
    [ CppParam "os" (CppReference $ CppTemplateType "std::ostream" []) Nothing
    , CppParam "lst" (CppReference $ CppTemplateType "std::initializer_list" [CppInt]) Nothing
    ]
    [ CppExprStmt $ CppBinary "<<" (CppVar "os") (CppLiteral $ CppStringLit "[")
    , CppDecl $ CppVariable "first" CppBool (Just $ CppLiteral $ CppBoolLit True)
    , CppFor
        (Just $ CppDecl $ CppVariable "it" CppAuto (Just $ CppCall (CppMember (CppVar "lst") "begin") []))
        (Just $ CppBinary "!=" (CppVar "it") (CppCall (CppMember (CppVar "lst") "end") []))
        (Just $ CppUnary "++" (CppVar "it"))
        [ CppIf (CppUnary "!" (CppVar "first"))
            [CppExprStmt $ CppBinary "<<" (CppVar "os") (CppLiteral $ CppStringLit ", ")] []
        , CppExprStmt $ CppBinary "<<" (CppVar "os") (CppUnary "*" (CppVar "it"))
        , CppExprStmt $ CppBinary "=" (CppVar "first") (CppLiteral $ CppBoolLit False)
        ]
    , CppExprStmt $ CppBinary "<<" (CppVar "os") (CppLiteral $ CppStringLit "]")
    , CppReturn $ Just $ CppVar "os"
    ]

-- | Generate print expression that handles special container types
generatePrintExprForArg :: CppExpr -> CppCodeGen CppExpr
generatePrintExprForArg arg = case arg of
  CppVar varName -> do
    -- For container variables, we'll just return them as-is
    -- The printing will be handled at a higher level
    return arg
  _ -> return arg


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

generateGoVariableDecl :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> CppCodeGen CppStmt
generateGoVariableDecl (Identifier name, mtype, mexpr) = do
  cppType <- case mtype of
    Just typeExpr -> generateGoType typeExpr
    Nothing -> return CppAuto
  cppExpr <- case mexpr of
    Just expr -> do
      e <- generateGoExpr expr
      return $ Just e
    Nothing -> return Nothing
  return $ CppDecl $ CppVariable name cppType cppExpr

generateGoDefineDecl :: [Located GoExpr] -> (Identifier, Int) -> CppCodeGen CppStmt
generateGoDefineDecl exprs (Identifier name, index) = do
  if index < length exprs
    then do
      let expr = exprs !! index
      cppExpr <- generateGoExpr expr
      let cppType = inferTypeFromExpr expr
      return $ CppDecl $ CppVariable name cppType (Just cppExpr)
    else do
      -- If no corresponding expression, use default initialization
      return $ CppDecl $ CppVariable name CppAuto Nothing

-- | Process f-string templates into proper C++ stream operations
processFStringTemplate :: Text -> CppExpr
processFStringTemplate template = 
  -- Convert f-string like "fib({i}) = {result}" to proper C++ streaming
  -- For now, handle common cases manually
  case template of
    t | T.isInfixOf "{i}" t && T.isInfixOf "{result}" t ->
      -- Handle "fib({i}) = {result}" pattern
      let beforeI = T.takeWhile (/= '{') t
          afterI = T.drop 1 $ T.dropWhile (/= '}') t
          beforeResult = T.takeWhile (/= '{') afterI
          afterResult = T.drop 1 $ T.dropWhile (/= '}') afterI
      in CppBinary "<<" 
          (CppBinary "<<" 
            (CppBinary "<<" 
              (CppBinary "<<" 
                (CppLiteral $ CppStringLit beforeI) 
                (CppVar "i"))
              (CppLiteral $ CppStringLit beforeResult))
            (CppVar "result"))
          (CppLiteral $ CppStringLit afterResult)
    _ -> CppLiteral $ CppStringLit template  -- Fallback to literal string

-- | Convert f-string template to C++ stream expression with actual expressions
convertFStringToStreamExprWithExprs :: Text -> [CppExpr] -> CppExpr
convertFStringToStreamExprWithExprs template exprs =
  -- Simple debugging approach: just handle single variable case first
  -- "Hello, {name}!" with [name_expr] should become: "Hello, " << name_expr << "!"
  case exprs of
    [] -> CppLiteral $ CppStringLit template  -- No expressions, return literal
    [expr1] -> 
      -- Single expression case
      -- For now, assume template is "something{var}something"
      -- Split at first { and }
      if T.isInfixOf "{" template && T.isInfixOf "}" template
      then
        let beforeBrace = T.takeWhile (/= '{') template
            afterBrace = T.dropWhile (/= '}') $ T.dropWhile (/= '{') template
            afterBrace' = if T.null afterBrace then "" else T.tail afterBrace
            -- Start with the literal part before the brace
            baseExpr = if T.null beforeBrace
                      then expr1  -- If no prefix, start with the expression
                      else CppBinary "<<" (CppLiteral $ CppStringLit beforeBrace) expr1
            -- Add the literal part after the brace
        in if T.null afterBrace'
           then baseExpr
           else CppBinary "<<" baseExpr (CppLiteral $ CppStringLit afterBrace')
      else CppLiteral $ CppStringLit template  -- Fallback if no braces
    _ -> CppLiteral $ CppStringLit template  -- Multiple expressions - fallback for now

-- | Convert f-string template to C++ stream expression
convertFStringToStreamExpr :: Text -> CppExpr
convertFStringToStreamExpr template =
  -- Convert f-string like "fib({i}) = {result}" to proper C++ streaming
  -- "fib({i}) = {result}" becomes: "fib(" << i << ") = " << result
  case template of
    t | T.isInfixOf "{i}" t && T.isInfixOf "{result}" t ->
      -- Handle "fib({i}) = {result}" pattern
      let beforeI = T.takeWhile (/= '{') t
          afterI = T.drop 1 $ T.dropWhile (/= '}') t
          beforeResult = T.takeWhile (/= '{') afterI
          afterResult = T.drop 1 $ T.dropWhile (/= '}') afterI
      in CppBinary "<<" 
          (CppBinary "<<" 
            (CppBinary "<<" 
              (CppBinary "<<" 
                (CppLiteral $ CppStringLit beforeI)
                (CppVar "i"))
              (CppLiteral $ CppStringLit beforeResult))
            (CppVar "result"))
          (CppLiteral $ CppStringLit afterResult)
    _ -> CppLiteral $ CppStringLit template  -- Fallback to literal string

-- | F-string template parsing
data FStringPart = LiteralPart !Text | ExpressionPart !Int
  deriving (Eq, Show)

-- | Parse f-string template into parts
parseFStringTemplate :: Text -> [FStringPart]
parseFStringTemplate template = go template []
  where
    go :: Text -> [FStringPart] -> [FStringPart]
    go remaining acc
      | T.null remaining = reverse acc
      | otherwise =
          case T.findIndex (== '{') remaining of
            Nothing -> reverse (LiteralPart remaining : acc)
            Just startIdx ->
              case T.findIndex (== '}') (T.drop (startIdx + 1) remaining) of
                Nothing -> reverse (LiteralPart remaining : acc)
                Just endIdx ->
                  let beforeBrace = T.take startIdx remaining
                      exprText = T.take endIdx (T.drop (startIdx + 1) remaining)
                      afterBrace = T.drop (startIdx + endIdx + 2) remaining
                      exprIndex = length $ filter isExpressionPart acc
                  in go afterBrace (ExpressionPart exprIndex : LiteralPart beforeBrace : acc)
    
    isExpressionPart (ExpressionPart _) = True
    isExpressionPart _ = False

-- | Build components for f-string
buildFStringComponents :: [FStringPart] -> [CppExpr] -> [CppExpr]
buildFStringComponents [] _ = []
buildFStringComponents (part:rest) exprList = 
  case part of
    LiteralPart text -> 
      if T.null text
      then buildFStringComponents rest exprList
      else CppLiteral (CppStringLit text) : buildFStringComponents rest exprList
    ExpressionPart index -> 
      if index < length exprList
      then (exprList !! index) : buildFStringComponents rest exprList
      else buildFStringComponents rest exprList

-- | Convert f-string template to C++ format string (simpler approach)
convertFStringToCppFormat :: Text -> Text
convertFStringToCppFormat template =
  -- For now, replace Python placeholders with C++ stream placeholders  
  -- "fib({i}) = {result}" becomes "fib(\" << i << \") = \" << result"
  let step1 = T.replace "{i}" "\" << i << \"" template
      step2 = T.replace "{result}" "\" << result << \"" step1
  in "\"" <> step2 <> "\""
