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
import Data.List (intercalate, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go
import Fluxus.Utils.Pretty
import Fluxus.Parser.Python.Lexer (runPythonLexer, PythonToken(..))
import Fluxus.Parser.Python.Parser (parseExpression)

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
  { cgsIncludes        :: ![Text]                          -- Required includes
  , cgsDeclarations    :: ![CppDecl]                       -- Generated declarations
  , cgsNamespaces      :: ![Text]                          -- Current namespace stack
  , cgsTempVarCount    :: !Int                             -- Temporary variable counter
  , cgsSymbolTable     :: !(HashMap Text CppType)          -- Symbol to type mapping
  , cgsHoistedGlobals  :: ![Text]                          -- Hoisted module-level globals (in encounter order)
  , cgsConfig          :: !CppGenConfig                    -- Configuration
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
  | CppAccessSpec !Text                                 -- access specifier (public/protected/private)
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
  | CppStmtSeq ![CppStmt]
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
  , cgsHoistedGlobals = []
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
  
  -- Generate module namespace (reserved for future use)
  let _moduleName = maybe "main" (\(ModuleName n) -> n) (pyModuleName pyModule)
  
  -- Process module body sequentially, capturing declarations and runtime statements
  moduleStmtsCpp <- mapM (generatePythonStmt ScopeModule) (pyModuleBody pyModule)
  -- Filter out comment statements that don't generate actual code
  let isActualStatement (CppComment _) = False
      isActualStatement _ = True
      actualStmts = filter isActualStatement moduleStmtsCpp
      nameBinding = CppDecl (CppVariable "__name__" (CppConst CppString) (Just (CppLiteral (CppStringLit "__main__"))))
      preludeStmts = if null actualStmts then [] else [nameBinding]
      bodyStmts = preludeStmts ++ actualStmts
  
  -- Ensure we have a main function for standalone execution
  hasMain <- gets (any isMainFunction . cgsDeclarations)
  unless hasMain $ do
    -- If we have module-level statements, wrap them in main function
    let mainBody = if null bodyStmts
                   then [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                   else bodyStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
    addDeclaration $ CppFunction "main" CppInt [] mainBody
  
  -- Build final unit
  includes <- gets cgsIncludes
  decls <- gets cgsDeclarations
  hoistedNames <- gets cgsHoistedGlobals
  
  let reversedDecls = reverse decls
      (hoistedDecls, remainingDecls) = extractHoisted hoistedNames reversedDecls
      finalDecls = hoistedDecls ++ remainingDecls
  
  return $ CppUnit includes [] finalDecls
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False

    extractHoisted :: [Text] -> [CppDecl] -> ([CppDecl], [CppDecl])
    extractHoisted [] ds = ([], ds)
    extractHoisted (name:names) ds =
      let (match, rest) = removeFirst (declMatches name) ds
          (more, remaining) = extractHoisted names rest
      in case match of
           Just decl -> (decl : more, remaining)
           Nothing -> (more, remaining)

    declMatches :: Text -> CppDecl -> Bool
    declMatches target (CppVariable name _ _) = name == target
    declMatches _ _ = False

    removeFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
    removeFirst _ [] = (Nothing, [])
    removeFirst p (x:xs)
      | p x = (Just x, xs)
      | otherwise =
          let (match, rest) = removeFirst p xs
          in (match, x : rest)

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

data RangeSpec = RangeSpec
  { rsStart :: CppExpr
  , rsEnd :: CppExpr
  , rsStep :: CppExpr
  , rsStepValue :: Maybe Integer
  }

data PythonScope
  = ScopeModule
  | ScopeFunction
  deriving (Eq, Show)

-- | Generate C++ from Python statements
generatePythonStmt :: PythonScope -> Located PythonStmt -> CppCodeGen CppStmt
generatePythonStmt scope (Located _ stmt) =
  case stmt of
    PyFuncDef funcDef -> do
      generatePythonFunction funcDef
      return $ CppComment "Function definition processed"
    PyClassDef classDef -> do
      generatePythonClass classDef
      return $ CppComment "Class definition processed"
    PyExprStmt expr -> do
      cppExpr <- generatePythonExpr expr
      return $ CppExprStmt cppExpr
    PyAssign patterns expr@(Located _ exprVal) ->
      case patterns of
        [Located _ (PatVar (Identifier varName))] ->
          handleSimpleAssignment scope varName expr exprVal
        _ ->
          -- Multiple assignment - not fully implemented
          return $ CppComment "Multiple assignment not implemented"
    PyReturn mexpr -> do
      mcppExpr <- mapM generatePythonExpr mexpr
      return $ CppReturn mcppExpr
    PyIf condition thenStmts elseStmts -> do
      cppCond <- generatePythonExpr condition
      cppThen <- mapM (generatePythonStmt scope) thenStmts
      cppElse <- mapM (generatePythonStmt scope) elseStmts
      return $ CppIf cppCond cppThen cppElse
    PyWhile condition bodyStmts _ -> do
      cppCond <- generatePythonExpr condition
      cppBody <- mapM (generatePythonStmt scope) bodyStmts
      return $ CppWhile cppCond cppBody
    PyFor (Located _ (PatVar (Identifier varName))) iterExpr bodyStmts _ -> do
      symtab <- gets cgsSymbolTable
      let varAlreadyDeclared = HM.member varName symtab
      unless varAlreadyDeclared $
        modify $ \r -> r { cgsSymbolTable = HM.insert varName CppAuto (cgsSymbolTable r) }
      cppBody <- mapM (generatePythonStmt scope) bodyStmts
      case locatedValue iterExpr of
        PyCall (Located _ (PyVar (Identifier "range"))) rangeArgs -> do
          mSpec <- parseRangeArgs rangeArgs
          case mSpec of
            Nothing -> return $ CppComment "range() with unsupported arguments"
            Just spec -> buildRangeLoop varAlreadyDeclared varName cppBody spec
        _ -> do
          addComment "Only range() iteration is currently supported"
          return $ CppComment "For loop not fully implemented"
    _ ->
      return $ CppComment $ "TODO: Implement Python statement: " <> T.pack (show stmt)
  where
    handleSimpleAssignment :: PythonScope -> Text -> Located PythonExpr -> PythonExpr -> CppCodeGen CppStmt
    handleSimpleAssignment scope' varName locatedExpr exprVal = do
      symtab <- gets cgsSymbolTable
      case exprVal of
        PyList elems ->
          handleListAssignment scope' symtab varName elems
        _ ->
          handleRegularAssignment scope' symtab varName locatedExpr

    handleListAssignment :: PythonScope -> HashMap Text CppType -> Text -> [Located PythonExpr] -> CppCodeGen CppStmt
    handleListAssignment scope' symtab varName elems = do
      (vectorType, vectorExpr) <- generatePythonListLiteral elems
      let updateSymbolTable = modify $ \s -> s { cgsSymbolTable = HM.insert varName vectorType (cgsSymbolTable s) }
          assignmentExpr = CppExprStmt (CppBinary "=" (CppVar varName) vectorExpr)
          declarationStmt = CppDecl (CppVariable varName vectorType (Just vectorExpr))
      case scope' of
        ScopeModule ->
          if HM.member varName symtab
            then do
              updateSymbolTable
              return assignmentExpr
            else do
              updateSymbolTable
              recordHoistedGlobal varName
              addDeclaration $ CppVariable varName vectorType (Just vectorExpr)
              return $ CppComment $ "Initialized module-level list " <> varName
        ScopeFunction ->
          if HM.member varName symtab
            then do
              updateSymbolTable
              return assignmentExpr
            else do
              updateSymbolTable
              return declarationStmt

    handleRegularAssignment :: PythonScope -> HashMap Text CppType -> Text -> Located PythonExpr -> CppCodeGen CppStmt
    handleRegularAssignment scope' symtab varName locatedExpr = do
      cppExpr <- generatePythonExpr locatedExpr
      let inferredType = fromMaybe CppAuto (inferPythonExprCppTypeLocated locatedExpr)
          updateSymbolTableWith t = modify $ \s -> s { cgsSymbolTable = HM.insert varName t (cgsSymbolTable s) }
          assignmentStmt = CppExprStmt (CppBinary "=" (CppVar varName) cppExpr)
          declarationStmt t = CppDecl (CppVariable varName t (Just cppExpr))
      case scope' of
        ScopeModule ->
          if HM.member varName symtab
            then do
              updateSymbolTableWith inferredType
              return assignmentStmt
            else do
              updateSymbolTableWith inferredType
              recordHoistedGlobal varName
              addDeclaration $ CppVariable varName inferredType (Just cppExpr)
              return $ CppComment $ "Initialized module-level variable " <> varName
        ScopeFunction ->
          if HM.member varName symtab
            then do
              updateSymbolTableWith inferredType
              return assignmentStmt
            else do
              updateSymbolTableWith inferredType
              return $ declarationStmt inferredType

    parseRangeArgs :: [Located PythonArgument] -> CppCodeGen (Maybe RangeSpec)
    parseRangeArgs args = case traverse extractPositional args of
      Nothing -> return Nothing
      Just posArgs -> case posArgs of
        [endExpr] -> do
          end <- generatePythonExpr endExpr
          let start = CppLiteral (CppIntLit 0)
              step = CppLiteral (CppIntLit 1)
          return $ Just RangeSpec
            { rsStart = start
            , rsEnd = end
            , rsStep = step
            , rsStepValue = Just 1
            }
        [startExpr, endExpr] -> do
          start <- generatePythonExpr startExpr
          end <- generatePythonExpr endExpr
          let step = CppLiteral (CppIntLit 1)
          return $ Just RangeSpec
            { rsStart = start
            , rsEnd = end
            , rsStep = step
            , rsStepValue = Just 1
            }
        [startExpr, endExpr, stepExpr] -> do
          start <- generatePythonExpr startExpr
          end <- generatePythonExpr endExpr
          stepRaw <- generatePythonExpr stepExpr
          let (stepNorm, stepVal) = normalizeStepExpr stepRaw
          return $ Just RangeSpec
            { rsStart = start
            , rsEnd = end
            , rsStep = stepNorm
            , rsStepValue = stepVal
            }
        _ -> return Nothing

    extractPositional :: Located PythonArgument -> Maybe (Located PythonExpr)
    extractPositional (Located _ arg) = case arg of
      ArgPositional expr -> Just expr
      _ -> Nothing

    normalizeStepExpr :: CppExpr -> (CppExpr, Maybe Integer)
    normalizeStepExpr expr = case expr of
      CppLiteral (CppIntLit n) -> (CppLiteral (CppIntLit n), Just n)
      CppUnary "-" inner -> case inner of
        CppLiteral (CppIntLit n) -> (CppLiteral (CppIntLit (-n)), Just (-n))
        _ -> (expr, Nothing)
      _ -> (expr, Nothing)

    buildRangeLoop :: Bool -> Text -> [CppStmt] -> RangeSpec -> CppCodeGen CppStmt
    buildRangeLoop varAlreadyDeclared varName cppBody (RangeSpec startExpr endExpr stepExpr stepVal) = do
      iterVarName <- generateTempVar
      let iterVar = CppVar iterVarName
          pythonVar = CppVar varName
          assignmentStmt = CppExprStmt (CppBinary "=" pythonVar iterVar)
          loopBody = assignmentStmt : cppBody
          prefixDecls = if varAlreadyDeclared
                        then []
                        else [CppDecl (CppVariable varName CppAuto (Just startExpr))]
          iterInitExpr = if varAlreadyDeclared then startExpr else pythonVar
      case stepVal of
        Just 0 -> do
          addComment "range() with step 0 is invalid"
          return $ CppComment "range step 0 not supported"
        Just n -> do
          let condition = if n > 0
                            then CppBinary "<" iterVar endExpr
                            else CppBinary ">" iterVar endExpr
              increment
                | n == 1 = CppUnary "++" iterVar
                | n == -1 = CppUnary "--" iterVar
                | otherwise = CppBinary "+=" iterVar (CppLiteral (CppIntLit n))
              iterInit = CppDecl (CppVariable iterVarName CppAuto (Just iterInitExpr))
              loopStmt = CppFor (Just iterInit) (Just condition) (Just increment) loopBody
              stmts = prefixDecls ++ [loopStmt]
          return $ case stmts of
            [single] -> single
            _ -> CppStmtSeq stmts
        Nothing -> do
          stepVarName <- generateTempVar
          let stepVar = CppVar stepVarName
              stepDecl = CppDecl (CppVariable stepVarName CppAuto (Just stepExpr))
              iterInit = CppDecl (CppVariable iterVarName CppAuto (Just iterInitExpr))
              positiveCond = CppBinary "&&"
                               (CppBinary ">" stepVar (CppLiteral (CppIntLit 0)))
                               (CppBinary "<" iterVar endExpr)
              negativeCond = CppBinary "&&"
                               (CppBinary "<" stepVar (CppLiteral (CppIntLit 0)))
                               (CppBinary ">" iterVar endExpr)
              condition = CppBinary "||" positiveCond negativeCond
              increment = CppBinary "+=" iterVar stepVar
              loopStmt = CppFor (Just iterInit) (Just condition) (Just increment) loopBody
              scopedLoop = CppBlock [stepDecl, loopStmt]
              stmts = prefixDecls ++ [scopedLoop]
          return $ case stmts of
            [single] -> single
            _ -> CppStmtSeq stmts

-- | Generate C++ from Python expressions
-- | Generate C++ expression from Python argument
generatePythonArgument :: Located PythonArgument -> CppCodeGen CppExpr
generatePythonArgument (Located _ arg) = case arg of
  ArgPositional expr -> generatePythonExpr expr
  ArgKeyword _ expr -> generatePythonExpr expr  -- Simplified: ignore keyword
  ArgStarred expr -> generatePythonExpr expr  -- Simplified
  ArgKwStarred expr -> generatePythonExpr expr  -- Simplified

-- | Segments extracted from an f-string literal
data FStringSegment
  = FStringLiteral !Text
  | FStringExpression !Text
  deriving (Eq, Show)

splitFStringSegments :: Text -> Either Text [FStringSegment]
splitFStringSegments input = go input []
  where
    go txt acc =
      case T.uncons txt of
        Nothing -> Right (reverse acc)
        Just ('{', rest)
          | "{{" `T.isPrefixOf` txt -> go (T.drop 2 txt) (addLiteral "{" acc)
          | otherwise -> do
              (exprText, remainder) <- takeExpression rest
              let (exprCoreRaw, debugLiteral) = stripFormatSpec exprText
                  exprCore = T.strip exprCoreRaw
              when (T.null exprCore) $
                Left "Empty expression in f-string"
              let accWithDebug = maybe acc (\lit -> addLiteral lit acc) debugLiteral
              go remainder (FStringExpression exprCore : accWithDebug)
        Just ('}', _)
          | "}}" `T.isPrefixOf` txt -> go (T.drop 2 txt) (addLiteral "}" acc)
          | otherwise -> Left "Single '}' in f-string"
        _ ->
          let (literal, remainder) = T.break (`elem` ("{}" :: String)) txt
          in go remainder (addLiteral literal acc)

    addLiteral lit acc
      | T.null lit = acc
      | otherwise = case acc of
          (FStringLiteral existing : rest) -> FStringLiteral (existing <> lit) : rest
          _ -> FStringLiteral lit : acc

    takeExpression :: Text -> Either Text (Text, Text)
    takeExpression txt = goExpr 0 [] txt
      where
        goExpr depth acc remaining =
          case T.uncons remaining of
            Nothing -> Left "Unterminated '{' in f-string"
            Just ('{', rest') -> goExpr (depth + 1) ('{' : acc) rest'
            Just ('}', rest')
              | depth == 0 -> Right (T.pack (reverse acc), rest')
              | otherwise  -> goExpr (depth - 1) ('}' : acc) rest'
            Just (c, rest') -> goExpr depth (c : acc) rest'

    stripFormatSpec :: Text -> (Text, Maybe Text)
    stripFormatSpec txt =
      let (core, _) = breakOnFormat txt
          trimmed = T.strip core
      in case T.unsnoc trimmed of
           Just (rest, '=') ->
             let exprPart = T.strip rest
             in (exprPart, Just core)
           _ -> (trimmed, Nothing)

    breakOnFormat :: Text -> (Text, Text)
    breakOnFormat txt = goFmt 0 0 0 [] txt
      where
        goFmt braceDepth parenDepth bracketDepth acc remaining =
          case T.uncons remaining of
            Nothing -> (T.pack (reverse acc), T.empty)
            Just (c, rest')
              | c == '{' -> goFmt (braceDepth + 1) parenDepth bracketDepth (c : acc) rest'
              | c == '}' && braceDepth > 0 -> goFmt (braceDepth - 1) parenDepth bracketDepth (c : acc) rest'
              | c == '(' -> goFmt braceDepth (parenDepth + 1) bracketDepth (c : acc) rest'
              | c == ')' && parenDepth > 0 -> goFmt braceDepth (parenDepth - 1) bracketDepth (c : acc) rest'
              | c == '[' -> goFmt braceDepth parenDepth (bracketDepth + 1) (c : acc) rest'
              | c == ']' && bracketDepth > 0 -> goFmt braceDepth parenDepth (bracketDepth - 1) (c : acc) rest'
              | (c == ':' || c == '!') && braceDepth == 0 && parenDepth == 0 && bracketDepth == 0 ->
                  (T.pack (reverse acc), rest')
              | otherwise -> goFmt braceDepth parenDepth bracketDepth (c : acc) rest'

-- | Parse a small Python expression (used in f-strings)
parseInlinePythonExpr :: Text -> Either Text (Located PythonExpr)
parseInlinePythonExpr exprText = do
  let trimmed = T.strip exprText
  when (T.null trimmed) $
    Left "Empty expression"
  tokens <- first (T.pack . MP.errorBundlePretty) $
    runPythonLexer "<f-string>" trimmed
  let eofToken = Located syntheticSpan TokenEOF
      tokenStream = tokens ++ [eofToken]
  first (T.pack . MP.errorBundlePretty) $
    MP.parse (parseExpression <* MP.eof) "<f-string>" tokenStream

-- | Generate a C++ expression from a Python f-string literal
generateFStringExpr :: Text -> CppCodeGen CppExpr
generateFStringExpr raw = do
  segments <- case splitFStringSegments raw of
    Left err -> do
      addComment $ "Failed to parse f-string: " <> err
      return [FStringLiteral raw]
    Right segs -> return segs
  compiled <- mapM compileSegment segments
  ossName <- generateTempVar
  addInclude "<sstream>"
  let ossVar = CppVar ossName
      ossType = CppClassType "std::ostringstream" []
      ossDecl = CppDecl (CppVariable ossName ossType Nothing)
      streamStmts = map (\expr -> CppExprStmt (CppBinary "<<" ossVar expr)) compiled
      resultExpr = CppCall (CppMember ossVar "str") []
      lambdaBody = ossDecl : streamStmts ++ [CppReturn (Just resultExpr)]
  return $ CppCall (CppLambda [] lambdaBody) []
  where
    compileSegment (FStringLiteral lit) = return $ CppLiteral (CppStringLit lit)
    compileSegment (FStringExpression exprTxt) =
      case parseInlinePythonExpr exprTxt of
        Left err -> do
          addComment $ "Failed to parse f-string expression: " <> err
          return $ CppLiteral (CppStringLit ("{" <> exprTxt <> "}"))
        Right parsed -> generatePythonExpr parsed

syntheticSpan :: SourceSpan
syntheticSpan = SourceSpan "<f-string>" (SourcePos 0 0) (SourcePos 0 0)

generatePythonExpr :: Located PythonExpr -> CppCodeGen CppExpr
generatePythonExpr (Located _ expr) = case expr of
  PyLiteral lit -> case lit of
    PyFString text _ -> generateFStringExpr text
    _ -> return $ CppLiteral $ mapPythonLiteral lit
  PyConst (QualifiedName _ (Identifier name)) -> case name of
    "True"  -> return $ CppLiteral (CppBoolLit True)
    "False" -> return $ CppLiteral (CppBoolLit False)
    "None"  -> return $ CppLiteral CppNullPtr
    _        -> return $ CppVar name
  PyVar (Identifier name) -> return $ CppVar name
  PyBinaryOp op left right -> do
    cppLeft <- generatePythonExpr left
    cppRight <- generatePythonExpr right
    case op of
      OpPow -> do
        addInclude "<cmath>"
        return $ CppCall (CppVar "std::pow") [cppLeft, cppRight]
      _ -> do
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
  PyAttribute obj (Identifier member) -> do
    cppObj <- generatePythonExpr obj
    return $ CppMember cppObj member
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
    Nothing
      | funcName == "main" -> return CppInt
      | otherwise -> inferFunctionReturnType (pyFuncBody funcDef)
  
  -- Generate function body
  bodyStmts <- mapM (generatePythonStmt ScopeFunction) (pyFuncBody funcDef)
  
  -- Add return statement for main function if needed
  let finalBodyStmts = if funcName == "main" && returnType == CppInt
                      then bodyStmts ++ [CppReturn (Just (CppLiteral $ CppIntLit 0))]
                      else bodyStmts
  
  addDeclaration $ CppFunction funcName returnType cppParams finalBodyStmts

-- | Infer the appropriate C++ return type for a Python function when no annotation is provided
inferFunctionReturnType :: [Located PythonStmt] -> CppCodeGen CppType
inferFunctionReturnType body = do
  let returnExprs = collectReturnExprs body
  if null returnExprs
    then return CppVoid
    else do
      let inferredTypes = map inferPythonExprCppTypeLocated returnExprs
          knownTypes = catMaybes inferredTypes
          hasUnknown = length knownTypes /= length returnExprs
          combinedType = case knownTypes of
            [] -> Nothing
            (t:ts) -> foldM unifyElementType t ts
      case combinedType of
        Just t -> return t
        Nothing ->
          case nub knownTypes of
            [] ->
              if hasUnknown
                then return CppAuto
                else return CppVoid
            [single] -> return single
            multiple -> do
              addInclude "<variant>"
              return (CppVariant multiple)

-- | Collect return expressions that yield values from a list of Python statements
collectReturnExprs :: [Located PythonStmt] -> [Located PythonExpr]
collectReturnExprs = concatMap collectStmt
  where
    collectStmt :: Located PythonStmt -> [Located PythonExpr]
    collectStmt (Located _ stmt) = case stmt of
      PyReturn (Just expr) -> [expr]
      PyIf _ thenStmts elseStmts ->
        collectReturnExprs thenStmts ++ collectReturnExprs elseStmts
      PyWhile _ bodyStmts elseStmts ->
        collectReturnExprs bodyStmts ++ collectReturnExprs elseStmts
      PyFor _ _ bodyStmts elseStmts ->
        collectReturnExprs bodyStmts ++ collectReturnExprs elseStmts
      PyWith _ bodyStmts ->
        collectReturnExprs bodyStmts
      PyTry tryBlock excepts orelse finally ->
        collectReturnExprs tryBlock
        ++ concatMap (collectReturnExprs . pyExceptBody . locatedValue) excepts
        ++ collectReturnExprs orelse
        ++ collectReturnExprs finally
      PyAsyncWith _ bodyStmts ->
        collectReturnExprs bodyStmts
      PyAsyncFor _ _ bodyStmts elseStmts ->
        collectReturnExprs bodyStmts ++ collectReturnExprs elseStmts
      PyFuncDef _ -> []
      PyAsyncFuncDef _ -> []
      PyClassDef _ -> []
      _ -> []

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
      paramGroups <- mapM mapGoParameter (goFuncParams func)
      let cppParams = concat paramGroups
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
hasReturnStmt = any isReturnStmt . concatMap flatten
  where
    flatten stmt = case stmt of
      CppStmtSeq seqStmts -> concatMap flatten seqStmts
      CppBlock stmts -> concatMap flatten stmts
      other -> [other]

    isReturnStmt (CppReturn _) = True
    isReturnStmt _ = False



-- | Generate block statement from Go (handling compound statements)
generateGoBlockStmt :: Located GoStmt -> CppCodeGen [CppStmt]
generateGoBlockStmt located@(Located _ stmt) = case stmt of
  GoBlock stmts -> mapM generateGoStmt stmts
  _ -> do
    singleStmt <- generateGoStmt located
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
  GoIf mInit cond thenStmt elseStmt -> do
    cppCond <- generateGoExpr cond
    cppThen <- generateGoBlockStmt thenStmt
    cppElse <- case elseStmt of
      Nothing -> return []
      Just stmt -> generateGoBlockStmt stmt
    let ifStmt = CppIf cppCond cppThen cppElse
    case mInit of
      Nothing -> return ifStmt
      Just initStmt -> do
        initCpp <- generateGoStmt initStmt
        return $ CppBlock [initCpp, ifStmt]
  GoFor mforClause bodyStmt -> do
    case mforClause of
      Nothing -> do
        -- Infinite loop - convert to while(true)
        bodyStmts <- generateGoBlockStmt bodyStmt
        return $ CppWhile (CppLiteral $ CppBoolLit True) bodyStmts
      Just forClause -> do
        initStmt <- case goForInit forClause of
          Nothing -> return Nothing
          Just initS -> generateGoForInit initS
        condExpr <- case goForCond forClause of
          Nothing -> return Nothing
          Just condE -> Just <$> generateGoExpr condE
        postExpr <- case goForPost forClause of
          Nothing -> return Nothing
          Just postS -> generateGoForPost postS
        bodyStmts <- generateGoBlockStmt bodyStmt
        return $ CppFor initStmt condExpr postExpr bodyStmts
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
    if length identifiers /= length exprs
      then do
        addComment $ "Mismatched variable definition arity"
        return $ CppComment "Unsupported variable definition"
      else do
        cppExprs <- mapM generateGoExpr exprs
        let stmts = zipWith defineBinding identifiers cppExprs
        return $ wrapStmts stmts
  GoAssign leftExprs rightExprs -> do
    -- Handle assignment: x, y = a, b
    case (leftExprs, rightExprs) of
      ([leftExpr], [rightExpr]) -> do
        cppRight <- generateGoExpr rightExpr
        case leftExpr of
          Located _ (GoIdent (Identifier "_")) ->
            return $ CppExprStmt cppRight
          _ -> do
            cppLeft <- generateGoExpr leftExpr
            return $ CppExprStmt $ CppBinary "=" cppLeft cppRight
      _ ->
        if length leftExprs /= length rightExprs
          then do
            addComment $ "Mismatched assignment arity"
            return $ CppComment "Multiple assignment"
          else do
            prepResults <- mapM prepareAssignment (zip leftExprs rightExprs)
            let evalStmts = map fst prepResults
                assignmentInputs = catMaybes (map snd prepResults)
            assignStmts <- mapM buildAssignment assignmentInputs
            return $ wrapStmts (evalStmts ++ assignStmts)
  GoIncDec expr isIncrement -> do
    cppExpr <- generateGoExpr expr
    let op = if isIncrement then "++" else "--"
    return $ CppExprStmt $ CppUnary op cppExpr
  _ -> do
    addComment $ "TODO: Implement Go statement: " <> T.pack (show stmt)
    return $ CppComment $ "Unimplemented Go statement"
  where
    wrapStmts :: [CppStmt] -> CppStmt
    wrapStmts [] = CppStmtSeq []
    wrapStmts [single] = single
    wrapStmts stmts = CppStmtSeq stmts

    defineBinding :: Identifier -> CppExpr -> CppStmt
    defineBinding (Identifier name) expr
      | name == "_" = CppExprStmt expr
      | otherwise    = CppDecl $ CppVariable name CppAuto (Just expr)

    prepareAssignment :: (Located GoExpr, Located GoExpr) -> CppCodeGen (CppStmt, Maybe (Located GoExpr, CppExpr))
    prepareAssignment (leftExpr, rightExpr) = do
      rhsCpp <- generateGoExpr rightExpr
      case leftExpr of
        Located _ (GoIdent (Identifier "_")) ->
          return (CppExprStmt rhsCpp, Nothing)
        _ -> do
          tempName <- generateTempVar
          let tempVar = CppVar tempName
              declStmt = CppDecl (CppVariable tempName CppAuto (Just rhsCpp))
          return (declStmt, Just (leftExpr, tempVar))

    buildAssignment :: (Located GoExpr, CppExpr) -> CppCodeGen CppStmt
    buildAssignment (leftExpr, tempVarExpr) = do
      leftCpp <- generateGoExpr leftExpr
      return $ CppExprStmt (CppBinary "=" leftCpp tempVarExpr)

-- | Helpers for translating Go for-loop components
generateGoForInit :: Located GoStmt -> CppCodeGen (Maybe CppStmt)
generateGoForInit stmt@(Located _ inner) = case inner of
  GoDefine _ _ -> Just <$> generateGoStmt stmt
  GoAssign _ _ -> Just <$> generateGoStmt stmt
  GoExprStmt _ -> Just <$> generateGoStmt stmt
  GoIncDec _ _ -> Just <$> generateGoStmt stmt
  GoEmpty -> return Nothing
  _ -> do
    addComment $ "Unsupported for-loop initializer: " <> T.pack (show inner)
    return Nothing

generateGoForPost :: Located GoStmt -> CppCodeGen (Maybe CppExpr)
generateGoForPost (Located _ inner) = case inner of
  GoIncDec expr isInc -> do
    cppExpr <- generateGoExpr expr
    let op = if isInc then "++" else "--"
    return $ Just $ CppUnary op cppExpr
  GoAssign [leftExpr] [rightExpr] -> do
    cppLeft <- generateGoExpr leftExpr
    cppRight <- generateGoExpr rightExpr
    return $ Just $ CppBinary "=" cppLeft cppRight
  GoExprStmt expr -> Just <$> generateGoExpr expr
  GoEmpty -> return Nothing
  _ -> do
    addComment $ "Unsupported for-loop post statement: " <> T.pack (show inner)
    return Nothing

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
  GoComparison compOp left right -> do
    cppLeft <- generateGoExpr left
    cppRight <- generateGoExpr right
    let cppOp = mapComparisonOp compOp
    return $ CppBinary cppOp cppLeft cppRight
  GoUnaryOp uop inner -> do
    cppInner <- generateGoExpr inner
    let cppOp = case uop of
          OpNot -> "!"
          OpNegate -> "-"
          OpPositive -> "+"
          OpBitNot -> "~"
    return $ CppUnary cppOp cppInner
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
  GoQualifiedIdent (Identifier pkg) (Identifier name) ->
    return $ CppVar (pkg <> "::" <> name)
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
  OpFloorDiv -> "/"
  OpMod -> "%"
  OpBitAnd -> "&"
  OpBitOr -> "|"
  OpBitXor -> "^"
  OpShiftL -> "<<"
  OpShiftR -> ">>"
  OpAnd -> "&&"
  OpOr -> "||"
  OpConcat -> "+"
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
      queueType = CppTemplateType "std::queue" [templateParam]
      mutexType = CppClassType "std::mutex" []
      cvType = CppClassType "std::condition_variable" []
      lockType = CppTemplateType "std::unique_lock" [mutexType]

      publicMembers =
        [ CppAccessSpec "public"
        , CppConstructor "Channel" [CppParam "capacity" CppSizeT Nothing]
            [ CppExprStmt $ CppBinary "=" (CppMember CppThis "capacity_") (CppVar "capacity")
            ]
        , CppMethod "send" CppVoid [CppParam "value" templateParam Nothing]
            [ CppDecl $ CppVariable "lock" lockType
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
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "notify_one") []
            ] False
        , CppMethod "receive" templateParam []
            [ CppDecl $ CppVariable "value" templateParam Nothing
            , CppDecl $ CppVariable "lock" lockType
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
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "notify_one") []
            , CppReturn $ Just $ CppVar "value"
            ] False
        ]

      privateMembers =
        [ CppAccessSpec "private"
        , CppVariable "queue_" queueType Nothing
        , CppVariable "mutex_" mutexType Nothing
        , CppVariable "cv_" cvType Nothing
        , CppVariable "capacity_" CppSizeT Nothing
        ]

  addDeclaration $ CppTemplate ["T"] (CppClass "Channel" [] (publicMembers ++ privateMembers))


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

recordHoistedGlobal :: Text -> CppCodeGen ()
recordHoistedGlobal name =
  modify $ \s ->
    let globals = cgsHoistedGlobals s
    in if name `elem` globals
         then s
         else s { cgsHoistedGlobals = globals ++ [name] }

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

mapGoParameter :: GoField -> CppCodeGen [CppParam]
mapGoParameter field = do
  cppType <- generateGoType (goFieldType field)
  let rawNames = [n | Identifier n <- goFieldNames field]
  names <- case rawNames of
    [] -> (:[]) <$> generateTempVar
    _  -> mapM sanitizeName rawNames
  return [CppParam name cppType Nothing | name <- names]
  where
    sanitizeName :: Text -> CppCodeGen Text
    sanitizeName name
      | name == "_" = generateTempVar
      | otherwise = return name

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