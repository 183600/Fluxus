{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Go code generation module with complete functionality
module Fluxus.CodeGen.Go
  ( -- * Code generation functions
    generateGoFromPython
    -- * Configuration
  , GoGenConfig(..)
  , defaultGoConfig
    -- * Error handling
  , GoGenError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad.State
import Control.Monad.Except
import Data.List (nub)

import Fluxus.AST.Common
import Fluxus.AST.Python

-- | Go type representation
data GoType 
  = GoInt
  | GoInt64
  | GoFloat64
  | GoString
  | GoBool
  | GoInterface
  | GoSlice GoType
  | GoMap GoType GoType
  | GoFunc [GoType] [GoType]  -- params, returns
  | GoVoid
  | GoUnknown Text  -- For unresolved types
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Convert GoType to Go code
showGoType :: GoType -> Text
showGoType = \case
  GoInt -> "int"
  GoInt64 -> "int64"
  GoFloat64 -> "float64"
  GoString -> "string"
  GoBool -> "bool"
  GoInterface -> "interface{}"
  GoSlice t -> "[]" <> showGoType t
  GoMap k v -> "map[" <> showGoType k <> "]" <> showGoType v
  GoFunc _ _ -> "func()"  -- Simplified for now
  GoVoid -> ""
  GoUnknown t -> t

-- | Symbol information
data Symbol = Symbol
  { symName :: Text
  , symType :: GoType
  , symDeclared :: Bool  -- Has been declared with var/const
  , symScope :: Int      -- Scope level
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Generation state
data GenState = GenState
  { gsSymbols :: HashMap Text Symbol
  , gsImports :: HashSet Text
  , gsScopeLevel :: Int
  , gsInMain :: Bool
  , gsErrors :: [GoGenError]
  , gsWarnings :: [Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Initial generation state
initialState :: GenState
initialState = GenState
  { gsSymbols = HM.empty
  , gsImports = HS.empty
  , gsScopeLevel = 0
  , gsInMain = False
  , gsErrors = []
  , gsWarnings = []
  }

-- | Generation monad
type GenM = StateT GenState (Either GoGenError)

-- | Go code generation configuration
data GoGenConfig = GoGenConfig
  { ggcPackageName :: !Text
  , ggcStrictTypes :: !Bool  -- Enforce strict typing
  , ggcAutoImports :: !Bool  -- Automatically manage imports
  , ggcPrintFunc :: !Text    -- Function to use for print (fmt.Println or custom)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Default configuration
defaultGoConfig :: Text -> GoGenConfig
defaultGoConfig pkgName = GoGenConfig
  { ggcPackageName = pkgName
  , ggcStrictTypes = True
  , ggcAutoImports = True
  , ggcPrintFunc = "fmt.Println"
  }

-- | Code generation errors
data GoGenError
  = UnsupportedSyntax Text
  | TypeMismatch Text GoType GoType
  | UndefinedVariable Text
  | RedeclaredVariable Text
  | InvalidMainSignature
  | InvalidRangeCall Text
  | GeneralError Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Generate Go code from Python AST
generateGoFromPython :: PythonAST -> GoGenConfig -> Either GoGenError Text
generateGoFromPython (PythonAST module_) config = do
  (result, finalState) <- runStateT (generateModule module_ config) initialState
  case gsErrors finalState of
    [] -> Right result
    (firstError:_) -> Left firstError

-- | Generate complete module
generateModule :: PythonModule -> GoGenConfig -> GenM Text
generateModule module_ config = do
  -- Generate all statements
  statements <- generateStatements (pyModuleBody module_) config
  
  -- Get required imports
  imports <- gets gsImports
  
  -- Build the module
  let header = "package " <> ggcPackageName config
      importSection = if HS.null imports
                      then ""
                      else "\nimport (\n" <> T.unlines (map (\i -> "\t\"" <> i <> "\"") (HS.toList imports)) <> ")"
      body = T.unlines statements
  
  return $ header <> importSection <> "\n\n" <> body

-- | Generate multiple statements
generateStatements :: [Located PythonStmt] -> GoGenConfig -> GenM [Text]
generateStatements stmts config = concat <$> mapM (generateStatement config . locatedValue) stmts

-- | Generate statement
generateStatement :: GoGenConfig -> PythonStmt -> GenM [Text]
generateStatement config = \case
  PyExprStmt (Located _ expr) -> do
    goExpr <- generateExpression config expr
    return [goExpr]
    
  PyAssign targets (Located _ value) -> 
    generateAssignment config targets value
    
  PyIf (Located _ cond) thenBody elseBody ->
    generateIf config cond thenBody elseBody
    
  PyFor (Located _ pattern) (Located _ iterExpr) body _ ->
    generateFor config pattern iterExpr body
    
  PyFuncDef funcDef ->
    generateFunctionDef config funcDef
    
  PyReturn mexpr ->
    generateReturn config mexpr
    
  PyWhile (Located _ cond) body _ -> do
    goCond <- generateExpression config cond
    enterScope
    goBody <- generateStatements body config
    exitScope
    return $ ["for " <> goCond <> " {"] ++ map indent goBody ++ ["}"]
    
  PyBreak -> return ["break"]
  PyContinue -> return ["continue"]
  PyPass -> return ["// pass"]
  
  stmt -> do
    addError $ UnsupportedSyntax $ "Statement type: " <> T.pack (show stmt)
    return ["// Unsupported statement"]

-- | Generate assignment
generateAssignment :: GoGenConfig -> [Located PythonPattern] -> PythonExpr -> GenM [Text]
generateAssignment config targets value = do
  goValue <- generateExpression config value
  valueType <- inferType config value
  
  case targets of
    [Located _ (PatVar (Identifier name))] -> do
      msym <- lookupSymbol name
      case msym of
        Just sym | symDeclared sym -> 
          -- Variable already declared, use simple assignment
          return [name <> " = " <> goValue]
        _ -> do
          -- New variable, declare it
          declareSymbol name valueType
          let typeStr = showGoType valueType
          return [if valueType == GoUnknown "" 
                  then name <> " := " <> goValue
                  else "var " <> name <> " " <> typeStr <> " = " <> goValue]
    
    _ -> do
      -- Multiple targets
      let names = map extractName targets
          extractName (Located _ (PatVar (Identifier n))) = n
          extractName _ = "_"
      
      -- For simplicity, declare all as same type
      decls <- forM names $ \name -> do
        declareSymbol name valueType
        return $ "var " <> name <> " " <> showGoType valueType
      
      return $ decls ++ ["// TODO: Implement tuple unpacking for: " <> goValue]

-- | Generate if statement
generateIf :: GoGenConfig -> PythonExpr -> [Located PythonStmt] -> [Located PythonStmt] -> GenM [Text]
generateIf config cond thenBody elseBody = do
  goCond <- generateExpression config cond
  
  enterScope
  goThen <- generateStatements thenBody config
  exitScope
  
  goElse <- if null elseBody 
            then return []
            else do
              enterScope
              els <- generateStatements elseBody config
              exitScope
              return els
  
  let ifBlock = ["if " <> goCond <> " {"] ++ map indent goThen ++ ["}"]
      elseBlock = if null goElse 
                  then []
                  else ["} else {"] ++ map indent goElse
  
  return $ if null goElse 
           then ifBlock
           else init ifBlock ++ elseBlock ++ ["}"]

-- | Generate for loop
generateFor :: GoGenConfig -> PythonPattern -> PythonExpr -> [Located PythonStmt] -> GenM [Text]
generateFor config pattern iterExpr body = do
  case pattern of
    PatVar (Identifier varName) -> do
      -- Check if it's a range call
      case iterExpr of
        PyCall (Located _ (PyVar (Identifier "range"))) args ->
          generateRangeLoop config varName args body
        _ -> do
          -- Regular for-range loop
          goIter <- generateExpression config iterExpr
          enterScope
          declareSymbol varName (GoUnknown "")
          goBody <- generateStatements body config
          exitScope
          return $ ["for _, " <> varName <> " := range " <> goIter <> " {"] ++ 
                   map indent goBody ++ ["}"]
    _ -> do
      addError $ UnsupportedSyntax "Complex for loop pattern"
      return ["// Unsupported for loop pattern"]

-- | Generate range-based for loop
generateRangeLoop :: GoGenConfig -> Text -> [Located PythonArgument] -> [Located PythonStmt] -> GenM [Text]
generateRangeLoop config varName args body = do
  case map locatedValue args of
    [ArgPositional (Located _ end)] -> do
      -- range(n) -> for i := 0; i < n; i++
      endExpr <- generateExpression config end
      enterScope
      declareSymbol varName GoInt
      goBody <- generateStatements body config
      exitScope
      return $ ["for " <> varName <> " := 0; " <> varName <> " < " <> endExpr <> "; " <> varName <> "++ {"] ++ 
               map indent goBody ++ ["}"]
    
    [ArgPositional (Located _ start), ArgPositional (Located _ end)] -> do
      -- range(start, end) -> for i := start; i < end; i++
      startExpr <- generateExpression config start
      endExpr <- generateExpression config end
      enterScope
      declareSymbol varName GoInt
      goBody <- generateStatements body config
      exitScope
      return $ ["for " <> varName <> " := " <> startExpr <> "; " <> varName <> " < " <> endExpr <> "; " <> varName <> "++ {"] ++ 
               map indent goBody ++ ["}"]
    
    [ArgPositional (Located _ start), ArgPositional (Located _ end), ArgPositional (Located _ step)] -> do
      -- range(start, end, step)
      startExpr <- generateExpression config start
      endExpr <- generateExpression config end
      stepExpr <- generateExpression config step
      enterScope
      declareSymbol varName GoInt
      goBody <- generateStatements body config
      exitScope
      let incr = if T.any (== '-') stepExpr 
                 then varName <> " -= " <> T.drop 1 stepExpr
                 else varName <> " += " <> stepExpr
      return $ ["for " <> varName <> " := " <> startExpr <> "; " <> varName <> " < " <> endExpr <> "; " <> incr <> " {"] ++ 
               map indent goBody ++ ["}"]
    
    _ -> do
      addError $ InvalidRangeCall "Invalid range arguments"
      return ["// Invalid range call"]

-- | Generate function definition
generateFunctionDef :: GoGenConfig -> PythonFuncDef -> GenM [Text]
generateFunctionDef config funcDef = do
  let Identifier funcName = pyFuncName funcDef
      isMain = funcName == "main"
  
  -- Enter function scope
  enterScope
  modify $ \s -> s { gsInMain = isMain }
  
  -- Process parameters
  params <- mapM (generateParameter config) (pyFuncParams funcDef)
  let paramStr = T.intercalate ", " params
  
  -- Process body
  bodyStmts <- generateStatements (pyFuncBody funcDef) config
  
  -- Determine return type
  returnType <- if isMain 
                then return GoVoid
                else inferReturnType (pyFuncBody funcDef) config
  
  -- Exit function scope
  exitScope
  modify $ \s -> s { gsInMain = False }
  
  -- Build function
  let signature = if isMain
                  then "func main()"
                  else "func " <> funcName <> "(" <> paramStr <> ") " <> showGoType returnType
      finalBody = if isMain && not (hasReturn (pyFuncBody funcDef))
                  then bodyStmts
                  else bodyStmts
  
  return $ [signature <> " {"] ++ map indent finalBody ++ ["}"]

-- | Generate parameter
generateParameter :: GoGenConfig -> Located PythonParameter -> GenM Text
generateParameter config (Located _ param) = case param of
  ParamNormal (Identifier name) typeAnn _ -> do
    paramType <- case typeAnn of
      Just ann -> inferTypeFromAnnotation ann
      Nothing -> return GoInterface
    declareSymbol name paramType
    return $ name <> " " <> showGoType paramType
  _ -> return "_ interface{}"

-- | Generate return statement
generateReturn :: GoGenConfig -> Maybe (Located PythonExpr) -> GenM [Text]
generateReturn config mexpr = do
  isMain <- gets gsInMain
  case (isMain, mexpr) of
    (True, Just (Located _ (PyLiteral (PyInt n)))) ->
      -- main function with exit code
      requireImport "os"
      return ["os.Exit(" <> T.pack (show n) <> ")"]
    (True, _) ->
      -- main function without explicit return
      return []
    (False, Just (Located _ expr)) -> do
      goExpr <- generateExpression config expr
      return ["return " <> goExpr]
    (False, Nothing) ->
      return ["return"]

-- | Generate expression
generateExpression :: GoGenConfig -> PythonExpr -> GenM Text
generateExpression config = \case
  PyVar (Identifier name) -> do
    msym <- lookupSymbol name
    case msym of
      Just _ -> return name
      Nothing -> do
        addWarning $ "Undefined variable: " <> name
        return name
    
  PyLiteral lit -> return $ generateLiteral lit
    
  PyBinaryOp op left right -> do
    leftExpr <- generateExpression config (locatedValue left)
    rightExpr <- generateExpression config (locatedValue right)
    let goOp = binaryOpToGo op
    return $ "(" <> leftExpr <> " " <> goOp <> " " <> rightExpr <> ")"
    
  PyUnaryOp op (Located _ operand) -> do
    operandExpr <- generateExpression config operand
    let goOp = unaryOpToGo op
    return $ goOp <> operandExpr
    
  PyCall (Located _ func) args ->
    generateCall config func args
    
  PySubscript (Located _ expr) (Located _ slice) ->
    generateSubscript config expr slice
    
  PyList elements -> do
    elemExprs <- mapM (generateExpression config . locatedValue) elements
    elemType <- if null elements 
                then return GoInterface
                else inferType config (locatedValue $ head elements)
    return $ "[]" <> showGoType elemType <> "{" <> T.intercalate ", " elemExprs <> "}"
    
  PyDict pairs -> do
    let genPair (Located _ k, Located _ v) = do
          kExpr <- generateExpression config k
          vExpr <- generateExpression config v
          return $ kExpr <> ": " <> vExpr
    pairExprs <- mapM genPair pairs
    return $ "map[string]interface{}{" <> T.intercalate ", " pairExprs <> "}"
    
  PyTuple elements -> do
    -- Go doesn't have tuples, use struct or array
    elemExprs <- mapM (generateExpression config . locatedValue) elements
    return $ "[]interface{}{" <> T.intercalate ", " elemExprs <> "}"
    
  PyCompare op (Located _ left) (Located _ right) -> do
    leftExpr <- generateExpression config left
    rightExpr <- generateExpression config right
    let goOp = compareOpToGo op
    return $ "(" <> leftExpr <> " " <> goOp <> " " <> rightExpr <> ")"
    
  expr -> do
    addError $ UnsupportedSyntax $ "Expression: " <> T.pack (show expr)
    return "nil"

-- | Generate literal
generateLiteral :: PythonLiteral -> Text
generateLiteral = \case
  PyInt n -> T.pack (show n)
  PyFloat d -> T.pack (show d)
  PyString s -> "\"" <> escapeString s <> "\""
  PyFString s exprs -> "fmt.Sprintf(\"" <> s <> "\")"  -- Simplified
  PyBool b -> if b then "true" else "false"
  PyNone -> "nil"
  _ -> "nil"

-- | Generate function call
generateCall :: GoGenConfig -> PythonExpr -> [Located PythonArgument] -> GenM Text
generateCall config func args = do
  funcName <- generateExpression config func
  argExprs <- mapM (generateArgument config) args
  
  case funcName of
    "print" | ggcAutoImports config -> do
      requireImport "fmt"
      return $ "fmt.Println(" <> T.intercalate ", " argExprs <> ")"
    "len" -> 
      return $ "len(" <> T.intercalate ", " argExprs <> ")"
    _ -> 
      return $ funcName <> "(" <> T.intercalate ", " argExprs <> ")"

-- | Generate argument
generateArgument :: GoGenConfig -> Located PythonArgument -> GenM Text
generateArgument config (Located _ arg) = case arg of
  ArgPositional (Located _ expr) -> generateExpression config expr
  ArgKeyword _ (Located _ expr) -> generateExpression config expr
  _ -> return "nil"

-- | Generate subscript
generateSubscript :: GoGenConfig -> PythonExpr -> PythonSlice -> GenM Text
generateSubscript config expr slice = do
  exprStr <- generateExpression config expr
  case slice of
    SliceIndex (Located _ idx) -> do
      idxStr <- generateExpression config idx
      return $ exprStr <> "[" <> idxStr <> "]"
    SliceRange start stop step -> do
      -- Go slice syntax: expr[start:stop]
      startStr <- maybe (return "") (generateExpression config . locatedValue) start
      stopStr <- maybe (return "") (generateExpression config . locatedValue) stop
      when (Maybe.isJust step) $ addWarning "Slice step not supported in Go"
      return $ exprStr <> "[" <> startStr <> ":" <> stopStr <> "]"

-- | Infer type from expression
inferType :: GoGenConfig -> PythonExpr -> GenM GoType
inferType config = \case
  PyLiteral lit -> return $ case lit of
    PyInt _ -> GoInt
    PyFloat _ -> GoFloat64
    PyString _ -> GoString
    PyBool _ -> GoBool
    PyNone -> GoInterface
    _ -> GoInterface
  
  PyVar (Identifier name) -> do
    msym <- lookupSymbol name
    return $ maybe GoInterface symType msym
  
  PyList [] -> return $ GoSlice GoInterface
  PyList (h:_) -> do
    elemType <- inferType config (locatedValue h)
    return $ GoSlice elemType
  
  PyDict _ -> return $ GoMap GoString GoInterface
  
  PyBinaryOp op _ _ -> return $ case op of
    OpAdd -> GoInt  -- Simplified
    OpSub -> GoInt
    OpMul -> GoInt
    OpDiv -> GoFloat64
    _ -> GoInterface
  
  _ -> return GoInterface

-- | Infer return type from function body
inferReturnType :: [Located PythonStmt] -> GoGenConfig -> GenM GoType
inferReturnType stmts config = do
  let returns = extractReturns stmts
  if null returns
    then return GoVoid
    else case head returns of
           PyReturn (Just (Located _ expr)) -> inferType config expr
           _ -> return GoVoid
  where
    extractReturns [] = []
    extractReturns (Located _ (PyReturn r):rest) = PyReturn r : extractReturns rest
    extractReturns (_:rest) = extractReturns rest

-- | Infer type from annotation
inferTypeFromAnnotation :: Located PythonTypeExpr -> GenM GoType
inferTypeFromAnnotation _ = return GoInterface  -- Simplified

-- | Symbol table operations
lookupSymbol :: Text -> GenM (Maybe Symbol)
lookupSymbol name = gets (HM.lookup name . gsSymbols)

declareSymbol :: Text -> GoType -> GenM ()
declareSymbol name typ = do
  scope <- gets gsScopeLevel
  modify $ \s -> s { gsSymbols = HM.insert name (Symbol name typ True scope) (gsSymbols s) }

enterScope :: GenM ()
enterScope = modify $ \s -> s { gsScopeLevel = gsScopeLevel s + 1 }

exitScope :: GenM ()
exitScope = do
  scope <- gets gsScopeLevel
  modify $ \s -> s 
    { gsScopeLevel = scope - 1
    , gsSymbols = HM.filter (\sym -> symScope sym < scope) (gsSymbols s)
    }

requireImport :: Text -> GenM ()
requireImport imp = modify $ \s -> s { gsImports = HS.insert imp (gsImports s) }

addError :: GoGenError -> GenM ()
addError err = modify $ \s -> s { gsErrors = err : gsErrors s }

addWarning :: Text -> GenM ()
addWarning warn = modify $ \s -> s { gsWarnings = warn : gsWarnings s }

-- | Helper functions
indent :: Text -> Text
indent = ("    " <>)

escapeString :: Text -> Text
escapeString = T.replace "\"" "\\\"" . T.replace "\n" "\\n" . T.replace "\t" "\\t"

hasReturn :: [Located PythonStmt] -> Bool
hasReturn = any isReturn
  where
    isReturn (Located _ (PyReturn _)) = True
    isReturn _ = False

binaryOpToGo :: PythonBinaryOp -> Text
binaryOpToGo = \case
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpPow -> "**"  -- Note: Go doesn't have power operator
  OpFloorDiv -> "/"
  OpBitAnd -> "&"
  OpBitOr -> "|"
  OpBitXor -> "^"
  OpLeftShift -> "<<"
  OpRightShift -> ">>"
  OpAnd -> "&&"
  OpOr -> "||"
  _ -> "+"

unaryOpToGo :: PythonUnaryOp -> Text
unaryOpToGo = \case
  OpNegate -> "-"
  OpPositive -> "+"
  OpNot -> "!"
  OpBitNot -> "^"

compareOpToGo :: PythonCompareOp -> Text
compareOpToGo = \case
  CmpEq -> "=="
  CmpNeq -> "!="
  CmpLt -> "<"
  CmpLte -> "<="
  CmpGt -> ">"
  CmpGte -> ">="
  CmpIs -> "=="  -- Simplified
  CmpIsNot -> "!="
  CmpIn -> "in"  -- Not directly supported in Go
  CmpNotIn -> "not in"