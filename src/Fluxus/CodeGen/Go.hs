{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Go code generation module
module Fluxus.CodeGen.Go
  ( -- * Code generation functions
    generateGoFromPython
  , generateGoFromPythonWithAnnotations
  , generateGoCode
  , generateGoCodeWithAnnotations
    -- * Configuration
  , GoGenConfig(..)
  , defaultGoConfig
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Fluxus.AST.Common hiding (TypeVar)
import qualified Fluxus.AST.Common as Common (TypeVar(..))
import Fluxus.AST.Python
import Fluxus.Analysis.CommonExprLowering (pythonExprToCommon, renderCommonExpr)

-- | Go code generation configuration
data GoGenConfig = GoGenConfig
  { ggcPackageName :: !Text
  , ggcEnableFmt   :: !Bool
  , ggcImportMap   :: !(HashMap Text Text)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Runtime environment for Go code generation
data GoGenEnv = GoGenEnv
  { ggeConfig       :: !GoGenConfig
  , ggeAnnotations  :: !AnalysisAnnotations
  }

data GoGenState = GoGenState
  { ggsScopes         :: ![HashMap Text Text]
  , ggsFunctionTypes  :: !(HashMap Text Text)
  , ggsReturnStack    :: ![[Text]]
  }

initialGoGenState :: GoGenState
initialGoGenState = GoGenState
  { ggsScopes = [HM.empty]
  , ggsFunctionTypes = HM.empty
  , ggsReturnStack = []
  }

type GoGen = ReaderT GoGenEnv (State GoGenState)

runGoGen :: GoGenEnv -> GoGen a -> a
runGoGen env action = evalState (runReaderT action env) initialGoGenState

pushScope :: GoGen ()
pushScope = modify $ \s -> s { ggsScopes = HM.empty : ggsScopes s }

popScope :: GoGen ()
popScope = modify $ \s -> case ggsScopes s of
  [] -> s
  (_:rest) -> s { ggsScopes = rest }

setVarType :: Text -> Text -> GoGen ()
setVarType name ty = modify $ \s -> case ggsScopes s of
  [] -> s { ggsScopes = [HM.singleton name ty] }
  scope:rest -> s { ggsScopes = HM.insert name ty scope : rest }

lookupVarType :: Text -> GoGen (Maybe Text)
lookupVarType name = do
  scopes <- gets ggsScopes
  pure $ foldl' (\acc scope -> acc <|> HM.lookup name scope) Nothing scopes

pushFunctionReturns :: GoGen ()
pushFunctionReturns = modify $ \s -> s { ggsReturnStack = [] : ggsReturnStack s }

popFunctionReturns :: GoGen [Text]
popFunctionReturns = do
  stack <- gets ggsReturnStack
  case stack of
    [] -> pure []
    current:rest -> do
      modify $ \s -> s { ggsReturnStack = rest }
      pure current

recordReturnType :: Text -> GoGen ()
recordReturnType ty = modify $ \s -> case ggsReturnStack s of
  [] -> s
  current:rest -> s { ggsReturnStack = (current ++ [ty]) : rest }

registerFunctionReturnType :: Text -> Text -> GoGen ()
registerFunctionReturnType name ty =
  modify $ \s -> s { ggsFunctionTypes = HM.insert name ty (ggsFunctionTypes s) }

lookupFunctionReturnType :: Text -> GoGen (Maybe Text)
lookupFunctionReturnType name = gets (HM.lookup name . ggsFunctionTypes)

-- | Default configuration
defaultGoConfig :: Text -> GoGenConfig
defaultGoConfig pkgName = GoGenConfig
  { ggcPackageName = pkgName
  , ggcEnableFmt = True
  , ggcImportMap = HM.fromList
      [ ("print", "fmt")
      , ("println", "fmt")
      ]
  }

-- | Generate Go code from Python AST with optional annotations
generateGoFromPythonWithAnnotations :: AnalysisAnnotations -> PythonAST -> GoGenConfig -> Text
generateGoFromPythonWithAnnotations annotations (PythonAST module_) config =
  let env = GoGenEnv config annotations
      (header, body) = generateModule env module_
  in header <> "\n" <> body

-- | Generate Go code from Python AST without annotations
generateGoFromPython :: PythonAST -> GoGenConfig -> Text
generateGoFromPython ast config =
  generateGoFromPythonWithAnnotations emptyAnnotations ast config

-- | Generate Go code as text with annotations
generateGoCodeWithAnnotations :: AnalysisAnnotations -> PythonAST -> GoGenConfig -> Text
generateGoCodeWithAnnotations annotations ast config =
  let env = GoGenEnv config annotations
      (header, body) = generateModule env (pyModule ast)
  in header <> "\n" <> body

-- | Generate Go code as text without annotations
generateGoCode :: PythonAST -> GoGenConfig -> Text
generateGoCode ast config =
  generateGoCodeWithAnnotations emptyAnnotations ast config

-- | Generate complete module
generateModule :: GoGenEnv -> PythonModule -> (Text, Text)
generateModule env pyModule =
  let imports = generateImports env pyModule
      bodyLines = runGoGen env (generateModuleBody pyModule)
      body = T.unlines bodyLines
  in (imports, body)

-- | Generate imports
generateImports :: GoGenEnv -> PythonModule -> Text
generateImports env _pyModule =
  let config = ggeConfig env
      header = "package " <> ggcPackageName config
      imports = case ggcEnableFmt config of
                  True -> ["\t\"fmt\""]
                  False -> []
  in if null imports
     then header
     else T.unlines (header : "" : "import (" : imports ++ [")"])

-- | Generate module declarations sequentially
generateModuleBody :: PythonModule -> GoGen [Text]
generateModuleBody pyModule = do
  stmtGroups <- mapM generateStatement (pyModuleBody pyModule)
  pure (concat stmtGroups)

data GeneratedParam = GeneratedParam
  { gpSignature :: !Text
  , gpVarType :: !Text
  , gpFuncType :: !Text
  }

-- | Generate statement
generateStatement :: Located PythonStmt -> GoGen [Text]
generateStatement (Located _ stmt) = case stmt of
  PyExprStmt exprLoc -> do
    goExpr <- generateExpression exprLoc
    pure [goExpr]

  PyAssign targets valueLoc -> case targets of
    [Located _ (PatVar (Identifier name))] -> do
      goValue <- generateExpression valueLoc
      goType <- inferGoType valueLoc
      setVarType name goType
      pure ["var " <> name <> " " <> goType <> " = " <> goValue]
    _ -> pure []

  PyAnnAssign (Located _ (PatVar (Identifier name))) typeExpr maybeValue -> do
    let goType = pythonTypeToGo (locatedValue typeExpr)
    setVarType name goType
    assignment <- case maybeValue of
      Nothing -> pure ""
      Just value -> do
        goValue <- generateExpression value
        pure (" = " <> goValue)
    pure ["var " <> name <> " " <> goType <> assignment]

  PyIf cond thenBody elseBody -> do
    goCond <- generateExpression cond
    goThen <- mapM generateStatement thenBody
    goElse <- mapM generateStatement elseBody
    let blockThen = concat goThen
        blockElse = concat goElse
        assembled = ["if " <> goCond <> " {"]
                    ++ map ("\t" <>) blockThen
                    ++ ["}"]
                    ++ if null blockElse
                         then []
                         else ["else {"] ++ map ("\t" <>) blockElse ++ ["}"]
    pure assembled

  PyFor (Located _ (PatVar (Identifier varName))) iterExpr body _elseBody -> do
    goIter <- generateExpression iterExpr
    iterType <- inferGoType iterExpr
    let elementType = iterableElementType iterType
    setVarType varName elementType
    goBody <- mapM generateStatement body
    pure (handleRangeLoop varName goIter (concat goBody))

  PyFuncDef funcDef ->
    generateFunctionDef funcDef

  PyReturn (Just exprLoc) -> do
    goExpr <- generateExpression exprLoc
    retType <- inferGoType exprLoc
    recordReturnType retType
    pure ["return " <> goExpr]

  PyReturn Nothing -> do
    recordReturnType ""
    pure ["return"]

  _ -> pure []

-- | Generate function definition
generateFunctionDef :: PythonFuncDef -> GoGen [Text]
generateFunctionDef funcDef = do
  let Identifier funcName = pyFuncName funcDef
      annotatedReturn = pythonTypeToGo . locatedValue <$> pyFuncReturns funcDef

  pushScope
  pushFunctionReturns

  params <- mapM generateParameter (pyFuncParams funcDef)
  let paramStr = if null params
                   then ""
                   else T.intercalate ", " (map gpSignature params)

  bodyGroups <- mapM generateStatement (pyFuncBody funcDef)
  returns <- popFunctionReturns
  popScope

  let body = concat bodyGroups
      inferredReturn = determineReturnType returns
      rawReturnType = fromMaybe inferredReturn annotatedReturn
      finalReturnType =
        if funcName == "main" && T.null rawReturnType
          then "int"
          else rawReturnType
      funcTypeText =
        let argTypes = map gpFuncType params
            retPart = if T.null finalReturnType then "" else " " <> finalReturnType
        in "func(" <> T.intercalate ", " argTypes <> ")" <> retPart

  registerFunctionReturnType funcName finalReturnType
  setVarType funcName funcTypeText

  let formatReturnType t
        | T.null t = ""
        | otherwise = " " <> t
      funcSignature = "func " <> funcName <> "(" <> paramStr <> ")" <> formatReturnType finalReturnType
      mainReturn =
        if funcName == "main" && null returns
          then ["\treturn 0"]
          else []
      renderedBody = map ("\t" <>) body ++ mainReturn

  pure ([funcSignature <> " {"] ++ renderedBody ++ ["}"])

-- | Determine final return type from collected returns
determineReturnType :: [Text] -> Text
determineReturnType returns =
  case filter (not . T.null) returns of
    [] -> ""
    (t:ts) -> foldl' combineGoTypes t ts

-- | Generate parameter
generateParameter :: Located PythonParameter -> GoGen GeneratedParam
generateParameter (Located _ param) = case param of
  ParamNormal (Identifier name) typeAnn defaultExpr -> do
    let typeFromAnnotation = pythonTypeToGo . locatedValue <$> typeAnn
    typeFromDefault <- traverse pythonTypeToGoLiteral defaultExpr
    let paramType = fromMaybe "interface{}" (typeFromAnnotation <|> typeFromDefault)
    setVarType name paramType
    pure GeneratedParam
      { gpSignature = name <> " " <> paramType
      , gpVarType = paramType
      , gpFuncType = paramType
      }

  ParamVarArgs (Identifier name) typeAnn -> do
    let baseType = fromMaybe "interface{}" (pythonTypeToGo . locatedValue <$> typeAnn)
        varType = "[]" <> baseType
    setVarType name varType
    pure GeneratedParam
      { gpSignature = name <> " ..." <> baseType
      , gpVarType = varType
      , gpFuncType = "..." <> baseType
      }

  ParamKwArgs (Identifier name) typeAnn -> do
    let valueType = fromMaybe "interface{}" (pythonTypeToGo . locatedValue <$> typeAnn)
        mapType = "map[string]" <> valueType
    setVarType name mapType
    pure GeneratedParam
      { gpSignature = name <> " " <> mapType
      , gpVarType = mapType
      , gpFuncType = mapType
      }

  ParamKwOnly (Identifier name) typeAnn defaultExpr -> do
    let typeFromAnnotation = pythonTypeToGo . locatedValue <$> typeAnn
    typeFromDefault <- traverse pythonTypeToGoLiteral defaultExpr
    let paramType = fromMaybe "interface{}" (typeFromAnnotation <|> typeFromDefault)
    setVarType name paramType
    pure GeneratedParam
      { gpSignature = name <> " " <> paramType
      , gpVarType = paramType
      , gpFuncType = paramType
      }

-- | Generate expression
generateExpression :: Located PythonExpr -> GoGen Text
generateExpression (Located _ expr) = case expr of
  PyVar (Identifier name) -> pure name

  PyLiteral lit -> case lit of
    PyInt n -> pure (T.pack (show n))
    PyFloat d -> pure (T.pack (show d))
    PyString s -> pure ("\"" <> s <> "\"")
    PyBool b -> pure (if b then "true" else "false")
    PyNone -> pure "nil"
    PyComplex _ _ -> pure "0"
    PyFString _ _ -> pure "\"\""
    PyBytes _ -> pure "[]byte{}"
    PyEllipsis -> pure "nil"

  PyBinaryOp op left right -> do
    leftExpr <- generateExpression left
    rightExpr <- generateExpression right
    let goOp = case op of
          OpAdd -> "+"
          OpSub -> "-"
          OpMul -> "*"
          OpDiv -> "/"
          OpMod -> "%"
          _ -> "+"
    pure ("(" <> leftExpr <> " " <> goOp <> " " <> rightExpr <> ")")

  PyUnaryOp op operand -> do
    operandExpr <- generateExpression operand
    let goOp = case op of
          OpNegate -> "-"
          OpPositive -> "+"
          OpNot -> "!"
          OpBitNot -> "^"
    pure (goOp <> operandExpr)

  PyCall func args -> do
    funcExpr <- generateExpression func
    argExprs <- mapM extractArg args
    let argStr = T.intercalate ", " argExprs
    pure $ case funcExpr of
      "print" -> "fmt.Println(" <> argStr <> ")"
      "println" -> "fmt.Println(" <> argStr <> ")"
      _ -> funcExpr <> "(" <> argStr <> ")"
    where
      extractArg (Located _ arg) = case arg of
        ArgPositional a -> generateExpression a
        ArgKeyword _ a -> generateExpression a
        ArgStarred a -> generateExpression a
        ArgKwStarred a -> generateExpression a

  PySubscript exprLoc (Located _ (SliceIndex index)) -> do
    exprStr <- generateExpression exprLoc
    indexStr <- generateExpression index
    pure (exprStr <> "[" <> indexStr <> "]")

  PyList elements -> do
    elemExprs <- mapM generateExpression elements
    elemType <- inferElementType elements
    pure ("[]" <> elemType <> "{" <> T.intercalate ", " elemExprs <> "}")

  PyTuple elements -> do
    elemExprs <- mapM generateExpression elements
    pure ("[]interface{}{" <> T.intercalate ", " elemExprs <> "}")

  PySet elements -> do
    elemType <- inferElementType elements
    pure ("map[" <> elemType <> "]struct{}{}")

  PyDict pairs -> do
    keyType <- inferDictKeyType pairs
    valueType <- inferDictValueType pairs
    entries <- mapM renderPair pairs
    pure ("map[" <> keyType <> "]" <> valueType <> "{" <> T.intercalate ", " entries <> "}")
    where
      renderPair (k, v) = do
        keyExpr <- generateExpression k
        valueExpr <- generateExpression v
        pure (keyExpr <> ": " <> valueExpr)

  _ -> pure "0"

-- | Extract text from Identifier
identifierText :: Identifier -> Text
identifierText (Identifier t) = t

-- | Convert a simple Python type name into its Go equivalent
goTypeFromSimpleName :: Text -> Text
goTypeFromSimpleName name =
  let lower = T.toLower name
  in case lower of
       "int" -> "int"
       "int8" -> "int8"
       "int16" -> "int16"
       "int32" -> "int32"
       "int64" -> "int64"
       "uint" -> "uint"
       "uint8" -> "uint8"
       "uint16" -> "uint16"
       "uint32" -> "uint32"
       "uint64" -> "uint64"
       "float" -> "float64"
       "float32" -> "float32"
       "float64" -> "float64"
       "complex" -> "complex128"
       "complex64" -> "complex64"
       "complex128" -> "complex128"
       "bool" -> "bool"
       "true" -> "bool"
       "false" -> "bool"
       "str" -> "string"
       "string" -> "string"
       "bytes" -> "[]byte"
       "bytearray" -> "[]byte"
       "any" -> "interface{}"
       "object" -> "interface{}"
       "none" -> "interface{}"
       "list" -> "[]interface{}"
       "tuple" -> "[]interface{}"
       "sequence" -> "[]interface{}"
       "set" -> "map[interface{}]struct{}"
       "dict" -> "map[interface{}]interface{}"
       "mapping" -> "map[interface{}]interface{}"
       _ -> name

-- | Map unified analysis types to Go
mapCommonTypeToGo :: Type -> Text
mapCommonTypeToGo = \case
  TInt bits -> case bits of
    8 -> "int8"
    16 -> "int16"
    32 -> "int32"
    64 -> "int64"
    _ -> "int"
  TUInt bits -> case bits of
    8 -> "uint8"
    16 -> "uint16"
    32 -> "uint32"
    64 -> "uint64"
    _ -> "uint"
  TFloat bits -> case bits of
    32 -> "float32"
    64 -> "float64"
    _ -> "float64"
  TBool -> "bool"
  TString -> "string"
  TBytes -> "[]byte"
  TChar -> "rune"
  TVoid -> ""
  TAny -> "interface{}"
  TList t -> "[]" <> mapCommonTypeToGo t
  TTuple _ -> "[]interface{}"
  TDict k v -> "map[" <> mapCommonTypeToGo k <> "]" <> mapCommonTypeToGo v
  TSet t -> "map[" <> mapCommonTypeToGo t <> "]struct{}"
  TOptional t -> let mapped = mapCommonTypeToGo t in if T.null mapped then "interface{}" else "*" <> mapped
  TFunction args ret ->
    let argTypes = T.intercalate ", " (map mapCommonTypeToGo args)
        retType = mapCommonTypeToGo ret
        retPart = if T.null retType then "" else " " <> retType
    in "func(" <> argTypes <> ")" <> retPart
  TMethod recv args ret ->
    let recvType = mapCommonTypeToGo recv
        argTypes = T.intercalate ", " (map mapCommonTypeToGo args)
        argsWithRecv = T.intercalate ", " (filter (not . T.null) (recvType : if T.null argTypes then [] else [argTypes]))
        retType = mapCommonTypeToGo ret
        retPart = if T.null retType then "" else " " <> retType
    in "func(" <> argsWithRecv <> ")" <> retPart
  TStruct qn _ -> qualifiedNameToText qn
  TEnum qn _ -> qualifiedNameToText qn
  TInterface qn _ -> qualifiedNameToText qn
  TUnion _ -> "interface{}"
  TVar (Common.TypeVar name) -> name
  TGeneric qn _ -> qualifiedNameToText qn
  TForall _ _ t -> mapCommonTypeToGo t
  TOwned t -> let mapped = mapCommonTypeToGo t in if T.null mapped then "interface{}" else "*" <> mapped
  TShared t -> let mapped = mapCommonTypeToGo t in if T.null mapped then "interface{}" else "*" <> mapped
  TBorrowed t -> let mapped = mapCommonTypeToGo t in if T.null mapped then "interface{}" else "*" <> mapped
  TMutable t -> let mapped = mapCommonTypeToGo t in if T.null mapped then "interface{}" else "*" <> mapped
  TError _ -> "interface{}"
  TInfer _ -> "interface{}"

-- | Helper to render qualified names
qualifiedNameToText :: QualifiedName -> Text
qualifiedNameToText (QualifiedName modules (Identifier name)) =
  case modules of
    [] -> name
    xs -> T.intercalate "." (map (\(ModuleName m) -> m) xs) <> "." <> name

-- | Infer element type for list/tuple/set literals
inferElementType :: [Located PythonExpr] -> GoGen Text
inferElementType [] = pure "interface{}"
inferElementType (x:xs) = do
  firstType <- inferGoType x
  foldM
    (\current exprLoc -> do
        nextType <- inferGoType exprLoc
        pure (combineGoTypes current nextType))
    firstType
    xs

-- | Infer key type for dict literals
inferDictKeyType :: [(Located PythonExpr, Located PythonExpr)] -> GoGen Text
inferDictKeyType [] = pure "interface{}"
inferDictKeyType ((k,_):rest) = do
  firstType <- inferGoType k
  foldM
    (\current (nextKey, _nextVal) -> do
        nextType <- inferGoType nextKey
        pure (combineGoTypes current nextType))
    firstType
    rest

-- | Infer value type for dict literals
inferDictValueType :: [(Located PythonExpr, Located PythonExpr)] -> GoGen Text
inferDictValueType [] = pure "interface{}"
inferDictValueType ((_,v):rest) = do
  firstType <- inferGoType v
  foldM
    (\current (_nextKey, nextVal) -> do
        nextType <- inferGoType nextVal
        pure (combineGoTypes current nextType))
    firstType
    rest

-- | Combine two Go types, picking a compatible supertype if possible
combineGoTypes :: Text -> Text -> Text
combineGoTypes a b
  | T.null a = b
  | T.null b = a
  | a == b = a
  | isNumericType a && isNumericType b = promoteNumericTypes a b
  | isStringType a && isStringType b = "string"
  | isBoolType a && isBoolType b = "bool"
  | isListType a && isListType b = "[]" <> combineGoTypes (stripListPrefix a) (stripListPrefix b)
  | isMapType a && isMapType b =
      let (ka, va) = splitMapType a
          (kb, vb) = splitMapType b
      in "map[" <> combineGoTypes ka kb <> "]" <> combineGoTypes va vb
  | otherwise = "interface{}"

-- | Helpers for working with Go type strings
isNumericType :: Text -> Bool
isNumericType t = isIntegralType t || isFloatType t || isComplexType t
  where
    isIntegralType v = T.isPrefixOf "int" v || T.isPrefixOf "uint" v
    isFloatType v = T.isPrefixOf "float" v
    isComplexType v = T.isPrefixOf "complex" v

isStringType :: Text -> Bool
isStringType t = t == "string"

isBoolType :: Text -> Bool
isBoolType t = t == "bool"

isListType :: Text -> Bool
isListType t = "[]" `T.isPrefixOf` t

stripListPrefix :: Text -> Text
stripListPrefix t
  | isListType t = T.drop 2 t
  | otherwise = "interface{}"

isMapType :: Text -> Bool
isMapType t = "map[" `T.isPrefixOf` t

splitMapType :: Text -> (Text, Text)
splitMapType t
  | isMapType t =
      let inner = T.drop 4 t
          (keyPart, rest) = T.breakOn "]" inner
          valuePart = T.drop 1 rest
          keyType = if T.null keyPart then "interface{}" else keyPart
          valueType = if T.null valuePart then "interface{}" else valuePart
      in (keyType, valueType)
  | otherwise = ("interface{}", "interface{}")

-- | Determine element type from container for loops
iterableElementType :: Text -> Text
iterableElementType container
  | isListType container = stripListPrefix container
  | isMapType container =
      let (_k, v) = splitMapType container
      in v
  | container == "string" = "rune"
  | otherwise = "interface{}"

-- | Attempt to refine type from analysis annotations
inferGoTypeFromAnnotations :: Located PythonExpr -> GoGen (Maybe Text)
inferGoTypeFromAnnotations exprLoc = do
  env <- ask
  case pythonExprToCommon exprLoc of
    Left _ -> pure Nothing
    Right common -> do
      let key = renderCommonExpr common
      case lookupAnnotations key (ggeAnnotations env) of
        Nothing -> pure Nothing
        Just anns -> pure (mapCommonTypeToGo <$> eaInferredType anns)

-- | Infer Go type from Python expression with analysis fallback
inferGoType :: Located PythonExpr -> GoGen Text
inferGoType exprLoc = do
  annotated <- inferGoTypeFromAnnotations exprLoc
  case annotated of
    Just ty | not (T.null ty) -> pure ty
    _ -> inferGoTypeHeuristic (locValue exprLoc)

-- | Heuristic Go type inference when analysis is unavailable
inferGoTypeHeuristic :: PythonExpr -> GoGen Text
inferGoTypeHeuristic expr = case expr of
  PyLiteral lit -> pure $ case lit of
    PyInt _ -> "int"
    PyFloat _ -> "float64"
    PyString _ -> "string"
    PyBool _ -> "bool"
    PyNone -> "interface{}"
    PyComplex _ _ -> "complex128"
    PyFString _ _ -> "string"
    PyBytes _ -> "[]byte"
    PyEllipsis -> "interface{}"
  PyConst qn -> pure (goTypeFromSimpleName (identifierText (qnName qn)))
  PyVar (Identifier name) -> do
    fromEnv <- lookupVarType name
    pure (fromMaybe "interface{}" fromEnv)
  PyList elements -> do
    elemType <- inferElementType elements
    pure ("[]" <> elemType)
  PyTuple elements -> do
    elemType <- inferElementType elements
    pure ("[]" <> elemType)
  PySet elements -> do
    elemType <- inferElementType elements
    pure ("map[" <> elemType <> "]struct{}")
  PyDict pairs -> do
    keyType <- inferDictKeyType pairs
    valueType <- inferDictValueType pairs
    pure ("map[" <> keyType <> "]" <> valueType)
  PyBinaryOp op left right -> inferBinaryOpType op left right
  PyUnaryOp op operand -> case op of
    OpNot -> pure "bool"
    _ -> inferGoType operand
  PyBoolOp _ _ -> pure "bool"
  PyComparison _ _ -> pure "bool"
  PyIfExp _ thenExpr elseExpr -> do
    thenType <- inferGoType thenExpr
    elseType <- inferGoType elseExpr
    pure (combineGoTypes thenType elseType)
  PyCall func _ -> inferCallReturnType (locValue func)
  PyNamedExpr _ valueExpr -> inferGoType valueExpr
  PyAttribute {} -> pure "interface{}"
  PySubscript container _ -> do
    containerType <- inferGoType container
    pure $ if isListType containerType
      then stripListPrefix containerType
      else if isMapType containerType
         then snd (splitMapType containerType)
         else "interface{}"
  PyAwait awaited -> inferGoType awaited
  PyJoinedStr _ -> pure "string"
  PyFormatSpec _ -> pure "string"
  PyListComp _ _ -> pure "[]interface{}"
  PySetComp _ _ -> pure "map[interface{}]struct{}"
  PyDictComp _ _ _ -> pure "map[interface{}]interface{}"
  PyGenComp _ _ -> pure "[]interface{}"
  _ -> pure "interface{}"

-- | Infer return type from binary operation
inferBinaryOpType :: BinaryOp -> Located PythonExpr -> Located PythonExpr -> GoGen Text
inferBinaryOpType op left right = do
  leftType <- inferGoType left
  rightType <- inferGoType right
  pure $ case op of
    OpAdd -> promoteNumericTypes leftType rightType
    OpSub -> promoteNumericTypes leftType rightType
    OpMul -> promoteNumericTypes leftType rightType
    OpDiv -> promoteNumericTypes leftType rightType
    OpMod -> promoteNumericTypes leftType rightType
    OpPow -> promoteNumericTypes leftType rightType
    OpFloorDiv -> "int"
    OpBitAnd -> "int"
    OpBitOr -> "int"
    OpBitXor -> "int"
    OpShiftL -> "int"
    OpShiftR -> "int"
    OpAnd -> "bool"
    OpOr -> "bool"
    OpConcat -> "string"
    OpIn -> "bool"
    OpNotIn -> "bool"

-- | Promote numeric types for binary operations
promoteNumericTypes :: Text -> Text -> Text
promoteNumericTypes t1 t2
  | t1 == "float64" || t2 == "float64" = "float64"
  | t1 == "float32" || t2 == "float32" = "float32"
  | t1 == "complex128" || t2 == "complex128" = "complex128"
  | t1 == "int64" || t2 == "int64" = "int64"
  | t1 == "int32" || t2 == "int32" = "int32"
  | t1 == "int" || t2 == "int" = "int"
  | otherwise = "interface{}"

-- | Infer return type from function call (heuristic fallback)
inferCallReturnType :: PythonExpr -> GoGen Text
inferCallReturnType expr = case expr of
  PyVar (Identifier name) -> do
    fromRegistry <- lookupFunctionReturnType name
    case fromRegistry of
      Just ty | not (T.null ty) -> pure ty
      _ -> pure $ case name of
        "range" -> "[]int"
        "len" -> "int"
        "str" -> "string"
        "int" -> "int"
        "float" -> "float64"
        "bool" -> "bool"
        "list" -> "[]interface{}"
        "dict" -> "map[interface{}]interface{}"
        "set" -> "map[interface{}]bool"
        _ -> "interface{}"
  _ -> do
    annotated <- inferGoTypeFromAnnotations (noLoc expr)
    pure (fromMaybe "interface{}" annotated)

-- | Convert Python type annotation to Go type
pythonTypeToGo :: PythonTypeExpr -> Text
pythonTypeToGo typeExpr = case typeExpr of
  TypeName qn -> qualifiedNameToGo qn
  TypeVar tv -> tv
  TypeSubscript base args -> case locatedValue base of
    TypeName qn -> case qualifiedNameToGo qn of
      "List" -> case args of
        [Located _ arg] -> "[]" <> pythonTypeToGo arg
        _ -> "[]interface{}"
      "Dict" -> case args of
        [Located _ k, Located _ v] -> "map[" <> pythonTypeToGo k <> "]" <> pythonTypeToGo v
        _ -> "map[interface{}]interface{}"
      "Set" -> case args of
        [Located _ arg] -> "map[" <> pythonTypeToGo arg <> "]bool"
        _ -> "map[interface{}]bool"
      "Tuple" -> "[]interface{}"
      "Optional" -> case args of
        [Located _ arg] -> "*" <> pythonTypeToGo arg
        _ -> "interface{}"
      _ -> "interface{}"
    _ -> "interface{}"
  TypeTuple types ->
    if null types
    then "[]interface{}"
    else "[]interface{}"
  TypeUnion _ -> "interface{}"
  TypeOptional (Located _ t) -> "*" <> pythonTypeToGo t
  TypeCallable params (Located _ ret) ->
    let paramTypes = T.intercalate ", " (map pythonTypeToGo (map locatedValue params))
        retType = pythonTypeToGo ret
        retPart = if T.null retType then "" else " " <> retType
    in "func(" <> paramTypes <> ")" <> retPart
  TypeLiteral _ -> "interface{}"

-- | Convert qualified name to Go type
qualifiedNameToGo :: QualifiedName -> Text
qualifiedNameToGo qn =
  let Identifier name = qnName qn
  in case name of
    "int" -> "int"
    "float" -> "float64"
    "str" -> "string"
    "bool" -> "bool"
    "bytes" -> "[]byte"
    "None" -> "interface{}"
    "Any" -> "interface{}"
    "list" -> "[]interface{}"
    "dict" -> "map[interface{}]interface{}"
    "set" -> "map[interface{}]bool"
    "tuple" -> "[]interface{}"
    "List" -> "[]interface{}"
    "Dict" -> "map[interface{}]interface{}"
    "Set" -> "map[interface{}]bool"
    "Tuple" -> "[]interface{}"
    "Optional" -> "interface{}"
    _ -> name

-- | Infer Go type from Python expression literal (for default parameter values)
pythonTypeToGoLiteral :: Located PythonExpr -> GoGen Text
pythonTypeToGoLiteral exprLoc = inferGoType exprLoc

-- | Handle range calls in for loops
handleRangeLoop :: Text -> Text -> [Text] -> [Text]
handleRangeLoop varName rangeCall body =
  case parseRangeCall rangeCall of
    Just n ->
      ["for " <> varName <> " := 0; " <> varName <> " < " <> n <> "; " <> varName <> "++ {"]
      ++ map ("\t" <>) body ++ ["}"]
    Nothing ->
      ["for " <> varName <> " := range " <> rangeCall <> " {"]
      ++ map ("\t" <>) body ++ ["}"]
  where
    parseRangeCall :: Text -> Maybe Text
    parseRangeCall call =
      if "range(" `T.isPrefixOf` call && ")" `T.isSuffixOf` call
      then Just $ T.take (T.length call - 7) (T.drop 6 call)
      else Nothing
