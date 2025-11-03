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

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

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
      decls = generateDeclarations env pyModule
  in (imports, decls)

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

-- | Generate all declarations
generateDeclarations :: GoGenEnv -> PythonModule -> Text
generateDeclarations env pyModule =
  let stmts = concatMap (generateStatement env . locatedValue) (pyModuleBody pyModule)
  in T.unlines stmts

-- | Generate statement
generateStatement :: GoGenEnv -> PythonStmt -> [Text]
generateStatement env stmt = case stmt of
  PyExprStmt exprLoc ->
    let goExpr = generateExpression env exprLoc
    in [goExpr]

  PyAssign targets valueLoc ->
    case targets of
      [Located _ (PatVar (Identifier name))] ->
        let goValue = generateExpression env valueLoc
            goType = inferGoType env valueLoc
        in ["var " <> name <> " " <> goType <> " = " <> goValue]
      _ -> []

  PyAnnAssign (Located _ (PatVar (Identifier name))) typeExpr maybeValue ->
    let goType = pythonTypeToGo (locatedValue typeExpr)
        assignment = case maybeValue of
                       Just value -> " = " <> generateExpression env value
                       Nothing -> ""
    in ["var " <> name <> " " <> goType <> assignment]

  PyIf cond thenBody elseBody ->
    let goCond = generateExpression env cond
        goThen = concatMap (generateStatement env . locatedValue) thenBody
        goElse = case elseBody of
                  [] -> []
                  body -> concatMap (generateStatement env . locatedValue) body
    in ["if " <> goCond <> " {"] ++
       map ("\t" <>) goThen ++
       ["}"] ++
       (if null goElse then [] else ["else {"] ++ map ("\t" <>) goElse ++ ["}"])

  PyFor (Located _ (PatVar (Identifier varName))) iterExpr body elseBody ->
    let goIter = generateExpression env iterExpr
        goBody = concatMap (generateStatement env . locatedValue) body
        rangeHandled = handleRangeLoop varName goIter goBody
    in rangeHandled

  PyFuncDef funcDef ->
    generateFunctionDef env funcDef

  PyReturn (Just exprLoc) ->
    let goExpr = generateExpression env exprLoc
    in ["return " <> goExpr]
  PyReturn Nothing ->
    ["return"]

  _ -> []

-- | Generate function definition
generateFunctionDef :: GoGenEnv -> PythonFuncDef -> [Text]
generateFunctionDef env funcDef =
  let Identifier funcName = pyFuncName funcDef
      params = map (generateParameter env) (pyFuncParams funcDef)
      paramStr = if null params then "" else T.intercalate ", " params
      body = concatMap (generateStatement env . locatedValue) (pyFuncBody funcDef)
      returnType = case pyFuncReturns funcDef of
                     Just locTypeExpr -> pythonTypeToGo (locatedValue locTypeExpr)
                     Nothing ->
                       if funcName == "main"
                         then "int"
                         else inferReturnTypeFromBody env (pyFuncBody funcDef)
      funcSignature = "func " <> funcName <> "(" <> paramStr <> ")" <> formatReturnType returnType
      mainReturn = if funcName == "main" && not (hasReturnStatement (pyFuncBody funcDef))
                     then ["\treturn 0"]
                     else []
  in [funcSignature <> " {"] ++
     map ("\t" <>) body ++
     mainReturn ++
     ["}"]
  where
    formatReturnType t
      | T.null t = ""
      | otherwise = " " <> t
    hasReturnStatement = any isReturnStmt
    isReturnStmt (Located _ (PyReturn _)) = True
    isReturnStmt _ = False

-- | Generate parameter
generateParameter :: GoGenEnv -> Located PythonParameter -> Text
generateParameter env (Located _ param) = case param of
  ParamNormal (Identifier name) typeAnn defaultExpr ->
    let typeFromAnnotation = pythonTypeToGo . locatedValue <$> typeAnn
        typeFromDefault = pythonTypeToGoLiteral env <$> defaultExpr
        paramType = fromMaybe "interface{}" (typeFromAnnotation <|> typeFromDefault)
    in name <> " " <> paramType
  ParamVarArgs (Identifier name) typeAnn ->
    let baseType = fromMaybe "interface{}" (pythonTypeToGo . locatedValue <$> typeAnn)
    in name <> " ..." <> baseType
  ParamKwArgs (Identifier name) typeAnn ->
    let valueType = fromMaybe "interface{}" (pythonTypeToGo . locatedValue <$> typeAnn)
    in name <> " map[string]" <> valueType
  ParamKwOnly (Identifier name) typeAnn defaultExpr ->
    let typeFromAnnotation = pythonTypeToGo . locatedValue <$> typeAnn
        typeFromDefault = pythonTypeToGoLiteral env <$> defaultExpr
        paramType = fromMaybe "interface{}" (typeFromAnnotation <|> typeFromDefault)
    in name <> " " <> paramType

-- | Generate expression
generateExpression :: GoGenEnv -> Located PythonExpr -> Text
generateExpression env (Located _ expr) = case expr of
  PyVar (Identifier name) -> name

  PyLiteral lit -> case lit of
    PyInt n -> T.pack (show n)
    PyFloat d -> T.pack (show d)
    PyString s -> "\"" <> s <> "\""
    PyBool b -> if b then "true" else "false"
    PyNone -> "nil"
    PyComplex _ _ -> "0" -- Complex numbers not directly supported in Go
    PyFString _ _ -> "\"\"" -- F-strings should be handled separately
    PyBytes _ -> "[]byte{}" -- Byte literals
    PyEllipsis -> "nil" -- Ellipsis not directly supported in Go

  PyBinaryOp op left right ->
    let leftExpr = generateExpression env left
        rightExpr = generateExpression env right
        goOp = case op of
                 OpAdd -> "+"
                 OpSub -> "-"
                 OpMul -> "*"
                 OpDiv -> "/"
                 OpMod -> "%"
                 _ -> "+"
    in "(" <> leftExpr <> " " <> goOp <> " " <> rightExpr <> ")"

  PyUnaryOp op operand ->
    let operandExpr = generateExpression env operand
        goOp = case op of
                 OpNegate -> "-"
                 OpPositive -> "+"
                 _ -> "-"
    in goOp <> operandExpr

  PyCall func args ->
    let funcExpr = generateExpression env func
        argExprs = map extractArg args
        argStr = T.intercalate ", " argExprs
        extractArg (Located _ arg) = case arg of
          ArgPositional a -> generateExpression env a
          ArgKeyword _ a -> generateExpression env a
          ArgStarred a -> generateExpression env a
          ArgKwStarred a -> generateExpression env a
    in case funcExpr of
         "print" -> case argExprs of
                       [] -> "fmt.Println()"
                       [arg] | T.all (\c -> c >= '0' && c <= '9') arg -> "fmt.Println(" <> arg <> ")"
                       [arg] | not (T.null arg) && T.head arg == '"' -> "fmt.Println(" <> arg <> ")"
                       [arg] -> "fmt.Println(" <> arg <> ")"
                       _ -> "fmt.Println(" <> argStr <> ")"
         "println" -> "fmt.Println(" <> argStr <> ")"
         _ -> funcExpr <> "(" <> argStr <> ")"

  PySubscript exprLoc (Located _ (SliceIndex index)) ->
    let exprStr = generateExpression env exprLoc
        indexStr = generateExpression env index
    in exprStr <> "[" <> indexStr <> "]"

  PyList elements ->
    let elemExprs = map (generateExpression env) elements
        elemType = inferElementType env elements
    in "[]" <> elemType <> "{" <> T.intercalate ", " elemExprs <> "}"

  PyTuple elements ->
    let elemExprs = map (generateExpression env) elements
    in "[]interface{}{" <> T.intercalate ", " elemExprs <> "}"

  PySet elements ->
    let setType = inferElementType env elements
    in "map[" <> setType <> "]struct{}{}"

  PyDict pairs ->
    let keyType = inferDictKeyType env pairs
        valueType = inferDictValueType env pairs
        entries = map (\(k, v) -> generateExpression env k <> ": " <> generateExpression env v) pairs
    in "map[" <> keyType <> "]" <> valueType <> "{" <> T.intercalate ", " entries <> "}"

  _ -> "0"

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
inferElementType :: GoGenEnv -> [Located PythonExpr] -> Text
inferElementType _ [] = "interface{}"
inferElementType env (x:xs) =
  foldl' combineGoTypes (inferGoType env x) (map (inferGoType env) xs)

-- | Infer key type for dict literals
inferDictKeyType :: GoGenEnv -> [(Located PythonExpr, Located PythonExpr)] -> Text
inferDictKeyType _ [] = "interface{}"
inferDictKeyType env ((k, _):rest) =
  foldl' combineGoTypes (inferGoType env k) (map (inferGoType env . fst) rest)

-- | Infer value type for dict literals
inferDictValueType :: GoGenEnv -> [(Located PythonExpr, Located PythonExpr)] -> Text
inferDictValueType _ [] = "interface{}"
inferDictValueType env ((_, v):rest) =
  foldl' combineGoTypes (inferGoType env v) (map (inferGoType env . snd) rest)

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

-- | Attempt to refine type from analysis annotations
inferGoTypeFromAnnotations :: GoGenEnv -> Located PythonExpr -> Maybe Text
inferGoTypeFromAnnotations env exprLoc = do
  common <- either (const Nothing) Just (pythonExprToCommon exprLoc)
  let key = renderCommonExpr common
  anns <- lookupAnnotations key (ggeAnnotations env)
  inferred <- eaInferredType anns
  let mapped = mapCommonTypeToGo inferred
  if T.null mapped then Nothing else Just mapped

-- | Infer Go type from Python expression with analysis fallback
inferGoType :: GoGenEnv -> Located PythonExpr -> Text
inferGoType env exprLoc =
  fromMaybe (inferGoTypeHeuristic env (locValue exprLoc))
            (inferGoTypeFromAnnotations env exprLoc)

-- | Heuristic Go type inference when analysis is unavailable
inferGoTypeHeuristic :: GoGenEnv -> PythonExpr -> Text
inferGoTypeHeuristic env expr = case expr of
  PyLiteral lit -> case lit of
    PyInt _ -> "int"
    PyFloat _ -> "float64"
    PyString _ -> "string"
    PyBool _ -> "bool"
    PyNone -> "interface{}"
    PyComplex _ _ -> "complex128"
    PyFString _ _ -> "string"
    PyBytes _ -> "[]byte"
    PyEllipsis -> "interface{}"
  PyConst qn -> goTypeFromSimpleName (identifierText (qnName qn))
  PyVar _ -> "interface{}"
  PyList elements -> "[]" <> inferElementType env elements
  PyTuple elements -> "[]" <> inferElementType env elements
  PySet elements -> "map[" <> inferElementType env elements <> "]struct{}"
  PyDict pairs -> "map[" <> inferDictKeyType env pairs <> "]" <> inferDictValueType env pairs
  PyBinaryOp op left right -> inferBinaryOpType env op left right
  PyUnaryOp op operand -> case op of
    OpNot -> "bool"
    _ -> inferGoType env operand
  PyBoolOp _ _ -> "bool"
  PyComparison _ _ -> "bool"
  PyIfExp _ thenExpr elseExpr -> combineGoTypes (inferGoType env thenExpr) (inferGoType env elseExpr)
  PyCall func _ -> inferCallReturnType env (locValue func)
  PyNamedExpr _ valueExpr -> inferGoType env valueExpr
  PyAttribute {} -> "interface{}"
  PySubscript container _ ->
    let containerType = inferGoType env container
    in if isListType containerType
         then stripListPrefix containerType
         else if isMapType containerType
                then snd (splitMapType containerType)
                else "interface{}"
  PyAwait awaited -> inferGoType env awaited
  PyJoinedStr _ -> "string"
  PyFormatSpec _ -> "string"
  PyListComp _ _ -> "[]interface{}"
  PySetComp _ _ -> "map[interface{}]struct{}"
  PyDictComp _ _ _ -> "map[interface{}]interface{}"
  PyGenComp _ _ -> "[]interface{}"
  _ -> "interface{}"

-- | Infer return type from binary operation
inferBinaryOpType :: GoGenEnv -> BinaryOp -> Located PythonExpr -> Located PythonExpr -> Text
inferBinaryOpType env op left right = case op of
  OpAdd -> promoteNumericTypes (inferGoType env left) (inferGoType env right)
  OpSub -> promoteNumericTypes (inferGoType env left) (inferGoType env right)
  OpMul -> promoteNumericTypes (inferGoType env left) (inferGoType env right)
  OpDiv -> promoteNumericTypes (inferGoType env left) (inferGoType env right)
  OpMod -> promoteNumericTypes (inferGoType env left) (inferGoType env right)
  OpPow -> promoteNumericTypes (inferGoType env left) (inferGoType env right)
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
inferCallReturnType :: GoGenEnv -> PythonExpr -> Text
inferCallReturnType env expr = case expr of
  PyVar (Identifier "range") -> "[]int"
  PyVar (Identifier "len") -> "int"
  PyVar (Identifier "str") -> "string"
  PyVar (Identifier "int") -> "int"
  PyVar (Identifier "float") -> "float64"
  PyVar (Identifier "bool") -> "bool"
  PyVar (Identifier "list") -> "[]interface{}"
  PyVar (Identifier "dict") -> "map[interface{}]interface{}"
  PyVar (Identifier "set") -> "map[interface{}]bool"
  _ ->
    case inferGoTypeFromAnnotations env (noLoc expr) of
      Just annotated -> annotated
      Nothing -> "interface{}"

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
pythonTypeToGoLiteral :: GoGenEnv -> Located PythonExpr -> Text
pythonTypeToGoLiteral env exprLoc = inferGoType env exprLoc

-- | Infer return type from function body by looking at return statements
inferReturnTypeFromBody :: GoGenEnv -> [Located PythonStmt] -> Text
inferReturnTypeFromBody env stmts =
  let returnTypes = mapMaybe extractReturnType stmts
  in case returnTypes of
    [] -> ""
    (t:_) -> t
  where
    extractReturnType :: Located PythonStmt -> Maybe Text
    extractReturnType (Located _ stmt) = case stmt of
      PyReturn (Just exprLoc) -> Just (inferGoType env exprLoc)
      PyReturn Nothing -> Just ""
      _ -> Nothing

-- | Handle range calls in for loops
handleRangeLoop :: Text -> Text -> [Text] -> [Text]
handleRangeLoop varName rangeCall body =
  case parseRangeCall rangeCall of
    Just n -> ["for " <> varName <> " := 0; " <> varName <> " < " <> n <> "; " <> varName <> "++ {"] ++ map ("\t" <>) body ++ ["}"]
    Nothing -> ["for " <> varName <> " := range " <> rangeCall <> " {"] ++ map ("\t" <>) body ++ ["}"]
  where
    parseRangeCall :: Text -> Maybe Text
    parseRangeCall call =
      if "range(" `T.isPrefixOf` call && ")" `T.isSuffixOf` call
      then Just $ T.take (T.length call - 7) (T.drop 6 call)
      else Nothing
