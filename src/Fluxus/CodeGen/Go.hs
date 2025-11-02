{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Go code generation module
module Fluxus.CodeGen.Go
  ( -- * Code generation functions
    generateGoFromPython
  , generateGoCode
    -- * Configuration
  , GoGenConfig(..)
  , defaultGoConfig
  ) where

-- import Control.Monad.State  -- unused
-- import Control.Monad.Writer  -- unused
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Fluxus.AST.Common
import Fluxus.AST.Python

-- | Go code generation configuration
data GoGenConfig = GoGenConfig
  { ggcPackageName :: !Text
  , ggcEnableFmt   :: !Bool
  , ggcImportMap   :: !(HashMap Text Text)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

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

-- | Generate Go code from Python AST
generateGoFromPython :: PythonAST -> GoGenConfig -> Text
generateGoFromPython (PythonAST module_) config =
  let (header, body) = generateModule module_ config
  in header <> "\n" <> body

-- | Generate module header
_generateModuleHeader :: GoGenConfig -> Text
_generateModuleHeader config =
  T.unlines
    [ "package " <> ggcPackageName config
    , ""
    , "import ("
    , "\t\"fmt\""
    , ")"
    , ""
    ]

-- | Generate complete module
generateModule :: PythonModule -> GoGenConfig -> (Text, Text)
generateModule pyModule config =
  let imports = generateImports pyModule config
      decls = generateDeclarations pyModule config
  in (imports, decls)

-- | Generate imports
generateImports :: PythonModule -> GoGenConfig -> Text
generateImports _pyModule config =
  let header = "package " <> ggcPackageName config
      imports = case ggcEnableFmt config of
                  True -> ["\t\"fmt\""]
                  False -> []
  in if null imports
     then header
     else T.unlines (header : "" : "import (" : imports ++ [")"])

-- | Generate all declarations
generateDeclarations :: PythonModule -> GoGenConfig -> Text
generateDeclarations pyModule config =
  let stmts = concatMap (generateStatement config . locatedValue) (pyModuleBody pyModule)
  in T.unlines stmts

-- | Generate Go code as text
generateGoCode :: PythonAST -> GoGenConfig -> Text
generateGoCode ast config =
  let (header, body) = generateModule (pyModule ast) config
  in header <> "\n" <> body

-- | Generate statement
generateStatement :: GoGenConfig -> PythonStmt -> [Text]
generateStatement config stmt = case stmt of
  PyExprStmt (Located _ expr) ->
    let goExpr = generateExpression config expr
    in [goExpr]
    
  PyAssign targets (Located _ value) ->
    case targets of
      [Located _ (PatVar (Identifier name))] ->
        let goValue = generateExpression config value
            goType = inferGoType value
        in ["var " <> name <> " " <> goType <> " = " <> goValue]
      _ -> []
  
  PyAnnAssign (Located _ (PatVar (Identifier name))) typeExpr maybeValue ->
    let goType = pythonTypeToGo (locatedValue typeExpr)
        assignment = case maybeValue of
                       Just (Located _ value) -> " = " <> generateExpression config value
                       Nothing -> ""
    in ["var " <> name <> " " <> goType <> assignment]
      
  PyIf (Located _ cond) thenBody elseBody ->
    let goCond = generateExpression config cond
        goThen = concatMap (\(Located _ stmt) -> generateStatement config stmt) thenBody
        goElse = case elseBody of
                  [] -> []
                  body -> concatMap (\(Located _ stmt) -> generateStatement config stmt) body
    in ["if " <> goCond <> " {"] ++
       map ("\t" <>) goThen ++
       ["}"] ++
       (if null goElse then [] else ["else {"] ++ map ("\t" <>) goElse ++ ["}"])
    
  PyFor (Located _ (PatVar (Identifier varName))) (Located _ iterExpr) body elseBody ->
    let goIter = generateExpression config iterExpr
        goBody = concatMap (\(Located _ stmt) -> generateStatement config stmt) body
        rangeHandled = handleRangeLoop varName goIter goBody
    in rangeHandled
    
  PyFuncDef funcDef ->
    generateFunctionDef config funcDef
    
  PyReturn (Just (Located _ expr)) ->
    let goExpr = generateExpression config expr
    in ["return " <> goExpr]
  PyReturn Nothing ->
    ["return"]
    
  _ -> []

-- | Generate function definition
generateFunctionDef :: GoGenConfig -> PythonFuncDef -> [Text]
generateFunctionDef config funcDef =
  let Identifier funcName = pyFuncName funcDef
      params = map (generateParameter config) (pyFuncParams funcDef)
      paramStr = if null params then "" else T.intercalate ", " params
      body = concatMap (\(Located _ stmt) -> generateStatement config stmt) (pyFuncBody funcDef)
      returnType = case pyFuncReturns funcDef of
                     Just locTypeExpr -> pythonTypeToGo (locatedValue locTypeExpr)
                     Nothing -> if funcName == "main" then "int" else inferReturnTypeFromBody (pyFuncBody funcDef)
      funcSignature = "func " <> funcName <> "(" <> paramStr <> ")" <> formatReturnType returnType
  in [funcSignature <> " {"] ++
     map ("\t" <>) body ++
     (if funcName == "main" && not (hasReturnStatement (pyFuncBody funcDef)) then ["\treturn 0"] else []) ++
     ["}"]
  where
    formatReturnType t
      | T.null t = ""
      | otherwise = " " <> t
    hasReturnStatement stmts = any isReturnStmt stmts
    isReturnStmt (Located _ (PyReturn _)) = True
    isReturnStmt _ = False

-- | Generate parameter
generateParameter :: GoGenConfig -> Located PythonParameter -> Text
generateParameter _ (Located _ param) = case param of
  ParamNormal (Identifier name) typeAnn defaultExpr ->
    let typeFromAnnotation = pythonTypeToGo . locatedValue <$> typeAnn
        typeFromDefault = pythonTypeToGoLiteral <$> defaultExpr
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
        typeFromDefault = pythonTypeToGoLiteral <$> defaultExpr
        paramType = fromMaybe "interface{}" (typeFromAnnotation <|> typeFromDefault)
    in name <> " " <> paramType

-- | Generate expression
generateExpression :: GoGenConfig -> PythonExpr -> Text
generateExpression config expr = case expr of
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
    let leftExpr = generateExpression config (locatedValue left)
        rightExpr = generateExpression config (locatedValue right)
        goOp = case op of
                 OpAdd -> "+"
                 OpSub -> "-"
                 OpMul -> "*"
                 OpDiv -> "/"
                 OpMod -> "%"
                 _ -> "+"
    in "(" <> leftExpr <> " " <> goOp <> " " <> rightExpr <> ")"
    
  PyUnaryOp op (Located _ operand) ->
    let operandExpr = generateExpression config operand
        goOp = case op of
                 OpNegate -> "-"
                 OpPositive -> "+"
                 _ -> "-"
    in goOp <> operandExpr
    
  PyCall (Located _ func) args ->
    let funcExpr = generateExpression config func
        argExprs = map extractArg args
        argStr = T.intercalate ", " argExprs
        extractArg (Located _ arg) = case arg of
          ArgPositional a -> generateExpression config (locatedValue a)
          ArgKeyword _ a -> generateExpression config (locatedValue a)
          ArgStarred a -> generateExpression config (locatedValue a)
          ArgKwStarred a -> generateExpression config (locatedValue a)
    in case funcExpr of
         "print" -> case argExprs of
                      [] -> "fmt.Println()"
                      [arg] | T.all (\c -> c >= '0' && c <= '9') arg -> "fmt.Println(" <> arg <> ")"
                      [arg] | T.head arg == '"' -> "fmt.Println(" <> arg <> ")"
                      [arg] -> "fmt.Println(" <> arg <> ")"
                      _ -> "fmt.Println(" <> argStr <> ")"
         "println" -> "fmt.Println(" <> argStr <> ")"
         _ -> funcExpr <> "(" <> argStr <> ")"
    
  PySubscript (Located _ expr) (Located _ (SliceIndex index)) ->
    let exprStr = generateExpression config expr
        indexStr = generateExpression config (locatedValue index)
    in exprStr <> "[" <> indexStr <> "]"
    
  PyList elements ->
    let elemExprs = map (\(Located _ elem) -> generateExpression config elem) elements
        elemType = inferElementType elements
    in "[]" <> elemType <> "{" <> T.intercalate ", " elemExprs <> "}"
    
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

-- | Infer element type for list/tuple/set literals
inferElementType :: [Located PythonExpr] -> Text
inferElementType [] = "interface{}"
inferElementType (x:xs) =
  foldl' combineGoTypes (inferGoType (locatedValue x)) (map (inferGoType . locatedValue) xs)

-- | Infer key type for dict literals
inferDictKeyType :: [(Located PythonExpr, Located PythonExpr)] -> Text
inferDictKeyType [] = "interface{}"
inferDictKeyType ((k, _):rest) =
  foldl' combineGoTypes (inferGoType (locatedValue k)) (map (inferGoType . locatedValue . fst) rest)

-- | Infer value type for dict literals
inferDictValueType :: [(Located PythonExpr, Located PythonExpr)] -> Text
inferDictValueType [] = "interface{}"
inferDictValueType ((_, v):rest) =
  foldl' combineGoTypes (inferGoType (locatedValue v)) (map (inferGoType . locatedValue . snd) rest)

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

-- | Infer Go type from Python expression
inferGoType :: PythonExpr -> Text
inferGoType expr = case expr of
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
  PyList elements -> "[]" <> inferElementType elements
  PyTuple elements -> "[]" <> inferElementType elements
  PySet elements -> "map[" <> inferElementType elements <> "]struct{}"
  PyDict pairs -> "map[" <> inferDictKeyType pairs <> "]" <> inferDictValueType pairs
  PyBinaryOp op left right -> inferBinaryOpType op (locatedValue left) (locatedValue right)
  PyUnaryOp op (Located _ operand) -> case op of
    OpNot -> "bool"
    _ -> inferGoType operand
  PyBoolOp _ _ -> "bool"
  PyComparison _ _ -> "bool"
  PyIfExp _ thenExpr elseExpr -> combineGoTypes (inferGoType (locatedValue thenExpr)) (inferGoType (locatedValue elseExpr))
  PyCall (Located _ func) _ -> inferCallReturnType func
  PyNamedExpr _ (Located _ valueExpr) -> inferGoType valueExpr
  PyAttribute {} -> "interface{}"
  PySubscript (Located _ container) _ ->
    let containerType = inferGoType container
    in if isListType containerType
       then stripListPrefix containerType
       else if isMapType containerType
            then snd (splitMapType containerType)
            else "interface{}"
  PyAwait (Located _ awaited) -> inferGoType awaited
  PyJoinedStr _ -> "string"
  PyFormatSpec _ -> "string"
  PyListComp _ _ -> "[]interface{}"
  PySetComp _ _ -> "map[interface{}]struct{}"
  PyDictComp _ _ _ -> "map[interface{}]interface{}"
  PyGenComp _ _ -> "[]interface{}"
  _ -> "interface{}"

-- | Infer return type from binary operation
inferBinaryOpType :: BinaryOp -> PythonExpr -> PythonExpr -> Text
inferBinaryOpType op left right = case op of
  OpAdd -> promoteNumericTypes (inferGoType left) (inferGoType right)
  OpSub -> promoteNumericTypes (inferGoType left) (inferGoType right)
  OpMul -> promoteNumericTypes (inferGoType left) (inferGoType right)
  OpDiv -> promoteNumericTypes (inferGoType left) (inferGoType right)
  OpMod -> promoteNumericTypes (inferGoType left) (inferGoType right)
  OpPow -> promoteNumericTypes (inferGoType left) (inferGoType right)
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

-- | Infer return type from function call
inferCallReturnType :: PythonExpr -> Text
inferCallReturnType expr = case expr of
  PyVar (Identifier "range") -> "[]int"
  PyVar (Identifier "len") -> "int"
  PyVar (Identifier "str") -> "string"
  PyVar (Identifier "int") -> "int"
  PyVar (Identifier "float") -> "float64"
  PyVar (Identifier "bool") -> "bool"
  PyVar (Identifier "list") -> "[]interface{}"
  PyVar (Identifier "dict") -> "map[interface{}]interface{}"
  PyVar (Identifier "set") -> "map[interface{}]bool"
  _ -> "interface{}"

-- | Convert Python type annotation to Go type
pythonTypeToGo :: PythonTypeExpr -> Text
pythonTypeToGo typeExpr = case typeExpr of
  TypeName qn -> qualifiedNameToGo qn
  TypeVar tv -> tv
  TypeSubscript (Located _ base) args -> case base of
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
  TypeUnion types -> "interface{}"
  TypeOptional (Located _ t) -> "*" <> pythonTypeToGo t
  TypeCallable _ (Located _ ret) -> "func() " <> pythonTypeToGo ret
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
pythonTypeToGoLiteral :: Located PythonExpr -> Text
pythonTypeToGoLiteral (Located _ expr) = inferGoType expr

-- | Infer return type from function body by looking at return statements
inferReturnTypeFromBody :: [Located PythonStmt] -> Text
inferReturnTypeFromBody stmts =
  let returnTypes = mapMaybe extractReturnType stmts
  in case returnTypes of
    [] -> ""
    (t:_) -> t
  where
    extractReturnType :: Located PythonStmt -> Maybe Text
    extractReturnType (Located _ stmt) = case stmt of
      PyReturn (Just (Located _ expr)) -> Just (inferGoType expr)
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