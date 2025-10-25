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
      returnType = if funcName == "main" then "int" else "int"
  in ["func " <> funcName <> "(" <> paramStr <> ") " <> returnType <> " {"] ++
     map ("\t" <>) body ++
     (if funcName == "main" && not (hasReturnStatement (pyFuncBody funcDef)) then ["\treturn 0"] else []) ++
     ["}"]
  where
    hasReturnStatement stmts = any isReturnStmt stmts
    isReturnStmt (Located _ (PyReturn _)) = True
    isReturnStmt _ = False

-- | Generate parameter
generateParameter :: GoGenConfig -> Located PythonParameter -> Text
generateParameter config (Located _ param) = case param of
  ParamNormal (Identifier name) typeAnn _ ->
    let paramType = case typeAnn of
                      Just _ -> "int"
                      Nothing -> "int"
    in name <> " " <> paramType
  _ -> "_ int"

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
    in "[]int{" <> T.intercalate ", " elemExprs <> "}"
    
  _ -> "0"

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
  PyVar _ -> "int"  -- Default fallback
  PyList [] -> "[]int"
  PyList _ -> "[]int"
  _ -> "int"

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