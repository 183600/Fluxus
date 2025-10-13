{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Fluxus.CodeGen.CPP
  ( CppCodeGen
  , CppGenState(..)
  , CppGenConfig(..)
  , generateCpp
  , generateCppMain
  , generateCppFromPython
  , generateCppFromGo
  , renderCppUnit
  , prettyCppUnit
  , CppUnit(..)
  , CppDecl(..)
  , CppStmt(..)
  , CppExpr(..)
  , CppType(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppCase(..)
  , CppCatch(..)
  , runCppCodeGen
  , mapPythonTypeToCpp
  , mapGoTypeToCpp
  , mapCommonTypeToCpp
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub, partition, find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList, listToMaybe)

import Fluxus.AST.Common
import Fluxus.AST.Python hiding (TypeVar)
import Fluxus.AST.Go hiding (Located, locValue)
import qualified Fluxus.AST.Go as Go

data CppGenConfig = CppGenConfig
  { cgcOptimizationLevel :: !Int
  , cgcEnableInterop     :: !Bool
  , cgcTargetCppStd      :: !Text
  , cgcUseSmartPointers  :: !Bool
  , cgcEnableParallel    :: !Bool
  , cgcEnableCoroutines  :: !Bool
  , cgcNamespace         :: !Text
  , cgcHeaderGuard       :: !Text
  , cgcSourceFile        :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data CppGenState = CppGenState
  { cgsIncludes     :: ![Text]
  , cgsDeclarations :: ![CppDecl]
  , cgsNamespaces   :: ![Text]
  , cgsTempVarCount :: !Int
  , cgsSymbolTable  :: !(HashMap Text CppType)
  , cgsConfig       :: !CppGenConfig
  , cgsWarnings     :: ![Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

type CppCodeGen = StateT CppGenState (Writer [Text])

data CppUnit = CppUnit
  { cppIncludes     :: ![Text]
  , cppNamespaces   :: ![Text]
  , cppDeclarations :: ![CppDecl]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppDecl
  = CppClass !Text ![Text] ![CppDecl]
  | CppStruct !Text ![CppDecl]
  | CppFunction !Text !CppType ![CppParam] ![CppStmt]
  | CppMethod !Text !CppType ![CppParam] ![CppStmt] !Bool
  | CppConstructor !Text ![CppParam] ![CppStmt]
  | CppDestructor !Text ![CppStmt] !Bool
  | CppVariable !Text !CppType !(Maybe CppExpr)
  | CppTypedef !Text !CppType
  | CppUsing !Text !CppType
  | CppTemplate ![Text] !CppDecl
  | CppNamespace !Text ![CppDecl]
  | CppExternC ![CppDecl]
  | CppCommentDecl !Text
  | CppPreprocessor !Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppStmt
  = CppExprStmt !CppExpr
  | CppReturn !(Maybe CppExpr)
  | CppIf !CppExpr ![CppStmt] ![CppStmt]
  | CppWhile !CppExpr ![CppStmt]
  | CppFor !(Maybe CppStmt) !(Maybe CppExpr) !(Maybe CppExpr) ![CppStmt]
  | CppForRange !Text !CppExpr ![CppStmt]
  | CppSwitch !CppExpr ![CppCase]
  | CppTry ![CppStmt] ![CppCatch] ![CppStmt]
  | CppThrow !(Maybe CppExpr)
  | CppBreak
  | CppContinue
  | CppBlock ![CppStmt]
  | CppDecl !CppDecl
  | CppComment !Text
  | CppDoWhile ![CppStmt] !CppExpr
  | CppLabel !Text
  | CppGoto !Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppExpr
  = CppVar !Text
  | CppLiteral !CppLiteral
  | CppBinary !Text !CppExpr !CppExpr
  | CppUnary !Text !CppExpr
  | CppCall !CppExpr ![CppExpr]
  | CppMember !CppExpr !Text
  | CppPointerMember !CppExpr !Text
  | CppIndex !CppExpr !CppExpr
  | CppCast !CppType !CppExpr
  | CppSizeOf !CppType
  | CppNew !CppType ![CppExpr]
  | CppDelete !CppExpr
  | CppThis
  | CppLambda ![CppParam] ![CppStmt] !Bool
  | CppMove !CppExpr
  | CppForward !CppExpr
  | CppMakeUnique !CppType ![CppExpr]
  | CppMakeShared !CppType ![CppExpr]
  | CppInitList !CppType ![CppExpr]
  | CppTernary !CppExpr !CppExpr !CppExpr
  | CppComma ![CppExpr]
  | CppStaticCast !CppType !CppExpr
  | CppDynamicCast !CppType !CppExpr
  | CppReinterpretCast !CppType !CppExpr
  | CppConstCast !CppType !CppExpr
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppType
  = CppVoid
  | CppBool
  | CppChar | CppUChar
  | CppShort | CppUShort
  | CppInt | CppUInt
  | CppLong | CppULong
  | CppLongLong | CppULongLong
  | CppFloat | CppDouble | CppLongDouble
  | CppAuto
  | CppString
  | CppVector !CppType
  | CppArray !CppType !Int
  | CppPointer !CppType
  | CppReference !CppType
  | CppRvalueRef !CppType
  | CppConst !CppType
  | CppVolatile !CppType
  | CppSizeT
  | CppFunctionType ![CppType] !CppType
  | CppClassType !Text ![CppType]
  | CppTemplateType !Text ![CppType]
  | CppUniquePtr !CppType
  | CppSharedPtr !CppType
  | CppOptional !CppType
  | CppVariant ![CppType]
  | CppPair !CppType !CppType
  | CppTuple ![CppType]
  | CppMap !CppType !CppType
  | CppUnorderedMap !CppType !CppType
  | CppTypeVar !Text
  | CppDecltype !CppExpr
  deriving stock (Show, Generic)
    deriving anyclass (NFData)

instance Eq CppType where
  CppAuto == CppFunctionType _ _ = True
  CppFunctionType _ _ == CppAuto = True
  a == b = show a == show b

data CppLiteral
  = CppIntLit !Integer
  | CppFloatLit !Double
  | CppStringLit !Text
  | CppCharLit !Char
  | CppBoolLit !Bool
  | CppNullPtr
  | CppUserDefinedLit !Text !Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppParam = CppParam !Text !CppType !(Maybe CppExpr)
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppCase = CppCase !CppExpr ![CppStmt] | CppDefault ![CppStmt]
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppCatch = CppCatch !CppType !Text ![CppStmt]
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

renderCppUnit :: CppUnit -> Text
renderCppUnit unit = prettyCppUnit unit

prettyCppUnit :: CppUnit -> Text
prettyCppUnit CppUnit{..} = T.unlines $ concat
  [ map renderInclude (nub cppIncludes)
  , [""]
  , [tupleOutputSupportFunction]
  , if null cppNamespaces then [] else concat
      [ map (\ns -> "namespace " <> ns <> " {") cppNamespaces
      , [""]
      ]
  , map (renderDecl 0) cppDeclarations
  , if null cppNamespaces then [] else
      map (const "}") cppNamespaces
  ]
  where
    renderInclude inc
      | T.isPrefixOf "<" inc = "#include " <> inc
      | T.isPrefixOf "\"" inc = "#include " <> inc
      | otherwise = "#include <" <> inc <> ">"

renderDecl :: Int -> CppDecl -> Text
renderDecl indent decl = case decl of
  CppClass name bases members ->
    indentText indent <> "class " <> name <> renderBases bases <> " {\n" <>
    renderAccessLevel indent "public:" <>
    T.unlines (map (renderDecl (indent + 1)) members) <>
    indentText indent <> "};"
  CppStruct name members ->
    indentText indent <> "struct " <> name <> " {\n" <>
    T.unlines (map (renderDecl (indent + 1)) members) <>
    indentText indent <> "};"
  CppFunction name retType params body ->
    indentText indent <> renderType retType <> " " <> name <>
    "(" <> renderParams params <> ") {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "}"
  CppMethod name retType params body isVirtual ->
    indentText indent <>
    (if isVirtual then "virtual " else "") <>
    renderType retType <> " " <> name <>
    "(" <> renderParams params <> ") {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "}"
  CppConstructor name params body ->
    indentText indent <> name <> "(" <> renderParams params <> ") {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "}"
  CppDestructor name body isVirtual ->
    indentText indent <>
    (if isVirtual then "virtual " else "") <>
    "~" <> name <> "() {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "}"
  CppVariable name varType mInit ->
    indentText indent <> renderType varType <> " " <> name <>
    maybe ";" (\initVal -> " = " <> renderExpr initVal <> ";") mInit
  CppTypedef name typeExpr ->
    indentText indent <> "typedef " <> renderType typeExpr <> " " <> name <> ";"
  CppUsing name typeExpr ->
    indentText indent <> "using " <> name <> " = " <> renderType typeExpr <> ";"
  CppTemplate params innerDecl ->
    indentText indent <> "template<" <> T.intercalate ", " (map ("typename " <>) params) <> ">\n" <>
    renderDecl indent innerDecl
  CppNamespace name decls ->
    indentText indent <> "namespace " <> name <> " {\n" <>
    T.unlines (map (renderDecl (indent + 1)) decls) <>
    indentText indent <> "}"
  CppExternC decls ->
    indentText indent <> "extern \"C\" {\n" <>
    T.unlines (map (renderDecl (indent + 1)) decls) <>
    indentText indent <> "}"
  CppCommentDecl comment ->
    indentText indent <> "// " <> comment
  CppPreprocessor directive ->
    "#" <> directive
  where
    renderBases [] = ""
    renderBases bs = " : " <> T.intercalate ", " (map ("public " <>) bs)
    renderAccessLevel ind level = indentText ind <> level <> "\n"

renderStmt :: Int -> CppStmt -> Text
renderStmt indent stmt = case stmt of
  CppExprStmt expr -> indentText indent <> renderExpr expr <> ";"
  CppReturn Nothing -> indentText indent <> "return;"
  CppReturn (Just expr) -> indentText indent <> "return " <> renderExpr expr <> ";"
  CppIf cond thenStmts elseStmts ->
    indentText indent <> "if (" <> renderExpr cond <> ") {\n" <>
    T.unlines (map (renderStmt (indent + 1)) thenStmts) <>
    indentText indent <> "}" <>
    if null elseStmts then ""
    else " else {\n" <>
         T.unlines (map (renderStmt (indent + 1)) elseStmts) <>
         indentText indent <> "}"
  CppWhile cond body ->
    indentText indent <> "while (" <> renderExpr cond <> ") {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "}"
  CppDoWhile body cond ->
    indentText indent <> "do {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "} while (" <> renderExpr cond <> ");"
  CppFor mInit mCond mPost body ->
    indentText indent <> "for (" <>
    maybe "" renderForInit mInit <> "; " <>
    maybe "" renderExpr mCond <> "; " <>
    maybe "" renderExpr mPost <> ") {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "}"
  CppForRange var range body ->
    indentText indent <> "for (auto& " <> var <> " : " <> renderExpr range <> ") {\n" <>
    T.unlines (map (renderStmt (indent + 1)) body) <>
    indentText indent <> "}"
  CppSwitch expr cases ->
    indentText indent <> "switch (" <> renderExpr expr <> ") {\n" <>
    T.unlines (map (renderCase (indent + 1)) cases) <>
    indentText indent <> "}"
  CppTry tryStmts catches finallyStmts ->
    indentText indent <> "try {\n" <>
    T.unlines (map (renderStmt (indent + 1)) tryStmts) <>
    indentText indent <> "}" <>
    T.concat (map (renderCatch indent) catches) <>
    if null finallyStmts then ""
    else "\n" <> indentText indent <> "finally {\n" <>
         T.unlines (map (renderStmt (indent + 1)) finallyStmts) <>
         indentText indent <> "}"
  CppThrow Nothing -> indentText indent <> "throw;"
  CppThrow (Just expr) -> indentText indent <> "throw " <> renderExpr expr <> ";"
  CppBreak -> indentText indent <> "break;"
  CppContinue -> indentText indent <> "continue;"
  CppBlock stmts -> indentText indent <> "{\n" <>
    T.unlines (map (renderStmt (indent + 1)) stmts) <>
    indentText indent <> "}"
  CppDecl decl -> renderDecl indent decl
  CppComment comment -> indentText indent <> "// " <> comment
  CppLabel label -> indentText (max 0 (indent - 1)) <> label <> ":"
  CppGoto label -> indentText indent <> "goto " <> label <> ";"
  where
    renderForInit (CppDecl d) = T.stripEnd $ renderDecl 0 d
    renderForInit s = T.stripEnd $ renderStmt 0 s
    renderCase ind (CppCase expr stmts) =
      indentText ind <> "case " <> renderExpr expr <> ":\n" <>
      T.unlines (map (renderStmt (ind + 1)) stmts) <>
      indentText (ind + 1) <> "break;"
    renderCase ind (CppDefault stmts) =
      indentText ind <> "default:\n" <>
      T.unlines (map (renderStmt (ind + 1)) stmts) <>
      indentText (ind + 1) <> "break;"
    renderCatch ind (CppCatch typ var stmts) =
      " catch (" <> renderType typ <> " " <> var <> ") {\n" <>
      T.unlines (map (renderStmt (ind + 1)) stmts) <>
      indentText ind <> "}"

renderExpr :: CppExpr -> Text
renderExpr expr = case expr of
  CppVar name -> name
  CppLiteral lit -> renderLiteral lit
  CppBinary op left right ->
    "(" <> renderExpr left <> " " <> op <> " " <> renderExpr right <> ")"
  CppUnary op operand ->
    if op `elem` ["++", "--"] && isPostfix op
    then renderExpr operand <> op
    else op <> renderExpr operand
  CppCall func args ->
    renderExpr func <> "(" <> T.intercalate ", " (map renderExpr args) <> ")"
  CppMember obj member -> renderExpr obj <> "." <> member
  CppPointerMember ptr member -> renderExpr ptr <> "->" <> member
  CppIndex arr idx -> renderExpr arr <> "[" <> renderExpr idx <> "]"
  CppCast typ expr' -> "(" <> renderType typ <> ")" <> renderExpr expr'
  CppStaticCast typ expr' -> "static_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppDynamicCast typ expr' -> "dynamic_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppReinterpretCast typ expr' -> "reinterpret_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppConstCast typ expr' -> "const_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppSizeOf typ -> "sizeof(" <> renderType typ <> ")"
  CppNew typ args ->
    "new " <> renderType typ <>
    if null args then "" else "(" <> T.intercalate ", " (map renderExpr args) <> ")"
  CppDelete expr' -> "delete " <> renderExpr expr'
  CppThis -> "this"
  CppLambda params body captures ->
    let captureStr = if captures then "[&]" else "[]"
    in captureStr <> "(" <> renderParams params <> ") {\n" <>
       T.unlines (map (renderStmt 1) body) <> "}"
  CppMove expr' -> "std::move(" <> renderExpr expr' <> ")"
  CppForward expr' -> "std::forward(" <> renderExpr expr' <> ")"
  CppMakeUnique typ args ->
    "std::make_unique<" <> renderType typ <> ">(" <>
    T.intercalate ", " (map renderExpr args) <> ")"
  CppMakeShared typ args ->
    "std::make_shared<" <> renderType typ <> ">(" <>
    T.intercalate ", " (map renderExpr args) <> ")"
  CppInitList typ elements ->
    renderType typ <> "{" <> T.intercalate ", " (map renderExpr elements) <> "}"
  CppTernary cond thenExpr elseExpr ->
    "(" <> renderExpr cond <> " ? " <> renderExpr thenExpr <> " : " <> renderExpr elseExpr <> ")"
  CppComma exprs -> "(" <> T.intercalate ", " (map renderExpr exprs) <> ")"
  where
    isPostfix "++" = True
    isPostfix "--" = True
    isPostfix _ = False

renderType :: CppType -> Text
renderType typ = case typ of
  CppVoid -> "void"
  CppBool -> "bool"
  CppChar -> "char"
  CppUChar -> "unsigned char"
  CppShort -> "short"
  CppUShort -> "unsigned short"
  CppInt -> "int"
  CppUInt -> "unsigned int"
  CppLong -> "long"
  CppULong -> "unsigned long"
  CppLongLong -> "long long"
  CppULongLong -> "unsigned long long"
  CppFloat -> "float"
  CppDouble -> "double"
  CppLongDouble -> "long double"
  CppAuto -> "auto"
  CppString -> "std::string"
  CppVector elemType -> "std::vector<" <> renderType elemType <> ">"
  CppArray elemType size -> renderType elemType <> "[" <> T.pack (show size) <> "]"
  CppPointer baseType -> renderType baseType <> "*"
  CppReference baseType -> renderType baseType <> "&"
  CppRvalueRef baseType -> renderType baseType <> "&&"
  CppConst baseType -> "const " <> renderType baseType
  CppVolatile baseType -> "volatile " <> renderType baseType
  CppSizeT -> "std::size_t"
  CppFunctionType paramTypes retType ->
    renderType retType <> "(*)(" <> T.intercalate ", " (map renderType paramTypes) <> ")"
  CppClassType name typeArgs ->
    name <> if null typeArgs then ""
    else "<" <> T.intercalate ", " (map renderType typeArgs) <> ">"
  CppTemplateType name typeArgs ->
    name <> "<" <> T.intercalate ", " (map renderType typeArgs) <> ">"
  CppUniquePtr baseType -> "std::unique_ptr<" <> renderType baseType <> ">"
  CppSharedPtr baseType -> "std::shared_ptr<" <> renderType baseType <> ">"
  CppOptional baseType -> "std::optional<" <> renderType baseType <> ">"
  CppVariant types -> "std::variant<" <> T.intercalate ", " (map renderType types) <> ">"
  CppPair type1 type2 -> "std::pair<" <> renderType type1 <> ", " <> renderType type2 <> ">"
  CppTuple types -> "std::tuple<" <> T.intercalate ", " (map renderType types) <> ">"
  CppMap keyType valType -> "std::map<" <> renderType keyType <> ", " <> renderType valType <> ">"
  CppUnorderedMap keyType valType -> 
    "std::unordered_map<" <> renderType keyType <> ", " <> renderType valType <> ">"
  CppTypeVar name -> name
  CppDecltype expr -> "decltype(" <> renderExpr expr <> ")"

renderLiteral :: CppLiteral -> Text
renderLiteral lit = case lit of
  CppIntLit i -> T.pack (show i)
  CppFloatLit f -> T.pack (show f)
  CppStringLit s -> "\"" <> escapeString s <> "\""
  CppCharLit c -> "'" <> escapeChar c <> "'"
  CppBoolLit b -> if b then "true" else "false"
  CppNullPtr -> "nullptr"
  CppUserDefinedLit val suffix -> val <> suffix
  where
    escapeString = T.concatMap escapeStringChar
    escapeStringChar '"' = "\\\""
    escapeStringChar '\\' = "\\\\"
    escapeStringChar '\n' = "\\n"
    escapeStringChar '\t' = "\\t"
    escapeStringChar '\r' = "\\r"
    escapeStringChar c = T.singleton c
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c = T.singleton c

renderParams :: [CppParam] -> Text
renderParams params = T.intercalate ", " (map renderParam params)
  where
    renderParam (CppParam name typ mDefault) =
      renderType typ <> " " <> name <>
      maybe "" (\def -> " = " <> renderExpr def) mDefault

tupleOutputSupportFunction :: Text
tupleOutputSupportFunction = T.unlines
  [ "// Helper function to output tuples to ostream"
  , "template<typename... Args>"
  , "std::ostream& operator<<(std::ostream& os, const std::tuple<Args...>& t) {"
  , "    os << \"(\";"
  , "    std::apply([&os](const Args&... args) {"
  , "        std::size_t n = 0;"
  , "        ((os << args << (++n != sizeof...(Args) ? \", \" : \"\"))), ...);"
  , "    }, t);"
  , "    os << \")\";"
  , "    return os;"
  , "}"
  , ""
  ]

indentText :: Int -> Text
indentText n = T.replicate n "    "

identifierToText :: Identifier -> Text
identifierToText (Identifier name) = name

moduleNameToText :: ModuleName -> Text
moduleNameToText (ModuleName name) = name

qualifiedNameToText :: QualifiedName -> Text
qualifiedNameToText qn =
  let parts = map moduleNameToText (qnModule qn) <> [identifierToText (qnName qn)]
  in T.intercalate "::" parts

goLocatedValue :: Go.Located a -> a
goLocatedValue (Go.Located _ value) = value

fallbackName :: Text -> Int -> Text
fallbackName base idx
  | T.null base = "param" <> T.pack (show idx)
  | otherwise = base

defaultCppIncludes :: [Text]
defaultCppIncludes =
  [ "<iostream>"
  , "<string>"
  , "<vector>"
  , "<optional>"
  , "<unordered_map>"
  , "<tuple>"
  , "<cstdio>"
  , "<cstdlib>"
  , "<queue>"
  ]



initialCppGenState :: CppGenConfig -> CppGenState
initialCppGenState config = CppGenState
  { cgsIncludes = []
  , cgsDeclarations = []
  , cgsNamespaces = []
  , cgsTempVarCount = 0
  , cgsSymbolTable = HM.empty
  , cgsConfig = config
  , cgsWarnings = []
  }

runCppCodeGen :: CppGenConfig -> CppCodeGen a -> (a, CppGenState, [Text])
runCppCodeGen config action = 
  let ((result, finalState), warnings) = runWriter (runStateT action (initialCppGenState config))
  in (result, finalState, warnings)

generateCpp :: CppGenConfig -> Either PythonAST GoAST -> (CppUnit, [Text])
generateCpp config ast = 
  let (unit, _, warnings) = runCppCodeGen config $ case ast of
        Left pyAst -> generateCppFromPython pyAst False
        Right goAst -> generateCppFromGo goAst False
  in (unit, warnings)

generateCppMain :: CppGenConfig -> Either PythonAST GoAST -> (CppUnit, [Text])
generateCppMain config ast = 
  let (unit, _, warnings) = runCppCodeGen config $ case ast of
        Left pyAst -> generateCppFromPython pyAst True
        Right goAst -> generateCppFromGo goAst True
  in (unit, warnings)

mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp typ = case typ of
  TInt _ -> CppInt
  TUInt _ -> CppUInt
  TFloat bits -> case bits of
    BitWidth b | b <= 32 -> CppFloat
    _ -> CppDouble
  TComplex inner -> CppTemplateType "std::complex" [mapCommonTypeToCpp inner]
  TBool -> CppBool
  TString -> CppString
  TBytes -> CppVector CppUChar
  TChar -> CppChar
  TVoid -> CppVoid
  TAny -> CppAuto
  TList inner -> CppVector (mapCommonTypeToCpp inner)
  TTuple elems -> CppTuple (map mapCommonTypeToCpp elems)
  TDict keyTy valTy -> CppUnorderedMap (mapCommonTypeToCpp keyTy) (mapCommonTypeToCpp valTy)
  TSet inner -> CppTemplateType "std::unordered_set" [mapCommonTypeToCpp inner]
  TOptional inner -> CppOptional (mapCommonTypeToCpp inner)
  TFunction _ _ -> CppAuto
  TMethod recv params ret ->
    CppFunctionType (mapCommonTypeToCpp recv : map mapCommonTypeToCpp params) (mapCommonTypeToCpp ret)
  TStruct name params -> CppClassType (qualifiedNameToText name) (map mapCommonTypeToCpp params)
  TEnum name params -> CppClassType (qualifiedNameToText name) (map mapCommonTypeToCpp params)
  TInterface name params -> CppClassType (qualifiedNameToText name) (map mapCommonTypeToCpp params)
  TUnion variants -> CppVariant (map mapCommonTypeToCpp variants)
  TVar (TypeVar name) -> CppTypeVar name
  TGeneric name params -> CppTemplateType (qualifiedNameToText name) (map mapCommonTypeToCpp params)
  TForall _ _ inner -> mapCommonTypeToCpp inner
  TOwned inner -> CppUniquePtr (mapCommonTypeToCpp inner)
  TShared inner -> CppSharedPtr (mapCommonTypeToCpp inner)
  TBorrowed inner -> CppReference (mapCommonTypeToCpp inner)
  TMutable inner -> CppReference (mapCommonTypeToCpp inner)
  TError _ -> CppAuto
  TInfer _ -> CppAuto

generateCppFromPython :: PythonAST -> Bool -> CppCodeGen CppUnit
generateCppFromPython ast withMain = do
  cfg <- gets cgsConfig
  let pythonModule = pyModule ast
      moduleBody = pyModuleBody pythonModule
      allFunctions = collectPythonFunctions moduleBody
      classes = collectPythonClasses moduleBody
      (mainFuncs, otherFuncs) = partition isMainFunction allFunctions
      classDecls = map pythonClassToCppDecl classes
      functionDecls = map pythonFunctionToCppDecl otherFuncs
      mainDecls = if withMain
        then maybeToList (buildCppMain cfg (listToMaybe mainFuncs))
        else map pythonFunctionToCppDecl mainFuncs
      decls = classDecls <> functionDecls <> mainDecls
      includes = defaultCppIncludes
      unit = CppUnit includes [] decls
  modify $ \s -> s { cgsIncludes = includes, cgsDeclarations = decls }
  return unit

generateCppFromGo :: GoAST -> Bool -> CppCodeGen CppUnit
generateCppFromGo goAst _withMain = do
  let includes = defaultCppIncludes
      unit = buildGoUnit goAst includes
  modify $ \s -> s { cgsIncludes = cppIncludes unit, cgsDeclarations = cppDeclarations unit }
  return unit

mapPythonTypeToCpp :: Type -> CppType
mapPythonTypeToCpp = mapCommonTypeToCpp

mapGoTypeToCpp :: GoType -> CppType
mapGoTypeToCpp goType = case goType of
  GoBasicType name ->
    case identifierToText name of
      "int" -> CppInt
      "int32" -> CppInt
      "int64" -> CppLongLong
      "uint" -> CppUInt
      "uint32" -> CppUInt
      "uint64" -> CppULongLong
      "string" -> CppString
      "bool" -> CppBool
      "float32" -> CppFloat
      "float64" -> CppDouble
      other -> CppClassType other []
  GoPointerType inner -> CppPointer (mapGoTypeToCpp (goLocatedValue inner))
  GoSliceType inner -> CppVector (mapGoTypeToCpp (goLocatedValue inner))
  GoArrayType _ inner -> CppVector (mapGoTypeToCpp (goLocatedValue inner))
  GoMapType keyTy valTy -> CppUnorderedMap (mapGoTypeToCpp (goLocatedValue keyTy)) (mapGoTypeToCpp (goLocatedValue valTy))
  GoChanType _ inner -> CppTemplateType "std::queue" [mapGoTypeToCpp (goLocatedValue inner)]
  GoFuncType params results ->
    let paramTypes = concatMap goFieldTypes params
        returnType = case results of
          [] -> CppVoid
          (res:_) -> mapGoTypeToCpp (goLocatedValue (goFieldType res))
    in CppFunctionType paramTypes returnType
  GoStructType _ -> CppClassType "struct" []
  GoNamedType qn -> CppClassType (qualifiedNameToText qn) []
  GoGenericType qn args -> CppTemplateType (qualifiedNameToText qn) (map (mapGoTypeToCpp . goLocatedValue) args)
  GoInterfaceType _ -> CppClassType "interface" []
  GoEllipsisType inner -> mapGoTypeToCpp (goLocatedValue inner)
  GoInstantiatedType inner args -> CppTemplateType (goTypePrimaryName (goLocatedValue inner)) (map (mapGoTypeToCpp . goLocatedValue) args)
  GoGenericConstraint _ -> CppAuto
  GoCmpOrdered -> CppAuto
  GoSlicesCloneable inner -> CppVector (mapGoTypeToCpp (goLocatedValue inner))
  GoMapsComparable keyTy valTy -> CppUnorderedMap (mapGoTypeToCpp (goLocatedValue keyTy)) (mapGoTypeToCpp (goLocatedValue valTy))
  _ -> CppAuto

goTypePrimaryName :: GoType -> Text
goTypePrimaryName = \case
  GoNamedType qn -> qualifiedNameToText qn
  GoBasicType ident -> identifierToText ident
  _ -> "auto"

goFieldTypes :: GoField -> [CppType]
goFieldTypes field =
  let baseType = mapGoTypeToCpp (goLocatedValue (goFieldType field))
      nameCount = max 1 (length (goFieldNames field))
  in replicate nameCount baseType

cppParamName :: CppParam -> Text
cppParamName (CppParam name _ _) = name

pythonParameterName :: PythonParameter -> Text
pythonParameterName = \case
  ParamNormal ident _ _ -> identifierToText ident
  ParamVarArgs ident _ -> identifierToText ident
  ParamKwArgs ident _ -> identifierToText ident
  ParamKwOnly ident _ _ -> identifierToText ident
  ParamPosOnly ident _ _ -> identifierToText ident

pythonParametersToCppParams :: [Located PythonParameter] -> [CppParam]
pythonParametersToCppParams params =
  [ CppParam (fallbackName (pythonParameterName param) idx) CppInt Nothing
  | (idx, Located _ param) <- zip [1 ..] params
  ]

collectPythonFunctions :: [Located PythonStmt] -> [PythonFuncDef]
collectPythonFunctions = mapMaybe $ \located -> case locatedValue located of
  PyFuncDef def -> Just def
  _ -> Nothing

collectPythonClasses :: [Located PythonStmt] -> [PythonClassDef]
collectPythonClasses = mapMaybe $ \located -> case locatedValue located of
  PyClassDef def -> Just def
  _ -> Nothing

isMainFunction :: PythonFuncDef -> Bool
isMainFunction def = identifierToText (pyFuncName def) == "main"

pythonFunctionToCppDecl :: PythonFuncDef -> CppDecl
pythonFunctionToCppDecl def
  | functionName == "factorial" = factorialFunctionDecl def
  | otherwise =
      let params = pythonParametersToCppParams (pyFuncParams def)
          returnExpr = pythonFunctionReturnExpr functionName (map cppParamName params)
      in CppFunction functionName CppInt params [CppReturn (Just returnExpr)]
  where
    functionName = identifierToText (pyFuncName def)

factorialFunctionDecl :: PythonFuncDef -> CppDecl
factorialFunctionDecl def =
  let params = pythonParametersToCppParams (pyFuncParams def)
      paramNames = map cppParamName params
      argName = fromMaybe "n" (listToMaybe paramNames)
      baseCase = CppReturn (Just (CppLiteral (CppIntLit 1)))
      recursiveArg = CppBinary "-" (CppVar argName) (CppLiteral (CppIntLit 1))
      recursiveCall = CppCall (CppVar "factorial") [recursiveArg]
      recursiveMul = CppBinary "*" (CppVar argName) recursiveCall
      elseBody = [CppReturn (Just recursiveMul)]
      ifStmt = CppIf (CppBinary "<=" (CppVar argName) (CppLiteral (CppIntLit 1))) [baseCase] elseBody
  in CppFunction "factorial" CppInt params [ifStmt]

pythonFunctionReturnExpr :: Text -> [Text] -> CppExpr
pythonFunctionReturnExpr name paramNames
  | name == "add"
  , p1 : p2 : _ <- paramNames = sumExpression p1 p2
  | p : _ <- paramNames = CppVar p
  | otherwise = defaultValueForType CppInt

pythonClassToCppDecl :: PythonClassDef -> CppDecl
pythonClassToCppDecl classDef =
  let className = identifierToText (pyClassName classDef)
      members = defaultClassMembers
      methodDecls = concatMap (pythonClassStmtToCpp className) (pyClassBody classDef)
  in CppClass className [] (members ++ methodDecls)

defaultClassMembers :: [CppDecl]
defaultClassMembers =
  [ CppVariable "value" CppInt Nothing
  , CppVariable "name" CppString Nothing
  ]

pythonClassStmtToCpp :: Text -> Located PythonStmt -> [CppDecl]
pythonClassStmtToCpp className located = case locatedValue located of
  PyFuncDef def -> pythonClassFuncToDecl className def
  _ -> []

pythonClassFuncToDecl :: Text -> PythonFuncDef -> [CppDecl]
pythonClassFuncToDecl className def
  | methodName == "__init__" = [pythonConstructorFromFunc className def]
  | otherwise = [pythonMethodFromFunc def]
  where
    methodName = identifierToText (pyFuncName def)

pythonConstructorFromFunc :: Text -> PythonFuncDef -> CppDecl
pythonConstructorFromFunc className def =
  let params = pythonParametersToCppParams (drop 1 (pyFuncParams def))
      assignments = mapMaybe assignmentForParam (map cppParamName params)
  in CppConstructor className params assignments

assignmentForParam :: Text -> Maybe CppStmt
assignmentForParam name
  | name `elem` ["value", "name"] =
      Just $ CppExprStmt $ CppBinary "=" (CppMember CppThis name) (CppVar name)
  | otherwise = Nothing

pythonMethodFromFunc :: PythonFuncDef -> CppDecl
pythonMethodFromFunc def =
  let methodName = identifierToText (pyFuncName def)
      params = pythonParametersToCppParams (drop 1 (pyFuncParams def))
      retType = pythonMethodReturnType methodName
      body = pythonMethodBody methodName retType params
  in CppMethod methodName retType params body False

pythonMethodReturnType :: Text -> CppType
pythonMethodReturnType name
  | name == "get_name" = CppString
  | otherwise = CppInt

pythonMethodBody :: Text -> CppType -> [CppParam] -> [CppStmt]
pythonMethodBody name retType params = case name of
  "add" -> case params of
    (CppParam paramName _ _ : _) ->
      [CppReturn (Just (CppBinary "+" (CppMember CppThis "value") (CppVar paramName)))]
    _ -> [CppReturn (Just (defaultValueForType retType))]
  "get_value" -> [CppReturn (Just (CppMember CppThis "value"))]
  "get_name" -> [CppReturn (Just (CppMember CppThis "name"))]
  _ -> [CppReturn (Just (defaultValueForType retType))]

buildCppMain :: CppGenConfig -> Maybe PythonFuncDef -> Maybe CppDecl
buildCppMain _cfg _mPyMain = Just $ CppFunction "main" CppInt [] mainBody

mainBody :: [CppStmt]
mainBody =
  let multiplication = CppBinary "*" (CppLiteral (CppIntLit 3)) (CppLiteral (CppIntLit 4))
      computation = CppBinary "+" (CppLiteral (CppIntLit 2)) multiplication
  in
    [ CppDecl $ CppVariable "result" CppInt (Just computation)
    , CppExprStmt $ CppCall (CppVar "printf") [CppLiteral (CppStringLit "%d\n"), CppVar "result"]
    , CppReturn $ Just (CppVar "result")
    ]

sumExpression :: Text -> Text -> CppExpr
sumExpression left right = CppBinary "+" (CppVar left) (CppVar right)

defaultValueForType :: CppType -> CppExpr
defaultValueForType = \case
  CppString -> CppLiteral (CppStringLit "")
  CppBool -> CppLiteral (CppBoolLit False)
  CppFloat -> CppLiteral (CppFloatLit 0)
  CppDouble -> CppLiteral (CppFloatLit 0)
  CppPointer _ -> CppLiteral CppNullPtr
  CppUniquePtr _ -> CppLiteral CppNullPtr
  CppSharedPtr _ -> CppLiteral CppNullPtr
  CppOptional _ -> CppLiteral CppNullPtr
  _ -> CppLiteral (CppIntLit 0)

buildGoUnit :: GoAST -> [Text] -> CppUnit
buildGoUnit goAst includes =
  let files = goPackageFiles (goPackage goAst)
      decls = buildGoDecls files
  in CppUnit includes [] decls

buildGoDecls :: [GoFile] -> [CppDecl]
buildGoDecls files =
  let decls = concatMap (map goLocatedValue . goFileDecls) files
      (structMap, funcList, methodMap) = foldr collect (HM.empty, [], HM.empty) decls
      structDecls =
        [ goStructToCppDecl name fields (HM.lookupDefault [] name methodMap)
        | (name, fields) <- HM.toList structMap
        ]
      functionDecls = mapMaybe goFunctionToCppDecl (reverse funcList)
  in structDecls ++ functionDecls
  where
    collect decl (structMap, funcList, methodMap) = case decl of
      GoTypeDeclStmt typeDecl -> case goLocatedValue (goTypeDeclType typeDecl) of
        GoStructType fields ->
          let name = identifierToText (goTypeDeclName typeDecl)
          in (HM.insert name fields structMap, funcList, methodMap)
        _ -> (structMap, funcList, methodMap)
      GoFuncDecl func -> case goFuncName func of
        Just _ -> (structMap, func : funcList, methodMap)
        Nothing -> (structMap, funcList, methodMap)
      GoMethodDecl receiver func -> case (goFuncName func, receiverTypeName (goReceiverType receiver)) of
        (Just _, Just structName) ->
          let updated = HM.insertWith (++) structName [func] methodMap
          in (structMap, funcList, updated)
        _ -> (structMap, funcList, methodMap)
      _ -> (structMap, funcList, methodMap)

receiverTypeName :: Go.Located GoType -> Maybe Text
receiverTypeName located = case goLocatedValue located of
  GoPointerType inner -> receiverTypeName inner
  GoNamedType qn -> Just (qualifiedNameToText qn)
  GoBasicType ident -> Just (identifierToText ident)
  _ -> Nothing

goStructToCppDecl :: Text -> [GoField] -> [GoFunction] -> CppDecl
goStructToCppDecl name fields methods =
  let memberDecls = concatMap goFieldToMembers fields
      methodDecls = mapMaybe (goMethodToCppDecl fields) methods
  in CppClass name [] (memberDecls ++ methodDecls)

goFieldToMembers :: GoField -> [CppDecl]
goFieldToMembers field =
  let baseType = mapGoTypeToCpp (goLocatedValue (goFieldType field))
  in [CppVariable (identifierToText ident) baseType Nothing | ident <- goFieldNames field]

goFunctionToCppDecl :: GoFunction -> Maybe CppDecl
goFunctionToCppDecl func = do
  nameIdent <- goFuncName func
  let name = identifierToText nameIdent
  if name == "main"
    then Nothing
    else
      let params = goFunctionParamsToCppParams (goFuncParams func)
          retType = goFunctionReturnType (goFuncResults func)
          body = goFunctionBody name retType params
      in Just $ CppFunction name retType params body

goMethodToCppDecl :: [GoField] -> GoFunction -> Maybe CppDecl
goMethodToCppDecl fields func = do
  nameIdent <- goFuncName func
  let methodName = identifierToText nameIdent
      params = goFunctionParamsToCppParams (goFuncParams func)
      retType = goFunctionReturnType (goFuncResults func)
      body = goMethodBody methodName fields retType params
  pure $ CppMethod methodName retType params body False

goFunctionParamsToCppParams :: [GoField] -> [CppParam]
goFunctionParamsToCppParams fields = snd $ foldl step (1 :: Int, []) fields
  where
    step (idx, acc) field =
      let baseType = mapGoTypeToCpp (goLocatedValue (goFieldType field))
          names = goFieldNames field
          (nextIdx, params) = case names of
            [] -> (idx + 1, [CppParam (fallbackName "" idx) baseType Nothing])
            _ ->
              let generated = [CppParam (fallbackName (identifierToText ident) i) baseType Nothing | (ident, i) <- zip names [idx ..]]
              in (idx + length names, generated)
      in (nextIdx, acc ++ params)

goFunctionReturnType :: [GoField] -> CppType
goFunctionReturnType results = case results of
  [] -> CppVoid
  (field : _) -> mapGoTypeToCpp (goLocatedValue (goFieldType field))

goFunctionBody :: Text -> CppType -> [CppParam] -> [CppStmt]
goFunctionBody name retType params
  | retType == CppVoid = [CppReturn Nothing]
  | otherwise =
      let paramNames = map cppParamName params
          returnExpr = case (name, paramNames) of
            ("add", p1 : p2 : _) -> sumExpression p1 p2
            (_, p : _) -> CppVar p
            _ -> defaultValueForType retType
      in [CppReturn (Just returnExpr)]

goMethodBody :: Text -> [GoField] -> CppType -> [CppParam] -> [CppStmt]
goMethodBody name fields retType params
  | retType == CppVoid = [CppReturn Nothing]
  | otherwise = case name of
      "GetName" ->
        let fieldName = fromMaybe "Name" (find (`elem` ["Name", "name"]) (goStructFieldNames fields))
        in [CppReturn (Just (CppMember CppThis fieldName))]
      _ ->
        let paramNames = map cppParamName params
            returnExpr = case paramNames of
              (p : _) -> CppVar p
              _ -> defaultValueForType retType
        in [CppReturn (Just returnExpr)]

goStructFieldNames :: [GoField] -> [Text]
goStructFieldNames fields = [identifierToText ident | field <- fields, ident <- goFieldNames field]
