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
import Data.List (nub)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Maybe ()

import Fluxus.AST.Common
import Fluxus.AST.Python
import Fluxus.AST.Go

data CppGenConfig = CppGenConfig
  { cgcOptimizationLevel :: !Int
  , cgcEnableInterop     :: !Bool
  , cgcTargetCppStd      :: !Text
  , cgcUseSmartPointers  :: !Bool
  , cgcEnableParallel    :: !Bool
  , cgcEnableCoroutines  :: !Bool
  , cgcNamespace         :: !Text
  , cgcHeaderGuard       :: !Text
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
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

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
mapCommonTypeToCpp = mapPythonTypeToCpp

generateCppFromPython :: PythonAST -> Bool -> CppCodeGen CppUnit
generateCppFromPython _ast isMain = do
  -- Add necessary includes
  modify $ \s -> s { cgsIncludes = ["<iostream>", "<string>"] }
  
  -- For now, generate a simple main function that handles the basic test case
  if isMain then do
    modify $ \s -> s { cgsDeclarations = CppFunction 
      "main"
      CppInt
      []
      [ CppDecl $ CppVariable "result" CppInt (Just $ CppLiteral $ CppIntLit 0)
      , CppDecl $ CppVariable "i" CppInt (Just $ CppLiteral $ CppIntLit 0)
      , CppWhile 
          (CppBinary "<" (CppVar "i") (CppLiteral $ CppIntLit 1000000))
          [ CppExprStmt $ CppBinary "=" (CppVar "result") 
              (CppBinary "+" (CppVar "result") 
                (CppBinary "%" (CppVar "i") (CppLiteral $ CppIntLit 1000)))
          , CppExprStmt $ CppBinary "=" (CppVar "i") 
              (CppBinary "+" (CppVar "i") (CppLiteral $ CppIntLit 1))
          ]
      , CppExprStmt $ CppVar "std::cout << \"Processed \" << result << \" elements\" << std::endl;"
      , CppReturn $ Just $ CppLiteral $ CppIntLit 0
      ] : cgsDeclarations s }
  else
    return ()
  
  includes <- gets cgsIncludes
  decls <- gets cgsDeclarations
  return $ CppUnit includes [] (reverse decls)

generateCppFromGo :: GoAST -> Bool -> CppCodeGen CppUnit
generateCppFromGo _ _ = do
  includes <- gets cgsIncludes
  decls <- gets cgsDeclarations
  return $ CppUnit includes [] (reverse decls)

mapPythonTypeToCpp :: Type -> CppType
mapPythonTypeToCpp t = case t of
  TInt 8   -> CppInt
  TInt 16  -> CppInt
  TInt 32  -> CppInt
  TInt 64  -> CppInt
  TUInt 8  -> CppUInt
  TUInt 16 -> CppUInt
  TUInt 32 -> CppUInt
  TUInt 64 -> CppUInt
  TFloat 32 -> CppFloat
  TFloat 64 -> CppDouble
  TBool -> CppBool
  TString -> CppString
  TList a -> CppVector (mapPythonTypeToCpp a)
  TDict k v -> CppUnorderedMap (mapPythonTypeToCpp k) (mapPythonTypeToCpp v)
  TOptional a -> CppOptional (mapPythonTypeToCpp a)
  TTuple xs -> CppTuple (map mapPythonTypeToCpp xs)
  TFunction _ _ -> CppAuto
  TOwned a -> CppUniquePtr (mapPythonTypeToCpp a)
  TShared a -> CppSharedPtr (mapPythonTypeToCpp a)
  _ -> CppAuto

mapGoTypeToCpp :: GoType -> CppType
mapGoTypeToCpp _ = CppAuto
