{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Fluxus.CodeGen.CPP
  ( -- * Code generation types
    CppCodeGen
  , CppGenState(..)
  , CppGenConfig(..)
    -- * Main code generation functions
  , generateCpp
  , generateCppMain
  , generateCppFromPython
  , generateCppFromGo
    -- * Pretty printing
  , renderCppUnit
  , prettyCppUnit
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
import Control.Monad (when, unless, forM, forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, partition, sortBy, nub)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)

import AST.Common
import AST.Python
import AST.Go

-- ============================================================================
-- Core Types
-- ============================================================================

-- | C++ code generation configuration
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

-- | Code generation state
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

-- | C++ statements
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

-- | C++ expressions
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

-- | C++ literals
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

-- | Function parameters
data CppParam = CppParam !Text !CppType !(Maybe CppExpr)
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Switch cases
data CppCase = CppCase !CppExpr ![CppStmt] | CppDefault ![CppStmt]
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- | Catch blocks
data CppCatch = CppCatch !CppType !Text ![CppStmt]
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

-- ============================================================================
-- Pretty Printer
-- ============================================================================

-- | Render a C++ unit to text
renderCppUnit :: CppUnit -> Text
renderCppUnit unit = prettyCppUnit unit

-- | Pretty print a C++ compilation unit
prettyCppUnit :: CppUnit -> Text
prettyCppUnit CppUnit{..} = T.unlines $ concat
  [ map renderInclude (nub cppIncludes)
  , [""]
  , -- Always include tuple output support function for safety
    [tupleOutputSupportFunction]
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

-- | Render declaration with indentation
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
    maybe ";" (\init -> " = " <> renderExpr init <> ";") mInit
  
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
    
    renderAccessLevel ind level =
      indentText ind <> level <> "\n"

-- | Render statement with indentation
renderStmt :: Int -> CppStmt -> Text
renderStmt indent stmt = case stmt of
  CppExprStmt expr ->
    indentText indent <> renderExpr expr <> ";"
  
  CppReturn Nothing ->
    indentText indent <> "return;"
  
  CppReturn (Just expr) ->
    indentText indent <> "return " <> renderExpr expr <> ";"
  
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
    T.concatMap (renderCatch indent) catches <>
    if null finallyStmts then ""
    else "\n" <> indentText indent <> "finally {\n" <>
         T.unlines (map (renderStmt (indent + 1)) finallyStmts) <>
         indentText indent <> "}"
  
  CppThrow Nothing ->
    indentText indent <> "throw;"
  
  CppThrow (Just expr) ->
    indentText indent <> "throw " <> renderExpr expr <> ";"
  
  CppBreak ->
    indentText indent <> "break;"
  
  CppContinue ->
    indentText indent <> "continue;"
  
  CppBlock stmts ->
    indentText indent <> "{\n" <>
    T.unlines (map (renderStmt (indent + 1)) stmts) <>
    indentText indent <> "}"
  
  CppDecl decl ->
    renderDecl indent decl
  
  CppComment comment ->
    indentText indent <> "// " <> comment
  
  CppLabel label ->
    indentText (max 0 (indent - 1)) <> label <> ":"
  
  CppGoto label ->
    indentText indent <> "goto " <> label <> ";"
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

-- | Render expression
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
  CppMember obj member ->
    renderExpr obj <> "." <> member
  CppPointerMember ptr member ->
    renderExpr ptr <> "->" <> member
  CppIndex arr idx ->
    renderExpr arr <> "[" <> renderExpr idx <> "]"
  CppCast typ expr' ->
    "(" <> renderType typ <> ")" <> renderExpr expr'
  CppStaticCast typ expr' ->
    "static_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppDynamicCast typ expr' ->
    "dynamic_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppReinterpretCast typ expr' ->
    "reinterpret_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppConstCast typ expr' ->
    "const_cast<" <> renderType typ <> ">(" <> renderExpr expr' <> ")"
  CppSizeOf typ ->
    "sizeof(" <> renderType typ <> ")"
  CppNew typ args ->
    "new " <> renderType typ <> 
    if null args then ""
    else "(" <> T.intercalate ", " (map renderExpr args) <> ")"
  CppDelete expr' ->
    "delete " <> renderExpr expr'
  CppThis -> "this"
  CppLambda params body captures ->
    let captureStr = if captures then "[&]" else "[]"
    in captureStr <> "(" <> renderParams params <> ") {\n" <>
       T.unlines (map (renderStmt 1) body) <> "}"
  CppMove expr' ->
    "std::move(" <> renderExpr expr' <> ")"
  CppForward expr' ->
    "std::forward(" <> renderExpr expr' <> ")"
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
  CppComma exprs ->
    "(" <> T.intercalate ", " (map renderExpr exprs) <> ")"
  where
    isPostfix "++" = True
    isPostfix "--" = True
    isPostfix _ = False

-- | Render type
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

-- | Render literal
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

-- | Render parameters
renderParams :: [CppParam] -> Text
renderParams params = T.intercalate ", " (map renderParam params)
  where
    renderParam (CppParam name typ mDefault) =
      renderType typ <> " " <> name <>
      maybe "" (\def -> " = " <> renderExpr def) mDefault

-- | Tuple output support function
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

-- | Generate indentation
indentText :: Int -> Text
indentText n = T.replicate n "    "

-- ============================================================================
-- Configuration and State Management
-- ============================================================================

-- | Default configuration
defaultCppGenConfig :: CppGenConfig
defaultCppGenConfig = CppGenConfig
  { cgcOptimizationLevel = 2
  , cgcEnableInterop = True
  , cgcTargetCppStd = "c++20"
  , cgcUseSmartPointers = True
  , cgcEnableParallel = True
  , cgcEnableCoroutines = True
  , cgcNamespace = "generated"
  , cgcHeaderGuard = "GENERATED_H"
  }

-- | Initial state
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

-- | Run code generation
runCppCodeGen :: CppGenConfig -> CppCodeGen a -> (a, CppGenState, [Text])
runCppCodeGen config action = 
  let ((result, finalState), warnings) = runWriter (runStateT action (initialCppGenState config))
  in (result, finalState, warnings)

-- ============================================================================
-- Main Entry Points
-- ============================================================================

-- | Main entry point for C++ code generation
generateCpp :: CppGenConfig -> Either PythonAST GoAST -> (CppUnit, [Text])
generateCpp config ast = 
  let (unit, _, warnings) = runCppCodeGen config $ case ast of
        Left pyAst -> generateCppFromPython pyAst False
        Right goAst -> generateCppFromGo goAst False
  in (unit, warnings)

-- | Generate C++ for main file (with main function)
generateCppMain :: CppGenConfig -> Either PythonAST GoAST -> (CppUnit, [Text])
generateCppMain config ast = 
  let (unit, _, warnings) = runCppCodeGen config $ case ast of
        Left pyAst -> generateCppFromPython pyAst True
        Right goAst -> generateCppFromGo goAst True
  in (unit, warnings)

-- ============================================================================
-- Python to C++ Code Generation
-- ============================================================================

-- | Generate C++ from Python AST
generateCppFromPython :: PythonAST -> Bool -> CppCodeGen CppUnit
generateCppFromPython (PythonAST pyModule) isMainFile = do
  -- Add standard includes
  addInclude "<iostream>"
  addInclude "<string>"
  addInclude "<vector>"
  addInclude "<memory>"
  addInclude "<algorithm>"
  addInclude "<sstream>"
  addInclude "<tuple>"
  
  let moduleName = fromMaybe "main" $ fmap (KATEX_INLINE_OPENModuleName n) -> n) (pyModuleName pyModule)
  
  -- Process module body
  let (funcDefs, moduleStmts) = partitionStmts (pyModuleBody pyModule)
  
  -- Process function definitions
  mapM_ generatePythonStmt funcDefs
  
  -- Process module-level statements
  moduleStmtsCpp <- catMaybes <$> mapM generatePythonStmtMaybe moduleStmts
  
  -- Ensure main function exists if needed
  when isMainFile $ ensureMainFunction moduleStmtsCpp
  
  -- Build final unit
  includes <- gets cgsIncludes
  decls <- gets cgsDeclarations
  
  return $ CppUnit includes [] (reverse decls)
  where
    partitionStmts = partition isFuncDef
    isFuncDef (Located _ (PyFuncDef _)) = True
    isFuncDef _ = False

-- | Generate C++ from Python statement (returning Maybe to filter comments)
generatePythonStmtMaybe :: Located PythonStmt -> CppCodeGen (Maybe CppStmt)
generatePythonStmtMaybe stmt = do
  result <- generatePythonStmt stmt
  case result of
    CppComment _ -> return Nothing
    other -> return (Just other)

-- | Generate C++ from Python statement
generatePythonStmt :: Located PythonStmt -> CppCodeGen CppStmt
generatePythonStmt (Located _ stmt) = case stmt of
  PyFuncDef funcDef -> do
    generatePythonFunction funcDef
    return $ CppComment "Function definition processed"
  
  PyClassDef classDef -> do
    generatePythonClass classDef
    return $ CppComment "Class definition processed"
  
  PyExprStmt expr -> do
    -- Check if this is a print statement
    case expr of
      Located _ (PyCall func args) ->
        case func of
          Located _ (PyVar (Identifier "print")) -> handleBuiltinFunction "print" args
          _ -> do
            cppExpr <- generatePythonExpr expr
            return $ CppExprStmt cppExpr
      _ -> do
        cppExpr <- generatePythonExpr expr
        return $ CppExprStmt cppExpr
  
  PyAssign patterns expr -> 
    generatePythonAssignment patterns expr
  
  PyReturn mexpr -> do
    mcppExpr <- mapM generatePythonExpr mexpr
    return $ CppReturn mcppExpr
  
  PyIf condition thenStmts elseStmts -> do
    cppCond <- generatePythonExpr condition
    cppThen <- mapM generatePythonStmt thenStmts
    cppElse <- mapM generatePythonStmt elseStmts
    return $ CppIf cppCond cppThen cppElse
  
  PyFor pattern iterExpr bodyStmts elseStmts -> 
    generatePythonForLoop pattern iterExpr bodyStmts elseStmts
  
  PyWhile condition bodyStmts elseStmts -> do
    cppCond <- generatePythonExpr condition
    cppBody <- mapM generatePythonStmt bodyStmts
    if null elseStmts
      then return $ CppWhile cppCond cppBody
      else do
        -- Python while-else: execute else block if loop completes normally
        addWarning "while-else clause not fully supported, ignoring else block"
        return $ CppWhile cppCond cppBody
  
  PyWith items bodyStmts -> do
    -- Simplified context manager handling
    addWarning "with statement simplified to block"
    cppBody <- mapM generatePythonStmt bodyStmts
    return $ CppBlock cppBody
  
  PyRaise mexpr -> do
    mcppExpr <- mapM generatePythonExpr mexpr
    return $ CppThrow mcppExpr
  
  PyTry tryStmts handlers elseStmts finallyStmts -> do
    cppTry <- mapM generatePythonStmt tryStmts
    cppCatches <- mapM generatePythonExceptHandler handlers
    cppFinally <- mapM generatePythonStmt finallyStmts
    return $ CppTry cppTry cppCatches cppFinally
  
  PyImport _ -> return $ CppComment "Import statement (handled separately)"
  PyFromImport _ _ -> return $ CppComment "From import statement (handled separately)"
  
  PyGlobal _ -> return $ CppComment "Global declaration"
  PyNonlocal _ -> return $ CppComment "Nonlocal declaration"
  
  PyPass -> return $ CppComment "pass"
  PyBreak -> return CppBreak
  PyContinue -> return CppContinue
  
  PyDelete exprs -> do
    cppExprs <- mapM generatePythonExpr exprs
    return $ CppBlock $ map (CppExprStmt . CppDelete) cppExprs
  
  PyAssert expr msg -> do
    cppExpr <- generatePythonExpr expr
    addInclude "<cassert>"
    return $ CppExprStmt $ CppCall (CppVar "assert") [cppExpr]
  
  PyAnnAssign target annotation value -> do
    -- Type annotated assignment
    cppValue <- mapM generatePythonExpr value
    case locatedValue target of
      PatVar (Identifier name) -> do
        cppType <- mapPythonTypeAnnotation annotation
        return $ CppDecl $ CppVariable name cppType cppValue
      _ -> do
        addWarning "Complex annotated assignment pattern not supported"
        return $ CppComment "Complex annotated assignment"
  
  PyAsyncDef _ -> do
    addWarning "Async functions not supported"
    return $ CppComment "Async function (not supported)"
  
  PyAsyncFor {} -> do
    addWarning "Async for loops not supported"
    return $ CppComment "Async for loop (not supported)"
  
  PyAsyncWith {} -> do
    addWarning "Async with statements not supported"
    return $ CppComment "Async with (not supported)"

-- | Generate Python assignment
generatePythonAssignment :: [Located PythonPattern] -> Located PythonExpr -> CppCodeGen CppStmt
generatePythonAssignment patterns expr = do
  cppExpr <- generatePythonExpr expr
  case patterns of
    [Located _ (PatVar (Identifier varName))] -> do
      -- Single variable assignment
      symTable <- gets cgsSymbolTable
      if HM.member varName symTable
        then return $ CppExprStmt $ CppBinary "=" (CppVar varName) cppExpr
        else do
          updateSymbol varName CppAuto
          return $ CppDecl $ CppVariable varName CppAuto (Just cppExpr)
    
    _ -> do
      -- Multiple assignment with tuple unpacking
      tempVar <- generateTempVar
      let tempDecl = CppDecl $ CppVariable tempVar CppAuto (Just cppExpr)
      assignments <- forM (zip patterns [0..]) $ KATEX_INLINE_OPENpat, idx) ->
        case locatedValue pat of
          PatVar (Identifier name) -> do
            let getExpr = CppCall (CppVar $ "std::get<" <> T.pack (show idx) <> ">") [CppVar tempVar]
            symTable <- gets cgsSymbolTable
            if HM.member name symTable
              then return $ CppExprStmt $ CppBinary "=" (CppVar name) getExpr
              else do
                updateSymbol name CppAuto
                return $ CppDecl $ CppVariable name CppAuto (Just getExpr)
          _ -> do
            addWarning "Complex pattern in assignment not supported"
            return $ CppComment "Complex pattern"
      return $ CppBlock (tempDecl : assignments)

-- | Generate Python for loop
generatePythonForLoop :: Located PythonPattern -> Located PythonExpr -> [Located PythonStmt] -> [Located PythonStmt] -> CppCodeGen CppStmt
generatePythonForLoop pattern iterExpr bodyStmts elseStmts = do
  case locatedValue pattern of
    PatVar (Identifier varName) -> do
      cppIter <- generatePythonExpr iterExpr
      cppBody <- mapM generatePythonStmt bodyStmts
      
      -- Handle range() specially
      case locatedValue iterExpr of
        PyCall (Located _ (PyVar (Identifier "range"))) args -> 
          generateRangeForLoop varName args cppBody
        _ -> return $ CppForRange varName cppIter cppBody
    
    _ -> do
      addWarning "Complex for loop pattern not supported"
      return $ CppComment "Complex for loop pattern"
  where
    generateRangeForLoop varName args body = do
      case map locatedValue args of
        [ArgPositional end] -> do
          cppEnd <- generatePythonExpr end
          let init = Just $ CppDecl $ CppVariable varName CppInt (Just $ CppLiteral $ CppIntLit 0)
              cond = Just $ CppBinary "<" (CppVar varName) cppEnd
              post = Just $ CppUnary "++" (CppVar varName)
          return $ CppFor init cond post body
        
        [ArgPositional start, ArgPositional end] -> do
          cppStart <- generatePythonExpr start
          cppEnd <- generatePythonExpr end
          let init = Just $ CppDecl $ CppVariable varName CppInt (Just cppStart)
              cond = Just $ CppBinary "<" (CppVar varName) cppEnd
              post = Just $ CppUnary "++" (CppVar varName)
          return $ CppFor init cond post body
        
        [ArgPositional start, ArgPositional end, ArgPositional step] -> do
          cppStart <- generatePythonExpr start
          cppEnd <- generatePythonExpr end
          cppStep <- generatePythonExpr step
          let init = Just $ CppDecl $ CppVariable varName CppInt (Just cppStart)
              cond = Just $ CppBinary "<" (CppVar varName) cppEnd
              post = Just $ CppBinary "+=" (CppVar varName) cppStep
          return $ CppFor init cond post body
        
        _ -> do
          addWarning "Unsupported range() arguments"
          return $ CppComment "Unsupported range() call"

-- | Generate Python expression
generatePythonExpr :: Located PythonExpr -> CppCodeGen CppExpr
generatePythonExpr (Located _ expr) = case expr of
  PyLiteral lit -> generatePythonLiteral lit
  PyVar (Identifier name) -> return $ CppVar name
  
  PyBinaryOp op left right -> do
    cppLeft <- generatePythonExpr left
    cppRight <- generatePythonExpr right
    generateBinaryOp op cppLeft cppRight
  
  PyUnaryOp op operand -> do
    cppOperand <- generatePythonExpr operand
    return $ CppUnary (mapPythonUnaryOp op) cppOperand
  
  PyComparison ops exprs -> 
    generatePythonComparison ops exprs
  
  PyCall func args -> 
    generatePythonCall func args
  
  PyAttribute obj attr -> do
    cppObj <- generatePythonExpr obj
    return $ CppMember cppObj (unIdentifier attr)
  
  PySubscript obj index -> do
    cppObj <- generatePythonExpr obj
    cppIndex <- generatePythonExpr index
    return $ CppIndex cppObj cppIndex
  
  PyList items -> do
    addInclude "<vector>"
    cppItems <- mapM generatePythonExpr items
    return $ CppInitList (CppVector CppAuto) cppItems
  
  PyTuple items -> do
    addInclude "<tuple>"
    cppItems <- mapM generatePythonExpr items
    return $ CppCall (CppVar "std::make_tuple") cppItems
  
  PyDict pairs -> do
    addInclude "<unordered_map>"
    let generatePair (k, v) = do
          cppKey <- generatePythonExpr k
          cppVal <- generatePythonExpr v
          return $ CppInitList (CppPair CppAuto CppAuto) [cppKey, cppVal]
    cppPairs <- mapM generatePair pairs
    return $ CppInitList (CppUnorderedMap CppAuto CppAuto) cppPairs
  
  PySet items -> do
    addInclude "<unordered_set>"
    cppItems <- mapM generatePythonExpr items
    return $ CppInitList (CppTemplateType "std::unordered_set" [CppAuto]) cppItems
  
  PyLambda params body -> do
    cppParams <- mapM mapPythonLambdaParam params
    cppBody <- case body of
      Left expr -> do
        cppExpr <- generatePythonExpr expr
        return [CppReturn (Just cppExpr)]
      Right stmts -> mapM generatePythonStmt stmts
    return $ CppLambda cppParams cppBody True
  
  PyIfExpr cond thenExpr elseExpr -> do
    cppCond <- generatePythonExpr cond
    cppThen <- generatePythonExpr thenExpr
    cppElse <- generatePythonExpr elseExpr
    return $ CppTernary cppCond cppThen cppElse
  
  PyComprehension kind comp -> do
    addWarning "Comprehensions not fully supported"
    return $ CppComment "Comprehension not implemented"
  
  PyYield _ -> do
    addWarning "Generators not supported"
    return $ CppComment "Yield expression"
  
  PyYieldFrom _ -> do
    addWarning "Generators not supported"
    return $ CppComment "Yield from expression"
  
  PyAwait expr -> do
    addWarning "Async/await not supported"
    generatePythonExpr expr
  
  PyStarred expr -> generatePythonExpr expr
  PySlice _ _ _ -> return $ CppComment "Slice expression"
  PyNamed _ _ -> return $ CppComment "Named expression"

-- | Generate Python literal
generatePythonLiteral :: PythonLiteral -> CppCodeGen CppExpr
generatePythonLiteral lit = case lit of
  PyInt i -> return $ CppLiteral $ CppIntLit i
  PyFloat f -> return $ CppLiteral $ CppFloatLit f
  PyBool b -> return $ CppLiteral $ CppBoolLit b
  PyString s -> return $ CppLiteral $ CppStringLit s
  PyFString template exprs -> generateFString template exprs
  PyNone -> return CppNullPtr
  PyEllipsis -> return $ CppComment "..."
  PyBytes _ -> do
    addWarning "Bytes literals not supported"
    return $ CppLiteral $ CppStringLit ""
  PyComplex _ _ -> do
    addWarning "Complex numbers not supported"
    return $ CppLiteral $ CppFloatLit 0.0

-- | Generate f-string
generateFString :: Text -> [Located PythonExpr] -> CppCodeGen CppExpr
generateFString template exprs = do
  addInclude "<sstream>"
  cppExprs <- mapM generatePythonExpr exprs
  let parts = parseFStringParts template
  streamVar <- generateTempVar
  let streamDecl = CppDecl $ CppVariable streamVar (CppTemplateType "std::stringstream" []) Nothing
      streamOps = buildStreamOps streamVar parts cppExprs
      strExpr = CppCall (CppMember (CppVar streamVar) "str") []
  return $ CppCall (CppLambda [] (streamDecl : streamOps ++ [CppReturn (Just strExpr)]) False) []

-- | Parse f-string into parts
parseFStringParts :: Text -> [FStringPart]
parseFStringParts = go [] where
  go acc "" = reverse acc
  go acc text = case T.breakOn "{" text of
    (before, "") -> reverse (if T.null before then acc else TextPart before : acc)
    (before, rest) -> case T.breakOn "}" (T.drop 1 rest) of
      (_, "") -> reverse (TextPart text : acc)
      (expr, after) -> 
        let acc' = if T.null before then acc else TextPart before : acc
            acc'' = ExprPart (T.strip expr) : acc'
        in go acc'' (T.drop 1 after)

data FStringPart = TextPart Text | ExprPart Text

-- | Build stream operations for f-string
buildStreamOps :: Text -> [FStringPart] -> [CppExpr] -> [CppStmt]
buildStreamOps streamVar parts exprs = go parts 0 where
  go [] _ = []
  go (TextPart text : rest) idx = 
    CppExprStmt (CppBinary "<<" (CppVar streamVar) (CppLiteral (CppStringLit text))) : go rest idx
  go (ExprPart _ : rest) idx =
    if idx < length exprs
    then CppExprStmt (CppBinary "<<" (CppVar streamVar) (exprs !! idx)) : go rest (idx + 1)
    else go rest idx

-- | Generate binary operation
generateBinaryOp :: BinaryOp -> CppExpr -> CppExpr -> CppCodeGen CppExpr
generateBinaryOp op left right = case op of
  OpAdd -> handleAddition left right
  OpSub -> return $ CppBinary "-" left right
  OpMul -> return $ CppBinary "*" left right
  OpDiv -> handleDivision left right
  OpFloorDiv -> do
    addInclude "<cmath>"
    return $ CppCall (CppVar "std::floor") [CppBinary "/" left right]
  OpMod -> return $ CppBinary "%" left right
  OpPow -> do
    addInclude "<cmath>"
    return $ CppCall (CppVar "std::pow") [left, right]
  OpLShift -> return $ CppBinary "<<" left right
  OpRShift -> return $ CppBinary ">>" left right
  OpBitOr -> return $ CppBinary "|" left right
  OpBitXor -> return $ CppBinary "^" left right
  OpBitAnd -> return $ CppBinary "&" left right
  OpAnd -> return $ CppBinary "&&" left right
  OpOr -> return $ CppBinary "||" left right
  OpMatMul -> do
    addWarning "Matrix multiplication not supported"
    return $ CppBinary "*" left right
  where
    handleAddition l r = 
      if isStringExpr l || isStringExpr r
      then do
        addInclude "<string>"
        let wrapString e = if isStringExpr e then e else CppCall (CppVar "std::to_string") [e]
        return $ CppBinary "+" (wrapString l) (wrapString r)
      else return $ CppBinary "+" l r
    
    handleDivision l r = do
      let ensureFloat e = case e of
            CppLiteral (CppIntLit i) -> CppLiteral (CppFloatLit (fromIntegral i))
            CppLiteral (CppFloatLit f) -> CppLiteral (CppFloatLit f)
            _ -> e  -- For variables and other expressions, rely on C++'s automatic promotion
      return $ CppBinary "/" (ensureFloat l) (ensureFloat r)
    
    isStringExpr (CppLiteral (CppStringLit _)) = True
    isStringExpr (CppCall (CppVar "std::to_string") _) = True
    isStringExpr (CppVar name) = "str" `T.isInfixOf` name
    isStringExpr _ = False

-- | Generate comparison
generatePythonComparison :: [ComparisonOp] -> [Located PythonExpr] -> CppCodeGen CppExpr
generatePythonComparison ops exprs = do
  cppExprs <- mapM generatePythonExpr exprs
  case (ops, cppExprs) of
    ([op], [left, right]) -> 
      return $ CppBinary (mapPythonComparisonOp op) left right
    _ -> do
      -- Chain comparisons with &&
      let pairs = zip3 ops cppExprs (tail cppExprs)
          comparisons = map (KATEX_INLINE_OPENop, l, r) -> CppBinary (mapPythonComparisonOp op) l r) pairs
      return $ foldr1 (CppBinary "&&") comparisons

-- | Generate function call
generatePythonCall :: Located PythonExpr -> [Located PythonArgument] -> CppCodeGen CppExpr
generatePythonCall func args = do
  cppFunc <- generatePythonExpr func
  cppArgs <- mapM generatePythonArgument args
  
  -- Handle special built-in functions
  case func of
    Located _ (PyVar (Identifier name)) -> 
      handleBuiltinFunction name cppArgs
    _ -> return $ CppCall cppFunc cppArgs
  where
    handleBuiltinFunction "print" args = do
      addInclude "<iostream>"
      addInclude "<tuple>"
      addInclude "<string>"
      
      -- Debug: print what arguments we receive
      addDeclaration $ CppCommentDecl $ "PRINT DEBUG: Received args: " <> T.pack (show args)
      
      case args of
        [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
        [singleArg] -> do
          -- Debug: check if it's a tuple
          case singleArg of
            CppCall (CppVar "std::make_tuple") tupleArgs -> do
              addDeclaration $ CppCommentDecl $ "PRINT DEBUG: Found tuple with args: " <> T.pack (show tupleArgs)
              -- Handle tuple by expanding it - print each element separately
              case tupleArgs of
                [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
                [singleElement] -> 
                  return $ CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") singleElement) (CppVar "std::endl")
                (label:value:rest) -> do
                  -- Print label and value, then handle remaining elements
                  let firstPart = CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") label) (CppLiteral (CppStringLit " "))
                      secondPart = CppBinary "<<" firstPart value
                  case rest of
                    [] -> return $ CppBinary "<<" secondPart (CppVar "std::endl")
                    _ -> do
                      -- Add space and continue with remaining elements
                      let withSpace = CppBinary "<<" secondPart (CppLiteral (CppStringLit " "))
                          remainingChain = foldl (\acc arg -> CppBinary "<<" acc arg) withSpace rest
                      return $ CppBinary "<<" remainingChain (CppVar "std::endl")
            _ -> do
              -- Single argument: print it directly
              return $ CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") singleArg) (CppVar "std::endl")
        multipleArgs -> do
          -- Multiple arguments: print them separated by spaces
          let buildChain [] acc = acc
              buildChain [x] acc = CppBinary "<<" acc x
              buildChain (x:xs) acc = buildChain xs (CppBinary "<<" (CppBinary "<<" acc x) (CppLiteral (CppStringLit " ")))
          let outputChain = buildChain multipleArgs (CppVar "std::cout")
          return $ CppBinary "<<" outputChain (CppVar "std::endl")
    
    handleBuiltinFunction "len" [arg] = do
      return $ CppCall (CppMember arg "size") []
    
    handleBuiltinFunction "range" args = do
      -- This should be handled by for loops
      addWarning "range() outside of for loop"
      return $ CppLiteral $ CppStringLit "range() call"
    
    handleBuiltinFunction "str" [arg] = do
      addInclude "<string>"
      return $ CppCall (CppVar "std::to_string") [arg]
    
    handleBuiltinFunction "int" [arg] = 
      return arg  -- For now, just return the argument as-is
    
    handleBuiltinFunction "float" [arg] = 
      return arg  -- For now, just return the argument as-is
    
    handleBuiltinFunction "bool" [arg] = 
      return arg  -- For now, just return the argument as-is
    
    handleBuiltinFunction "abs" [arg] = do
      addInclude "<cmath>"
      return $ CppCall (CppVar "std::abs") [arg]
    
    handleBuiltinFunction "min" args = do
      addInclude "<algorithm>"
      return $ CppCall (CppVar "std::min") args
    
    handleBuiltinFunction "max" args = do
      addInclude "<algorithm>"
      return $ CppCall (CppVar "std::max") args
    
    handleBuiltinFunction _ args = 
      return $ CppCall cppFunc args
    
    intercalateWith sep [] = []
    intercalateWith sep [x] = [x]
    intercalateWith sep (x:xs) = x : sep : intercalateWith sep xs

-- | Generate Python argument
generatePythonArgument :: Located PythonArgument -> CppCodeGen CppExpr
generatePythonArgument (Located _ arg) = case arg of
  ArgPositional expr -> generatePythonExpr expr
  ArgKeyword _ expr -> generatePythonExpr expr
  ArgStarred expr -> generatePythonExpr expr
  ArgKwStarred expr -> generatePythonExpr expr

-- | Generate Python function
generatePythonFunction :: PythonFuncDef -> CppCodeGen ()
generatePythonFunction funcDef = do
  let funcName = unIdentifier (pyFuncName funcDef)
  cppParams <- mapM mapPythonParameter (pyFuncParams funcDef)
  
  returnType <- case pyFuncReturns funcDef of
    Just typeExpr -> mapPythonTypeAnnotation typeExpr
    Nothing -> 
      if funcName == "main" 
      then return CppInt
      else inferReturnType (pyFuncBody funcDef)
  
  bodyStmts <- mapM generatePythonStmt (pyFuncBody funcDef)
  
  let finalBody = if funcName == "main" && not (hasReturn bodyStmts)
                  then bodyStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                  else bodyStmts
  
  addDeclaration $ CppFunction funcName returnType cppParams finalBody

-- | Generate Python class
generatePythonClass :: PythonClassDef -> CppCodeGen ()
generatePythonClass classDef = do
  let className = unIdentifier (pyClassName classDef)
  baseClasses <- mapM extractBaseClassName (pyClassBases classDef)
  
  -- Process class body
  members <- concat <$> mapM (generateClassMember className) (pyClassBody classDef)
  
  addDeclaration $ CppClass className baseClasses members
  where
    generateClassMember className stmt = case locatedValue stmt of
      PyFuncDef funcDef -> do
        let methodName = unIdentifier (pyFuncName funcDef)
        cppParams <- mapM mapPythonParameter (pyFuncParams funcDef)
        returnType <- inferReturnType (pyFuncBody funcDef)
        bodyStmts <- mapM generatePythonStmt (pyFuncBody funcDef)
        
        if methodName == "__init__"
        then return [CppConstructor className (tail cppParams) bodyStmts]
        else return [CppMethod methodName returnType (tail cppParams) bodyStmts False]
      
      PyAssign [Located _ (PatVar (Identifier varName))] expr -> do
        cppExpr <- generatePythonExpr expr
        return [CppVariable varName CppAuto (Just cppExpr)]
      
      _ -> return []

-- | Generate exception handler
generatePythonExceptHandler :: Located PythonExceptHandler -> CppCodeGen CppCatch
generatePythonExceptHandler (Located _ (ExceptHandler mtype mname body)) = do
  let exType = fromMaybe (CppClassType "std::exception" []) 
                        (fmap (const $ CppClassType "std::exception" []) mtype)
      varName = fromMaybe "e" (fmap unIdentifier mname)
  bodyStmts <- mapM generatePythonStmt body
  return $ CppCatch exType varName bodyStmts

-- ============================================================================
-- Go to C++ Code Generation
-- ============================================================================

-- | Generate C++ from Go AST
generateCppFromGo :: GoAST -> Bool -> CppCodeGen CppUnit
generateCppFromGo (GoAST goPackage) isMainFile = do
  -- Add standard includes
  addInclude "<iostream>"
  addInclude "<string>"
  addInclude "<vector>"
  addInclude "<thread>"
  addInclude "<mutex>"
  addInclude "<condition_variable>"
  addInclude "<memory>"
  addInclude "<tuple>"
  addInclude "<chrono>"
  
  let packageName = unIdentifier (goPackageName goPackage)
  
  -- Generate channel implementation if needed
  when (needsChannels goPackage) generateChannelClass
  
  -- Process files
  mapM_ generateGoFile (goPackageFiles goPackage)
  
  -- Ensure main function if needed
  when (isMainFile && packageName == "main") ensureMainFunction []
  
  -- Build final unit
  includes <- gets cgsIncludes
  decls <- gets cgsDeclarations
  
  return $ CppUnit includes [] (reverse decls)

-- | Check if Go package needs channel implementation
needsChannels :: GoPackage -> Bool
needsChannels _ = False -- Simplified for now

-- | Generate Go file
generateGoFile :: GoFile -> CppCodeGen ()
generateGoFile goFile = do
  -- DEBUG: Print information about what we're processing
  let declCount = length (goFileDecls goFile)
  liftIO $ putStrLn $ "[DEBUG CODEGEN] Processing Go file with " ++ show declCount ++ " declarations"
  when (declCount == 0) $ liftIO $ putStrLn "[DEBUG CODEGEN] WARNING: No declarations found!"
  -- Print each declaration type for debugging
  forM_ (goFileDecls goFile) $ \(Located _ decl) -> case decl of
    GoFuncDecl _ -> liftIO $ putStrLn "[DEBUG CODEGEN]   Found function declaration"
    GoMethodDecl _ _ -> liftIO $ putStrLn "[DEBUG CODEGEN]   Found method declaration"
    GoTypeDecl _ _ -> liftIO $ putStrLn "[DEBUG CODEGEN]   Found type declaration"
    GoVarDecl _ -> liftIO $ putStrLn "[DEBUG CODEGEN]   Found variable declaration"
    GoConstDecl _ -> liftIO $ putStrLn "[DEBUG CODEGEN]   Found constant declaration"
    GoInitDecl _ -> liftIO $ putStrLn "[DEBUG CODEGEN]   Found init declaration"
    _ -> liftIO $ putStrLn "[DEBUG CODEGEN]   Found other declaration"

  -- Now actually process the declarations
  mapM_ generateGoDecl (goFileDecls goFile)

-- | Generate Go declaration
generateGoDecl :: Located GoDecl -> CppCodeGen ()
generateGoDecl (Located _ decl) = case decl of
  GoFuncDecl func -> generateGoFunction func
  
  GoTypeDecl name typeExpr -> do
    cppType <- generateGoType typeExpr
    addDeclaration $ CppTypedef (unIdentifier name) cppType
  
  GoVarDecl vars -> mapM_ generateGoVariable vars
  
  GoConstDecl consts -> mapM_ generateGoConstant consts
  
  GoMethodDecl receiver func -> 
    generateGoMethod receiver func
  
  _ -> addWarning $ "Unsupported Go declaration: " <> T.pack (show decl)

-- | Generate Go function
generateGoFunction :: GoFunction -> CppCodeGen ()
generateGoFunction func = case goFuncName func of
  Nothing -> return () -- Anonymous function
  Just name -> do
    let funcName = unIdentifier name
    cppParams <- concat <$> mapM expandGoField (goFuncParams func)
    returnType <- mapGoResults funcName (goFuncResults func)
    
    bodyStmts <- case goFuncBody func of
      Nothing -> return []
      Just body -> generateGoBlockStmt body
    
    let finalBody = if funcName == "main" && not (hasReturn bodyStmts)
                    then bodyStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                    else bodyStmts
    
    addDeclaration $ CppFunction funcName returnType cppParams finalBody

-- | Generate Go method
generateGoMethod :: GoReceiver -> GoFunction -> CppCodeGen ()
generateGoMethod receiver func = case goFuncName func of
  Nothing -> return ()
  Just name -> do
    let methodName = unIdentifier name
        receiverType = case goReceiverType receiver of
          Located _ t -> renderType (mapGoTypeToCpp t)
        receiverName = fromMaybe "this" (fmap unIdentifier (goReceiverName receiver))
    
    cppParams <- concat <$> mapM expandGoField (goFuncParams func)
    returnType <- mapGoResults methodName (goFuncResults func)
    
    bodyStmts <- case goFuncBody func of
      Nothing -> return []
      Just body -> generateGoBlockStmt body
    
    -- For now, generate as a regular function with receiver as first parameter
    let fullParams = CppParam receiverName (CppReference (CppAuto)) Nothing : cppParams
    addDeclaration $ CppFunction (receiverType <> "_" <> methodName) returnType fullParams bodyStmts

-- | Generate Go statement
generateGoStmt :: Located GoStmt -> CppCodeGen CppStmt
generateGoStmt (Located _ stmt) = case stmt of
  GoReturn exprs -> do
    case exprs of
      [] -> return $ CppReturn Nothing
      [expr] -> CppReturn . Just <$> generateGoExpr expr
      _ -> do
        cppExprs <- mapM generateGoExpr exprs
        return $ CppReturn $ Just $ CppCall (CppVar "std::make_tuple") cppExprs
  
  GoExprStmt expr -> CppExprStmt <$> generateGoExpr expr
  
  GoIf mInit cond thenStmt elseStmt -> do
    mInitStmt <- mapM generateGoStmt mInit
    cppCond <- generateGoExpr cond
    thenStmts <- generateGoStmtBlock thenStmt
    elseStmts <- maybe (return []) generateGoStmtBlock elseStmt
    
    let ifStmt = CppIf cppCond thenStmts elseStmts
    case mInitStmt of
      Nothing -> return ifStmt
      Just initStmt -> return $ CppBlock [initStmt, ifStmt]
  
  GoFor mClause body -> case mClause of
    Nothing -> do
      bodyStmts <- generateGoBlockStmt body
      return $ CppWhile (CppLiteral (CppBoolLit True)) bodyStmts
    
    Just clause -> do
      initStmt <- mapM generateGoStmt (goForInit clause)
      condExpr <- mapM generateGoExpr (goForCond clause)
      postStmt <- mapM generateGoStmt (goForPost clause)
      bodyStmts <- generateGoBlockStmt body
      
      let postExpr = case postStmt of
            Just (CppExprStmt e) -> Just e
            _ -> Nothing
      
      return $ CppFor initStmt condExpr postExpr bodyStmts
  
  GoSwitch mInit mExpr cases -> do
    mInitStmt <- mapM generateGoStmt mInit
    mCppExpr <- mapM generateGoExpr mExpr
    cppCases <- mapM generateGoCase cases
    
    let switchStmt = case mCppExpr of
          Nothing -> CppComment "Type switch not supported"
          Just expr -> CppSwitch expr cppCases
    
    case mInitStmt of
      Nothing -> return switchStmt
      Just initStmt -> return $ CppBlock [initStmt, switchStmt]
  
  GoBlock stmts -> CppBlock <$> mapM generateGoStmt stmts
  
  GoGo expr -> do
    cppExpr <- generateGoExpr expr
    addInclude "<thread>"
    return $ CppExprStmt $ CppCall (CppVar "std::thread") [cppExpr]
  
  GoDefer expr -> do
    addWarning "defer not fully supported"
    cppExpr <- generateGoExpr expr
    return $ CppComment $ "defer: " <> renderExpr cppExpr
  
  GoSend channel value -> do
    cppChannel <- generateGoExpr channel
    cppValue <- generateGoExpr value
    return $ CppExprStmt $ CppCall (CppMember cppChannel "send") [cppValue]
  
  GoDefine names exprs -> generateGoDefine names exprs
  
  GoAssign leftExprs rightExprs -> generateGoAssign leftExprs rightExprs
  
  GoIncDec expr isInc -> do
    cppExpr <- generateGoExpr expr
    let op = if isInc then "++" else "--"
    return $ CppExprStmt $ CppUnary op cppExpr
  
  GoVarStmt vars -> do
    stmts <- mapM generateGoVarDecl vars
    case stmts of
      [single] -> return single
      multiple -> return $ CppBlock multiple
  
  GoConstStmt consts -> do
    stmts <- mapM generateGoConstDecl consts
    case stmts of
      [single] -> return single
      multiple -> return $ CppBlock multiple
  
  GoTypeStmt types -> do
    mapM_ generateGoTypeDecl types
    return $ CppComment "Type declarations processed"
  
  GoBreak _ -> return CppBreak
  GoContinue _ -> return CppContinue
  GoGoto label -> return $ CppGoto (unIdentifier label)
  GoLabel label -> return $ CppLabel (unIdentifier label)
  GoFallthrough -> return $ CppComment "fallthrough"
  
  _ -> do
    addWarning $ "Unsupported Go statement: " <> T.pack (show stmt)
    return $ CppComment "Unsupported statement"

-- | Generate Go expression
generateGoExpr :: Located GoExpr -> CppCodeGen CppExpr
generateGoExpr (Located _ expr) = do
  liftIO $ putStrLn $ "[DEBUG] generateGoExpr: " <> T.pack (show expr)
  case expr of
  GoLiteral lit -> return $ CppLiteral $ mapGoLiteral lit
  GoIdent name -> return $ CppVar (unIdentifier name)
  
  GoBinaryOp op left right -> do
    cppLeft <- generateGoExpr left
    cppRight <- generateGoExpr right
    return $ CppBinary (mapGoBinaryOp op) cppLeft cppRight
  
  GoUnaryOp op operand -> do
    cppOperand <- generateGoExpr operand
    return $ CppUnary (mapGoUnaryOp op) cppOperand
  
  GoComparison op left right -> do
    cppLeft <- generateGoExpr left
    cppRight <- generateGoExpr right
    return $ CppBinary (mapPythonComparisonOp op) cppLeft cppRight
  
  GoCall func args -> generateGoCall func args
  
  GoSelector obj member -> do
    cppObj <- generateGoExpr obj
    return $ CppMember cppObj (unIdentifier member)
  
  GoIndex array index -> do
    cppArray <- generateGoExpr array
    cppIndex <- generateGoExpr index
    return $ CppIndex cppArray cppIndex
  
  GoSlice array low high max -> do
    addWarning "Slice operations not fully supported"
    generateGoExpr array
  
  GoTypeAssertion expr typ -> do
    cppExpr <- generateGoExpr expr
    cppType <- generateGoType typ
    return $ CppDynamicCast cppType cppExpr
  
  GoReceive channel -> do
    cppChannel <- generateGoExpr channel
    return $ CppCall (CppMember cppChannel "receive") []
  
  GoCompositeLit mType elements -> generateGoCompositeLiteral mType elements

  GoFuncLit func -> generateGoFuncLiteral func

  GoBuiltinCall name args -> do
    liftIO $ putStrLn $ "[DEBUG] GoBuiltinCall reached: " <> T.pack (show name) <> " with " <> T.pack (show $ length args) <> " args"
    generateGoBuiltinCall name args

  expr -> do
    liftIO $ putStrLn $ "[DEBUG] Other expression type: " <> T.pack (show expr)
    addWarning $ "Unsupported Go expression: " <> T.pack (show expr)
    return $ CppLiteral $ CppIntLit 0

-- | Generate Go call
generateGoCall :: Located GoExpr -> [Located GoExpr] -> CppCodeGen CppExpr
generateGoCall func args = do
  cppFunc <- generateGoExpr func
  liftIO $ putStrLn $ "[DEBUG] generateGoCall: func=" <> T.pack (show cppFunc)
  cppArgs <- mapM generateGoExpr args
  liftIO $ putStrLn $ "[DEBUG] generateGoCall: args=" <> T.pack (show $ length cppArgs)
  
  -- Handle special functions
  case func of
    Located _ (GoQualifiedIdent (Identifier "fmt") (Identifier fname)) ->
      handleFmtFunction fname cppArgs
    Located _ (GoIdent (Identifier "println")) -> do
      addInclude "<iostream>"
      return $ buildPrintExpr cppArgs True
    Located _ (GoIdent (Identifier "print")) -> do
      addInclude "<iostream>"
      return $ buildPrintExpr cppArgs False
    _ -> return $ CppCall cppFunc cppArgs
  where
    handleFmtFunction "Printf" (CppLiteral (CppStringLit fmt) : args) = do
      addInclude "<iostream>"
      addInclude "<iomanip>"
      return $ buildPrintfExpr fmt args
    handleFmtFunction "Println" args = do
      addInclude "<iostream>"
      return $ buildPrintExpr args True
    handleFmtFunction "Print" args = do
      addInclude "<iostream>"
      return $ buildPrintExpr args False
    handleFmtFunction _ args = return $ CppCall cppFunc args

-- | Generate Go builtin function call
generateGoBuiltinCall :: Text -> [Located GoExpr] -> CppCodeGen CppExpr
generateGoBuiltinCall name args = do
  cppArgs <- mapM generateGoExpr args
  case name of
    "println" -> do
      addInclude "<iostream>"
      return $ buildPrintExpr cppArgs True
    "print" -> do
      addInclude "<iostream>"
      return $ buildPrintExpr cppArgs False
    "printf" -> do
      case cppArgs of
        (CppLiteral (CppStringLit fmt) : restArgs) -> do
          addInclude "<iostream>"
          addInclude "<iomanip>"
          return $ buildPrintfExpr fmt restArgs
        _ -> do
          addWarning "printf with non-string format not supported"
          return $ CppLiteral $ CppStringLit "printf_error"
    _ -> do
      addWarning $ "Unsupported builtin function: " <> name
      return $ CppLiteral $ CppIntLit 0

-- | Build print expression
buildPrintExpr :: [CppExpr] -> Bool -> CppExpr
buildPrintExpr args addNewline = 
  let base = foldl (\acc arg -> CppBinary "<<" acc arg) (CppVar "std::cout") $
             intercalateWith (CppLiteral (CppStringLit " ")) args
  in if addNewline 
     then CppBinary "<<" base (CppVar "std::endl")
     else base
  where
    intercalateWith _ [] = []
    intercalateWith _ [x] = [x]
    intercalateWith sep (x:xs) = x : sep : intercalateWith sep xs

-- | Build printf expression
buildPrintfExpr :: Text -> [CppExpr] -> CppExpr
buildPrintfExpr fmt args = 
  let parts = T.splitOn "%" fmt
      build [] _ acc = acc
      build (p:ps) as acc = 
        let acc' = if T.null p then acc else CppBinary "<<" acc (CppLiteral (CppStringLit p))
        in case as of
          [] -> acc'
          (a:as') -> build ps as' (CppBinary "<<" acc' a)
  in build parts args (CppVar "std::cout")

-- | Generate Go composite literal
generateGoCompositeLiteral :: Maybe (Located GoType) -> [Located GoExpr] -> CppCodeGen CppExpr
generateGoCompositeLiteral mType elements = do
  cppElements <- mapM generateGoExpr elements
  case mType of
    Nothing -> return $ CppInitList (CppVector CppAuto) cppElements
    Just typ -> do
      cppType <- generateGoType typ
      return $ CppInitList cppType cppElements

-- | Generate Go function literal
generateGoFuncLiteral :: GoFunction -> CppCodeGen CppExpr
generateGoFuncLiteral func = do
  params <- concat <$> mapM expandGoField (goFuncParams func)
  body <- case goFuncBody func of
    Nothing -> return []
    Just stmt -> generateGoBlockStmt stmt
  return $ CppLambda params body True

-- | Generate Go define statement
generateGoDefine :: [Identifier] -> [Located GoExpr] -> CppCodeGen CppStmt
generateGoDefine names exprs = do
  if length names == length exprs
  then do
    decls <- forM (zip names exprs) $ KATEX_INLINE_OPENname, expr) -> do
      cppExpr <- generateGoExpr expr
      return $ CppDecl $ CppVariable (unIdentifier name) CppAuto (Just cppExpr)
    case decls of
      [single] -> return single
      multiple -> return $ CppBlock multiple
  else if length exprs == 1
  then do
    -- Tuple unpacking
    case exprs of
      [] -> do
        addWarning "Empty expression list in define statement"
        return $ CppComment "Empty define"
      (firstExpr:_) -> do
        cppExpr <- generateGoExpr firstExpr
        let varNames = map unIdentifier names
            decls = map (\n -> CppDecl $ CppVariable n CppAuto Nothing) varNames
            tie = CppCall (CppVar "std::tie") (map CppVar varNames)
            assign = CppExprStmt $ CppBinary "=" tie cppExpr
        return $ CppBlock (decls ++ [assign])
  else do
    addWarning "Mismatched define statement"
    return $ CppComment "Mismatched define"

-- | Generate Go assignment
generateGoAssign :: [Located GoExpr] -> [Located GoExpr] -> CppCodeGen CppStmt
generateGoAssign lefts rights = do
  if length lefts == length rights
  then do
    assigns <- forM (zip lefts rights) $ KATEX_INLINE_OPENleft, right) -> do
      cppLeft <- generateGoExpr left
      cppRight <- generateGoExpr right
      return $ CppExprStmt $ CppBinary "=" cppLeft cppRight
    case assigns of
      [single] -> return single
      multiple -> return $ CppBlock multiple
  else if length rights == 1
  then do
    -- Tuple unpacking
    case rights of
      [] -> do
        addWarning "Empty right-hand side in assignment"
        return $ CppComment "Empty assignment"
      (firstRight:_) -> do
        cppRight <- generateGoExpr firstRight
        cppLefts <- mapM generateGoExpr lefts
        let tie = CppCall (CppVar "std::tie") cppLefts
        return $ CppExprStmt $ CppBinary "=" tie cppRight
  else do
    addWarning "Mismatched assignment"
    return $ CppComment "Mismatched assignment"

-- | Generate Go variable declaration
generateGoVarDecl :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> CppCodeGen CppStmt
generateGoVarDecl (name, mType, mExpr) = do
  cppType <- case mType of
    Nothing -> return CppAuto
    Just t -> generateGoType t
  cppExpr <- mapM generateGoExpr mExpr
  return $ CppDecl $ CppVariable (unIdentifier name) cppType cppExpr

-- | Generate Go constant declaration
generateGoConstDecl :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> CppCodeGen CppStmt
generateGoConstDecl (name, mType, mExpr) = do
  cppType <- case mType of
    Nothing -> return CppAuto
    Just t -> generateGoType t
  cppExpr <- mapM generateGoExpr mExpr
  return $ CppDecl $ CppVariable (unIdentifier name) (CppConst cppType) cppExpr

-- | Generate Go type declaration
generateGoTypeDecl :: (Identifier, Located GoType) -> CppCodeGen ()
generateGoTypeDecl (name, typ) = do
  cppType <- generateGoType typ
  addDeclaration $ CppTypedef (unIdentifier name) cppType

-- | Generate Go case
generateGoCase :: GoCase -> CppCodeGen CppCase
generateGoCase goCase = case goCaseExprs goCase of
  [] -> CppDefault <$> mapM generateGoStmt (goCaseStmts goCase)
  (expr:_) -> do
    cppExpr <- generateGoExpr expr
    stmts <- mapM generateGoStmt (goCaseStmts goCase)
    return $ CppCase cppExpr stmts

-- | Generate Go block statement
generateGoBlockStmt :: Located GoStmt -> CppCodeGen [CppStmt]
generateGoBlockStmt stmt = generateGoStmtBlock stmt

-- | Generate Go statement block
generateGoStmtBlock :: Located GoStmt -> CppCodeGen [CppStmt]
generateGoStmtBlock (Located _ (GoBlock stmts)) = mapM generateGoStmt stmts
generateGoStmtBlock stmt = (:[]) <$> generateGoStmt stmt

-- | Generate Go variable
generateGoVariable :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> CppCodeGen ()
generateGoVariable (name, mType, mExpr) = do
  cppType <- case mType of
    Nothing -> return CppAuto
    Just t -> generateGoType t
  cppExpr <- mapM generateGoExpr mExpr
  addDeclaration $ CppVariable (unIdentifier name) cppType cppExpr

-- | Generate Go constant
generateGoConstant :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> CppCodeGen ()
generateGoConstant (name, mType, mExpr) = do
  cppType <- case mType of
    Nothing -> return CppAuto
    Just t -> generateGoType t
  cppExpr <- mapM generateGoExpr mExpr
  addDeclaration $ CppVariable (unIdentifier name) (CppConst cppType) cppExpr

-- | Generate channel class
generateChannelClass :: CppCodeGen ()
generateChannelClass = do
  addInclude "<queue>"
  addInclude "<mutex>"
  addInclude "<condition_variable>"
  
  let members = 
        [ CppVariable "queue_" (CppTemplateType "std::queue" [CppTypeVar "T"]) Nothing
        , CppVariable "mutex_" (CppTemplateType "mutable std::mutex" []) Nothing
        , CppVariable "cv_" (CppTemplateType "std::condition_variable" []) Nothing
        ]
  
  let methods =
        [ CppMethod "send" CppVoid [CppParam "value" (CppConst (CppReference (CppTypeVar "T"))) Nothing]
            [ CppDecl $ CppVariable "lock" 
                (CppTemplateType "std::unique_lock" [CppTemplateType "std::mutex" []]) 
                (Just $ CppCall (CppVar "std::unique_lock") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "push") [CppVar "value"]
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "notify_one") []
            ] False
        , CppMethod "receive" (CppTypeVar "T") []
            [ CppDecl $ CppVariable "lock" 
                (CppTemplateType "std::unique_lock" [CppTemplateType "std::mutex" []]) 
                (Just $ CppCall (CppVar "std::unique_lock") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "wait") 
                [CppVar "lock", CppLambda [] 
                  [CppReturn $ Just $ CppUnary "!" $ 
                    CppCall (CppMember (CppMember CppThis "queue_") "empty") []] False]
            , CppDecl $ CppVariable "value" (CppTypeVar "T") 
                (Just $ CppCall (CppMember (CppMember CppThis "queue_") "front") [])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "pop") []
            , CppReturn $ Just $ CppVar "value"
            ] False
        ]
  
  addDeclaration $ CppTemplate ["T"] $ CppClass "Channel" [] (members ++ methods)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Add include
addInclude :: Text -> CppCodeGen ()
addInclude inc = do
  includes <- gets cgsIncludes
  unless (inc `elem` includes) $
    modify $ \s -> s { cgsIncludes = inc : cgsIncludes s }

-- | Add declaration
addDeclaration :: CppDecl -> CppCodeGen ()
addDeclaration decl = modify $ \s -> s { cgsDeclarations = decl : cgsDeclarations s }

-- | Add warning
addWarning :: Text -> CppCodeGen ()
addWarning msg = do
  modify $ \s -> s { cgsWarnings = msg : cgsWarnings s }
  tell [msg]

-- | Generate temporary variable
generateTempVar :: CppCodeGen Text
generateTempVar = do
  count <- gets cgsTempVarCount
  modify $ \s -> s { cgsTempVarCount = count + 1 }
  return $ "tmp_" <> T.pack (show count)

-- | Update symbol table
updateSymbol :: Text -> CppType -> CppCodeGen ()
updateSymbol name typ = 
  modify $ \s -> s { cgsSymbolTable = HM.insert name typ (cgsSymbolTable s) }

-- | Ensure main function exists
ensureMainFunction :: [CppStmt] -> CppCodeGen ()
ensureMainFunction moduleStmts = do
  decls <- gets cgsDeclarations
  unless (any isMainFunction decls) $ do
    let body = if null moduleStmts
               then [CppReturn (Just (CppLiteral (CppIntLit 0)))]
               else moduleStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
    addDeclaration $ CppFunction "main" CppInt [] body
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False

-- | Check if statements have return
hasReturn :: [CppStmt] -> Bool
hasReturn = any isReturn where
  isReturn (CppReturn _) = True
  isReturn (CppBlock stmts) = hasReturn stmts
  isReturn (CppIf _ t e) = hasReturn t || hasReturn e
  isReturn _ = False

-- | Infer return type from function body
inferReturnType :: [Located PythonStmt] -> CppCodeGen CppType
inferReturnType stmts = 
  if any hasReturnValue stmts
  then return CppAuto
  else return CppVoid
  where
    hasReturnValue (Located _ (PyReturn (Just _))) = True
    hasReturnValue _ = False

-- | Extract base class name
extractBaseClassName :: Located PythonExpr -> CppCodeGen Text
extractBaseClassName expr = case locatedValue expr of
  PyVar (Identifier name) -> return name
  _ -> return "BaseClass"

-- | Map Python parameter
mapPythonParameter :: Located PythonParameter -> CppCodeGen CppParam
mapPythonParameter (Located _ param) = case param of
  ParamNormal name mType mDefault -> do
    cppType <- maybe (return CppAuto) mapPythonTypeAnnotation mType
    cppDefault <- mapM generatePythonExpr mDefault
    return $ CppParam (unIdentifier name) cppType cppDefault
  ParamArgs name _ -> 
    return $ CppParam (unIdentifier name) (CppVector CppAuto) Nothing
  ParamKwargs name _ -> 
    return $ CppParam (unIdentifier name) (CppUnorderedMap CppString CppAuto) Nothing
  ParamPosOnly name mType mDefault -> do
    cppType <- maybe (return CppAuto) mapPythonTypeAnnotation mType
    cppDefault <- mapM generatePythonExpr mDefault
    return $ CppParam (unIdentifier name) cppType cppDefault
  ParamKwOnly name mType mDefault -> do
    cppType <- maybe (return CppAuto) mapPythonTypeAnnotation mType
    cppDefault <- mapM generatePythonExpr mDefault
    return $ CppParam (unIdentifier name) cppType cppDefault

-- | Map Python lambda parameter
mapPythonLambdaParam :: Located PythonParameter -> CppCodeGen CppParam
mapPythonLambdaParam = mapPythonParameter

-- | Map Python type annotation
mapPythonTypeAnnotation :: Located PythonTypeExpr -> CppCodeGen CppType
mapPythonTypeAnnotation (Located _ typeExpr) = case typeExpr of
  TypeName (QualifiedName _ (Identifier name)) -> return $ case name of
    "int" -> CppInt
    "float" -> CppDouble
    "bool" -> CppBool
    "str" -> CppString
    "None" -> CppVoid
    _ -> CppAuto
  TypeSubscript base args -> do
    baseType <- mapPythonTypeAnnotation base
    case baseType of
      CppAuto -> case locatedValue base of
        TypeName (QualifiedName _ (Identifier "List")) -> 
          case args of
            [] -> return CppVector  -- Handle empty args gracefully
            (firstArg:_) -> CppVector <$> mapPythonTypeAnnotation firstArg
        TypeName (QualifiedName _ (Identifier "Dict")) -> 
          case args of
            [] -> return CppUnorderedMap  -- Handle insufficient args
            (_:[]) -> return CppUnorderedMap  -- Handle insufficient args
            (keyArg:valArg:_) -> do
              keyType <- mapPythonTypeAnnotation keyArg
              valType <- mapPythonTypeAnnotation valArg
              return $ CppUnorderedMap keyType valType
        TypeName (QualifiedName _ (Identifier "Optional")) ->
          case args of
            [] -> return CppOptional  -- Handle empty args gracefully
            (firstArg:_) -> CppOptional <$> mapPythonTypeAnnotation firstArg
        TypeName (QualifiedName _ (Identifier "Tuple")) -> do
          types <- mapM mapPythonTypeAnnotation args
          return $ CppTuple types
        _ -> return CppAuto
      _ -> return baseType
  TypeUnion types -> do
    cppTypes <- mapM mapPythonTypeAnnotation types
    return $ CppVariant cppTypes
  TypeCallable params ret -> do
    paramTypes <- mapM mapPythonTypeAnnotation params
    retType <- mapPythonTypeAnnotation ret
    return $ CppFunctionType paramTypes retType
  _ -> return CppAuto

-- | Expand Go field to parameters
expandGoField :: GoField -> CppCodeGen [CppParam]
expandGoField field = 
  let names = if null (goFieldNames field) 
              then [Identifier "param"]
              else goFieldNames field
      typ = mapGoTypeToCpp (locatedValue (goFieldType field))
  in return $ map (\n -> CppParam (unIdentifier n) typ Nothing) names

-- | Map Go results
mapGoResults :: Text -> [GoField] -> CppCodeGen CppType
mapGoResults "main" [] = return CppInt
mapGoResults _ [] = return CppVoid
mapGoResults _ [field] = generateGoType (goFieldType field)
mapGoResults _ fields = do
  types <- mapM (generateGoType . goFieldType) fields
  return $ CppTuple types

-- | Generate Go type
generateGoType :: Located GoType -> CppCodeGen CppType
generateGoType (Located _ goType) = return $ mapGoTypeToCpp goType

-- | Map Go literal
mapGoLiteral :: GoLiteral -> CppLiteral
mapGoLiteral lit = case lit of
  GoInt i -> CppIntLit i
  GoFloat f -> CppFloatLit f
  GoBool b -> CppBoolLit b
  GoString s -> CppStringLit s
  GoRune c -> CppCharLit c
  GoNil -> CppNullPtr
  _ -> CppIntLit 0

-- | Map operators
mapPythonUnaryOp :: UnaryOp -> Text
mapPythonUnaryOp op = case op of
  OpNot -> "!"
  OpInvert -> "~"
  OpUAdd -> "+"
  OpUSub -> "-"

mapGoBinaryOp :: BinaryOp -> Text
mapGoBinaryOp = mapPythonBinaryOp

mapGoUnaryOp :: UnaryOp -> Text
mapGoUnaryOp = mapPythonUnaryOp

mapPythonBinaryOp :: BinaryOp -> Text
mapPythonBinaryOp op = case op of
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpAnd -> "&&"
  OpOr -> "||"
  OpBitAnd -> "&"
  OpBitOr -> "|"
  OpBitXor -> "^"
  OpLShift -> "<<"
  OpRShift -> ">>"
  _ -> "+"

mapPythonComparisonOp :: ComparisonOp -> Text
mapPythonComparisonOp op = case op of
  OpEq -> "=="
  OpNe -> "!="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpIs -> "=="
  OpIsNot -> "!="
  OpIn -> "=="
  OpNotIn -> "!="

-- | Type mapping
mapPythonTypeToCpp :: Type -> CppType
mapPythonTypeToCpp typ = case typ of
  TInt _ -> CppInt
  TFloat _ -> CppDouble
  TBool -> CppBool
  TString -> CppString
  TVoid -> CppVoid
  TList t -> CppVector (mapPythonTypeToCpp t)
  TDict k v -> CppUnorderedMap (mapPythonTypeToCpp k) (mapPythonTypeToCpp v)
  TOptional t -> CppOptional (mapPythonTypeToCpp t)
  _ -> CppAuto

mapGoTypeToCpp :: GoType -> CppType
mapGoTypeToCpp typ = case typ of
  GoBasicType (Identifier name) -> case name of
    "int" -> CppInt
    "int8" -> CppChar
    "int16" -> CppShort
    "int32" -> CppInt
    "int64" -> CppLongLong
    "uint" -> CppUInt
    "uint8" -> CppUChar
    "uint16" -> CppUShort
    "uint32" -> CppUInt
    "uint64" -> CppULongLong
    "float32" -> CppFloat
    "float64" -> CppDouble
    "bool" -> CppBool
    "string" -> CppString
    "byte" -> CppUChar
    "rune" -> CppInt
    _ -> CppAuto
  GoSliceType (Located _ elemType) -> CppVector (mapGoTypeToCpp elemType)
  GoArrayType size (Located _ elemType) -> 
    let s = case locatedValue size of
              GoLiteral (GoInt i) -> fromIntegral i
              _ -> 0
    in CppArray (mapGoTypeToCpp elemType) s
  GoMapType (Located _ k) (Located _ v) -> 
    CppUnorderedMap (mapGoTypeToCpp k) (mapGoTypeToCpp v)
  GoPointerType (Located _ base) -> CppPointer (mapGoTypeToCpp base)
  GoChanType _ (Located _ elem) -> 
    CppTemplateType "Channel" [mapGoTypeToCpp elem]
  GoFuncType params results ->
    let paramTypes = map (mapGoTypeToCpp . locatedValue . goFieldType) params
        resultType = case results of
          [] -> CppVoid
          [r] -> mapGoTypeToCpp (locatedValue (goFieldType r))
          rs -> CppTuple (map (mapGoTypeToCpp . locatedValue . goFieldType) rs)
    in CppFunctionType paramTypes resultType
  GoInterfaceType _ -> CppPointer CppVoid
  GoStructType fields -> CppAuto -- Simplified
  _ -> CppAuto

mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp = mapPythonTypeToCpp

-- | Extract identifier
unIdentifier :: Identifier -> Text
unIdentifier (Identifier name) = name