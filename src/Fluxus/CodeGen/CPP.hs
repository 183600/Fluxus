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
import Control.Monad (when, unless, forM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (partition, nub)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe, catMaybes)

import Fluxus.AST.Common
import qualified Fluxus.AST.Common as Common
import Fluxus.AST.Python
import Fluxus.AST.Go hiding (Located, Identifier)
import qualified Fluxus.AST.Go as Go

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
-- Helper Functions
-- ============================================================================

-- | Helper function to extract value from Go's Located type
goLocatedValue :: Go.Located a -> a
goLocatedValue = Go.locValue

-- | Convert Go.Located to Common.Located
convertGoLocated :: Go.Located a -> Common.Located a
convertGoLocated (Go.Located ann val) = 
  Common.Located (convertGoNodeAnn ann) val
  where
    convertGoNodeAnn (Go.NodeAnn mSpan _ _) = 
      case mSpan of
        Nothing -> Common.SourceSpan (T.pack "<go-file>") (Common.SourcePos 0 0) (Common.SourcePos 0 0)
        Just goSpan -> convertGoSpan goSpan
    convertGoSpan (Go.Span start end) = 
      Common.SourceSpan 
        (T.pack "<go-file>") 
        (convertGoPos start) 
        (convertGoPos end)
    convertGoPos (Go.Position line col) = 
      Common.SourcePos line col

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
    
    renderAccessLevel ind level =
      indentText ind <> level <> "\n"

-- | Render case statement
renderCase :: Int -> CppCase -> Text
renderCase indent (CppCase expr stmts) =
  indentText indent <> "case " <> renderExpr expr <> ":\n" <>
  T.unlines (map (renderStmt (indent + 1)) stmts) <>
  indentText (indent + 1) <> "break;"
renderCase indent (CppDefault stmts) =
  indentText indent <> "default:\n" <>
  T.unlines (map (renderStmt (indent + 1)) stmts) <>
  indentText (indent + 1) <> "break;"

-- | Render catch block
renderCatch :: Int -> CppCatch -> Text
renderCatch indent (CppCatch typ var stmts) =
  " catch (" <> renderType typ <> " " <> var <> ") {\n" <>
  T.unlines (map (renderStmt (indent + 1)) stmts) <>
  indentText indent <> "}"

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
    T.concat (map (renderCatch indent) catches) <>
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

-- | Generate indentation
indentText :: Int -> Text
indentText n = T.replicate n "    "

-- ============================================================================
-- Configuration and State Management
-- ============================================================================

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
-- Helper Functions
-- ============================================================================

-- | Add a statement to the current declarations
addStatement :: CppStmt -> CppCodeGen ()
addStatement _stmt = do
  -- Convert statement to declaration and add to state
  modify $ \s -> s { cgsDeclarations = CppVariable "__temp" CppVoid Nothing : cgsDeclarations s }
  -- For now, just track that we've added a statement
  return ()

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
    isFuncDef (Common.Located _ (PyFuncDef _)) = True
    isFuncDef _ = False

-- | Generate C++ from Python statement (returning Maybe to filter comments)
generatePythonStmtMaybe :: Common.Located PythonStmt -> CppCodeGen (Maybe CppStmt)
generatePythonStmtMaybe stmt = do
  result <- generatePythonStmt stmt
  case result of
    CppComment _ -> return Nothing
    other -> return (Just other)

-- | Generate C++ from Python statement
generatePythonStmt :: Common.Located PythonStmt -> CppCodeGen CppStmt
generatePythonStmt (Common.Located _ stmt) = case stmt of
  PyFuncDef funcDef -> do
    generatePythonFunction funcDef
    return $ CppComment "Function definition processed"
  
  PyClassDef classDef -> do
    generatePythonClass classDef
    return $ CppComment "Class definition processed"
  
  PyExprStmt expr -> do
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
  
  PyFor {pyForAsync = False, pyForTarget = pattern, pyForIter = iterExpr, pyForBody = bodyStmts, pyForElse = elseStmts} -> do
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
  
  PyWith {pyWithAsync = async, pyWithItems = _, pyWithBody = bodyStmts} -> do
    -- Simplified context manager handling
    when async $ addWarning "async with statements not supported"
    addWarning "with statement simplified to block"
    cppBody <- mapM generatePythonStmt bodyStmts
    return $ CppBlock cppBody
  
  PyRaise mexpr mcause -> do
    mcppExpr <- mapM generatePythonExpr mexpr
    _ <- mapM generatePythonExpr mcause  -- TODO: Handle cause in C++ exception
    return $ CppThrow mcppExpr
  
  PyTry tryStmts handlers _ finallyStmts -> do
    cppTry <- mapM generatePythonStmt tryStmts
    cppCatches <- mapM generatePythonExceptHandler handlers
    cppFinally <- mapM generatePythonStmt finallyStmts
    return $ CppTry cppTry cppCatches cppFinally
  
  PyImport _ -> return $ CppComment "Import statement (handled separately)"
  
  PyGlobal _ -> return $ CppComment "Global declaration"
  PyNonlocal _ -> return $ CppComment "Nonlocal declaration"
  
  PyPass -> return $ CppComment "pass"
  PyBreak -> return CppBreak
  PyContinue -> return CppContinue
  
  PyDel exprs -> do
    cppExprs <- mapM generatePythonExpr exprs
    return $ CppBlock $ map (CppExprStmt . CppDelete) cppExprs
  
  PyAssert expr _ -> do
    cppExpr <- generatePythonExpr expr
    addInclude "<cassert>"
    return $ CppExprStmt $ CppCall (CppVar "assert") [cppExpr]
  
  PyAnnAssign target annotation value -> do
    -- Type annotated assignment
    cppValue <- mapM generatePythonExpr value
    case locatedValue target of
      PatVar (Common.Identifier name) -> do
        cppType <- mapPythonTypeAnnotation annotation
        return $ CppDecl $ CppVariable name cppType cppValue
      _ -> do
        addWarning "Complex annotated assignment pattern not supported"
        return $ CppComment "Complex annotated assignment"
  
  PyFor {pyForAsync = True} -> do
    addWarning "Async for loops not supported"
    return $ CppComment "Async for loop (not supported)"
  
  PyAugAssign _ _ _ -> do
    addWarning "Augmented assignment not fully supported"
    return $ CppComment "Augmented assignment (not supported)"
  
  PyYield _ -> do
    addWarning "Yield expressions not supported"
    return $ CppComment "Yield expression (not supported)"
  
  PyYieldFrom _ -> do
    addWarning "Yield from expressions not supported"
    return $ CppComment "Yield from expression (not supported)"
  
  PyMatch _ -> do
    addWarning "Match statements not supported"
    return $ CppComment "Match statement (not supported)"

-- | Generate Python assignment
generatePythonAssignment :: [Common.Located PythonPattern] -> Common.Located PythonExpr -> CppCodeGen CppStmt
generatePythonAssignment patterns expr = do
  cppExpr <- generatePythonExpr expr
  case patterns of
    [Common.Located _ (PatVar (Common.Identifier varName))] -> do
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
      assignments <- forM (zip patterns [(0::Int)..]) $ \(pat, idx) ->
        case locatedValue pat of
          PatVar (Common.Identifier name) -> do
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
generatePythonForLoop :: Common.Located PythonPattern -> Common.Located PythonExpr -> [Common.Located PythonStmt] -> [Common.Located PythonStmt] -> CppCodeGen CppStmt
generatePythonForLoop pattern iterExpr bodyStmts _elseStmts = do
  case locatedValue pattern of
    PatVar (Common.Identifier varName) -> do
      cppIter <- generatePythonExpr iterExpr
      cppBody <- mapM generatePythonStmt bodyStmts
      
      -- Handle range() specially
      case locatedValue iterExpr of
        PyCall (Common.Located _ (PyVar (Common.Identifier "range"))) args -> 
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
          let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just $ CppLiteral $ CppIntLit 0)
              cond = Just $ CppBinary "<" (CppVar varName) cppEnd
              post = Just $ CppUnary "++" (CppVar varName)
          return $ CppFor initStmt cond post body
        
        [ArgPositional start, ArgPositional end] -> do
          cppStart <- generatePythonExpr start
          cppEnd <- generatePythonExpr end
          let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just cppStart)
              cond = Just $ CppBinary "<" (CppVar varName) cppEnd
              post = Just $ CppUnary "++" (CppVar varName)
          return $ CppFor initStmt cond post body
        
        [ArgPositional start, ArgPositional end, ArgPositional step] -> do
          cppStart <- generatePythonExpr start
          cppEnd <- generatePythonExpr end
          cppStep <- generatePythonExpr step
          let initStmt = Just $ CppDecl $ CppVariable varName CppInt (Just cppStart)
              cond = Just $ CppBinary "<" (CppVar varName) cppEnd
              post = Just $ CppBinary "+=" (CppVar varName) cppStep
          return $ CppFor initStmt cond post body
        
        _ -> do
          addWarning "Unsupported range() arguments"
          return $ CppComment "Unsupported range() call"

-- | Generate Python expression
generatePythonExpr :: Common.Located PythonExpr -> CppCodeGen CppExpr
generatePythonExpr (Common.Located _ expr) = case expr of
  PyLiteral lit -> generatePythonLiteral lit
  PyVar (Common.Identifier name) -> return $ CppVar name
  
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
    return $ CppMember cppObj (case attr of Identifier name -> name)
  
  PySubscript obj index -> do
    cppObj <- generatePythonExpr obj
    cppIndex <- generatePythonSlice index
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
    cppExpr <- generatePythonExpr body
    return $ CppLambda cppParams [CppReturn (Just cppExpr)] True
  
  PyIfExp cond thenExpr elseExpr -> do
    cppCond <- generatePythonExpr cond
    cppThen <- generatePythonExpr thenExpr
    cppElse <- generatePythonExpr elseExpr
    return $ CppTernary cppCond cppThen cppElse
  
  PyListComp _expr _comps -> do
    addWarning "Comprehensions not fully supported"
    return $ CppLiteral $ CppStringLit "comprehension"
  
  PyAwait _expr -> do
    addWarning "Async/await not supported"
    generatePythonExpr _expr
  
  PyStarred _expr -> generatePythonExpr _expr
  PySlice _ _ _ -> return $ CppLiteral $ CppStringLit "slice"
  PyNamedExpr _ _ -> return $ CppLiteral $ CppStringLit "named_expr"
  
  PyBoolOp _ _ -> do
    addWarning "Boolean operations not fully supported"
    return $ CppLiteral $ CppStringLit "bool_op"
  
  PySetComp _ _ -> do
    addWarning "Set comprehensions not supported"
    return $ CppLiteral $ CppStringLit "set_comp"
  
  PyDictComp _ _ _ -> do
    addWarning "Dictionary comprehensions not supported"
    return $ CppLiteral $ CppStringLit "dict_comp"
  
  PyGenComp _ _ -> do
    addWarning "Generator comprehensions not supported"
    return $ CppLiteral $ CppStringLit "gen_comp"
  
  PyFString _ -> do
    addWarning "F-strings not fully supported"
    return $ CppLiteral $ CppStringLit "fstring"

-- | Generate Python slice expression
generatePythonSlice :: Located PythonSlice -> CppCodeGen CppExpr
generatePythonSlice (Located _ slice) = case slice of
  SliceIndex index -> do
    cppIndex <- generatePythonExpr index
    return cppIndex
  SliceSlice start end _ -> do
    cppStart <- maybe (return $ CppLiteral $ CppIntLit 0) generatePythonExpr start
    cppEnd <- maybe (return $ CppLiteral $ CppIntLit (-1)) generatePythonExpr end
    return $ CppCall (CppVar "std::slice") [cppStart, cppEnd]
  SliceExtSlice _slices -> do
    addWarning "Extended slices not fully supported"
    return $ CppLiteral $ CppStringLit "extended_slice"

-- | Generate Python literal
generatePythonLiteral :: PythonLiteral -> CppCodeGen CppExpr
generatePythonLiteral lit = case lit of
  PyInt i -> return $ CppLiteral $ CppIntLit i
  PyFloat f -> return $ CppLiteral $ CppFloatLit f
  PyBool b -> return $ CppLiteral $ CppBoolLit b
  PyString s -> return $ CppLiteral $ CppStringLit s
  
  PyNone -> return $ CppLiteral CppNullPtr
  PyEllipsis -> do
    addWarning "Ellipsis not supported in expressions"
    return $ CppLiteral $ CppStringLit "..."
  PyBytes _ -> do
    addWarning "Bytes literals not supported"
    return $ CppLiteral $ CppStringLit ""
  PyComplex _ _ -> do
    addWarning "Complex numbers not supported"
    return $ CppLiteral $ CppFloatLit 0.0

-- F-string support functions temporarily removed to fix warnings

-- | Generate binary operation
generateBinaryOp :: Common.BinaryOp -> CppExpr -> CppExpr -> CppCodeGen CppExpr
generateBinaryOp op left right = case op of
  Common.OpAdd -> handleAddition left right
  Common.OpSub -> return $ CppBinary "-" left right
  Common.OpMul -> return $ CppBinary "*" left right
  OpDiv -> handleDivision left right
  OpFloorDiv -> do
    addInclude "<cmath>"
    return $ CppCall (CppVar "std::floor") [CppBinary "/" left right]
  OpMod -> return $ CppBinary "%" left right
  OpPow -> do
    addInclude "<cmath>"
    return $ CppCall (CppVar "std::pow") [left, right]
  Common.OpShiftL -> return $ CppBinary "<<" left right
  Common.OpShiftR -> return $ CppBinary ">>" left right
  OpBitOr -> return $ CppBinary "|" left right
  OpBitXor -> return $ CppBinary "^" left right
  OpBitAnd -> return $ CppBinary "&" left right
  Common.OpAnd -> return $ CppBinary "&&" left right
  Common.OpOr -> return $ CppBinary "||" left right
  _ -> do
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
            _ -> CppStaticCast CppDouble e
      return $ CppBinary "/" (ensureFloat l) (ensureFloat r)
    
    isStringExpr (CppLiteral (CppStringLit _)) = True
    isStringExpr (CppVar name) = "str" `T.isInfixOf` name
    isStringExpr _ = False

-- | Generate comparison
generatePythonComparison :: [Common.ComparisonOp] -> [Located PythonExpr] -> CppCodeGen CppExpr
generatePythonComparison ops exprs = do
  cppExprs <- mapM generatePythonExpr exprs
  case (ops, cppExprs) of
    ([op], [left, right]) -> 
      return $ CppBinary (mapPythonComparisonOp op) left right
    _ -> do
      -- Chain comparisons with &&
      case cppExprs of
        [] -> return $ CppLiteral $ CppBoolLit True  -- Empty list is vacuously true
        [_] -> return $ CppLiteral $ CppBoolLit True  -- Single expression is vacuously true
        _ -> let pairs = zip3 ops cppExprs (drop 1 cppExprs)
                 comparisons = map (\(op, l, r) -> CppBinary (mapPythonComparisonOp op) l r) pairs
             in return $ foldr1 (CppBinary "&&") comparisons

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
    handleBuiltinFunction "print" cppArgs = do
      addInclude "<iostream>"
      case cppArgs of
        [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
        _ -> do
          let chainOutput = foldl (CppBinary "<<") (CppVar "std::cout") $
                intercalateWith (CppLiteral (CppStringLit " ")) cppArgs
          return $ CppBinary "<<" chainOutput (CppVar "std::endl")
    
    handleBuiltinFunction "len" [arg] = do
      return $ CppCall (CppMember arg "size") []
    
    handleBuiltinFunction "range" _cppArgs = do
      -- This should be handled by for loops
      addWarning "range() outside of for loop"
      return $ CppVar "range_error"  -- Return a placeholder variable
    
    handleBuiltinFunction "str" [arg] = do
      addInclude "<string>"
      return $ CppCall (CppVar "std::to_string") [arg]
    
    handleBuiltinFunction "int" [arg] = 
      return $ CppStaticCast CppInt arg
    
    handleBuiltinFunction "float" [arg] = 
      return $ CppStaticCast CppDouble arg
    
    handleBuiltinFunction "bool" [arg] = 
      return $ CppStaticCast CppBool arg
    
    handleBuiltinFunction "abs" [arg] = do
      addInclude "<cmath>"
      return $ CppCall (CppVar "std::abs") [arg]
    
    handleBuiltinFunction "min" cppArgs = do
      addInclude "<algorithm>"
      return $ CppCall (CppVar "std::min") cppArgs
    
    handleBuiltinFunction "max" cppArgs = do
      addInclude "<algorithm>"
      return $ CppCall (CppVar "std::max") cppArgs
    
    handleBuiltinFunction _ _ = 
      -- This should never be reached since non-builtin functions are handled in the main case
      error "handleBuiltinFunction: should not reach here for non-builtin functions"
    
    intercalateWith _sep [] = []
    intercalateWith _sep [x] = [x]
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
  let funcName = case pyFuncName funcDef of Identifier name -> name
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
  let className = case pyClassName classDef of Identifier name -> name
  baseClasses <- mapM extractBaseClassName (pyClassBases classDef)
  
  -- Process class body
  members <- concat <$> mapM (generateClassMember className) (pyClassBody classDef)
  
  addDeclaration $ CppClass className baseClasses members
  where
    generateClassMember className stmt = case locatedValue stmt of
      PyFuncDef funcDef -> do
        let methodName = case pyFuncName funcDef of Identifier name -> name
        cppParams <- mapM mapPythonParameter (pyFuncParams funcDef)
        returnType <- inferReturnType (pyFuncBody funcDef)
        bodyStmts <- mapM generatePythonStmt (pyFuncBody funcDef)
        
        if methodName == "__init__"
        then case cppParams of
               [] -> return [CppConstructor className [] bodyStmts]
               (_:rest) -> return [CppConstructor className rest bodyStmts]
        else case cppParams of
               [] -> return [CppMethod methodName returnType [] bodyStmts False]
               (_:rest) -> return [CppMethod methodName returnType rest bodyStmts False]
      
      PyAssign [Located _ (PatVar (Identifier varName))] expr -> do
        cppExpr <- generatePythonExpr expr
        return [CppVariable varName CppAuto (Just cppExpr)]
      
      _ -> return []

-- | Generate exception handler
generatePythonExceptHandler :: Located PythonExcept -> CppCodeGen CppCatch
generatePythonExceptHandler (Located _ (PythonExcept mtype mname body _)) = do
  let exType = fromMaybe (CppClassType "std::exception" []) 
                        (fmap (const $ CppClassType "std::exception" []) mtype)
      varName = fromMaybe "e" (fmap (\(Identifier name) -> name) mname)
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
  
  let packageName = case goPackageName goPackage of Identifier name -> name
  
  -- Generate channel implementation if needed
  when (needsChannels goPackage) generateChannelClass
  
  -- Process files
  mapM_ generateGoFile (goPackageFiles goPackage)
  
  -- Ensure main function if needed
  when (isMainFile && packageName == "main") $ ensureMainFunction []
  
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
  let decls = goFileDecls goFile
  -- Debug: Print number of declarations found
  addWarning $ "Processing Go file with " <> T.pack (show (length decls)) <> " declarations"
  mapM_ (generateGoDecl . convertGoLocated) decls

-- | Generate Go declaration
generateGoDecl :: Located GoDecl -> CppCodeGen ()
generateGoDecl (Located _ decl) = do
  -- Debug: Print declaration type
  addWarning $ "Processing Go declaration: " <> T.pack (show decl)
  case decl of
    GoFuncDecl func -> generateGoFunction func
    
    GoTypeDeclStmt typeDecl -> do
      cppType <- generateGoType (convertGoLocated (goTypeDeclType typeDecl))
      let name = case goTypeDeclName typeDecl of Identifier nameStr -> nameStr
      -- TODO: Handle type parameters and alias vs distinction
      addDeclaration $ CppTypedef name cppType
    
    GoBindDecl bindings -> mapM_ generateGoBinding bindings
    
    GoConstDecl consts -> mapM_ generateGoConstant' consts
    
    GoMethodDecl receiver func -> 
      generateGoMethod receiver func
    
    _ -> addWarning $ "Unsupported Go declaration: " <> T.pack (show decl)

-- | Generate Go function
generateGoFunction :: GoFunction -> CppCodeGen ()
generateGoFunction func = case goFuncName func of
  Nothing -> return () -- Anonymous function
  Just name -> do
    let funcName = case name of Identifier funcNameStr -> funcNameStr
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
    let methodName = case name of Identifier methodNameStr -> methodNameStr
        receiverType = case goReceiverType receiver of
          Go.Located _ t -> renderType (mapGoTypeToCpp t)
        receiverName = fromMaybe "this" (fmap (\(Identifier nameStr) -> nameStr) (goReceiverName receiver))
    
    cppParams <- concat <$> mapM expandGoField (goFuncParams func)
    returnType <- mapGoResults methodName (goFuncResults func)
    
    bodyStmts <- case goFuncBody func of
      Nothing -> return []
      Just body -> generateGoBlockStmt body
    
    -- For now, generate as a regular function with receiver as first parameter
    let fullParams = CppParam receiverName (CppReference (CppAuto)) Nothing : cppParams
    addDeclaration $ CppFunction (receiverType <> "_" <> methodName) returnType fullParams bodyStmts

-- | Generate Go statement
generateGoStmt :: Go.Located GoStmt -> CppCodeGen CppStmt
generateGoStmt (Go.Located _ stmt) = case stmt of
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
  
  GoBind binding -> case bindKind binding of
    BindDefine -> case bindLHS binding of
      LHSIdents ids -> generateGoDefine ids (bindRHS binding)
      LHSExprs _ -> do
        addWarning "Complex LHS in define binding not supported"
        return $ CppComment "Complex LHS in define"
    BindAssign -> case bindLHS binding of
      LHSExprs exprs -> generateGoAssign exprs (bindRHS binding)
      LHSIdents _ -> do
        addWarning "Identifier LHS in assign binding not supported"
        return $ CppComment "Identifier LHS in assign"
    BindVar -> generateGoVarDecls (bindLHS binding) (bindType binding) (bindRHS binding)
  
  GoIncDec expr isInc -> do
    cppExpr <- generateGoExpr expr
    let op = if isInc then "++" else "--"
    return $ CppExprStmt $ CppUnary op cppExpr
  
  GoBreak _ -> return CppBreak
  GoContinue _ -> return CppContinue
  GoGoto label -> return $ CppGoto (case label of Identifier name -> name)
  GoLabeled label stmt -> do
    let name = case label of Identifier name -> name
    _ <- generateGoStmt stmt  -- Generate the labeled statement
    return $ CppLabel name
  GoFallthrough -> return $ CppComment "fallthrough"
  
  _ -> do
    addWarning $ "Unsupported Go statement: " <> T.pack (show stmt)
    return $ CppComment "Unsupported statement"

-- | Generate Go expression
generateGoExpr :: Go.Located GoExpr -> CppCodeGen CppExpr
generateGoExpr (Go.Located _ expr) = case expr of
  GoLiteral lit -> return $ CppLiteral $ mapGoLiteral lit
  GoIdent name -> return $ CppVar (case name of Identifier nameStr -> nameStr)
  
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
    return $ CppBinary (mapGoComparisonOp op) cppLeft cppRight
  
  GoCall func args -> generateGoCall func args
  
  GoSelector obj member -> do
    cppObj <- generateGoExpr obj
    return $ CppMember cppObj (case member of Identifier name -> name)
  
  GoIndex array index -> do
    cppArray <- generateGoExpr array
    cppIndex <- generateGoExpr index
    return $ CppIndex cppArray cppIndex
  
  GoSlice array sliceExpr -> do
    addWarning "Slice operations not fully supported"
    generateGoExpr array
  
  
  
  GoReceive channel -> do
    cppChannel <- generateGoExpr channel
    return $ CppCall (CppMember cppChannel "receive") []
  
  GoCompositeLit compositeLit -> generateGoCompositeLiteral compositeLit
  
  
  
  _ -> do
    addWarning $ "Unsupported Go expression: " <> T.pack (show expr)
    return $ CppLiteral $ CppIntLit 0

-- | Generate Go call
generateGoCall :: Go.Located GoExpr -> [Go.Located GoExpr] -> CppCodeGen CppExpr
generateGoCall func args = do
  cppFunc <- generateGoExpr func
  cppArgs <- mapM generateGoExpr args
  
  -- Handle special functions
  case func of
    Go.Located _ (GoQualifiedIdent (Identifier "fmt") (Identifier fname)) ->
      handleFmtFunction fname cppArgs
    Go.Located _ (GoIdent (Identifier "println")) -> do
      addInclude "<iostream>"
      return $ buildPrintExpr cppArgs True
    Go.Located _ (GoIdent (Identifier "print")) -> do
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
    handleFmtFunction name args = 
      return $ CppCall (CppVar $ "fmt_" <> name) args  -- Fallback for unknown fmt functions

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
generateGoCompositeLiteral :: GoCompositeLiteral -> CppCodeGen CppExpr
generateGoCompositeLiteral (GoCompositeLiteral mType elems) = do
  -- Generate type if present
  case mType of
    Just typeExpr -> do
      cppType <- generateGoExpr typeExpr
      cppElems <- mapM generateGoCompElem elems
      return $ CppInitList (cppTypeToCppType cppType) cppElems
    Nothing -> do
      -- Infer type from elements
      cppElems <- mapM generateGoCompElem elems
      return $ CppInitList CppAuto cppElems
  where
    cppTypeToCppType (CppVar name) = CppTypeVar name
    cppTypeToCppType _other = CppAuto
    
    generateGoCompElem :: GoCompElem -> CppCodeGen CppExpr
    generateGoCompElem (CompElemValue expr) = generateGoExpr expr
    generateGoCompElem (CompElemKeyed key value) = do
      cppKey <- generateGoExpr key
      cppValue <- generateGoExpr value
      return $ CppInitList (CppPair CppAuto CppAuto) [cppKey, cppValue]
    generateGoCompElem (CompElemField (Identifier fieldName) value) = do
      cppValue <- generateGoExpr value
      return $ CppInitList CppAuto [CppLiteral (CppStringLit fieldName), cppValue]

-- Go literal support functions temporarily removed to fix warnings

-- | Generate Go define statement
generateGoDefine :: [Identifier] -> [Go.Located GoExpr] -> CppCodeGen CppStmt
generateGoDefine names exprs = do
  if length names == length exprs
  then do
    decls <- forM (zip names exprs) $ \(name, expr) -> do
      cppExpr <- generateGoExpr expr
      return $ CppDecl $ CppVariable (case name of Identifier nameStr -> nameStr) CppAuto (Just cppExpr)
    case decls of
      [single] -> return single
      multiple -> return $ CppBlock multiple
  else if length exprs == 1
  then do
    -- Tuple unpacking
    case exprs of
      [expr] -> do
        cppExpr <- generateGoExpr expr
        let varNames = map (\(Identifier name) -> name) names
            decls = map (\n -> CppDecl $ CppVariable n CppAuto Nothing) varNames
            tie = CppCall (CppVar "std::tie") (map CppVar varNames)
            assign = CppExprStmt $ CppBinary "=" tie cppExpr
        return $ CppBlock (decls ++ [assign])
      _ -> do
        addWarning "Unexpected empty expression list in tuple unpacking"
        return $ CppComment "Empty expression list"
  else do
    addWarning "Mismatched define statement"
    return $ CppComment "Mismatched define"

-- | Generate Go assignment
generateGoAssign :: [Go.Located GoExpr] -> [Go.Located GoExpr] -> CppCodeGen CppStmt
generateGoAssign lefts rights = do
  if length lefts == length rights
  then do
    assigns <- forM (zip lefts rights) $ \(left, right) -> do
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
      [right] -> do
        cppRight <- generateGoExpr right
        cppLefts <- mapM generateGoExpr lefts
        let tie = CppCall (CppVar "std::tie") cppLefts
        return $ CppExprStmt $ CppBinary "=" tie cppRight
      _ -> do
        addWarning "Unexpected empty expression list in assignment"
        return $ CppComment "Empty expression list"
  else do
    addWarning "Mismatched assignment"
    return $ CppComment "Mismatched assignment"

-- | Generate Go variable declarations (multiple)
generateGoVarDecls :: BindingLHS -> Maybe (Go.Located GoType) -> [Go.Located GoExpr] -> CppCodeGen CppStmt
generateGoVarDecls lhs mType exprs = case lhs of
  LHSIdents identifiers -> do
    cppType <- case mType of
      Nothing -> return CppAuto
      Just t -> generateGoType (convertGoLocated t)
    case exprs of
      [] -> do
        -- No initializers, declare all variables
        let decls = map (\ident -> CppVariable (case ident of Identifier name -> name) cppType Nothing) identifiers
        return $ CppBlock $ map CppDecl decls
      [expr] -> do
        -- Single initializer, handle multiple variables with same value
        cppExpr <- generateGoExpr expr
        let decls = map (\ident -> CppVariable (case ident of Identifier name -> name) cppType (Just cppExpr)) identifiers
        return $ CppBlock $ map CppDecl decls
      _ -> do
        -- Multiple initializers, match one-to-one
        if length identifiers == length exprs
        then do
          cppExprs <- mapM generateGoExpr exprs
          let decls = zipWith (\ident expr -> CppVariable (case ident of Identifier name -> name) cppType (Just expr)) identifiers cppExprs
          return $ CppBlock $ map CppDecl decls
        else do
          addWarning "Mismatched number of variables and initializers"
          return $ CppComment "Mismatched variable declaration"
  LHSExprs _ -> do
    addWarning "Complex LHS in variable declaration not supported"
    return $ CppComment "Unsupported variable declaration"

-- | Generate Go binding
generateGoBinding :: GoBinding -> CppCodeGen ()
generateGoBinding binding = case bindKind binding of
  BindVar -> do
    stmt <- generateGoVarDecls (bindLHS binding) (bindType binding) (bindRHS binding)
    addStatement stmt
  BindDefine -> do
    case bindLHS binding of
      LHSIdents identifiers -> do
        case bindRHS binding of
          [expr] -> do
            cppExpr <- generateGoExpr expr
            let stmts = map (\ident -> CppExprStmt $ CppBinary "=" (CppVar (case ident of Identifier name -> name)) cppExpr) identifiers
            addStatement $ CppBlock stmts
          _ -> addWarning "Multiple expressions in define binding not supported"
      LHSExprs exprs -> do
        case bindRHS binding of
          [expr] -> do
            cppExpr <- generateGoExpr expr
            cppExprs <- mapM generateGoExpr exprs
            let stmt = CppExprStmt $ foldl1 (\acc e -> CppBinary "=" acc e) (cppExprs ++ [cppExpr])
            addStatement stmt
          _ -> addWarning "Multiple expressions in define binding not supported"
  BindAssign -> do
    case bindLHS binding of
      LHSExprs exprs -> do
        case bindRHS binding of
          [expr] -> do
            cppExpr <- generateGoExpr expr
            cppExprs <- mapM generateGoExpr exprs
            let stmt = CppExprStmt $ foldl1 (\acc e -> CppBinary "=" acc e) (cppExprs ++ [cppExpr])
            addStatement stmt
          _ -> addWarning "Multiple expressions in assign binding not supported"
      LHSIdents _ -> addWarning "Identifier LHS in assign binding not supported"



-- | Generate Go case
generateGoCase :: CaseClause [Go.Located GoExpr] -> CppCodeGen CppCase
generateGoCase goCase = case caseCond goCase of
  Nothing -> CppDefault <$> mapM generateGoStmt (caseBody goCase)
  Just [] -> CppDefault <$> mapM generateGoStmt (caseBody goCase)
  Just (expr:_) -> do
    cppExpr <- generateGoExpr expr
    stmts <- mapM generateGoStmt (caseBody goCase)
    return $ CppCase cppExpr stmts

-- | Generate Go block statement
generateGoBlockStmt :: Go.Located GoStmt -> CppCodeGen [CppStmt]
generateGoBlockStmt stmt = generateGoStmtBlock stmt

-- | Generate Go statement block
generateGoStmtBlock :: Go.Located GoStmt -> CppCodeGen [CppStmt]
generateGoStmtBlock (Go.Located _ (GoBlock stmts)) = mapM generateGoStmt stmts
generateGoStmtBlock stmt = (:[]) <$> generateGoStmt stmt

-- | Generate Go variable
generateGoVariable :: (Identifier, Maybe (Go.Located GoType), Maybe (Go.Located GoExpr)) -> CppCodeGen ()
generateGoVariable (name, mType, mExpr) = do
  cppType <- case mType of
    Nothing -> return CppAuto
    Just t -> generateGoType (convertGoLocated t)
  cppExpr <- mapM generateGoExpr mExpr
  addDeclaration $ CppVariable (case name of Identifier nameStr -> nameStr) cppType cppExpr

-- | Generate Go constant
generateGoConstant :: (Identifier, Maybe (Go.Located GoType), Maybe (Go.Located GoExpr)) -> CppCodeGen ()
generateGoConstant (name, mType, mExpr) = do
  cppType <- case mType of
    Nothing -> return CppAuto
    Just t -> generateGoType (convertGoLocated t)
  cppExpr <- mapM generateGoExpr mExpr
  addDeclaration $ CppVariable (case name of Identifier nameStr -> nameStr) (CppConst cppType) cppExpr

-- | Generate Go constant from GoConstSpec
generateGoConstant' :: GoConstSpec -> CppCodeGen ()
generateGoConstant' (GoConstSpec names mType mValues) = do
  cppType <- case mType of
    Nothing -> return CppAuto
    Just t -> generateGoType (convertGoLocated t)
  case mValues of
    Nothing -> return ()  -- TODO: Handle repeating last expression
    Just values -> do
      cppValues <- mapM generateGoExpr values
      case zip names cppValues of
        [(name, value)] -> do
          addDeclaration $ CppVariable (case name of Identifier nameStr -> nameStr) (CppConst cppType) (Just value)
        _ -> do
          -- Multiple constants with same values (simplified)
          case cppValues of
            [] -> return ()  -- No values to assign
            (firstValue:_) -> mapM_ (\name -> addDeclaration $ CppVariable (case name of Identifier nameStr -> nameStr) (CppConst cppType) (Just firstValue)) names

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
    return $ CppParam (case name of Identifier nameStr -> nameStr) cppType cppDefault
  ParamVarArgs name mType -> do
    cppType <- maybe (return CppAuto) mapPythonTypeAnnotation mType
    return $ CppParam (case name of Identifier nameStr -> nameStr) (CppVector CppAuto) Nothing
  ParamKwArgs name mType -> do
    cppType <- maybe (return CppAuto) mapPythonTypeAnnotation mType
    return $ CppParam (case name of Identifier nameStr -> nameStr) (CppUnorderedMap CppString CppAuto) Nothing
  ParamPosOnly name mType mDefault -> do
    cppType <- maybe (return CppAuto) mapPythonTypeAnnotation mType
    cppDefault <- mapM generatePythonExpr mDefault
    return $ CppParam (case name of Identifier nameStr -> nameStr) cppType cppDefault
  ParamKwOnly name mType mDefault -> do
    cppType <- maybe (return CppAuto) mapPythonTypeAnnotation mType
    cppDefault <- mapM generatePythonExpr mDefault
    return $ CppParam (case name of Identifier nameStr -> nameStr) cppType cppDefault

-- | Map Python lambda parameter
mapPythonLambdaParam :: Located PythonParameter -> CppCodeGen CppParam
mapPythonLambdaParam = mapPythonParameter

-- | Map Python type annotation
mapPythonTypeAnnotation :: Located PythonTypeExpr -> CppCodeGen CppType
mapPythonTypeAnnotation (Located _ typeExpr) = case typeExpr of
  TypeName (Common.QualifiedName _ (Common.Identifier name)) -> return $ case name of
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
        TypeName (Common.QualifiedName _ (Common.Identifier "List")) -> 
          case args of
            [] -> return CppAuto  -- Fallback for empty args
            (arg:_) -> CppVector <$> mapPythonTypeAnnotation arg
        TypeName (Common.QualifiedName _ (Common.Identifier "Dict")) -> do
          case args of
            [] -> return CppAuto  -- Fallback for empty args
            [keyArg] -> do  -- Only key provided
              keyType <- mapPythonTypeAnnotation keyArg
              return $ CppUnorderedMap keyType CppAuto
            (keyArg:valArg:_) -> do  -- Both key and value provided
              keyType <- mapPythonTypeAnnotation keyArg
              valType <- mapPythonTypeAnnotation valArg
              return $ CppUnorderedMap keyType valType
        TypeName (Common.QualifiedName _ (Common.Identifier "Optional")) ->
          case args of
            [] -> return CppAuto  -- Fallback for empty args
            (arg:_) -> CppOptional <$> mapPythonTypeAnnotation arg
        TypeName (Common.QualifiedName _ (Common.Identifier "Tuple")) -> do
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
      typ = mapGoTypeToCpp (goLocatedValue (goFieldType field))
  in return $ map (\(Common.Identifier n) -> CppParam n typ Nothing) names

-- | Map Go results
mapGoResults :: Text -> [GoField] -> CppCodeGen CppType
mapGoResults "main" [] = return CppInt
mapGoResults _ [] = return CppVoid
mapGoResults _ [field] = generateGoType (convertGoLocated (goFieldType field))
mapGoResults _ fields = do
  types <- mapM (generateGoType . convertGoLocated . goFieldType) fields
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
mapPythonUnaryOp :: Common.UnaryOp -> Text
mapPythonUnaryOp op = case op of
  Common.OpNot -> "!"
  Common.OpBitNot -> "~"
  Common.OpPositive -> "+"
  Common.OpNegate -> "-"

mapGoBinaryOp :: Go.BinaryOp -> Text
mapGoBinaryOp op = case op of
  Go.OpAdd -> "+"
  Go.OpSub -> "-"
  Go.OpMul -> "*"
  Go.OpQuo -> "/"
  Go.OpRem -> "%"
  Go.OpAnd -> "&"
  Go.OpOr -> "|"
  Go.OpXor -> "^"
  Go.OpAndNot -> "&^"
  Go.OpShiftL -> "<<"
  Go.OpShiftR -> ">>"
  Go.OpAndAnd -> "&&"
  Go.OpOrOr -> "||"

mapGoUnaryOp :: Go.UnaryOp -> Text
mapGoUnaryOp op = case op of
  Go.OpPos -> "+"
  Go.OpNeg -> "-"
  Go.OpNot -> "!"
  Go.OpBitNot -> "^"

mapGoComparisonOp :: Go.ComparisonOp -> Text
mapGoComparisonOp op = case op of
  Go.OpEq -> "=="
  Go.OpNe -> "!="
  Go.OpLt -> "<"
  Go.OpLe -> "<="
  Go.OpGt -> ">"
  Go.OpGe -> ">="

mapPythonBinaryOp :: Common.BinaryOp -> Text
mapPythonBinaryOp op = case op of
  Common.OpAdd -> "+"
  Common.OpSub -> "-"
  Common.OpMul -> "*"
  Common.OpDiv -> "/"
  Common.OpMod -> "%"
  Common.OpAnd -> "&&"
  Common.OpOr -> "||"
  Common.OpBitAnd -> "&"
  Common.OpBitOr -> "|"
  Common.OpBitXor -> "^"
  Common.OpShiftL -> "<<"
  Common.OpShiftR -> ">>"
  _ -> "+"

mapPythonComparisonOp :: Common.ComparisonOp -> Text
mapPythonComparisonOp op = case op of
  Common.OpEq -> "=="
  Common.OpNe -> "!="
  Common.OpLt -> "<"
  Common.OpLe -> "<="
  Common.OpGt -> ">"
  Common.OpGe -> ">="
  Common.OpIs -> "=="
  Common.OpIsNot -> "!="

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
  GoSliceType (Go.Located _ elemType) -> CppVector (mapGoTypeToCpp elemType)
  GoArrayType size (Go.Located _ elemType) -> 
    let s = case goLocatedValue size of
              GoLiteral (GoInt i) -> fromIntegral i
              _ -> 0
    in CppArray (mapGoTypeToCpp elemType) s
  GoMapType (Go.Located _ k) (Go.Located _ v) -> 
    CppUnorderedMap (mapGoTypeToCpp k) (mapGoTypeToCpp v)
  GoPointerType (Go.Located _ base) -> CppPointer (mapGoTypeToCpp base)
  GoChanType _ (Go.Located _ elem) -> 
    CppTemplateType "Channel" [mapGoTypeToCpp elem]
  GoFuncType params results ->
    let paramTypes = map (mapGoTypeToCpp . goLocatedValue . goFieldType) params
        resultType = case results of
          [] -> CppVoid
          [r] -> mapGoTypeToCpp (goLocatedValue (goFieldType r))
          rs -> CppTuple (map (mapGoTypeToCpp . goLocatedValue . goFieldType) rs)
    in CppFunctionType paramTypes resultType
  GoInterfaceType _ -> CppPointer CppVoid
  GoStructType fields -> CppAuto -- Simplified
  _ -> CppAuto

mapCommonTypeToCpp :: Type -> CppType
mapCommonTypeToCpp = mapPythonTypeToCpp

-- | Extract identifier
unIdentifier :: Identifier -> Text
unIdentifier (Identifier name) = name