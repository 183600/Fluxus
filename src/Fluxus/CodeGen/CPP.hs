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
  , CppVar(..)
  , CppFunction(..)
  , CppClass(..)
  , CppTypeDef(..)
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (when, unless, forM, forM_, foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (partition, nub)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | C++ code generation monad
type CppCodeGen = State CppGenState

-- | C++ code generation state
data CppGenState = CppGenState
  { cgIndentLevel :: Int
  , cgOutput :: Text
  , cgIncludes :: [Text]
  , cgForwardDecls :: [Text]
  , cgNamespaceStack :: [Text]
  , cgCurrentFunction :: Maybe Text
  , cgLocalVars :: HashMap Text CppType
  , cgGlobalVars :: HashMap Text CppType
  , cgTypeDefs :: HashMap Text CppType
  , cgFunctions :: HashMap Text CppFunction
  , cgClasses :: HashMap Text CppClass
  , cgNextTempId :: Int
  , cgConfig :: CppGenConfig
  }

-- | C++ code generation configuration
data CppGenConfig = CppGenConfig
  { cgUseModernCpp :: Bool
  , cgOptimizeLevel :: Int
  , cgDebugMode :: Bool
  , cgTargetStandard :: Text
  , cgUseExceptions :: Bool
  , cgUseRTTI :: Bool
  , cgNamespace :: Maybe Text
  , cgHeaderGuardPrefix :: Text
  }

-- | C++ AST unit (top-level construct)
data CppUnit = CppUnit
  { cuIncludes :: [Text]
  , cuForwardDecls :: [Text]
  , cuNamespace :: Maybe Text
  , cuDeclarations :: [CppDecl]
  }

-- | C++ declaration
data CppDecl
  = CppFunctionDecl CppFunction
  | CppClassDecl CppClass
  | CppVarDecl CppVar
  | CppVariable !Text !CppType !(Maybe CppExpr)
  | CppTypeDecl CppTypeDef
  | CppNamespaceDecl Text [CppDecl]
  | CppIncludeDecl Text
  | CppUsingDecl Text Text
  deriving (Eq, Show, Generic)

-- | C++ statement
data CppStmt
  = CppExprStmt CppExpr
  | CppReturn (Maybe CppExpr)
  | CppIf CppExpr CppStmt (Maybe CppStmt)
  | CppWhile CppExpr CppStmt
  | CppFor (Maybe CppExpr) (Maybe CppExpr) (Maybe CppExpr) CppStmt
  | CppForRange Text CppExpr [CppStmt]
  | CppBlock [CppStmt]
  | CppVarDeclStmt CppVar
  | CppBreak
  | CppContinue
  | CppThrow CppExpr
  | CppTry CppStmt [(CppType, Text, CppStmt)]
  deriving (Eq, Show, Generic)

-- | C++ expression
data CppExpr
  = CppVarRef Text
  | CppLiteral CppLiteral
  | CppBinary Text CppExpr CppExpr
  | CppUnary Text CppExpr
  | CppCall CppExpr [CppExpr]
  | CppMember CppExpr Text
  | CppPointerMember CppExpr Text
  | CppIndex CppExpr CppExpr
  | CppCast CppType CppExpr
  | CppSizeOf CppType
  | CppNew CppType [CppExpr]
  | CppDelete CppExpr
  | CppThis
  | CppLambda [CppParam] [CppStmt] Bool
  | CppMove CppExpr
  | CppForward CppExpr
  | CppMakeUnique CppType [CppExpr]
  | CppMakeShared CppType [CppExpr]
  | CppInitList CppType [CppExpr]
  | CppTernary CppExpr CppExpr CppExpr
  | CppComma [CppExpr]
  | CppStaticCast CppType CppExpr
  | CppDynamicCast CppType CppExpr
  | CppReinterpretCast CppType CppExpr
  | CppConstCast CppType CppExpr
  deriving (Eq, Show, Generic)

-- | C++ type
data CppType
  = CppVoid
  | CppBool
  | CppInt Int -- bit width
  | CppUInt Int -- bit width
  | CppFloat
  | CppDouble
  | CppChar
  | CppString
  | CppAuto
  | CppPointer CppType
  | CppReference CppType
  | CppConst CppType
  | CppVolatile CppType
  | CppSizeT
  | CppFunctionType [CppType] CppType
  | CppClassType Text [CppType]
  | CppTemplateType Text [CppType]
  | CppUniquePtr CppType
  | CppSharedPtr CppType
  | CppOptional CppType
  | CppVariant [CppType]
  | CppPair CppType CppType
  | CppTuple [CppType]
  | CppMap CppType CppType
  | CppUnorderedMap CppType CppType
  | CppTypeVar Text
  | CppDecltype CppExpr
  deriving (Eq, Show, Generic)

-- | C++ literal
data CppLiteral
  = CppIntLit Integer
  | CppFloatLit Double
  | CppStringLit Text
  | CppCharLit Char
  | CppBoolLit Bool
  | CppNullLit
  deriving (Eq, Show, Generic)

-- | C++ function parameter
data CppParam = CppParam
  { cpName :: Text
  , cpType :: CppType
  , cpDefault :: Maybe CppExpr
  }
  deriving (Eq, Show, Generic)

-- | C++ function
data CppFunction = CppFunction
  { cfName :: Text
  , cfReturnType :: CppType
  , cfParams :: [CppParam]
  , cfBody :: Maybe CppStmt
  , cfIsInline :: Bool
  , cfIsConst :: Bool
  , cfIsVirtual :: Bool
  , cfIsPureVirtual :: Bool
  , cfTemplateParams :: [Text]
  }
  deriving (Eq, Show, Generic)

-- | C++ typedef
data CppTypeDef = CppTypeDef
  { tdName :: Text
  , tdType :: CppType
  }
  deriving (Eq, Show, Generic)

-- | C++ class
data CppClass = CppClass
  { ccName :: Text
  , ccBaseClasses :: [Text]
  , ccMembers :: [CppVar]
  , ccMethods :: [CppFunction]
  , ccIsStruct :: Bool
  , ccTemplateParams :: [Text]
  }
  deriving (Eq, Show, Generic)

-- | C++ variable
data CppVar = CppVar
  { cvName :: Text
  , cvType :: CppType
  , cvInitializer :: Maybe CppExpr
  , cvIsStatic :: Bool
  , cvIsConst :: Bool
  , cvIsExtern :: Bool
  }
  deriving (Eq, Show, Generic)

-- Default configurations
defaultCppGenConfig :: CppGenConfig
defaultCppGenConfig = CppGenConfig
  { cgUseModernCpp = True
  , cgOptimizeLevel = 2
  , cgDebugMode = False
  , cgTargetStandard = "C++20"
  , cgUseExceptions = True
  , cgUseRTTI = False
  , cgNamespace = Nothing
  , cgHeaderGuardPrefix = "FLUXUS_"
  }

initialCppGenState :: CppGenConfig -> CppGenState
initialCppGenState config = CppGenState
  { cgIndentLevel = 0
  , cgOutput = ""
  , cgIncludes = []
  , cgForwardDecls = []
  , cgNamespaceStack = []
  , cgCurrentFunction = Nothing
  , cgLocalVars = HM.empty
  , cgGlobalVars = HM.empty
  , cgTypeDefs = HM.empty
  , cgFunctions = HM.empty
  , cgClasses = HM.empty
  , cgNextTempId = 0
  , cgConfig = config
  }

-- Placeholder implementations for required functions
generateCpp :: CppGenConfig -> CppUnit -> Text
generateCpp _config unit = "// C++ code generation placeholder\n" <> prettyCppUnit unit

generateCppMain :: CppGenConfig -> CppUnit -> Text
generateCppMain config unit = generateCpp config unit <> "\nint main() { return 0; }"

generateCppFromPython :: CppGenConfig -> Text -> Either Text Text
generateCppFromPython _config _pythonCode = Right "// Python to C++ conversion placeholder"

generateCppFromGo :: CppGenConfig -> Text -> Either Text Text
generateCppFromGo _config _goCode = Right "// Go to C++ conversion placeholder"

-- Pretty printing
renderCppUnit :: CppUnit -> Text
renderCppUnit (CppUnit includes fwdDecls namespace decls) = T.unlines
  [ T.unlines (map ("#include " <>) includes)
  , T.unlines fwdDecls
  , maybe "" (\ns -> "namespace " <> ns <> " {") namespace
  , T.unlines (map renderCppDecl decls)
  , maybe "" (const "}") namespace
  ]

prettyCppUnit :: CppUnit -> Text
prettyCppUnit = renderCppUnit

renderCppDecl :: CppDecl -> Text
renderCppDecl decl = case decl of
  CppFunctionDecl func -> renderCppFunction func
  CppClassDecl cls -> renderCppClass cls
  CppVarDecl var -> renderCppVar var
  CppTypeDecl typedef -> renderCppTypeDef typedef
  CppNamespaceDecl name decls -> "namespace " <> name <> " {\n" <> T.unlines (map renderCppDecl decls) <> "}"
  CppIncludeDecl inc -> "#include " <> inc
  CppUsingDecl from to -> "using " <> to <> " = " <> from <> ";"

renderCppFunction :: CppFunction -> Text
renderCppFunction (CppFunction name retType params body _inline _const _virtual _pureVirtual _template) =
  let paramStr = T.intercalate ", " (map renderCppParam params)
      bodyStr = maybe ";" (\b -> " {\n" <> renderCppStmt b <> "}") body
  in renderCppType retType <> " " <> name <> "(" <> paramStr <> ")" <> bodyStr

renderCppClass :: CppClass -> Text
renderCppClass (CppClass name bases members methods isStruct _template) =
  let classKw = if isStruct then "struct" else "class"
      baseStr = if null bases then "" else " : " <> T.intercalate ", " bases
      memberStr = T.unlines (map renderCppVar members)
      methodStr = T.unlines (map renderCppFunction methods)
  in classKw <> " " <> name <> baseStr <> " {\n" <> memberStr <> "\n" <> methodStr <> "};"

renderCppVar :: CppVar -> Text
renderCppVar (CppVar name typ init _static _const _extern) =
  let initStr = maybe "" (\i -> " = " <> renderCppExpr i) init
  in renderCppType typ <> " " <> name <> initStr <> ";"

renderCppTypeDef :: CppTypeDef -> Text
renderCppTypeDef (CppTypeDef name typ) = "using " <> name <> " = " <> renderCppType typ <> ";"

renderCppParam :: CppParam -> Text
renderCppParam (CppParam name typ mDefault) =
  let defaultStr = maybe "" (\d -> " = " <> renderCppExpr d) mDefault
  in renderCppType typ <> " " <> name <> defaultStr

renderCppStmt :: CppStmt -> Text
renderCppStmt stmt = case stmt of
  CppExprStmt expr -> renderCppExpr expr <> ";"
  CppReturn Nothing -> "return;"
  CppReturn (Just expr) -> "return " <> renderCppExpr expr <> ";"
  CppIf cond thenStmt elseStmt ->
    "if (" <> renderCppExpr cond <> ") {\n" <> renderCppStmt thenStmt <> "\n}"
    <> maybe "" (\s -> " else {\n" <> renderCppStmt s <> "\n}") elseStmt
  CppWhile cond body -> "while (" <> renderCppExpr cond <> ") {\n" <> renderCppStmt body <> "\n}"
  CppFor init cond step body ->
    "for (" <> maybe "" renderCppExpr init <> "; " <> maybe "" renderCppExpr cond <> "; " <> maybe "" renderCppExpr step <> ") {\n" <> renderCppStmt body <> "\n}"
  CppForRange var range body ->
    "for (auto& " <> var <> " : " <> renderCppExpr range <> ") {\n" <>
    T.unlines (map renderCppStmt body) <> "}"
  CppBlock stmts -> "{\n" <> T.unlines (map renderCppStmt stmts) <> "}"
  CppVarDeclStmt var -> renderCppVar var
  CppBreak -> "break;"
  CppContinue -> "continue;"
  CppThrow expr -> "throw " <> renderCppExpr expr <> ";"
  CppTry body catches ->
    "try {\n" <> renderCppStmt body <> "\n}"
    <> T.concat [" catch (" <> renderCppType typ <> " " <> name <> ") {\n" <> renderCppStmt handler <> "\n}" | (typ, name, handler) <- catches]

renderCppExpr :: CppExpr -> Text
renderCppExpr expr = case expr of
  CppVarRef name -> name
  CppLiteral lit -> renderCppLiteral lit
  CppBinary op left right -> "(" <> renderCppExpr left <> " " <> op <> " " <> renderCppExpr right <> ")"
  CppUnary op operand -> op <> renderCppExpr operand
  CppCall func args -> renderCppExpr func <> "(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMember obj field -> renderCppExpr obj <> "." <> field
  CppPointerMember obj field -> renderCppExpr obj <> "->" <> field
  CppIndex arr idx -> renderCppExpr arr <> "[" <> renderCppExpr idx <> "]"
  CppCast typ expr -> "(" <> renderCppType typ <> ")" <> renderCppExpr expr
  CppSizeOf typ -> "sizeof(" <> renderCppType typ <> ")"
  CppNew typ mInit -> "new " <> renderCppType typ <> if null mInit then "" else "(" <> T.intercalate ", " (map renderCppExpr mInit) <> ")"
  CppDelete expr -> "delete " <> renderCppExpr expr
  CppTernary cond thenExpr elseExpr -> renderCppExpr cond <> " ? " <> renderCppExpr thenExpr <> " : " <> renderCppExpr elseExpr
  CppLambda params body _ -> "[" <> T.intercalate ", " (map cpName params) <> "](" <> T.intercalate ", " (map renderCppParam params) <> ") {\n" <> T.unlines (map renderCppStmt body) <> "}"

renderCppLiteral :: CppLiteral -> Text
renderCppLiteral lit = case lit of
  CppIntLit n -> T.pack (show n)
  CppFloatLit f -> T.pack (show f)
  CppStringLit s -> "\"" <> s <> "\""
  CppCharLit c -> "'" <> T.singleton c <> "'"
  CppBoolLit b -> if b then "true" else "false"
  CppNullLit -> "nullptr"

renderCppType :: CppType -> Text
renderCppType typ = case typ of
  CppVoid -> "void"
  CppBool -> "bool"
  CppInt bits -> "int" <> T.pack (show bits) <> "_t"
  CppUInt bits -> "uint" <> T.pack (show bits) <> "_t"
  CppFloat -> "float"
  CppDouble -> "double"
  CppChar -> "char"
  CppString -> "std::string"
  CppAuto -> "auto"
  CppPointer t -> renderCppType t <> "*"
  CppReference t -> renderCppType t <> "&"
  CppConst t -> "const " <> renderCppType t
  CppVolatile t -> "volatile " <> renderCppType t
  CppSizeT -> "size_t"
  CppFunctionType params ret -> renderCppType ret <> "(" <> T.intercalate ", " (map renderCppType params) <> ")"
  CppClassType name args -> name <> (if null args then "" else "<" <> T.intercalate ", " (map renderCppType args) <> ">")
  CppTemplateType name args -> name <> "<" <> T.intercalate ", " (map renderCppType args) <> ">"
  CppUniquePtr t -> "std::unique_ptr<" <> renderCppType t <> ">"
  CppSharedPtr t -> "std::shared_ptr<" <> renderCppType t <> ">"
  CppOptional t -> "std::optional<" <> renderCppType t <> ">"
  CppVariant types -> "std::variant<" <> T.intercalate ", " (map renderCppType types) <> ">"
  CppPair t1 t2 -> "std::pair<" <> renderCppType t1 <> ", " <> renderCppType t2 <> ">"
  CppTuple types -> "std::tuple<" <> T.intercalate ", " (map renderCppType types) <> ">"
  CppMap k v -> "std::map<" <> renderCppType k <> ", " <> renderCppType v <> ">"
  CppUnorderedMap k v -> "std::unordered_map<" <> renderCppType k <> ", " <> renderCppType v <> ">"
  CppTypeVar name -> name
  CppDecltype expr -> "decltype(" <> renderCppExpr expr <> ")"
