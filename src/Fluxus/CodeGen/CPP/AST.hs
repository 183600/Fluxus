{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Core C++ AST and lightweight pretty printer utilities used by the
--   Fluxus code generator. The goal of this module is to provide a single
--   source of truth for the tree structure that the Python and Go backends
--   emit while offering a deterministic textual renderer that can be used
--   in tests and debug output.
module Fluxus.CodeGen.CPP.AST
  ( -- * AST nodes
    CppUnit(..)
  , CppDecl(..)
  , CppStmt(..)
  , CppExpr(..)
  , CppType(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppCase(..)
  , CppCatch(..)
    -- * Rendering helpers
  , renderCppUnit
  , renderCppDecl
  , renderCppStmt
  , renderCppExpr
  , renderCppType
  ) where

import Control.DeepSeq (NFData)
import Data.Char (isPrint)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- AST definitions ------------------------------------------------------------
-------------------------------------------------------------------------------

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
  | CppAccessSpec !Text
  | CppCommentDecl !Text
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
  | CppStmtSeq ![CppStmt]
  | CppBlock ![CppStmt]
  | CppDecl !CppDecl
  | CppComment !Text
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppExpr
  = CppVar !Text
  | CppLiteral !CppLiteral
  | CppBinary !Text !CppExpr !CppExpr
  | CppConditional !CppExpr !CppExpr !CppExpr
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
  | CppLambda ![CppParam] ![CppStmt]
  | CppMove !CppExpr
  | CppForward !CppExpr
  | CppMakeUnique !CppType ![CppExpr]
  | CppMakeShared !CppType ![CppExpr]
  | CppBracedInit !CppType ![CppExpr]
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
  | CppStdArray !CppType !Int
  | CppArray !CppType !Int
  | CppPointer !CppType
  | CppReference !CppType
  | CppRvalueRef !CppType
  | CppConst !CppType
  | CppVolatile !CppType
  | CppSizeT
  | CppFunctionType ![CppType] !CppType
  | CppClassType !Text ![CppType]
  | CppStructLiteral ![(Text, CppType)]
  | CppTemplateType !Text ![CppType]
  | CppUniquePtr !CppType
  | CppSharedPtr !CppType
  | CppOptional !CppType
  | CppVariant ![CppType]
  | CppPair !CppType !CppType
  | CppTuple ![CppType]
  | CppMap !CppType !CppType
  | CppUnorderedMap !CppType !CppType
  deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data CppLiteral
  = CppIntLit !Integer
  | CppFloatLit !Double
  | CppStringLit !Text
  | CppCharLit !Char
  | CppBoolLit !Bool
  | CppNullPtr
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

-------------------------------------------------------------------------------
-- Rendering ------------------------------------------------------------------
-------------------------------------------------------------------------------

renderCppUnit :: CppUnit -> Text
renderCppUnit CppUnit { cppIncludes, cppNamespaces, cppDeclarations } =
  let includeLines = map ("#include " <>) cppIncludes
      declText = renderDecls 0 cppDeclarations
      body = wrapNamespaces cppNamespaces declText
      preludeLines = "// Generated by Fluxus C++ Compiler" : includeLines
      sections = filter (not . T.null . T.stripEnd)
        [ T.intercalate "\n" preludeLines
        , body
        ]
  in T.intercalate "\n\n" sections

renderCppDecl :: Int -> CppDecl -> Text
renderCppDecl indentLevel decl = case decl of
  CppClass name bases members ->
    renderClassLike indentLevel "class" name bases members
  CppStruct name members ->
    renderClassLike indentLevel "struct" name [] members
  CppFunction name ret params body ->
    renderFunctionLike indentLevel False name ret params body
  CppMethod name ret params body isVirtual ->
    renderFunctionLike indentLevel isVirtual name ret params body
  CppConstructor name params body ->
    renderCtorOrDtor indentLevel False name params body
  CppDestructor name body isVirtual ->
    renderCtorOrDtor indentLevel isVirtual ("~" <> name) [] body
  CppVariable name ty initializer ->
    indentLine indentLevel $ T.intercalate " "
      (catMaybes [Just (renderCppType ty), Just name, fmap (("= " <>) . renderCppExpr) initializer]) <> ";"
  CppTypedef alias ty ->
    indentLine indentLevel $ "typedef " <> renderCppType ty <> " " <> alias <> ";"
  CppUsing alias ty ->
    indentLine indentLevel $ "using " <> alias <> " = " <> renderCppType ty <> ";"
  CppTemplate params inner ->
    let templateLine = indentLine indentLevel $ "template <" <> renderTemplateParams params <> ">"
        innerText = renderCppDecl indentLevel inner
    in templateLine <> "\n" <> innerText
  CppNamespace name decls ->
    renderNamespace indentLevel name decls
  CppExternC decls ->
    let header = indentLine indentLevel "extern \"C\" {"
        body = renderDecls (indentLevel + 2) decls
        footer = indentLine indentLevel "}"
    in T.intercalate "\n" $ filter (not . T.null)
         [header, body, footer]
  CppAccessSpec spec ->
    indentLine indentLevel (spec <> ":")
  CppCommentDecl msg ->
    indentLine indentLevel ("// " <> msg)

renderCppStmt :: Int -> CppStmt -> Text
renderCppStmt indentLevel stmt = case stmt of
  CppExprStmt expr -> indentLine indentLevel (renderCppExpr expr <> ";")
  CppReturn Nothing -> indentLine indentLevel "return;"
  CppReturn (Just expr) -> indentLine indentLevel ("return " <> renderCppExpr expr <> ";")
  CppIf cond thenStmts elseStmts ->
    let header = indentLine indentLevel ("if (" <> renderCppExpr cond <> ")")
        thenBlock = renderBlock indentLevel thenStmts
        elseBlock = if null elseStmts
                    then ""
                    else "\n" <> indentLine indentLevel "else" <> "\n" <> renderBlock indentLevel elseStmts
    in header <> "\n" <> thenBlock <> elseBlock
  CppWhile cond body ->
    indentLine indentLevel ("while (" <> renderCppExpr cond <> ")")
      <> "\n" <> renderBlock indentLevel body
  CppFor initStmt condExpr postExpr body ->
    let initPart = maybe "" (renderForInit . stripStatementWrapper) initStmt
        condPart = maybe "" renderCppExpr condExpr
        postPart = maybe "" renderCppExpr postExpr
        header = indentLine indentLevel $ "for (" <> initPart <> "; " <> condPart <> "; " <> postPart <> ")"
    in header <> "\n" <> renderBlock indentLevel body
  CppForRange var rangeExpr body ->
    indentLine indentLevel ("for (auto " <> var <> " : " <> renderCppExpr rangeExpr <> ")")
      <> "\n" <> renderBlock indentLevel body
  CppSwitch scrut cases ->
    let header = indentLine indentLevel ("switch (" <> renderCppExpr scrut <> ")")
        renderedCases = T.intercalate "\n" (map (renderCppCase (indentLevel + 2)) cases)
        body = indentLine indentLevel "{" <> "\n" <> renderedCases <> "\n" <> indentLine indentLevel "}"
    in header <> "\n" <> body
  CppTry tryStmts catches finallyStmts ->
    let tryPart = indentLine indentLevel "try" <> "\n" <> renderBlock indentLevel tryStmts
        catchParts = map (renderCppCatch indentLevel) catches
        baseText = T.intercalate "\n" (filter (not . T.null) (tryPart : catchParts))
    in if null finallyStmts
         then baseText
         else baseText
           <> "\n" <> indentLine indentLevel "/* finally */"
           <> "\n" <> renderBlock indentLevel finallyStmts
  CppThrow Nothing -> indentLine indentLevel "throw;"
  CppThrow (Just expr) -> indentLine indentLevel ("throw " <> renderCppExpr expr <> ";")
  CppBreak -> indentLine indentLevel "break;"
  CppContinue -> indentLine indentLevel "continue;"
  CppStmtSeq stmts ->
    T.intercalate "\n" (map (renderCppStmt indentLevel) stmts)
  CppBlock stmts -> renderBlock indentLevel stmts
  CppDecl decl -> renderCppDecl indentLevel decl
  CppComment msg -> indentLine indentLevel ("// " <> msg)

renderCppExpr :: CppExpr -> Text
renderCppExpr expr = case expr of
  CppVar name -> name
  CppLiteral lit -> renderCppLiteral lit
  CppBinary op lhs rhs -> "(" <> renderCppExpr lhs <> " " <> op <> " " <> renderCppExpr rhs <> ")"
  CppConditional cond thenExpr elseExpr ->
    "(" <> renderCppExpr cond <> " ? " <> renderCppExpr thenExpr <> " : " <> renderCppExpr elseExpr <> ")"
  CppUnary op inner -> op <> renderCppExpr inner
  CppCall fun args -> renderCppExpr fun <> "(" <> T.intercalate ", " (map renderCppExpr args) <> ")"
  CppMember obj member -> renderCppExpr obj <> "." <> member
  CppPointerMember obj member -> renderCppExpr obj <> "->" <> member
  CppIndex arr idx -> renderCppExpr arr <> "[" <> renderCppExpr idx <> "]"
  CppCast ty inner -> "(" <> renderCppType ty <> ")" <> renderCppExpr inner
  CppSizeOf ty -> "sizeof(" <> renderCppType ty <> ")"
  CppNew ty args -> "new " <> renderCppType ty <> tupled args
  CppDelete inner -> "delete " <> renderCppExpr inner
  CppThis -> "this"
  CppLambda params body ->
    let paramText = T.intercalate ", " (map renderCppParam params)
        bodyText = renderInlineBlock body
    in "[&](" <> paramText <> ") " <> bodyText
  CppMove inner -> "std::move(" <> renderCppExpr inner <> ")"
  CppForward inner -> "std::forward(" <> renderCppExpr inner <> ")"
  CppMakeUnique ty args -> "std::make_unique<" <> renderCppType ty <> ">(" <> argList args <> ")"
  CppMakeShared ty args -> "std::make_shared<" <> renderCppType ty <> ">(" <> argList args <> ")"
  CppBracedInit ty exprs -> renderCppType ty <> "{" <> argList exprs <> "}"
  where
    tupled xs = "(" <> argList xs <> ")"
    argList = T.intercalate ", " . map renderCppExpr

renderCppType :: CppType -> Text
renderCppType ty = case ty of
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
  CppVector inner -> "std::vector<" <> renderCppType inner <> ">"
  CppStdArray inner n -> "std::array<" <> renderCppType inner <> ", " <> T.pack (show n) <> ">"
  CppArray inner n -> renderCppType inner <> "[" <> T.pack (show n) <> "]"
  CppPointer inner -> renderCppType inner <> "*"
  CppReference inner -> renderCppType inner <> "&"
  CppRvalueRef inner -> renderCppType inner <> "&&"
  CppConst inner -> "const " <> renderCppType inner
  CppVolatile inner -> "volatile " <> renderCppType inner
  CppSizeT -> "std::size_t"
  CppFunctionType args ret ->
    let params = T.intercalate ", " (map renderCppType args)
    in renderCppType ret <> "(" <> params <> ")"
  CppClassType name args -> renderTemplate name args
  CppStructLiteral fields -> renderStructLiteral fields
  CppTemplateType name args -> renderTemplate name args
  CppUniquePtr inner -> "std::unique_ptr<" <> renderCppType inner <> ">"
  CppSharedPtr inner -> "std::shared_ptr<" <> renderCppType inner <> ">"
  CppOptional inner -> "std::optional<" <> renderCppType inner <> ">"
  CppVariant inners -> "std::variant<" <> T.intercalate ", " (map renderCppType inners) <> ">"
  CppPair lhs rhs -> "std::pair<" <> renderCppType lhs <> ", " <> renderCppType rhs <> ">"
  CppTuple inners -> "std::tuple<" <> T.intercalate ", " (map renderCppType inners) <> ">"
  CppMap k v -> "std::map<" <> renderCppType k <> ", " <> renderCppType v <> ">"
  CppUnorderedMap k v -> "std::unordered_map<" <> renderCppType k <> ", " <> renderCppType v <> ">"

-------------------------------------------------------------------------------
-- Internal helpers -----------------------------------------------------------
-------------------------------------------------------------------------------

renderDecls :: Int -> [CppDecl] -> Text
renderDecls indentLevel decls =
  T.intercalate "\n\n" (map (renderCppDecl indentLevel) decls)

renderClassLike :: Int -> Text -> Text -> [Text] -> [CppDecl] -> Text
renderClassLike indentLevel keyword name bases members =
  let baseSuffix = case bases of
        [] -> ""
        _  -> " : " <> T.intercalate ", " (map ("public " <>) bases)
      header = indentLine indentLevel (keyword <> " " <> name <> baseSuffix <> " {")
      body = if null members
             then indentLine (indentLevel + 2) "// empty"
             else renderDecls (indentLevel + 2) members
      footer = indentLine indentLevel "};"
  in T.intercalate "\n" $ filter (not . T.null)
       [header, body, footer]

renderFunctionLike :: Int -> Bool -> Text -> CppType -> [CppParam] -> [CppStmt] -> Text
renderFunctionLike indentLevel isVirtual name ret params body =
  let prefix = if isVirtual then "virtual " else ""
      signature = prefix <> renderCppType ret <> " " <> name
        <> "(" <> T.intercalate ", " (map renderCppParam params) <> ")"
      header = indentLine indentLevel signature
      bodyText = renderBlock indentLevel body
      suffix = if isVirtual then "\n" <> indentLine indentLevel "/* virtual */" else ""
  in header <> "\n" <> bodyText <> suffix

renderCtorOrDtor :: Int -> Bool -> Text -> [CppParam] -> [CppStmt] -> Text
renderCtorOrDtor indentLevel isVirtual name params body =
  let prefix = if isVirtual then "virtual " else ""
      signature = prefix <> name <> "(" <> T.intercalate ", " (map renderCppParam params) <> ")"
      header = indentLine indentLevel signature
  in header <> "\n" <> renderBlock indentLevel body

renderNamespace :: Int -> Text -> [CppDecl] -> Text
renderNamespace indentLevel name decls =
  let header = indentLine indentLevel ("namespace " <> name <> " {")
      body = renderDecls (indentLevel + 2) decls
      footer = indentLine indentLevel "}"
  in T.intercalate "\n" $ filter (not . T.null)
       [header, body, footer]

renderBlock :: Int -> [CppStmt] -> Text
renderBlock indentLevel stmts =
  case stmts of
    [] -> indentLine indentLevel "{}"
    _  ->
      let header = indentLine indentLevel "{"
          body = T.intercalate "\n" (map (renderCppStmt (indentLevel + 2)) stmts)
          footer = indentLine indentLevel "}"
      in T.intercalate "\n" $ filter (not . T.null) [header, body, footer]

renderInlineBlock :: [CppStmt] -> Text
renderInlineBlock stmts =
  case stmts of
    [] -> "{}"
    _  -> "{ " <> T.intercalate " " (map (T.strip . renderCppStmt 0) stmts) <> " }"

renderCppParam :: CppParam -> Text
renderCppParam (CppParam name ty mDefault) =
  let base = renderCppType ty <> " " <> name
  in case mDefault of
       Nothing -> base
       Just expr -> base <> " = " <> renderCppExpr expr

renderCppLiteral :: CppLiteral -> Text
renderCppLiteral lit = case lit of
  CppIntLit i -> T.pack (show i)
  CppFloatLit d -> T.pack (show d)
  CppStringLit t -> "\"" <> escapeString t <> "\""
  CppCharLit c -> "'" <> escapeChar c <> "'"
  CppBoolLit True -> "true"
  CppBoolLit False -> "false"
  CppNullPtr -> "nullptr"

renderCppCase :: Int -> CppCase -> Text
renderCppCase indentLevel caseBranch = case caseBranch of
  CppCase expr body ->
    let header = indentLine indentLevel ("case " <> renderCppExpr expr <> ":")
        bodyText = T.intercalate "\n" (map (renderCppStmt (indentLevel + 2)) body)
    in T.intercalate "\n" $ filter (not . T.null) [header, bodyText]
  CppDefault body ->
    let header = indentLine indentLevel "default:"
        bodyText = T.intercalate "\n" (map (renderCppStmt (indentLevel + 2)) body)
    in T.intercalate "\n" $ filter (not . T.null) [header, bodyText]

renderCppCatch :: Int -> CppCatch -> Text
renderCppCatch indentLevel (CppCatch ty name body) =
  let header = indentLine indentLevel ("catch (" <> renderCppType ty <> " " <> name <> ")")
  in header <> "\n" <> renderBlock indentLevel body

renderTemplateParams :: [Text] -> Text
renderTemplateParams params =
  T.intercalate ", " (map ("typename " <>) params)

renderTemplate :: Text -> [CppType] -> Text
renderTemplate name args =
  case args of
    [] -> name
    _  -> name <> "<" <> T.intercalate ", " (map renderCppType args) <> ">"

renderStructLiteral :: [(Text, CppType)] -> Text
renderStructLiteral fields =
  case fields of
    [] -> "struct { }"
    _  -> "struct { " <> T.intercalate " " (map renderField fields) <> " }"
  where
    renderField (name, ty)
      | T.null (T.strip name) = renderCppType ty <> ";"
      | otherwise = renderCppType ty <> " " <> name <> ";"

renderForInit :: CppStmt -> Text
renderForInit stmt =
  case stmt of
    CppExprStmt expr -> renderCppExpr expr
    CppDecl (CppVariable name ty mInit) ->
      renderCppType ty <> " " <> name <> maybe "" ((" = " <>) . renderCppExpr) mInit
    CppDecl (CppCommentDecl msg) -> "/* " <> msg <> " */"
    CppDecl other -> T.strip (renderCppDecl 0 other)
    CppComment msg -> "/* " <> msg <> " */"
    _ -> T.strip (renderCppStmt 0 stmt)

stripStatementWrapper :: CppStmt -> CppStmt
stripStatementWrapper (CppStmtSeq [single]) = stripStatementWrapper single
stripStatementWrapper other = other

wrapNamespaces :: [Text] -> Text -> Text
wrapNamespaces namespaces body = foldr apply body namespaces
  where
    apply ns inner
      | T.null (T.strip inner) = indentLine 0 ("namespace " <> ns <> " {}")
      | otherwise =
          let header = indentLine 0 ("namespace " <> ns <> " {")
              bodyText = indentBlock 2 inner
              footer = indentLine 0 "}"
          in T.intercalate "\n" $ filter (not . T.null)
               [header, bodyText, footer]

indentLine :: Int -> Text -> Text
indentLine indentLevel text =
  let spaces = T.replicate indentLevel " "
  in spaces <> text

indentBlock :: Int -> Text -> Text
indentBlock indentLevel blockText =
  let spaces = T.replicate indentLevel " "
      indentOne line
        | T.null line = line
        | otherwise = spaces <> line
  in T.intercalate "\n" (map indentOne (T.lines blockText))

escapeString :: Text -> Text
escapeString = T.concatMap escapeCharSafe

escapeChar :: Char -> Text
escapeChar c = escapeCharSafe c

escapeCharSafe :: Char -> Text
escapeCharSafe c
  | c == '"' = "\\\""
  | c == '\\' = "\\\\"
  | c == '\n' = "\\n"
  | c == '\t' = "\\t"
  | isPrint c = T.singleton c
  | otherwise = T.pack ("\\x" ++ showHex (fromEnum c))

showHex :: Int -> String
showHex n = let digits = "0123456789abcdef" :: String
                go 0 acc = if null acc then "0" else acc
                go x acc = let (q, r) = x `divMod` 16
                           in go q (digits !! r : acc)
            in go n ""
