{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.CodeGen.CPP.IdentifierSanitizer
  ( sanitizeCppUnit
  , sanitizeIdentifier
  ) where

import Data.Char (isDigit, isSpace, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashSet as HS

import Fluxus.CodeGen.CPP.AST

-- | Sanitize all identifiers contained in a C++ unit, ensuring that user
-- supplied names do not collide with reserved C++ keywords or implementation
-- defined identifier spaces (double underscores, leading underscore + uppercase
-- letter, etc.).
sanitizeCppUnit :: CppUnit -> CppUnit
sanitizeCppUnit unit =
  unit
    { cppNamespaces = map sanitizeIdentifier (cppNamespaces unit)
    , cppDeclarations = map sanitizeDecl (cppDeclarations unit)
    }

sanitizeDecl :: CppDecl -> CppDecl
sanitizeDecl decl = case decl of
  CppClass name bases members ->
    CppClass (sanitizeIdentifier name) (map sanitizeIdentifier bases) (map sanitizeDecl members)
  CppStruct name members ->
    CppStruct (sanitizeIdentifier name) (map sanitizeDecl members)
  CppFunction name ret params body ->
    CppFunction (sanitizeIdentifier name) (sanitizeType ret) (map sanitizeParam params) (map sanitizeStmt body)
  CppMethod name ret params body isVirtual ->
    CppMethod (sanitizeIdentifier name) (sanitizeType ret) (map sanitizeParam params) (map sanitizeStmt body) isVirtual
  CppConstructor name params body ->
    CppConstructor (sanitizeIdentifier name) (map sanitizeParam params) (map sanitizeStmt body)
  CppDestructor name body isVirtual ->
    CppDestructor (sanitizeIdentifier name) (map sanitizeStmt body) isVirtual
  CppVariable name ty mInit ->
    CppVariable (sanitizeIdentifier name) (sanitizeType ty) (sanitizeExpr <$> mInit)
  CppTypedef alias ty ->
    CppTypedef (sanitizeIdentifier alias) (sanitizeType ty)
  CppUsing alias ty ->
    CppUsing (sanitizeIdentifier alias) (sanitizeType ty)
  CppTemplate params inner ->
    CppTemplate (map sanitizeTemplateParam params) (sanitizeDecl inner)
  CppNamespace name members ->
    CppNamespace (sanitizeIdentifier name) (map sanitizeDecl members)
  CppExternC members ->
    CppExternC (map sanitizeDecl members)
  CppAccessSpec spec -> CppAccessSpec spec
  CppCommentDecl msg -> CppCommentDecl msg

sanitizeTemplateParam :: Text -> Text
sanitizeTemplateParam param
  | T.any isSpace param || "::" `T.isInfixOf` param = param
  | otherwise = sanitizeIdentifier param

sanitizeStmt :: CppStmt -> CppStmt
sanitizeStmt stmt = case stmt of
  CppExprStmt expr -> CppExprStmt (sanitizeExpr expr)
  CppReturn mexpr -> CppReturn (sanitizeExpr <$> mexpr)
  CppIf cond thenStmts elseStmts ->
    CppIf (sanitizeExpr cond) (map sanitizeStmt thenStmts) (map sanitizeStmt elseStmts)
  CppWhile cond body ->
    CppWhile (sanitizeExpr cond) (map sanitizeStmt body)
  CppFor mInit mCond mPost body ->
    CppFor (sanitizeStmt <$> mInit) (sanitizeExpr <$> mCond) (sanitizeExpr <$> mPost) (map sanitizeStmt body)
  CppForRange var expr body ->
    CppForRange (sanitizeIdentifier var) (sanitizeExpr expr) (map sanitizeStmt body)
  CppSwitch scrut cases ->
    CppSwitch (sanitizeExpr scrut) (map sanitizeCase cases)
  CppTry tryStmts catches finallyStmts ->
    CppTry (map sanitizeStmt tryStmts) (map sanitizeCatch catches) (map sanitizeStmt finallyStmts)
  CppThrow mexpr -> CppThrow (sanitizeExpr <$> mexpr)
  CppBreak -> CppBreak
  CppContinue -> CppContinue
  CppStmtSeq stmts -> CppStmtSeq (map sanitizeStmt stmts)
  CppBlock stmts -> CppBlock (map sanitizeStmt stmts)
  CppDecl declNode -> CppDecl (sanitizeDecl declNode)
  CppComment msg -> CppComment msg

sanitizeCase :: CppCase -> CppCase
sanitizeCase caseNode = case caseNode of
  CppCase expr stmts -> CppCase (sanitizeExpr expr) (map sanitizeStmt stmts)
  CppDefault stmts -> CppDefault (map sanitizeStmt stmts)

sanitizeCatch :: CppCatch -> CppCatch
sanitizeCatch (CppCatch ty name body) =
  CppCatch (sanitizeType ty) (sanitizeIdentifier name) (map sanitizeStmt body)

sanitizeExpr :: CppExpr -> CppExpr
sanitizeExpr expr = case expr of
  CppVar name -> CppVar (sanitizeIdentifier name)
  CppLiteral lit -> CppLiteral lit
  CppBinary op lhs rhs -> CppBinary op (sanitizeExpr lhs) (sanitizeExpr rhs)
  CppUnary op inner -> CppUnary op (sanitizeExpr inner)
  CppCall fun args -> CppCall (sanitizeExpr fun) (map sanitizeExpr args)
  CppMember obj member -> CppMember (sanitizeExpr obj) (sanitizeIdentifier member)
  CppPointerMember obj member -> CppPointerMember (sanitizeExpr obj) (sanitizeIdentifier member)
  CppIndex arr idx -> CppIndex (sanitizeExpr arr) (sanitizeExpr idx)
  CppCast ty inner -> CppCast (sanitizeType ty) (sanitizeExpr inner)
  CppSizeOf ty -> CppSizeOf (sanitizeType ty)
  CppNew ty args -> CppNew (sanitizeType ty) (map sanitizeExpr args)
  CppDelete inner -> CppDelete (sanitizeExpr inner)
  CppThis -> CppThis
  CppLambda params body -> CppLambda (map sanitizeParam params) (map sanitizeStmt body)
  CppMove inner -> CppMove (sanitizeExpr inner)
  CppForward inner -> CppForward (sanitizeExpr inner)
  CppMakeUnique ty args -> CppMakeUnique (sanitizeType ty) (map sanitizeExpr args)
  CppMakeShared ty args -> CppMakeShared (sanitizeType ty) (map sanitizeExpr args)
  CppBracedInit ty exprs -> CppBracedInit (sanitizeType ty) (map sanitizeExpr exprs)

sanitizeParam :: CppParam -> CppParam
sanitizeParam (CppParam name ty mDefault) =
  CppParam (sanitizeIdentifier name) (sanitizeType ty) (sanitizeExpr <$> mDefault)

sanitizeType :: CppType -> CppType
sanitizeType ty = case ty of
  CppConst inner -> CppConst (sanitizeType inner)
  CppVolatile inner -> CppVolatile (sanitizeType inner)
  CppPointer inner -> CppPointer (sanitizeType inner)
  CppReference inner -> CppReference (sanitizeType inner)
  CppRvalueRef inner -> CppRvalueRef (sanitizeType inner)
  CppVector inner -> CppVector (sanitizeType inner)
  CppStdArray inner n -> CppStdArray (sanitizeType inner) n
  CppArray inner n -> CppArray (sanitizeType inner) n
  CppOptional inner -> CppOptional (sanitizeType inner)
  CppVariant inners -> CppVariant (map sanitizeType inners)
  CppPair lhs rhs -> CppPair (sanitizeType lhs) (sanitizeType rhs)
  CppTuple inners -> CppTuple (map sanitizeType inners)
  CppMap k v -> CppMap (sanitizeType k) (sanitizeType v)
  CppUnorderedMap k v -> CppUnorderedMap (sanitizeType k) (sanitizeType v)
  CppFunctionType args ret -> CppFunctionType (map sanitizeType args) (sanitizeType ret)
  CppClassType name args -> CppClassType (sanitizeIdentifier name) (map sanitizeType args)
  CppStructLiteral fields -> CppStructLiteral (map sanitizeStructField fields)
  CppTemplateType name args -> CppTemplateType (sanitizeIdentifier name) (map sanitizeType args)
  CppUniquePtr inner -> CppUniquePtr (sanitizeType inner)
  CppSharedPtr inner -> CppSharedPtr (sanitizeType inner)
  other -> other

sanitizeStructField :: (Text, CppType) -> (Text, CppType)
sanitizeStructField (fieldName, fieldType)
  | T.null (T.strip fieldName) = (fieldName, sanitizeType fieldType)
  | otherwise = (sanitizeIdentifier fieldName, sanitizeType fieldType)

-- | Reserved C++ keywords and contextual keywords that we avoid generating.
reservedKeywords :: HS.HashSet Text
reservedKeywords = HS.fromList
  [ "alignas", "alignof", "and", "and_eq", "asm", "auto"
  , "bitand", "bitor", "bool", "break", "case", "catch"
  , "char", "char8_t", "char16_t", "char32_t", "class", "compl"
  , "concept", "const", "consteval", "constexpr", "constinit"
  , "const_cast", "continue", "co_await", "co_return", "co_yield"
  , "decltype", "default", "delete", "do", "double", "dynamic_cast"
  , "else", "enum", "explicit", "export", "extern", "false"
  , "float", "for", "friend", "goto", "if", "inline", "int"
  , "long", "mutable", "namespace", "new", "noexcept", "not"
  , "not_eq", "nullptr", "operator", "or", "or_eq", "private"
  , "protected", "public", "register", "reinterpret_cast", "requires"
  , "return", "short", "signed", "sizeof", "static", "static_assert"
  , "static_cast", "struct", "switch", "template", "this"
  , "thread_local", "throw", "true", "try", "typedef", "typeid"
  , "typename", "union", "unsigned", "using", "virtual", "void"
  , "volatile", "wchar_t", "while", "xor", "xor_eq", "final"
  , "override", "import", "module"
  ]

startsWithDigit :: Text -> Bool
startsWithDigit txt = case T.uncons txt of
  Just (c, _) -> isDigit c
  Nothing -> False

startsWithForbiddenUnderscore :: Text -> Bool
startsWithForbiddenUnderscore txt = case T.uncons txt of
  Just ('_', rest) -> case T.uncons rest of
    Just (next, _) -> next == '_' || isUpper next
    Nothing -> False
  _ -> False

-- | Sanitize a single identifier.
sanitizeIdentifier :: Text -> Text
sanitizeIdentifier name
  | T.null name = fallbackName
  | "::" `T.isInfixOf` name = name
  | T.any isSpace name = name
  | otherwise = finalize adjusted
  where
    adjusted = applyDigitRule $ applyUnderscoreRule name
    finalize candidate
      | HS.member candidate reservedKeywords = candidate <> "_fluxus"
      | otherwise = candidate
    fallbackName = "fluxus_symbol"

    applyDigitRule candidate
      | startsWithDigit candidate = "fluxus_" <> candidate
      | otherwise = candidate

    applyUnderscoreRule candidate
      | startsWithForbiddenUnderscore candidate = "fluxus" <> candidate
      | otherwise = candidate
