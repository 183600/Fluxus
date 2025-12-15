{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Analysis.CommonExprLowering
  ( collectCommonExpressions
  , LoweringIssue(..)
  , renderLoweringIssue
  , isUnsupportedIssue
  , pythonExprToCommon
  , pythonExprToLocatedCommon
  , pythonArgumentToCommon
  , pythonLiteralToLiteral
  , goExprToCommon
  , goExprToLocatedCommon
  , goLiteralToLiteral
  , renderCommonExpr
  , renderLocatedCommonExpr
  , fingerprintCommonExpr
  , fingerprintSpan
  , formatSpan
  ) where

import Data.Either (partitionEithers)
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.AST.Go
import Fluxus.AST.Python

-- | Structured issue information produced during lowering.
data LoweringIssue
  = LoweringUnsupported !Text
  | LoweringFailure !Text
  deriving (Eq, Show)

renderLoweringIssue :: LoweringIssue -> Text
renderLoweringIssue = \case
  LoweringUnsupported msg -> "[unsupported] " <> msg
  LoweringFailure msg -> "[failure] " <> msg

isUnsupportedIssue :: LoweringIssue -> Bool
isUnsupportedIssue = \case
  LoweringUnsupported _ -> True
  LoweringFailure _ -> False

unsupportedAt :: SourceSpan -> Text -> LoweringIssue
unsupportedAt sp msg = LoweringUnsupported (msg <> " at " <> formatSpan sp)

failureAt :: SourceSpan -> Text -> LoweringIssue
failureAt sp msg = LoweringFailure (msg <> " at " <> formatSpan sp)

-- | Collect analyzable expressions that can be fed into the shared analysis passes.
collectCommonExpressions :: Either PythonAST GoAST -> ([Located CommonExpr], [LoweringIssue])
collectCommonExpressions = \case
  Left (PythonAST pyMod) ->
    let pythonExprs = collectPythonExpressions pyMod
        (issues, commons) = partitionEithers (map pythonExprToLocatedCommon pythonExprs)
    in (commons, issues)
  Right (GoAST goPkg) ->
    let goExprs = collectGoExpressions goPkg
        (issues, commons) = partitionEithers (map goExprToLocatedCommon goExprs)
    in (commons, issues)

collectPythonExpressions :: PythonModule -> [Located PythonExpr]
collectPythonExpressions pyMod =
  concatMap collectPythonStmt (pyModuleBody pyMod)

collectPythonStmt :: Located PythonStmt -> [Located PythonExpr]
collectPythonStmt (Located _ stmt) = case stmt of
  PyExprStmt expr -> [expr]
  PyAssign _ value -> [value]
  PyAugAssign _ _ value -> [value]
  PyAnnAssign _ _ maybeValue -> maybeToList maybeValue
  PyReturn maybeExpr -> maybeToList maybeExpr
  PyYield maybeExpr -> maybeToList maybeExpr
  PyYieldFrom expr -> [expr]
  PyDel exprs -> exprs
  PyAssert expr maybeMsg -> expr : maybeToList maybeMsg
  PyIf cond body orelse -> cond : collectNested body ++ collectNested orelse
  PyWhile cond body orelse -> cond : collectNested body ++ collectNested orelse
  PyFor _ iter body orelse -> iter : collectNested body ++ collectNested orelse
  PyAsyncFor _ iter body orelse -> iter : collectNested body ++ collectNested orelse
  PyWith items body -> concatMap collectPythonWithItem items ++ collectNested body
  PyAsyncWith items body -> concatMap collectPythonWithItem items ++ collectNested body
  PyTry body excepts orelse finally ->
    collectNested body ++ concatMap collectPythonExcept excepts ++ collectNested orelse ++ collectNested finally
  PyMatch subject cases -> subject : concatMap collectPythonCase cases
  PyRaise maybeExc maybeFrom -> catMaybes [maybeExc, maybeFrom]
  PyFuncDef func -> collectPythonFunc func
  PyAsyncFuncDef func -> collectPythonFunc func
  PyClassDef cls -> collectPythonClass cls
  PyImport _ -> []
  PyGlobal _ -> []
  PyNonlocal _ -> []
  PyPass -> []
  PyBreak -> []
  PyContinue -> []
  where
    collectNested = concatMap collectPythonStmt

collectPythonWithItem :: Located PythonWithItem -> [Located PythonExpr]
collectPythonWithItem (Located _ item) = [pyWithContext item]

collectPythonExcept :: Located PythonExcept -> [Located PythonExpr]
collectPythonExcept (Located _ except) =
  maybeToList (pyExceptType except) ++ concatMap collectPythonStmt (pyExceptBody except)

collectPythonCase :: Located PythonCase -> [Located PythonExpr]
collectPythonCase (Located _ clause) =
  collectPatternExprs (pyCasePattern clause)
    ++ maybeToList (pyCaseGuard clause)
    ++ concatMap collectPythonStmt (pyCaseBody clause)

collectPatternExprs :: Located PythonPattern -> [Located PythonExpr]
collectPatternExprs (Located _ pat) = case pat of
  PatTuple pats -> concatMap collectPatternExprs pats
  PatList pats -> concatMap collectPatternExprs pats
  PatStarred inner -> collectPatternExprs inner
  PatOr alts -> concatMap collectPatternExprs (NE.toList alts)
  PatAs inner _ -> collectPatternExprs inner
  PatValue expr -> [expr]
  PatMapping entries _ -> map fst entries ++ concatMap (collectPatternExprs . snd) entries
  PatClass classExpr posArgs kwArgs ->
    classExpr : concatMap collectPatternExprs posArgs ++ concatMap (collectPatternExprs . snd) kwArgs
  _ -> []

collectPythonFunc :: PythonFuncDef -> [Located PythonExpr]
collectPythonFunc func =
  concat
    [ concatMap collectPythonDecorator (pyFuncDecorators func)
    , concatMap collectPythonParam (pyFuncParams func)
    , concatMap collectPythonStmt (pyFuncBody func)
    ]

collectPythonDecorator :: Located PythonDecorator -> [Located PythonExpr]
collectPythonDecorator (Located _ deco) =
  pyDecoratorName deco : concatMap collectPythonArgument (pyDecoratorArgs deco)

collectPythonArgument :: Located PythonArgument -> [Located PythonExpr]
collectPythonArgument (Located _ arg) = case arg of
  ArgPositional expr -> [expr]
  ArgKeyword _ expr -> [expr]
  ArgStarred expr -> [expr]
  ArgKwStarred expr -> [expr]

collectPythonParam :: Located PythonParameter -> [Located PythonExpr]
collectPythonParam (Located _ param) = case param of
  ParamNormal _ _ maybeDefault -> maybeToList maybeDefault
  ParamVarArgs _ _ -> []
  ParamKwArgs _ _ -> []
  ParamKwOnly _ _ maybeDefault -> maybeToList maybeDefault

collectPythonClass :: PythonClassDef -> [Located PythonExpr]
collectPythonClass cls =
  concat
    [ concatMap collectPythonDecorator (pyClassDecorators cls)
    , pyClassBases cls
    , map snd (pyClassKeywords cls)
    , concatMap collectPythonStmt (pyClassBody cls)
    ]

collectGoExpressions :: GoPackage -> [Located GoExpr]
collectGoExpressions goPkg =
  concatMap (concatMap collectGoDecl . goFileDecls) (goPackageFiles goPkg)

collectGoDecl :: Located GoDecl -> [Located GoExpr]
collectGoDecl (Located _ decl) = case decl of
  GoImportDecl _ -> []
  GoConstDecl entries -> [expr | (_, _, expr) <- entries]
  GoTypeDecl _ _ -> []
  GoVarDecl entries -> catMaybes [expr | (_, _, expr) <- entries]
  GoFuncDecl func -> collectGoFunction func
  GoMethodDecl _ func -> collectGoFunction func

collectGoFunction :: GoFunction -> [Located GoExpr]
collectGoFunction func = collectGoStmtMaybe (goFuncBody func)

collectGoStmtMaybe :: Maybe (Located GoStmt) -> [Located GoExpr]
collectGoStmtMaybe = maybe [] collectGoStmt

collectGoStmt :: Located GoStmt -> [Located GoExpr]
collectGoStmt (Located _ stmt) = case stmt of
  GoExprStmt expr -> [expr]
  GoAssign lhs rhs -> lhs ++ rhs
  GoDefine _ rhs -> rhs
  GoIncDec expr _ -> [expr]
  GoSend chanExpr valueExpr -> [chanExpr, valueExpr]
  GoReturn exprs -> exprs
  GoBreak _ -> []
  GoContinue _ -> []
  GoGoto _ -> []
  GoFallthrough -> []
  GoEmpty -> []
  GoBlock stmts -> concatMap collectGoStmt stmts
  GoIf initStmt cond thenStmt elseStmt ->
    collectGoStmtMaybe initStmt ++ [cond] ++ collectGoStmt thenStmt ++ collectGoStmtMaybe elseStmt
  GoSwitch initStmt maybeExpr cases ->
    collectGoStmtMaybe initStmt ++ maybeToList maybeExpr ++ concatMap collectGoStmt cases
  GoTypeSwitch initStmt clause cases ->
    collectGoStmtMaybe initStmt ++ collectGoTypeSwitchClause clause ++ concatMap collectGoStmt cases
  GoFor clause body -> collectGoForClause clause ++ collectGoStmt body
  GoRange clause body -> collectGoRangeClause clause ++ collectGoStmt body
  GoSelect clauses -> concatMap collectGoCommClause clauses
  GoDefer expr -> [expr]
  GoGo expr -> [expr]
  GoCase exprs stmts -> exprs ++ concatMap collectGoStmt stmts
  GoDefault stmts -> concatMap collectGoStmt stmts
  GoCommCase maybeStmt stmts -> collectGoStmtMaybe maybeStmt ++ concatMap collectGoStmt stmts
  GoCommDefault stmts -> concatMap collectGoStmt stmts
  GoLabeled _ inner -> collectGoStmt inner

collectGoTypeSwitchClause :: GoTypeSwitchClause -> [Located GoExpr]
collectGoTypeSwitchClause clause = [goTypeSwitchExpr clause]

collectGoForClause :: Maybe GoForClause -> [Located GoExpr]
collectGoForClause Nothing = []
collectGoForClause (Just clause) =
  collectGoStmtMaybe (goForInit clause) ++ maybeToList (goForCond clause) ++ collectGoStmtMaybe (goForPost clause)

collectGoRangeClause :: GoRangeClause -> [Located GoExpr]
collectGoRangeClause clause = [goRangeExpr clause]

collectGoCommClause :: Located GoCommClause -> [Located GoExpr]
collectGoCommClause (Located _ clause) =
  collectGoStmtMaybe (goCommStmt clause) ++ concatMap collectGoStmt (goCommBody clause)

pythonExprToCommon :: Located PythonExpr -> Either LoweringIssue CommonExpr
pythonExprToCommon located = case locValue located of
  PyLiteral lit -> CELiteral <$> pythonLiteralToLiteral (locSpan located) lit
  PyVar ident -> Right $ CEVar ident
  PyConst qn -> Right $ CEVar (qnName qn)
  PyBinaryOp op left right -> do
    left' <- pythonExprToLocatedCommon left
    right' <- pythonExprToLocatedCommon right
    pure $ CEBinaryOp op left' right'
  PyUnaryOp op operand -> do
    operand' <- pythonExprToLocatedCommon operand
    pure $ CEUnaryOp op operand'
  PyComparison [op] (left:right:[]) -> do
    left' <- pythonExprToLocatedCommon left
    right' <- pythonExprToLocatedCommon right
    pure $ CEComparison op left' right'
  PyComparison _ _ -> Left $ unsupportedAt (locSpan located) "Chained comparisons are not supported in common expression lowering"
  PyBoolOp op operands -> do
    locatedOperands <- traverse pythonExprToLocatedCommon operands
    case locatedOperands of
      [] -> Left $ failureAt (locSpan located) "Empty boolean operation in common expression lowering"
      (firstOperand:restOperands) ->
        let combined = foldl'
              (\acc next -> Located (mergeSpans (locSpan acc) (locSpan next)) (CEBinaryOp op acc next))
              firstOperand
              restOperands
        in pure $ locValue combined
  PySubscript value sliceNode -> do
    value' <- pythonExprToLocatedCommon value
    case locValue sliceNode of
      SliceIndex idx -> do
        idx' <- pythonExprToLocatedCommon idx
        pure $ CEIndex value' idx'
      SliceSlice start end step -> case step of
        Just _ -> Left $ unsupportedAt (locSpan located) "Slice step is not supported in common expression lowering"
        Nothing -> do
          start' <- traverse pythonExprToLocatedCommon start
          end' <- traverse pythonExprToLocatedCommon end
          pure $ CESlice value' start' end'
      SliceExtSlice _ -> Left $ unsupportedAt (locSpan located) "Extended slicing is not supported in common expression lowering"
  PyCall func args -> do
    func' <- pythonExprToLocatedCommon func
    args' <- traverse pythonArgumentToCommon args
    pure $ CECall func' args'
  PyAttribute obj attr -> do
    obj' <- pythonExprToLocatedCommon obj
    pure $ CEAttribute obj' attr
  PySlice _ _ _ -> Left $ unsupportedAt (locSpan located) "Standalone slice expressions are not supported in common expression lowering"
  PyList elems -> CEList <$> traverse pythonExprToLocatedCommon elems
  PyTuple elems -> CETuple <$> traverse pythonExprToLocatedCommon elems
  PySet elems -> CESet <$> traverse pythonExprToLocatedCommon elems
  PyDict pairs -> do
    converted <- traverse
      (\(k, v) -> do
         k' <- pythonExprToLocatedCommon k
         v' <- pythonExprToLocatedCommon v
         pure (k', v'))
      pairs
    pure $ CEDict converted
  PyListComp value comps -> do
    value' <- pythonExprToLocatedCommon value
    comps' <- traverse pythonComprehensionToCommon comps
    pure $ CEListComp value' comps'
  PySetComp value comps -> do
    value' <- pythonExprToLocatedCommon value
    comps' <- traverse pythonComprehensionToCommon comps
    pure $ CESetComp value' comps'
  PyDictComp key value comps -> do
    key' <- pythonExprToLocatedCommon key
    value' <- pythonExprToLocatedCommon value
    comps' <- traverse pythonComprehensionToCommon comps
    pure $ CEDictComp key' value' comps'
  PyAwait _ -> Left $ unsupportedAt (locSpan located) "Await expressions are not supported in common expression lowering"
  PyIfExp test body orelse -> do
    test' <- pythonExprToLocatedCommon test
    body' <- pythonExprToLocatedCommon body
    orelse' <- pythonExprToLocatedCommon orelse
    pure $ CEConditional test' body' orelse'
  PyNamedExpr _ _ -> Left $ unsupportedAt (locSpan located) "Named expressions are not supported in common expression lowering"
  PyLambda _ _ -> Left $ unsupportedAt (locSpan located) "Lambda expressions are not supported in common expression lowering"
  PyGenComp _ _ -> Left $ unsupportedAt (locSpan located) "Generator expressions are not supported in common expression lowering"
  PyStarred{} -> Left $ unsupportedAt (locSpan located) "Starred expressions are not supported in common expression lowering"
  PyAsyncCall{} -> Left $ unsupportedAt (locSpan located) "Async call expressions are not supported in common expression lowering"
  PyJoinedStr{} -> Left $ unsupportedAt (locSpan located) "Formatted string expressions are not supported in common expression lowering"
  PyFormatSpec{} -> Left $ unsupportedAt (locSpan located) "Format specifier expressions are not supported in common expression lowering"

pythonExprToLocatedCommon :: Located PythonExpr -> Either LoweringIssue (Located CommonExpr)
pythonExprToLocatedCommon located@(Located sp _) = do
  converted <- pythonExprToCommon located
  pure $ Located sp converted

pythonLiteralToLiteral :: SourceSpan -> PythonLiteral -> Either LoweringIssue Literal
pythonLiteralToLiteral sp = \case
  PyInt n -> Right $ LInt (fromIntegral n :: Int64)
  PyFloat f -> Right $ LFloat f
  PyString s -> Right $ LString s
  PyFString _ -> Left $ unsupportedAt sp "F-string literals are not supported in common expression lowering"
  PyBytes b -> Right $ LBytes b
  PyBool b -> Right $ LBool b
  PyNone -> Right LNone
  PyEllipsis -> Left $ unsupportedAt sp "Ellipsis literal is not supported in common expression lowering"
  PyComplex _ _ -> Left $ unsupportedAt sp "Complex literals are not supported in common expression lowering"

pythonArgumentToCommon :: Located PythonArgument -> Either LoweringIssue (Located CommonExpr)
pythonArgumentToCommon arg = case locValue arg of
  ArgPositional expr -> pythonExprToLocatedCommon expr
  ArgKeyword _ expr -> pythonExprToLocatedCommon expr
  ArgStarred _ -> Left $ unsupportedAt (locSpan arg) "Starred positional arguments are not supported in common expression lowering"
  ArgKwStarred _ -> Left $ unsupportedAt (locSpan arg) "Starred keyword arguments are not supported in common expression lowering"

pythonComprehensionToCommon :: PythonComprehension -> Either LoweringIssue CommonCompClause
pythonComprehensionToCommon comp = do
  bindings <- pythonPatternBindings (pyCompTarget comp)
  iter' <- pythonExprToLocatedCommon (pyCompIter comp)
  filters' <- traverse pythonExprToLocatedCommon (pyCompFilters comp)
  pure CommonCompClause
    { cccBindings = bindings
    , cccIter = iter'
    , cccFilters = filters'
    , cccIsAsync = pyCompAsync comp
    }

pythonPatternBindings :: Located PythonPattern -> Either LoweringIssue [Identifier]
pythonPatternBindings pat = case locValue pat of
  PatVar ident -> Right [ident]
  PatTuple pats -> fmap concat $ traverse pythonPatternBindings pats
  PatList pats -> fmap concat $ traverse pythonPatternBindings pats
  PatWildcard -> Right []
  PatLiteral _ -> Right []
  PatStarred{} -> Left $ unsupportedAt (locSpan pat) "Starred patterns in comprehensions are not supported in common expression lowering"
  PatOr alts -> fmap concat $ traverse pythonPatternBindings (NE.toList alts)
  PatAs inner _ -> pythonPatternBindings inner
  PatValue _ -> Left $ unsupportedAt (locSpan pat) "Value patterns in comprehensions are not supported in common expression lowering"
  PatMapping _ _ -> Left $ unsupportedAt (locSpan pat) "Mapping patterns in comprehensions are not supported in common expression lowering"
  PatClass _ _ _ -> Left $ unsupportedAt (locSpan pat) "Class patterns in comprehensions are not supported in common expression lowering"

goExprToCommon :: Located GoExpr -> Either LoweringIssue CommonExpr
goExprToCommon located = case locValue located of
  GoLiteral lit -> CELiteral <$> goLiteralToLiteral (locSpan located) lit
  GoIdent ident -> Right $ CEVar ident
  GoQualifiedIdent pkg ident ->
    let pkgVar = Located (locSpan located) (CEVar pkg)
    in Right $ CEAttribute pkgVar ident
  GoBinaryOp op left right -> do
    left' <- goExprToLocatedCommon left
    right' <- goExprToLocatedCommon right
    pure $ CEBinaryOp op left' right'
  GoUnaryOp op operand -> do
    operand' <- goExprToLocatedCommon operand
    pure $ CEUnaryOp op operand'
  GoComparison op left right -> do
    left' <- goExprToLocatedCommon left
    right' <- goExprToLocatedCommon right
    pure $ CEComparison op left' right'
  GoCall func args -> do
    func' <- goExprToLocatedCommon func
    args' <- traverse goExprToLocatedCommon args
    pure $ CECall func' args'
  GoIndex container indexExpr -> do
    container' <- goExprToLocatedCommon container
    index' <- goExprToLocatedCommon indexExpr
    pure $ CEIndex container' index'
  GoSlice container sliceExpr -> do
    container' <- goExprToLocatedCommon container
    case goSliceMax sliceExpr of
      Just _ -> Left $ unsupportedAt (locSpan located) "Three-index slices are not supported in common expression lowering"
      Nothing -> do
        low' <- traverse goExprToLocatedCommon (goSliceLow sliceExpr)
        high' <- traverse goExprToLocatedCommon (goSliceHigh sliceExpr)
        pure $ CESlice container' low' high'
  GoSelector obj ident -> do
    obj' <- goExprToLocatedCommon obj
    pure $ CEAttribute obj' ident
  GoTypeConversion _ _ -> Left $ unsupportedAt (locSpan located) "Type conversions are not supported in common expression lowering"
  GoCompositeLit _ elems -> CEList <$> traverse goExprToLocatedCommon elems
  GoArrayLit _ elems -> CEList <$> traverse goExprToLocatedCommon elems
  GoSliceLit _ elems -> CEList <$> traverse goExprToLocatedCommon elems
  GoMapLit _ pairs -> do
    converted <- traverse
      (\(k, v) -> do
         k' <- goExprToLocatedCommon k
         v' <- goExprToLocatedCommon v
         pure (k', v'))
      pairs
    pure $ CEDict converted
  GoStructLit _ fields -> do
    converted <- traverse
      (\(name, exprLoc) -> do
         value <- goExprToLocatedCommon exprLoc
         let keySpan = locSpan exprLoc
             keyExpr = Located keySpan (CELiteral (LString (identifierToText name)))
         pure (keyExpr, value))
      fields
    pure $ CEDict converted
  GoAddress{} -> Left $ unsupportedAt (locSpan located) "Address-of expressions are not supported in common expression lowering"
  GoDeref{} -> Left $ unsupportedAt (locSpan located) "Pointer dereference expressions are not supported in common expression lowering"
  GoReceive{} -> Left $ unsupportedAt (locSpan located) "Channel receive expressions are not supported in common expression lowering"
  GoTypeAssert{} -> Left $ unsupportedAt (locSpan located) "Type assertions are not supported in common expression lowering"
  GoFuncLit{} -> Left $ unsupportedAt (locSpan located) "Function literals are not supported in common expression lowering"

goExprToLocatedCommon :: Located GoExpr -> Either LoweringIssue (Located CommonExpr)
goExprToLocatedCommon located = do
  converted <- goExprToCommon located
  pure $ Located (locSpan located) converted

goLiteralToLiteral :: SourceSpan -> GoLiteral -> Either LoweringIssue Literal
goLiteralToLiteral sp = \case
  GoInt n -> Right $ LInt (fromIntegral n :: Int64)
  GoFloat f -> Right $ LFloat f
  GoImag _ -> Left $ unsupportedAt sp "Imaginary literals are not supported in common expression lowering"
  GoRune c -> Right $ LChar c
  GoString s -> Right $ LString s
  GoRawString s -> Right $ LString s
  GoBool b -> Right $ LBool b
  GoNil -> Right LNone

mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan file start _) (SourceSpan _ _ end) = SourceSpan file start end

formatSpan :: SourceSpan -> Text
formatSpan (SourceSpan file start _) =
  file <> ":" <> textShow (posLine start) <> ":" <> textShow (posColumn start)

textShow :: Show a => a -> Text
textShow = T.pack . show

renderCommonExpr :: CommonExpr -> Text
renderCommonExpr = textShow

renderLocatedCommonExpr :: Located CommonExpr -> Text
renderLocatedCommonExpr (Located sp expr) =
  renderCommonExpr expr <> "@" <> formatSpan sp

fingerprintCommonExpr :: Located CommonExpr -> Text
fingerprintCommonExpr (Located sp expr) =
  renderCommonExpr expr <> "#" <> fingerprintSpan sp

fingerprintSpan :: SourceSpan -> Text
fingerprintSpan sp =
  let SourceSpan file start end = sp
      startText = posText start
      endText = posText end
  in file <> ":" <> startText <> "-" <> endText
  where
    posText :: SourcePos -> Text
    posText (SourcePos line column) =
      textShow line <> ":" <> textShow column

identifierToText :: Identifier -> Text
identifierToText (Identifier name) = name
