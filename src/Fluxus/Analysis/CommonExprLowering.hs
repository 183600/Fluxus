{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.Analysis.CommonExprLowering
  ( collectCommonExpressions
  , pythonExprToCommon
  , pythonExprToLocatedCommon
  , pythonArgumentToCommon
  , pythonLiteralToLiteral
  , goExprToCommon
  , goExprToLocatedCommon
  , goLiteralToLiteral
  , renderCommonExpr
  , formatSpan
  ) where

import Data.Either (partitionEithers)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Maybe (maybeToList, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.AST.Go
import Fluxus.AST.Python

-- | Collect analyzable expressions that can be fed into the shared analysis passes.
collectCommonExpressions :: Either PythonAST GoAST -> ([CommonExpr], [Text])
collectCommonExpressions = \case
  Left (PythonAST pyModule) ->
    let pythonExprs = collectPythonExpressions pyModule
        (issues, commons) = partitionEithers (map pythonExprToCommon pythonExprs)
    in (commons, issues)
  Right (GoAST goPackage) ->
    let goExprs = collectGoExpressions goPackage
        (issues, commons) = partitionEithers (map goExprToCommon goExprs)
    in (commons, issues)

collectPythonExpressions :: PythonModule -> [Located PythonExpr]
collectPythonExpressions pyModule =
  concatMap collectPythonStmt (pyModuleBody pyModule)

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
collectGoExpressions goPackage =
  concatMap collectGoFile (goPackageFiles goPackage)

collectGoFile :: GoFile -> [Located GoExpr]
collectGoFile goFile = concatMap collectGoDecl (goFileDecls goFile)

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

pythonExprToCommon :: Located PythonExpr -> Either Text CommonExpr
pythonExprToCommon located@(Located span expr) = case expr of
  PyLiteral lit -> CELiteral <$> pythonLiteralToLiteral span lit
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
  PyComparison _ _ -> Left $ "Unsupported chained comparison at " <> formatSpan span
  PyBoolOp op operands -> do
    locatedOperands <- traverse pythonExprToLocatedCommon operands
    case locatedOperands of
      [] -> Left $ "Empty boolean operation at " <> formatSpan span
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
        Just _ -> Left $ "Slice step is not supported in common expression lowering at " <> formatSpan span
        Nothing -> do
          start' <- traverse pythonExprToLocatedCommon start
          end' <- traverse pythonExprToLocatedCommon end
          pure $ CESlice value' start' end'
      SliceExtSlice _ -> Left $ "Extended slicing is not supported at " <> formatSpan span
  PyCall func args -> do
    func' <- pythonExprToLocatedCommon func
    args' <- traverse pythonArgumentToCommon args
    pure $ CECall func' args'
  PyAttribute obj attr -> do
    obj' <- pythonExprToLocatedCommon obj
    pure $ CEAttribute obj' attr
  PySlice _ _ _ -> Left $ "Standalone slice expressions are not supported at " <> formatSpan span
  PyList _ -> Left $ "List literals are not supported at " <> formatSpan span
  PyTuple _ -> Left $ "Tuple literals are not supported at " <> formatSpan span
  PySet _ -> Left $ "Set literals are not supported at " <> formatSpan span
  PyDict _ -> Left $ "Dictionary literals are not supported at " <> formatSpan span
  PyListComp _ _ -> Left $ "List comprehensions are not supported at " <> formatSpan span
  PySetComp _ _ -> Left $ "Set comprehensions are not supported at " <> formatSpan span
  PyDictComp _ _ _ -> Left $ "Dict comprehensions are not supported at " <> formatSpan span
  PyGenComp _ _ -> Left $ "Generator expressions are not supported at " <> formatSpan span
  PyLambda _ _ -> Left $ "Lambda expressions are not supported at " <> formatSpan span
  PyIfExp{} -> Left $ "Conditional expressions are not supported at " <> formatSpan span
  PyStarred{} -> Left $ "Starred expressions are not supported at " <> formatSpan span
  PyNamedExpr{} -> Left $ "Walrus operator expressions are not supported at " <> formatSpan span
  PyAwait{} -> Left $ "Await expressions are not supported at " <> formatSpan span
  PyAsyncCall{} -> Left $ "Async call expressions are not supported at " <> formatSpan span
  PyJoinedStr{} -> Left $ "Formatted string expressions are not supported at " <> formatSpan span
  PyFormatSpec{} -> Left $ "Format specifier expressions are not supported at " <> formatSpan span

pythonExprToLocatedCommon :: Located PythonExpr -> Either Text (Located CommonExpr)
pythonExprToLocatedCommon located@(Located span _) = do
  converted <- pythonExprToCommon located
  pure $ Located span converted

pythonLiteralToLiteral :: SourceSpan -> PythonLiteral -> Either Text Literal
pythonLiteralToLiteral span = \case
  PyInt n -> Right $ LInt (fromIntegral n :: Int64)
  PyFloat f -> Right $ LFloat f
  PyString s -> Right $ LString s
  PyFString _ _ -> Left $ "F-string literals are not supported at " <> formatSpan span
  PyBytes b -> Right $ LBytes b
  PyBool b -> Right $ LBool b
  PyNone -> Right LNone
  PyEllipsis -> Left $ "Ellipsis literal is not supported at " <> formatSpan span
  PyComplex _ _ -> Left $ "Complex literals are not supported at " <> formatSpan span

pythonArgumentToCommon :: Located PythonArgument -> Either Text (Located CommonExpr)
pythonArgumentToCommon argLocated@(Located span arg) = case arg of
  ArgPositional expr -> pythonExprToLocatedCommon expr
  ArgKeyword _ expr -> pythonExprToLocatedCommon expr
  ArgStarred _ -> Left $ "Starred positional arguments are not supported at " <> formatSpan span
  ArgKwStarred _ -> Left $ "Starred keyword arguments are not supported at " <> formatSpan span

goExprToCommon :: Located GoExpr -> Either Text CommonExpr
goExprToCommon located@(Located span expr) = case expr of
  GoLiteral lit -> CELiteral <$> goLiteralToLiteral span lit
  GoIdent ident -> Right $ CEVar ident
  GoQualifiedIdent pkg ident ->
    let pkgVar = Located span (CEVar pkg)
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
      Just _ -> Left $ "Three-index slices are not supported at " <> formatSpan span
      Nothing -> do
        low' <- traverse goExprToLocatedCommon (goSliceLow sliceExpr)
        high' <- traverse goExprToLocatedCommon (goSliceHigh sliceExpr)
        pure $ CESlice container' low' high'
  GoSelector obj ident -> do
    obj' <- goExprToLocatedCommon obj
    pure $ CEAttribute obj' ident
  GoTypeConversion _ _ -> Left $ "Type conversions are not supported at " <> formatSpan span
  GoCompositeLit{} -> Left $ "Composite literals are not supported at " <> formatSpan span
  GoArrayLit{} -> Left $ "Array literals are not supported at " <> formatSpan span
  GoSliceLit{} -> Left $ "Slice literals are not supported at " <> formatSpan span
  GoMapLit{} -> Left $ "Map literals are not supported at " <> formatSpan span
  GoStructLit{} -> Left $ "Struct literals are not supported at " <> formatSpan span
  GoAddress{} -> Left $ "Address-of expressions are not supported at " <> formatSpan span
  GoDeref{} -> Left $ "Pointer dereference expressions are not supported at " <> formatSpan span
  GoReceive{} -> Left $ "Channel receive expressions are not supported at " <> formatSpan span
  GoTypeAssert{} -> Left $ "Type assertions are not supported at " <> formatSpan span
  GoFuncLit{} -> Left $ "Function literals are not supported at " <> formatSpan span

goExprToLocatedCommon :: Located GoExpr -> Either Text (Located CommonExpr)
goExprToLocatedCommon located@(Located span _) = do
  converted <- goExprToCommon located
  pure $ Located span converted

goLiteralToLiteral :: SourceSpan -> GoLiteral -> Either Text Literal
goLiteralToLiteral span = \case
  GoInt n -> Right $ LInt (fromIntegral n :: Int64)
  GoFloat f -> Right $ LFloat f
  GoImag _ -> Left $ "Imaginary literals are not supported at " <> formatSpan span
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
