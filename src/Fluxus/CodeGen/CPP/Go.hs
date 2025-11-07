{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fluxus.CodeGen.CPP.Go
  ( generateCppFromGo
  ) where

import Control.Monad (unless, when)
import Control.Monad.State (gets)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.AST.Go
import Fluxus.AST.Common (BinaryOp(..), ComparisonOp(..), Identifier(..), Located(..), SourcePos(..), SourceSpan(..), UnaryOp(..))
import Fluxus.Analysis.CommonExprLowering (goExprToCommon, renderCommonExpr, renderLoweringIssue)
import Fluxus.CodeGen.CPP.AST
  ( CppDecl(..)
  , CppExpr(..)
  , CppLiteral(..)
  , CppParam(..)
  , CppStmt(..)
  , CppType(..)
  , CppUnit(..)
  )
import Fluxus.CodeGen.CPP.Monad
  ( CppCodeGen
  , CppGenState(..)
  , addDeclaration
  , addInclude
  , cppNoop
  , emitInfo
  , generateTempVar
  , reportFatalNotImplemented
  , reportNotImplemented
  , reportUnsupported
  )
import Fluxus.CodeGen.CPP.Go.TypeMapping
  ( collectCppTypeIncludes
  , mapGoTypeToCpp
  )
import Fluxus.CodeGen.CPP.Shared
  ( lookupAndApplyAnnotations
  , spaceSeparate
  , streamChain
  )

-- | Entry point for Go to C++ translation.
generateCppFromGo :: GoAST -> CppCodeGen CppUnit
generateCppFromGo (GoAST goPackage) = do
  let packageName = (\(Identifier n) -> n) (goPackageName goPackage)
  emitInfo $ "Generating C++ for Go package: " <> packageName

  let files = goPackageFiles goPackage
  emitInfo $ "Found " <> T.pack (show (length files)) <> " files in package"
  when (null files) $
    reportUnsupported $ "No files found in Go package '" <> packageName <> "'"

  mapM_ generateGoFile files

  when (packageName == "main") $ do
    hasMain <- gets (any isMainFunction . cgsDeclarations)
    unless hasMain $
      reportUnsupported "Go package \"main\" does not define a main function"

  includes <- gets cgsIncludes
  namespaces <- gets cgsNamespaces
  decls <- gets cgsDeclarations
  pure $ CppUnit includes namespaces (reverse decls)
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False

formatSourceSpanShort :: SourceSpan -> Text
formatSourceSpanShort SourceSpan { spanFilename, spanStart = SourcePos line col } =
  spanFilename <> ":" <> T.pack (show line) <> ":" <> T.pack (show col)

reportDeclIssue :: (Text -> CppCodeGen ()) -> SourceSpan -> GoDecl -> Text -> Maybe Text -> CppCodeGen ()
reportDeclIssue reporter span decl detail extra =
  reporter $
    detail
      <> " ("
      <> describeGoDecl decl
      <> " at "
      <> formatSourceSpanShort span
      <> ")"
      <> maybe "" (": " <>) extra

reportStmtIssue :: (Text -> CppCodeGen ()) -> SourceSpan -> GoStmt -> Text -> Maybe Text -> CppCodeGen ()
reportStmtIssue reporter span stmt detail extra =
  reporter $
    detail
      <> " ("
      <> describeGoStmt stmt
      <> " at "
      <> formatSourceSpanShort span
      <> ")"
      <> maybe "" (": " <>) extra

reportExprIssue :: (Text -> CppCodeGen ()) -> SourceSpan -> GoExpr -> Text -> Maybe Text -> CppCodeGen ()
reportExprIssue reporter span expr detail extra =
  reporter $
    detail
      <> " ("
      <> describeGoExpr expr
      <> " at "
      <> formatSourceSpanShort span
      <> ")"
      <> maybe "" (": " <>) extra

describeGoDecl :: GoDecl -> Text
describeGoDecl = \case
  GoImportDecl _ -> "import declaration"
  GoConstDecl _ -> "const declaration"
  GoTypeDecl _ _ -> "type declaration"
  GoVarDecl _ -> "variable declaration"
  GoFuncDecl _ -> "function declaration"
  GoMethodDecl _ _ -> "method declaration"

describeGoStmt :: GoStmt -> Text
describeGoStmt stmt = case stmt of
  GoExprStmt _ -> "expression statement"
  GoAssign _ _ -> "assignment statement"
  GoDefine _ _ -> "short variable declaration"
  GoIncDec _ True -> "increment statement"
  GoIncDec _ False -> "decrement statement"
  GoSend _ _ -> "channel send"
  GoReturn _ -> "return statement"
  GoBreak _ -> "break statement"
  GoContinue _ -> "continue statement"
  GoGoto _ -> "goto statement"
  GoFallthrough -> "fallthrough statement"
  GoEmpty -> "empty statement"
  GoBlock _ -> "block"
  GoIf {} -> "if statement"
  GoSwitch {} -> "switch statement"
  GoTypeSwitch {} -> "type switch statement"
  GoFor {} -> "for loop"
  GoRange {} -> "range loop"
  GoSelect {} -> "select statement"
  GoDefer {} -> "defer statement"
  GoGo {} -> "go statement"
  GoCase {} -> "case clause"
  GoDefault {} -> "default clause"
  GoCommCase {} -> "select communication clause"
  GoCommDefault {} -> "select default clause"
  GoLabeled {} -> "labeled statement"

describeGoExpr :: GoExpr -> Text
describeGoExpr expr = case expr of
  GoLiteral _ -> "literal expression"
  GoIdent _ -> "identifier"
  GoQualifiedIdent _ _ -> "qualified identifier"
  GoBinaryOp _ _ _ -> "binary operation"
  GoUnaryOp _ _ -> "unary operation"
  GoComparison _ _ _ -> "comparison expression"
  GoCall _ _ -> "call expression"
  GoIndex _ _ -> "index expression"
  GoSlice _ _ -> "slice expression"
  GoSelector _ _ -> "selector expression"
  GoTypeAssert _ _ -> "type assertion"
  GoCompositeLit _ _ -> "composite literal"
  GoArrayLit _ _ -> "array literal"
  GoSliceLit _ _ -> "slice literal"
  GoMapLit _ _ -> "map literal"
  GoStructLit _ _ -> "struct literal"
  GoAddress _ -> "address-of expression"
  GoDeref _ -> "dereference expression"
  GoReceive _ -> "receive expression"
  GoTypeConversion _ _ -> "type conversion"
  GoFuncLit _ -> "function literal"
  _ -> "expression"

generateGoFile :: GoFile -> CppCodeGen ()
generateGoFile goFile = do
  let decls = goFileDecls goFile
  emitInfo $ "Processing Go file with " <> T.pack (show (length decls)) <> " declarations"
  when (null decls) $
    reportUnsupported $ "No declarations found in Go file '" <> goFileName goFile <> "' - parser may need to be fixed"
  mapM_ generateGoDecl decls

generateGoDecl :: Located GoDecl -> CppCodeGen ()
generateGoDecl (Located span decl) = case decl of
  GoFuncDecl func -> do
    emitInfo $ "Generating function: " <> maybe "anonymous" (\(Identifier n) -> n) (goFuncName func)
    generateGoFunction func
  GoTypeDecl name typeExpr -> do
    emitInfo $ "Generating type declaration: " <> (\(Identifier n) -> n) name
    cppType <- generateGoType typeExpr
    addDeclaration $ CppTypedef ((\(Identifier n) -> n) name) cppType
  GoVarDecl vars -> do
    emitInfo "Generating variable declaration(s)"
    mapM_ generateGoVariable vars
  _ ->
    reportDeclIssue reportFatalNotImplemented span decl "Unsupported Go declaration" (Just (T.pack (show decl)))

generateGoFunction :: GoFunction -> CppCodeGen ()
generateGoFunction func =
  case goFuncName func of
    Nothing -> pure ()
    Just (Identifier name) -> do
      paramGroups <- mapM mapGoParameter (goFuncParams func)
      let cppParams = concat paramGroups
      returnType <- mapGoResultsForMain name (goFuncResults func)
      case goFuncBody func of
        Nothing -> addDeclaration $ CppFunction name returnType cppParams []
        Just bodyStmt -> do
          bodyStmts <- generateGoBlockStmt bodyStmt
          let finalStmts =
                if name == "main" && not (hasReturnStmt bodyStmts)
                  then bodyStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                  else bodyStmts
          addDeclaration $ CppFunction name returnType cppParams finalStmts
          when (name == "main") $
            addInclude "<iostream>"

hasReturnStmt :: [CppStmt] -> Bool
hasReturnStmt = any isReturnStmt . concatMap flatten
  where
    flatten stmt = case stmt of
      CppStmtSeq seqStmts -> concatMap flatten seqStmts
      CppBlock stmts -> concatMap flatten stmts
      other -> [other]

    isReturnStmt (CppReturn _) = True
    isReturnStmt _ = False

generateGoBlockStmt :: Located GoStmt -> CppCodeGen [CppStmt]
generateGoBlockStmt located@(Located _ stmt) = case stmt of
  GoBlock stmts -> mapM generateGoStmt stmts
  _ -> do
    single <- generateGoStmt located
    pure [single]

generateGoStmt :: Located GoStmt -> CppCodeGen CppStmt
generateGoStmt (Located span stmt) = case stmt of
  GoReturn exprs -> do
    case exprs of
      [] -> pure $ CppReturn Nothing
      [expr] -> CppReturn . Just <$> generateGoExpr expr
      _ -> do
        addInclude "<tuple>"
        cppExprs <- mapM generateGoExpr exprs
        pure $ CppReturn (Just (CppCall (CppVar "std::make_tuple") cppExprs))
  GoExprStmt expr -> CppExprStmt <$> generateGoExpr expr
  GoIf mInit cond thenStmt elseStmt -> do
    cppCond <- generateGoExpr cond
    cppThen <- generateGoBlockStmt thenStmt
    cppElse <- maybe (pure []) generateGoBlockStmt elseStmt
    let ifStmt = CppIf cppCond cppThen cppElse
    case mInit of
      Nothing -> pure ifStmt
      Just initStmt -> do
        initCpp <- generateGoStmt initStmt
        pure $ CppBlock [initCpp, ifStmt]
  GoFor mClause bodyStmt ->
    case mClause of
      Nothing -> do
        bodyStmts <- generateGoBlockStmt bodyStmt
        pure $ CppWhile (CppLiteral (CppBoolLit True)) bodyStmts
      Just clause -> do
        initStmt <- case goForInit clause of
          Nothing -> pure Nothing
          Just initS -> generateGoForInit initS
        condExpr <- case goForCond clause of
          Nothing -> pure Nothing
          Just condE -> Just <$> generateGoExpr condE
        postExpr <- case goForPost clause of
          Nothing -> pure Nothing
          Just postS -> generateGoForPost postS
        bodyStmts <- generateGoBlockStmt bodyStmt
        pure $ CppFor initStmt condExpr postExpr bodyStmts
  GoBlock stmts -> CppBlock <$> mapM generateGoStmt stmts
  GoGo expr -> do
    addInclude "<thread>"
    addInclude "<functional>"
    cppExpr <- generateGoExpr expr
    tempName <- generateTempVar
    let threadVar = tempName <> "_thread"
        threadType = CppClassType "std::thread" []
        threadInit = case cppExpr of
          CppCall func args -> CppCall (CppVar "std::thread") (func : args)
          _ -> CppCall (CppVar "std::thread")
                 [CppLambda [] [CppExprStmt cppExpr]]
        declStmt = CppDecl (CppVariable threadVar threadType (Just threadInit))
        detachStmt = CppExprStmt (CppCall (CppMember (CppVar threadVar) "detach") [])
    pure $ CppBlock [declStmt, detachStmt]
  GoSend channel value -> do
    ensureChannelSupport
    cppChannel <- generateGoExpr channel
    cppValue <- generateGoExpr value
    pure $ CppExprStmt $ CppCall (CppMember cppChannel "send") [cppValue]
  GoDefine identifiers exprs ->
    if length identifiers /= length exprs
      then do
        reportStmtIssue reportUnsupported span stmt "Mismatched variable definition arity" Nothing
        pure cppNoop
      else do
        cppExprs <- mapM generateGoExpr exprs
        decls <- mapM defineBinding (zip3 identifiers exprs cppExprs)
        pure $ wrapStmts decls
  GoAssign leftExprs rightExprs ->
    case (leftExprs, rightExprs) of
      ([leftExpr], [rightExpr]) -> do
        cppRight <- generateGoExpr rightExpr
        case leftExpr of
          Located _ (GoIdent (Identifier "_")) -> pure $ CppExprStmt cppRight
          _ -> do
            cppLeft <- generateGoExpr leftExpr
            pure $ CppExprStmt (CppBinary "=" cppLeft cppRight)
      _ ->
        if length leftExprs /= length rightExprs
          then do
            reportStmtIssue reportUnsupported span stmt "Mismatched assignment arity" Nothing
            pure cppNoop
          else do
            prepResults <- mapM prepareAssignment (zip leftExprs rightExprs)
            let evalStmts = map fst prepResults
                assignmentInputs = catMaybes (map snd prepResults)
            assignStmts <- mapM buildAssignment assignmentInputs
            pure $ wrapStmts (evalStmts ++ assignStmts)
  GoIncDec expr isIncrement -> do
    cppExpr <- generateGoExpr expr
    let op = if isIncrement then "++" else "--"
    pure $ CppExprStmt $ CppUnary op cppExpr
  _ -> do
    reportStmtIssue reportNotImplemented span stmt "Unsupported Go statement" (Just (T.pack (show stmt)))
    pure cppNoop
  where
    wrapStmts [] = CppStmtSeq []
    wrapStmts [single] = single
    wrapStmts stmts = CppStmtSeq stmts

    defineBinding (Identifier name, locatedExpr, expr)
      | name == "_" = pure $ CppExprStmt expr
      | otherwise = do
          refinedType <- refineGoExprType ("definition of " <> name) locatedExpr CppAuto
          pure $ CppDecl (CppVariable name refinedType (Just expr))

    prepareAssignment (leftExpr, rightExpr) = do
      rhsCpp <- generateGoExpr rightExpr
      case leftExpr of
        Located _ (GoIdent (Identifier "_")) ->
          pure (CppExprStmt rhsCpp, Nothing)
        _ -> do
          tempName <- generateTempVar
          refinedType <- refineGoExprType ("temporary binding " <> tempName) rightExpr CppAuto
          let tempVar = CppVar tempName
              declStmt = CppDecl (CppVariable tempName refinedType (Just rhsCpp))
          pure (declStmt, Just (leftExpr, tempVar))

    buildAssignment (leftExpr, tempVarExpr) = do
      leftCpp <- generateGoExpr leftExpr
      pure $ CppExprStmt (CppBinary "=" leftCpp tempVarExpr)

generateGoForInit :: Located GoStmt -> CppCodeGen (Maybe CppStmt)
generateGoForInit stmt@(Located span inner) = case inner of
  GoDefine _ _ -> Just <$> generateGoStmt stmt
  GoAssign _ _ -> Just <$> generateGoStmt stmt
  GoExprStmt _ -> Just <$> generateGoStmt stmt
  GoIncDec _ _ -> Just <$> generateGoStmt stmt
  GoEmpty -> pure Nothing
  _ -> do
    reportStmtIssue reportUnsupported span inner "Unsupported for-loop initializer" (Just (T.pack (show inner)))
    pure Nothing

generateGoForPost :: Located GoStmt -> CppCodeGen (Maybe CppExpr)
generateGoForPost (Located span inner) = case inner of
  GoIncDec expr isInc -> do
    cppExpr <- generateGoExpr expr
    let op = if isInc then "++" else "--"
    pure $ Just $ CppUnary op cppExpr
  GoAssign [leftExpr] [rightExpr] -> do
    cppLeft <- generateGoExpr leftExpr
    cppRight <- generateGoExpr rightExpr
    pure $ Just $ CppBinary "=" cppLeft cppRight
  GoExprStmt expr -> Just <$> generateGoExpr expr
  GoEmpty -> pure Nothing
  _ -> do
    reportStmtIssue reportUnsupported span inner "Unsupported for-loop post statement" (Just (T.pack (show inner)))
    pure Nothing

generateGoExpr :: Located GoExpr -> CppCodeGen CppExpr
generateGoExpr (Located span expr) = case expr of
  GoLiteral lit -> pure $ CppLiteral $ mapGoLiteral lit
  GoIdent (Identifier name) -> pure $ CppVar name
  GoBinaryOp op left right -> do
    cppLeft <- generateGoExpr left
    cppRight <- generateGoExpr right
    let cppOp = mapGoBinaryOp op
    pure $ CppBinary cppOp cppLeft cppRight
  GoComparison compOp left right -> do
    cppLeft <- generateGoExpr left
    cppRight <- generateGoExpr right
    let cppOp = mapComparisonOp compOp
    pure $ CppBinary cppOp cppLeft cppRight
  GoUnaryOp uop inner -> do
    cppInner <- generateGoExpr inner
    let cppOp = case uop of
          OpNot -> "!"
          OpNegate -> "-"
          OpPositive -> "+"
          OpBitNot -> "~"
    pure $ CppUnary cppOp cppInner
  GoCall func args -> do
    cppFunc <- generateGoExpr func
    cppArgs <- mapM generateGoExpr args
    case cppFunc of
      CppMember (CppVar "fmt") "Printf" -> do
        addInclude "<cstdio>"
        pure $ CppCall (CppVar "std::printf") cppArgs
      CppMember (CppVar "fmt") "Println" -> do
        addInclude "<iostream>"
        pure $ buildPrintlnExpr cppArgs
      CppMember (CppVar "fmt") "Print" -> do
        addInclude "<iostream>"
        pure $ buildPrintExpr cppArgs
      _ -> pure $ CppCall cppFunc cppArgs
  GoQualifiedIdent (Identifier pkg) (Identifier name) ->
    pure $ CppVar (pkg <> "::" <> name)
  GoSelector obj (Identifier member) -> do
    cppObj <- generateGoExpr obj
    pure $ CppMember cppObj member
  GoReceive channelExpr -> do
    ensureChannelSupport
    cppExpr <- generateGoExpr channelExpr
    pure $ CppCall (CppMember cppExpr "receive") []
  _ -> do
    reportExprIssue reportFatalNotImplemented span expr "Unsupported Go expression" (Just (T.pack (show expr)))
    pure $ CppLiteral (CppIntLit 0)

buildPrintExpr :: [CppExpr] -> CppExpr
buildPrintExpr args =
  let components = if null args
        then [CppLiteral (CppStringLit "")]
        else spaceSeparate args
  in streamChain (CppVar "std::cout") components

buildPrintlnExpr :: [CppExpr] -> CppExpr
buildPrintlnExpr args =
  let components =
        if null args
          then [CppVar "std::endl"]
          else spaceSeparate args ++ [CppVar "std::endl"]
  in streamChain (CppVar "std::cout") components

cppTypeContainsChannel :: CppType -> Bool
cppTypeContainsChannel ty = case ty of
  CppTemplateType name args -> (name == "Channel") || any cppTypeContainsChannel args
  CppVector inner -> cppTypeContainsChannel inner
  CppStdArray inner _ -> cppTypeContainsChannel inner
  CppArray inner _ -> cppTypeContainsChannel inner
  CppPointer inner -> cppTypeContainsChannel inner
  CppReference inner -> cppTypeContainsChannel inner
  CppRvalueRef inner -> cppTypeContainsChannel inner
  CppConst inner -> cppTypeContainsChannel inner
  CppVolatile inner -> cppTypeContainsChannel inner
  CppOptional inner -> cppTypeContainsChannel inner
  CppUniquePtr inner -> cppTypeContainsChannel inner
  CppSharedPtr inner -> cppTypeContainsChannel inner
  CppVariant inners -> any cppTypeContainsChannel inners
  CppPair lhs rhs -> cppTypeContainsChannel lhs || cppTypeContainsChannel rhs
  CppTuple inners -> any cppTypeContainsChannel inners
  CppMap keyTy valueTy -> cppTypeContainsChannel keyTy || cppTypeContainsChannel valueTy
  CppUnorderedMap keyTy valueTy -> cppTypeContainsChannel keyTy || cppTypeContainsChannel valueTy
  CppFunctionType args ret -> any cppTypeContainsChannel args || cppTypeContainsChannel ret
  CppClassType _ args -> any cppTypeContainsChannel args
  CppStructLiteral fields -> any (cppTypeContainsChannel . snd) fields
  _ -> False

ensureChannelSupport :: CppCodeGen ()
ensureChannelSupport = do
  alreadyDefined <- gets (any isChannelDecl . cgsDeclarations)
  addInclude "<mutex>"
  addInclude "<condition_variable>"
  addInclude "<queue>"
  addInclude "<cstddef>"
  unless alreadyDefined $
    generateChannelClass
  where
    isChannelDecl (CppTemplate _ (CppClass "Channel" _ _)) = True
    isChannelDecl _ = False

generateChannelClass :: CppCodeGen ()
generateChannelClass = do
  let templateParam = CppTemplateType "T" []
      queueType = CppTemplateType "std::queue" [templateParam]
      mutexType = CppClassType "std::mutex" []
      cvType = CppClassType "std::condition_variable" []
      lockType = CppTemplateType "std::unique_lock" [mutexType]

      publicMembers =
        [ CppAccessSpec "public"
        , CppConstructor "Channel" [CppParam "capacity" CppSizeT Nothing]
            [ CppExprStmt $ CppBinary "=" (CppMember CppThis "capacity_") (CppVar "capacity")
            ]
        , CppMethod "send" CppVoid [CppParam "value" templateParam Nothing]
            [ CppDecl $ CppVariable "lock" lockType
                (Just $ CppCall (CppVar "std::unique_lock<std::mutex>") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "wait")
                [ CppVar "lock"
                , CppLambda []
                    [ CppReturn $ Just $ CppBinary "<"
                        (CppCall (CppMember (CppMember CppThis "queue_") "size") [])
                        (CppMember CppThis "capacity_")
                    ]
                ]
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "push") [CppVar "value"]
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "notify_one") []
            ] False
        , CppMethod "receive" templateParam []
            [ CppDecl $ CppVariable "value" templateParam Nothing
            , CppDecl $ CppVariable "lock" lockType
                (Just $ CppCall (CppVar "std::unique_lock<std::mutex>") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "wait")
                [ CppVar "lock"
                , CppLambda []
                    [ CppReturn $ Just $ CppUnary "!"
                        (CppCall (CppMember (CppMember CppThis "queue_") "empty") [])
                    ]
                ]
            , CppExprStmt $ CppBinary "=" (CppVar "value")
                (CppCall (CppMember (CppMember CppThis "queue_") "front") [])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "pop") []
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "notify_one") []
            , CppReturn $ Just $ CppVar "value"
            ] False
        ]

      privateMembers =
        [ CppAccessSpec "private"
        , CppVariable "queue_" queueType Nothing
        , CppVariable "mutex_" mutexType Nothing
        , CppVariable "cv_" cvType Nothing
        , CppVariable "capacity_" CppSizeT Nothing
        ]

  addDeclaration $ CppTemplate ["T"] (CppClass "Channel" [] (publicMembers ++ privateMembers))

mapGoParameter :: GoField -> CppCodeGen [CppParam]
mapGoParameter field = do
  cppType <- generateGoType (goFieldType field)
  let rawNames = [n | Identifier n <- goFieldNames field]
  names <- case rawNames of
    [] -> (:[]) <$> generateTempVar
    _  -> mapM sanitizeName rawNames
  pure [CppParam name cppType Nothing | name <- names]
  where
    sanitizeName name
      | name == "_" = generateTempVar
      | otherwise = pure name

generateGoType :: Located GoType -> CppCodeGen CppType
generateGoType (Located _ goType) = do
  let cppType = mapGoTypeToCpp goType
  when (cppTypeContainsChannel cppType) ensureChannelSupport
  mapM_ addInclude (collectCppTypeIncludes cppType)
  pure cppType

mapGoResults :: [GoField] -> CppCodeGen CppType
mapGoResults [] = pure CppVoid
mapGoResults [field] = generateGoType (goFieldType field)
mapGoResults fields = do
  addInclude "<tuple>"
  types <- mapM (generateGoType . goFieldType) fields
  pure $ CppTuple types

mapGoResultsForMain :: Text -> [GoField] -> CppCodeGen CppType
mapGoResultsForMain "main" [] = pure CppInt
mapGoResultsForMain _ results = mapGoResults results

generateGoVariable :: (Identifier, Maybe (Located GoType), Maybe (Located GoExpr)) -> CppCodeGen ()
generateGoVariable (Identifier name, mtype, mexpr) = do
  baseType <- case mtype of
    Just typeExpr -> generateGoType typeExpr
    Nothing -> pure CppAuto
  case mexpr of
    Just locatedExpr -> do
      cppExpr <- generateGoExpr locatedExpr
      refinedType <- refineGoExprType ("variable " <> name) locatedExpr baseType
      addDeclaration $ CppVariable name refinedType (Just cppExpr)
    Nothing -> addDeclaration $ CppVariable name baseType Nothing

mapGoLiteral :: GoLiteral -> CppLiteral
mapGoLiteral = \case
  GoInt i -> CppIntLit i
  GoFloat f -> CppFloatLit f
  GoBool b -> CppBoolLit b
  GoString s -> CppStringLit s
  GoNil -> CppNullPtr
  _ -> CppIntLit 0

mapGoBinaryOp :: BinaryOp -> Text
mapGoBinaryOp = \case
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpFloorDiv -> "/"
  OpMod -> "%"
  OpBitAnd -> "&"
  OpBitOr -> "|"
  OpBitXor -> "^"
  OpShiftL -> "<<"
  OpShiftR -> ">>"
  OpAnd -> "&&"
  OpOr -> "||"
  OpConcat -> "+"
  _ -> "+"

mapComparisonOp :: ComparisonOp -> Text
mapComparisonOp = \case
  OpEq -> "=="
  OpNe -> "!="
  OpLt -> "<"
  OpLe -> "<="
  OpGt -> ">"
  OpGe -> ">="
  OpIs -> "=="
  OpIsNot -> "!="

refineGoExprType :: Text -> Located GoExpr -> CppType -> CppCodeGen CppType
refineGoExprType context locatedExpr defaultType =
  case goExprToCommon locatedExpr of
    Left err -> do
      emitInfo $ context <> ": unable to fingerprint expression for annotations - " <> renderLoweringIssue err
      pure defaultType
    Right common ->
      let exprKey = renderCommonExpr common
      in lookupAndApplyAnnotations context exprKey defaultType
