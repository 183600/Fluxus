{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Fluxus.CodeGen.CPP.Go
  ( generateCppFromGo
  ) where

import Control.Monad (unless, when)
import Control.Monad.State (gets)
import Data.List (partition)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.AST.Go
import Fluxus.AST.Common (BinaryOp(..), ComparisonOp(..), Identifier(..), Located(..), SourcePos(..), SourceSpan(..), UnaryOp(..))
import Fluxus.Analysis.CommonExprLowering (goExprToLocatedCommon, fingerprintCommonExpr, renderLoweringIssue)
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
  , emitInfo
  , emitWarning
  , generateTempVar
  , reportFatalNotImplemented
  , reportNotImplemented
  , reportUnsupported
  , runtimeAbortStmt
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
generateCppFromGo (GoAST goPkg) = do
  let packageName = (\(Identifier n) -> n) (goPackageName goPkg)
  emitInfo $ "Generating C++ for Go package: " <> packageName

  let files = goPackageFiles goPkg
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
formatSourceSpanShort SourceSpan { spanFilename = filename, spanStart = SourcePos line col } =
  filename <> ":" <> textShow line <> ":" <> textShow col

textShow :: Show a => a -> Text
textShow = T.pack . show

wrapStatements :: [CppStmt] -> CppStmt
wrapStatements [] = CppStmtSeq []
wrapStatements [single] = single
wrapStatements stmts = CppStmtSeq stmts

reportDeclIssue :: (Text -> CppCodeGen ()) -> SourceSpan -> GoDecl -> Text -> Maybe Text -> CppCodeGen ()
reportDeclIssue reporter srcSpan decl detail extra =
  reporter (formatDeclIssue srcSpan decl detail extra)

reportStmtIssue :: (Text -> CppCodeGen ()) -> SourceSpan -> GoStmt -> Text -> Maybe Text -> CppCodeGen ()
reportStmtIssue reporter srcSpan stmt detail extra =
  reporter (formatStmtIssue srcSpan stmt detail extra)

reportExprIssue :: (Text -> CppCodeGen ()) -> SourceSpan -> GoExpr -> Text -> Maybe Text -> CppCodeGen ()
reportExprIssue reporter srcSpan expr detail extra =
  reporter (formatExprIssue srcSpan expr detail extra)

formatDeclIssue :: SourceSpan -> GoDecl -> Text -> Maybe Text -> Text
formatDeclIssue srcSpan decl detail extra =
  detail
    <> " ("
    <> describeGoDecl decl
    <> " at "
    <> formatSourceSpanShort srcSpan
    <> ")"
    <> maybe "" (": " <>) extra

formatStmtIssue :: SourceSpan -> GoStmt -> Text -> Maybe Text -> Text
formatStmtIssue srcSpan stmt detail extra =
  detail
    <> " ("
    <> describeGoStmt stmt
    <> " at "
    <> formatSourceSpanShort srcSpan
    <> ")"
    <> maybe "" (": " <>) extra

formatExprIssue :: SourceSpan -> GoExpr -> Text -> Maybe Text -> Text
formatExprIssue srcSpan expr detail extra =
  detail
    <> " ("
    <> describeGoExpr expr
    <> " at "
    <> formatSourceSpanShort srcSpan
    <> ")"
    <> maybe "" (": " <>) extra

abortStmtIssue :: (Text -> CppCodeGen ()) -> SourceSpan -> GoStmt -> Text -> Maybe Text -> CppCodeGen CppStmt
abortStmtIssue reporter srcSpan stmt detail extra = do
  let message = formatStmtIssue srcSpan stmt detail extra
  reporter message
  abortStmt <- runtimeAbortStmt message
  pure $ CppStmtSeq
    [ CppComment ("unsupported: " <> detail)
    , abortStmt
    ]

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

generateGoFile :: GoFile -> CppCodeGen ()
generateGoFile goFile = do
  let decls = goFileDecls goFile
  emitInfo $ "Processing Go file with " <> T.pack (show (length decls)) <> " declarations"
  when (null decls) $
    reportUnsupported $ "No declarations found in Go file '" <> goFileName goFile <> "' - parser may need to be fixed"
  mapM_ generateGoDecl decls

generateGoDecl :: Located GoDecl -> CppCodeGen ()
generateGoDecl (Located srcSpan decl) = case decl of
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
    reportDeclIssue reportFatalNotImplemented srcSpan decl "Unsupported Go declaration" (Just (T.pack (show decl)))

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
generateGoStmt (Located srcSpan stmt) = case stmt of
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
  GoSwitch mInit mExpr clauses ->
    generateGoSwitch srcSpan mInit mExpr clauses
  GoSelect clauses ->
    generateGoSelect srcSpan clauses
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
      then abortStmtIssue reportUnsupported srcSpan stmt "Mismatched variable definition arity" Nothing
      else do
        cppExprs <- mapM generateGoExpr exprs
        decls <- mapM defineBinding (zip3 identifiers exprs cppExprs)
        pure $ wrapStatements decls
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
          then abortStmtIssue reportUnsupported srcSpan stmt "Mismatched assignment arity" Nothing
          else do
            prepResults <- mapM prepareAssignment (zip leftExprs rightExprs)
            let evalStmts = map fst prepResults
                assignmentInputs = catMaybes (map snd prepResults)
            assignStmts <- mapM buildAssignment assignmentInputs
            pure $ wrapStatements (evalStmts ++ assignStmts)
  GoIncDec expr isIncrement -> do
    cppExpr <- generateGoExpr expr
    let op = if isIncrement then "++" else "--"
    pure $ CppExprStmt $ CppUnary op cppExpr
  GoBreak mLabel -> do
    case mLabel of
      Just (Identifier label) ->
        emitWarning $ "labeled break '" <> label <> "' is not supported; emitting plain break"
      Nothing -> pure ()
    pure CppBreak
  GoContinue mLabel -> do
    case mLabel of
      Just (Identifier label) ->
        emitWarning $ "labeled continue '" <> label <> "' is not supported; emitting plain continue"
      Nothing -> pure ()
    pure CppContinue
  GoEmpty -> pure (CppStmtSeq [])
  _ ->
    abortStmtIssue reportNotImplemented srcSpan stmt "Unsupported Go statement" (Just (T.pack (show stmt)))
  where
    defineBinding (Identifier name, locatedExpr, expr)
      | name == "_" = pure $ CppExprStmt expr
      | otherwise = do
          let baseType = inferBaseType locatedExpr
          refinedType <- refineGoExprType ("definition of " <> name) locatedExpr baseType
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

data SwitchBranch = SwitchBranch
  { sbCondition :: Maybe CppExpr
  , sbBody :: [CppStmt]
  }

combineWithOr :: [CppExpr] -> CppExpr
combineWithOr [] = CppLiteral (CppBoolLit False)
combineWithOr (firstExpr : restExprs) =
  foldl' (\acc nextExpr -> CppBinary "||" acc nextExpr) firstExpr restExprs

generateGoSwitch :: SourceSpan -> Maybe (Located GoStmt) -> Maybe (Located GoExpr) -> [Located GoStmt] -> CppCodeGen CppStmt
generateGoSwitch srcSpan mInit mExpr clauses = do
  initStmt <- traverse generateGoStmt mInit
  (valueDecl, scrutExpr) <- case mExpr of
    Nothing -> pure (Nothing, Nothing)
    Just expr -> do
      cppExpr <- generateGoExpr expr
      tempName <- generateTempVar
      refinedType <- refineGoExprType "switch expression" expr CppAuto
      let decl = CppDecl (CppVariable tempName refinedType (Just cppExpr))
      pure (Just decl, Just (CppVar tempName))
  loweredClauses <- mapM (lowerClause scrutExpr) clauses
  let switchClauses = catMaybes loweredClauses
  when (null switchClauses) $
    emitWarning $ "switch statement at " <> formatSourceSpanShort srcSpan <> " has no clauses that can be emitted"
  matchFlagName <- generateTempVar
  let matchVarExpr = CppVar matchFlagName
      matchDecl = CppDecl (CppVariable matchFlagName CppBool (Just (CppLiteral (CppBoolLit False))))
      guardExpr = CppUnary "!" matchVarExpr
      setMatched = CppExprStmt (CppBinary "=" matchVarExpr (CppLiteral (CppBoolLit True)))
      buildClause SwitchBranch { sbCondition = branchCondition, sbBody = branchBody } =
        let condition = maybe guardExpr (\condExpr -> CppBinary "&&" guardExpr condExpr) branchCondition
            body = setMatched : branchBody
        in CppIf condition body []
      clauseStmts = map buildClause switchClauses
      prefixStmts = catMaybes [initStmt, valueDecl]
      blockStmts = prefixStmts ++ [matchDecl] ++ clauseStmts
  pure $ CppBlock blockStmts
  where
    lowerClause scrut (Located clauseSpan clause) = case clause of
      GoCase exprs body -> do
        bodyStmts <- generateSwitchClauseBody body
        condition <- buildCaseCondition scrut exprs
        pure $ Just (SwitchBranch condition bodyStmts)
      GoDefault body -> do
        bodyStmts <- generateSwitchClauseBody body
        pure $ Just (SwitchBranch Nothing bodyStmts)
      other -> do
        reportStmtIssue emitWarning clauseSpan other "switch clause is not supported and will be ignored" Nothing
        pure Nothing

    buildCaseCondition _ [] = pure Nothing
    buildCaseCondition (Just scrutExpr) exprs = do
      comparisons <- mapM (buildComparison scrutExpr) exprs
      pure $ Just (combineWithOr comparisons)
    buildCaseCondition Nothing exprs = do
      conditions <- mapM generateGoExpr exprs
      pure $ Just (combineWithOr conditions)

    buildComparison scrutExpr targetExpr = do
      rhs <- generateGoExpr targetExpr
      pure (CppBinary "==" scrutExpr rhs)

generateSwitchClauseBody :: [Located GoStmt] -> CppCodeGen [CppStmt]
generateSwitchClauseBody = go
  where
    go [] = pure []
    go (Located fallSpan GoFallthrough : _) = do
      emitWarning $ "fallthrough at " <> formatSourceSpanShort fallSpan <> " is not yet supported in the C++ backend"
      pure []
    go (stmt : rest) = do
      cppStmt <- generateGoStmt stmt
      remaining <- go rest
      pure (cppStmt : remaining)

generateGoSelect :: SourceSpan -> [Located GoCommClause] -> CppCodeGen CppStmt
generateGoSelect srcSpan clauses = do
  let unwrap (Located _ clause) = clause
      (defaultClauses, commClauses) = partition (isNothing . goCommStmt . unwrap) clauses
  case (commClauses, defaultClauses) of
    ([], []) -> do
      emitWarning $ "select statement at " <> formatSourceSpanShort srcSpan <> " contains no clauses; emitting runtime abort helper"
      runtimeAbortStmt "select statements must contain at least one clause"
    ([], defClause : extraDefaults) -> do
      unless (null extraDefaults) $
        emitWarning $ "select statement at " <> formatSourceSpanShort srcSpan <> " has multiple default clauses; only the first one will be used"
      branch <- generateSelectBranch defClause
      pure $ wrapStatements branch
    ([singleClause], []) -> do
      branch <- generateSelectBranch singleClause
      pure $ wrapStatements branch
    _ -> do
      emitWarning $ "select statement at " <> formatSourceSpanShort srcSpan <> " uses combinations that are not yet supported; emitting runtime abort helper"
      runtimeAbortStmt "select statements with multiple clauses are not yet supported"

generateSelectBranch :: Located GoCommClause -> CppCodeGen [CppStmt]
generateSelectBranch (Located _ clause) = do
  prefix <- maybe (pure []) (\commStmt -> (:[]) <$> generateGoStmt commStmt) (goCommStmt clause)
  body <- mapM generateGoStmt (goCommBody clause)
  pure (prefix ++ body)

generateGoForInit :: Located GoStmt -> CppCodeGen (Maybe CppStmt)
generateGoForInit stmt@(Located srcSpan inner) = case inner of
  GoDefine _ _ -> Just <$> generateGoStmt stmt
  GoAssign _ _ -> Just <$> generateGoStmt stmt
  GoExprStmt _ -> Just <$> generateGoStmt stmt
  GoIncDec _ _ -> Just <$> generateGoStmt stmt
  GoEmpty -> pure Nothing
  _ -> do
    reportStmtIssue reportUnsupported srcSpan inner "Unsupported for-loop initializer" (Just (T.pack (show inner)))
    pure Nothing

generateGoForPost :: Located GoStmt -> CppCodeGen (Maybe CppExpr)
generateGoForPost (Located srcSpan inner) = case inner of
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
    reportStmtIssue reportUnsupported srcSpan inner "Unsupported for-loop post statement" (Just (T.pack (show inner)))
    pure Nothing

generateGoExpr :: Located GoExpr -> CppCodeGen CppExpr
generateGoExpr (Located srcSpan expr) = case expr of
  GoLiteral lit -> pure $ CppLiteral $ mapGoLiteral lit
  GoIdent (Identifier name) ->
    pure $ case name of
      "true" -> CppLiteral (CppBoolLit True)
      "false" -> CppLiteral (CppBoolLit False)
      "nil" -> CppLiteral CppNullPtr
      _ -> CppVar name
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
  GoStructLit structType fields ->
    generateGoStructLiteral srcSpan structType fields
  GoArrayLit typeExpr elements ->
    generateGoArrayLiteral srcSpan typeExpr elements
  GoSliceLit typeExpr elements ->
    generateGoSliceLiteral srcSpan typeExpr elements
  GoMapLit typeExpr entries ->
    generateGoMapLiteral srcSpan typeExpr entries
  GoCompositeLit Nothing _ -> do
    reportExprIssue reportFatalNotImplemented srcSpan expr "Composite literal requires an explicit type" Nothing
    pure $ CppLiteral (CppIntLit 0)
  GoCompositeLit (Just typeExpr) values ->
    generateGoCompositeLiteral srcSpan typeExpr values
  _ -> do
    reportExprIssue reportFatalNotImplemented srcSpan expr "Unsupported Go expression" (Just (T.pack (show expr)))
    pure $ CppLiteral (CppIntLit 0)

generateGoStructLiteral :: SourceSpan -> Located GoType -> [(Identifier, Located GoExpr)] -> CppCodeGen CppExpr
generateGoStructLiteral srcSpan typeExpr fields = do
  cppType <- generateGoType typeExpr
  values <- case cppType of
    CppStructLiteral structFields ->
      resolveStructLiteralValues srcSpan structFields fields
    _ -> do
      let keyedPresent = any (\(Identifier name, _) -> not (T.null name)) fields
      when keyedPresent $
        emitWarning $ "struct literal at " <> formatSourceSpanShort srcSpan <> " targets a named type; field keys will be emitted in source order"
      mapM (generateGoExpr . snd) fields
  pure $ CppBracedInit cppType values

generateGoCompositeLiteral :: SourceSpan -> Located GoType -> [Located GoExpr] -> CppCodeGen CppExpr
generateGoCompositeLiteral _ typeExpr values = do
  cppType <- generateGoType typeExpr
  valueExprs <- mapM generateGoExpr values
  pure (CppBracedInit cppType valueExprs)

generateGoArrayLiteral :: SourceSpan -> Located GoType -> [Located GoExpr] -> CppCodeGen CppExpr
generateGoArrayLiteral srcSpan typeExpr elements = do
  cppType <- generateGoType typeExpr
  cppElements <- mapM generateGoExpr elements
  case cppType of
    CppStdArray inner size -> do
      filled <- padSequentialLiteral srcSpan "array" size inner cppElements
      pure (CppBracedInit cppType filled)
    CppArray inner size -> do
      filled <- padSequentialLiteral srcSpan "array" size inner cppElements
      pure (CppBracedInit cppType filled)
    CppVector _ -> pure (CppBracedInit cppType cppElements)
    other -> do
      emitWarning $ "array literal at " <> formatSourceSpanShort srcSpan <> " targets unsupported type " <> T.pack (show other)
      pure (CppBracedInit cppType cppElements)

generateGoSliceLiteral :: SourceSpan -> Located GoType -> [Located GoExpr] -> CppCodeGen CppExpr
generateGoSliceLiteral srcSpan typeExpr elements = do
  cppType <- generateGoType typeExpr
  cppElements <- mapM generateGoExpr elements
  case cppType of
    CppVector _ -> pure (CppBracedInit cppType cppElements)
    other -> do
      emitWarning $ "slice literal at " <> formatSourceSpanShort srcSpan <> " targets unsupported type " <> T.pack (show other)
      pure (CppBracedInit cppType cppElements)

generateGoMapLiteral :: SourceSpan -> Located GoType -> [(Located GoExpr, Located GoExpr)] -> CppCodeGen CppExpr
generateGoMapLiteral srcSpan typeExpr entries = do
  cppType <- generateGoType typeExpr
  case cppType of
    CppMap keyTy valueTy -> buildInitializer cppType keyTy valueTy
    CppUnorderedMap keyTy valueTy -> buildInitializer cppType keyTy valueTy
    other -> do
      emitWarning $ "map literal at " <> formatSourceSpanShort srcSpan <> " targets unsupported type " <> T.pack (show other)
      pure (CppBracedInit cppType [])
  where
    buildInitializer targetType _keyTy _valueTy = do
      addInclude "<utility>"
      pairExprs <- mapM (uncurry buildPair) entries
      pure (CppBracedInit targetType pairExprs)
    buildPair keyExpr valueExpr = do
      keyCpp <- generateGoExpr keyExpr
      valCpp <- generateGoExpr valueExpr
      pure $ CppCall (CppVar "std::make_pair") [keyCpp, valCpp]

padSequentialLiteral :: SourceSpan -> Text -> Int -> CppType -> [CppExpr] -> CppCodeGen [CppExpr]
padSequentialLiteral srcSpan construct size innerType provided = do
  let truncated = take size provided
      extra = drop size provided
  unless (null extra) $
    emitWarning $
      construct <> " literal at " <> formatSourceSpanShort srcSpan <> " provides "
        <> textShow (length extra) <> " extra element(s) that will be ignored"
  let missing = size - length truncated
  when (missing > 0) $
    emitWarning $
      construct <> " literal at " <> formatSourceSpanShort srcSpan <> " is missing "
        <> textShow missing <> " element(s); default-initializing remaining entries"
  let defaults = replicate (max 0 missing) (defaultValueForType innerType)
  pure (truncated ++ defaults)

resolveStructLiteralValues :: SourceSpan -> [(Text, CppType)] -> [(Identifier, Located GoExpr)] -> CppCodeGen [CppExpr]
resolveStructLiteralValues srcSpan structFields provided =
  let keyedEntries = [(name, expr) | (Identifier name, expr) <- provided, not (T.null name)]
  in if null keyedEntries
       then resolvePositional
       else resolveKeyed keyedEntries
  where
    resolvePositional = do
      let needed = length structFields
          providedCount = length provided
          trimmed = take needed provided
          extra = drop needed provided
      exprs <- mapM (generateGoExpr . snd) trimmed
      unless (null extra) $
        emitWarning $ "struct literal at " <> formatSourceSpanShort srcSpan <> " provides extra values that will be ignored"
      when (providedCount < needed) $
        emitWarning $ "struct literal at " <> formatSourceSpanShort srcSpan <> " is missing values; default initialization will be used"
      let missingFields = drop (length trimmed) structFields
          defaults = map (defaultValueForType . snd) missingFields
      pure (exprs ++ defaults)

    resolveKeyed keyedEntries = do
      let providedMap = HM.fromList keyedEntries
          structNames = map fst structFields
          missingNames = [name | (name, _) <- structFields, not (HM.member name providedMap)]
          extraNames = [name | (name, _) <- keyedEntries, name `notElem` structNames]
      unless (null missingNames) $
        emitWarning $
          "struct literal at " <> formatSourceSpanShort srcSpan <> " is missing fields: "
            <> T.intercalate ", " missingNames
      unless (null extraNames) $
        emitWarning $
          "struct literal at " <> formatSourceSpanShort srcSpan <> " provided unknown fields: "
            <> T.intercalate ", " extraNames
      mapM
        (\(name, fieldType) ->
          case HM.lookup name providedMap of
            Just valueExpr -> generateGoExpr valueExpr
            Nothing -> pure (defaultValueForType fieldType))
        structFields

defaultValueForType :: CppType -> CppExpr
defaultValueForType ty = case ty of
  CppBool -> CppLiteral (CppBoolLit False)
  CppChar -> CppLiteral (CppCharLit '\0')
  CppUChar -> CppLiteral (CppCharLit '\0')
  CppShort -> CppLiteral (CppIntLit 0)
  CppUShort -> CppLiteral (CppIntLit 0)
  CppInt -> CppLiteral (CppIntLit 0)
  CppUInt -> CppLiteral (CppIntLit 0)
  CppLong -> CppLiteral (CppIntLit 0)
  CppULong -> CppLiteral (CppIntLit 0)
  CppLongLong -> CppLiteral (CppIntLit 0)
  CppULongLong -> CppLiteral (CppIntLit 0)
  CppFloat -> CppLiteral (CppFloatLit 0)
  CppDouble -> CppLiteral (CppFloatLit 0)
  CppLongDouble -> CppLiteral (CppFloatLit 0)
  CppPointer _ -> CppLiteral CppNullPtr
  CppString -> CppBracedInit CppString []
  _ -> CppBracedInit ty []

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
  GoImag f -> CppFloatLit f
  GoBool b -> CppBoolLit b
  GoString s -> CppStringLit s
  GoRawString s -> CppStringLit s
  GoRune c -> CppCharLit c
  GoNil -> CppNullPtr

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
  OpIn -> error "mapComparisonOp: OpIn requires special handling"
  OpNotIn -> error "mapComparisonOp: OpNotIn requires special handling"

inferBaseType :: Located GoExpr -> CppType
inferBaseType (Located _ expr) = case expr of
  GoLiteral (GoString _) -> CppString
  GoLiteral (GoRawString _) -> CppString
  GoBinaryOp OpAdd left right
    | isStringExpr left || isStringExpr right -> CppString
  _ -> CppAuto
  where
    isStringExpr (Located _ e) = case e of
      GoLiteral (GoString _) -> True
      GoLiteral (GoRawString _) -> True
      GoBinaryOp OpAdd l r -> isStringExpr l || isStringExpr r
      _ -> False

refineGoExprType :: Text -> Located GoExpr -> CppType -> CppCodeGen CppType
refineGoExprType context locatedExpr defaultType =
  case goExprToLocatedCommon locatedExpr of
    Left err -> do
      emitInfo $ context <> ": unable to fingerprint expression for annotations - " <> renderLoweringIssue err
      pure defaultType
    Right commonLocated ->
      let exprKey = fingerprintCommonExpr commonLocated
      in lookupAndApplyAnnotations context exprKey defaultType
