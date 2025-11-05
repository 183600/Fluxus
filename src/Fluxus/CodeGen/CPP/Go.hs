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
import Fluxus.AST.Common (BinaryOp(..), ComparisonOp(..), Identifier(..), Located(..), UnaryOp(..))
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
  addInclude "<iostream>"
  addInclude "<string>"
  addInclude "<thread>"
  addInclude "<mutex>"
  addInclude "<condition_variable>"
  addInclude "<queue>"
  addInclude "<vector>"
  addInclude "<functional>"
  addInclude "<atomic>"
  addInclude "<chrono>"

  let packageName = (\(Identifier n) -> n) (goPackageName goPackage)
  emitInfo $ "Generating C++ for Go package: " <> packageName

  generateChannelClass

  let files = goPackageFiles goPackage
  emitInfo $ "Found " <> T.pack (show (length files)) <> " files in package"
  when (null files) $
    reportUnsupported "No files found in package"

  mapM_ generateGoFile files

  when (packageName == "main") $ do
    hasMain <- gets (any isMainFunction . cgsDeclarations)
    unless hasMain $ do
      reportUnsupported "Generating fallback main function - Go parser not working properly"
      addInclude "<iostream>"
      addDeclaration $ CppFunction "main" CppInt [] [CppReturn (Just (CppLiteral (CppIntLit 0)))]

  includes <- gets cgsIncludes
  namespaces <- gets cgsNamespaces
  decls <- gets cgsDeclarations
  pure $ CppUnit includes namespaces (reverse decls)
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False

generateGoFile :: GoFile -> CppCodeGen ()
generateGoFile goFile = do
  let decls = goFileDecls goFile
  emitInfo $ "Processing Go file with " <> T.pack (show (length decls)) <> " declarations"
  when (null decls) $
    reportUnsupported "No declarations found in Go file - parser may need to be fixed"
  mapM_ generateGoDecl decls

generateGoDecl :: Located GoDecl -> CppCodeGen ()
generateGoDecl (Located _ decl) = case decl of
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
  _ -> do
    let msg = "Go declaration not implemented: " <> T.pack (show decl)
    reportFatalNotImplemented msg

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
generateGoStmt (Located _ stmt) = case stmt of
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
    cppChannel <- generateGoExpr channel
    cppValue <- generateGoExpr value
    pure $ CppExprStmt $ CppCall (CppMember cppChannel "send") [cppValue]
  GoDefine identifiers exprs ->
    if length identifiers /= length exprs
      then do
        reportUnsupported "Mismatched variable definition arity"
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
            reportUnsupported "Mismatched assignment arity"
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
    reportNotImplemented $ "TODO: Implement Go statement: " <> T.pack (show stmt)
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
generateGoForInit stmt@(Located _ inner) = case inner of
  GoDefine _ _ -> Just <$> generateGoStmt stmt
  GoAssign _ _ -> Just <$> generateGoStmt stmt
  GoExprStmt _ -> Just <$> generateGoStmt stmt
  GoIncDec _ _ -> Just <$> generateGoStmt stmt
  GoEmpty -> pure Nothing
  _ -> do
    reportUnsupported $ "Unsupported for-loop initializer: " <> T.pack (show inner)
    pure Nothing

generateGoForPost :: Located GoStmt -> CppCodeGen (Maybe CppExpr)
generateGoForPost (Located _ inner) = case inner of
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
    reportUnsupported $ "Unsupported for-loop post statement: " <> T.pack (show inner)
    pure Nothing

generateGoExpr :: Located GoExpr -> CppCodeGen CppExpr
generateGoExpr (Located _ expr) = case expr of
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
    cppExpr <- generateGoExpr channelExpr
    pure $ CppCall (CppMember cppExpr "receive") []
  _ -> do
    let msg = "Go expression not implemented: " <> T.pack (show expr)
    reportFatalNotImplemented msg
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
