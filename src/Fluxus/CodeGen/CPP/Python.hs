{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Fluxus.CodeGen.CPP.Python
  ( generateCppFromPython
  ) where

import Control.Monad (unless, when, foldM)
import Control.Monad.State (gets, modify)
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', intercalate, nub, partition)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common hiding (TypeVar)
import Fluxus.AST.Python
import Fluxus.Analysis.CommonExprLowering (pythonExprToLocatedCommon, renderLocatedCommonExpr, fingerprintCommonExpr, renderLoweringIssue, formatSpan)
import Fluxus.CodeGen.CPP.AST
import Fluxus.CodeGen.CPP.Monad
import Fluxus.CodeGen.CPP.Shared
import Fluxus.Parser.Python.Lexer (runPythonLexer, PythonToken(..))
import Fluxus.Parser.Python.Parser (parseExpression)

-- | Generate C++ from Python AST
generateCppFromPython :: PythonAST -> CppCodeGen CppUnit
generateCppFromPython (PythonAST pyModule) = do
  -- Add basic C++ includes
  addInclude "<iostream>"
  addInclude "<string>"
  ensurePythonHelpers
  
  -- Generate module namespace (reserved for future use)
  let _moduleName = maybe "main" (\(ModuleName n) -> n) (pyModuleName pyModule)
  
  -- Process module body sequentially, capturing declarations and runtime statements
  moduleStmtsCpp <- mapM (generatePythonStmt ScopeModule) (pyModuleBody pyModule)
  -- Filter out comment statements that don't generate actual code
  let isActualStatement stmt = case stmt of
        CppComment _ -> False
        CppStmtSeq [] -> False
        _ -> True
      actualStmts = filter isActualStatement moduleStmtsCpp
      nameBinding = CppDecl (CppVariable "__name__" (CppConst CppString) (Just (CppLiteral (CppStringLit "__main__"))))
      preludeStmts = if null actualStmts then [] else [nameBinding]
      bodyStmts = preludeStmts ++ actualStmts
  
  -- Ensure we have a main function for standalone execution
  hasMain <- gets (any isMainFunction . cgsDeclarations)
  unless hasMain $ do
    -- If we have module-level statements, wrap them in main function
    let mainBody = if null bodyStmts
                   then [CppReturn (Just (CppLiteral (CppIntLit 0)))]
                   else bodyStmts ++ [CppReturn (Just (CppLiteral (CppIntLit 0)))]
    addDeclaration $ CppFunction "main" CppInt [] mainBody
  
  -- Build final unit
  includes <- gets cgsIncludes
  decls <- gets cgsDeclarations
  hoistedNames <- gets cgsHoistedGlobals
  
  let reversedDecls = reverse decls
      (hoistedDecls, remainingDecls) = extractHoisted hoistedNames reversedDecls
      finalDecls = hoistedDecls ++ remainingDecls
  
  return $ CppUnit includes [] finalDecls
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False

    extractHoisted :: [Text] -> [CppDecl] -> ([CppDecl], [CppDecl])
    extractHoisted [] ds = ([], ds)
    extractHoisted (name:names) ds =
      let (match, rest) = removeFirst (declMatches name) ds
          (more, remaining) = extractHoisted names rest
      in case match of
           Just decl -> (decl : more, remaining)
           Nothing -> (more, remaining)

    declMatches :: Text -> CppDecl -> Bool
    declMatches target (CppVariable name _ _) = name == target
    declMatches _ _ = False

    removeFirst :: (a -> Bool) -> [a] -> (Maybe a, [a])
    removeFirst _ [] = (Nothing, [])
    removeFirst p (x:xs)
      | p x = (Just x, xs)
      | otherwise =
          let (match, rest) = removeFirst p xs
          in (match, x : rest)


data RangeSpec = RangeSpec
  { rsStart :: CppExpr
  , rsEnd :: CppExpr
  , rsStep :: CppExpr
  , rsStepValue :: Maybe Integer
  }

data PythonScope
  = ScopeModule
  | ScopeFunction
  deriving (Eq, Show)

type LoweredCase = (CppExpr, [CppStmt])

data MatchLowering = MatchLowering
  { mlCondition :: CppExpr
  , mlBindings :: [PatternBinding]
  }

data PatternBinding = PatternBinding
  { pbName :: Text
  , pbType :: CppType
  , pbStmt :: CppStmt
  }

data IterableInfo = IterableInfo
  { iiExpr :: CppExpr
  , iiElementType :: Maybe CppType
  , iiDescription :: Text
  }

data MembershipCategory
  = MembershipStringCategory
  | MembershipSequenceCategory
  | MembershipMapCategory MapKind

data MapKind = OrderedMap | UnorderedMap

type OperandInfo = (Located PythonExpr, CppType, CppExpr)

-- | Generate C++ from Python statements
generatePythonStmt :: PythonScope -> Located PythonStmt -> CppCodeGen CppStmt
generatePythonStmt scope (Located span stmt) =
  case stmt of
    PyFuncDef funcDef -> do
      generatePythonFunction funcDef
      emitInfo "Function definition processed"
      return cppNoop
    PyClassDef classDef -> do
      generatePythonClass classDef
      emitInfo "Class definition processed"
      return cppNoop
    PyExprStmt expr -> do
      cppExpr <- generatePythonExpr expr
      return $ CppExprStmt cppExpr
    PyAssign patterns locatedExpr ->
      case patterns of
        [Located _ (PatVar (Identifier varName))] ->
          handleSimpleAssignment scope varName locatedExpr
        [Located _ (PatTuple patElems)] ->
          handleTupleUnpacking scope patElems locatedExpr
        _ | length patterns > 1 ->
              handleChainedAssignment scope span patterns locatedExpr
          | otherwise -> do
              let msg = "Assignment target pattern is not supported"
              strict <- gets (cgcStrictMode . cgsConfig)
              if strict
                then unsupportedStatement reportFatalNotImplemented span msg
                else unsupportedStatement reportNotImplemented span msg
    PyAugAssign target op valueExpr ->
      handleAugmentedAssignment scope span target op valueExpr
    PyAnnAssign target typeExpr mValue ->
      handleAnnotatedAssignment scope span target typeExpr mValue
    PyReturn mexpr -> do
      mcppExpr <- mapM generatePythonExpr mexpr
      return $ CppReturn mcppExpr
    PyAssert condition message ->
      generatePythonAssert span condition message
    PyBreak ->
      return CppBreak
    PyContinue ->
      return CppContinue
    PyPass ->
      return cppNoop
    PyIf condition thenStmts elseStmts -> do
      cppCond <- generatePythonExpr condition
      cppThen <- mapM (generatePythonStmt scope) thenStmts
      cppElse <- mapM (generatePythonStmt scope) elseStmts
      return $ CppIf cppCond cppThen cppElse
    PyWhile condition bodyStmts _ -> do
      cppCond <- generatePythonExpr condition
      cppBody <- mapM (generatePythonStmt scope) bodyStmts
      return $ CppWhile cppCond cppBody
    PyFor (Located _ (PatVar (Identifier varName))) iterExpr bodyStmts _ -> do
      symtab <- gets cgsSymbolTable
      let varAlreadyDeclared = HM.member varName symtab
      cppBody <- mapM (generatePythonStmt scope) bodyStmts
      case locatedValue iterExpr of
        PyCall (Located _ (PyVar (Identifier "range"))) rangeArgs -> do
          unless varAlreadyDeclared $
            modify $ \r ->
              r { cgsSymbolTable = HM.insert varName CppAuto (cgsSymbolTable r) }
          mSpec <- parseRangeArgs rangeArgs
          case mSpec of
            Nothing -> do
              let msg = "range() with unsupported arguments"
              reportFatalUnsupported msg
              return cppNoop
            Just spec -> buildRangeLoop varAlreadyDeclared varName cppBody spec
        _ -> do
          iterableInfo <- resolveIterableInfo iterExpr
          buildIterableLoop span varName varAlreadyDeclared iterableInfo cppBody
    PyWith items bodyStmts ->
      generatePythonWith scope span items bodyStmts
    PyTry tryBody excepts elseStmts finallyStmts ->
      generatePythonTry scope span tryBody excepts elseStmts finallyStmts
    PyMatch subject cases ->
      generatePythonMatch scope span subject cases
    PyImport imports -> do
      emitInfo $
        "Ignoring Python import at " <> formatSpan span <> ": "
        <> T.intercalate ", " (map (describeImport . locValue) imports)
      return cppNoop
    PyGlobal names ->
      buildRuntimeFallback span ("Python 'global' statement (" <> renderIdentifiers names <> ") requires runtime fallback")
    PyNonlocal names ->
      buildRuntimeFallback span ("Python 'nonlocal' statement (" <> renderIdentifiers names <> ") requires runtime fallback")
    PyDel _ ->
      buildRuntimeFallback span "Python 'del' statement requires runtime fallback"
    PyAsyncWith _ _ ->
      buildRuntimeFallback span "Python 'async with' statement requires runtime fallback"
    PyAsyncFor _ _ _ _ ->
      buildRuntimeFallback span "Python 'async for' statement requires runtime fallback"
    PyAsyncFuncDef funcDef -> do
      generateAsyncFunctionFallback span funcDef
      return cppNoop
    PyRaise excExpr causeExpr ->
      generatePythonRaise span excExpr causeExpr
    PyYield _ ->
      buildRuntimeFallback span "Python 'yield' expression requires runtime fallback"
    PyYieldFrom _ ->
      buildRuntimeFallback span "Python 'yield from' expression requires runtime fallback"
    _ -> do
      let msg = "Python statement not implemented: " <> T.pack (show stmt)
      unsupportedStatement reportFatalNotImplemented span msg
  where
    runtimeFallbackMessage :: CppGenConfig -> SourceSpan -> Text -> CppCodeGen Text
    runtimeFallbackMessage config loc baseMessage = do
      let location = baseMessage <> " at " <> formatSpan loc
          message
            | not (cgcEnableInterop config) = location <> " (runtime fallback unavailable: interop runtime is disabled)"
            | otherwise = location <> " (runtime fallback)"
      if cgcStrictMode config
        then reportFatalNotImplemented message
        else emitWarning message
      pure message

    buildRuntimeFallback :: SourceSpan -> Text -> CppCodeGen CppStmt
    buildRuntimeFallback loc baseMessage = do
      config <- gets cgsConfig
      message <- runtimeFallbackMessage config loc baseMessage
      let interopAvailable = cgcEnableInterop config
          strict = cgcStrictMode config
      fallbackStmt <- if strict || not interopAvailable
        then runtimeAbortStmt message
        else runtimeFallbackStmt message
      pure $ CppStmtSeq
        [ CppComment ("runtime fallback: " <> baseMessage)
        , fallbackStmt
        ]

    unsupportedStatement :: (Text -> CppCodeGen ()) -> SourceSpan -> Text -> CppCodeGen CppStmt
    unsupportedStatement reporter loc baseMessage = do
      let message = baseMessage <> " at " <> formatSpan loc
      reporter message
      abortStmt <- runtimeAbortStmt message
      pure $ CppStmtSeq
        [ CppComment ("unsupported: " <> baseMessage)
        , abortStmt
        ]

    generateAsyncFunctionFallback :: SourceSpan -> PythonFuncDef -> CppCodeGen ()
    generateAsyncFunctionFallback loc funcDef = do
      let funcName = (\(Identifier n) -> n) (pyFuncName funcDef)
          baseMessage = "Python async function '" <> funcName <> "' requires runtime fallback"
      cppParams <- mapM mapPythonParameter (pyFuncParams funcDef)
      returnType <- case pyFuncReturns funcDef of
        Just typeExpr ->
          if isNoneTypeExpr (locValue typeExpr)
            then pure CppVoid
            else mapPythonType typeExpr
        Nothing -> pure CppAuto
      fallbackStmt <- buildRuntimeFallback loc baseMessage
      let body = [fallbackStmt]
      addDeclaration $ CppFunction funcName returnType cppParams body
      emitInfo $ "Generated fallback stub for async function " <> funcName

    generatePythonMatch :: PythonScope -> SourceSpan -> Located PythonExpr -> [Located PythonCase] -> CppCodeGen CppStmt
    generatePythonMatch scope span subject cases
      | null cases =
          buildRuntimeFallback span "match statement requires at least one case clause"
      | otherwise = do
          subjectVar <- generateTempVar
          subjectExpr <- generatePythonExpr subject
          loweredCases <- mapM (lowerMatchCase subjectVar) cases
          case sequence loweredCases of
            Left reason -> buildRuntimeFallback span reason
            Right lowered -> do
              let matchStmt = buildCaseChain lowered
              pure $ CppStmtSeq
                [ CppDecl (CppVariable subjectVar CppAuto (Just subjectExpr))
                , matchStmt
                ]

    lowerMatchCase :: Text -> Located PythonCase -> CppCodeGen (Either Text LoweredCase)
    lowerMatchCase subjectVar (Located _ caseNode) = do
      patternResult <- lowerPattern subjectVar (pyCasePattern caseNode)
      case patternResult of
        Left err -> pure $ Left err
        Right lowering -> do
          (guardExpr, bodyStmts) <- withPatternBindings (mlBindings lowering) $ do
            guardExpr <- traverse generatePythonExpr (pyCaseGuard caseNode)
            body <- mapM (generatePythonStmt scope) (pyCaseBody caseNode)
            pure (guardExpr, body)
          let bindingStmts = map pbStmt (mlBindings lowering)
              guardedBody =
                case guardExpr of
                  Nothing -> bindingStmts ++ bodyStmts
                  Just guardCond -> bindingStmts ++ [CppIf guardCond bodyStmts []]
          pure $ Right (mlCondition lowering, guardedBody)

    buildCaseChain :: [LoweredCase] -> CppStmt
    buildCaseChain [] = cppNoop
    buildCaseChain ((cond, body):rest) =
      let elseBranch = case rest of
            [] -> []
            _ -> [buildCaseChain rest]
      in CppIf cond body elseBranch

    lowerPattern :: Text -> Located PythonPattern -> CppCodeGen (Either Text MatchLowering)
    lowerPattern subjectVar locatedPattern =
      case locValue locatedPattern of
        PatWildcard ->
          pure $ Right $ MatchLowering (CppLiteral (CppBoolLit True)) []
        PatVar (Identifier name) ->
          pure $ Right $ MatchLowering (CppLiteral (CppBoolLit True)) [bindingFor name]
        PatLiteral lit ->
          pure $ Right $ MatchLowering (CppBinary "==" subjectExpr (CppLiteral (mapPythonLiteral lit))) []
        PatValue expr -> do
          valueExpr <- generatePythonExpr expr
          pure $ Right $ MatchLowering (CppBinary "==" subjectExpr valueExpr) []
        PatAs inner (Identifier aliasName) -> do
          lowered <- lowerPattern subjectVar inner
          case lowered of
            Left err -> pure $ Left err
            Right res ->
              let aliasBinding = bindingFor aliasName
              in pure $ Right res { mlBindings = mlBindings res ++ [aliasBinding] }
        PatOr patterns -> do
          lowered <- traverse (lowerPattern subjectVar) (NE.toList patterns)
          case sequence lowered of
            Left err -> pure $ Left err
            Right options ->
              if any (not . null . mlBindings) options
                then pure $ Left (unsupportedPattern "alternatives that bind names are not supported")
                else
                  let condExprs = map mlCondition options
                      combinedCond = foldl1 (\acc expr -> CppBinary "||" acc expr) condExprs
                  in pure $ Right $ MatchLowering combinedCond []
        _ ->
          pure $ Left (unsupportedPattern "is not supported")
      where
        subjectExpr = CppVar subjectVar
        bindingFor name =
          PatternBinding
            { pbName = name
            , pbType = CppAuto
            , pbStmt = CppDecl (CppVariable name CppAuto (Just subjectExpr))
            }
        unsupportedPattern detail =
          "match pattern '" <> describePattern (locValue locatedPattern) <> "' at "
          <> formatSpan (locSpan locatedPattern) <> " " <> detail

    withPatternBindings :: [PatternBinding] -> CppCodeGen a -> CppCodeGen a
    withPatternBindings [] action = action
    withPatternBindings bindings action = do
      modify $ \s ->
        let table = cgsSymbolTable s
            updated = foldl' (\acc binding -> HM.insert (pbName binding) (pbType binding) acc) table bindings
        in s { cgsSymbolTable = updated }
      result <- action
      modify $ \s ->
        let table = cgsSymbolTable s
            cleaned = foldl' (flip HM.delete) table (map pbName bindings)
        in s { cgsSymbolTable = cleaned }
      pure result

    handleSimpleAssignment :: PythonScope -> Text -> Located PythonExpr -> CppCodeGen CppStmt
    handleSimpleAssignment scope' varName locatedExpr = do
      symtab <- gets cgsSymbolTable
      case locValue locatedExpr of
        PyList _ ->
          handleListAssignment scope' symtab varName locatedExpr
        _ ->
          handleRegularAssignment scope' symtab varName locatedExpr

    handleListAssignment :: PythonScope -> HashMap Text CppType -> Text -> Located PythonExpr -> CppCodeGen CppStmt
    handleListAssignment scope' symtab varName listExpr = do
      emitInfo $ "handleListAssignment for " <> varName <> " with expression type: " <> T.pack (show (locValue listExpr))
      (vectorType, vectorExpr) <- generatePythonListLiteral listExpr
      let updateSymbolTable = modify $ \s -> s { cgsSymbolTable = HM.insert varName vectorType (cgsSymbolTable s) }
          assignmentExpr = CppExprStmt (CppBinary "=" (CppVar varName) vectorExpr)
          declarationStmt = CppDecl (CppVariable varName vectorType (Just vectorExpr))
      case scope' of
        ScopeModule ->
          if HM.member varName symtab
            then do
              updateSymbolTable
              return assignmentExpr
            else do
              updateSymbolTable
              recordHoistedGlobal varName
              addDeclaration $ CppVariable varName vectorType (Just vectorExpr)
              emitInfo $ "Initialized module-level list " <> varName
              return cppNoop
        ScopeFunction ->
          if HM.member varName symtab
            then do
              updateSymbolTable
              return assignmentExpr
            else do
              updateSymbolTable
              return declarationStmt

    handleRegularAssignment :: PythonScope -> HashMap Text CppType -> Text -> Located PythonExpr -> CppCodeGen CppStmt
    handleRegularAssignment scope' symtab varName locatedExpr = do
      emitInfo $ "handleRegularAssignment for " <> varName <> " with expression type: " <> T.pack (show (locValue locatedExpr))
      cppExpr <- generatePythonExpr locatedExpr
      let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated locatedExpr)
      refinedType <- do
        t <- refinePythonExprType ("assignment to " <> varName) locatedExpr defaultType
        emitInfo $ "refinedType for " <> varName <> " is " <> T.pack (show t)
        pure t
      let updateSymbolTableWith t = modify $ \s -> s { cgsSymbolTable = HM.insert varName t (cgsSymbolTable s) }
          assignmentStmt = CppExprStmt (CppBinary "=" (CppVar varName) cppExpr)
          declarationStmt t = CppDecl (CppVariable varName t (Just cppExpr))
      case scope' of
        ScopeModule ->
          if HM.member varName symtab
            then do
              updateSymbolTableWith refinedType
              return assignmentStmt
            else do
              updateSymbolTableWith refinedType
              recordHoistedGlobal varName
              addDeclaration $ CppVariable varName refinedType (Just cppExpr)
              emitInfo $ "Initialized module-level variable " <> varName
              return cppNoop
        ScopeFunction ->
          if HM.member varName symtab
            then do
              updateSymbolTableWith refinedType
              return assignmentStmt
            else do
              updateSymbolTableWith refinedType
              return $ declarationStmt refinedType

    handleAugmentedAssignment :: PythonScope -> SourceSpan -> Located PythonPattern -> BinaryOp -> Located PythonExpr -> CppCodeGen CppStmt
    handleAugmentedAssignment _ loc target op valueExpr =
      case locValue target of
        PatVar (Identifier name) -> do
          symtab <- gets cgsSymbolTable
          if HM.member name symtab
            then buildAugmentedAssignmentStmt loc name op valueExpr
            else unsupportedStatement reportFatalUnsupported loc
                  ("Augmented assignment to '" <> name <> "' references an undefined name")
        _ ->
          unsupportedStatement reportFatalUnsupported loc
            "Augmented assignment targets other than simple names are not supported"

    buildAugmentedAssignmentStmt :: SourceSpan -> Text -> BinaryOp -> Located PythonExpr -> CppCodeGen CppStmt
    buildAugmentedAssignmentStmt loc name op valueExpr = do
      cppValue <- generatePythonExpr valueExpr
      let targetExpr = CppVar name
      case op of
        OpPow -> do
          addInclude "<cmath>"
          let powCall = CppCall (CppVar "std::pow") [targetExpr, cppValue]
          pure $ CppExprStmt (CppBinary "=" targetExpr powCall)
        OpDiv -> do
          let numerator = promoteToTrueDivOperand targetExpr
              denominator = promoteToTrueDivOperand cppValue
              division = CppBinary "/" numerator denominator
          pure $ CppExprStmt (CppBinary "=" targetExpr division)
        OpFloorDiv -> do
          addInclude "<cmath>"
          let numerator = promoteToTrueDivOperand targetExpr
              denominator = promoteToTrueDivOperand cppValue
              division = CppBinary "/" numerator denominator
              floored = CppCall (CppVar "std::floor") [division]
          pure $ CppExprStmt (CppBinary "=" targetExpr floored)
        _ ->
          case mapAugmentedAssignmentOp op of
            Just compound ->
              pure $ CppExprStmt (CppBinary compound targetExpr cppValue)
            Nothing ->
              unsupportedStatement reportFatalUnsupported loc
                ("Augmented assignment using operator '" <> T.pack (show op) <> "' is not supported")

    handleChainedAssignment :: PythonScope -> SourceSpan -> [Located PythonPattern] -> Located PythonExpr -> CppCodeGen CppStmt
    handleChainedAssignment scope' loc targetPatterns valueExpr = do
      case traverse extractVarName targetPatterns of
        Nothing ->
          unsupportedStatement reportNotImplemented loc "Chained assignment targets must be simple names"
        Just [] -> pure cppNoop
        Just names -> do
          cppValue <- generatePythonExpr valueExpr
          let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated valueExpr)
              contextLabel = case reverse names of
                (lastName:_) -> "assignment to " <> lastName <> " (chained)"
                [] -> "chained assignment"
          refinedType <- refinePythonExprType contextLabel valueExpr defaultType
          case scope' of
            ScopeModule -> do
              stmts <- generateModuleChainAssignments names refinedType cppValue
              pure $ finalizeChainStatements stmts
            ScopeFunction -> do
              stmts <- generateFunctionChainAssignments names refinedType cppValue
              pure $ finalizeChainStatements stmts

    generateModuleChainAssignments :: [Text] -> CppType -> CppExpr -> CppCodeGen [CppStmt]
    generateModuleChainAssignments names refinedType initialExpr =
      go initialExpr (reverse names) []
      where
        go _ [] acc = pure (reverse acc)
        go current (name:rest) acc = do
          symtab <- gets cgsSymbolTable
          let varExpr = CppVar name
          if HM.member name symtab
            then do
              let stmt = CppExprStmt (CppBinary "=" varExpr current)
              go varExpr rest (stmt:acc)
            else do
              modify $ \s -> s { cgsSymbolTable = HM.insert name refinedType (cgsSymbolTable s) }
              recordHoistedGlobal name
              addDeclaration $ CppVariable name refinedType (Just current)
              go varExpr rest acc

    generateFunctionChainAssignments :: [Text] -> CppType -> CppExpr -> CppCodeGen [CppStmt]
    generateFunctionChainAssignments names refinedType initialExpr =
      go initialExpr (reverse names) []
      where
        go _ [] acc = pure (reverse acc)
        go current (name:rest) acc = do
          symtab <- gets cgsSymbolTable
          let varExpr = CppVar name
          if HM.member name symtab
            then do
              let stmt = CppExprStmt (CppBinary "=" varExpr current)
              go varExpr rest (stmt:acc)
            else do
              modify $ \s -> s { cgsSymbolTable = HM.insert name refinedType (cgsSymbolTable s) }
              let decl = CppDecl (CppVariable name refinedType (Just current))
              go varExpr rest (decl:acc)

    finalizeChainStatements :: [CppStmt] -> CppStmt
    finalizeChainStatements [] = cppNoop
    finalizeChainStatements [single] = single
    finalizeChainStatements stmts = CppStmtSeq stmts

    extractVarName :: Located PythonPattern -> Maybe Text
    extractVarName (Located _ (PatVar (Identifier name))) = Just name
    extractVarName _ = Nothing

    handleAnnotatedAssignment :: PythonScope -> SourceSpan -> Located PythonPattern -> Located PythonTypeExpr -> Maybe (Located PythonExpr) -> CppCodeGen CppStmt
    handleAnnotatedAssignment scope' loc target typeExpr mValue =
      case locValue target of
        PatVar (Identifier name) -> do
          cppType <- mapPythonType typeExpr
          mCppValue <- traverse generatePythonExpr mValue
          symtab <- gets cgsSymbolTable
          let alreadyDeclared = HM.member name symtab
          modify $ \s -> s { cgsSymbolTable = HM.insert name cppType (cgsSymbolTable s) }
          case scope' of
            ScopeModule ->
              if alreadyDeclared
                then case mCppValue of
                  Just expr -> pure $ CppExprStmt (CppBinary "=" (CppVar name) expr)
                  Nothing -> pure cppNoop
                else do
                  recordHoistedGlobal name
                  addDeclaration $ CppVariable name cppType mCppValue
                  emitInfo $ "Declared annotated module-level variable " <> name
                  pure cppNoop
            ScopeFunction ->
              if alreadyDeclared
                then case mCppValue of
                  Just expr -> pure $ CppExprStmt (CppBinary "=" (CppVar name) expr)
                  Nothing -> pure cppNoop
                else pure $ CppDecl (CppVariable name cppType mCppValue)
        _ -> do
          let message = "Annotated assignment target is not supported: " <> T.pack (show (locValue target))
          unsupportedStatement reportNotImplemented loc message

    handleTupleUnpacking :: PythonScope -> [Located PythonPattern] -> Located PythonExpr -> CppCodeGen CppStmt
    handleTupleUnpacking scope' patterns locatedExpr = do
      addInclude "<tuple>"
      cppExpr <- generatePythonExpr locatedExpr
      symtab <- gets cgsSymbolTable
      
      -- Extract variable names from patterns
      let extractVarName (Located _ (PatVar (Identifier name))) = Just name
          extractVarName _ = Nothing
          varNames = catMaybes (map extractVarName patterns)
      
      -- Check if all patterns are simple variables
      if length varNames /= length patterns
        then do
          let msg = "Complex tuple unpacking patterns not supported"
          reportNotImplemented msg
          strict <- gets (cgcStrictMode . cgsConfig)
          if strict
            then reportFatalNotImplemented msg >> return cppNoop
            else emitWarning msg >> return cppNoop
        else do
          -- Generate tuple destructuring using std::tie
          let tieExpr = CppCall (CppVar "std::tie") (map CppVar varNames)
              updateSymbolTables = mapM_ (\name -> modify $ \s -> 
                s { cgsSymbolTable = HM.insert name CppAuto (cgsSymbolTable s) }) varNames
          
          case scope' of
            ScopeModule -> do
              -- For module scope, we need to declare and then assign
              let alreadyDeclared = filter (`HM.member` symtab) varNames
                  needsDeclaration = filter (`notElem` alreadyDeclared) varNames
              
              -- Declare undeclared variables
              mapM_ (\name -> do
                modify $ \s -> s { cgsSymbolTable = HM.insert name CppAuto (cgsSymbolTable s) }
                recordHoistedGlobal name
                addDeclaration $ CppVariable name CppAuto Nothing
                emitInfo $ "Declared module-level variable " <> name <> " for tuple unpacking"
                ) needsDeclaration
              
              -- Generate assignment statement
              return $ CppExprStmt (CppBinary "=" tieExpr cppExpr)
              
            ScopeFunction -> do
              let alreadyDeclared = filter (`HM.member` symtab) varNames
                  needsDeclaration = filter (`notElem` alreadyDeclared) varNames
              
              updateSymbolTables
              
              if null needsDeclaration
                then 
                  -- All variables already declared, just assign
                  return $ CppExprStmt (CppBinary "=" tieExpr cppExpr)
                else
                  -- Need to declare some variables first
                  -- Use auto declarations followed by tie assignment
                  let declarations = map (\name -> CppDecl (CppVariable name CppAuto Nothing)) needsDeclaration
                      assignment = CppExprStmt (CppBinary "=" tieExpr cppExpr)
                  in return $ CppStmtSeq (declarations ++ [assignment])

    parseRangeArgs :: [Located PythonArgument] -> CppCodeGen (Maybe RangeSpec)
    parseRangeArgs args = case traverse extractPositional args of
      Nothing -> return Nothing
      Just posArgs -> case posArgs of
        [endExpr] -> do
          end <- generatePythonExpr endExpr
          let start = CppLiteral (CppIntLit 0)
              step = CppLiteral (CppIntLit 1)
          return $ Just RangeSpec
            { rsStart = start
            , rsEnd = end
            , rsStep = step
            , rsStepValue = Just 1
            }
        [startExpr, endExpr] -> do
          start <- generatePythonExpr startExpr
          end <- generatePythonExpr endExpr
          let step = CppLiteral (CppIntLit 1)
          return $ Just RangeSpec
            { rsStart = start
            , rsEnd = end
            , rsStep = step
            , rsStepValue = Just 1
            }
        [startExpr, endExpr, stepExpr] -> do
          start <- generatePythonExpr startExpr
          end <- generatePythonExpr endExpr
          stepRaw <- generatePythonExpr stepExpr
          let (stepNorm, stepVal) = normalizeStepExpr stepRaw
          return $ Just RangeSpec
            { rsStart = start
            , rsEnd = end
            , rsStep = stepNorm
            , rsStepValue = stepVal
            }
        _ -> return Nothing

    extractPositional :: Located PythonArgument -> Maybe (Located PythonExpr)
    extractPositional (Located _ arg) = case arg of
      ArgPositional expr -> Just expr
      _ -> Nothing

    normalizeStepExpr :: CppExpr -> (CppExpr, Maybe Integer)
    normalizeStepExpr expr = case expr of
      CppLiteral (CppIntLit n) -> (CppLiteral (CppIntLit n), Just n)
      CppUnary "-" inner -> case inner of
        CppLiteral (CppIntLit n) -> (CppLiteral (CppIntLit (-n)), Just (-n))
        _ -> (expr, Nothing)
      _ -> (expr, Nothing)

    buildRangeLoop :: Bool -> Text -> [CppStmt] -> RangeSpec -> CppCodeGen CppStmt
    buildRangeLoop varAlreadyDeclared varName cppBody (RangeSpec startExpr endExpr stepExpr stepVal) = do
      iterVarName <- generateTempVar
      let iterVar = CppVar iterVarName
          pythonVar = CppVar varName
          assignmentStmt = CppExprStmt (CppBinary "=" pythonVar iterVar)
          loopBody = assignmentStmt : cppBody
          prefixDecls = if varAlreadyDeclared
                        then []
                        else [CppDecl (CppVariable varName CppAuto (Just startExpr))]
          iterInitExpr = if varAlreadyDeclared then startExpr else pythonVar
      case stepVal of
        Just 0 -> do
          reportInternalError "range() with step 0 is invalid"
          return cppNoop
        Just n -> do
          let condition = if n > 0
                            then CppBinary "<" iterVar endExpr
                            else CppBinary ">" iterVar endExpr
              increment
                | n == 1 = CppUnary "++" iterVar
                | n == -1 = CppUnary "--" iterVar
                | otherwise = CppBinary "+=" iterVar (CppLiteral (CppIntLit n))
              iterInit = CppDecl (CppVariable iterVarName CppAuto (Just iterInitExpr))
              loopStmt = CppFor (Just iterInit) (Just condition) (Just increment) loopBody
              stmts = prefixDecls ++ [loopStmt]
          return $ case stmts of
            [single] -> single
            _ -> CppStmtSeq stmts
        Nothing -> do
          stepVarName <- generateTempVar
          let stepVar = CppVar stepVarName
              stepDecl = CppDecl (CppVariable stepVarName CppAuto (Just stepExpr))
              iterInit = CppDecl (CppVariable iterVarName CppAuto (Just iterInitExpr))
              positiveCond = CppBinary "&&"
                               (CppBinary ">" stepVar (CppLiteral (CppIntLit 0)))
                               (CppBinary "<" iterVar endExpr)
              negativeCond = CppBinary "&&"
                               (CppBinary "<" stepVar (CppLiteral (CppIntLit 0)))
                               (CppBinary ">" iterVar endExpr)
              condition = CppBinary "||" positiveCond negativeCond
              increment = CppBinary "+=" iterVar stepVar
              loopStmt = CppFor (Just iterInit) (Just condition) (Just increment) loopBody
              scopedLoop = CppBlock [stepDecl, loopStmt]
              stmts = prefixDecls ++ [scopedLoop]
          return $ case stmts of
            [single] -> single
            _ -> CppStmtSeq stmts

    buildIterableLoop :: SourceSpan -> Text -> Bool -> IterableInfo -> [CppStmt] -> CppCodeGen CppStmt
    buildIterableLoop loc varName varAlreadyDeclared info cppBody = do
      symtab <- gets cgsSymbolTable
      let existingType = HM.lookup varName symtab >>= knownVarType
          candidateType =
            if varAlreadyDeclared
              then maybe (iiElementType info) Just existingType
              else iiElementType info
      case candidateType of
        Nothing ->
          buildRuntimeFallback loc ("Python 'for' iteration over " <> iiDescription info <> " requires a known element type")
        Just targetType -> do
          when (not varAlreadyDeclared) $
            modify $ \s -> s { cgsSymbolTable = HM.insert varName targetType (cgsSymbolTable s) }
          let prefixDecls =
                if varAlreadyDeclared
                  then []
                  else [CppDecl (CppVariable varName targetType (Just (CppBracedInit targetType [])))]
          loopVarName <- generateTempVar
          let assignmentStmt = CppExprStmt (CppBinary "=" (CppVar varName) (CppVar loopVarName))
              loopBody = assignmentStmt : cppBody
              loopStmt = CppForRange loopVarName (iiExpr info) loopBody
          pure $ case prefixDecls of
            [] -> loopStmt
            _  -> CppStmtSeq (prefixDecls ++ [loopStmt])

    knownVarType :: CppType -> Maybe CppType
    knownVarType ty =
      case stripTypeQualifiers ty of
        CppAuto -> Nothing
        _ -> Just ty

    resolveIterableInfo :: Located PythonExpr -> CppCodeGen IterableInfo
    resolveIterableInfo located =
      case locValue located of
        PyList _ -> do
          (vectorType, vectorExpr) <- generatePythonListLiteral located
          pure $
            IterableInfo
              { iiExpr = vectorExpr
              , iiElementType = extractListElementType vectorType
              , iiDescription = "list literal"
              }
        PySet elems -> do
          (setType, setExpr) <- generatePythonSetLiteral elems
          pure $
            IterableInfo
              { iiExpr = setExpr
              , iiElementType = extractSetElementType setType
              , iiDescription = "set literal"
              }
        PyVar (Identifier name) -> do
          symtab <- gets cgsSymbolTable
          let mType = HM.lookup name symtab >>= deriveElementTypeFromCppType
          pure $ IterableInfo (CppVar name) mType ("variable '" <> name <> "'")
        _ -> do
          expr <- generatePythonExpr located
          let containerType = inferPythonExprCppTypeLocated located
              elementType = containerType >>= deriveElementTypeFromCppType
          pure $
            IterableInfo
              { iiExpr = expr
              , iiElementType = elementType
              , iiDescription = describeIterableSource located
              }

    extractListElementType :: CppType -> Maybe CppType
    extractListElementType ty =
      case stripTypeQualifiers ty of
        CppVector elemType -> normalizeElementType elemType
        _ -> Nothing

    extractSetElementType :: CppType -> Maybe CppType
    extractSetElementType ty =
      case stripTypeQualifiers ty of
        CppClassType name [elemType]
          | name == "std::set" || name == "std::unordered_set" ->
              normalizeElementType elemType
        _ -> Nothing

    normalizeElementType :: CppType -> Maybe CppType
    normalizeElementType ty =
      case stripTypeQualifiers ty of
        CppAuto -> Nothing
        other -> Just other

    deriveElementTypeFromCppType :: CppType -> Maybe CppType
    deriveElementTypeFromCppType ty =
      case stripTypeQualifiers ty of
        CppVector elemType ->
          normalizeElementType elemType
        CppClassType name params ->
          case (name, params) of
            ("std::vector", [elemType]) -> normalizeElementType elemType
            ("std::list", [elemType]) -> normalizeElementType elemType
            ("std::deque", [elemType]) -> normalizeElementType elemType
            ("std::set", [elemType]) -> normalizeElementType elemType
            ("std::unordered_set", [elemType]) -> normalizeElementType elemType
            ("std::basic_string", _) -> Just CppChar
            _ -> Nothing
        CppString -> Just CppChar
        _ -> Nothing

    describeIterableSource :: Located PythonExpr -> Text
    describeIterableSource (Located _ expr) =
      case expr of
        PyList _ -> "list literal"
        PySet _ -> "set literal"
        PyTuple _ -> "tuple literal"
        PyDict _ -> "dict literal"
        PyVar (Identifier name) -> "variable '" <> name <> "'"
        PyCall inner _ ->
          "call to " <> describeCall inner
        _ -> "expression '" <> T.pack (show expr) <> "'"
      where
        describeCall (Located _ callExpr) =
          case callExpr of
            PyVar (Identifier name) -> name
            PyAttribute _ (Identifier attr) -> attr
            _ -> T.pack (show callExpr)

    renderIdentifiers :: [Identifier] -> Text
    renderIdentifiers names = T.intercalate ", " (map identifierText names)

    describeImport :: PythonImport -> Text
    describeImport importStmt = case importStmt of
      ImportModule modName alias ->
        moduleNameText modName <> maybe "" (" as " <>) (fmap identifierText alias)
      ImportFrom modName items ->
        moduleNameText modName <> ": " <> T.intercalate ", " (map renderItem items)
      ImportFromStar modName ->
        moduleNameText modName <> ".*"
      where
        renderItem (ident, mAlias) =
          identifierText ident <> maybe "" (" as " <>) (fmap identifierText mAlias)

generatePythonRaise :: SourceSpan -> Maybe (Located PythonExpr) -> Maybe (Located PythonExpr) -> CppCodeGen CppStmt
generatePythonRaise _ Nothing _ = pure (CppThrow Nothing)
generatePythonRaise _ (Just excExpr) mFrom = do
  addInclude "<stdexcept>"
  let normalizedCause =
        case mFrom of
          Just cause | isNoneExpr cause -> Nothing
          other -> other
  case extractExceptionDetails excExpr of
    Just (typeName, mMessageExpr) -> do
      messageText <- traverse renderExprAsString mMessageExpr
      causeSummary <- traverse renderExceptionSummary normalizedCause
      finalMessage <- buildExceptionMessage typeName messageText causeSummary
      pure (CppThrow (Just (CppCall (CppVar "std::runtime_error") [finalMessage])))
    Nothing -> do
      cppExpr <- generatePythonExpr excExpr
      pure (CppThrow (Just cppExpr))

generatePythonAssert :: SourceSpan -> Located PythonExpr -> Maybe (Located PythonExpr) -> CppCodeGen CppStmt
generatePythonAssert span condition mMessage = do
  cppCond <- generatePythonExpr condition
  let baseMessage = "Python assertion failed at " <> formatSpan span
      fullMessage = maybe baseMessage (\expr -> baseMessage <> ": " <> T.pack (show (locValue expr))) mMessage
  failureStmt <- runtimeAbortStmt fullMessage
  pure $ CppIf (CppUnary "!" cppCond) [failureStmt] []

extractExceptionDetails :: Located PythonExpr -> Maybe (Text, Maybe (Located PythonExpr))
extractExceptionDetails located@(Located _ expr) =
  case expr of
    PyCall callee args -> do
      typeName <- exceptionTypeName callee
      pure (typeName, firstPositional args)
    _ ->
      case exceptionTypeName located of
        Just typeName -> Just (typeName, Nothing)
        Nothing -> Nothing
  where
    firstPositional :: [Located PythonArgument] -> Maybe (Located PythonExpr)
    firstPositional [] = Nothing
    firstPositional (Located _ argument : rest) =
      case argument of
        ArgPositional inner -> Just inner
        _ -> firstPositional rest

renderExceptionSummary :: Located PythonExpr -> CppCodeGen CppExpr
renderExceptionSummary expr =
  case extractExceptionDetails expr of
    Just (typeName, mMessage) -> do
      messageText <- traverse renderExprAsString mMessage
      buildExceptionMessage typeName messageText Nothing
    Nothing ->
      renderExprAsString expr

buildExceptionMessage :: Text -> Maybe CppExpr -> Maybe CppExpr -> CppCodeGen CppExpr
buildExceptionMessage typeName mMessage mCause = do
  addInclude "<sstream>"
  addInclude "<string>"
  bufferName <- generateTempVar
  let bufferVar = CppVar bufferName
      bufferDecl = CppDecl (CppVariable bufferName (CppClassType "std::ostringstream" []) Nothing)
      streamLiteral txt = CppExprStmt (CppBinary "<<" bufferVar (CppLiteral (CppStringLit txt)))
      streamExpr expr = CppExprStmt (CppBinary "<<" bufferVar expr)
      baseStmts = [bufferDecl, streamLiteral typeName]
      messageStmts = maybe [] (\msg -> [streamLiteral ": ", streamExpr msg]) mMessage
      causeStmts = maybe [] (\cause -> [streamLiteral " (caused by ", streamExpr cause, streamLiteral ")"]) mCause
      finalStmt = CppReturn (Just (CppCall (CppMember bufferVar "str") []))
  pure (CppCall (CppLambda [] (baseStmts ++ messageStmts ++ causeStmts ++ [finalStmt])) [])

renderExprAsString :: Located PythonExpr -> CppCodeGen CppExpr
renderExprAsString locatedExpr = do
  addInclude "<string>"
  cppExpr <- generatePythonExpr locatedExpr
  let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated locatedExpr)
  refinedType <- refinePythonExprType "exception message" locatedExpr defaultType
  let coreType = stripTypeQualifiers refinedType
  case coreType of
    CppString ->
      pure (CppCall (CppVar "std::string") [cppExpr])
    CppBool ->
      pure (wrapBoolForPrint cppExpr)
    CppPointer inner
      | stripTypeQualifiers inner == CppChar ->
          pure (CppCall (CppVar "std::string") [cppExpr])
    _ | isFloatingType coreType ->
          pure (wrapFloatForPrint cppExpr)
      | isNumericType coreType -> do
          addInclude "<string>"
          pure (CppCall (CppVar "std::to_string") [cppExpr])
      | otherwise ->
          toStringViaStream cppExpr
  where
    isFloatingType ty =
      case ty of
        CppConst inner -> isFloatingType inner
        CppReference inner -> isFloatingType inner
        CppRvalueRef inner -> isFloatingType inner
        CppVolatile inner -> isFloatingType inner
        _ -> isFloatingCppType ty

    toStringViaStream expr = do
      addInclude "<sstream>"
      tempName <- generateTempVar
      let ossVar = CppVar tempName
          ossDecl = CppDecl (CppVariable tempName (CppClassType "std::ostringstream" []) Nothing)
          streamStmt = CppExprStmt (CppBinary "<<" ossVar expr)
          returnStmt = CppReturn (Just (CppCall (CppMember ossVar "str") []))
      pure (CppCall (CppLambda [] [ossDecl, streamStmt, returnStmt]) [])

stripTypeQualifiers :: CppType -> CppType
stripTypeQualifiers ty =
  case ty of
    CppConst inner -> stripTypeQualifiers inner
    CppReference inner -> stripTypeQualifiers inner
    CppRvalueRef inner -> stripTypeQualifiers inner
    CppVolatile inner -> stripTypeQualifiers inner
    _ -> ty

isNoneExpr :: Located PythonExpr -> Bool
isNoneExpr (Located _ expr) =
  case expr of
    PyConst (QualifiedName _ (Identifier name)) -> name == "None"
    PyLiteral PyNone -> True
    _ -> False

generatePythonWith :: PythonScope -> SourceSpan -> [Located PythonWithItem] -> [Located PythonStmt] -> CppCodeGen CppStmt
generatePythonWith scope _ items body = do
  stmtSeq <- lowerWith items
  case stmtSeq of
    [single] -> pure single
    _        -> pure (CppStmtSeq stmtSeq)
  where
    lowerWith [] = mapM (generatePythonStmt scope) body
    lowerWith (Located itemSpan item : rest) = do
      contextExpr <- generatePythonExpr (pyWithContext item)
      contextVar <- generateTempVar
      let contextDecl = CppDecl (CppVariable contextVar CppAuto (Just contextExpr))
      enterStmts <- bindWithTarget itemSpan (pyWithVar item) contextVar
      innerBody <- lowerWith rest
      guardVar <- generateTempVar
      ensureFinallyGuardHelper
      let exitCall =
            CppExprStmt
              (CppCall
                 (CppMember (CppVar contextVar) "__exit__")
                 [ CppLiteral CppNullPtr
                 , CppLiteral CppNullPtr
                 , CppLiteral CppNullPtr
                 ])
          guardLambda = CppLambda [] [exitCall]
          guardDecl =
            CppDecl
              (CppVariable guardVar finallyGuardType
                (Just (CppCall (CppVar finallyGuardStructName) [guardLambda])))
          guardBlock = CppBlock (guardDecl : innerBody)
      pure (contextDecl : enterStmts ++ [guardBlock])

finallyBlockSupportsRAII :: [CppStmt] -> Bool
finallyBlockSupportsRAII = all stmtSupported
  where
    stmtSupported :: CppStmt -> Bool
    stmtSupported (CppReturn Nothing) = True
    stmtSupported (CppReturn (Just _)) = False
    stmtSupported CppBreak = False
    stmtSupported CppContinue = False
    stmtSupported (CppIf _ thenStmts elseStmts) =
      all stmtSupported thenStmts && all stmtSupported elseStmts
    stmtSupported (CppWhile _ body) = all stmtSupported body
    stmtSupported (CppFor mInit _ _ body) =
      maybe True stmtSupported mInit && all stmtSupported body
    stmtSupported (CppForRange _ _ body) = all stmtSupported body
    stmtSupported (CppSwitch _ cases) = all caseSupported cases
    stmtSupported (CppTry tryStmts catches finallyStmts) =
      all stmtSupported tryStmts
        && all catchSupported catches
        && all stmtSupported finallyStmts
    stmtSupported (CppStmtSeq stmts) = all stmtSupported stmts
    stmtSupported (CppBlock stmts) = all stmtSupported stmts
    stmtSupported (CppDecl _) = True
    stmtSupported (CppExprStmt _) = True
    stmtSupported (CppThrow _) = True
    stmtSupported (CppComment _) = True

    caseSupported :: CppCase -> Bool
    caseSupported (CppCase _ stmts) = all stmtSupported stmts
    caseSupported (CppDefault stmts) = all stmtSupported stmts

    catchSupported :: CppCatch -> Bool
    catchSupported (CppCatch _ _ stmts) = all stmtSupported stmts

finallyUnsupportedFallback :: SourceSpan -> Text -> CppCodeGen CppStmt
finallyUnsupportedFallback span reason = do
  let baseMessage = "Python 'finally' block " <> reason
      message = baseMessage <> " at " <> formatSpan span <> " (runtime fallback)"
  reportFatalUnsupported message
  abortStmt <- runtimeAbortStmt message
  pure $ CppStmtSeq
    [ CppComment ("runtime fallback: " <> baseMessage)
    , abortStmt
    ]

bindWithTarget :: SourceSpan -> Maybe (Located PythonPattern) -> Text -> CppCodeGen [CppStmt]
bindWithTarget _ Nothing contextVar =
  pure [CppExprStmt (CppCall (CppMember (CppVar contextVar) "__enter__") [])]
bindWithTarget span (Just locatedPattern) contextVar = do
  let enterCall = CppCall (CppMember (CppVar contextVar) "__enter__") []
  case locValue locatedPattern of
    PatVar (Identifier name) -> do
      symtab <- gets cgsSymbolTable
      if HM.member name symtab
        then pure [CppExprStmt (CppBinary "=" (CppVar name) enterCall)]
        else do
          modify $ \s -> s { cgsSymbolTable = HM.insert name CppAuto (cgsSymbolTable s) }
          pure [CppDecl (CppVariable name CppAuto (Just enterCall))]
    _ -> do
      reportFatalUnsupported $
        "Python 'with' statement binding pattern at "
        <> formatSpan span <> " is not supported"
      pure [CppExprStmt enterCall]

generatePythonTry :: PythonScope -> SourceSpan -> [Located PythonStmt] -> [Located PythonExcept] -> [Located PythonStmt] -> [Located PythonStmt] -> CppCodeGen CppStmt
generatePythonTry scope span tryStmts excepts elseStmts finallyStmts = do
  tryBody <- mapM (generatePythonStmt scope) tryStmts
  catches <- mapM (generateExceptHandler scope) excepts
  elseBody <- mapM (generatePythonStmt scope) elseStmts
  finallyBody <- mapM (generatePythonStmt scope) finallyStmts
  (prefix, tryBody', finalBlock) <- prepareElseBlock elseBody tryBody finallyBody
  let baseTry = CppTry tryBody' catches []
      withPrefix node = case prefix of
        [] -> node
        _  -> CppStmtSeq (prefix ++ [node])
  case finalBlock of
    [] -> pure (withPrefix baseTry)
    _ | finallyBlockSupportsRAII finalBlock -> do
          ensureFinallyGuardHelper
          guardVar <- generateTempVar
          let guardLambda = CppLambda [] finalBlock
              guardDecl =
                CppDecl
                  (CppVariable guardVar finallyGuardType
                    (Just (CppCall (CppVar finallyGuardStructName) [guardLambda])))
              scoped = CppBlock [guardDecl, baseTry]
          pure (withPrefix scoped)
      | otherwise -> finallyUnsupportedFallback span "contains control flow that cannot be lowered to RAII"

prepareElseBlock :: [CppStmt] -> [CppStmt] -> [CppStmt] -> CppCodeGen ([CppStmt], [CppStmt], [CppStmt])
prepareElseBlock elseBody tryBody finallyBody
  | null elseBody = pure ([], tryBody, finallyBody)
  | otherwise = do
      successVar <- generateTempVar
      let successDecl =
            CppDecl (CppVariable successVar CppBool (Just (CppLiteral (CppBoolLit False))))
          markSuccess =
            CppExprStmt (CppBinary "=" (CppVar successVar) (CppLiteral (CppBoolLit True)))
          tryBody' = tryBody ++ [markSuccess]
          elseStmt = CppIf (CppVar successVar) elseBody []
      pure ([successDecl], tryBody', elseStmt : finallyBody)

generateExceptHandler :: PythonScope -> Located PythonExcept -> CppCodeGen CppCatch
generateExceptHandler scope (Located span except) = do
  catchType <- resolveExceptType span (pyExceptType except)
  (catchVar, restoreAction) <- case pyExceptName except of
    Just (Identifier name) -> do
      current <- gets cgsSymbolTable
      let previous = HM.lookup name current
      modify $ \s -> s { cgsSymbolTable = HM.insert name CppAuto (cgsSymbolTable s) }
      let restore table = case previous of
            Just ty -> HM.insert name ty table
            Nothing -> HM.delete name table
      pure (name, Just restore)
    Nothing -> do
      tempName <- generateTempVar
      pure (tempName, Nothing)
  bodyStmts <- mapM (generatePythonStmt scope) (pyExceptBody except)
  case restoreAction of
    Just restore -> modify $ \s -> s { cgsSymbolTable = restore (cgsSymbolTable s) }
    Nothing -> pure ()
  pure $ CppCatch catchType catchVar bodyStmts

resolveExceptType :: SourceSpan -> Maybe (Located PythonExpr) -> CppCodeGen CppType
resolveExceptType span mTypeExpr = do
  addInclude "<exception>"
  let defaultType = CppClassType "std::exception" []
  case mTypeExpr of
    Nothing -> pure defaultType
    Just locatedExpr ->
      case exceptionTypeName locatedExpr of
        Just name
          | name `elem` ["Exception", "builtins::Exception", "BaseException", "builtins::BaseException"] ->
              pure defaultType
          | otherwise ->
              pure (CppClassType name [])
        Nothing -> do
          emitWarning $
            "Unsupported exception type in 'except' at "
            <> formatSpan span <> "; defaulting to std::exception"
          pure defaultType

exceptionTypeName :: Located PythonExpr -> Maybe Text
exceptionTypeName (Located _ expr) =
  case expr of
    PyVar (Identifier name) -> Just name
    PyConst qn -> Just (qualifiedNameToCpp qn)
    PyAttribute base (Identifier attr) -> do
      prefix <- exceptionTypeName base
      pure (prefix <> "::" <> attr)
    _ -> Nothing

-- | Generate C++ from Python expressions
-- | Generate C++ expression from Python argument
generatePythonArgument :: Located PythonArgument -> CppCodeGen CppExpr
generatePythonArgument (Located span arg) = case arg of
  ArgPositional expr -> generatePythonExpr expr
  ArgKeyword name expr -> do
    emitWarning $ "Keyword argument '" <> (\(Identifier n) -> n) name <> 
      "' at " <> formatSpan span <> " is being treated as positional argument"
    generatePythonExpr expr
  ArgStarred expr -> do
    emitWarning $ "*args unpacking at " <> formatSpan span <> 
      " is not fully supported, treating as single argument"
    generatePythonExpr expr
  ArgKwStarred expr -> do
    emitWarning $ "**kwargs unpacking at " <> formatSpan span <> 
      " is not fully supported, treating as single argument"
    generatePythonExpr expr


generateFStringExpr :: [PythonFStringSegment] -> CppCodeGen CppExpr
generateFStringExpr segments = do
  compiled <- mapM compileSegment segments
  ossName <- generateTempVar
  addInclude "<sstream>"
  let ossVar = CppVar ossName
      ossType = CppClassType "std::ostringstream" []
      ossDecl = CppDecl (CppVariable ossName ossType Nothing)
      streamStmts = map (\expr -> CppExprStmt (CppBinary "<<" ossVar expr)) compiled
      resultExpr = CppCall (CppMember ossVar "str") []
      lambdaBody = ossDecl : streamStmts ++ [CppReturn (Just resultExpr)]
  pure $ CppCall (CppLambda [] lambdaBody) []
  where
    compileSegment (PythonFStringLiteral text) = pure $ CppLiteral (CppStringLit text)
    compileSegment (PythonFStringExpr expr) = generatePythonExpr expr


generateJoinedStringExpr :: SourceSpan -> [Located PythonExpr] -> CppCodeGen CppExpr
generateJoinedStringExpr span segments = do
  addInclude "<sstream>"
  addInclude "<string>"
  compiled <- mapM segmentToString segments
  builderName <- generateTempVar
  let builderVar = CppVar builderName
      builderType = CppClassType "std::ostringstream" []
      decl = CppDecl (CppVariable builderName builderType Nothing)
      streamStmt expr = CppExprStmt (CppBinary "<<" builderVar expr)
      body = decl : map streamStmt compiled ++ [CppReturn (Just (CppCall (CppMember builderVar "str") []))]
  emitInfo $ "Joined string at " <> formatSpan span <> " lowered via std::ostringstream"
  pure $ CppCall (CppLambda [] body) []
  where
    segmentToString located =
      case locValue located of
        PyLiteral (PyString text) -> pure (CppLiteral (CppStringLit text))
        PyFormatSpec inner -> renderExprAsString inner
        _ -> renderExprAsString located


generatePythonExpr :: Located PythonExpr -> CppCodeGen CppExpr
generatePythonExpr (Located span expr) = case expr of
  PyLiteral lit -> case lit of
    PyFString segments -> generateFStringExpr segments
    _ -> return $ CppLiteral $ mapPythonLiteral lit
  PyConst (QualifiedName _ (Identifier name)) -> case name of
    "True"  -> return $ CppLiteral (CppBoolLit True)
    "False" -> return $ CppLiteral (CppBoolLit False)
    "None"  -> return $ CppLiteral CppNullPtr
    _        -> return $ CppVar name
  PyVar (Identifier name) -> return $ CppVar name
  PyBinaryOp op left right -> do
    let leftDefault = fromMaybe CppAuto (inferPythonExprCppTypeLocated left)
        rightDefault = fromMaybe CppAuto (inferPythonExprCppTypeLocated right)
        leftContext = "left operand of '" <> renderBinaryOpLabel op <> "' at " <> formatSpan (locSpan left)
        rightContext = "right operand of '" <> renderBinaryOpLabel op <> "' at " <> formatSpan (locSpan right)
    leftType <- refinePythonExprType leftContext left leftDefault
    rightType <- refinePythonExprType rightContext right rightDefault
    cppLeft <- generatePythonExpr left
    cppRight <- generatePythonExpr right
    let leftInfo = (left, leftType, cppLeft)
        rightInfo = (right, rightType, cppRight)
    case op of
      OpPow -> do
        addInclude "<cmath>"
        return $ CppCall (CppVar "std::pow") [cppLeft, cppRight]
      OpDiv -> do
        let leftDiv = promoteToTrueDivOperand cppLeft
            rightDiv = promoteToTrueDivOperand cppRight
        return $ CppBinary "/" leftDiv rightDiv
      OpMul -> do
        stringResult <- lowerStringMultiplication leftInfo rightInfo
        case stringResult of
          Just lowered -> pure lowered
          Nothing -> pure (CppBinary "*" cppLeft cppRight)
      _ -> do
        let cppOp = mapPythonBinaryOp op
        return $ CppBinary cppOp cppLeft cppRight
  PyUnaryOp op inner -> do
    cppInner <- generatePythonExpr inner
    let uop = case op of
          OpNot      -> "!"
          OpNegate   -> "-"
          OpBitNot   -> "~"
          OpPositive -> "+"
    return $ CppUnary uop cppInner
  PyBoolOp op exprs -> do
    cppExprs <- mapM generatePythonExpr exprs
    let cppOp = case op of
          OpAnd -> "&&"
          OpOr  -> "||"
          _     -> "&&"
    return $ foldl1 (\acc e -> CppBinary cppOp acc e) cppExprs
  PyComparison ops exprs -> do
    -- Handle chained comparisons: a < b < c becomes (a < b) && (b < c)
    case (ops, exprs) of
      ([op], [left, right]) ->
        generateComparison op left right
      (ops', exprs') | length ops' + 1 == length exprs' -> do
        let lefts = init exprs'
            rights = tail exprs'
        comparisons <- sequence [generateComparison op left right | (op, left, right) <- zip3 ops' lefts rights]
        return $ foldl1 (\acc comp -> CppBinary "&&" acc comp) comparisons
      _ -> do
        reportInternalError "Invalid comparison expression"
        return $ CppLiteral $ CppBoolLit False
  PySubscript obj sliceExpr -> do
    cppObj <- generatePythonExpr obj
    case locValue sliceExpr of
      SliceIndex idx -> do
        cppIdx <- generatePythonExpr idx
        return $ CppIndex cppObj cppIdx
      _ ->
        generateSliceAccess obj cppObj sliceExpr
  PyAttribute obj (Identifier member) -> do
    cppObj <- generatePythonExpr obj
    return $ CppMember cppObj member
  PyCall func args ->
    case func of
      Located _ (PyVar (Identifier "print")) -> do
        addInclude "<iostream>"
        formattedArgs <- mapM formatPrintArgument args
        case formattedArgs of
          [] -> return $ CppBinary "<<" (CppVar "std::cout") (CppVar "std::endl")
          [arg] -> return $ CppBinary "<<" (CppBinary "<<" (CppVar "std::cout") arg) (CppVar "std::endl")
          manyArgs -> do
            let chainedOutput =
                  foldl
                    (\acc expr -> CppBinary "<<" (CppBinary "<<" acc expr) (CppLiteral (CppStringLit " ")))
                    (CppVar "std::cout")
                    (init manyArgs)
            return $ CppBinary "<<" (CppBinary "<<" chainedOutput (last manyArgs)) (CppVar "std::endl")
      Located _ (PyVar (Identifier "len")) -> do
        cppArgs <- mapM generatePythonArgument args
        case cppArgs of
          [arg0] -> return $ CppCall (CppMember arg0 "size") []
          _      -> return $ CppLiteral (CppIntLit 0)
      Located _ (PyVar (Identifier "range")) -> do
        cppArgs <- mapM generatePythonArgument args
        case cppArgs of
          [CppLiteral (CppIntLit n)] -> return $ CppCall (CppVar "range") [CppLiteral (CppIntLit n)]
          _ -> return $ CppCall (CppVar "range") cppArgs
      Located _ (PyVar (Identifier "runtime_execute")) -> do
        ensureRuntimeExecuteHelper
        cppFunc <- generatePythonExpr func
        cppArgs <- mapM generatePythonArgument args
        return $ CppCall cppFunc cppArgs
      _ -> do
        cppFunc <- generatePythonExpr func
        cppArgs <- mapM generatePythonArgument args
        return $ CppCall cppFunc cppArgs
  PyAsyncCall func args -> do
    emitWarning $ "'async' call at " <> formatSpan span <> " is lowered to a synchronous call"
    cppFunc <- generatePythonExpr func
    cppArgs <- mapM generatePythonArgument args
    return $ CppCall cppFunc cppArgs
  PyList _ -> do
    (_, vectorExpr) <- generatePythonListLiteral (Located span expr)
    return vectorExpr
  PyTuple exprs -> do
    generatePythonTupleLiteral exprs
  PySet exprs -> do
    (_, setExpr) <- generatePythonSetLiteral exprs
    return setExpr
  PyDict pairs -> do
    generatePythonDictLiteral pairs
  PyLambda params body -> do
    generatePythonLambda params body
  PyListComp element comprehensions -> do
    generatePythonListComprehension span element comprehensions
  PySetComp element comprehensions -> do
    emitWarning "PySetComp branch reached!"
    generatePythonSetComprehension span element comprehensions
  PyDictComp keyExpr valueExpr comprehensions -> do
    generatePythonDictComprehension span keyExpr valueExpr comprehensions
  PyGenComp element comprehensions -> do
    generatePythonGeneratorExpression span element comprehensions
  PyNamedExpr target valueExpr ->
    generateWalrusExpr span target valueExpr
  PyJoinedStr segments ->
    generateJoinedStringExpr span segments
  PyFormatSpec inner ->
    renderExprAsString inner
  PyStarred inner -> do
    emitWarning $ "Unpacking expression at " <> formatSpan span <> " is treated as a plain value"
    generatePythonExpr inner
  PyAwait awaitedExpr -> do
    emitWarning $ "Python 'await' expression at " <> formatSpan span <> " is executed synchronously"
    generatePythonExpr awaitedExpr
  PyIfExp testExpr thenExpr elseExpr -> do
    -- Python ternary: `thenExpr if testExpr else elseExpr`
    -- C++ ternary: `testExpr ? thenExpr : elseExpr`
    cppTest <- generatePythonExpr testExpr
    cppThen <- generatePythonExpr thenExpr
    cppElse <- generatePythonExpr elseExpr
    return $ CppConditional cppTest cppThen cppElse
  _ -> do
    let message = "TODO: Implement Python expression: " <> T.pack (show expr)
    reportNotImplemented message
    strict <- gets (cgcStrictMode . cgsConfig)
    if strict
      then do
        abortExpr <- runtimeAbortCall message
        return $ CppBinary "," abortExpr (CppLiteral (CppIntLit 0))
      else
        return $ CppLiteral $ CppIntLit 0
  where
    generateComparison op leftExpr rightExpr = case op of
      OpIn -> generateMembershipComparison leftExpr rightExpr False
      OpNotIn -> generateMembershipComparison leftExpr rightExpr True
      _ -> do
        cppLeft <- generatePythonExpr leftExpr
        cppRight <- generatePythonExpr rightExpr
        let cppOp = mapComparisonOp op
        return $ CppBinary cppOp cppLeft cppRight

    generateMembershipComparison leftExpr rightExpr isNegated = do
      let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated rightExpr)
          contextLabel = "membership haystack at " <> formatSpan (locSpan rightExpr)
      haystackType <- refinePythonExprType contextLabel rightExpr defaultType
      let classification = classifyMembershipTarget (stripTypeQualifiers haystackType)
      haystackExpr <- generatePythonExpr rightExpr
      case classification of
        Just MembershipStringCategory ->
          lowerStringMembership leftExpr haystackExpr
        Just MembershipSequenceCategory -> do
          needleExpr <- generatePythonExpr leftExpr
          lowerSequenceMembership needleExpr haystackExpr
        Just (MembershipMapCategory mapKind) -> do
          needleExpr <- generatePythonExpr leftExpr
          lowerMapMembership needleExpr haystackExpr mapKind
        Nothing -> do
          needleExpr <- generatePythonExpr leftExpr
          membershipFallback haystackType needleExpr haystackExpr
      where
        membershipFallback resolvedType needleValue haystackValue = do
          let operatorLabel = if isNegated then "not in" else "in"
              renderedType = case stripTypeQualifiers resolvedType of
                CppAuto -> "unknown type"
                actual -> renderCppType actual
              baseMessage = "Python membership operator '" <> operatorLabel
                            <> "' over " <> renderedType <> " requires runtime fallback"
              message = baseMessage <> " at " <> formatSpan span
          strict <- gets (cgcStrictMode . cgsConfig)
          action <- if strict
            then runtimeAbortStmt message
            else runtimeFallbackStmt message
          let resultLiteral = CppLiteral (CppBoolLit (if isNegated then True else False))
              lambdaBody =
                [ CppComment ("runtime fallback: membership comparison '" <> operatorLabel <> "'")
                , CppExprStmt needleValue
                , CppExprStmt haystackValue
                , action
                , CppReturn (Just resultLiteral)
                ]
          return $ CppCall (CppLambda [] lambdaBody) []

        lowerStringMembership needleSource haystackValue = do
          addInclude "<string>"
          haystackVar <- generateTempVar
          needleVar <- generateTempVar
          needleString <- renderExprAsString needleSource
          let haystackDecl = bindConstRef haystackVar haystackValue
              needleDecl = bindConstRef needleVar needleString
              findCall = CppCall (CppMember (CppVar haystackVar) "find") [CppVar needleVar]
              membership = CppBinary "!=" findCall (CppVar "std::string::npos")
          pure $ wrapLambda [haystackDecl, needleDecl] (applyNegation membership)

        lowerSequenceMembership needleValue haystackValue = do
          addInclude "<algorithm>"
          addInclude "<iterator>"
          haystackVar <- generateTempVar
          needleVar <- generateTempVar
          endVar <- generateTempVar
          let haystackDecl = bindConstRef haystackVar haystackValue
              needleDecl = bindConstRef needleVar needleValue
              endDecl = CppDecl (CppVariable endVar CppAuto (Just (CppCall (CppVar "std::end") [CppVar haystackVar])))
              beginCall = CppCall (CppVar "std::begin") [CppVar haystackVar]
              findCall = CppCall (CppVar "std::find") [beginCall, CppVar endVar, CppVar needleVar]
              membership = CppBinary "!=" findCall (CppVar endVar)
          pure $ wrapLambda [haystackDecl, needleDecl, endDecl] (applyNegation membership)

        lowerMapMembership needleValue haystackValue mapKind = do
          case mapKind of
            OrderedMap -> addInclude "<map>"
            UnorderedMap -> addInclude "<unordered_map>"
          haystackVar <- generateTempVar
          needleVar <- generateTempVar
          endVar <- generateTempVar
          let haystackDecl = bindConstRef haystackVar haystackValue
              needleDecl = bindConstRef needleVar needleValue
              endDecl = CppDecl (CppVariable endVar CppAuto (Just (CppCall (CppMember (CppVar haystackVar) "end") [])))
              findCall = CppCall (CppMember (CppVar haystackVar) "find") [CppVar needleVar]
              membership = CppBinary "!=" findCall (CppVar endVar)
          pure $ wrapLambda [haystackDecl, needleDecl, endDecl] (applyNegation membership)

        bindConstRef name expr =
          CppDecl (CppVariable name (CppConst (CppReference CppAuto)) (Just expr))

        wrapLambda bindings resultExpr =
          CppCall (CppLambda [] (bindings ++ [CppReturn (Just resultExpr)])) []

        applyNegation expr
          | isNegated = CppUnary "!" expr
          | otherwise = expr

        classifyMembershipTarget ty = case ty of
          CppString -> Just MembershipStringCategory
          CppClassType name _
            | name == "std::string" || name == "std::basic_string" -> Just MembershipStringCategory
          CppVector _ -> Just MembershipSequenceCategory
          CppStdArray _ _ -> Just MembershipSequenceCategory
          CppClassType name _
            | name `elem` ["std::vector", "std::list", "std::deque", "std::set", "std::unordered_set"] ->
                Just MembershipSequenceCategory
          CppMap _ _ -> Just (MembershipMapCategory OrderedMap)
          CppUnorderedMap _ _ -> Just (MembershipMapCategory UnorderedMap)
          CppClassType name _
            | name == "std::map" -> Just (MembershipMapCategory OrderedMap)
            | name == "std::unordered_map" -> Just (MembershipMapCategory UnorderedMap)
          _ -> Nothing

    renderBinaryOpLabel :: BinaryOp -> Text
    renderBinaryOpLabel = T.pack . show

    lowerStringMultiplication :: OperandInfo -> OperandInfo -> CppCodeGen (Maybe CppExpr)
    lowerStringMultiplication primary secondary = do
      firstAttempt <- attempt primary secondary
      case firstAttempt of
        Just expr -> pure (Just expr)
        Nothing -> attempt secondary primary
      where
        attempt (stringExpr, stringType, stringValue) (countExpr, countType, countValue)
          | isStringOperand stringExpr stringType
          , isIntegralOperand countExpr countType = do
              ensureStringRepeatHelper
              materialized <- materializeStringOperand stringExpr stringType stringValue
              let countArg = CppCast CppLongLong countValue
              pure $ Just (CppCall (CppVar stringRepeatHelperName) [materialized, countArg])
          | otherwise = pure Nothing

    materializeStringOperand :: Located PythonExpr -> CppType -> CppExpr -> CppCodeGen CppExpr
    materializeStringOperand locatedExpr inferredType valueExpr =
      case stripTypeQualifiers inferredType of
        ty | isStdStringType ty -> pure valueExpr
        CppPointer inner
          | isCharLikeType inner -> do
              addInclude "<string>"
              pure (CppCall (CppVar "std::string") [valueExpr])
        CppBool ->
          pure (wrapBoolForPrint valueExpr)
        ty | isFloatingCppType ty ->
          pure (wrapFloatForPrint valueExpr)
        ty | isIntegralCppType ty -> do
          addInclude "<string>"
          pure (CppCall (CppVar "std::to_string") [valueExpr])
        _ -> do
          addInclude "<sstream>"
          tempValue <- generateTempVar
          streamName <- generateTempVar
          let valueDecl = CppDecl (CppVariable tempValue CppAuto (Just valueExpr))
              streamDecl = CppDecl (CppVariable streamName (CppClassType "std::ostringstream" []) Nothing)
              streamStmt = CppExprStmt (CppBinary "<<" (CppVar streamName) (CppVar tempValue))
              returnStmt = CppReturn (Just (CppCall (CppMember (CppVar streamName) "str") []))
          pure $ CppCall (CppLambda [] [valueDecl, streamDecl, streamStmt, returnStmt]) []

    isStdStringType :: CppType -> Bool
    isStdStringType ty = case stripTypeQualifiers ty of
      CppString -> True
      CppClassType name _
        | name == "std::string" || name == "std::basic_string" -> True
      _ -> False

    isCharLikeType :: CppType -> Bool
    isCharLikeType ty = case stripTypeQualifiers ty of
      CppChar -> True
      CppUChar -> True
      _ -> False

    isStringOperand :: Located PythonExpr -> CppType -> Bool
    isStringOperand locatedExpr inferredType =
      let coreType = stripTypeQualifiers inferredType
      in isStdStringType coreType
         || case coreType of
              CppPointer inner -> isCharLikeType inner
              _ -> case locValue locatedExpr of
                     PyLiteral (PyString _) -> True
                     PyLiteral (PyFString _) -> True
                     PyJoinedStr _ -> True
                     PyFormatSpec _ -> True
                     _ -> False

    isIntegralOperand :: Located PythonExpr -> CppType -> Bool
    isIntegralOperand locatedExpr inferredType =
      let coreType = stripTypeQualifiers inferredType
      in isIntegralCppType coreType
         || case locValue locatedExpr of
              PyLiteral (PyInt _) -> True
              _ -> False

-- | Materialize a Python list literal into a C++ vector expression
data ListFallback
  = ListFallbackAny Text
  | ListFallbackVariant [CppType] Text

generatePythonListLiteral :: Located PythonExpr -> CppCodeGen (CppType, CppExpr)
generatePythonListLiteral locatedList@(Located listSpan expr) =
  case expr of
    PyList elems -> do
      addInclude "<vector>"
      indexedElems <- mapM annotateElement (zip [0 :: Int ..] elems)
      let cppElems = map (\(val, _, _) -> val) indexedElems
          refinedTypes = map (\(_, ty, _) -> ty) indexedElems
          knownTypes = filter (not . isUnknownType) refinedTypes
          unknownPositions = [idx | (_, ty, idx) <- indexedElems, isUnknownType ty]
          hasUnknown = not (null unknownPositions)
          combinedType = case knownTypes of
            [] -> Nothing
            (t:ts) -> foldM unifyElementType t ts
          uniqueTypes = nub knownTypes
          listLocation = formatSpan listSpan
          listContext = "list literal at " <> listLocation
          describeTypeList types = T.intercalate ", " (map renderCppType types)
          unknownReason =
            case unknownPositions of
              [] -> "element types could not be resolved"
              [i] -> "element " <> renderIndex i <> " lacks a resolved type"
              _ -> "elements " <> renderIndexList unknownPositions <> " lack resolved types"
          heterogeneousReason =
            "distinct element types detected: " <> describeTypeList uniqueTypes
          fallbackPlan = case (combinedType, hasUnknown, uniqueTypes) of
            (Just t, False, _) -> Right t
            (Just _, True, _) -> Left (ListFallbackAny unknownReason)
            (Nothing, False, types) | length types > 1 -> Left (ListFallbackVariant types heterogeneousReason)
            (Nothing, False, _) -> Left (ListFallbackAny "element types could not be unified")
            _ -> Left (ListFallbackAny unknownReason)
      elementType <-
        if null elems
          then do
            annotated <- tryAnnotatedElementType listContext locatedList
            pure (fromMaybe CppLongLong annotated)
          else case fallbackPlan of
            Right resolved -> pure resolved
            Left plan -> do
              annotated <- tryAnnotatedElementType listContext locatedList
              case annotated of
                Just annotatedType -> pure annotatedType
                Nothing -> applyFallback listLocation plan
      ensureElementIncludes elementType
      let vectorType = CppVector elementType
      pure (vectorType, CppBracedInit vectorType cppElems)
    _ -> do
      reportInternalError "generatePythonListLiteral called with non-list expression"
      let vectorType = CppVector CppAuto
      pure (vectorType, CppBracedInit vectorType [])
  where
    annotateElement (idx, element) = do
      cppExpr <- generatePythonExpr element
      let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated element)
          elementSpanText = formatSpan (locSpan element)
          context =
            "list literal element #" <> T.pack (show idx) <> " at " <> elementSpanText
      refinedType <- refinePythonExprType context element defaultType
      pure (cppExpr, refinedType, idx)
    tryAnnotatedElementType context exprToRefine = do
      let defaultType = CppVector CppAuto
      refined <- refinePythonExprType context exprToRefine defaultType
      case dropQualifiers refined of
        CppVector elementTy
          | not (isUnknownType elementTy) -> pure (Just elementTy)
        _ -> pure Nothing
    applyFallback location = \case
      ListFallbackAny reason -> do
        elemType <- ensureStdAny
        emitInfo $
          "List literal at " <> location <> " falling back to " <> renderCppType elemType <> " because " <> reason
        pure elemType
      ListFallbackVariant types reason -> do
        addInclude "<variant>"
        let variantType = CppVariant types
        emitInfo $
          "List literal at " <> location <> " using " <> renderCppType variantType <> " because " <> reason
        pure variantType
    ensureElementIncludes ty = case ty of
      CppConst inner -> ensureElementIncludes inner
      CppVariant _ -> addInclude "<variant>"
      CppClassType "std::any" [] -> do
        _ <- ensureStdAny
        pure ()
      _ -> pure ()
    dropQualifiers ty = case ty of
      CppConst inner -> dropQualifiers inner
      CppReference inner -> dropQualifiers inner
      CppRvalueRef inner -> dropQualifiers inner
      other -> other
    renderIndex :: Int -> Text
    renderIndex = T.pack . show
    renderIndexList :: [Int] -> Text
    renderIndexList = T.intercalate ", " . map renderIndex
    isUnknownType :: CppType -> Bool
    isUnknownType ty = case ty of
      CppAuto -> True
      CppConst inner -> isUnknownType inner
      _ -> False

-- | Generate C++ code for Python tuple literals
generatePythonTupleLiteral :: [Located PythonExpr] -> CppCodeGen CppExpr
generatePythonTupleLiteral exprs = do
  addInclude "<tuple>"
  cppExprs <- mapM generatePythonExpr exprs
  types <- mapM (\e -> pure $ fromMaybe CppAuto (inferPythonExprCppTypeLocated e)) exprs
  let tupleType = CppClassType "std::tuple" types
  return $ CppCall (CppVar "std::make_tuple") cppExprs

-- | Generate C++ code for Python set literals  
generatePythonSetLiteral :: [Located PythonExpr] -> CppCodeGen (CppType, CppExpr)
generatePythonSetLiteral exprs = do
  addInclude "<set>"
  cppExprs <- mapM generatePythonExpr exprs
  let elementType = case exprs of
        [] -> CppLongLong  -- Default for empty set
        (e:es) -> fromMaybe CppAuto $
          foldM unifyElementType (fromMaybe CppAuto $ inferPythonExprCppTypeLocated e)
            (map (fromMaybe CppAuto . inferPythonExprCppTypeLocated) es)
      setType = CppClassType "std::set" [elementType]
  pure (setType, CppBracedInit setType cppExprs)
-- | Generate C++ code for Python dict literals
generatePythonDictLiteral :: [(Located PythonExpr, Located PythonExpr)] -> CppCodeGen CppExpr
generatePythonDictLiteral pairs = do
  addInclude "<map>"
  cppPairs <- mapM generatePair pairs
  let (keyType, valueType) = case pairs of
        [] -> (CppString, CppAuto)  -- Default for empty dict
        ((k,v):_) -> 
          ( fromMaybe CppString (inferPythonExprCppTypeLocated k)
          , fromMaybe CppAuto (inferPythonExprCppTypeLocated v)
          )
  let mapType = CppClassType "std::map" [keyType, valueType]
  return $ CppBracedInit mapType cppPairs
  where
    generatePair (k, v) = do
      cppKey <- generatePythonExpr k
      cppValue <- generatePythonExpr v
      return $ CppBracedInit (CppClassType "std::pair" [CppAuto, CppAuto]) [cppKey, cppValue]

-- | Generate C++ lambda from Python lambda
generatePythonLambda :: [Located PythonParameter] -> Located PythonExpr -> CppCodeGen CppExpr
generatePythonLambda params bodyExpr = do
  cppParams <- mapM mapPythonParameter params
  cppBody <- generatePythonExpr bodyExpr
  return $ CppLambda cppParams [CppReturn (Just cppBody)]

-- | Generate C++ code for Python list comprehensions
generatePythonListComprehension :: SourceSpan -> Located PythonExpr -> [PythonComprehension] -> CppCodeGen CppExpr
generatePythonListComprehension span element comps
  | null comps =
      listComprehensionFallback span "requires at least one comprehension clause"
  | otherwise = do
      addInclude "<vector>"
      elementType <- inferListElementType span element
      builderName <- generateTempVar
      elementExpr <- generatePythonExpr element
      let builderType = CppVector elementType
          builderVar = CppVar builderName
          pushStmt = CppExprStmt (CppCall (CppMember builderVar "push_back") [elementExpr])
      bodyResult <- buildComprehensionStmts span comps [pushStmt]
      case bodyResult of
        Left reason ->
          listComprehensionFallback span reason
        Right stmtBody -> do
          let initExpr = CppBracedInit builderType []
              builderDecl = CppDecl (CppVariable builderName builderType (Just initExpr))
              lambdaBody = builderDecl : stmtBody ++ [CppReturn (Just builderVar)]
          pure $ CppCall (CppLambda [] lambdaBody) []

inferListElementType :: SourceSpan -> Located PythonExpr -> CppCodeGen CppType
inferListElementType span element =
  inferComprehensionElementType "list comprehension result" span element ensureStdAny

inferSetElementType :: SourceSpan -> Located PythonExpr -> CppCodeGen CppType
inferSetElementType span element =
  inferComprehensionElementType "set comprehension element" span element (pure CppLongLong)

inferDictKeyType :: SourceSpan -> Located PythonExpr -> CppCodeGen CppType
inferDictKeyType span keyExpr =
  inferComprehensionElementType "dict comprehension key" span keyExpr fallback
  where
    fallback = do
      addInclude "<string>"
      pure CppString

inferDictValueType :: SourceSpan -> Located PythonExpr -> CppCodeGen CppType
inferDictValueType span valueExpr =
  inferComprehensionElementType "dict comprehension value" span valueExpr ensureStdAny

inferComprehensionElementType :: Text -> SourceSpan -> Located PythonExpr -> CppCodeGen CppType -> CppCodeGen CppType
inferComprehensionElementType label span element fallbackAction = do
  let context = label <> " at " <> formatSpan span
      defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated element)
  refined <- refinePythonExprType context element defaultType
  let coreType = stripTypeQualifiers refined
  if coreType == CppAuto
    then fallbackAction
    else pure coreType

listComprehensionFallback :: SourceSpan -> Text -> CppCodeGen CppExpr
listComprehensionFallback span reason = do
  addInclude "<vector>"
  let defaultExpr = CppBracedInit (CppVector CppAuto) []
  comprehensionFallback span ("Python list comprehension " <> reason) (Just defaultExpr)

buildComprehensionStmts :: SourceSpan -> [PythonComprehension] -> [CppStmt] -> CppCodeGen (Either Text [CppStmt])
buildComprehensionStmts _ [] terminalBody = pure (Right terminalBody)
buildComprehensionStmts span (comp:rest) terminalBody
  | pyCompAsync comp =
      let locText = formatSpan (locSpan (pyCompIter comp))
      in pure (Left ("contains 'async for' clause near " <> locText))
  | otherwise = do
      innerResult <- buildComprehensionStmts span rest terminalBody
      case innerResult of
        Left err -> pure (Left err)
        Right innerBody ->
          case extractComprehensionTargetName (pyCompTarget comp) of
            Left err -> pure (Left err)
            Right targetName -> do
              iterExpr <- generatePythonExpr (pyCompIter comp)
              filterExprs <- mapM generatePythonExpr (pyCompFilters comp)
              let filteredBody = applyComprehensionFilters filterExprs innerBody
              pure (Right [CppForRange targetName iterExpr filteredBody])

extractComprehensionTargetName :: Located PythonPattern -> Either Text Text
extractComprehensionTargetName locatedPattern =
  case locValue locatedPattern of
    PatVar (Identifier name) -> Right name
    _ ->
      let desc = describePattern (locValue locatedPattern)
          message =
            "uses unsupported target pattern '"
            <> desc
            <> "' at "
            <> formatSpan (locSpan locatedPattern)
      in Left message

applyComprehensionFilters :: [CppExpr] -> [CppStmt] -> [CppStmt]
applyComprehensionFilters [] body = body
applyComprehensionFilters (cond:conds) body =
  [CppIf cond (applyComprehensionFilters conds body) []]

describePattern :: PythonPattern -> Text
describePattern pat = T.pack (show pat)


generateWalrusExpr :: SourceSpan -> Located PythonPattern -> Located PythonExpr -> CppCodeGen CppExpr
generateWalrusExpr span target valueExpr =
  case locValue target of
    PatVar (Identifier name) -> lowerNamed name
    _ -> do
      let message =
            "Assignment expression target '"
            <> describePattern (locValue target)
            <> "' at "
            <> formatSpan span
            <> " is not supported"
      reportNotImplemented message
      generatePythonExpr valueExpr
  where
    lowerNamed name = do
      symtab <- gets cgsSymbolTable
      let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated valueExpr)
          context = "assignment expression to " <> name <> " at " <> formatSpan span
      refinedType <- refinePythonExprType context valueExpr defaultType
      case HM.lookup name symtab of
        Nothing -> do
          let message =
                "Assignment expression introduces new name '"
                <> name
                <> "' at "
                <> formatSpan span
                <> " which is not yet supported"
          reportNotImplemented message
          generatePythonExpr valueExpr
        Just _ -> do
          cppValue <- generatePythonExpr valueExpr
          tempName <- generateTempVar
          let decl = CppDecl (CppVariable tempName refinedType (Just cppValue))
              assignStmt = CppExprStmt (CppBinary "=" (CppVar name) (CppVar tempName))
              returnStmt = CppReturn (Just (CppVar tempName))
          modify $ \s -> s { cgsSymbolTable = HM.insert name refinedType (cgsSymbolTable s) }
          pure $ CppCall (CppLambda [] [decl, assignStmt, returnStmt]) []

comprehensionFallback :: SourceSpan -> Text -> Maybe CppExpr -> CppCodeGen CppExpr
comprehensionFallback span baseMessage defaultExpr = do
  let message = baseMessage <> " at " <> formatSpan span
  reportNotImplemented message
  strict <- gets (cgcStrictMode . cgsConfig)
  if strict
    then do
      abortExpr <- runtimeAbortCall message
      pure $ CppBinary "," abortExpr (CppLiteral (CppIntLit 0))
    else pure (fromMaybe (CppLiteral (CppIntLit 0)) defaultExpr)

-- | Generate C++ code for Python set comprehensions
generatePythonSetComprehension :: SourceSpan -> Located PythonExpr -> [PythonComprehension] -> CppCodeGen CppExpr
generatePythonSetComprehension span element comps
  | null comps =
      setComprehensionFallback span "requires at least one comprehension clause"
  | otherwise = do
      addInclude "<set>"
      elementType <- inferSetElementType span element
      builderName <- generateTempVar
      elementExpr <- generatePythonExpr element
      let builderType = CppClassType "std::set" [elementType]
          builderVar = CppVar builderName
          insertStmt = CppExprStmt (CppCall (CppMember builderVar "insert") [elementExpr])
      bodyResult <- buildComprehensionStmts span comps [insertStmt]
      case bodyResult of
        Left reason ->
          setComprehensionFallback span reason
        Right stmtBody -> do
          let initExpr = CppBracedInit builderType []
              builderDecl = CppDecl (CppVariable builderName builderType (Just initExpr))
              lambdaBody = builderDecl : stmtBody ++ [CppReturn (Just builderVar)]
          pure $ CppCall (CppLambda [] lambdaBody) []

setComprehensionFallback :: SourceSpan -> Text -> CppCodeGen CppExpr
setComprehensionFallback span reason = do
  addInclude "<set>"
  let defaultExpr = CppBracedInit (CppClassType "std::set" [CppAuto]) []
  comprehensionFallback span ("Python set comprehension " <> reason) (Just defaultExpr)

-- | Generate C++ code for Python dict comprehensions
generatePythonDictComprehension :: SourceSpan -> Located PythonExpr -> Located PythonExpr -> [PythonComprehension] -> CppCodeGen CppExpr
generatePythonDictComprehension span keyExpr valueExpr comps
  | null comps =
      dictComprehensionFallback span "requires at least one comprehension clause"
  | otherwise = do
      addInclude "<map>"
      keyType <- inferDictKeyType span keyExpr
      valueType <- inferDictValueType span valueExpr
      builderName <- generateTempVar
      cppKey <- generatePythonExpr keyExpr
      cppValue <- generatePythonExpr valueExpr
      let builderType = CppClassType "std::map" [keyType, valueType]
          builderVar = CppVar builderName
          assignment = CppExprStmt (CppBinary "=" (CppIndex builderVar cppKey) cppValue)
      bodyResult <- buildComprehensionStmts span comps [assignment]
      case bodyResult of
        Left reason ->
          dictComprehensionFallback span reason
        Right stmtBody -> do
          let initExpr = CppBracedInit builderType []
              builderDecl = CppDecl (CppVariable builderName builderType (Just initExpr))
              lambdaBody = builderDecl : stmtBody ++ [CppReturn (Just builderVar)]
          pure $ CppCall (CppLambda [] lambdaBody) []

dictComprehensionFallback :: SourceSpan -> Text -> CppCodeGen CppExpr
dictComprehensionFallback span reason = do
  addInclude "<map>"
  let defaultExpr = CppBracedInit (CppClassType "std::map" [CppString, CppAuto]) []
  comprehensionFallback span ("Python dict comprehension " <> reason) (Just defaultExpr)


generatePythonGeneratorExpression :: SourceSpan -> Located PythonExpr -> [PythonComprehension] -> CppCodeGen CppExpr
generatePythonGeneratorExpression span element comps = do
  emitWarning $
    "Python generator expression at "
    <> formatSpan span
    <> " is eagerly materialized into std::vector"
  generatePythonListComprehension span element comps

generateSliceAccess :: Located PythonExpr -> CppExpr -> Located PythonSlice -> CppCodeGen CppExpr
generateSliceAccess targetExpr cppTarget locatedSlice =
  case locValue locatedSlice of
    SliceSlice start stop step ->
      generateLinearSlice targetExpr cppTarget (locSpan locatedSlice) start stop step
    SliceExtSlice [single] ->
      generateSliceAccess targetExpr cppTarget single
    SliceExtSlice _ ->
      sliceFallback (locSpan locatedSlice) "with multiple indices requires runtime fallback"
    SliceIndex idx -> do
      cppIdx <- generatePythonExpr idx
      pure $ CppIndex cppTarget cppIdx

generateLinearSlice
  :: Located PythonExpr
  -> CppExpr
  -> SourceSpan
  -> Maybe (Located PythonExpr)
  -> Maybe (Located PythonExpr)
  -> Maybe (Located PythonExpr)
  -> CppCodeGen CppExpr
generateLinearSlice targetExpr cppTarget sliceSpan startExpr stopExpr stepExpr = do
  let contextLabel = "slice target at " <> formatSpan (locSpan targetExpr)
      defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated targetExpr)
  resolvedType <- refinePythonExprType contextLabel targetExpr defaultType
  let coreType = stripTypeQualifiers resolvedType
  case coreType of
    CppVector _ -> do
      addInclude "<vector>"
      lowerSequenceSlice
    CppString -> do
      addInclude "<string>"
      lowerSequenceSlice
    _ ->
      let typeLabel = renderCppType coreType
      in sliceFallback sliceSpan ("over type '" <> typeLabel <> "' requires runtime fallback")
  where
    lowerSequenceSlice = do
      ensureSliceHelper
      startOpt <- buildSliceOptional startExpr
      stopOpt <- buildSliceOptional stopExpr
      stepOpt <- buildSliceOptional stepExpr
      pure $ CppCall (CppVar sliceHelperName) [cppTarget, startOpt, stopOpt, stepOpt]

buildSliceOptional :: Maybe (Located PythonExpr) -> CppCodeGen CppExpr
buildSliceOptional Nothing = pure (CppBracedInit sliceOptionalType [])
buildSliceOptional (Just expr) = do
  value <- generatePythonExpr expr
  pure (CppBracedInit sliceOptionalType [CppCast CppLongLong value])

sliceOptionalType :: CppType
sliceOptionalType = CppOptional CppLongLong

sliceFallback :: SourceSpan -> Text -> CppCodeGen CppExpr
sliceFallback sliceSpan reason = do
  let baseMessage = "Python slicing " <> reason
      message = baseMessage <> " at " <> formatSpan sliceSpan
  reportUnsupported message
  abortExpr <- runtimeAbortCall message
  pure $ CppBinary "," abortExpr (CppLiteral (CppIntLit 0))

inferPythonExprCppTypeLocated :: Located PythonExpr -> Maybe CppType
inferPythonExprCppTypeLocated expr = do
  -- First try to infer from the expression itself
  case inferPythonExprCppType (locValue expr) of
    Just t -> Just t
    Nothing -> do
      -- If that fails, try to look up in the symbol table for variables
      case locValue expr of
        PyVar (Identifier name) -> do
          -- Note: We can't access the symbol table here since this is a pure function
          -- This will be handled in refinePythonExprType instead
          Nothing
        _ -> Nothing

inferPythonExprCppType :: PythonExpr -> Maybe CppType
inferPythonExprCppType = \case
  PyLiteral lit -> inferPythonLiteralType lit
  PyConst (QualifiedName _ (Identifier name)) -> case name of
    "True" -> Just CppBool
    "False" -> Just CppBool
    _ -> Nothing
  PyVar (Identifier _) -> Nothing -- Will be handled in refinePythonExprType
  PyUnaryOp op inner -> case op of
    OpNot -> Just CppBool
    _ -> inferPythonExprCppTypeLocated inner
  PyBinaryOp _ left right -> do
    t1 <- inferPythonExprCppTypeLocated left
    t2 <- inferPythonExprCppTypeLocated right
    unifyElementType t1 t2
  PyBoolOp _ _ -> Just CppBool
  PyComparison _ _ -> Just CppBool
  PyNamedExpr _ valueExpr -> inferPythonExprCppTypeLocated valueExpr
  PyAwait awaitedExpr -> inferPythonExprCppTypeLocated awaitedExpr
  PyStarred inner -> inferPythonExprCppTypeLocated inner
  PyJoinedStr _ -> Just CppString
  PyFormatSpec _ -> Just CppString
  PyGenComp _ _ -> Just (CppVector CppAuto)
  _ -> Nothing

inferPythonLiteralType :: PythonLiteral -> Maybe CppType
inferPythonLiteralType = \case
  PyInt _ -> Just CppLongLong
  PyFloat _ -> Just CppDouble
  PyBool _ -> Just CppBool
  PyString _ -> Just CppString
  PyFString _ -> Just CppString
  PyBytes _ -> Just CppString
  _ -> Nothing

unifyElementType :: CppType -> CppType -> Maybe CppType
unifyElementType t1 t2
  | t1 == t2 = Just t1
  | isNumericType t1 && isNumericType t2 = Just (promoteNumericType t1 t2)
  | t1 == CppBool && t2 == CppBool = Just CppBool
  | t1 == CppBool && isNumericType t2 = Just (promoteNumericType CppLongLong t2)
  | isNumericType t1 && t2 == CppBool = Just (promoteNumericType t1 CppLongLong)
  | otherwise = Nothing

isNumericType :: CppType -> Bool
isNumericType = \case
  CppShort -> True
  CppUShort -> True
  CppInt -> True
  CppUInt -> True
  CppLong -> True
  CppULong -> True
  CppLongLong -> True
  CppULongLong -> True
  CppFloat -> True
  CppDouble -> True
  CppLongDouble -> True
  _ -> False

isIntegralCppType :: CppType -> Bool
isIntegralCppType = \case
  CppBool -> True
  CppChar -> True
  CppUChar -> True
  CppShort -> True
  CppUShort -> True
  CppInt -> True
  CppUInt -> True
  CppLong -> True
  CppULong -> True
  CppLongLong -> True
  CppULongLong -> True
  _ -> False

promoteNumericType :: CppType -> CppType -> CppType
promoteNumericType t1 t2
  | t1 `elem` floatingTypes || t2 `elem` floatingTypes = CppDouble
  | otherwise = CppLongLong
  where
    floatingTypes = [CppFloat, CppDouble, CppLongDouble]

-- | Promote an operand to double for Python's true division
promoteToTrueDivOperand :: CppExpr -> CppExpr
promoteToTrueDivOperand expr = case expr of
  CppLiteral (CppIntLit n) -> CppLiteral (CppFloatLit (fromIntegral n))
  _ -> CppCast CppDouble expr

-- | Format an argument for printing, converting Python bool values to "True"/"False"
formatPrintArgument :: Located PythonArgument -> CppCodeGen CppExpr
formatPrintArgument (Located span argument) =
  case argument of
    ArgPositional expr -> formatExpr expr
    ArgKeyword name expr -> do
      emitWarning $ "Keyword argument '" <> (\(Identifier n) -> n) name
        <> "' at " <> formatSpan span <> " is being treated as positional argument"
      formatExpr expr
    ArgStarred expr -> do
      emitWarning $ "*args unpacking at " <> formatSpan span
        <> " is not fully supported, treating as single argument"
      generatePythonExpr expr
    ArgKwStarred expr -> do
      emitWarning $ "**kwargs unpacking at " <> formatSpan span
        <> " is not fully supported, treating as single argument"
      generatePythonExpr expr
  where
    formatExpr :: Located PythonExpr -> CppCodeGen CppExpr
    formatExpr locatedExpr = do
      cppExpr <- generatePythonExpr locatedExpr
      case locValue locatedExpr of
        PyBinaryOp OpDiv _ _ -> pure (wrapFloatForPrint cppExpr)
        _ ->
          case inferPythonExprCppTypeLocated locatedExpr of
            Just CppBool -> pure (wrapBoolForPrint cppExpr)
            Just t | isFloatingCppType t -> pure (wrapFloatForPrint cppExpr)
            _ -> pure cppExpr

-- | Wrap a boolean expression to print as "True" or "False"
wrapBoolForPrint :: CppExpr -> CppExpr
wrapBoolForPrint boolExpr =
  CppCall (CppVar boolToStringHelperName) [boolExpr]

boolToStringHelperName :: Text
boolToStringHelperName = "py_bool_to_string"

-- | Check if a C++ type is a floating-point type
isFloatingCppType :: CppType -> Bool
isFloatingCppType = \case
  CppFloat -> True
  CppDouble -> True
  CppLongDouble -> True
  _ -> False

-- | Wrap a floating-point expression for Python-style printing
wrapFloatForPrint :: CppExpr -> CppExpr
wrapFloatForPrint floatExpr =
  CppCall (CppVar floatToStringHelperName) [floatExpr]

floatToStringHelperName :: Text
floatToStringHelperName = "py_float_to_string"

-- | Ensure Python helper functions are added
ensurePythonHelpers :: CppCodeGen ()
ensurePythonHelpers = do
  ensureHelper boolToStringHelperName boolHelperDecl
  addInclude "<sstream>"
  ensureHelper floatToStringHelperName floatHelperDecl
  ensureRangeHelper
  where
    ensureHelper name decl = do
      existing <- gets cgsDeclarations
      unless (any (isHelper name) existing) $ addDeclaration decl
    isHelper targetName (CppFunction name _ _ _) = name == targetName
    isHelper _ _ = False

    boolHelperDecl =
      CppFunction boolToStringHelperName CppString
        [CppParam "value" CppBool Nothing]
        [ CppIf (CppVar "value")
            [CppReturn (Just (CppLiteral (CppStringLit "True")))]
            [CppReturn (Just (CppLiteral (CppStringLit "False")))]
        ]

    floatHelperDecl =
      let resultVar = CppVar "result"
          findWith lit = CppCall (CppMember resultVar "find") [CppLiteral (CppStringLit lit)]
          npos = CppVar "std::string::npos"
          dotMissing = CppBinary "==" (findWith ".") npos
          lowerEMissing = CppBinary "==" (findWith "e") npos
          upperEMissing = CppBinary "==" (findWith "E") npos
          exponentMissing = CppBinary "&&" lowerEMissing upperEMissing
          appendStmt = CppExprStmt (CppCall (CppMember resultVar "append") [CppLiteral (CppStringLit ".0")])
      in CppFunction floatToStringHelperName CppString
          [CppParam "value" CppDouble Nothing]
          [ CppDecl (CppVariable "oss" (CppClassType "std::ostringstream" []) Nothing)
          , CppExprStmt (CppBinary "<<" (CppVar "oss") (CppVar "value"))
          , CppDecl (CppVariable "result" CppString (Just (CppCall (CppMember (CppVar "oss") "str") [])))
          , CppIf (CppBinary "&&" dotMissing exponentMissing)
              [appendStmt]
              []
          , CppReturn (Just resultVar)
          ]

-- | Ensure range() helper function is added
ensureRangeHelper :: CppCodeGen ()
ensureRangeHelper = do
  addInclude "<vector>"
  existing <- gets cgsDeclarations
  unless (any isRangeHelper existing) $ do
    addDeclaration rangeHelperDecl
  where
    isRangeHelper (CppFunction name _ _ _) = name == "range"
    isRangeHelper _ = False
    
    rangeHelperDecl =
      CppFunction "range" (CppVector CppLongLong)
        [CppParam "n" CppLongLong Nothing]
        [ CppDecl (CppVariable "result" (CppVector CppLongLong) Nothing)
        , CppExprStmt (CppCall (CppMember (CppVar "result") "reserve") [CppVar "n"])
        , CppFor
            (Just (CppDecl (CppVariable "i" CppLongLong (Just (CppLiteral (CppIntLit 0))))))
            (Just (CppBinary "<" (CppVar "i") (CppVar "n")))
            (Just (CppUnary "++" (CppVar "i")))
            [CppExprStmt (CppCall (CppMember (CppVar "result") "push_back") [CppVar "i"])]
        , CppReturn (Just (CppVar "result"))
        ]

stringRepeatHelperName :: Text
stringRepeatHelperName = "fluxus_repeat_string"

ensureStringRepeatHelper :: CppCodeGen ()
ensureStringRepeatHelper = do
  addInclude "<string>"
  existing <- gets cgsDeclarations
  unless (any isRepeatHelper existing) $
    addDeclaration repeatHelperDecl
  where
    isRepeatHelper (CppFunction name _ _ _) = name == stringRepeatHelperName
    isRepeatHelper _ = False

    repeatHelperDecl =
      CppFunction stringRepeatHelperName CppString
        [ CppParam "value" (CppConst (CppReference CppString)) Nothing
        , CppParam "count" CppLongLong Nothing
        ]
        [ CppIf (CppBinary "<=" (CppVar "count") (CppLiteral (CppIntLit 0)))
            [CppReturn (Just (CppLiteral (CppStringLit "")))]
            []
        , CppDecl (CppVariable "result" CppString (Just (CppLiteral (CppStringLit ""))))
        , CppFor
            (Just (CppDecl (CppVariable "i" CppLongLong (Just (CppLiteral (CppIntLit 0))))))
            (Just (CppBinary "<" (CppVar "i") (CppVar "count")))
            (Just (CppUnary "++" (CppVar "i")))
            [CppExprStmt (CppBinary "+=" (CppVar "result") (CppVar "value"))]
        , CppReturn (Just (CppVar "result"))
        ]

sliceHelperName :: Text
sliceHelperName = "fluxus_slice"

ensureSliceHelper :: CppCodeGen ()
ensureSliceHelper = do
  addInclude "<optional>"
  addInclude "<cstddef>"
  ensureRuntimeAbortHelper
  existing <- gets cgsDeclarations
  unless (any isSliceHelper existing) $ do
    abortExpr <- runtimeAbortCall "Python slice step cannot be zero"
    addDeclaration (sliceHelperDecl abortExpr)
  where
    isSliceHelper (CppTemplate _ (CppFunction name _ _ _)) = name == sliceHelperName
    isSliceHelper _ = False

    sliceHelperDecl abortExpr =
      let seqType = CppTemplateType "Seq" []
          seqParam = CppParam "seq" (CppConst (CppReference seqType)) Nothing
          optionalParam name = CppParam name sliceOptionalType Nothing
          sizeVar = "size_"
          stepVar = "step_"
          forwardVar = "forward_"
          startVar = "start_"
          stopVar = "stop_"
          indexVar = "idx_"
          resultVar = "result_"
          hasValue name = CppCall (CppMember (CppVar name) "has_value") []
          optionalValue name = CppCall (CppMember (CppVar name) "value") []
          assign name expr = CppExprStmt (CppBinary "=" (CppVar name) expr)
          adjustNegative name =
            CppIf (CppBinary "<" (CppVar name) (CppLiteral (CppIntLit 0)))
              [CppExprStmt (CppBinary "+=" (CppVar name) (CppVar sizeVar))]
              []
          clampWith name op test replacement =
            CppIf (CppBinary op (CppVar name) test)
              [assign name replacement]
              []
          stepExpr =
            CppConditional (hasValue "step")
              (CppCast CppLongLong (optionalValue "step"))
              (CppLiteral (CppIntLit 1))
          startFallback =
            CppConditional (CppVar forwardVar)
              (CppLiteral (CppIntLit 0))
              (CppBinary "-" (CppVar sizeVar) (CppLiteral (CppIntLit 1)))
          stopFallback =
            CppConditional (CppVar forwardVar)
              (CppVar sizeVar)
              (CppLiteral (CppIntLit (-1)))
          startValue =
            CppConditional (hasValue "start")
              (CppCast CppLongLong (optionalValue "start"))
              startFallback
          stopValue =
            CppConditional (hasValue "stop")
              (CppCast CppLongLong (optionalValue "stop"))
              stopFallback
          startClamp =
            CppIf (CppVar forwardVar)
              [ clampWith startVar "<" (CppLiteral (CppIntLit 0)) (CppLiteral (CppIntLit 0))
              , clampWith startVar ">" (CppVar sizeVar) (CppVar sizeVar)
              ]
              [ clampWith startVar "<" (CppLiteral (CppIntLit (-1))) (CppLiteral (CppIntLit (-1)))
              , clampWith startVar ">=" (CppVar sizeVar) (CppBinary "-" (CppVar sizeVar) (CppLiteral (CppIntLit 1)))
              ]
          stopClamp =
            CppIf (CppVar forwardVar)
              [ clampWith stopVar "<" (CppLiteral (CppIntLit 0)) (CppLiteral (CppIntLit 0))
              , clampWith stopVar ">" (CppVar sizeVar) (CppVar sizeVar)
              ]
              [ clampWith stopVar "<" (CppLiteral (CppIntLit (-1))) (CppLiteral (CppIntLit (-1)))
              , clampWith stopVar ">=" (CppVar sizeVar) (CppBinary "-" (CppVar sizeVar) (CppLiteral (CppIntLit 1)))
              ]
          boundsCheck =
            CppIf
              (CppBinary "||"
                (CppBinary "<" (CppVar indexVar) (CppLiteral (CppIntLit 0)))
                (CppBinary ">=" (CppVar indexVar) (CppVar sizeVar)))
              [CppBreak]
              []
          pushBack =
            CppExprStmt
              (CppCall (CppMember (CppVar resultVar) "push_back")
                [CppIndex (CppVar "seq") (CppCast CppSizeT (CppVar indexVar))])
          loopBody = [boundsCheck, pushBack]
          positiveLoop =
            CppFor
              (Just (CppDecl (CppVariable indexVar CppLongLong (Just (CppVar startVar)))))
              (Just (CppBinary "<" (CppVar indexVar) (CppVar stopVar)))
              (Just (CppBinary "+=" (CppVar indexVar) (CppVar stepVar)))
              loopBody
          negativeLoop =
            CppFor
              (Just (CppDecl (CppVariable indexVar CppLongLong (Just (CppVar startVar)))))
              (Just (CppBinary ">" (CppVar indexVar) (CppVar stopVar)))
              (Just (CppBinary "+=" (CppVar indexVar) (CppVar stepVar)))
              loopBody
          helperBody =
            [ CppDecl (CppVariable sizeVar CppLongLong (Just (CppCast CppLongLong (CppCall (CppMember (CppVar "seq") "size") []))))
            , CppDecl (CppVariable stepVar CppLongLong (Just stepExpr))
            , CppIf (CppBinary "==" (CppVar stepVar) (CppLiteral (CppIntLit 0)))
                [ CppExprStmt abortExpr
                , CppReturn (Just (CppVar "seq"))
                ]
                []
            , CppDecl (CppVariable forwardVar CppBool (Just (CppBinary ">" (CppVar stepVar) (CppLiteral (CppIntLit 0)))))
            , CppDecl (CppVariable startVar CppLongLong (Just startValue))
            , adjustNegative startVar
            , startClamp
            , CppDecl (CppVariable stopVar CppLongLong (Just stopValue))
            , adjustNegative stopVar
            , stopClamp
            , CppDecl (CppVariable resultVar seqType (Just (CppBracedInit seqType [])))
            , CppIf (CppVar forwardVar) [positiveLoop] [negativeLoop]
            , CppReturn (Just (CppVar resultVar))
            ]
      in CppTemplate ["typename Seq"]
           (CppFunction sliceHelperName seqType
             [seqParam, optionalParam "start", optionalParam "stop", optionalParam "step"]
             helperBody)

finallyGuardStructName :: Text
finallyGuardStructName = "FluxusFinallyGuard"

finallyGuardType :: CppType
finallyGuardType = CppClassType finallyGuardStructName []

ensureFinallyGuardHelper :: CppCodeGen ()
ensureFinallyGuardHelper = do
  addInclude "<functional>"
  addInclude "<utility>"
  existing <- gets cgsDeclarations
  unless (any isFinallyGuard existing) $
    addDeclaration finallyGuardDecl
  where
    isFinallyGuard (CppStruct name _) = name == finallyGuardStructName
    isFinallyGuard _ = False

finallyGuardDecl :: CppDecl
finallyGuardDecl =
  CppStruct finallyGuardStructName
    [ CppAccessSpec "public"
    , guardConstructor
    , guardMoveConstructor
    , guardDestructor
    , guardDismiss
    , CppAccessSpec "private"
    , CppVariable "handler_" handlerType Nothing
    , CppVariable "active_" CppBool (Just (CppLiteral (CppBoolLit False)))
    ]
  where
    handlerType = CppClassType "std::function" [CppFunctionType [] CppVoid]
    guardConstructor =
      CppConstructor finallyGuardStructName
        [CppParam "handler" handlerType Nothing]
        [ CppExprStmt
            (CppBinary "="
              (CppVar "handler_")
              (CppMove (CppVar "handler")))
        , CppExprStmt
            (CppBinary "="
              (CppVar "active_")
              (CppLiteral (CppBoolLit True)))
        ]
    guardMoveConstructor =
      CppConstructor finallyGuardStructName
        [CppParam "other" (CppRvalueRef finallyGuardType) Nothing]
        [ CppExprStmt
            (CppBinary "="
              (CppVar "handler_")
              (CppMove (CppMember (CppVar "other") "handler_")))
        , CppExprStmt
            (CppBinary "="
              (CppVar "active_")
              (CppMember (CppVar "other") "active_"))
        , CppExprStmt
            (CppBinary "="
              (CppMember (CppVar "other") "active_")
              (CppLiteral (CppBoolLit False)))
        ]
    guardDestructor =
      CppDestructor finallyGuardStructName
        [ CppIf (CppVar "active_")
            [CppExprStmt (CppCall (CppVar "handler_") [])]
            []
        ] False
    guardDismiss =
      CppMethod "dismiss" CppVoid []
        [ CppExprStmt
            (CppBinary "="
              (CppVar "active_")
              (CppLiteral (CppBoolLit False)))
        ] False

generatePythonFunction :: PythonFuncDef -> CppCodeGen ()
generatePythonFunction funcDef = do
  let funcName = (\(Identifier n) -> n) (pyFuncName funcDef)

  -- Map parameters
  cppParams <- mapM mapPythonParameter (pyFuncParams funcDef)

  -- Determine return type
  returnType <- case pyFuncReturns funcDef of
    Just typeExpr ->
      if isNoneTypeExpr (locValue typeExpr)
        then pure CppVoid
        else do
          annotatedType <- mapPythonType typeExpr
          refineAnnotatedReturnType funcName annotatedType (pyFuncBody funcDef)
    Nothing
      | funcName == "main" -> return CppInt
      | otherwise -> inferFunctionReturnType funcName (pyFuncBody funcDef)

  -- Generate function body
  bodyStmts <- withFunctionScope $ do
    registerParameters cppParams
    mapM (generatePythonStmt ScopeFunction) (pyFuncBody funcDef)

  -- Add return statement for main function if needed
  let finalBodyStmts = if funcName == "main" && returnType == CppInt
                      then bodyStmts ++ [CppReturn (Just (CppLiteral $ CppIntLit 0))]
                      else bodyStmts

  addDeclaration $ CppFunction funcName returnType cppParams finalBodyStmts

refineAnnotatedReturnType :: Text -> CppType -> [Located PythonStmt] -> CppCodeGen CppType
refineAnnotatedReturnType funcName annotatedType body
  | annotatedType == CppVoid = pure CppVoid
  | otherwise = do
      let returnExprs = collectReturnExprs body
      if null returnExprs
        then pure annotatedType
        else do
          refinedTypes <-
            mapM
              (\expr -> refinePythonExprType ("annotated return from " <> funcName) expr annotatedType)
              returnExprs
          case nub refinedTypes of
            [] -> pure annotatedType
            (firstType:_) -> pure firstType

-- | Infer the appropriate C++ return type for a Python function when no annotation is provided
inferFunctionReturnType :: Text -> [Located PythonStmt] -> CppCodeGen CppType
inferFunctionReturnType funcName body = do
  let returnExprs = collectReturnExprs body
  if null returnExprs
    then return CppVoid
    else do
      refinedTypes <- mapM resolveReturnType returnExprs
      let knownTypes = filter (/= CppAuto) refinedTypes
          hasUnknown = length knownTypes /= length refinedTypes
          combinedType = case knownTypes of
            [] -> Nothing
            (t:ts) -> foldM unifyElementType t ts
      case combinedType of
        Just t -> return t
        Nothing ->
          case nub knownTypes of
            [] ->
              if hasUnknown
                then return CppAuto
                else return CppVoid
            [single] -> return single
            multiple -> do
              addInclude "<variant>"
              return (CppVariant multiple)
  where
    resolveReturnType :: Located PythonExpr -> CppCodeGen CppType
    resolveReturnType expr = do
      let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated expr)
          context = "return from " <> funcName
      refinePythonExprType context expr defaultType

-- | Collect return expressions that yield values from a list of Python statements
collectReturnExprs :: [Located PythonStmt] -> [Located PythonExpr]
collectReturnExprs = concatMap collectStmt
  where
    collectStmt :: Located PythonStmt -> [Located PythonExpr]
    collectStmt (Located _ stmt) = case stmt of
      PyReturn (Just expr) -> [expr]
      PyIf _ thenStmts elseStmts ->
        collectReturnExprs thenStmts ++ collectReturnExprs elseStmts
      PyWhile _ bodyStmts elseStmts ->
        collectReturnExprs bodyStmts ++ collectReturnExprs elseStmts
      PyFor _ _ bodyStmts elseStmts ->
        collectReturnExprs bodyStmts ++ collectReturnExprs elseStmts
      PyWith _ bodyStmts ->
        collectReturnExprs bodyStmts
      PyTry tryBlock excepts orelse finally ->
        collectReturnExprs tryBlock
        ++ concatMap (collectReturnExprs . pyExceptBody . locatedValue) excepts
        ++ collectReturnExprs orelse
        ++ collectReturnExprs finally
      PyAsyncWith _ bodyStmts ->
        collectReturnExprs bodyStmts
      PyAsyncFor _ _ bodyStmts elseStmts ->
        collectReturnExprs bodyStmts ++ collectReturnExprs elseStmts
      PyFuncDef _ -> []
      PyAsyncFuncDef _ -> []
      PyClassDef _ -> []
      _ -> []

-- | Generate C++ classes from Python
generatePythonClass :: PythonClassDef -> CppCodeGen ()
generatePythonClass classDef = do
  let className = (\(Identifier n) -> n) (pyClassName classDef)
  
  baseClasses <- fmap catMaybes $ mapM extractBaseClassName (pyClassBases classDef)
  members <- fmap catMaybes $ mapM (generatePythonClassMember className) (pyClassBody classDef)
  let memberDecls =
        case members of
          [] -> []
          xs -> CppAccessSpec "public" : xs
  
  addDeclaration $ CppClass className baseClasses memberDecls



refinePythonExprType :: Text -> Located PythonExpr -> CppType -> CppCodeGen CppType
refinePythonExprType context locatedExpr defaultType = do
  -- First check if this is a variable and look up its type in the symbol table
  case locValue locatedExpr of
    PyVar (Identifier name) -> do
      symtab <- gets cgsSymbolTable
      case HM.lookup name symtab of
        Just varType -> do
          emitInfo $ context <> ": found variable '" <> name <> "' with type " <> T.pack (show varType) <> " in symbol table"
          pure varType
        Nothing -> do
          emitInfo $ context <> ": variable '" <> name <> "' not found in symbol table, using default type " <> T.pack (show defaultType)
          -- Continue with annotation lookup
          lookupAnnotationsIfNeeded
    PyListComp element comps -> do
      -- Handle list comprehensions explicitly
      addInclude "<vector>"
      elementType <- inferListElementType (locSpan locatedExpr) element
      let listType = CppVector elementType
      emitInfo $ context <> ": list comprehension detected, using type " <> T.pack (show listType)
      pure listType
    PySetComp element comps -> do
      -- Handle set comprehensions explicitly  
      addInclude "<unordered_set>"
      elementType <- inferSetElementType (locSpan locatedExpr) element
      let setType = CppTemplateType "std::unordered_set" [elementType]
      emitInfo $ context <> ": set comprehension detected, using type " <> T.pack (show setType)
      pure setType
    PyDictComp keyExpr valueExpr comps -> do
      -- Handle dict comprehensions explicitly
      addInclude "<unordered_map>"
      keyType <- inferDictKeyType (locSpan locatedExpr) keyExpr
      valueType <- inferDictValueType (locSpan locatedExpr) valueExpr
      let dictType = CppTemplateType "std::unordered_map" [keyType, valueType]
      emitInfo $ context <> ": dict comprehension detected, using type " <> T.pack (show dictType)
      pure dictType
    _ -> lookupAnnotationsIfNeeded
  where
    lookupAnnotationsIfNeeded = do
      case pythonExprToLocatedCommon locatedExpr of
        Left err -> do
          emitInfo $ context <> ": unable to fingerprint expression for annotations - " <> renderLoweringIssue err
          pure defaultType
        Right commonLocated ->
          let exprKey = fingerprintCommonExpr commonLocated
          in lookupAndApplyAnnotations context exprKey defaultType

mapPythonLiteral :: PythonLiteral -> CppLiteral
mapPythonLiteral = \case
  PyInt i -> CppIntLit i
  PyFloat f -> CppFloatLit f
  PyBool b -> CppBoolLit b
  PyString s -> CppStringLit s
  PyFString segments -> CppStringLit (mconcat [txt | PythonFStringLiteral txt <- segments])
  PyNone -> CppNullPtr
  _ -> CppIntLit 0

mapPythonBinaryOp :: BinaryOp -> Text
mapPythonBinaryOp = \case
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
  _ -> "+"  -- Fallback

mapAugmentedAssignmentOp :: BinaryOp -> Maybe Text
mapAugmentedAssignmentOp = \case
  OpAdd -> Just "+="
  OpSub -> Just "-="
  OpMul -> Just "*="
  OpMod -> Just "%="
  OpBitAnd -> Just "&="
  OpBitOr -> Just "|="
  OpBitXor -> Just "^="
  OpShiftL -> Just "<<="
  OpShiftR -> Just ">>="
  OpConcat -> Just "+="
  _ -> Nothing

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

generatePythonInteropBindings :: Text -> CppCodeGen ()
generatePythonInteropBindings moduleName =
  emitInfo $ "Python interop bindings for module: " <> moduleName

mapPythonType :: Located PythonTypeExpr -> CppCodeGen CppType
mapPythonType (Located span expr) =
  case expr of
    TypeName qn -> mapTypeName span qn
    TypeVar name ->
      fallbackToStdAny span expr ("type variable '" <> name <> "' is not supported in C++ generation")
    TypeSubscript base args -> mapTypeSubscript span base args
    TypeTuple elems -> mapTypeTuple span elems
    TypeUnion members -> mapTypeUnion span members
    TypeOptional inner -> mapTypeOptional span inner
    TypeCallable args ret -> mapTypeCallable span args ret
    TypeLiteral literalExpr -> mapTypeLiteral span literalExpr

mapTypeName :: SourceSpan -> QualifiedName -> CppCodeGen CppType
mapTypeName span qn = do
  let canonical = canonicalQualifiedName qn
      simple = T.toLower (identifierText (qnName qn))
  case () of
    _ | simple == "int" -> pure CppLongLong
      | simple == "float" -> pure CppDouble
      | simple == "bool" -> pure CppBool
      | simple == "str" -> do
          addInclude "<string>"
          pure CppString
      | simple == "bytes" -> do
          addInclude "<string>"
          pure CppString
      | simple == "none" -> do
          addInclude "<variant>"
          pure stdMonostateType
      | canonical == "typing.any" || simple == "any" -> do
          addInclude "<any>"
          pure stdAnyType
      | canonical == "typing.noreturn" || canonical == "typing.never" -> pure CppVoid
      | canonical == "builtins.object" || simple == "object" -> do
          addInclude "<any>"
          pure stdAnyType
      | otherwise ->
          pure (CppClassType (qualifiedNameToCpp qn) [])

mapTypeSubscript :: SourceSpan -> Located PythonTypeExpr -> [Located PythonTypeExpr] -> CppCodeGen CppType
mapTypeSubscript span base args =
  case locValue base of
    TypeName qn ->
      let canonical = canonicalQualifiedName qn
          simple = T.toLower (identifierText (qnName qn))
      in case () of
        _ | simple == "list" || canonical `elem` listNames -> mapList
          | simple == "tuple" || canonical `elem` tupleNames -> mapTupleFromArgs
          | simple == "set" || canonical `elem` setNames -> mapSet
          | simple == "dict" || canonical `elem` dictNames -> mapDict
          | simple == "optional" || canonical `elem` optionalNames -> mapOptionalFromArgs
          | simple == "union" || canonical `elem` unionNames -> mapUnionFromArgs
          | simple == "callable" || canonical `elem` callableNames -> mapCallableFromArgs
          | simple == "annotated" || canonical `elem` annotatedNames -> mapAnnotated
          | simple == "final" || canonical `elem` finalNames -> mapFinal
          | otherwise -> mapGenericType qn
    _ ->
      fallbackToStdAny span (TypeSubscript base args) "uses an unsupported base expression"
  where
    listNames = ["typing.list", "builtins.list", "typing.sequence", "typing.mutablesequence"]
    tupleNames = ["typing.tuple"]
    setNames = ["typing.set", "typing.mutableset", "builtins.set", "typing.frozenset"]
    dictNames = ["typing.dict", "typing.mapping", "typing.mutablemapping"]
    optionalNames = ["typing.optional"]
    unionNames = ["typing.union"]
    callableNames = ["typing.callable"]
    annotatedNames = ["typing.annotated"]
    finalNames = ["typing.final"]

    mapList = case args of
      [elemExpr] -> do
        elemType <- mapPythonType elemExpr
        addInclude "<vector>"
        pure $ CppVector elemType
      [] -> do
        addInclude "<vector>"
        elemType <- ensureStdAny
        emitWarning $
          "Type constructor 'list' used without element type at "
          <> formatSpan span <> "; assuming std::vector<std::any>"
        pure $ CppVector elemType
      _ -> do
        addInclude "<vector>"
        elemType <- ensureStdAny
        emitWarning $
          "Type constructor 'list' expects exactly one argument at "
          <> formatSpan span <> "; using std::vector<std::any>"
        pure $ CppVector elemType

    mapTupleFromArgs = mapTypeTuple span args

    mapSet = case args of
      [elemExpr] -> do
        elemType <- mapPythonType elemExpr
        addInclude "<unordered_set>"
        pure $ CppClassType "std::unordered_set" [elemType]
      [] -> do
        addInclude "<unordered_set>"
        elemType <- ensureStdAny
        emitWarning $
          "Type constructor 'set' used without element type at "
          <> formatSpan span <> "; assuming std::unordered_set<std::any>"
        pure $ CppClassType "std::unordered_set" [elemType]
      _ -> do
        addInclude "<unordered_set>"
        elemType <- ensureStdAny
        emitWarning $
          "Type constructor 'set' expects exactly one argument at "
          <> formatSpan span <> "; using std::unordered_set<std::any>"
        pure $ CppClassType "std::unordered_set" [elemType]

    mapDict = case args of
      [keyExpr, valueExpr] -> do
        keyType <- mapPythonType keyExpr
        valueType <- mapPythonType valueExpr
        addInclude "<unordered_map>"
        pure $ CppUnorderedMap keyType valueType
      _ -> do
        addInclude "<unordered_map>"
        addInclude "<string>"
        valueType <- ensureStdAny
        emitWarning $
          "Type constructor 'dict' expects two arguments at "
          <> formatSpan span <> "; using std::unordered_map<std::string, std::any>"
        pure $ CppUnorderedMap CppString valueType

    mapOptionalFromArgs = case args of
      [innerExpr] -> mapTypeOptional span innerExpr
      _ -> fallbackToStdAny span (TypeSubscript base args) "expects exactly one type argument"

    mapUnionFromArgs = mapTypeUnion span args

    mapCallableFromArgs =
      case unsnoc args of
        Just (paramArgs, retExpr) -> mapTypeCallable span paramArgs retExpr
        Nothing -> fallbackToStdAny span (TypeSubscript base args) "expects argument list and return type"

    unsnoc [] = Nothing
    unsnoc xs = Just (init xs, last xs)

    mapAnnotated = case args of
      (primary:_) -> mapPythonType primary
      _ -> fallbackToStdAny span (TypeSubscript base args) "expects at least one underlying type"

    mapFinal = case args of
      (primary:_) -> mapPythonType primary
      _ -> fallbackToStdAny span (TypeSubscript base args) "expects a concrete type argument"

    mapGenericType qn = do
      argTypes <- mapM mapPythonType args
      pure $ CppClassType (qualifiedNameToCpp qn) argTypes

mapTypeTuple :: SourceSpan -> [Located PythonTypeExpr] -> CppCodeGen CppType
mapTypeTuple span elems =
  case elems of
    [elemExpr, ellipsisExpr] | isEllipsisTypeExpr (locValue ellipsisExpr) -> do
      elemType <- mapPythonType elemExpr
      addInclude "<vector>"
      pure $ CppVector elemType
    _ -> do
      tupleTypes <- mapM mapPythonType elems
      addInclude "<tuple>"
      pure $ CppTuple tupleTypes

mapTypeUnion :: SourceSpan -> [Located PythonTypeExpr] -> CppCodeGen CppType
mapTypeUnion span members = do
  let (noneMembers, otherMembers) = partition (isNoneTypeExpr . locValue) members
  mapped <- mapM mapPythonType otherMembers
  let uniqueMapped = nub mapped
  case (not (null noneMembers), uniqueMapped) of
    (True, []) -> do
      addInclude "<optional>"
      addInclude "<variant>"
      pure $ CppOptional stdMonostateType
    (True, [single]) -> do
      addInclude "<optional>"
      pure $ CppOptional single
    (True, more) -> do
      addInclude "<optional>"
      addInclude "<variant>"
      pure $ CppOptional (CppVariant more)
    (False, []) ->
      fallbackToStdAny span (TypeUnion members) "did not provide any supported member types"
    (False, [single]) -> pure single
    (False, more) -> do
      addInclude "<variant>"
      pure $ CppVariant more

mapTypeOptional :: SourceSpan -> Located PythonTypeExpr -> CppCodeGen CppType
mapTypeOptional span inner = do
  innerType <- mapPythonType inner
  addInclude "<optional>"
  pure $ CppOptional innerType

mapTypeCallable :: SourceSpan -> [Located PythonTypeExpr] -> Located PythonTypeExpr -> CppCodeGen CppType
mapTypeCallable _ args ret = do
  argTypes <- case args of
    [] -> pure []
    [singleArg] -> callableArgList singleArg
    _ -> mapM mapPythonType args
  returnType <- mapPythonType ret
  addInclude "<functional>"
  pure $ CppClassType "std::function" [CppFunctionType argTypes returnType]
  where
    callableArgList argExpr =
      case locValue argExpr of
        TypeTuple tupleElems -> mapM mapPythonType tupleElems
        _ | isEllipsisTypeExpr (locValue argExpr) -> pure []
        _ -> (:[]) <$> mapPythonType argExpr

mapTypeLiteral :: SourceSpan -> Located PythonExpr -> CppCodeGen CppType
mapTypeLiteral span literalExpr =
  case inferPythonExprCppTypeLocated literalExpr of
    Just ty -> pure ty
    Nothing -> fallbackToStdAny span (TypeLiteral literalExpr) "could not infer literal type"

mapPythonParameter :: Located PythonParameter -> CppCodeGen CppParam
mapPythonParameter (Located span param) =
  case param of
    ParamNormal (Identifier name) mtype mdefault -> do
      baseType <- maybe (pure CppAuto) mapPythonType mtype
      refinedType <- applyParameterAnnotations span name baseType
      cppDefault <- mapM generatePythonExpr mdefault
      pure $ CppParam name refinedType cppDefault
    ParamKwOnly (Identifier name) mtype mdefault -> do
      baseType <- maybe (pure CppAuto) mapPythonType mtype
      refinedType <- applyParameterAnnotations span name baseType
      cppDefault <- mapM generatePythonExpr mdefault
      pure $ CppParam name refinedType cppDefault
    ParamVarArgs (Identifier name) mtype -> do
      elemType <- maybe ensureStdAny mapPythonType mtype
      addInclude "<vector>"
      let vectorType = CppVector elemType
      refinedType <- applyParameterAnnotations span name vectorType
      pure $ CppParam name refinedType Nothing
    ParamKwArgs (Identifier name) mtype -> do
      valueType <- maybe ensureStdAny mapPythonType mtype
      addInclude "<unordered_map>"
      addInclude "<string>"
      let mapType = CppUnorderedMap CppString valueType
      refinedType <- applyParameterAnnotations span name mapType
      pure $ CppParam name refinedType Nothing

applyParameterAnnotations :: SourceSpan -> Text -> CppType -> CppCodeGen CppType
applyParameterAnnotations span name baseType
  | T.null name = pure baseType
  | otherwise =
      let locatedCommon = Located span (CEVar (Identifier name))
          exprKey = fingerprintCommonExpr locatedCommon
          context = "parameter " <> name
      in lookupAndApplyAnnotations context exprKey baseType

ensureStdAny :: CppCodeGen CppType
ensureStdAny = do
  addInclude "<any>"
  pure stdAnyType

stdAnyType :: CppType
stdAnyType = CppClassType "std::any" []

stdMonostateType :: CppType
stdMonostateType = CppClassType "std::monostate" []

qualifiedNameToText :: QualifiedName -> Text
qualifiedNameToText qn =
  let modules = map moduleNameText (qnModule qn)
  in T.intercalate "." (modules ++ [identifierText (qnName qn)])

qualifiedNameToCpp :: QualifiedName -> Text
qualifiedNameToCpp qn =
  let modules = map moduleNameText (qnModule qn)
  in T.intercalate "::" (modules ++ [identifierText (qnName qn)])

canonicalQualifiedName :: QualifiedName -> Text
canonicalQualifiedName qn =
  let modules = map (T.toLower . moduleNameText) (qnModule qn)
  in T.intercalate "." (modules ++ [T.toLower (identifierText (qnName qn))])

identifierText :: Identifier -> Text
identifierText (Identifier txt) = txt

moduleNameText :: ModuleName -> Text
moduleNameText (ModuleName txt) = txt

describeTypeExpr :: PythonTypeExpr -> Text
describeTypeExpr = \case
  TypeName qn -> qualifiedNameToText qn
  TypeVar tv -> tv
  other -> T.pack (show other)

isNoneTypeExpr :: PythonTypeExpr -> Bool
isNoneTypeExpr = \case
  TypeName qn ->
    let simple = T.toLower (identifierText (qnName qn))
        canonical = canonicalQualifiedName qn
    in simple == "none"
       || canonical == "builtins.none"
       || canonical == "typing.none"
       || simple == "nonetype"
  _ -> False

isEllipsisTypeExpr :: PythonTypeExpr -> Bool
isEllipsisTypeExpr = \case
  TypeName qn ->
    let nameTxt = identifierText (qnName qn)
        lowered = T.toLower nameTxt
    in nameTxt == "..." || lowered == "ellipsis"
  _ -> False

fallbackToStdAny :: SourceSpan -> PythonTypeExpr -> Text -> CppCodeGen CppType
fallbackToStdAny span expr reason = do
  addInclude "<any>"
  emitWarning $
    "Python type annotation '" <> describeTypeExpr expr <> "' at "
    <> formatSpan span <> " " <> reason <> "; falling back to std::any"
  pure stdAnyType

generatePythonAssignment :: Located PythonPattern -> CppExpr -> CppCodeGen ()
generatePythonAssignment (Located _ pattern) cppExpr = case pattern of
  PatVar (Identifier name) -> do
    addDeclaration $ CppVariable name CppAuto (Just cppExpr)
  _ -> reportNotImplemented "TODO: Complex pattern assignment"

generatePythonClassMember :: Text -> Located PythonStmt -> CppCodeGen (Maybe CppDecl)
generatePythonClassMember className located@(Located span stmt) =
  case stmt of
    PyFuncDef funcDef
      | pyFuncIsAsync funcDef ->
          Just <$> generateClassMethodFallback className funcDef span "async methods are not supported"
      | not (null (pyFuncDecorators funcDef)) ->
          Just <$> generateClassMethodFallback className funcDef span "decorated methods are not yet supported"
      | otherwise ->
          Just <$> generateClassMethodDecl className funcDef
    PyAsyncFuncDef funcDef ->
      Just <$> generateClassMethodFallback className funcDef span "async methods are not supported"
    PyAssign targets expr ->
      case targets of
        [Located _ (PatVar (Identifier name))] ->
          Just <$> generateClassAttributeDecl className name expr
        _ ->
          classMemberUnsupported className span "assignment target is not supported"
    PyAnnAssign target typeExpr mValue ->
      case locValue target of
        PatVar (Identifier name) ->
          Just <$> generateAnnotatedClassAttributeDecl className name typeExpr mValue
        _ ->
          classMemberUnsupported className span "annotated assignment target is not supported"
    PyExprStmt expr
      | isDocstringExpr expr -> pure Nothing
      | otherwise ->
          classMemberUnsupported className span "expression statement is not supported at class scope"
    PyPass ->
      pure Nothing
    _ ->
      classMemberUnsupported className span ("statement '" <> T.pack (show stmt) <> "' is not supported at class scope")

extractBaseClassName :: Located PythonExpr -> CppCodeGen (Maybe Text)
extractBaseClassName (Located span expr) =
  case expr of
    PyVar (Identifier name) -> pure (Just name)
    PyConst qn -> pure (Just (qualifiedNameToCpp qn))
    PyAttribute base (Identifier attr) -> do
      prefix <- extractBaseClassName base
      pure $ fmap (<> "::" <> attr) prefix
    PySubscript base _ ->
      extractBaseClassName base
    _ -> do
      emitWarning $
        "Unsupported class base expression '" <> T.pack (show expr) <> "' at "
        <> formatSpan span <> "; skipping base"
      pure Nothing

generateClassMethodDecl :: Text -> PythonFuncDef -> CppCodeGen CppDecl
generateClassMethodDecl className funcDef = do
  let methodName = identifierText (pyFuncName funcDef)
      (paramsWithoutSelf, hasSelf) = dropSelfParameter (pyFuncParams funcDef)
  cppParams <- mapM mapPythonParameter paramsWithoutSelf
  bodyStmts <- withFunctionScope $ do
    registerParameters cppParams
    mapM (generatePythonStmt ScopeFunction) (pyFuncBody funcDef)
  let sanitizedBody = if hasSelf then replaceSelfWithThis bodyStmts else bodyStmts
  case methodName of
    "__init__" ->
      pure $ CppConstructor className cppParams sanitizedBody
    _ -> do
      returnType <- resolveMethodReturnType methodName funcDef
      pure $ CppMethod methodName returnType cppParams sanitizedBody False

generateClassMethodFallback :: Text -> PythonFuncDef -> SourceSpan -> Text -> CppCodeGen CppDecl
generateClassMethodFallback className funcDef span reason = do
  let methodName = identifierText (pyFuncName funcDef)
      (paramsWithoutSelf, _) = dropSelfParameter (pyFuncParams funcDef)
      baseMessage = "Python class '" <> className <> "', method '" <> methodName <> "' " <> reason
      fullMessage = baseMessage <> " at " <> formatSpan span <> " (runtime fallback)"
  cppParams <- mapM mapPythonParameter paramsWithoutSelf
  emitWarning fullMessage
  abortStmt <- runtimeAbortStmt fullMessage
  let body = [CppComment ("runtime fallback: " <> baseMessage), abortStmt]
  case methodName of
    "__init__" ->
      pure $ CppConstructor className cppParams body
    _ -> do
      returnType <- resolveMethodReturnType methodName funcDef
      pure $ CppMethod methodName returnType cppParams body False

generateClassAttributeDecl :: Text -> Text -> Located PythonExpr -> CppCodeGen CppDecl
generateClassAttributeDecl className name expr = do
  cppExpr <- generatePythonExpr expr
  let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated expr)
      context = className <> "." <> name <> " class attribute"
  refinedType <- refinePythonExprType context expr defaultType
  pure $ CppVariable name refinedType (Just cppExpr)

generateAnnotatedClassAttributeDecl :: Text -> Text -> Located PythonTypeExpr -> Maybe (Located PythonExpr) -> CppCodeGen CppDecl
generateAnnotatedClassAttributeDecl _ name typeExpr mValue = do
  cppType <- mapPythonType typeExpr
  cppValue <- mapM generatePythonExpr mValue
  pure $ CppVariable name cppType cppValue

classMemberUnsupported :: Text -> SourceSpan -> Text -> CppCodeGen (Maybe CppDecl)
classMemberUnsupported className span reason = do
  fallbackName <- generateTempVar
  let methodName = "__fluxus_class_fallback_" <> fallbackName
      baseMessage = "Python class '" <> className <> "': " <> reason
      fullMessage = baseMessage <> " at " <> formatSpan span <> " (runtime fallback)"
  emitWarning fullMessage
  abortStmt <- runtimeAbortStmt fullMessage
  let body = [CppComment baseMessage, abortStmt]
  pure $ Just $ CppMethod methodName CppVoid [] body False

dropSelfParameter :: [Located PythonParameter] -> ([Located PythonParameter], Bool)
dropSelfParameter params =
  case params of
    (first:rest) ->
      case locValue first of
        ParamNormal (Identifier name) _ _ | name == "self" -> (rest, True)
        _ -> (params, False)
    [] -> ([], False)

withFunctionScope :: CppCodeGen a -> CppCodeGen a
withFunctionScope action = do
  original <- gets cgsSymbolTable
  result <- action
  modify $ \s -> s { cgsSymbolTable = original }
  pure result

registerParameters :: [CppParam] -> CppCodeGen ()
registerParameters =
  mapM_ $ \(CppParam name ty _) ->
    modify $ \s -> s { cgsSymbolTable = HM.insert name ty (cgsSymbolTable s) }

resolveMethodReturnType :: Text -> PythonFuncDef -> CppCodeGen CppType
resolveMethodReturnType methodName funcDef =
  case pyFuncReturns funcDef of
    Just typeExpr ->
      if isNoneTypeExpr (locValue typeExpr)
        then pure CppVoid
        else do
          annotatedType <- mapPythonType typeExpr
          refineAnnotatedReturnType methodName annotatedType (pyFuncBody funcDef)
    Nothing -> inferFunctionReturnType methodName (pyFuncBody funcDef)

isDocstringExpr :: Located PythonExpr -> Bool
isDocstringExpr (Located _ expr) =
  case expr of
    PyLiteral (PyString _) -> True
    _ -> False

replaceSelfWithThis :: [CppStmt] -> [CppStmt]
replaceSelfWithThis = map replaceSelfStmt

replaceSelfStmt :: CppStmt -> CppStmt
replaceSelfStmt stmt =
  case stmt of
    CppExprStmt expr -> CppExprStmt (replaceSelfExpr expr)
    CppReturn mexpr -> CppReturn (replaceSelfExpr <$> mexpr)
    CppIf cond thenStmts elseStmts ->
      CppIf (replaceSelfExpr cond) (map replaceSelfStmt thenStmts) (map replaceSelfStmt elseStmts)
    CppWhile cond body ->
      CppWhile (replaceSelfExpr cond) (map replaceSelfStmt body)
    CppFor mInit mCond mPost body ->
      CppFor (replaceSelfStmt <$> mInit) (replaceSelfExpr <$> mCond) (replaceSelfExpr <$> mPost) (map replaceSelfStmt body)
    CppForRange var expr body ->
      CppForRange var (replaceSelfExpr expr) (map replaceSelfStmt body)
    CppSwitch expr cases ->
      CppSwitch (replaceSelfExpr expr) (map replaceSelfCase cases)
    CppTry tryStmts catches finallyStmts ->
      CppTry (map replaceSelfStmt tryStmts) (map replaceSelfCatch catches) (map replaceSelfStmt finallyStmts)
    CppStmtSeq stmts -> CppStmtSeq (map replaceSelfStmt stmts)
    CppBlock stmts -> CppBlock (map replaceSelfStmt stmts)
    CppDecl decl -> CppDecl (replaceSelfDecl decl)
    CppThrow mexpr -> CppThrow (replaceSelfExpr <$> mexpr)
    CppComment _ -> stmt
    CppBreak -> stmt
    CppContinue -> stmt

replaceSelfCase :: CppCase -> CppCase
replaceSelfCase caseNode =
  case caseNode of
    CppCase expr stmts -> CppCase (replaceSelfExpr expr) (map replaceSelfStmt stmts)
    CppDefault stmts -> CppDefault (map replaceSelfStmt stmts)

replaceSelfCatch :: CppCatch -> CppCatch
replaceSelfCatch (CppCatch ty name stmts) =
  CppCatch ty name (map replaceSelfStmt stmts)

replaceSelfDecl :: CppDecl -> CppDecl
replaceSelfDecl decl =
  case decl of
    CppVariable name ty initializer ->
      CppVariable name ty (replaceSelfExpr <$> initializer)
    CppFunction name ret params body ->
      CppFunction name ret (map replaceSelfParam params) (map replaceSelfStmt body)
    CppMethod name ret params body isVirtual ->
      CppMethod name ret (map replaceSelfParam params) (map replaceSelfStmt body) isVirtual
    CppConstructor name params body ->
      CppConstructor name (map replaceSelfParam params) (map replaceSelfStmt body)
    CppDestructor name body isVirtual ->
      CppDestructor name (map replaceSelfStmt body) isVirtual
    CppClass name bases members ->
      CppClass name bases (map replaceSelfDecl members)
    CppStruct name members ->
      CppStruct name (map replaceSelfDecl members)
    CppNamespace name members ->
      CppNamespace name (map replaceSelfDecl members)
    CppTemplate params inner ->
      CppTemplate params (replaceSelfDecl inner)
    CppExternC members ->
      CppExternC (map replaceSelfDecl members)
    CppAccessSpec _ -> decl
    CppCommentDecl _ -> decl
    CppUsing _ _ -> decl
    CppTypedef _ _ -> decl

replaceSelfParam :: CppParam -> CppParam
replaceSelfParam (CppParam name ty mDefault) =
  CppParam name ty (replaceSelfExpr <$> mDefault)

replaceSelfExpr :: CppExpr -> CppExpr
replaceSelfExpr expr =
  case expr of
    CppVar name | name == "self" -> CppThis
    CppVar _ -> expr
    CppLiteral _ -> expr
    CppBinary op lhs rhs -> CppBinary op (replaceSelfExpr lhs) (replaceSelfExpr rhs)
    CppConditional cond thenExpr elseExpr ->
      CppConditional (replaceSelfExpr cond) (replaceSelfExpr thenExpr) (replaceSelfExpr elseExpr)
    CppUnary op inner -> CppUnary op (replaceSelfExpr inner)
    CppCall func args -> CppCall (replaceSelfExpr func) (map replaceSelfExpr args)
    CppMember obj member -> CppMember (replaceSelfExpr obj) member
    CppPointerMember obj member -> CppPointerMember (replaceSelfExpr obj) member
    CppIndex arr idx -> CppIndex (replaceSelfExpr arr) (replaceSelfExpr idx)
    CppCast ty inner -> CppCast ty (replaceSelfExpr inner)
    CppSizeOf ty -> CppSizeOf ty
    CppNew ty args -> CppNew ty (map replaceSelfExpr args)
    CppDelete inner -> CppDelete (replaceSelfExpr inner)
    CppThis -> CppThis
    CppLambda params body -> CppLambda (map replaceSelfParam params) (map replaceSelfStmt body)
    CppMove inner -> CppMove (replaceSelfExpr inner)
    CppForward inner -> CppForward (replaceSelfExpr inner)
    CppMakeUnique ty args -> CppMakeUnique ty (map replaceSelfExpr args)
    CppMakeShared ty args -> CppMakeShared ty (map replaceSelfExpr args)
    CppBracedInit ty exprs -> CppBracedInit ty (map replaceSelfExpr exprs)
