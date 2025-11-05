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
import Data.List (intercalate, nub, partition)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

import Fluxus.AST.Common hiding (TypeVar)
import Fluxus.AST.Python
import Fluxus.Analysis.CommonExprLowering (pythonExprToCommon, renderCommonExpr, renderLoweringIssue, formatSpan)
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
    PyAssign patterns expr@(Located _ exprVal) ->
      case patterns of
        [Located _ (PatVar (Identifier varName))] ->
          handleSimpleAssignment scope varName expr exprVal
        _ -> do
          let msg = "Multiple assignment not implemented"
          reportFatalNotImplemented msg
          return cppNoop
    PyReturn mexpr -> do
      mcppExpr <- mapM generatePythonExpr mexpr
      return $ CppReturn mcppExpr
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
      unless varAlreadyDeclared $
        modify $ \r -> r { cgsSymbolTable = HM.insert varName CppAuto (cgsSymbolTable r) }
      cppBody <- mapM (generatePythonStmt scope) bodyStmts
      case locatedValue iterExpr of
        PyCall (Located _ (PyVar (Identifier "range"))) rangeArgs -> do
          mSpec <- parseRangeArgs rangeArgs
          case mSpec of
            Nothing -> do
              let msg = "range() with unsupported arguments"
              reportFatalUnsupported msg
              return cppNoop
            Just spec -> buildRangeLoop varAlreadyDeclared varName cppBody spec
        _ -> do
          let msg = "Only range() iteration is currently supported"
          reportFatalUnsupported msg
          return cppNoop
    PyWith _ _ ->
      runtimeFallbackStmt span "Python 'with' statement requires runtime fallback"
    PyTry _ _ _ _ ->
      runtimeFallbackStmt span "Python 'try' statement requires runtime fallback"
    PyAsyncWith _ _ ->
      runtimeFallbackStmt span "Python 'async with' statement requires runtime fallback"
    PyAsyncFor _ _ _ _ ->
      runtimeFallbackStmt span "Python 'async for' statement requires runtime fallback"
    PyAsyncFuncDef funcDef -> do
      generateAsyncFunctionFallback span funcDef
      return cppNoop
    PyRaise _ _ ->
      runtimeFallbackStmt span "Python 'raise' statement requires runtime fallback"
    PyYield _ ->
      runtimeFallbackStmt span "Python 'yield' expression requires runtime fallback"
    PyYieldFrom _ ->
      runtimeFallbackStmt span "Python 'yield from' expression requires runtime fallback"
    _ -> do
      let msg = "Python statement not implemented: " <> T.pack (show stmt)
      reportFatalNotImplemented msg
      return cppNoop
  where
    runtimeFallbackMessage :: SourceSpan -> Text -> CppCodeGen Text
    runtimeFallbackMessage loc baseMessage = do
      let message = baseMessage <> " at " <> formatSpan loc <> " (runtime fallback)"
      strict <- gets (cgcStrictMode . cgsConfig)
      if strict
        then emitWarning message
        else reportNotImplemented message
      pure message

    runtimeFallbackStmt :: SourceSpan -> Text -> CppCodeGen CppStmt
    runtimeFallbackStmt loc baseMessage = do
      message <- runtimeFallbackMessage loc baseMessage
      abortStmt <- runtimeAbortStmt message
      pure $ CppStmtSeq
        [ CppComment ("runtime fallback: " <> baseMessage)
        , abortStmt
        ]

    generateAsyncFunctionFallback :: SourceSpan -> PythonFuncDef -> CppCodeGen ()
    generateAsyncFunctionFallback loc funcDef = do
      let funcName = (\(Identifier n) -> n) (pyFuncName funcDef)
          baseMessage = "Python async function '" <> funcName <> "' requires runtime fallback"
      message <- runtimeFallbackMessage loc baseMessage
      cppParams <- mapM mapPythonParameter (pyFuncParams funcDef)
      returnType <- case pyFuncReturns funcDef of
        Just typeExpr ->
          if isNoneTypeExpr (locValue typeExpr)
            then pure CppVoid
            else mapPythonType typeExpr
        Nothing -> pure CppAuto
      abortStmt <- runtimeAbortStmt message
      let body =
            [ CppComment ("runtime fallback: async function '" <> funcName <> "'")
            , abortStmt
            ]
      addDeclaration $ CppFunction funcName returnType cppParams body
      emitInfo $ "Generated fallback stub for async function " <> funcName

    handleSimpleAssignment :: PythonScope -> Text -> Located PythonExpr -> PythonExpr -> CppCodeGen CppStmt
    handleSimpleAssignment scope' varName locatedExpr exprVal = do
      symtab <- gets cgsSymbolTable
      case exprVal of
        PyList elems ->
          handleListAssignment scope' symtab varName elems
        _ ->
          handleRegularAssignment scope' symtab varName locatedExpr

    handleListAssignment :: PythonScope -> HashMap Text CppType -> Text -> [Located PythonExpr] -> CppCodeGen CppStmt
    handleListAssignment scope' symtab varName elems = do
      (vectorType, vectorExpr) <- generatePythonListLiteral elems
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
      cppExpr <- generatePythonExpr locatedExpr
      let defaultType = fromMaybe CppAuto (inferPythonExprCppTypeLocated locatedExpr)
      refinedType <- refinePythonExprType ("assignment to " <> varName) locatedExpr defaultType
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

-- | Generate C++ from Python expressions
-- | Generate C++ expression from Python argument
generatePythonArgument :: Located PythonArgument -> CppCodeGen CppExpr
generatePythonArgument (Located _ arg) = case arg of
  ArgPositional expr -> generatePythonExpr expr
  ArgKeyword _ expr -> generatePythonExpr expr  -- Simplified: ignore keyword
  ArgStarred expr -> generatePythonExpr expr  -- Simplified
  ArgKwStarred expr -> generatePythonExpr expr  -- Simplified

-- | Segments extracted from an f-string literal
data FStringSegment
  = FStringLiteral !Text
  | FStringExpression !Text
  deriving (Eq, Show)

splitFStringSegments :: Text -> Either Text [FStringSegment]
splitFStringSegments input = go input []
  where
    go txt acc =
      case T.uncons txt of
        Nothing -> Right (reverse acc)
        Just ('{', rest)
          | "{{" `T.isPrefixOf` txt -> go (T.drop 2 txt) (addLiteral "{" acc)
          | otherwise -> do
              (exprText, remainder) <- takeExpression rest
              let (exprCoreRaw, debugLiteral) = stripFormatSpec exprText
                  exprCore = T.strip exprCoreRaw
              when (T.null exprCore) $
                Left "Empty expression in f-string"
              let accWithDebug = maybe acc (\lit -> addLiteral lit acc) debugLiteral
              go remainder (FStringExpression exprCore : accWithDebug)
        Just ('}', _)
          | "}}" `T.isPrefixOf` txt -> go (T.drop 2 txt) (addLiteral "}" acc)
          | otherwise -> Left "Single '}' in f-string"
        _ ->
          let (literal, remainder) = T.break (`elem` ("{}" :: String)) txt
          in go remainder (addLiteral literal acc)

    addLiteral lit acc
      | T.null lit = acc
      | otherwise = case acc of
          (FStringLiteral existing : rest) -> FStringLiteral (existing <> lit) : rest
          _ -> FStringLiteral lit : acc

    takeExpression :: Text -> Either Text (Text, Text)
    takeExpression txt = goExpr 0 [] txt
      where
        goExpr :: Int -> String -> Text -> Either Text (Text, Text)
        goExpr depth acc remaining =
          case T.uncons remaining of
            Nothing -> Left "Unterminated '{' in f-string"
            Just ('{', rest') -> goExpr (depth + 1) ('{' : acc) rest'
            Just ('}', rest')
              | depth == 0 -> Right (T.pack (reverse acc), rest')
              | otherwise  -> goExpr (depth - 1) ('}' : acc) rest'
            Just (c, rest') -> goExpr depth (c : acc) rest'

    stripFormatSpec :: Text -> (Text, Maybe Text)
    stripFormatSpec txt =
      let (core, _) = breakOnFormat txt
          trimmed = T.strip core
      in case T.unsnoc trimmed of
           Just (rest, '=') ->
             let exprPart = T.strip rest
             in (exprPart, Just core)
           _ -> (trimmed, Nothing)

    breakOnFormat :: Text -> (Text, Text)
    breakOnFormat txt = goFmt 0 0 0 [] txt
      where
        goFmt :: Int -> Int -> Int -> String -> Text -> (Text, Text)
        goFmt braceDepth parenDepth bracketDepth acc remaining =
          case T.uncons remaining of
            Nothing -> (T.pack (reverse acc), T.empty)
            Just (c, rest')
              | c == '{' -> goFmt (braceDepth + 1) parenDepth bracketDepth (c : acc) rest'
              | c == '}' && braceDepth > 0 -> goFmt (braceDepth - 1) parenDepth bracketDepth (c : acc) rest'
              | c == '(' -> goFmt braceDepth (parenDepth + 1) bracketDepth (c : acc) rest'
              | c == ')' && parenDepth > 0 -> goFmt braceDepth (parenDepth - 1) bracketDepth (c : acc) rest'
              | c == '[' -> goFmt braceDepth parenDepth (bracketDepth + 1) (c : acc) rest'
              | c == ']' && bracketDepth > 0 -> goFmt braceDepth parenDepth (bracketDepth - 1) (c : acc) rest'
              | (c == ':' || c == '!') && braceDepth == 0 && parenDepth == 0 && bracketDepth == 0 ->
                  (T.pack (reverse acc), rest')
              | otherwise -> goFmt braceDepth parenDepth bracketDepth (c : acc) rest'

-- | Parse a small Python expression (used in f-strings)
parseInlinePythonExpr :: Text -> Either Text (Located PythonExpr)
parseInlinePythonExpr exprText = do
  let trimmed = T.strip exprText
  when (T.null trimmed) $
    Left "Empty expression"
  tokens <- first (T.pack . MP.errorBundlePretty) $
    runPythonLexer "<f-string>" trimmed
  let eofToken = Located syntheticSpan TokenEOF
      tokenStream = tokens ++ [eofToken]
  case MP.parse (parseExpression <* MP.eof) "<f-string>" tokenStream of
    Left _ -> Left $ "Failed to parse inline Python expression: " <> trimmed
    Right parsed -> Right parsed

-- | Generate a C++ expression from a Python f-string literal
generateFStringExpr :: Text -> CppCodeGen CppExpr
generateFStringExpr raw = do
  segments <- case splitFStringSegments raw of
    Left err -> do
      reportInternalError $ "Failed to parse f-string: " <> err
      return [FStringLiteral raw]
    Right segs -> return segs
  compiled <- mapM compileSegment segments
  ossName <- generateTempVar
  addInclude "<sstream>"
  let ossVar = CppVar ossName
      ossType = CppClassType "std::ostringstream" []
      ossDecl = CppDecl (CppVariable ossName ossType Nothing)
      streamStmts = map (\expr -> CppExprStmt (CppBinary "<<" ossVar expr)) compiled
      resultExpr = CppCall (CppMember ossVar "str") []
      lambdaBody = ossDecl : streamStmts ++ [CppReturn (Just resultExpr)]
  return $ CppCall (CppLambda [] lambdaBody) []
  where
    compileSegment (FStringLiteral lit) = return $ CppLiteral (CppStringLit lit)
    compileSegment (FStringExpression exprTxt) =
      case parseInlinePythonExpr exprTxt of
        Left err -> do
          reportInternalError $ "Failed to parse f-string expression: " <> err
          return $ CppLiteral (CppStringLit ("{" <> exprTxt <> "}"))
        Right parsed -> generatePythonExpr parsed

syntheticSpan :: SourceSpan
syntheticSpan = SourceSpan "<f-string>" (SourcePos 0 0) (SourcePos 0 0)

generatePythonExpr :: Located PythonExpr -> CppCodeGen CppExpr
generatePythonExpr (Located _ expr) = case expr of
  PyLiteral lit -> case lit of
    PyFString text _ -> generateFStringExpr text
    _ -> return $ CppLiteral $ mapPythonLiteral lit
  PyConst (QualifiedName _ (Identifier name)) -> case name of
    "True"  -> return $ CppLiteral (CppBoolLit True)
    "False" -> return $ CppLiteral (CppBoolLit False)
    "None"  -> return $ CppLiteral CppNullPtr
    _        -> return $ CppVar name
  PyVar (Identifier name) -> return $ CppVar name
  PyBinaryOp op left right -> do
    cppLeft <- generatePythonExpr left
    cppRight <- generatePythonExpr right
    case op of
      OpPow -> do
        addInclude "<cmath>"
        return $ CppCall (CppVar "std::pow") [cppLeft, cppRight]
      OpDiv -> do
        let leftDiv = promoteToTrueDivOperand cppLeft
            rightDiv = promoteToTrueDivOperand cppRight
        return $ CppBinary "/" leftDiv rightDiv
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
      ([op], [left, right]) -> do
        -- Simple comparison
        cppLeft <- generatePythonExpr left
        cppRight <- generatePythonExpr right
        let cppOp = mapComparisonOp op
        return $ CppBinary cppOp cppLeft cppRight
      (ops', exprs') | length ops' + 1 == length exprs' -> do
        -- Chained comparison
        cppExprs <- mapM generatePythonExpr exprs'
        let pairs = zip3 (init cppExprs) (map mapComparisonOp ops') (tail cppExprs)
        let comparisons = map (\(l, op, r) -> CppBinary op l r) pairs
        -- Chain with && operators
        return $ foldl1 (\acc comp -> CppBinary "&&" acc comp) comparisons
      _ -> do
        reportInternalError "Invalid comparison expression"
        return $ CppLiteral $ CppBoolLit False
  PySubscript obj sliceExpr -> do
    cppObj <- generatePythonExpr obj
    case sliceExpr of
      Located _ (SliceIndex idx) -> do
        cppIdx <- generatePythonExpr idx
        return $ CppIndex cppObj cppIdx
      _ -> do
        reportUnsupported "Unsupported slice expression"
        abortExpr <- runtimeAbortCall "Python slicing is not supported in the C++ backend"
        return $ CppBinary "," abortExpr (CppLiteral (CppIntLit 0))
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
      _ -> do
        cppFunc <- generatePythonExpr func
        cppArgs <- mapM generatePythonArgument args
        return $ CppCall cppFunc cppArgs
  PyList exprs -> do
    (_, vectorExpr) <- generatePythonListLiteral exprs
    return vectorExpr
  _ -> do
    reportNotImplemented $ "TODO: Implement Python expression: " <> T.pack (show expr)
    return $ CppLiteral $ CppIntLit 0

-- | Materialize a Python list literal into a C++ vector expression
generatePythonListLiteral :: [Located PythonExpr] -> CppCodeGen (CppType, CppExpr)
generatePythonListLiteral elems = do
  addInclude "<vector>"
  cppElems <- mapM generatePythonExpr elems
  let inferredTypes = map inferPythonExprCppTypeLocated elems
      knownTypes = catMaybes inferredTypes
      hasUnknown = length knownTypes /= length elems
      combinedType = case knownTypes of
        [] -> Nothing
        (t:ts) -> foldM unifyElementType t ts
      uniqueTypes = nub knownTypes
      fallbackAny = do
        addInclude "<any>"
        return $ CppClassType "std::any" []
  elementType <-
    if null elems
      then return CppLongLong
      else case combinedType of
        Just t | not hasUnknown -> return t
        Just _ -> fallbackAny
        Nothing | not hasUnknown && not (null uniqueTypes) -> do
          addInclude "<variant>"
          return $ CppVariant uniqueTypes
        _ -> fallbackAny
  let vectorType = CppVector elementType
  return (vectorType, CppBracedInit vectorType cppElems)

inferPythonExprCppTypeLocated :: Located PythonExpr -> Maybe CppType
inferPythonExprCppTypeLocated (Located _ e) = inferPythonExprCppType e

inferPythonExprCppType :: PythonExpr -> Maybe CppType
inferPythonExprCppType = \case
  PyLiteral lit -> inferPythonLiteralType lit
  PyConst (QualifiedName _ (Identifier name)) -> case name of
    "True" -> Just CppBool
    "False" -> Just CppBool
    _ -> Nothing
  PyUnaryOp op inner -> case op of
    OpNot -> Just CppBool
    _ -> inferPythonExprCppTypeLocated inner
  PyBinaryOp _ left right -> do
    t1 <- inferPythonExprCppTypeLocated left
    t2 <- inferPythonExprCppTypeLocated right
    unifyElementType t1 t2
  PyBoolOp _ _ -> Just CppBool
  PyComparison _ _ -> Just CppBool
  _ -> Nothing

inferPythonLiteralType :: PythonLiteral -> Maybe CppType
inferPythonLiteralType = \case
  PyInt _ -> Just CppLongLong
  PyFloat _ -> Just CppDouble
  PyBool _ -> Just CppBool
  PyString _ -> Just CppString
  PyFString _ _ -> Just CppString
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
formatPrintArgument (Located _ argument) =
  case argument of
    ArgPositional expr -> formatExpr expr
    ArgKeyword _ expr -> formatExpr expr
    ArgStarred expr -> generatePythonExpr expr
    ArgKwStarred expr -> generatePythonExpr expr
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
  bodyStmts <- mapM (generatePythonStmt ScopeFunction) (pyFuncBody funcDef)
  
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
refinePythonExprType context locatedExpr defaultType =
  case pythonExprToCommon locatedExpr of
    Left err -> do
      emitInfo $ context <> ": unable to fingerprint expression for annotations - " <> renderLoweringIssue err
      pure defaultType
    Right common ->
      let exprKey = renderCommonExpr common
      in lookupAndApplyAnnotations context exprKey defaultType

mapPythonLiteral :: PythonLiteral -> CppLiteral
mapPythonLiteral = \case
  PyInt i -> CppIntLit i
  PyFloat f -> CppFloatLit f
  PyBool b -> CppBoolLit b
  PyString s -> CppStringLit s
  PyFString s _ -> CppStringLit s  -- For now, treat f-strings as regular strings
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
mapPythonParameter (Located _ param) =
  case param of
    ParamNormal (Identifier name) mtype mdefault -> do
      baseType <- maybe (pure CppAuto) mapPythonType mtype
      refinedType <- applyParameterAnnotations name baseType
      cppDefault <- mapM generatePythonExpr mdefault
      pure $ CppParam name refinedType cppDefault
    ParamKwOnly (Identifier name) mtype mdefault -> do
      baseType <- maybe (pure CppAuto) mapPythonType mtype
      refinedType <- applyParameterAnnotations name baseType
      cppDefault <- mapM generatePythonExpr mdefault
      pure $ CppParam name refinedType cppDefault
    ParamVarArgs (Identifier name) mtype -> do
      elemType <- maybe ensureStdAny mapPythonType mtype
      addInclude "<vector>"
      let vectorType = CppVector elemType
      refinedType <- applyParameterAnnotations name vectorType
      pure $ CppParam name refinedType Nothing
    ParamKwArgs (Identifier name) mtype -> do
      valueType <- maybe ensureStdAny mapPythonType mtype
      addInclude "<unordered_map>"
      addInclude "<string>"
      let mapType = CppUnorderedMap CppString valueType
      refinedType <- applyParameterAnnotations name mapType
      pure $ CppParam name refinedType Nothing

applyParameterAnnotations :: Text -> CppType -> CppCodeGen CppType
applyParameterAnnotations name baseType
  | T.null name = pure baseType
  | otherwise =
      let exprKey = renderCommonExpr (CEVar (Identifier name))
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
    other -> other

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
    _ -> expr
