{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Python lexical analyzer
module Fluxus.Parser.Python.Lexer
  ( -- * Token types
    PythonToken(..)
  , FStringSegment(..)
  , Keyword(..)
  , Operator(..)
  , Delimiter(..)
    -- * Lexer
  , PythonLexer
  , runPythonLexer
  , lexPython
    -- * Individual token parsers
  , keyword
  , identifier
  , operator
  , delimiter
  , stringLiteral
  , numberLiteral
  , whitespace
  , comment
    -- * Utilities
  , isKeyword
  , keywordToText
  ) where

import Control.Monad (void, when)
import Control.Monad.State hiding (state)
import Data.Char (chr, digitToInt, isHexDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (many, choice, try, notFollowedBy, optional, eof, getSourcePos, satisfy, takeWhileP, manyTill, (<|>), lookAhead, skipMany)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Fluxus.AST.Common (SourcePos(..), SourceSpan(..), Located(..))

-- | Python token types
data PythonToken
  = -- Keywords
    TokenKeyword !Keyword
    
  -- Identifiers and literals
  | TokenIdent !Text
  | TokenString !Text
  | TokenFString ![FStringSegment]                              -- f-string split into segments with spans
  | TokenNumber !Text !Bool                             -- Text representation, isFloat
  | TokenBytes !Text
  
  -- Operators and delimiters
  | TokenOperator !Operator
  | TokenDelimiter !Delimiter
  
  -- Whitespace and structure
  | TokenNewline
  | TokenIndent !Int                                    -- Indentation level
  | TokenDedent !Int                                    -- Dedentation level
  | TokenComment !Text
  
  -- Special tokens
  | TokenEOF
  | TokenError !Text
  
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Segments inside an f-string token
data FStringSegment
  = FStringLiteralSegment !Text !SourceSpan
  | FStringExpressionSegment !Text !SourceSpan
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python keywords
data Keyword
  = KwAnd
  | KwAs
  | KwAssert
  | KwAsync
  | KwAwait
  | KwBreak
  | KwCase
  | KwClass
  | KwContinue
  | KwDef
  | KwDel
  | KwElif
  | KwElse
  | KwExcept
  | KwFalse
  | KwFinally
  | KwFor
  | KwFrom
  | KwGlobal
  | KwIf
  | KwImport
  | KwIn
  | KwIs
  | KwLambda
  | KwMatch
  | KwNone
  | KwNonlocal
  | KwNot
  | KwOr
  | KwPass
  | KwRaise
  | KwReturn
  | KwTrue
  | KwTry
  | KwWhile
  | KwWith
  | KwYield
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python operators
data Operator
  = -- Arithmetic
    OpPlus | OpMinus | OpMult | OpDiv | OpMod | OpPower | OpFloorDiv
    
  -- Bitwise
  | OpBitAnd | OpBitOr | OpBitXor | OpBitNot | OpLeftShift | OpRightShift
  
  -- Comparison
  | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
  
  -- Logical
  | OpAnd | OpOr | OpNot
  
  -- Assignment
  | OpAssign
  | OpPlusAssign | OpMinusAssign | OpMultAssign | OpDivAssign | OpModAssign
  | OpPowerAssign | OpFloorDivAssign
  | OpBitAndAssign | OpBitOrAssign | OpBitXorAssign
  | OpLeftShiftAssign | OpRightShiftAssign
  
  -- Special
  | OpWalrus                                            -- :=
  | OpArrow                                             -- ->
  | OpEllipsis                                          -- ...
  
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python delimiters
data Delimiter
  = DelimLeftParen | DelimRightParen                    -- ( )
  | DelimLeftBracket | DelimRightBracket                -- [ ]
  | DelimLeftBrace | DelimRightBrace                    -- { }
  | DelimComma | DelimColon | DelimSemicolon | DelimDot
  | DelimAt                                             -- @ (decorator)
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Lexer state for tracking indentation
data LexerState = LexerState
  { indentStack :: [Int]  -- Stack of current indentation levels
  , atLineStart :: Bool   -- Whether we're at the start of a line
  , currentFilename :: Text  -- Current filename being lexed
  } deriving (Show, Eq)

-- | Initial lexer state
initialLexerState :: LexerState
initialLexerState = LexerState
  { indentStack = [0]  -- Start with base indentation level 0
  , atLineStart = True
  , currentFilename = ""
  }

-- | Lexer type alias with state
type PythonLexer = StateT LexerState (MP.Parsec Void Text)

-- | Run the Python lexer
runPythonLexer :: Text -> Text -> Either (MP.ParseErrorBundle Text Void) [Located PythonToken]
runPythonLexer filename input = MP.parse (evalStateT lexPython (initialLexerState { currentFilename = filename })) (T.unpack filename) input

-- | Main lexer entry point
lexPython :: PythonLexer [Located PythonToken]
lexPython = do
  -- Initialize state
  modify $ \s -> s { indentStack = [0], atLineStart = True }
  
  -- Process all tokens
  tokens <- manyTill processLine eof
  
  -- Generate final dedent tokens
  finalState <- get
  let fname = currentFilename finalState
      finalDedents = case indentStack finalState of
                      (_:rest) -> map (Located (SourceSpan fname (SourcePos 0 0) (SourcePos 0 0)) . TokenDedent) 
                                      (reverse rest)
                      [] -> []
  
  return $ concat tokens ++ finalDedents

-- | Process a single line or token
processLine :: PythonLexer [Located PythonToken]
processLine = do
  -- Skip whitespace at line start if not at line start
  lexerState <- get
  when (not $ atLineStart lexerState) $ do
    lift $ skipMany (char ' ' <|> char '\t')
  
  -- Create span for tokens
  start <- lift getSourcePos
  
  -- Handle indentation at line start
  indentTokens <- if atLineStart lexerState
                   then handleIndentation
                   else return []
  
  -- Parse tokens on this line
  lineTokens <- manyTill (try locatedPythonToken) (lookAhead $ eof <|> void (Fluxus.Parser.Python.Lexer.newline))
  
  -- Parse newlines
  newlineTokens <- many (Fluxus.Parser.Python.Lexer.newline)
  
  end <- lift getSourcePos
  fname <- gets currentFilename
  let sourceSpan = SourceSpan fname (convertPos start) (convertPos end)
      locatedIndentTokens = indentTokens
      locatedNewlineTokens = map (Located sourceSpan) (map (\_ -> TokenNewline) newlineTokens)
  
  -- Update state for new line
  when (not (null newlineTokens)) $ do
    modify $ \s -> s { atLineStart = True }
  
  return $ locatedIndentTokens ++ lineTokens ++ locatedNewlineTokens

-- | Handle indentation at the start of a line
handleIndentation :: PythonLexer [Located PythonToken]
handleIndentation = do
  start <- lift getSourcePos
  -- Count spaces/tabs
  spaces <- many (char ' ' <|> char '\t')
  let level = length spaces
  
  lexerState <- get
  let currentStack = indentStack lexerState
      currentLevel = case currentStack of
                      (x:_) -> x
                      [] -> 0
  
  modify $ \s -> s { atLineStart = False }
  
  end <- lift getSourcePos
  fname <- gets currentFilename
  let sourceSpan = SourceSpan fname (convertPos start) (convertPos end)
  
  if level > currentLevel
    then do
      -- Increase indentation
      modify $ \s -> s { indentStack = level : indentStack s }
      return [Located sourceSpan (TokenIndent level)]
    else if level == currentLevel
      then return []  -- Same level
      else do
        -- Decrease indentation
        let (newStack, dedentTokens) = generateDedents level currentStack
        modify $ \s -> s { indentStack = newStack }
        return $ map (Located sourceSpan) dedentTokens
  where
    generateDedents :: Int -> [Int] -> ([Int], [PythonToken])
    generateDedents targetLevel stack = go stack []
      where
        go [] tokens = ([0], tokens)
        go (x:xs) tokens
          | x > targetLevel = go xs (TokenDedent x : tokens)
          | x == targetLevel = (x:xs, tokens)
          | otherwise = error $ "Indentation error: no matching indentation level for " ++ show targetLevel

-- | Convert Megaparsec SourcePos to our SourcePos
convertPos :: MP.SourcePos -> SourcePos
convertPos pos = SourcePos
  { posLine = MP.unPos (MP.sourceLine pos)
  , posColumn = MP.unPos (MP.sourceColumn pos)
  }

-- | Parse a single Python token
pythonToken :: PythonLexer PythonToken
pythonToken = choice
  [ comment
  , try numberLiteral
  , try stringLiteral
  , try bytesLiteral
  , try operator
  , try keyword
  , identifier
  , delimiter
  , Fluxus.Parser.Python.Lexer.newline
    ]

-- | Parse a located Python token with position information
locatedPythonToken :: PythonLexer (Located PythonToken)
locatedPythonToken = do
  -- Skip whitespace (but not newlines) before parsing token
  _ <- lift $ many (satisfy (\c -> c == ' ' || c == '\t'))
  start <- lift getSourcePos
  token <- pythonToken
  end <- lift getSourcePos
  fname <- gets currentFilename
  let sourceSpan = SourceSpan fname (convertPos start) (convertPos end)
  return $ Located sourceSpan token

-- | Parse keywords
keyword :: PythonLexer PythonToken
keyword = choice (map tryKeyword allKeywords)
  where
    allKeywords = [minBound .. maxBound]
    identChar = alphaNumChar <|> char '_'
    tryKeyword :: Keyword -> PythonLexer PythonToken
    tryKeyword kw = do
      _ <- lift $ MP.try (string (keywordToText kw) <* notFollowedBy identChar)
      pure $ TokenKeyword kw

-- | Parse identifiers
identifier :: PythonLexer PythonToken
identifier = do
  first <- lift $ letterChar <|> char '_'
  rest <- lift $ many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  if isKeyword ident
    then fail "identifier cannot be a keyword"
    else return $ TokenIdent ident

-- | Parse operators
operator :: PythonLexer PythonToken
operator = TokenOperator <$> choice
  [ lift (string "**=") $> OpPowerAssign
  , lift (string "//=") $> OpFloorDivAssign
  , lift (string "<<=") $> OpLeftShiftAssign
  , lift (string ">>=") $> OpRightShiftAssign
  , lift (string "+=") $> OpPlusAssign
  , lift (string "-=") $> OpMinusAssign
  , lift (string "*=") $> OpMultAssign
  , lift (string "/=") $> OpDivAssign
  , lift (string "%=") $> OpModAssign
  , lift (string "&=") $> OpBitAndAssign
  , lift (string "|=") $> OpBitOrAssign
  , lift (string "^=") $> OpBitXorAssign
  , lift (string "**") $> OpPower
  , lift (string "//") $> OpFloorDiv
  , lift (string "<<") $> OpLeftShift
  , lift (string ">>") $> OpRightShift
  , lift (string "==") $> OpEq
  , lift (string "!=") $> OpNe
  , lift (string "<=") $> OpLe
  , lift (string ">=") $> OpGe
  , lift (string ":=") $> OpWalrus
  , lift (string "->") $> OpArrow
  , lift (string "...") $> OpEllipsis
  , lift (string "+") $> OpPlus
  , lift (string "-") $> OpMinus
  , lift (string "*") $> OpMult
  , lift (string "/") $> OpDiv
  , lift (string "%") $> OpMod
  , lift (string "&") $> OpBitAnd
  , lift (string "|") $> OpBitOr
  , lift (string "^") $> OpBitXor
  , lift (string "~") $> OpBitNot
  , lift (string "<") $> OpLt
  , lift (string ">") $> OpGt
  , lift (string "=") $> OpAssign
  ]

-- | Parse delimiters
delimiter :: PythonLexer PythonToken
delimiter = TokenDelimiter <$> choice
  [ lift (char '(') $> DelimLeftParen
  , lift (char ')') $> DelimRightParen
  , lift (char '[') $> DelimLeftBracket
  , lift (char ']') $> DelimRightBracket
  , lift (char '{') $> DelimLeftBrace
  , lift (char '}') $> DelimRightBrace
  , lift (char ',') $> DelimComma
  , lift (char ':') $> DelimColon
  , lift (char ';') $> DelimSemicolon
  , lift (char '.') $> DelimDot
  , lift (char '@') $> DelimAt
  ]

-- | Parse string literals (including f-strings)
stringLiteral :: PythonLexer PythonToken
stringLiteral = choice
  [ try parseFString
  , parseRegularString
  ]
  where
    parseRegularString = do
      quote <- lift $ choice [string "\"\"\"", string "'''", string "\"", string "'"]
      content <- lift $ parseStringContent quote
      return $ TokenString (T.pack content)
    
    parseStringContent :: Text -> MP.Parsec Void Text String
    parseStringContent quote = case quote of
      "'''" -> manyTillNonEmpty (L.charLiteral <|> (single '\'' <* notFollowedBy (string "''"))) (string "'''")
      "\"\"\"" -> manyTillNonEmpty (L.charLiteral <|> (single '"' <* notFollowedBy (string "\"\""))) (string "\"\"\"")
      _ -> manyTill L.charLiteral (string quote)
    
    manyTillNonEmpty :: MP.Parsec Void Text Char -> MP.Parsec Void Text Text -> MP.Parsec Void Text String
    manyTillNonEmpty p end = go
      where
        go = (end $> []) <|> do
          c <- p
          (end $> [c]) <|> do
            cs <- go
            return (c:cs)
    
    parseFString = do
      _ <- lift $ char 'f' <|> char 'F'
      quote <- lift $ choice [string "\"\"\"", string "'''", string "\"", string "'"]
      contentStart <- lift getSourcePos
      (rawContent, _) <- lift $ MP.match (parseFStringContent quote)
      segments <- case buildFStringSegments contentStart rawContent of
        Left err -> fail (T.unpack err)
        Right segs -> pure segs
      _ <- lift $ string quote
      return $ TokenFString segments

-- Helpers for parsing f-strings ------------------------------------------------

parseFStringContent :: Text -> MP.Parsec Void Text ()
parseFStringContent quote = void $ MP.manyTill consumeChar (MP.lookAhead (string quote))
  where
    consumeChar = do
      c <- MP.anySingle
      when (c == '\\') $ void MP.anySingle

buildFStringSegments :: MP.SourcePos -> Text -> Either Text [FStringSegment]
buildFStringSegments start rawContent = do
  (processedText, offsetsVec) <- decodeRawStringWithMap rawContent
  segmentRecords <- splitProcessedSegments processedText
  let filename = T.pack (MP.sourceName start)
      basePos = convertPos start
      positionsVec = buildRawPositions basePos rawContent
  pure $ map (segmentToToken filename rawContent offsetsVec positionsVec) segmentRecords

data SegmentRecord
  = SegmentLiteralRecord !Text !Int !Int
  | SegmentExpressionRecord !Int !Int
  deriving (Eq, Show)

segmentToToken :: Text -> Text -> UV.Vector Int -> V.Vector SourcePos -> SegmentRecord -> FStringSegment
segmentToToken filename rawContent offsets positions = \case
  SegmentLiteralRecord text startIdx endIdx ->
    let startRaw = offsets UV.! startIdx
        endRaw = offsets UV.! endIdx
        sp = SourceSpan filename (positions V.! startRaw) (positions V.! endRaw)
    in FStringLiteralSegment text sp
  SegmentExpressionRecord startIdx endIdx ->
    let startRaw = offsets UV.! startIdx
        endRaw = offsets UV.! endIdx
        exprText = T.take (endRaw - startRaw) (T.drop startRaw rawContent)
        sp = SourceSpan filename (positions V.! startRaw) (positions V.! endRaw)
    in FStringExpressionSegment exprText sp

decodeRawStringWithMap :: Text -> Either Text (Text, UV.Vector Int)
decodeRawStringWithMap input = do
  (builder, offsetsRev, finalRaw) <- go input 0 mempty []
  let offsetsList = reverse offsetsRev ++ [finalRaw]
  pure (TL.toStrict (TB.toLazyText builder), UV.fromList offsetsList)
  where
    go txt rawOffset builder offsetsRev =
      case T.uncons txt of
        Nothing -> Right (builder, offsetsRev, rawOffset)
        Just ('\\', rest) ->
          case T.uncons rest of
            Nothing -> Left "unterminated escape in f-string literal"
            Just (esc, rest') -> do
              (mChar, consumed, rest'') <- decodeEscape esc rest'
              let builder' = case mChar of
                               Just ch -> builder <> TB.singleton ch
                               Nothing -> builder
                  offsetsRev' = case mChar of
                                  Just _ -> rawOffset : offsetsRev
                                  Nothing -> offsetsRev
              go rest'' (rawOffset + consumed) builder' offsetsRev'
        Just (c, rest) ->
          go rest (rawOffset + 1) (builder <> TB.singleton c) (rawOffset : offsetsRev)

decodeEscape :: Char -> Text -> Either Text (Maybe Char, Int, Text)
decodeEscape esc rest = case esc of
  '\\' -> Right (Just '\\', 2, rest)
  '"'  -> Right (Just '"', 2, rest)
  '\'' -> Right (Just '\'', 2, rest)
  'n'  -> Right (Just '\n', 2, rest)
  't'  -> Right (Just '\t', 2, rest)
  'r'  -> Right (Just '\r', 2, rest)
  'b'  -> Right (Just '\b', 2, rest)
  'f'  -> Right (Just '\f', 2, rest)
  '\n' -> Right (Nothing, 2, rest)
  '\r' ->
    case T.uncons rest of
      Just ('\n', rest') -> Right (Nothing, 3, rest')
      _ -> Right (Nothing, 2, rest)
  'x' ->
    let (digits, rest') = T.splitAt 2 rest
    in case T.unpack digits of
         [d1, d2] | isHexDigit d1 && isHexDigit d2 ->
           let value = digitToInt d1 * 16 + digitToInt d2
           in Right (Just (chr value), 4, rest')
         _ -> Left "invalid hexadecimal escape in f-string literal"
  _ -> Right (Just esc, 2, rest)

splitProcessedSegments :: Text -> Either Text [SegmentRecord]
splitProcessedSegments input = fmap reverse (go input 0 Nothing mempty [])
  where
    go txt offset mStart builder acc =
      case T.uncons txt of
        Nothing -> flushLiteral offset mStart builder acc
        Just ('{', _) | "{{" `T.isPrefixOf` txt ->
          let builder' = builder <> TB.singleton '{'
              offset' = offset + 2
              mStart' = Just (fromMaybe offset mStart)
          in go (T.drop 2 txt) offset' mStart' builder' acc
        Just ('}', _) | "}}" `T.isPrefixOf` txt ->
          let builder' = builder <> TB.singleton '}'
              offset' = offset + 2
              mStart' = Just (fromMaybe offset mStart)
          in go (T.drop 2 txt) offset' mStart' builder' acc
        Just ('{', rest) -> do
          acc' <- flushLiteral offset mStart builder acc
          let offsetAfterBrace = offset + 1
          (exprText, remainder) <- takeExpression rest
          let consumedTotal = T.length rest - T.length remainder
              (exprCoreText, debugLiteral) = stripFormatSpecWithLen exprText
              exprStartIdx = offsetAfterBrace
              exprEndIdx = exprStartIdx + T.length exprCoreText
              accWithDebug = maybe acc' (\txtDebug -> SegmentLiteralRecord txtDebug exprStartIdx (exprStartIdx + T.length txtDebug) : acc') debugLiteral
              accWithExpr = SegmentExpressionRecord exprStartIdx exprEndIdx : accWithDebug
              offsetAfterExpr = offsetAfterBrace + consumedTotal
          go remainder offsetAfterExpr Nothing mempty accWithExpr
        Just ('}', _) -> Left (T.pack "Single '}' in f-string literal")
        _ ->
          let (chunk, remainder) = T.break (`elem` ("{}" :: String)) txt
          in if T.null chunk
               then Left (T.pack "Unexpected state while parsing f-string literal")
               else
                 let builder' = builder <> TB.fromText chunk
                     mStart' = Just (fromMaybe offset mStart)
                     offset' = offset + T.length chunk
                 in go remainder offset' mStart' builder' acc

    flushLiteral :: Int -> Maybe Int -> TB.Builder -> [SegmentRecord] -> Either Text [SegmentRecord]
    flushLiteral currentOffset mStart builder acc =
      case mStart of
        Nothing -> Right acc
        Just startIdx ->
          let text = TL.toStrict (TB.toLazyText builder)
          in if T.null text
               then Right acc
               else Right (SegmentLiteralRecord text startIdx currentOffset : acc)

stripFormatSpecWithLen :: Text -> (Text, Maybe Text)
stripFormatSpecWithLen txt =
  let (core, _rest) = breakOnFormat txt
  in case T.unsnoc core of
       Just (withoutEq, '=') -> (withoutEq, Just core)
       _ -> (core, Nothing)

takeExpression :: Text -> Either Text (Text, Text)
takeExpression txt = goExpr 0 [] txt
  where
    goExpr :: Int -> String -> Text -> Either Text (Text, Text)
    goExpr depth acc remaining =
      case T.uncons remaining of
        Nothing -> Left "Unterminated '{' in f-string literal"
        Just ('{', rest') -> goExpr (depth + 1) ('{' : acc) rest'
        Just ('}', rest')
          | depth == 0 -> Right (T.pack (reverse acc), rest')
          | otherwise  -> goExpr (depth - 1) ('}' : acc) rest'
        Just (c, rest') -> goExpr depth (c : acc) rest'

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

buildRawPositions :: SourcePos -> Text -> V.Vector SourcePos
buildRawPositions base rawContent =
  V.fromList (scanl advance base (T.unpack rawContent))
  where
    advance pos ch
      | ch == '\n' = SourcePos (posLine pos + 1) 1
      | ch == '\r' = SourcePos (posLine pos + 1) 1
      | otherwise  = SourcePos (posLine pos) (posColumn pos + 1)

-- | Parse bytes literals
bytesLiteral :: PythonLexer PythonToken
bytesLiteral = do
  _ <- lift $ char 'b' <|> char 'B'
  quote <- lift $ choice [string "\"\"\"", string "'''", string "\"", string "'"]
  content <- lift $ manyTill L.charLiteral (string quote)
  return $ TokenBytes (T.pack content)

-- | Parse number literals
numberLiteral :: PythonLexer PythonToken
numberLiteral = do
  num <- choice [try hexNumber, try octNumber, try binNumber, decNumber]
  return $ TokenNumber num (T.any (== '.') num || T.any (\c -> c `elem` ("eE" :: String)) num)
  where
    hexNumber = do
      _ <- lift $ string "0x" <|> string "0X"
      digits <- lift $ MP.some hexDigitChar
      return $ "0x" <> T.pack digits
    
    octNumber = do
      _ <- lift $ string "0o" <|> string "0O"
      digits <- lift $ MP.some octDigitChar
      return $ "0o" <> T.pack digits
    
    binNumber = do
      _ <- lift $ string "0b" <|> string "0B"
      digits <- lift $ MP.some binDigitChar
      return $ "0b" <> T.pack digits
    
    decNumber = do
      intPart <- lift $ MP.some digitChar
      fractPart <- lift $ optional $ do
        _ <- char '.'
        MP.some digitChar
      expPart <- lift $ optional $ do
        _ <- char 'e' <|> char 'E'
        sign <- optional (char '+' <|> char '-')
        expDigits <- MP.some digitChar
        return $ 'e' : maybe "" (:[]) sign ++ expDigits
      
      let result = intPart ++ maybe "" ('.':) fractPart ++ maybe "" id expPart
      return $ T.pack result

-- | Parse whitespace (but not newlines)
whitespace :: PythonLexer ()
whitespace = void $ lift $ takeWhileP (Just "whitespace") (`elem` [' ', '\t'])

-- | Parse comments
comment :: PythonLexer PythonToken
comment = do
  _ <- lift $ char '#'
  content <- lift $ takeWhileP (Just "comment") (/= '\n')
  return $ TokenComment content

-- | Parse newlines and update line start state
newline :: PythonLexer PythonToken
newline = do
  _ <- lift $ choice [string "\r\n", string "\n", string "\r"]
  modify $ \s -> s { atLineStart = True }
  return TokenNewline

-- | Check if text is a keyword
isKeyword :: Text -> Bool
isKeyword text = text `elem` map keywordToText [minBound .. maxBound]

-- | Convert keyword to text
keywordToText :: Keyword -> Text
keywordToText = \case
  KwAnd -> "and"
  KwAs -> "as"
  KwAssert -> "assert"
  KwAsync -> "async"
  KwAwait -> "await"
  KwBreak -> "break"
  KwCase -> "case"
  KwClass -> "class"
  KwContinue -> "continue"
  KwDef -> "def"
  KwDel -> "del"
  KwElif -> "elif"
  KwElse -> "else"
  KwExcept -> "except"
  KwFalse -> "False"
  KwFinally -> "finally"
  KwFor -> "for"
  KwFrom -> "from"
  KwGlobal -> "global"
  KwIf -> "if"
  KwImport -> "import"
  KwIn -> "in"
  KwIs -> "is"
  KwLambda -> "lambda"
  KwMatch -> "match"
  KwNone -> "None"
  KwNonlocal -> "nonlocal"
  KwNot -> "not"
  KwOr -> "or"
  KwPass -> "pass"
  KwRaise -> "raise"
  KwReturn -> "return"
  KwTrue -> "True"
  KwTry -> "try"
  KwWhile -> "while"
  KwWith -> "with"
  KwYield -> "yield"