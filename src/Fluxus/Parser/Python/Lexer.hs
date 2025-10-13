{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Python lexical analyzer
module Fluxus.Parser.Python.Lexer
  ( -- * Token types
    PythonToken(..)
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
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char hiding (newline)
import Text.Megaparsec (many, choice, try, notFollowedBy, optional, eof, getSourcePos, satisfy, takeWhileP, manyTill, (<|>), lookAhead, skipMany, anySingle)
import Control.Applicative ()
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
  | TokenFString !Text ![Text]                              -- f-string content and expressions
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

-- | Python keywords
data Keyword
  = KwAnd | KwAs | KwAssert | KwAsync | KwAwait | KwBreak | KwClass | KwContinue
  | KwDef | KwDel | KwElif | KwElse | KwExcept | KwFalse | KwFinally | KwFor
  | KwFrom | KwGlobal | KwIf | KwImport | KwIn | KwIs | KwLambda | KwNone
  | KwNonlocal | KwNot | KwOr | KwPass | KwRaise | KwReturn | KwTrue | KwTry
  | KwWhile | KwWith | KwYield
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
  } deriving (Show, Eq)

-- | Initial lexer state
initialLexerState :: LexerState
initialLexerState = LexerState
  { indentStack = [0]  -- Start with base indentation level 0
  , atLineStart = True
  }

-- | Lexer type alias with state
type PythonLexer = StateT LexerState (MP.Parsec Void Text)

-- | Run the Python lexer
runPythonLexer :: Text -> Text -> Either (MP.ParseErrorBundle Text Void) [Located PythonToken]
runPythonLexer filename input = MP.parse (evalStateT lexPython initialLexerState) (T.unpack filename) input

-- | Main lexer entry point
lexPython :: PythonLexer [Located PythonToken]
lexPython = do
  -- Initialize state
  modify $ \s -> s { indentStack = [0], atLineStart = True }
  
  -- Process all tokens
  tokens <- manyTill processLine eof
  
  -- Generate final dedent tokens
  finalState <- get
  let finalDedents = case indentStack finalState of
                      (_:xs) -> map (Located (SourceSpan "<input>" (SourcePos 0 0) (SourcePos 0 0)) . TokenDedent) (reverse xs)
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
  let sourceSpan = SourceSpan "<input>" (convertPos start) (convertPos end)
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
  
  indentState <- get
  let currentStack = indentStack indentState
      currentLevel = case currentStack of
                      [] -> 0  -- Default to 0 if empty
                      (x:_) -> x
  
  modify $ \s -> s { atLineStart = False }
  
  end <- lift getSourcePos
  let sourceSpan = SourceSpan "<input>" (convertPos start) (convertPos end)

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
  , try delimiter
  , try keyword  -- Try keywords before identifiers to ensure True/False are recognized as keywords
  , try identifier
  , newline
    ]

-- | Parse a located Python token with position information
locatedPythonToken :: PythonLexer (Located PythonToken)
locatedPythonToken = do
  -- Skip whitespace (but not newlines) before parsing token
  _ <- lift $ many (satisfy (\c -> c == ' ' || c == '\t'))
  start <- lift getSourcePos
  token <- pythonToken
  end <- lift getSourcePos
  let sourceSpan = SourceSpan "<input>" (convertPos start) (convertPos end)
  return $ Located sourceSpan token

-- | Parse keywords
keyword :: PythonLexer PythonToken
keyword = choice (map tryKeyword allKeywords)
  where
    allKeywords = [minBound .. maxBound]
    tryKeyword kw = lift $ try $ do
      let kwText = keywordToText kw
      _ <- string kwText
      notFollowedBy alphaNumChar
      return $ TokenKeyword kw

-- | Parse identifiers
identifier :: PythonLexer PythonToken
identifier = do
  first <- lift $ letterChar <|> char '_'
  rest <- lift $ many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  -- Check if the identifier is a keyword, and if so, fail to allow keyword parsing to handle it
  if isKeyword ident
    then fail $ "identifier '" ++ T.unpack ident ++ "' cannot be a keyword"
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
      quoteStr <- lift $ choice [string "\"\"\"", string "'''", string "\"", string "'"]
      let quoteChar = case T.unpack quoteStr of
            ('\"':'\"':'\"':_) -> '\"'
            ('\'':'\'':'\'':_) -> '\''
            ('\"':_) -> '\"'
            ('\'':_) -> '\''
            _ -> '\"'
      content <- lift $ manyTill (satisfy (\c -> c /= quoteChar && c /= '\\') <|> parseEscapeSequence) (string quoteStr)
      return $ TokenString (T.pack content)
      where
        parseEscapeSequence = do
          _ <- char '\\'
          c <- choice
            [ char 'n' $> '\n'
            , char 't' $> '\t'
            , char 'r' $> '\r'
            , char '\\' $> '\\'
            , char '\"' $> '\"'
            , char '\'' $> '\''
            , anySingle  -- fallback for other escape sequences
            ]
          return c
    
    parseFString = do
      _ <- lift $ char 'f' <|> char 'F'
      quoteStr <- lift $ choice [string "\"\"\"", string "'''", string "\"", string "'"]
      let quoteChar = case T.unpack quoteStr of
            ('\"':'\"':'\"':_) -> '\"'
            ('\'':'\'':'\'':_) -> '\''
            ('\"':_) -> '\"'
            ('\'':_) -> '\''
            _ -> '\"'
      content <- lift $ manyTill (satisfy (\c -> c /= quoteChar && c /= '\\') <|> parseEscapeSequence) (string quoteStr)
      let contentText = T.pack content
      -- Extract expressions from within {} braces
      let expressions = extractFStringExpressions contentText
      return $ TokenFString contentText expressions
      where
        parseEscapeSequence = do
          _ <- char '\\'
          c <- choice
            [ char 'n' $> '\n'
            , char 't' $> '\t'
            , char 'r' $> '\r'
            , char '\\' $> '\\'
            , char '\"' $> '\"'
            , char '\'' $> '\''
            , anySingle  -- fallback for other escape sequences
            ]
          return c
    
    -- Parse characters in f-string, handling escaped characters properly\n    parseFStringChar = choice\n      [ try $ do\n          _ <- char '\\\\'\n          c <- anySingle\n          return ['\\\\', c]\n      , do\n          c <- anySingle\n          return [c]\n      ]
    
    -- Extract expressions from f-string content like "Hello, {name}!" -> ["name"]
    extractFStringExpressions :: Text -> [Text]
    extractFStringExpressions text = extractBraces text []
      where
        extractBraces :: Text -> [Text] -> [Text]
        extractBraces remaining acc
          | T.null remaining = reverse acc
          | otherwise =
              case T.findIndex (== '{') remaining of
                Nothing -> reverse acc
                Just startIdx ->
                  let afterOpen = T.drop (startIdx + 1) remaining
                  in case T.findIndex (== '}') afterOpen of
                       Nothing -> reverse acc  -- Malformed f-string, no closing brace
                       Just endIdx ->
                         let exprText = T.take endIdx afterOpen
                             afterExpr = T.drop (endIdx + 1) afterOpen
                         in extractBraces afterExpr (exprText : acc)

-- | Parse bytes literals
bytesLiteral :: PythonLexer PythonToken
bytesLiteral = do
  _ <- lift $ char 'b' <|> char 'B'
  quoteStr <- lift $ choice [string "\"\"\"", string "'''", string "\"", string "'"]
  let quoteChar = case T.unpack quoteStr of
        ('\"':'\"':'\"':_) -> '\"'
        ('\'':'\'':'\'':_) -> '\''
        ('\"':_) -> '\"'
        ('\'':_) -> '\''
        _ -> '\"'
  content <- lift $ manyTill (satisfy (\c -> c /= quoteChar && c /= '\\') <|> parseEscapeSequence) (string quoteStr)
  return $ TokenBytes (T.pack content)
  where
    parseEscapeSequence = do
      _ <- char '\\'
      c <- choice
        [ char 'n' $> '\n'
        , char 't' $> '\t'
        , char 'r' $> '\r'
        , char '\\' $> '\\'
        , char '\"' $> '\"'
        , char '\'' $> '\''
        , anySingle  -- fallback for other escape sequences
        ]
      return c

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
        exponentDigits <- MP.some digitChar
        return $ 'e' : maybe "" (:[]) sign ++ exponentDigits
      
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
  return $ TokenComment $ "#" <> content

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