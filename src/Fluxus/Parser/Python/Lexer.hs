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
  , indentation
    -- * Utilities
  , isKeyword
  , keywordToText
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isAlpha, isDigit)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (many, choice, try, notFollowedBy, optional, eof, getSourcePos, satisfy, takeWhileP, manyTill, anySingle, (<|>))
import Text.Megaparsec.Char (string)
import Control.Applicative ((<*), (*>))
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
  
  deriving stock (Eq, Show, Generic)
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

-- | Lexer type alias
type PythonLexer = MP.Parsec Void Text

-- | Run the Python lexer
runPythonLexer :: Text -> Text -> Either (MP.ParseErrorBundle Text Void) [Located PythonToken]
runPythonLexer filename input = MP.parse lexPython (T.unpack filename) input

-- | Main lexer entry point
lexPython :: PythonLexer [Located PythonToken]
lexPython = do
  tokens <- many (locatedToken <* optional whitespace)
  eof
  return tokens
  where
    locatedToken = do
      start <- getSourcePos
      token <- pythonToken
      end <- getSourcePos
      let span = SourceSpan "<input>" (convertPos start) (convertPos end)
      return $ Located span token

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
  , try indentationToken
  , Fluxus.Parser.Python.Lexer.newline
  ]

-- | Parse keywords
keyword :: PythonLexer PythonToken
keyword = do
  kw <- choice (map tryKeyword allKeywords)
  notFollowedBy alphaNumChar
  return $ TokenKeyword kw
  where
    allKeywords = [minBound .. maxBound]
    tryKeyword kw = string (keywordToText kw) $> kw

-- | Parse identifiers
identifier :: PythonLexer PythonToken
identifier = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  if isKeyword ident
    then fail "identifier cannot be a keyword"
    else return $ TokenIdent ident

-- | Parse operators
operator :: PythonLexer PythonToken
operator = TokenOperator <$> choice
  [ string "**=" $> OpPowerAssign
  , string "//=" $> OpFloorDivAssign
  , string "<<=" $> OpLeftShiftAssign
  , string ">>=" $> OpRightShiftAssign
  , string "+=" $> OpPlusAssign
  , string "-=" $> OpMinusAssign
  , string "*=" $> OpMultAssign
  , string "/=" $> OpDivAssign
  , string "%=" $> OpModAssign
  , string "&=" $> OpBitAndAssign
  , string "|=" $> OpBitOrAssign
  , string "^=" $> OpBitXorAssign
  , string "**" $> OpPower
  , string "//" $> OpFloorDiv
  , string "<<" $> OpLeftShift
  , string ">>" $> OpRightShift
  , string "==" $> OpEq
  , string "!=" $> OpNe
  , string "<=" $> OpLe
  , string ">=" $> OpGe
  , string ":=" $> OpWalrus
  , string "->" $> OpArrow
  , string "..." $> OpEllipsis
  , string "+" $> OpPlus
  , string "-" $> OpMinus
  , string "*" $> OpMult
  , string "/" $> OpDiv
  , string "%" $> OpMod
  , string "&" $> OpBitAnd
  , string "|" $> OpBitOr
  , string "^" $> OpBitXor
  , string "~" $> OpBitNot
  , string "<" $> OpLt
  , string ">" $> OpGt
  , string "=" $> OpAssign
  ]

-- | Parse delimiters
delimiter :: PythonLexer PythonToken
delimiter = TokenDelimiter <$> choice
  [ char '(' $> DelimLeftParen
  , char ')' $> DelimRightParen
  , char '[' $> DelimLeftBracket
  , char ']' $> DelimRightBracket
  , char '{' $> DelimLeftBrace
  , char '}' $> DelimRightBrace
  , char ',' $> DelimComma
  , char ':' $> DelimColon
  , char ';' $> DelimSemicolon
  , char '.' $> DelimDot
  , char '@' $> DelimAt
  ]

-- | Parse string literals
stringLiteral :: PythonLexer PythonToken
stringLiteral = do
  quote <- choice [string "\"\"\"", string "'''", string "\"", string "'"]
  content <- manyTill L.charLiteral (string quote)
  return $ TokenString (T.pack content)

-- | Parse bytes literals
bytesLiteral :: PythonLexer PythonToken
bytesLiteral = do
  _ <- char 'b' <|> char 'B'
  quote <- choice [string "\"\"\"", string "'''", string "\"", string "'"]
  content <- manyTill L.charLiteral (string quote)
  return $ TokenBytes (T.pack content)

-- | Parse number literals
numberLiteral :: PythonLexer PythonToken
numberLiteral = do
  num <- choice [try hexNumber, try octNumber, try binNumber, decNumber]
  return $ TokenNumber num (T.any (== '.') num || T.any (`elem` "eE") num)
  where
    hexNumber = do
      _ <- string "0x" <|> string "0X"
      digits <- MP.some hexDigitChar
      return $ "0x" <> T.pack digits
    
    octNumber = do
      _ <- string "0o" <|> string "0O"
      digits <- MP.some octDigitChar
      return $ "0o" <> T.pack digits
    
    binNumber = do
      _ <- string "0b" <|> string "0B"
      digits <- MP.some binDigitChar
      return $ "0b" <> T.pack digits
    
    decNumber = do
      intPart <- MP.some digitChar
      fractPart <- optional $ do
        _ <- char '.'
        MP.some digitChar
      expPart <- optional $ do
        _ <- char 'e' <|> char 'E'
        sign <- optional (char '+' <|> char '-')
        exp <- MP.some digitChar
        return $ 'e' : maybe "" (:[]) sign ++ exp
      
      let result = intPart ++ maybe "" ('.':) fractPart ++ maybe "" id expPart
      return $ T.pack result

-- | Parse whitespace (but not newlines)
whitespace :: PythonLexer ()
whitespace = void $ takeWhileP (Just "whitespace") (`elem` [' ', '\t'])

-- | Parse comments
comment :: PythonLexer PythonToken
comment = do
  _ <- char '#'
  content <- takeWhileP (Just "comment") (/= '\n')
  return $ TokenComment content

-- | Parse newlines
newline :: PythonLexer PythonToken
newline = do
  _ <- choice [string "\r\n", string "\n", string "\r"]
  return TokenNewline

-- | Parse indentation tokens (simplified version)
indentationToken :: PythonLexer PythonToken
indentationToken = do
  level <- length <$> many (char ' ' <|> char '\t')
  if level > 0
    then return $ TokenIndent level
    else fail "no indentation"

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