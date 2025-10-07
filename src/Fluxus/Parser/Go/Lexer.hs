{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Go lexical analyzer
module Fluxus.Parser.Go.Lexer
  ( -- * Token types
    GoToken(..)
  , GoKeyword(..)
  , GoOperator(..)
  , GoDelimiter(..)
    -- * Lexer
  , GoLexer
  , runGoLexer
  , lexGo
    -- * Individual token parsers
  , goKeyword
  , goIdentifier
  , goOperator
  , goDelimiter
  , goStringLiteral
  , goRuneLiteral
  , goNumberLiteral
  , goWhitespace
  , goComment
    -- * Utilities
  , isGoKeyword
  , goKeywordToText
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec (many, choice, try, notFollowedBy, optional, eof, getSourcePos, satisfy, takeWhileP, manyTill, anySingle, (<|>))
import Data.Functor (($>))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import qualified Fluxus.AST.Go as Go

-- | Go token types
data GoToken
  = -- Keywords
    GoTokenKeyword !GoKeyword
    
  -- Identifiers and literals
  | GoTokenIdent !Text
  | GoTokenString !Text
  | GoTokenRawString !Text                              -- Raw string literals
  | GoTokenRune !Char
  | GoTokenInt !Text
  | GoTokenFloat !Text
  | GoTokenImag !Text                                   -- Imaginary numbers
  
  -- Operators and delimiters
  | GoTokenOperator !GoOperator
  | GoTokenDelimiter !GoDelimiter
  
  -- Whitespace and structure
  | GoTokenNewline
  | GoTokenComment !Text
  
  -- Special tokens
  | GoTokenEOF
  | GoTokenError !Text
  
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go keywords
data GoKeyword
  = GoKwBreak | GoKwCase | GoKwChan | GoKwConst | GoKwContinue | GoKwDefault
  | GoKwDefer | GoKwElse | GoKwFallthrough | GoKwFor | GoKwFunc | GoKwGo
  | GoKwGoto | GoKwIf | GoKwImport | GoKwInterface | GoKwMap | GoKwPackage
  | GoKwRange | GoKwReturn | GoKwSelect | GoKwStruct | GoKwSwitch | GoKwType
  | GoKwVar
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go operators
data GoOperator
  = -- Arithmetic
    GoOpPlus | GoOpMinus | GoOpMult | GoOpDiv | GoOpMod
  
  -- Bitwise
  | GoOpBitAnd | GoOpBitOr | GoOpBitXor | GoOpBitClear | GoOpLeftShift | GoOpRightShift
  
  -- Assignment
  | GoOpAssign | GoOpDefine                             -- = and :=
  | GoOpPlusAssign | GoOpMinusAssign | GoOpMultAssign | GoOpDivAssign | GoOpModAssign
  | GoOpBitAndAssign | GoOpBitOrAssign | GoOpBitXorAssign | GoOpBitClearAssign
  | GoOpLeftShiftAssign | GoOpRightShiftAssign
  
  -- Comparison
  | GoOpEq | GoOpNe | GoOpLt | GoOpLe | GoOpGt | GoOpGe
  
  -- Logical
  | GoOpAnd | GoOpOr | GoOpNot
  
  -- Channels
  | GoOpArrow                                           -- <-

  -- Increment/Decrement
  | GoOpIncrement | GoOpDecrement                       -- ++ and --

  -- Address/Dereference
  | GoOpAddress | GoOpDeref                             -- & and *

  -- Ellipsis
  | GoOpEllipsis                                        -- ...

  -- Approximation constraint (Go 1.18+)
  | GoOpTilde                                           -- ~

  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go delimiters
data GoDelimiter
  = GoDelimLeftParen | GoDelimRightParen                -- ( )
  | GoDelimLeftBracket | GoDelimRightBracket            -- [ ]
  | GoDelimLeftBrace | GoDelimRightBrace                -- { }
  | GoDelimComma | GoDelimSemicolon | GoDelimDot | GoDelimColon
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Lexer type alias
type GoLexer = MP.Parsec Void Text

-- | Run the Go lexer
runGoLexer :: Text -> Text -> Either (MP.ParseErrorBundle Text Void) [Go.Located GoToken]
runGoLexer filename input = MP.parse lexGo (T.unpack filename) input

-- | Main lexer entry point
lexGo :: GoLexer [Go.Located GoToken]
lexGo = do
  tokens <- manyTill locatedToken eof
  return tokens
  where
    locatedToken = do
      -- Skip whitespace (but not newlines)
      void $ many (satisfy (\c -> c == ' ' || c == '\t'))
      start <- getSourcePos
      token <- goToken
      end <- getSourcePos
      let position = Go.Position { Go.posLine = MP.unPos (MP.sourceLine start), Go.posColumn = MP.unPos (MP.sourceColumn start) }
      let endPosition = Go.Position { Go.posLine = MP.unPos (MP.sourceLine end), Go.posColumn = MP.unPos (MP.sourceColumn end) }
      let tokenSpan = Just $ Go.Span { Go.spanStart = position, Go.spanEnd = endPosition }
      let nodeAnn = Go.NodeAnn { Go.annSpan = tokenSpan, Go.annLeading = [], Go.annTrailing = [] }
      return $ Go.Located nodeAnn token


-- | Parse a single Go token
goToken :: GoLexer GoToken
goToken = choice
  [ goComment
  , try goNumberLiteral
  , try goRuneLiteral
  , try goRawStringLiteral
  , try goStringLiteral
  , try goOperator
  , try goKeyword
  , goIdentifier
  , goDelimiter
  , goNewline
  ]

-- | Parse Go keywords
goKeyword :: GoLexer GoToken
goKeyword = do
  kw <- choice (map tryKeyword allKeywords)
  notFollowedBy alphaNumChar
  return $ GoTokenKeyword kw
  where
    allKeywords = [minBound .. maxBound]
    tryKeyword kw = string (goKeywordToText kw) $> kw

-- | Parse Go identifiers
goIdentifier :: GoLexer GoToken
goIdentifier = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let ident = T.pack (first : rest)
  if isGoKeyword ident
    then fail "identifier cannot be a keyword"
    else return $ GoTokenIdent ident

-- | Parse Go operators
goOperator :: GoLexer GoToken
goOperator = GoTokenOperator <$> choice
  [ string "<<=" $> GoOpLeftShiftAssign
  , string ">>=" $> GoOpRightShiftAssign
  , string "&^=" $> GoOpBitClearAssign
  , string "+=" $> GoOpPlusAssign
  , string "-=" $> GoOpMinusAssign
  , string "*=" $> GoOpMultAssign
  , string "/=" $> GoOpDivAssign
  , string "%=" $> GoOpModAssign
  , string "&=" $> GoOpBitAndAssign
  , string "|=" $> GoOpBitOrAssign
  , string "^=" $> GoOpBitXorAssign
  , string "<<" $> GoOpLeftShift
  , string ">>" $> GoOpRightShift
  , string "==" $> GoOpEq
  , string "!=" $> GoOpNe
  , string "<=" $> GoOpLe
  , string ">=" $> GoOpGe
  , string "&&" $> GoOpAnd
  , string "||" $> GoOpOr
  , string "<-" $> GoOpArrow
  , string "++" $> GoOpIncrement
  , string "--" $> GoOpDecrement
  , string ":=" $> GoOpDefine
  , string "&^" $> GoOpBitClear
  , string "..." $> GoOpEllipsis
  , string "~" $> GoOpTilde
  , string "+" $> GoOpPlus
  , string "-" $> GoOpMinus
  , string "*" $> GoOpMult
  , string "/" $> GoOpDiv
  , string "%" $> GoOpMod
  , string "&" $> GoOpAddress
  , string "|" $> GoOpBitOr
  , string "^" $> GoOpBitXor
  , string "!" $> GoOpNot
  , string "<" $> GoOpLt
  , string ">" $> GoOpGt
  , string "=" $> GoOpAssign
  ]

-- | Parse Go delimiters
goDelimiter :: GoLexer GoToken
goDelimiter = GoTokenDelimiter <$> choice
  [ char '(' $> GoDelimLeftParen
  , char ')' $> GoDelimRightParen
  , char '[' $> GoDelimLeftBracket
  , char ']' $> GoDelimRightBracket
  , char '{' $> GoDelimLeftBrace
  , char '}' $> GoDelimRightBrace
  , char ',' $> GoDelimComma
  , char ';' $> GoDelimSemicolon
  , char '.' $> GoDelimDot
  , char ':' $> GoDelimColon
  ]

-- | Parse Go string literals
goStringLiteral :: GoLexer GoToken
goStringLiteral = do
  _ <- char '"'
  content <- many goStringChar
  _ <- char '"'
  return $ GoTokenString (T.pack content)
  where
    goStringChar = choice
      [ try (char '\\' *> escapeChar)
      , satisfy (/= '"')
      ]
    escapeChar = choice
      [ char 'n' $> '\n'
      , char 't' $> '\t'
      , char 'r' $> '\r'
      , char '\\' $> '\\'
      , char '"' $> '"'
      , char '\'' $> '\''
      ]

-- | Parse Go raw string literals
goRawStringLiteral :: GoLexer GoToken
goRawStringLiteral = do
  _ <- char '`'
  content <- many (satisfy (/= '`'))
  _ <- char '`'
  return $ GoTokenRawString (T.pack content)

-- | Parse Go rune literals
goRuneLiteral :: GoLexer GoToken
goRuneLiteral = do
  _ <- char '\''
  c <- choice
    [ try (char '\\' *> escapeChar)
    , satisfy (/= '\'')
    ]
  _ <- char '\''
  return $ GoTokenRune c
  where
    escapeChar = choice
      [ char 'n' $> '\n'
      , char 't' $> '\t'
      , char 'r' $> '\r'
      , char '\\' $> '\\'
      , char '"' $> '"'
      , char '\'' $> '\''
      ]

-- | Parse Go number literals
goNumberLiteral :: GoLexer GoToken
goNumberLiteral = choice
  [ try goImaginary
  , try goFloat
  , try goHexInt
  , try goOctInt
  , try goBinInt
  , goDecInt
  ]
  where
    goImaginary = do
      numToken <- choice [try goFloat, goDecInt]
      _ <- char 'i'
      let numText = case numToken of
            GoTokenFloat t -> t
            GoTokenInt t -> t
            _ -> ""
      return $ GoTokenImag (numText <> "i")

    goFloat = do
      intPart <- MP.some digitChar
      fractPart <- optional $ do
        _ <- char '.'
        MP.some digitChar
      expPart <- optional $ do
        _ <- char 'e' <|> char 'E'
        signChar <- optional (char '+' <|> char '-')
        expDigits <- MP.some digitChar
        return $ 'e' : maybe "" (:[]) signChar ++ expDigits

      -- At least one of fractPart or expPart must be present for a float
      case (fractPart, expPart) of
        (Nothing, Nothing) -> fail "Not a float - missing fractional or exponent part"
        _ -> do
          let result = intPart ++ maybe "" ('.':) fractPart ++ maybe "" id expPart
          return $ GoTokenFloat (T.pack result)

    goHexInt = do
      _ <- string "0x" <|> string "0X"
      digits <- MP.some hexDigitChar
      return $ GoTokenInt ("0x" <> T.pack digits)

    goOctInt = do
      _ <- string "0o" <|> string "0O" <|> string "0"
      digits <- MP.some octDigitChar
      return $ GoTokenInt ("0o" <> T.pack digits)

    goBinInt = do
      _ <- string "0b" <|> string "0B"
      digits <- MP.some binDigitChar
      return $ GoTokenInt ("0b" <> T.pack digits)

    goDecInt = do
      digits <- MP.some digitChar
      return $ GoTokenInt (T.pack digits)

-- | Parse Go whitespace (but not newlines)
goWhitespace :: GoLexer ()
goWhitespace = void $ takeWhileP (Just "whitespace") (`elem` [' ', '\t'])

-- | Parse Go comments
goComment :: GoLexer GoToken
goComment = choice
  [ lineComment
  , blockComment
  ]
  where
    lineComment = do
      _ <- string "//"
      content <- takeWhileP (Just "comment") (/= '\n')
      return $ GoTokenComment content

    blockComment = do
      _ <- string "/*"
      content <- manyTill anySingle (string "*/")
      return $ GoTokenComment (T.pack content)

-- | Parse newlines
goNewline :: GoLexer GoToken
goNewline = do
  _ <- choice [string "\r\n", string "\n", string "\r"]
  return GoTokenNewline

-- | Check if text is a Go keyword
isGoKeyword :: Text -> Bool
isGoKeyword text = text `elem` map goKeywordToText [minBound .. maxBound]

-- | Convert Go keyword to text
goKeywordToText :: GoKeyword -> Text
goKeywordToText = \case
  GoKwBreak -> "break"
  GoKwCase -> "case"
  GoKwChan -> "chan"
  GoKwConst -> "const"
  GoKwContinue -> "continue"
  GoKwDefault -> "default"
  GoKwDefer -> "defer"
  GoKwElse -> "else"
  GoKwFallthrough -> "fallthrough"
  GoKwFor -> "for"
  GoKwFunc -> "func"
  GoKwGo -> "go"
  GoKwGoto -> "goto"
  GoKwIf -> "if"
  GoKwImport -> "import"
  GoKwInterface -> "interface"
  GoKwMap -> "map"
  GoKwPackage -> "package"
  GoKwRange -> "range"
  GoKwReturn -> "return"
  GoKwSelect -> "select"
  GoKwStruct -> "struct"
  GoKwSwitch -> "switch"
  GoKwType -> "type"
  GoKwVar -> "var"