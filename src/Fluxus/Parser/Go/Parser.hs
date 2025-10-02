{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Go parser that converts tokens to AST
module Fluxus.Parser.Go.Parser
  ( -- * Parser types
    GoParser
  , GoParseError(..)
    -- * Main parsing functions
  , parseGo
  , runGoParser
    -- * Top-level parsers
  , parsePackage
  , parseFile
    -- * Declaration parsers
  , parseDeclaration
  , parseFuncDecl
  , parseTypeDecl
  , parseVarDecl
  , parseConstDecl
    -- * Statement parsers
  , parseStatement
  , parseBlockStmt
  , parseIfStmt
  , parseForStmt
  , parseSwitchStmt
    -- * Expression parsers
  , parseExpression
  , parseUnaryExpr
  , parseAtomExpr
    -- * Type parsers
  , parseGoType
  , parseStructType
  , parseInterfaceType
    -- * Utility parsers
  , parseIdentifierList
  , parseExpressionList
  ) where

import Control.Monad (void, when)
import Control.Applicative (many)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Maybe (isNothing, fromMaybe)
import Text.Megaparsec hiding (many)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char ()
import qualified Data.List.NonEmpty as NE ()

import qualified Fluxus.AST.Common as Common
import Fluxus.AST.Go hiding (Identifier, QualifiedName)
import Fluxus.AST.Go (Identifier(..), QualifiedName(..))
import Fluxus.AST.Common (SourceSpan, ModuleName(..))
import Fluxus.Parser.Go.Lexer

-- | Simple chainl1 implementation for left-associative operators
chainl1 :: GoParser a -> GoParser (a -> a -> a) -> GoParser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> return x

-- | Parser error type
data GoParseError = GoParseError
  { peMessage :: !Text
  , peLocation :: !SourceSpan
  } deriving (Eq, Show)

-- | Go parser type
type GoParser = Parsec Void [Located GoToken]

-- | Run the Go parser
runGoParser :: Text -> [Located GoToken] -> Either (ParseErrorBundle [Located GoToken] Void) GoAST
runGoParser filename inputTokens = parse parseGo (T.unpack filename) inputTokens

-- | Main parser entry point
parseGo :: GoParser GoAST
parseGo = do
  package <- parsePackage
  return $ GoAST package

-- | Parse a Go package (single file for now)
parsePackage :: GoParser GoPackage
parsePackage = do
  file <- parseFile
  return $ GoPackage
    { goPackageName = goFilePackage file
    , goPackageFiles = [file]
    }

-- | Parse a Go file
parseFile :: GoParser GoFile
parseFile = do
  skipCommentsAndNewlines
  void $ goKeywordP GoKwPackage
  skipCommentsAndNewlines
  packageName <- parseGoIdentifier
  
  -- Parse imports
  skipCommentsAndNewlines
  imports <- concat <$> parseWhileLookingImport
  
  -- Parse declarations
  skipCommentsAndNewlines
  declarations <- parseWhileLookingDecl
  
  return $ GoFile
    { goFileName = "<input>"
    , goFilePackage = packageName
    , goFileImports = map (located') imports
    , goFileDecls = declarations
    }
-- | Parse imports while looking for import keywords
parseWhileLookingImport :: GoParser [[GoImport]]
parseWhileLookingImport = do
  skipCommentsAndNewlines
  result <- optional $ try $ do
    lookAhead anySingle >>= \case
      Located _ (GoTokenKeyword GoKwImport) -> parseImportDecl
      _ -> fail "Not an import"
  case result of
    Just imp -> do
      rest <- parseWhileLookingImport
      return (imp : rest)
    Nothing -> return []

-- | Parse declarations while looking for declaration keywords  
parseWhileLookingDecl :: GoParser [Located GoDecl]
parseWhileLookingDecl = do
  skipCommentsAndNewlines
  -- Check if we've reached the end of input
  isEof <- MP.atEnd
  if isEof
    then return []
    else do
      -- Look ahead to see what kind of token we have
      nextToken <- lookAhead anySingle
      case locValue nextToken of
        GoTokenKeyword kw -> case kw of
          GoKwFunc -> do
            -- Found a function declaration
            decl <- parseDeclaration
            rest <- parseWhileLookingDecl
            return (decl : rest)
          GoKwType -> do
            -- Found a type declaration
            decl <- parseDeclaration
            rest <- parseWhileLookingDecl
            return (decl : rest)
          GoKwVar -> do
            -- Found a variable declaration
            decl <- parseDeclaration
            rest <- parseWhileLookingDecl
            return (decl : rest)
          GoKwConst -> do
            -- Found a constant declaration
            decl <- parseDeclaration
            rest <- parseWhileLookingDecl
            return (decl : rest)
          GoKwImport -> do
            -- Skip import declarations and continue
            skipImportsRobust
            parseWhileLookingDecl
          _ -> do
            -- Unknown keyword, consume token and continue
            _ <- anySingle
            parseWhileLookingDecl
        GoTokenDelimiter GoDelimRightBrace -> return []
        GoTokenNewline -> do
          -- Skip newlines and continue
          skipNewlines
          parseWhileLookingDecl
        GoTokenComment _ -> do
          -- Skip comments and continue
          skipComments
          parseWhileLookingDecl
        GoTokenDelimiter GoDelimSemicolon -> do
          -- Skip semicolons and continue
          void anySingle
          parseWhileLookingDecl
        _ -> do
          -- Check if we're at end of file
          isReallyEof <- MP.atEnd
          if isReallyEof
            then return []
            else do
              -- Unknown token, consume and continue
              _ <- anySingle
              parseWhileLookingDecl

-- | Skip imports by consuming tokens until we see a function keyword
skipImportsRobust :: GoParser ()
skipImportsRobust = do
  skipCommentsAndNewlines
  result <- optional $ try $ do
    void $ goKeywordP GoKwImport
    -- Found import, skip until we find a non-import token
    skipUntilFunc
  case result of
    Just _ -> skipImportsRobust  -- Recursively skip more imports
    Nothing -> return ()  -- No more imports
  where
    skipUntilFunc = do
      skipCommentsAndNewlines
      currentToken <- lookAhead anySingle
      case locValue currentToken of
        GoTokenKeyword GoKwFunc -> return ()  -- Stop here
        GoTokenKeyword GoKwImport -> skipUntilFunc  -- Skip more imports
        _ -> do
          void anySingle  -- Consume and continue
          skipUntilFunc

-- | Parse import declarations - comprehensive version
parseImportDecl :: GoParser [GoImport]
parseImportDecl = do
  void $ goKeywordP GoKwImport
  skipCommentsAndNewlines
  choice
    [ do
        -- Multiple imports: import ( ... )
        void $ goDelimiterP GoDelimLeftParen
        skipCommentsAndNewlines
        imports <- many (parseImportSpec <* skipCommentsAndNewlines)
        void $ goDelimiterP GoDelimRightParen
        return imports
    , do
        -- Single import: import "path" or import alias "path"
        imp <- parseImportSpec
        return [imp]
    ]
  where
    parseImportSpec = choice
      [ try $ do
          -- Dot import: . "path"
          void $ goDelimiterP GoDelimDot
          importPath <- parseGoString
          return $ GoImportDot importPath
      , try $ do
          -- Blank import: _ "path"
          void $ lookAhead $ satisfy $ \case
            Located _ (GoTokenIdent "_") -> True
            _ -> False
          void $ parseGoIdentifier  -- Actually consume the identifier
          importPath <- parseGoString
          return $ GoImportBlank importPath
      , try $ do
          -- Alias import: alias "path"
          alias <- parseGoIdentifier
          importPath <- parseGoString
          return $ GoImportNormal (Just alias) importPath
      , do
          -- Normal import: "path"
          importPath <- parseGoString
          return $ GoImportNormal Nothing importPath
      ]

-- | Parse top-level declarations
parseDeclaration :: GoParser (Located GoDecl)
parseDeclaration = located $ do
  -- Try to parse function declaration first
  result <- choice
    [ try $ do
        -- Debug output for function parsing
        -- liftIO $ putStrLn "Trying to parse function declaration"
        parseFuncDecl
    , try $ do
        -- Debug output for type parsing  
        -- liftIO $ putStrLn "Trying to parse type declaration"
        parseTypeDecl
    , try $ do
        -- Debug output for var parsing
        -- liftIO $ putStrLn "Trying to parse variable declaration"
        parseVarDecl
    , try $ do
        -- Debug output for const parsing
        -- liftIO $ putStrLn "Trying to parse constant declaration"
        parseConstDecl
    , do
        -- If all specific parsers fail, check what token we're looking at
        -- and report a proper error instead of silently skipping
        nextToken <- lookAhead anySingle
        case locValue nextToken of
          GoTokenKeyword kw -> case kw of
            GoKwPackage -> do
              -- Skip package declaration (we've already processed it)
              void $ goKeywordP GoKwPackage
              _ <- parseGoIdentifier  -- Skip package name
              skipCommentsAndNewlines
              -- Try to parse the next declaration
              parseDeclarationNoLocated
            GoKwImport -> do
              -- Skip import declarations to avoid confusion
              skipImportsRobust
              skipCommentsAndNewlines
              -- Try to parse the next declaration
              parseDeclarationNoLocated
            _ -> fail $ "Syntax error: Unsupported declaration keyword '" ++ show kw ++ "'"
          GoTokenNewline -> do
            -- Skip newlines
            skipNewlines
            parseDeclarationNoLocated
          GoTokenComment _ -> do
            -- Skip comments
            skipComments
            parseDeclarationNoLocated
          _ -> fail $ "Syntax error: Unexpected token '" ++ show (locValue nextToken) ++ "' in declaration context"
    ]
  return result
  where
    -- Helper function to avoid infinite recursion - now properly reports errors
    parseDeclarationNoLocated = do
      -- Look ahead to see if we have any valid declaration tokens
      nextToken <- lookAhead anySingle
      case locValue nextToken of
        GoTokenKeyword kw -> case kw of
          GoKwFunc -> parseFuncDecl
          GoKwType -> parseTypeDecl
          GoKwVar -> parseVarDecl
          GoKwConst -> parseConstDecl
          GoKwPackage -> do
            void $ goKeywordP GoKwPackage
            _ <- parseGoIdentifier
            skipCommentsAndNewlines
            parseDeclarationNoLocated
          GoKwImport -> do
            skipImportsRobust
            skipCommentsAndNewlines
            parseDeclarationNoLocated
          _ -> fail $ "Syntax error: Expected declaration (func, type, var, const), found keyword '" ++ show kw ++ "'"
        GoTokenNewline -> do
          skipNewlines
          parseDeclarationNoLocated
        GoTokenComment _ -> do
          skipComments
          parseDeclarationNoLocated
        GoTokenDelimiter GoDelimRightBrace -> fail "Syntax error: Unexpected closing brace in declaration context"
        GoTokenDelimiter GoDelimSemicolon -> do
          void anySingle
          parseDeclarationNoLocated
        _ -> fail $ "Syntax error: Expected declaration, found '" ++ show (locValue nextToken) ++ "'"

-- | Parse function declarations (including methods and init functions)
parseFuncDecl :: GoParser GoDecl
parseFuncDecl = do
  void $ goKeywordP GoKwFunc
  
  -- Check for receiver (method)
  receiver <- optional $ try $ do
    void $ goDelimiterP GoDelimLeftParen
    recv <- parseReceiver
    void $ goDelimiterP GoDelimRightParen
    return recv
  
  name <- parseGoIdentifier
  
  -- Check for init function
  if name == Identifier "init"
    then parseInitFunction
    else parseRegularFunction receiver name
  where
    parseInitFunction = do
      -- Init functions have no parameters or return values
      void $ goDelimiterP GoDelimLeftParen
      void $ goDelimiterP GoDelimRightParen
      skipCommentsAndNewlines
      initBody <- parseBlockStmt'
      return $ GoInitDecl (located' initBody)
    
    parseRegularFunction receiver functionName = do
      -- Parse optional type parameters [T any, U comparable]
      funcTypeParams <- optional $ try $ do
        void $ goDelimiterP GoDelimLeftBracket
        params <- parseParameterList
        void $ goDelimiterP GoDelimRightBracket
        return params
      
      void $ goDelimiterP GoDelimLeftParen
      -- Parse parameters
      funcParams <- parseParameterList
      void $ goDelimiterP GoDelimRightParen
      
      -- Parse optional return type(s)
      skipCommentsAndNewlines
      funcResults <- optional $ try $ do
        -- Look ahead to make sure we're not at a '{'
        notFollowedBy (goDelimiterP GoDelimLeftBrace)
        choice
          [ do
              -- Multiple return values: (int, error)
              void $ goDelimiterP GoDelimLeftParen
              types <- parseParameterList
              void $ goDelimiterP GoDelimRightBracket
              return types
          , do
              -- Single return value: int
              t <- parseGoType
              return [GoField [] t Nothing]
          ]
      skipCommentsAndNewlines
      
      -- Parse function body - now properly handle functions without bodies (forward declarations)
      funcBody <- optional $ try $ do
        skipCommentsAndNewlines
        notFollowedBy (goDelimiterP GoDelimSemicolon)  -- Not a forward declaration
        parseBlockStmt'
      
      let func = GoFunction
            { goFuncName = Just functionName
            , goFuncTypeParams = maybe [] id funcTypeParams
            , goFuncParams = funcParams
            , goFuncResults = maybe [] id funcResults
            , goFuncBody = fmap located' funcBody
            }
      
      case receiver of
        Just recv -> return $ GoMethodDecl recv func
        Nothing -> return $ GoFuncDecl func

-- | Parse type declarations
parseTypeDecl :: GoParser GoDecl
parseTypeDecl = do
  void $ goKeywordP GoKwType
  name <- parseGoIdentifier
  -- Check if the next token is struct or interface to handle those specially
  skipCommentsAndNewlines
  nextToken <- lookAhead anySingle
  typeExpr <- case locValue nextToken of
    GoTokenKeyword GoKwStruct -> located' <$> parseStructType
    GoTokenKeyword GoKwInterface -> located' <$> parseInterfaceType
    _ -> parseGoType
  return $ GoTypeDeclStmt $ GoTypeDecl name [] False typeExpr  -- No type params, not alias

-- | Parse variable declarations
parseVarDecl :: GoParser GoDecl
parseVarDecl = do
  void $ goKeywordP GoKwVar
  choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        specs <- many (parseVarSpec <* skipNewlines)
        void $ goDelimiterP GoDelimRightParen
        return $ GoBindDecl (concat specs)
    , do
        specs <- parseVarSpec
        return $ GoBindDecl specs
    ]
  where
    parseVarSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      values <- optional $ do
        void $ goOperatorP GoOpAssign
        parseExpressionList
      
      let bindings = map (\name -> GoBinding BindVar (LHSIdents [name]) typeExpr (fromMaybe [] values)) names
      return bindings

-- | Parse constant declarations
parseConstDecl :: GoParser GoDecl
parseConstDecl = do
  void $ goKeywordP GoKwConst
  choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        specs <- many (parseConstSpec <* skipNewlines)
        void $ goDelimiterP GoDelimRightParen
        return $ GoConstDecl specs
    , do
        spec <- parseConstSpec
        return $ GoConstDecl [spec]
    ]
  where
    parseConstSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      void $ goOperatorP GoOpAssign
      values <- parseExpressionList
      
      let spec = GoConstSpec names typeExpr (Just values)
      return spec

-- | Parse variable declaration statements (inside function bodies)
parseVarStmt :: GoParser GoStmt
parseVarStmt = do
  void $ goKeywordP GoKwVar
  (names, typeExpr, values) <- parseVarSpec
  return $ GoBind $ GoBinding BindVar (LHSIdents names) typeExpr (fromMaybe [] values)
  where
    parseVarSpec = do
      names <- parseIdentifierList
      typeExpr <- optional parseGoType
      values <- optional $ do
        void $ goOperatorP GoOpAssign
        parseExpressionList
      return (names, typeExpr, values)

-- | Parse statements
parseStatement :: GoParser (Located GoStmt)
parseStatement = located $ choice
  [ try parseSimpleStmt  -- Move this to the front to test
  , try parseReturnStmt
  , try parseBreakStmt
  , try parseContinueStmt
  , try parseGotoStmt
  , try parseFallthroughStmt
  , try parseDeferStmt
  , try parseGoStmt
  , try parseIfStmt
  , try parseForStmt
  , try parseSwitchStmt
  , try parseSelectStmt
  , try parseBlockStmt'
  , try parseVarStmt
  , parseEmptyStmt
  ]

-- | Parse simple statements
parseSimpleStmt :: GoParser GoStmt
parseSimpleStmt = choice
  [ try parseShortVarDecl   -- Try short var decl first (x := value)
  , try parseAssignment     -- Then assignment (x = value)
  , try parseIncDecStmt     -- Then increment/decrement (x++, x--)
  , try parseSendStmt       -- Then channel sends (ch <- value)
  , parseExprStmt           -- Finally, expression statements
  ]

-- | Parse expression statements
parseExprStmt :: GoParser GoStmt
parseExprStmt = GoExprStmt <$> parseExpression

-- | Parse assignment
parseAssignment :: GoParser GoStmt
parseAssignment = do
  lhs <- parseExpressionList
  void $ goOperatorP GoOpAssign
  rhs <- parseExpressionList
  return $ GoBind $ GoBinding BindAssign (LHSExprs lhs) Nothing rhs

-- | Parse short variable declaration
parseShortVarDecl :: GoParser GoStmt
parseShortVarDecl = do
  names <- parseIdentifierList
  void $ goOperatorP GoOpDefine
  values <- parseExpressionList
  return $ GoBind $ GoBinding BindDefine (LHSIdents names) Nothing values

-- | Parse increment/decrement statements
parseIncDecStmt :: GoParser GoStmt
parseIncDecStmt = do
  expr <- parseExpression
  op <- choice
    [ goOperatorP GoOpIncrement $> True
    , goOperatorP GoOpDecrement $> False
    ]
  return $ GoIncDec expr op

-- | Parse send statements
parseSendStmt :: GoParser GoStmt
parseSendStmt = do
  channel <- parseExpression
  void $ goOperatorP GoOpArrow
  value <- parseExpression
  return $ GoSend channel value

-- | Parse if statements
parseIfStmt :: GoParser GoStmt
parseIfStmt = do
  void $ goKeywordP GoKwIf
  
  -- Optional simple statement
  simpleStmt <- optional $ try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiterP GoDelimSemicolon
    return $ located' stmt
  
  condition <- parseExpression
  thenBody <- located parseBlockStmt'
  
  elseBody <- optional $ do
    void $ goKeywordP GoKwElse
    choice
      [ located parseIfStmt      -- else if
      , located parseBlockStmt'  -- else block
      ]
  
  return $ GoIf simpleStmt condition thenBody elseBody

-- | Parse for statements
parseForStmt :: GoParser GoStmt
parseForStmt = do
  void $ goKeywordP GoKwFor
  
  choice
    [ try parseRangeLoop
    , try parseForClause
    , parseInfiniteFor
    ]
  where
    parseRangeLoop = do
      -- Parse range loop: for k, v := range expr or for v := range expr or for range n (Go 1.22+)
      -- Also support: for k, v = range expr, for k := range expr, etc.
      
      -- Try to parse key and value variables
      key <- optional parseGoIdentifier
      value <- optional $ do
        case key of
          Just _ -> do
            void $ goDelimiterP GoDelimComma
            parseGoIdentifier
          Nothing -> parseGoIdentifier
      
      -- Check if we have an assignment operator (:= or =)
      isDefine <- choice
        [ goOperatorP GoOpDefine $> True
        , goOperatorP GoOpAssign $> False
        ]
      
      void $ goKeywordP GoKwRange
      
      -- Parse range expression or integer (Go 1.22+)
      rangeTarget <- choice
        [ try $ do
            -- Integer range: for range 10 (Go 1.22+)
            Located _ (GoTokenInt text) <- satisfy $ \case
              Located _ (GoTokenInt _) -> True
              _ -> False
            let intVal = read $ T.unpack text
            body <- located parseBlockStmt'
            let rangeClause = GoRangeClause
                  { goRangeBinding = Just $ GoRangeBinding
                    { rangeKey = key
                    , rangeValue = value
                    , rangeDefine = isDefine
                    }
                  , goRangeExpr = located' $ GoLiteral $ GoInt intVal
                  , goRangeInteger = Just intVal
                  }
            return $ GoRange rangeClause body
        , try $ do
            -- Float range: for range 10.5 (Go 1.22+)
            Located _ (GoTokenFloat text) <- satisfy $ \case
              Located _ (GoTokenFloat _) -> True
              _ -> False
            let floatVal = read $ T.unpack text
            body <- located parseBlockStmt'
            let rangeClause = GoRangeClause
                  { goRangeBinding = Just $ GoRangeBinding
                    { rangeKey = key
                    , rangeValue = value
                    , rangeDefine = isDefine
                    }
                  , goRangeExpr = located' $ GoLiteral $ GoFloat floatVal
                  , goRangeInteger = Nothing
                  }
            return $ GoRange rangeClause body
        , do
            -- Expression range: for range slice, for range channel, etc.
            expr <- parseExpression
            body <- located parseBlockStmt'
            let rangeClause = GoRangeClause
                  { goRangeBinding = Just $ GoRangeBinding
                    { rangeKey = key
                    , rangeValue = value
                    , rangeDefine = isDefine
                    }
                  , goRangeExpr = expr
                  , goRangeInteger = Nothing
                  }
            return $ GoRange rangeClause body
        ]
      return rangeTarget
    
    parseForClause = do
      -- Check if this is a while-style for loop: for condition { ... }
      -- Look ahead to see if there's a semicolon after the first expression
      firstToken <- lookAhead anySingle
      case locValue firstToken of
        GoTokenDelimiter GoDelimLeftBrace -> do
          -- This is just "for { ... }" - infinite loop
          body <- located parseBlockStmt'
          return $ GoFor Nothing body
        _ -> do
          -- Try to parse init statement
          initStmt <- optional $ try $ do
            stmt <- parseSimpleStmt
            void $ goDelimiterP GoDelimSemicolon
            return $ located' stmt
          
          -- If we have no init, check if this is a while-style for loop
          if isNothing initStmt
            then do
              -- Try to parse condition (may be followed by semicolon or brace)
              condition <- optional $ try $ do
                expr <- parseExpression
                -- Look ahead to see what comes next
                nextToken <- lookAhead anySingle
                case locValue nextToken of
                  GoTokenDelimiter GoDelimSemicolon -> do
                    void $ goDelimiterP GoDelimSemicolon
                    return expr
                  GoTokenDelimiter GoDelimLeftBrace -> do
                    return expr
                  _ -> fail "Expected semicolon or left brace after condition"
              
              -- If we have a condition but no semicolon, this is a while-style loop
              case condition of
                Just expr -> do
                  -- This is "for condition { ... }" - treat as while loop
                  body <- located parseBlockStmt'
                  let whileClause = Just $ GoForClause
                        { goForInit = Nothing
                        , goForCond = Just expr
                        , goForPost = Nothing
                        }
                  return $ GoFor whileClause body
                Nothing -> do
                  -- Try to parse post statement (should have semicolon before)
                  post <- optional $ try $ do
                    stmt <- parseSimpleStmt
                    return $ located' stmt
                  
                  -- Ensure we parsed at least something for a for clause
                  when (isNothing post) $
                    fail "Expected for clause components"
                  
                  body <- located parseBlockStmt'
                  
                  let forClause = Just $ GoForClause
                        { goForInit = Nothing
                        , goForCond = Nothing
                        , goForPost = post
                        }
                  return $ GoFor forClause body
            else do
              -- We have an init statement, continue with normal for clause parsing
              -- Try to parse condition
              condition <- optional $ try $ do
                expr <- parseExpression
                void $ goDelimiterP GoDelimSemicolon
                return expr
              
              -- Try to parse post statement (no semicolon after)
              post <- optional $ do
                stmt <- parseSimpleStmt
                return $ located' stmt
              
              body <- located parseBlockStmt'
              
              let forClause = Just $ GoForClause
                    { goForInit = initStmt
                    , goForCond = condition
                    , goForPost = post
                    }
              return $ GoFor forClause body
    
    parseInfiniteFor = do
      body <- located parseBlockStmt'
      return $ GoFor Nothing body

-- | Parse switch statements
parseSwitchStmt :: GoParser GoStmt
parseSwitchStmt = do
  void $ goKeywordP GoKwSwitch
  
  simpleStmt <- optional $ try $ do
    stmt <- parseSimpleStmt
    void $ goDelimiterP GoDelimSemicolon
    return $ located' stmt
  
  expr <- optional parseExpression
  
  void $ goDelimiterP GoDelimLeftBrace
  cases <- many parseCaseClause
  void $ goDelimiterP GoDelimRightBrace
  
  return $ GoSwitch simpleStmt expr cases
  where
    parseCaseClause = choice
      [ do
          void $ goKeywordP GoKwCase
          exprs <- parseExpressionList
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ CaseClause (Just exprs) stmts
      , do
          void $ goKeywordP GoKwDefault
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ CaseClause Nothing stmts
      ]

-- | Parse select statements
parseSelectStmt :: GoParser GoStmt
parseSelectStmt = do
  void $ goKeywordP GoKwSelect
  void $ goDelimiterP GoDelimLeftBrace
  cases <- many parseCommClause
  void $ goDelimiterP GoDelimRightBrace
  return $ GoSelect cases
  where
    parseCommClause = choice
      [ do
          void $ goKeywordP GoKwCase
          comm <- located' <$> parseSimpleStmt
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ CaseClause (Just comm) stmts
      , do
          void $ goKeywordP GoKwDefault
          void $ goDelimiterP GoDelimColon
          stmts <- many parseStatement
          return $ CaseClause Nothing stmts
      ]

-- | Parse block statements
parseBlockStmt :: GoParser (Located GoStmt)
parseBlockStmt = located parseBlockStmt'

parseBlockStmt' :: GoParser GoStmt
parseBlockStmt' = do
  void $ goDelimiterP GoDelimLeftBrace
  skipCommentsAndNewlines
  -- Parse statements until we find the closing brace
  stmts <- manyTill (parseStatementWithSkip) (lookAhead $ goDelimiterP GoDelimRightBrace)
  void $ goDelimiterP GoDelimRightBrace
  return $ GoBlock stmts
  where
    parseStatementWithSkip = do
      skipCommentsAndNewlines
      stmt <- parseStatement
      skipCommentsAndNewlines
      return stmt

-- | Parse return statements
parseReturnStmt :: GoParser GoStmt
parseReturnStmt = do
  void $ goKeywordP GoKwReturn
  -- Check if we have a newline or semicolon immediately after return
  isEnd <- lookAhead $ optional $ choice 
    [ void $ satisfy $ \case 
        Located _ GoTokenNewline -> True
        Located _ (GoTokenDelimiter GoDelimSemicolon) -> True
        Located _ (GoTokenDelimiter GoDelimRightBrace) -> True
        _ -> False
    ]
  case isEnd of
    Just _ -> return $ GoReturn []  -- Empty return
    Nothing -> do
      exprs <- parseExpressionList
      return $ GoReturn exprs

-- | Parse break statements
parseBreakStmt :: GoParser GoStmt
parseBreakStmt = do
  void $ goKeywordP GoKwBreak
  breakLabel <- optional parseGoIdentifier
  return $ GoBreak breakLabel

-- | Parse continue statements
parseContinueStmt :: GoParser GoStmt
parseContinueStmt = do
  void $ goKeywordP GoKwContinue
  continueLabel <- optional parseGoIdentifier
  return $ GoContinue continueLabel

-- | Parse goto statements
parseGotoStmt :: GoParser GoStmt
parseGotoStmt = do
  void $ goKeywordP GoKwGoto
  gotoLabel <- parseGoIdentifier
  return $ GoGoto gotoLabel

-- | Parse fallthrough statements
parseFallthroughStmt :: GoParser GoStmt
parseFallthroughStmt = goKeywordP GoKwFallthrough $> GoFallthrough

-- | Parse defer statements
parseDeferStmt :: GoParser GoStmt
parseDeferStmt = do
  void $ goKeywordP GoKwDefer
  expr <- parseExpression
  return $ GoDefer expr

-- | Parse go statements (goroutines)
parseGoStmt :: GoParser GoStmt
parseGoStmt = do
  void $ goKeywordP GoKwGo
  expr <- parseExpression
  return $ GoGo expr

-- | Parse empty statements
parseEmptyStmt :: GoParser GoStmt
parseEmptyStmt = goDelimiterP GoDelimSemicolon $> GoEmpty

-- | Parse expressions
parseExpression :: GoParser (Located GoExpr)
parseExpression = parseOrExpr

parseOrExpr :: GoParser (Located GoExpr)
parseOrExpr = chainl1 parseAndExpr parseOrOp
  where
    parseOrOp = do
      void $ goOperatorP GoOpOr
      return $ \l r -> located' $ GoBinaryOp OpOr l r

parseAndExpr :: GoParser (Located GoExpr)
parseAndExpr = chainl1 parseEqualityExpr parseAndOp
  where
    parseAndOp = do
      void $ goOperatorP GoOpAnd
      return $ \l r -> located' $ GoBinaryOp OpAnd l r

parseEqualityExpr :: GoParser (Located GoExpr)
parseEqualityExpr = chainl1 parseRelationalExpr parseEqOp
  where
    parseEqOp = choice
      [ goOperatorP GoOpEq $> (\l r -> located' $ GoComparison OpEq l r)
      , goOperatorP GoOpNe $> (\l r -> located' $ GoComparison OpNe l r)
      ]

parseRelationalExpr :: GoParser (Located GoExpr)
parseRelationalExpr = chainl1 parseAdditiveExpr parseRelOp
  where
    parseRelOp = choice
      [ goOperatorP GoOpLt $> (\l r -> located' $ GoComparison OpLt l r)
      , goOperatorP GoOpLe $> (\l r -> located' $ GoComparison OpLe l r)
      , goOperatorP GoOpGt $> (\l r -> located' $ GoComparison OpGt l r)
      , goOperatorP GoOpGe $> (\l r -> located' $ GoComparison OpGe l r)
      ]

parseAdditiveExpr :: GoParser (Located GoExpr)
parseAdditiveExpr = chainl1 parseMultiplicativeExpr parseAddOp
  where
    parseAddOp = choice
      [ goOperatorP GoOpPlus $> (\l r -> located' $ GoBinaryOp OpAdd l r)
      , goOperatorP GoOpMinus $> (\l r -> located' $ GoBinaryOp OpSub l r)
      , goOperatorP GoOpBitOr $> (\l r -> located' $ GoBinaryOp OpOr l r)
      , goOperatorP GoOpBitXor $> (\l r -> located' $ GoBinaryOp OpXor l r)
      ]

parseMultiplicativeExpr :: GoParser (Located GoExpr)
parseMultiplicativeExpr = chainl1 parseUnaryExpr parseMulOp
  where
    parseMulOp = choice
      [ goOperatorP GoOpMult $> (\l r -> located' $ GoBinaryOp OpMul l r)
      , goOperatorP GoOpDiv $> (\l r -> located' $ GoBinaryOp OpQuo l r)
      , goOperatorP GoOpMod $> (\l r -> located' $ GoBinaryOp OpRem l r)
      , goOperatorP GoOpBitAnd $> (\l r -> located' $ GoBinaryOp OpAnd l r)
      , goOperatorP GoOpBitClear $> (\l r -> located' $ GoBinaryOp OpAndNot l r)  -- &^ implemented as AND NOT
      , goOperatorP GoOpLeftShift $> (\l r -> located' $ GoBinaryOp OpShiftL l r)
      , goOperatorP GoOpRightShift $> (\l r -> located' $ GoBinaryOp OpShiftR l r)
      ]

-- | Parse unary expressions
parseUnaryExpr :: GoParser (Located GoExpr)
parseUnaryExpr = choice
  [ do
      op <- choice
        [ goOperatorP GoOpPlus $> OpPos
        , goOperatorP GoOpMinus $> OpNeg
        , goOperatorP GoOpNot $> OpNot
        , goOperatorP GoOpBitXor $> OpBitNot
        ]
      expr <- parseUnaryExpr
      return $ located' $ GoUnaryOp op expr
  , do
      -- Address-of operator: &expr
      void $ goOperatorP GoOpAddress
      expr <- parseUnaryExpr
      return $ located' $ GoAddress expr
  , do
      -- Dereference operator: *expr
      void $ goOperatorP GoOpMult
      expr <- parseUnaryExpr
      return $ located' $ GoDeref expr
  , do
      -- Channel receive operator: <-expr
      void $ goOperatorP GoOpArrow
      expr <- parseUnaryExpr
      return $ located' $ GoReceive expr
  , parseAtomExpr
  ]

-- | Parse atomic expressions with postfix operators
parseAtomExpr :: GoParser (Located GoExpr)
parseAtomExpr = do
  atom <- parseAtom
  postfixes <- many parsePostfix
  return $ foldl applyPostfix atom postfixes
  where
    applyPostfix expr postfix = postfix expr

-- | Parse atomic expressions
parseAtom :: GoParser (Located GoExpr)
parseAtom = located $ choice
  [ parseGoLiteral
  , parseGoIdentifierExpr
  , parseParenExpr
  , parseCompositeLit
  ]

-- | Parse Go literals
parseGoLiteral :: GoParser GoExpr
parseGoLiteral = do
  literalToken <- satisfy $ \case
    Located _ (GoTokenInt _) -> True
    Located _ (GoTokenFloat _) -> True
    Located _ (GoTokenImag _) -> True
    Located _ (GoTokenString _) -> True
    Located _ (GoTokenRawString _) -> True
    Located _ (GoTokenRune _) -> True
    _ -> False
  case locValue literalToken of
    GoTokenInt text -> return $ GoLiteral $ GoInt (read (T.unpack text) :: Integer)
    GoTokenFloat text -> return $ GoLiteral $ GoFloat (read $ T.unpack text)
    GoTokenImag text -> return $ GoLiteral $ GoImag (read $ T.unpack $ T.init text)  -- Remove 'i'
    GoTokenString text -> return $ GoLiteral $ GoString text
    GoTokenRawString text -> return $ GoLiteral $ GoRawString text
    GoTokenRune runeChar -> return $ GoLiteral $ GoRune runeChar
    _ -> fail "Expected literal"  -- This should never happen due to satisfy

-- | Parse identifiers as expressions
parseGoIdentifierExpr :: GoParser GoExpr
parseGoIdentifierExpr = do
  ident <- parseGoIdentifier
  case ident of
    Common.Identifier "true" -> return $ GoLiteral $ GoBool True
    Common.Identifier "false" -> return $ GoLiteral $ GoBool False
    Common.Identifier "nil" -> return $ GoLiteral GoNil
    _ -> return $ GoIdent ident

-- | Parse parenthesized expressions
parseParenExpr :: GoParser GoExpr
parseParenExpr = do
  void $ goDelimiterP GoDelimLeftParen
  expr <- parseExpression
  void $ goDelimiterP GoDelimRightParen
  return $ locValue expr

-- | Parse composite literals
parseCompositeLit :: GoParser GoExpr
parseCompositeLit = do
  _ <- optional parseGoType
  void $ goDelimiterP GoDelimLeftBrace
  _ <- parseExpression `sepBy` goDelimiterP GoDelimComma
  void $ goDelimiterP GoDelimRightBrace
  -- TODO: Fix composite literal - need to convert type and elements to proper format
  -- For now, return a simple literal
  return $ GoLiteral (GoInt 0)  -- Placeholder

-- | Parse postfix operators
parsePostfix :: GoParser (Located GoExpr -> Located GoExpr)
parsePostfix = choice
  [ parseCall
  , parseIndex
  , parseSlice
  , parseSelector
  , parseTypeAssertion
  ]

-- | Parse function calls (including built-in functions)
parseCall :: GoParser (Located GoExpr -> Located GoExpr)
parseCall = do
  void $ goDelimiterP GoDelimLeftParen
  args <- option [] parseExpressionList  -- Allow empty argument lists
  void $ goDelimiterP GoDelimRightParen
  return $ \expr -> 
    case locValue expr of
      GoIdent name -> case parseBuiltinFunction name of
        Just builtin -> located' $ GoBuiltinCall builtin args
        Nothing -> located' $ GoCall expr args
      _ -> located' $ GoCall expr args

-- | Check if an identifier is a built-in function
parseBuiltinFunction :: Common.Identifier -> Maybe Text
parseBuiltinFunction (Common.Identifier name) = case name of
  "make" -> Just "make"
  "new" -> Just "new"
  "len" -> Just "len"
  "cap" -> Just "cap"
  "append" -> Just "append"
  "copy" -> Just "copy"
  "delete" -> Just "delete"
  "close" -> Just "close"
  "panic" -> Just "panic"
  "recover" -> Just "recover"
  "real" -> Just "real"
  "imag" -> Just "imag"
  "complex" -> Just "complex"
  "min" -> Just "min"
  "max" -> Just "max"
  "clear" -> Just "clear"
  "print" -> Just "print"
  "println" -> Just "println"
  -- Go 1.21+ builtins
  "any" -> Just "any"
  "comparable" -> Just "comparable"
  _ -> Nothing

-- | Parse array/slice indexing
parseIndex :: GoParser (Located GoExpr -> Located GoExpr)
parseIndex = do
  void $ goDelimiterP GoDelimLeftBracket
  index <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  return $ \expr -> located' $ GoIndex expr index

-- | Parse slice expressions
parseSlice :: GoParser (Located GoExpr -> Located GoExpr)
parseSlice = do
  void $ goDelimiterP GoDelimLeftBracket
  low <- optional parseExpression
  void $ goDelimiterP GoDelimColon
  high <- optional parseExpression
  max_ <- optional $ do
    void $ goDelimiterP GoDelimColon
    parseExpression
  void $ goDelimiterP GoDelimRightBracket
  
  let sliceExpr = GoSliceExpr
        { goSliceLow = low
        , goSliceHigh = high
        , goSliceMax = max_
        }
  return $ \expr -> located' $ GoSlice expr sliceExpr

-- | Parse selector expressions
parseSelector :: GoParser (Located GoExpr -> Located GoExpr)
parseSelector = do
  void $ goDelimiterP GoDelimDot
  field <- parseGoIdentifier
  return $ \expr -> located' $ GoSelector expr field

-- | Parse type assertions
parseTypeAssertion :: GoParser (Located GoExpr -> Located GoExpr)
parseTypeAssertion = do
  void $ goDelimiterP GoDelimDot
  void $ goDelimiterP GoDelimLeftParen
  _ <- parseGoType
  void $ goDelimiterP GoDelimRightParen
  -- TODO: Fix GoTypeAssert - not implemented in main AST
  -- return $ \expr -> located' $ GoTypeAssert expr typeExpr
  return $ \expr -> expr  -- For now, just return the expression unchanged

-- | Parse Go types
parseGoType :: GoParser (Located GoType)
parseGoType = located $ choice
  [ try parseArrayType
  , try parseSliceType
  , try parseMapType
  , try parseChanType
  , try parsePointerType
  , try parseFuncType
  , try parseInterfaceType
  , try parseStructType
  , parseBasicType
  , try parseTypeParam
  ]



-- | Parse basic types
parseBasicType :: GoParser GoType
parseBasicType = GoBasicType <$> parseGoIdentifier

-- | Parse array types
parseArrayType :: GoParser GoType
parseArrayType = do
  void $ goDelimiterP GoDelimLeftBracket
  size <- parseExpression
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  return $ GoArrayType size elemType

-- | Parse slice types
parseSliceType :: GoParser GoType
parseSliceType = do
  void $ goDelimiterP GoDelimLeftBracket
  void $ goDelimiterP GoDelimRightBracket
  elemType <- parseGoType
  return $ GoSliceType elemType

-- | Parse map types
parseMapType :: GoParser GoType
parseMapType = do
  void $ goKeywordP GoKwMap
  void $ goDelimiterP GoDelimLeftBracket
  keyType <- parseGoType
  void $ goDelimiterP GoDelimRightBracket
  valueType <- parseGoType
  return $ GoMapType keyType valueType

-- | Parse channel types
parseChanType :: GoParser GoType
parseChanType = choice
  [ do
      void $ goOperatorP GoOpArrow
      void $ goKeywordP GoKwChan
      elemType <- parseGoType
      return $ GoChanType GoChanRecv elemType
  , do
      void $ goKeywordP GoKwChan
      choice
        [ do
            void $ goOperatorP GoOpArrow
            elemType <- parseGoType
            return $ GoChanType GoChanSend elemType
        , do
            elemType <- parseGoType
            return $ GoChanType GoChanBidi elemType
        ]
  ]

-- | Parse pointer types
parsePointerType :: GoParser GoType
parsePointerType = do
  void $ goOperatorP GoOpMult
  baseType <- parseGoType
  return $ GoPointerType baseType

-- | Parse function types
parseFuncType :: GoParser GoType
parseFuncType = do
  void $ goKeywordP GoKwFunc
  void $ goDelimiterP GoDelimLeftParen
  _ <- parseParameterList  -- We don't need the params for now, just consume them
  void $ goDelimiterP GoDelimRightParen
  
  results <- optional $ choice
    [ do
        void $ goDelimiterP GoDelimLeftParen
        res <- parseParameterList
        void $ goDelimiterP GoDelimRightParen
        return res
    , do
        res <- parseGoType
        return [GoField [] res Nothing]
    ]
  
  -- We'll need to parse the parameters properly if we want to use them
  -- For now, we'll just return an empty parameter list
  return $ GoFuncType [] (maybe [] id results)

-- | Parse interface types with support for type constraints and embedding
parseInterfaceType :: GoParser GoType
parseInterfaceType = do
  void $ goKeywordP GoKwInterface
  void $ goDelimiterP GoDelimLeftBrace
  skipCommentsAndNewlines
  methods <- many (parseMethodOrConstraint <* skipCommentsAndNewlines)
  void $ goDelimiterP GoDelimRightBrace
  return $ GoInterfaceType methods
  where
    parseMethodOrConstraint = choice
      [ try parseEmbeddedInterface
      , try parseEmbeddedConstraint
      , try parseGenericInterface
      , parseMethodSpec
      ]
    
    parseEmbeddedInterface = do
      -- Parse embedded interface like io.Reader
      typeExpr <- parseConstraintType
      skipCommentsAndNewlines
      return $ GoEmbeddedInterface typeExpr
    
    parseEmbeddedConstraint = do
      -- Parse embedded constraint like ~int
      constraint <- parseTypeConstraint
      skipCommentsAndNewlines
      return $ GoTypeConstraint constraint
    
    parseGenericInterface = do
      -- Parse generic interface like Container[T]
      identifier <- parseGoIdentifier
      void $ goDelimiterP GoDelimLeftBracket
      typeParams <- parseGoType `sepBy` goDelimiterP GoDelimComma
      void $ goDelimiterP GoDelimRightBracket
      let genericType = GoGenericType (QualifiedName [] identifier) typeParams
      return $ GoEmbeddedInterface (located' genericType)
    
    parseMethodSpec = do
      void $ goKeywordP GoKwFunc  -- Consume 'func' keyword for interface methods
      name <- parseGoIdentifier
      void $ goDelimiterP GoDelimLeftParen
      _ <- parseParameterList
      void $ goDelimiterP GoDelimRightParen
      returnType <- parseGoType
      skipCommentsAndNewlines
      -- Consume optional semicolon at end of interface method
      void $ optional $ goDelimiterP GoDelimSemicolon
      return $ GoMethod name returnType

-- | Parse type constraints with improved approximation constraint support
parseTypeConstraint :: GoParser (Located GoConstraint)
parseTypeConstraint = located $ do
  skipCommentsAndNewlines
  -- Try to parse approximation constraint first
  result <- optional $ try $ do
    void $ goOperatorP GoOpTilde
    typeExpr <- parseConstraintType
    return $ GoApproximationConstraint typeExpr
  
  case result of
    Just constraint -> return constraint
    Nothing -> parseComplexConstraint
  where
    parseComplexConstraint = do
      first <- parseBasicConstraint
      skipCommentsAndNewlines
      rest <- many $ do
        void $ goOperatorP GoOpBitOr  -- | operator
        skipCommentsAndNewlines
        parseBasicConstraint
      skipCommentsAndNewlines
      case rest of
        [] -> return $ locValue first
        _ -> return $ GoUnionConstraint (first : rest)
    
    parseBasicConstraint = located $ do
      -- Try built-in constraints first
      result <- optional $ choice
        [ try $ do
            constraintToken <- lookAhead anySingle
            case locValue constraintToken of
              GoTokenIdent "comparable" -> do
                void anySingle
                return GoComparableConstraint
              _ -> fail "Not comparable"
        , try $ do
            constraintToken <- lookAhead anySingle
            case locValue constraintToken of
              GoTokenIdent "Ordered" -> do
                void anySingle
                return GoOrderedConstraint
              _ -> fail "Not Ordered"
        ]
      case result of
        Just constraint -> return constraint
        Nothing -> do
          typeExpr <- parseConstraintType
          return $ GoBasicConstraint typeExpr

-- | Parse types used in constraints (avoid recursion)
parseConstraintType :: GoParser (Located GoType)
parseConstraintType = located $ do
  skipCommentsAndNewlines
  choice
    [ try parseQualifiedType
    , try parsePointerTypeConstraint
    , try parseSliceTypeConstraint
    , try parseMapTypeConstraint
    , try parseChanTypeConstraint
    , try parseArrayTypeConstraint
    , parseBasicType
    ]
  where
    parseQualifiedType = do
      Common.Identifier pkgName <- parseGoIdentifier
      void $ goDelimiterP GoDelimDot  
      skipCommentsAndNewlines
      typeName <- parseGoIdentifier
      return $ GoNamedType $ QualifiedName [ModuleName pkgName] typeName
    
    parsePointerTypeConstraint = do
      void $ goOperatorP GoOpMult
      baseType <- parseConstraintType
      return $ GoPointerType baseType
    
    parseSliceTypeConstraint = do
      void $ goDelimiterP GoDelimLeftBracket
      void $ goDelimiterP GoDelimRightBracket
      elemType <- parseConstraintType
      return $ GoSliceType elemType
    
    parseMapTypeConstraint = do
      void $ goKeywordP GoKwMap
      void $ goDelimiterP GoDelimLeftBracket
      keyType <- parseConstraintType
      void $ goDelimiterP GoDelimRightBracket
      valueType <- parseConstraintType
      return $ GoMapType keyType valueType
    
    parseChanTypeConstraint = do
      choice
        [ do
            void $ goOperatorP GoOpArrow
            void $ goKeywordP GoKwChan
            elemType <- parseConstraintType
            return $ GoChanType GoChanRecv elemType
        , do
            void $ goKeywordP GoKwChan
            choice
              [ do
                  void $ goOperatorP GoOpArrow
                  elemType <- parseConstraintType
                  return $ GoChanType GoChanSend elemType
              , do
                  elemType <- parseConstraintType
                  return $ GoChanType GoChanBidi elemType
              ]
        ]
    
    parseArrayTypeConstraint = do
      void $ goDelimiterP GoDelimLeftBracket
      size <- parseExpression
      void $ goDelimiterP GoDelimRightBracket
      elemType <- parseConstraintType
      return $ GoArrayType size elemType

-- | Parse type parameters with full constraint support
parseTypeParam :: GoParser GoType
parseTypeParam = do
  skipCommentsAndNewlines
  identifier <- parseGoIdentifier
  skipCommentsAndNewlines
  constraint <- optional parseTypeConstraint
  return $ GoTypeParam identifier constraint

-- | Parse struct types
parseStructType :: GoParser GoType
parseStructType = do
  void $ goKeywordP GoKwStruct
  void $ goDelimiterP GoDelimLeftBrace
  skipCommentsAndNewlines
  fields <- many (try (parseFieldDecl <* skipCommentsAndNewlines))
  void $ goDelimiterP GoDelimRightBrace
  return $ GoStructType (concat fields)
  where
    parseFieldDecl = do
      choice
        [ try $ do
            -- Named fields: name1, name2 type [tag]
            -- 对于struct字段，单个标识符后面直接跟类型，没有逗号
            name <- parseGoIdentifier
            skipCommentsAndNewlines
            typeExpr <- parseGoType
            skipCommentsAndNewlines
            tag <- optional parseGoString
            -- Consume optional semicolon at end of field declaration
            void $ optional $ goDelimiterP GoDelimSemicolon
            return [GoField [name] typeExpr tag]
        , do
            -- Anonymous field: just type [tag]
            typeExpr <- parseGoType
            skipCommentsAndNewlines
            tag <- optional parseGoString
            -- Consume optional semicolon at end of field declaration
            void $ optional $ goDelimiterP GoDelimSemicolon
            return [GoField [] typeExpr tag]
        ]

-- | Parse method receivers
parseReceiver :: GoParser GoReceiver
parseReceiver = do
  name <- optional parseGoIdentifier
  typeExpr <- parseGoType
  return $ GoReceiver name typeExpr

-- | Utility parsers
parseGoIdentifier :: GoParser Identifier
parseGoIdentifier = do
  identifierToken <- satisfy $ \case
    Located _ (GoTokenIdent _) -> True
    _ -> False
  case locValue identifierToken of
    GoTokenIdent text -> return $ Identifier text
    _ -> fail "Expected identifier"

parseGoString :: GoParser Text
parseGoString = do
  stringToken <- satisfy $ \case
    Located _ (GoTokenString _) -> True
    Located _ (GoTokenRawString _) -> True
    _ -> False
  case locValue stringToken of
    GoTokenString text -> return text
    GoTokenRawString text -> return text
    _ -> fail "Expected string"

parseIdentifierList :: GoParser [Identifier]
parseIdentifierList = parseGoIdentifier `sepBy1` goDelimiterP GoDelimComma

parseExpressionList :: GoParser [Located GoExpr]
parseExpressionList = parseExpression `sepBy1` goDelimiterP GoDelimComma

parseParameterList :: GoParser [GoField]
parseParameterList = do
  skipCommentsAndNewlines
  -- Check if we have an empty parameter list
  isEmpty <- lookAhead $ optional $ goDelimiterP GoDelimRightParen
  case isEmpty of
    Just _ -> return []  -- Empty parameter list
    Nothing -> do
      fields <- parseFieldDecl `sepBy` goDelimiterP GoDelimComma
      return $ concat fields
  where
    parseFieldDecl = do
      skipCommentsAndNewlines
      choice
        [ try $ do
            -- Named parameters: name1, name2 type
            names <- parseIdentifierList
            skipCommentsAndNewlines
            typeExpr <- parseGoType
            return [GoField names typeExpr Nothing]
        , do
            -- Unnamed parameter: just type
            typeExpr <- parseGoType
            return [GoField [] typeExpr Nothing]
        ]

-- | Token matching utilities
goKeywordP :: GoKeyword -> GoParser ()
goKeywordP kw = void $ satisfy $ \case
  Located _ (GoTokenKeyword kw') -> kw == kw'
  _ -> False

goOperatorP :: GoOperator -> GoParser ()
goOperatorP op = void $ satisfy $ \case
  Located _ (GoTokenOperator op') -> op == op'
  _ -> False

goDelimiterP :: GoDelimiter -> GoParser ()
goDelimiterP delim = void $ satisfy $ \case
  Located _ (GoTokenDelimiter delim') -> delim == delim'
  _ -> False

skipNewlines :: GoParser ()
skipNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  _ -> False

skipComments :: GoParser ()
skipComments = void $ MP.many $ satisfy $ \case
  Located _ (GoTokenComment _) -> True
  _ -> False

skipCommentsAndNewlines :: GoParser ()
skipCommentsAndNewlines = void $ MP.many $ satisfy $ \case
  Located _ GoTokenNewline -> True
  Located _ (GoTokenComment _) -> True
  _ -> False

-- | Helper for creating located expressions
located :: GoParser a -> GoParser (Located a)
located parser = do
  value <- parser
  -- Create a dummy annotation since we can't easily get source positions
  let nodeAnn = NodeAnn Nothing [] []
  return $ Located nodeAnn value

located' :: a -> Located a
located' value = Located (NodeAnn Nothing [] []) value
