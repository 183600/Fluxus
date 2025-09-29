{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Core types for the Fluxus compiler, providing the foundation for
-- compilation state management, error handling, and configuration.
module Fluxus.Internal.Types
  ( -- * Core Compiler Monad
    CompilerM
    
    -- * Compiler State
  , CompilerState(..)
  , initialCompilerState
  , enterScope
  , exitScope
  , lookupSymbol
  , insertSymbol
  
    -- * Compiler Environment
  , CompilerEnv(..)
  , CppVersion(..)
  , OptimizationLevel(..)
  , defaultCompilerEnv
  
    -- * Error Types
  , CompilerError(..)
  , TypeErrorInfo(..)
  , SourceSpan(..)
  , SourcePos(..)
  
    -- * Symbol Tables
  , SymbolTable
  , TypeTable
  
    -- * Helper Types
  , Identifier(..)
  , QualifiedName(..)
  , Type(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Maybe (listToMaybe, mapMaybe)

-- =============================================================================
-- Helper Types (normally from Fluxus.AST.Common)
-- =============================================================================

-- | A simple identifier in the source code
newtype Identifier = Identifier Text
  deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (NFData, Hashable)


-- | A qualified name with namespace path
data QualifiedName = QualifiedName
  { qnNamespace :: [Text]
  , qnName :: Text
  } deriving stock (Eq, Show, Ord)

instance Hashable QualifiedName where
  hashWithSalt salt (QualifiedName ns n) = hashWithSalt salt (ns, n)


-- | Basic type representation (simplified for this example)
data Type
  = TInt
  | TFloat
  | TBool
  | TString
  | TUnit
  | TFunction [Type] Type
  | TCustom QualifiedName
  | TGeneric Text
  deriving stock (Eq, Show, Generic)


-- =============================================================================
-- Source Location Types
-- =============================================================================

-- | A position in the source file
data SourcePos = SourcePos
  { spLine   :: !Int  -- ^ Line number (1-based)
  , spColumn :: !Int  -- ^ Column number (1-based)
  } deriving stock (Eq, Show, Ord, Generic)
    
-- | A span of source code between two positions
data SourceSpan = SourceSpan
  { ssStart :: !SourcePos  -- ^ Starting position
  , ssEnd   :: !SourcePos  -- ^ Ending position
  , ssFile  :: !Text       -- ^ Source file name
  } deriving stock (Eq, Show, Generic)
    
-- =============================================================================
-- Configuration Types
-- =============================================================================

-- | Supported C++ standard versions
data CppVersion
  = Cpp11  -- ^ C++11 standard
  | Cpp14  -- ^ C++14 standard
  | Cpp17  -- ^ C++17 standard
  | Cpp20  -- ^ C++20 standard
  | Cpp23  -- ^ C++23 standard
  deriving stock (Eq, Show, Ord, Generic)
  
-- | Optimization levels for code generation
data OptimizationLevel
  = O0  -- ^ No optimization (fastest compilation)
  | O1  -- ^ Basic optimization
  | O2  -- ^ Standard optimization
  | O3  -- ^ Aggressive optimization
  | Os  -- ^ Optimize for size
  deriving stock (Eq, Show, Ord, Generic)
  
-- =============================================================================
-- Core Compiler Types
-- =============================================================================

-- | The core monad for the Fluxus compiler, encapsulating state, 
-- environment, and error handling.
type CompilerM = ReaderT CompilerEnv (StateT CompilerState (Except CompilerError))

-- | A symbol table mapping identifiers to their types
type SymbolTable = HashMap Identifier Type

-- | A type table for top-level type definitions (structs, enums, etc.)
type TypeTable = HashMap QualifiedName Type

-- | Represents the mutable state of the compiler as it processes code
data CompilerState = CompilerState
  { -- | A stack of symbol tables, where the head is the innermost scope
    csSymbolTable :: ![SymbolTable]

    -- | Global table for top-level type definitions
  , csTypeTable :: !TypeTable

    -- | Counter for generating unique identifiers (e.g., for desugaring)
  , csNextId :: !Int

    -- | Accumulated warnings during compilation
  , csWarnings :: ![Text]
  } deriving stock (Show, Generic)

-- | Immutable configuration for the compiler
data CompilerEnv = CompilerEnv
  { -- | Path to the source file being compiled
    ceSourceFile :: !Text

    -- | Optimization level for code generation
  , ceOptimizationLevel :: !OptimizationLevel

    -- | Target C++ standard version
  , ceTargetCppVersion :: !CppVersion

    -- | Enable verbose debug output
  , ceDebugMode :: !Bool

    -- | Additional include paths for imports
  , ceIncludePaths :: ![Text]
  } deriving stock (Show, Generic)
    
-- =============================================================================
-- Error Types
-- =============================================================================

-- | Detailed information about a type error
data TypeErrorInfo = TypeErrorInfo
  { -- | Location where the error occurred
    teLocation :: !SourceSpan

    -- | The type that was expected
  , teExpected :: !Type

    -- | The actual type that was found
  , teActual   :: !Type

    -- | Additional context information
  , teContext  :: !Text

    -- | Optional suggestion for fixing the error
  , teSuggestion :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)
    
-- | All possible compiler errors with structured information
data CompilerError
  = -- | Parse error with location and message
    ParseError !SourceSpan !Text

    -- | Type checking error with detailed information
  | TypeError !TypeErrorInfo

    -- | Semantic analysis error
  | AnalysisError !SourceSpan !Text !(Maybe Text)

    -- | Code generation error
  | CodeGenError !Text

    -- | Error during name resolution
  | NameResolutionError !SourceSpan !Identifier !Text

    -- | Duplicate definition error
  | DuplicateDefinition !SourceSpan !Identifier !SourceSpan

    -- | Import/module system error
  | ImportError !Text !Text
  deriving stock (Eq, Show, Generic)
  
-- =============================================================================
-- Helper Functions
-- =============================================================================

-- | Create an initial compiler state with an empty global scope
initialCompilerState :: CompilerState
initialCompilerState = CompilerState
  { csSymbolTable = [HM.empty]  -- Start with one empty scope (global)
  , csTypeTable = HM.empty
  , csNextId = 0
  , csWarnings = []
  }

-- | Create a default compiler environment
defaultCompilerEnv :: Text -> CompilerEnv
defaultCompilerEnv sourceFile = CompilerEnv
  { ceSourceFile = sourceFile
  , ceOptimizationLevel = O0
  , ceTargetCppVersion = Cpp17
  , ceDebugMode = False
  , ceIncludePaths = []
  }

-- | Enter a new scope by pushing an empty symbol table onto the stack
enterScope :: State CompilerState ()
enterScope = modify $ \s -> 
  s { csSymbolTable = HM.empty : csSymbolTable s }

-- | Exit the current scope by popping the top symbol table
exitScope :: State CompilerState ()
exitScope = modify $ \s -> 
  case csSymbolTable s of
    []     -> s  -- Should not happen, but handle gracefully
    [_]    -> s  -- Don't pop the global scope
    (_:xs) -> s { csSymbolTable = xs }

-- | Look up a symbol in the symbol table stack (innermost scope first)
lookupSymbol :: Identifier -> CompilerState -> Maybe Type
lookupSymbol ident compilerState = 
  listToMaybe $ mapMaybe (HM.lookup ident) (csSymbolTable compilerState)

-- | Insert a symbol into the current (innermost) scope
insertSymbol :: Identifier -> Type -> State CompilerState ()
insertSymbol ident ty = modify $ \s ->
  case csSymbolTable s of
    []     -> s { csSymbolTable = [HM.singleton ident ty] }
    (x:xs) -> s { csSymbolTable = HM.insert ident ty x : xs }

-- | Generate a fresh unique identifier
{-# ANN freshId ("HLint: ignore" :: String) #-}
freshId :: State CompilerState Int
freshId = do
  s <- get
  let n = csNextId s
  put $ s { csNextId = n + 1 }
  return n

-- | Add a warning to the compiler state
{-# ANN addWarning ("HLint: ignore" :: String) #-}
addWarning :: Text -> State CompilerState ()
addWarning warning = modify $ \s ->
  s { csWarnings = warning : csWarnings s }

-- | Convert C++ version to its command-line flag representation
{-# ANN cppVersionToFlag ("HLint: ignore" :: String) #-}
cppVersionToFlag :: CppVersion -> Text
cppVersionToFlag = \case
  Cpp11 -> "-std=c++11"
  Cpp14 -> "-std=c++14"
  Cpp17 -> "-std=c++17"
  Cpp20 -> "-std=c++20"
  Cpp23 -> "-std=c++23"

-- | Convert optimization level to its command-line flag representation
{-# ANN optimizationLevelToFlag ("HLint: ignore" :: String) #-}
optimizationLevelToFlag :: OptimizationLevel -> Text
optimizationLevelToFlag = \case
  O0 -> "-O0"
  O1 -> "-O1"
  O2 -> "-O2"
  O3 -> "-O3"
  Os -> "-Os"

-- | Create a pretty error message from a CompilerError
{-# ANN prettyError ("HLint: ignore" :: String) #-}
prettyError :: CompilerError -> Text
prettyError = \case
  ParseError srcSpan msg ->
    formatError srcSpan "Parse Error" msg Nothing
    
  TypeError info ->
    formatError (teLocation info) "Type Error"
      (T.unwords [ "Expected type", T.pack (show $ teExpected info)
                 , "but found", T.pack (show $ teActual info)
                 , "-", teContext info
                 ])
      (teSuggestion info)
      
  AnalysisError srcSpan msg suggestion ->
    formatError srcSpan "Analysis Error" msg suggestion
    
  CodeGenError msg ->
    T.unwords ["Code Generation Error:", msg]
    
  NameResolutionError srcSpan (Identifier name) msg ->
    formatError srcSpan "Name Resolution Error"
      (T.unwords ["Undefined identifier", quote name <> ":", msg])
      Nothing
      
  DuplicateDefinition srcSpan1 (Identifier name) srcSpan2 ->
    formatError srcSpan1 "Duplicate Definition"
      (T.unwords ["Identifier", quote name, "already defined at", formatSpan srcSpan2])
      (Just "Consider using a different name or removing the duplicate")
      
  ImportError module' msg ->
    T.unwords ["Import Error for module", quote module' <> ":", msg]
  where
    quote t = "'" <> t <> "'"
    
    formatSpan (SourceSpan start _ file) =
      T.concat [file, ":", T.pack (show $ spLine start), ":", T.pack (show $ spColumn start)]
    
    formatError srcSpan title msg maybeSuggestion =
      T.unlines $ filter (not . T.null)
        [ T.concat [formatSpan srcSpan, ": ", title]
        , "  " <> msg
        , maybe "" ("  Suggestion: " <>) maybeSuggestion
        ]