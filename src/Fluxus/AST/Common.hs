{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Common AST types and utilities shared across all source languages
module Fluxus.AST.Common
  ( -- * Source location information
    SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , noLoc
  , locatedValue
    -- * Identifiers and names
  , Identifier(..)
  , QualifiedName(..)
  , ModuleName(..)
    -- * Type information
  , Type(..)
  , TypeVar(..)
  , TypeConstraint(..)
    -- * Literals
  , Literal(..)
    -- * Binary and unary operators
  , BinaryOp(..)
  , UnaryOp(..)
  , ComparisonOp(..)
    -- * Common expression patterns
  , CommonExpr(..)
    -- * Ownership and memory semantics
  , OwnershipInfo(..)
  , MemoryLocation(..)
  , EscapeInfo(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Word (Word64)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Source position information (line, column)
data SourcePos = SourcePos
  { posLine   :: !Int
  , posColumn :: !Int
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Source span from start to end position
data SourceSpan = SourceSpan
  { spanFilename :: !Text
  , spanStart    :: !SourcePos
  , spanEnd      :: !SourcePos
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Add source location information to any AST node
data Located a = Located
  { locSpan  :: !SourceSpan
  , locValue :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving anyclass (NFData)

instance Hashable a => Hashable (Located a)

-- | Create a node without location information (for testing/internal use)
noLoc :: a -> Located a
noLoc x = Located
  { locSpan = SourceSpan (T.pack "<no-file>") (SourcePos 0 0) (SourcePos 0 0)
  , locValue = x
  }

-- | Extract the value from a Located node
locatedValue :: Located a -> a
locatedValue = locValue

-- | Simple identifier
newtype Identifier = Identifier Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, NFData)

-- | Qualified name (e.g., module.submodule.function)
data QualifiedName = QualifiedName
  { qnModule :: ![ModuleName]
  , qnName   :: !Identifier
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

instance Hashable QualifiedName

-- | Module name component
newtype ModuleName = ModuleName Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, NFData)

-- | Type variables for generic types
newtype TypeVar = TypeVar Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, NFData)

-- | Type constraints/bounds
data TypeConstraint = TypeConstraint
  { tcVar        :: !TypeVar
  , tcBounds     :: ![Type]
  , tcTraits     :: ![QualifiedName]  -- Rust-like traits or Go interfaces
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

instance Hashable TypeConstraint

-- | Unified type system across all source languages
data Type
  = -- Primitive types
    TInt !Int                           -- Sized integers (8, 16, 32, 64)
  | TUInt !Int                          -- Unsigned integers
  | TFloat !Int                         -- Floating point (32, 64)
  | TBool
  | TString
  | TBytes
  | TChar
  | TVoid
  | TAny                                -- Dynamic type (Python object, Go interface{})
  
  -- Composite types
  | TList !Type                         -- Lists/arrays
  | TTuple ![Type]                      -- Tuples
  | TDict !Type !Type                   -- Dictionaries/maps
  | TSet !Type                          -- Sets
  | TOptional !Type                     -- Optional/nullable types
  
  -- Function types
  | TFunction ![Type] !Type             -- (arg types) -> return type
  | TMethod !Type ![Type] !Type         -- receiver -> (arg types) -> return type
  
  -- User-defined types
  | TStruct !QualifiedName ![Type]      -- Struct/class with type parameters
  | TEnum !QualifiedName ![Type]        -- Enum with type parameters
  | TInterface !QualifiedName ![Type]   -- Interface/protocol
  | TUnion ![Type]                      -- Union types
  
  -- Generic types
  | TVar !TypeVar                       -- Type variable
  | TGeneric !QualifiedName ![Type]     -- Generic type application
  | TForall ![TypeVar] ![TypeConstraint] !Type  -- Polymorphic type
  
  -- Ownership and memory types (for optimization)
  | TOwned !Type                        -- Owned/unique reference (std::unique_ptr)
  | TShared !Type                       -- Shared reference (std::shared_ptr)
  | TBorrowed !Type                     -- Borrowed reference (&T in Rust)
  | TMutable !Type                      -- Mutable reference (&mut T in Rust)
  
  -- Error/inference types
  | TError !Text                        -- Type error
  | TInfer !Int                         -- Type inference variable
  
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Hashable Type

-- | Literal values
data Literal
  = LInt !Int64
  | LUInt !Word64
  | LFloat !Double
  | LBool !Bool
  | LString !Text
  | LBytes !Text                        -- Base64 encoded for now
  | LChar !Char
  | LNone                               -- Python None, Go nil
  deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Binary operators
data BinaryOp
  = -- Arithmetic
    OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow
  | OpFloorDiv                          -- Python //
  
  -- Bitwise
  | OpBitAnd | OpBitOr | OpBitXor
  | OpShiftL | OpShiftR
  
  -- Logical
  | OpAnd | OpOr
  
  -- String/list operations
  | OpConcat                            -- String/list concatenation
  | OpIn | OpNotIn                      -- Membership testing
  
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Unary operators
data UnaryOp
  = OpNot
  | OpNegate
  | OpBitNot
  | OpPositive                          -- Unary +
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Comparison operators
data ComparisonOp
  = OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
  | OpIs | OpIsNot                      -- Python identity comparison
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Common expression patterns that appear in multiple languages
data CommonExpr
  = CELiteral !Literal
  | CEVar !Identifier
  | CEBinaryOp !BinaryOp !(Located CommonExpr) !(Located CommonExpr)
  | CEUnaryOp !UnaryOp !(Located CommonExpr)
  | CEComparison !ComparisonOp !(Located CommonExpr) !(Located CommonExpr)
  | CECall !(Located CommonExpr) ![Located CommonExpr]
  | CEIndex !(Located CommonExpr) !(Located CommonExpr)
  | CESlice !(Located CommonExpr) !(Maybe (Located CommonExpr)) !(Maybe (Located CommonExpr))
  | CEAttribute !(Located CommonExpr) !Identifier
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Hashable CommonExpr

-- | Ownership information for memory management optimization
data OwnershipInfo = OwnershipInfo
  { ownsMemory     :: !Bool             -- True if this value owns its memory
  , canMove        :: !Bool             -- True if value can be moved (std::move)
  , refCount       :: !(Maybe Int)      -- Reference count if known at compile time
  , escapes        :: !EscapeInfo       -- Where this value can escape to
  , memLocation    :: !MemoryLocation   -- Stack vs heap allocation
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Memory allocation location
data MemoryLocation
  = Stack                               -- Stack allocated
  | Heap                                -- Heap allocated
  | Global                              -- Global/static storage
  | Unknown                             -- Cannot determine at compile time
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Escape analysis information
data EscapeInfo
  = NoEscape                            -- Value doesn't escape current scope
  | EscapeToReturn                      -- Value escapes only via return
  | EscapeToHeap                        -- Value escapes to heap
  | EscapeToGlobal                      -- Value escapes to global scope
  | EscapeUnknown                       -- Cannot analyze escape behavior
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)