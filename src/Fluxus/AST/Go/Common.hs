{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Go-specific common types and operators
module Fluxus.AST.Go.Common
  ( -- * Common types
    Identifier(..)
  , QualifiedName(..)
  , Position(..)
  , Span(..)
  , CommentKind(..)
  , Comment(..)
  , NodeAnn(..)
  , Located(..)

    -- * Operators
  , BinaryOp(..)
  , UnaryOp(..)
  , ComparisonOp(..)
  ) where

import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Fluxus.AST.Common (Identifier(..), QualifiedName(..))

--------------------------------------------------------------------------------
-- Go-specific location and comment types
--------------------------------------------------------------------------------

-- | Source position and span information (Go-specific).
data Position = Position
  { posLine   :: !Int
  , posColumn :: !Int
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

data Span = Span
  { spanStart :: !Position
  , spanEnd   :: !Position
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Comments (line or block), stored with nodes as trivia (Go-specific).
data CommentKind = LineComment | BlockComment
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data Comment = Comment
  { commentKind :: !CommentKind
  , commentText :: !Text
  , commentSpan :: !(Maybe Span)
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Node annotation with location and leading/trailing comments (Go-specific).
data NodeAnn = NodeAnn
  { annSpan      :: !(Maybe Span)
  , annLeading   :: ![Comment]
  , annTrailing  :: ![Comment]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Value with location + comments (Go-specific).
data Located a = Located
  { locAnn   :: !NodeAnn
  , locValue :: !a
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

--------------------------------------------------------------------------------
-- Operators
--------------------------------------------------------------------------------

data BinaryOp
  = OpAdd      -- +
  | OpSub      -- -
  | OpMul      -- *
  | OpQuo      -- /
  | OpRem      -- %
  | OpAnd      -- &
  | OpOr       -- |
  | OpXor      -- ^
  | OpAndNot   -- &^
  | OpShiftL   -- <<
  | OpShiftR   -- >>
  | OpAndAnd   -- &&
  | OpOrOr     -- ||
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data UnaryOp
  = OpPos      -- +
  | OpNeg      -- -
  | OpNot      -- !
  | OpBitNot   -- ^
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data ComparisonOp
  = OpEq       -- ==
  | OpNe       -- !=
  | OpLt       -- <
  | OpLe       -- <=
  | OpGt       -- >
  | OpGe       -- >=
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)