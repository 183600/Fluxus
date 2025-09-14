{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Go-specific expression types
module Fluxus.AST.Go.Expressions
  ( -- * Go literals and constants
    GoLiteral(..)

    -- * Go expressions
  , GoSliceExpr(..)
  , GoCompositeLiteral(..)
  , GoCompElem(..)
  , GoExpr(..)
  ) where

import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Fluxus.AST.Go.Common

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data GoLiteral
  = GoInt   !Integer
  | GoFloat !Double
  | GoImag  !Double                     -- Imaginary numeric literal
  | GoRune  !Char
  | GoString !Text
  | GoRawString !Text
  | GoBool  !Bool
  | GoNil
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Composite literal (unified).
data GoCompElem
  = CompElemValue !(Located GoExpr)                     -- value
  | CompElemKeyed !(Located GoExpr) !(Located GoExpr)   -- key: value (map, keyed array)
  | CompElemField !Identifier !(Located GoExpr)         -- field: value (struct with named fields)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoCompositeLiteral = GoCompositeLiteral
  { compType  :: !(Maybe (Located GoExpr))              -- usually required in Go; kept Maybe for flexibility
  , compElems :: ![GoCompElem]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoSliceExpr = GoSliceExpr
  { goSliceLow  :: !(Maybe (Located GoExpr))
  , goSliceHigh :: !(Maybe (Located GoExpr))
  , goSliceMax  :: !(Maybe (Located GoExpr))            -- for 3-index slices
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoExpr
  = -- Literals and identifiers
    GoLiteral !GoLiteral
  | GoIdent !Identifier
  | GoQualifiedIdent !Identifier !Identifier            -- package.identifier

  -- Special expressions
  | GoIota                                             -- iota (contextual const generator)

  -- Operators
  | GoBinaryOp   !BinaryOp     !(Located GoExpr) !(Located GoExpr)
  | GoUnaryOp    !UnaryOp      !(Located GoExpr)
  | GoComparison !ComparisonOp !(Located GoExpr) !(Located GoExpr)

  -- Calls and selectors
  | GoCall        !(Located GoExpr) ![Located GoExpr]
  | GoBuiltinCall !Text ![Located GoExpr]              -- flexible: accept any builtin name
  | GoIndex       !(Located GoExpr) !(Located GoExpr)
  | GoSlice       !(Located GoExpr) !GoSliceExpr
  | GoSelector    !(Located GoExpr) !Identifier

  -- Addressing and channel receive
  | GoAddress !(Located GoExpr)                         -- &expr
  | GoDeref   !(Located GoExpr)                         -- *expr
  | GoReceive !(Located GoExpr)                         -- <-channel

  -- Composite literal (unified)
  | GoCompositeLit !GoCompositeLiteral

  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)



