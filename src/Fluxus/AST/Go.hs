{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Go-specific AST definitions (modular, refactored)
module Fluxus.AST.Go
  ( -- * Common
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

    -- * Go AST types
  , GoAST(..)
  , GoPackage(..)
  , GoFile(..)
  , GoDecl(..)
  , GoTypeDecl(..)
  , GoConstSpec(..)
  , GoStmt(..)
  , GoExpr(..)
  , GoType(..)

    -- * Go-specific constructs
  , GoImport(..)
  , GoField(..)
  , GoMethod(..)
  , GoConstraint(..)
  , GoFunction(..)
  , GoReceiver(..)
  , GoChannel(..)

    -- * Go literals and constants
  , GoLiteral(..)

    -- * Go statements
  , GoForClause(..)
  , GoRangeBinding(..)
  , GoRangeClause(..)
  , GoTypeSwitchClause(..)
  
    -- * Go bindings
  , GoBinding(..)
  , BindKind(..)
  , BindingLHS(..)

    -- * Case clauses (unified)
  , CaseClause(..)

    -- * Go expressions
  , GoSliceExpr(..)
  , GoCompositeLiteral(..)
  , GoCompElem(..)

    -- * Visibility helpers
  , isGoPointerType
  , isGoChannelType
  , isGoInterfaceType
  , isPublicIdentifier
  , isPrivateIdentifier

    -- * Builtins (flexible)
  , isKnownBuiltin
  ) where

import Fluxus.AST.Go.Common
import Fluxus.AST.Go.Types
import Fluxus.AST.Go.Helpers
import Fluxus.AST.Go.Expressions