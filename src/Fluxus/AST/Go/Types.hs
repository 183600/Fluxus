{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Go-specific AST types
module Fluxus.AST.Go.Types
  ( -- * Go AST types
    GoAST(..)
  , GoPackage(..)
  , GoFile(..)
  , GoDecl(..)
  , GoTypeDecl(..)
  , GoConstSpec(..)
  , GoStmt(..)
  , GoType(..)

    -- * Go-specific constructs
  , GoImport(..)
  , GoField(..)
  , GoMethod(..)
  , GoConstraint(..)
  , GoFunction(..)
  , GoReceiver(..)
  , GoChannel(..)

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
  ) where

import Data.Text (Text)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Fluxus.AST.Go.Common
import Fluxus.AST.Go.Expressions (GoExpr)


--------------------------------------------------------------------------------
-- Go AST
--------------------------------------------------------------------------------

data GoAST = GoAST
  { goPackage :: !GoPackage
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data GoPackage = GoPackage
  { goPackageName  :: !Identifier
  , goPackageFiles :: ![GoFile]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data GoFile = GoFile
  { goFileName    :: !Text
  , goFilePackage :: !Identifier
  , goFileImports :: ![Located GoImport]
  , goFileDecls   :: ![Located GoDecl]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

-- | Unified const spec for a const block: identifiers, optional type, optional values.
-- If constValues is Nothing, it repeats the last expression list (Go semantics).
data GoConstSpec = GoConstSpec
  { constNames  :: ![Identifier]
  , constType   :: !(Maybe (Located GoType))
  , constValues :: !(Maybe [Located GoExpr])
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Distinguish type definition vs type alias, and allow type parameters.
data GoTypeDecl = GoTypeDecl
  { goTypeDeclName    :: !Identifier
  , goTypeDeclParams  :: ![GoField]             -- e.g. [T comparable, U any]
  , goTypeDeclIsAlias :: !Bool                  -- True: 'type T = U' (alias), False: 'type T U' (definition)
  , goTypeDeclType    :: !(Located GoType)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data GoDecl
  = GoImportDecl ![Located GoImport]
  | GoConstDecl  ![GoConstSpec]
  | GoTypeDeclStmt !GoTypeDecl
  | GoBindDecl   ![GoBinding]              -- top-level 'var' bindings (BindVar only by convention)
  | GoFuncDecl   !GoFunction
  | GoMethodDecl !GoReceiver !GoFunction
  | GoInitDecl   !(Located GoStmt)         -- init() { ... }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

-- | Unified binding/assignment statement.
data BindKind = BindAssign | BindDefine | BindVar
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | LHS for bindings: either identifiers (var/define) or general expressions (assign '=').
data BindingLHS
  = LHSIdents ![Identifier]
  | LHSExprs  ![Located GoExpr]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Unified binding structure used in both decls and stmts.
-- Semantics:
--   - BindVar: LHS must be LHSIdents; bindType allowed; RHS may be empty.
--   - BindDefine: LHS must be LHSIdents; bindType must be Nothing; RHS required.
--   - BindAssign: LHS may be LHSExprs; bindType must be Nothing; RHS required.
data GoBinding = GoBinding
  { bindKind :: !BindKind
  , bindLHS  :: !BindingLHS
  , bindType :: !(Maybe (Located GoType))   -- only valid for BindVar
  , bindRHS  :: ![Located GoExpr]           -- may be [] for pure 'var' without init
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Generic case clause used by switch (value/type) and select.
data CaseClause a = CaseClause
  { caseCond :: !(Maybe a)                 -- Nothing means 'default'
  , caseBody :: ![Located GoStmt]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoStmt
  = -- Simple statements
    GoExprStmt !(Located GoExpr)
  | GoBind     !GoBinding                   -- unified var/:=/=
  | GoIncDec   !(Located GoExpr) !Bool      -- True: expr++, False: expr--
  | GoSend     !(Located GoExpr) !(Located GoExpr)  -- channel <- value
  | GoReturn   ![Located GoExpr]
  | GoBreak    !(Maybe Identifier)
  | GoContinue !(Maybe Identifier)
  | GoGoto     !Identifier
  | GoFallthrough
  | GoEmpty

    -- Compound statements
  | GoBlock     ![Located GoStmt]
  | GoIf        !(Maybe (Located GoStmt)) !(Located GoExpr) !(Located GoStmt) !(Maybe (Located GoStmt))
  | GoSwitch    !(Maybe (Located GoStmt)) !(Maybe (Located GoExpr)) ![CaseClause [Located GoExpr]]
  | GoTypeSwitch !(Maybe (Located GoStmt)) !GoTypeSwitchClause ![CaseClause [Located GoType]] -- case T1, T2: ...
  | GoFor       !(Maybe GoForClause) !(Located GoStmt)
  | GoRange     !GoRangeClause !(Located GoStmt)
  | GoSelect    ![CaseClause (Located GoStmt)]        -- cond is a comm statement (send/receive/assign)
  | GoDefer     !(Located GoExpr)
  | GoGo        !(Located GoExpr)                     -- go statement (goroutine)

  -- Labeled statements
  | GoLabeled   !Identifier !(Located GoStmt)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data GoType
  = -- Basic types
    GoBasicType !Identifier                                 -- int, string, bool, etc.

  -- Composite types
  | GoArrayType  !(Located GoExpr) !(Located GoType)        -- [size]type
  | GoSliceType  !(Located GoType)                          -- []type
  | GoMapType    !(Located GoType) !(Located GoType)        -- map[key]value
  | GoChanType   !GoChannel !(Located GoType)               -- chan type, <-chan type, chan<- type
  | GoPointerType !(Located GoType)                         -- *type
  | GoFuncType   ![GoField] ![GoField]                      -- func(params) results
  | GoInterfaceType ![GoMethod]                             -- interface { methods }
  | GoStructType ![GoField]                                 -- struct { fields }

  -- Named and generic types
  | GoNamedType !QualifiedName
  | GoGenericType !QualifiedName ![Located GoType]          -- Type[T1, T2, ...]
  | GoTypeParam !Identifier !(Maybe (Located GoConstraint)) -- T constraint

  -- Special types
  | GoEllipsisType !(Located GoType)                        -- ...type (variadic)

  -- Go 1.18+ Generics helpers
  | GoGenericConstraint ![Located GoConstraint]             -- T1 | T2 | T3
  | GoInstantiatedType !(Located GoType) ![Located GoType]  -- Container[int, string]

  -- Example: constraints that can be reified as types in analysis
  | GoCmpOrdered                                            -- cmp.Ordered (marker)
  | GoSlicesCloneable !(Located GoType)                     -- marker for slices.Clone[T]
  | GoMapsComparable !(Located GoType) !(Located GoType)    -- marker for comparable map keys

  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

--------------------------------------------------------------------------------
-- Imports, fields, methods, constraints, functions, receivers, channels
--------------------------------------------------------------------------------

data GoImport
  = GoImportNormal !(Maybe Identifier) !Text    -- [alias] "path"
  | GoImportDot !Text                            -- . "path"
  | GoImportBlank !Text                          -- _ "path"
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoField = GoField
  { goFieldNames :: ![Identifier]                -- can be empty for anonymous fields
  , goFieldType  :: !(Located GoType)
  , goFieldTag   :: !(Maybe Text)                -- struct tag
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoMethod
  = GoMethod !Identifier !(Located GoType)       -- method: func signature
  | GoTypeConstraint !(Located GoConstraint)     -- embedded type constraint
  | GoEmbeddedInterface !(Located GoType)        -- embedded interface
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoConstraint
  = GoBasicConstraint !(Located GoType)                -- T
  | GoApproximationConstraint !(Located GoType)        -- ~T
  | GoUnionConstraint ![Located GoConstraint]          -- A | B | C
  | GoInterfaceConstraint ![GoMethod]                  -- interface { methods }
  | GoMethodSetConstraint ![Located GoType]            -- {T; U; V} method set constraint
  | GoComparableConstraint                             -- comparable
  | GoOrderedConstraint                                -- constraints.Ordered
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoFunction = GoFunction
  { goFuncName       :: !(Maybe Identifier)            -- Nothing for function literals
  , goFuncTypeParams :: ![GoField]                     -- generic type parameters
  , goFuncParams     :: ![GoField]
  , goFuncResults    :: ![GoField]
  , goFuncBody       :: !(Maybe (Located GoStmt))      -- Nothing for function signatures
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoReceiver = GoReceiver
  { goReceiverName :: !(Maybe Identifier)
  , goReceiverType :: !(Located GoType)
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data GoChannel
  = GoChanBidi                                         -- chan
  | GoChanSend                                         -- chan<-
  | GoChanRecv                                         -- <-chan
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

--------------------------------------------------------------------------------
-- Control-flow clauses
--------------------------------------------------------------------------------

data GoForClause = GoForClause
  { goForInit :: !(Maybe (Located GoStmt))
  , goForCond :: !(Maybe (Located GoExpr))
  , goForPost :: !(Maybe (Located GoStmt))
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data GoRangeBinding = GoRangeBinding
  { rangeKey    :: !(Maybe Identifier)
  , rangeValue  :: !(Maybe Identifier)
  , rangeDefine :: !Bool                               -- True for :=, False for =
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data GoRangeClause = GoRangeClause
  { goRangeBinding :: !(Maybe GoRangeBinding)          -- Nothing for 'for range expr { ... }' (no assignment)
  , goRangeExpr    :: !(Located GoExpr)
  , goRangeInteger :: !(Maybe Integer)                 -- Go 1.22: 'range n'
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

data GoTypeSwitchClause = GoTypeSwitchClause
  { goTypeSwitchAssign :: !(Maybe Identifier)          -- x := expr.(type)
  , goTypeSwitchExpr   :: !(Located GoExpr)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)