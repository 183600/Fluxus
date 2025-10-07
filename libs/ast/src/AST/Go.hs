{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Go-specific AST definitions (single-file, refactored)
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

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

--------------------------------------------------------------------------------
-- Common (inlined from previous Fluxus.AST.Common)
--------------------------------------------------------------------------------

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | A qualified name, optionally with a package/module qualifier.
data QualifiedName = QualifiedName
  { qnPackage :: !(Maybe Identifier)
  , qnName    :: !Identifier
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Source position and span information.
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

-- | Comments (line or block), stored with nodes as trivia.
data CommentKind = LineComment | BlockComment
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

data Comment = Comment
  { commentKind :: !CommentKind
  , commentText :: !Text
  , commentSpan :: !(Maybe Span)
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Node annotation with location and leading/trailing comments.
data NodeAnn = NodeAnn
  { annSpan      :: !(Maybe Span)
  , annLeading   :: ![Comment]
  , annTrailing  :: ![Comment]
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Value with location + comments.
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
  | GoTypeDeclD  !GoTypeDecl
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
  { compType  :: !(Maybe (Located GoType))              -- usually required in Go; kept Maybe for flexibility
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
  | GoTypeAssert  !(Located GoExpr) !(Located GoType)
  | GoTypeConversion !(Located GoType) !(Located GoExpr)

  -- Addressing and channel receive
  | GoAddress !(Located GoExpr)                         -- &expr
  | GoDeref   !(Located GoExpr)                         -- *expr
  | GoReceive !(Located GoExpr)                         -- <-channel

  -- Composite literal (unified)
  | GoCompositeLit !GoCompositeLiteral

  -- Function literals
  | GoFuncLit !GoFunction

  -- Go 1.18+ Generics
  | GoGenericInstance !(Located GoExpr) ![Located GoType]   -- f[T](args)
  | GoTypeInference !(Located GoExpr)                       -- f(_)(...) style inference marker (optional)

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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isGoPointerType :: GoType -> Bool
isGoPointerType (GoPointerType _) = True
isGoPointerType _ = False

isGoChannelType :: GoType -> Bool
isGoChannelType (GoChanType _ _) = True
isGoChannelType _ = False

isGoInterfaceType :: GoType -> Bool
isGoInterfaceType (GoInterfaceType _) = True
isGoInterfaceType _ = False

isPublicIdentifier :: Identifier -> Bool
isPublicIdentifier (Identifier name) =
  case T.uncons name of
    Just (c, _) -> isUpper c
    Nothing     -> False

isPrivateIdentifier :: Identifier -> Bool
isPrivateIdentifier ident = not (isPublicIdentifier ident)

-- | Recognize known builtins (extensible: builtin calls use Text).
isKnownBuiltin :: Text -> Bool
isKnownBuiltin t = t `elem`
  [ "make","new","len","cap","append","copy","delete","close","panic","recover"
  , "real","imag","complex","min","max","clear"
  , "print","println"
  , "any","comparable"
  ]