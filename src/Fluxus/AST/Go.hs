{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Go-specific AST definitions
module Fluxus.AST.Go
  ( -- * Go AST types
    GoAST(..)
  , GoPackage(..)
  , GoFile(..)
  , GoDecl(..)
  , GoStmt(..)
  , GoExpr(..)
  , GoType(..)
    -- * Go-specific constructs
  , GoImport(..)
  , GoField(..)
  , GoMethod(..)
  , GoFunction(..)
  , GoReceiver(..)
  , GoChannel(..)
    -- * Go literals and constants
  , GoLiteral(..)
    -- * Go statements
  , GoForClause(..)
  , GoRangeClause(..)
  , GoTypeSwitchClause(..)
  , GoCommClause(..)
    -- * Go expressions
  , GoSliceExpr(..)
  , isGoPointerType
  , isGoChannelType
  , isGoInterfaceType
  ) where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Fluxus.AST.Common

-- | Top-level Go AST
data GoAST = GoAST
  { goPackage :: !GoPackage
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go package containing multiple files
data GoPackage = GoPackage
  { goPackageName :: !Identifier
  , goPackageFiles :: ![GoFile]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go source file
data GoFile = GoFile
  { goFileName    :: !Text
  , goFilePackage :: !Identifier
  , goFileImports :: ![Located GoImport]
  , goFileDecls   :: ![Located GoDecl]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go top-level declarations
data GoDecl
  = -- Import declarations
    GoImportDecl ![Located GoImport]
    
  -- Constant declarations
  | GoConstDecl ![(Identifier, Maybe (Located GoType), Located GoExpr)]
  
  -- Type declarations
  | GoTypeDecl !Identifier !(Located GoType)
  
  -- Variable declarations
  | GoVarDecl ![(Identifier, Maybe (Located GoType), Maybe (Located GoExpr))]
  
  -- Function declarations
  | GoFuncDecl !GoFunction
  
  -- Method declarations (functions with receivers)
  | GoMethodDecl !GoReceiver !GoFunction
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go statements
data GoStmt
  = -- Simple statements
    GoExprStmt !(Located GoExpr)
  | GoAssign ![Located GoExpr] ![Located GoExpr]        -- x, y = a, b
  | GoDefine ![Identifier] ![Located GoExpr]            -- x, y := a, b
  | GoIncDec !(Located GoExpr) !Bool                    -- expr++ (True) or expr-- (False)
  | GoSend !(Located GoExpr) !(Located GoExpr)          -- channel <- value
  | GoReturn ![Located GoExpr]
  | GoBreak !(Maybe Identifier)
  | GoContinue !(Maybe Identifier)
  | GoGoto !Identifier
  | GoFallthrough
  | GoEmpty
  
  -- Compound statements
  | GoBlock ![Located GoStmt]
  | GoIf !(Maybe (Located GoStmt)) !(Located GoExpr) !(Located GoStmt) !(Maybe (Located GoStmt))
  | GoSwitch !(Maybe (Located GoStmt)) !(Maybe (Located GoExpr)) ![Located GoStmt]  -- Case statements in body
  | GoTypeSwitch !(Maybe (Located GoStmt)) !GoTypeSwitchClause ![Located GoStmt]
  | GoFor !(Maybe GoForClause) !(Located GoStmt)
  | GoRange !GoRangeClause !(Located GoStmt)
  | GoSelect ![Located GoCommClause]
  | GoDefer !(Located GoExpr)
  | GoGo !(Located GoExpr)                              -- go statement (goroutine)
  
  -- Case and default statements
  | GoCase ![Located GoExpr] ![Located GoStmt]
  | GoDefault ![Located GoStmt]
  
  -- Communication cases (for select)
  | GoCommCase !(Maybe (Located GoStmt)) ![Located GoStmt]  -- case stmt: body
  | GoCommDefault ![Located GoStmt]
  
  -- Labeled statements
  | GoLabeled !Identifier !(Located GoStmt)
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go expressions
data GoExpr
  = -- Literals and identifiers
    GoLiteral !GoLiteral
  | GoIdent !Identifier
  | GoQualifiedIdent !Identifier !Identifier           -- package.identifier
  
  -- Operators
  | GoBinaryOp !BinaryOp !(Located GoExpr) !(Located GoExpr)
  | GoUnaryOp !UnaryOp !(Located GoExpr)
  | GoComparison !ComparisonOp !(Located GoExpr) !(Located GoExpr)
  
  -- Function calls and indexing
  | GoCall !(Located GoExpr) ![Located GoExpr]
  | GoIndex !(Located GoExpr) !(Located GoExpr)
  | GoSlice !(Located GoExpr) !GoSliceExpr
  | GoSelector !(Located GoExpr) !Identifier
  | GoTypeAssert !(Located GoExpr) !(Located GoType)
  
  -- Composite literals
  | GoCompositeLit !(Maybe (Located GoType)) ![Located GoExpr]
  | GoArrayLit !(Located GoType) ![Located GoExpr]
  | GoSliceLit !(Located GoType) ![Located GoExpr]
  | GoMapLit !(Located GoType) ![(Located GoExpr, Located GoExpr)]
  | GoStructLit !(Located GoType) ![(Identifier, Located GoExpr)]
  
  -- Special expressions
  | GoAddress !(Located GoExpr)                         -- &expr
  | GoDeref !(Located GoExpr)                           -- *expr
  | GoReceive !(Located GoExpr)                         -- <-channel
  | GoTypeConversion !(Located GoType) !(Located GoExpr)
  
  -- Function literals
  | GoFuncLit !GoFunction
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go types
data GoType
  = -- Basic types
    GoBasicType !Identifier                             -- int, string, bool, etc.
  
  -- Composite types
  | GoArrayType !(Located GoExpr) !(Located GoType)     -- [size]type
  | GoSliceType !(Located GoType)                       -- []type
  | GoMapType !(Located GoType) !(Located GoType)       -- map[key]value
  | GoChanType !GoChannel !(Located GoType)             -- chan type, <-chan type, chan<- type
  | GoPointerType !(Located GoType)                     -- *type
  | GoFuncType ![GoField] ![GoField]                    -- func(params) results
  | GoInterfaceType ![GoMethod]                         -- interface { methods }
  | GoStructType ![GoField]                             -- struct { fields }
  
  -- Named and generic types
  | GoNamedType !QualifiedName
  | GoGenericType !QualifiedName ![Located GoType]      -- Type[T1, T2, ...]
  | GoTypeParam !Identifier !(Maybe (Located GoType))   -- T ~constraint
  
  -- Special types
  | GoEllipsisType !(Located GoType)                     -- ...type (variadic)
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go literals
data GoLiteral
  = GoInt !Integer
  | GoFloat !Double
  | GoImag !Double                                      -- Imaginary number
  | GoRune !Char
  | GoString !Text
  | GoRawString !Text                                   -- Raw string literal
  | GoBool !Bool
  | GoNil
  deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go import specifications
data GoImport
  = GoImportNormal !(Maybe Identifier) !Text           -- [alias] "path"
  | GoImportDot !Text                                   -- . "path"
  | GoImportBlank !Text                                 -- _ "path"
  deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go struct/function fields
data GoField = GoField
  { goFieldNames :: ![Identifier]                       -- Can be empty for anonymous fields
  , goFieldType  :: !(Located GoType)
  , goFieldTag   :: !(Maybe Text)                       -- Struct tags
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go interface methods
data GoMethod = GoMethod
  { goMethodName :: !Identifier
  , goMethodType :: !(Located GoType)                   -- Function type
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go function definitions
data GoFunction = GoFunction
  { goFuncName    :: !(Maybe Identifier)                -- Nothing for function literals
  , goFuncParams  :: ![GoField]
  , goFuncResults :: ![GoField]
  , goFuncBody    :: !(Maybe (Located GoStmt))          -- Nothing for function signatures
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go method receiver
data GoReceiver = GoReceiver
  { goReceiverName :: !(Maybe Identifier)
  , goReceiverType :: !(Located GoType)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go channel direction
data GoChannel
  = GoChanBidi                                          -- chan
  | GoChanSend                                          -- chan<-
  | GoChanRecv                                          -- <-chan
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go for loop clauses
data GoForClause = GoForClause
  { goForInit :: !(Maybe (Located GoStmt))
  , goForCond :: !(Maybe (Located GoExpr))
  , goForPost :: !(Maybe (Located GoStmt))
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go range clauses
data GoRangeClause = GoRangeClause
  { goRangeKey   :: !(Maybe Identifier)
  , goRangeValue :: !(Maybe Identifier)
  , goRangeDefine :: !Bool                              -- := vs =
  , goRangeExpr  :: !(Located GoExpr)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go type switch clause
data GoTypeSwitchClause = GoTypeSwitchClause
  { goTypeSwitchAssign :: !(Maybe Identifier)           -- x := expr.(type)
  , goTypeSwitchExpr   :: !(Located GoExpr)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go communication clauses (for select statements)
data GoCommClause = GoCommClause
  { goCommStmt :: !(Maybe (Located GoStmt))             -- Send/receive statement
  , goCommBody :: ![Located GoStmt]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Go slice expressions
data GoSliceExpr = GoSliceExpr
  { goSliceLow  :: !(Maybe (Located GoExpr))
  , goSliceHigh :: !(Maybe (Located GoExpr))
  , goSliceMax  :: !(Maybe (Located GoExpr))            -- For 3-index slices
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Check if a Go type is a pointer type
isGoPointerType :: GoType -> Bool
isGoPointerType (GoPointerType _) = True
isGoPointerType _ = False

-- | Check if a Go type is a channel type
isGoChannelType :: GoType -> Bool
isGoChannelType (GoChanType _ _) = True
isGoChannelType _ = False

-- | Check if a Go type is an interface type
isGoInterfaceType :: GoType -> Bool
isGoInterfaceType (GoInterfaceType _) = True
isGoInterfaceType _ = False