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
  , GoConstraint(..)
  , GoBuiltin(..)
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
    -- * Visibility helpers
  , isPublicIdentifier
  , isPrivateIdentifier
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict ()
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
  
  -- Init function declarations (special handling)
  | GoInitDecl !(Located GoStmt)  -- init() { ... }
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go statements
data GoStmt
  = -- Simple statements
    GoExprStmt !(Located GoExpr)
  | GoAssign ![Located GoExpr] ![Located GoExpr]        -- x, y = a, b
  | GoDefine ![Identifier] ![Located GoExpr]            -- x, y := a, b
  | GoVarStmt ![(Identifier, Maybe (Located GoType), Maybe (Located GoExpr))]  -- var x int = 42
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
  | GoBuiltinCall !GoBuiltin ![Located GoExpr]           -- Built-in function calls
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
  
  -- Go 1.18+ features - Generics and type parameters
  | GoGenericInstance !(Located GoExpr) ![Located GoType] -- func[T](args)
  | GoTypeInference !(Located GoExpr)                    -- Type inference expression
  
  -- Go 1.21+ features
  | GoRangeOverInt !(Located GoExpr)                     -- range n
  | GoSlicesIndex !(Located GoExpr) !(Located GoExpr)    -- slices.Index(s, v)
  | GoSlicesContains !(Located GoExpr) !(Located GoExpr) -- slices.Contains(s, v)
  | GoMapsKeys !(Located GoExpr)                         -- maps.Keys(m)
  | GoMapsValues !(Located GoExpr)                       -- maps.Values(m)
  
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
  | GoTypeParam !Identifier !(Maybe (Located GoConstraint))   -- T constraint
  
  -- Special types
  | GoEllipsisType !(Located GoType)                     -- ...type (variadic)
  
  -- Go 1.18+ Generics
  | GoGenericConstraint ![Located GoConstraint]         -- T1 | T2 | T3
  | GoInstantiatedType !(Located GoType) ![Located GoType] -- Container[int, string]
  
  -- Go 1.21+ types
  | GoCmpOrdered                                         -- cmp.Ordered
  | GoSlicesCloneable !(Located GoType)                  -- For slices.Clone[T]
  | GoMapsComparable !(Located GoType) !(Located GoType) -- For maps with comparable keys
  
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

-- | Go interface methods or embedded constraints
data GoMethod 
  = GoMethod !Identifier !(Located GoType)             -- method: func signature
  | GoTypeConstraint !(Located GoConstraint)            -- embedded type constraint  
  | GoEmbeddedInterface !(Located GoType)               -- embedded interface
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go generic type constraints
data GoConstraint
  = GoBasicConstraint !(Located GoType)                 -- T
  | GoApproximationConstraint !(Located GoType)         -- ~T  
  | GoUnionConstraint ![Located GoConstraint]           -- A | B | C
  | GoInterfaceConstraint ![GoMethod]                   -- interface { methods }
  | GoMethodSetConstraint ![Located GoType]            -- {T; U; V} method set constraint
  | GoComparableConstraint                              -- comparable built-in constraint
  | GoOrderedConstraint                                 -- constraints.Ordered
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go built-in functions
data GoBuiltin
  = GoMake                -- make(type, args...)
  | GoNew                 -- new(type)
  | GoLen                 -- len(expr)
  | GoCap                 -- cap(expr)
  | GoAppend              -- append(slice, elems...)
  | GoCopy                -- copy(dst, src)
  | GoDelete              -- delete(map, key)
  | GoClose               -- close(channel)
  | GoPanic               -- panic(expr)
  | GoRecover             -- recover()
  | GoReal                -- real(complex)
  | GoImagBuiltin         -- imag(complex) - renamed to avoid conflict
  | GoComplex             -- complex(real, imag)
  | GoMin                 -- min(a, b) - Go 1.21+
  | GoMax                 -- max(a, b) - Go 1.21+
  | GoClear               -- clear(map/slice) - Go 1.21+
  | GoUnsafeString        -- unsafe.String(ptr, len) - Go 1.20+
  | GoUnsafeSlice         -- unsafe.Slice(ptr, len) - Go 1.20+
  | GoErrorsJoin          -- errors.Join(errs...) - Go 1.20+
  | GoPrint               -- print(args...) - built-in
  | GoPrintln             -- println(args...) - built-in
  -- Additional Go 1.21+ builtins
  | GoAny                 -- any type constraint
  | GoComparable          -- comparable type constraint
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

-- | Go function definitions
data GoFunction = GoFunction
  { goFuncName       :: !(Maybe Identifier)                -- Nothing for function literals
  , goFuncTypeParams :: ![GoField]                         -- Generic type parameters
  , goFuncParams     :: ![GoField]
  , goFuncResults    :: ![GoField]
  , goFuncBody       :: !(Maybe (Located GoStmt))          -- Nothing for function signatures
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
  , goRangeInteger :: !(Maybe Integer)                  -- For "range 10" syntax (Go 1.22+)
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

-- | Check if an identifier is public (starts with uppercase)
isPublicIdentifier :: Identifier -> Bool
isPublicIdentifier (Identifier name) = 
  case T.uncons name of
    Just (c, _) -> isUpper c
    Nothing -> False
  where
    isUpper c = c >= 'A' && c <= 'Z'

-- | Check if an identifier is private (starts with lowercase)
isPrivateIdentifier :: Identifier -> Bool
isPrivateIdentifier ident = not (isPublicIdentifier ident)