{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Python-specific AST definitions
module Fluxus.AST.Python
  ( -- * Python AST types
    PythonAST(..)
  , PythonModule(..)
  , PythonStmt(..)
  , PythonExpr(..)
  , PythonPattern(..)
    -- * Python-specific constructs
  , PythonDecorator(..)
  , PythonComprehension(..)
  , PythonSlice(..)
  , PythonImport(..)
  , PythonExcept(..)
  , PythonWithItem(..)
  , PythonMatch(..)
  , PythonCase(..)
    -- * Python literals and constants
  , PythonLiteral(..)
  , FStringPart(..)
  , ConversionFlag(..)
    -- * Function and class definitions
  , PythonFuncDef(..)
  , PythonClassDef(..)
  , PythonArgument(..)
  , PythonParameter(..)
  , TypeParam(..)
    -- * Type annotations
  , PythonTypeExpr(..)
    -- * Utilities
  , isAsyncStmt
  , isAsyncExpr
  ) where

import Data.Text (Text)
import Data.HashMap.Strict ()
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Fluxus.AST.Common

-- | Top-level Python AST
data PythonAST = PythonAST
  { pyModule :: !PythonModule
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Python module
data PythonModule = PythonModule
  { pyModuleName    :: !(Maybe ModuleName)
  , pyModuleDoc     :: !(Maybe Text)
  , pyModuleImports :: ![Located PythonImport]
  , pyModuleBody    :: ![Located PythonStmt]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | Python statements
data PythonStmt
  = -- Simple statements
    PyExprStmt !(Located PythonExpr)
  | PyAssign ![Located PythonPattern] !(Located PythonExpr)
  | PyAugAssign !(Located PythonPattern) !BinaryOp !(Located PythonExpr)
  | PyAnnAssign !(Located PythonPattern) !(Located PythonTypeExpr) !(Maybe (Located PythonExpr))
  | PyReturn !(Maybe (Located PythonExpr))
  | PyYield !(Maybe (Located PythonExpr))
  | PyYieldFrom !(Located PythonExpr)
  | PyBreak
  | PyContinue
  | PyPass
  | PyDel ![Located PythonExpr]
  | PyAssert !(Located PythonExpr) !(Maybe (Located PythonExpr))
  | PyImport ![Located PythonImport]
  | PyGlobal ![Identifier]
  | PyNonlocal ![Identifier]
  
  -- Compound statements with async support
  | PyIf !(Located PythonExpr) ![Located PythonStmt] ![Located PythonStmt]
  | PyWhile !(Located PythonExpr) ![Located PythonStmt] ![Located PythonStmt]
  | PyFor
    { pyForAsync  :: !Bool
    , pyForTarget :: !(Located PythonPattern)
    , pyForIter   :: !(Located PythonExpr)
    , pyForBody   :: ![Located PythonStmt]
    , pyForElse   :: ![Located PythonStmt]
    }
  | PyWith
    { pyWithAsync :: !Bool
    , pyWithItems :: ![Located PythonWithItem]
    , pyWithBody  :: ![Located PythonStmt]
    }
  | PyTry
    { pyTryBody    :: ![Located PythonStmt]
    , pyTryExcepts :: ![Located PythonExcept]
    , pyTryElse    :: ![Located PythonStmt]
    , pyTryFinally :: ![Located PythonStmt]
    }
  | PyRaise !(Maybe (Located PythonExpr)) !(Maybe (Located PythonExpr))
  
  -- Function and class definitions
  | PyFuncDef !PythonFuncDef
  | PyClassDef !PythonClassDef
  
  -- Pattern matching (Python 3.10+)
  | PyMatch !PythonMatch
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python expressions
data PythonExpr
  = -- Literals and identifiers
    PyLiteral !PythonLiteral
  | PyVar !Identifier
  
  -- Operators
  | PyBinaryOp !BinaryOp !(Located PythonExpr) !(Located PythonExpr)
  | PyUnaryOp !UnaryOp !(Located PythonExpr)
  | PyComparison ![ComparisonOp] ![Located PythonExpr]  -- Chained comparisons
  | PyBoolOp !BinaryOp ![Located PythonExpr]           -- and/or with multiple operands
  
  -- Subscripting and attribute access
  | PySubscript !(Located PythonExpr) !(Located PythonSlice)
  | PyAttribute !(Located PythonExpr) !Identifier
  | PySlice !(Maybe (Located PythonExpr)) !(Maybe (Located PythonExpr)) !(Maybe (Located PythonExpr))
  
  -- Function calls
  | PyCall !(Located PythonExpr) ![Located PythonArgument]
  
  -- Collections
  | PyList ![Located PythonExpr]
  | PyTuple ![Located PythonExpr]
  | PySet ![Located PythonExpr]
  | PyDict ![(Located PythonExpr, Located PythonExpr)]
  
  -- Comprehensions
  | PyListComp !(Located PythonExpr) ![PythonComprehension]
  | PySetComp !(Located PythonExpr) ![PythonComprehension]
  | PyDictComp !(Located PythonExpr) !(Located PythonExpr) ![PythonComprehension]
  | PyGenComp !(Located PythonExpr) ![PythonComprehension]  -- Generator expression
  
  -- Lambda and conditional expressions
  | PyLambda ![Located PythonParameter] !(Located PythonExpr)
  | PyIfExp !(Located PythonExpr) !(Located PythonExpr) !(Located PythonExpr)  -- test if body else orelse
  
  -- Special expressions
  | PyStarred !(Located PythonExpr)     -- *expr
  | PyNamedExpr !(Located PythonPattern) !(Located PythonExpr)  -- Assignment expression (walrus operator)
  
  -- Async expressions
  | PyAwait !(Located PythonExpr)
  
  -- String formatting
  | PyFString ![FStringPart]   -- Improved f-string representation
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python patterns (for assignment, function parameters, and pattern matching)
data PythonPattern
  = -- Basic patterns
    PatVar !Identifier
  | PatWildcard                         -- _
  | PatLiteral !PythonLiteral
  
  -- Sequence patterns
  | PatTuple ![Located PythonPattern]
  | PatList ![Located PythonPattern]
  | PatStarred !(Located PythonPattern)
  
  -- Pattern matching patterns (Python 3.10+)
  | PatCapture !Identifier              -- name
  | PatAs !(Located PythonPattern) !Identifier  -- pattern as name
  | PatOr ![Located PythonPattern]      -- pattern | pattern
  | PatValue !(Located PythonExpr)      -- Literal or constant value pattern
  | PatSequence ![Located PythonPattern] -- Sequence pattern
  | PatMapping ![(Located PythonExpr, Located PythonPattern)] !(Maybe Identifier) -- {key: pattern, **rest}
  | PatClass !(Located PythonExpr) ![Located PythonPattern] ![(Identifier, Located PythonPattern)] -- Class(pos, keyword=pattern)
  
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | F-string parts
data FStringPart
  = FStringLiteral !Text
  | FStringExpr
    { fstrExpr       :: !(Located PythonExpr)
    , fstrConversion :: !(Maybe ConversionFlag)
    , fstrFormatSpec :: !(Maybe Text)
    } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable, NFData)

-- | F-string conversion flags
data ConversionFlag
  = ConvStr     -- !s
  | ConvRepr    -- !r
  | ConvAscii   -- !a
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python slice expressions
data PythonSlice
  = SliceIndex !(Located PythonExpr)
  | SliceSlice !(Maybe (Located PythonExpr)) !(Maybe (Located PythonExpr)) !(Maybe (Located PythonExpr))
  | SliceExtSlice ![Located PythonSlice]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python literals
data PythonLiteral
  = PyInt !Integer
  | PyFloat !Double
  | PyComplex !Double !Double
  | PyString !Text
  | PyBytes !Text
  | PyBool !Bool
  | PyNone
  | PyEllipsis
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Type parameters (Python 3.12+, PEP 695)
data TypeParam
  = TypeParamVar !Identifier !(Maybe (Located PythonTypeExpr))      -- T: bound
  | TypeParamTypeVar !Identifier !(Maybe (Located PythonTypeExpr))  -- *T: bound
  | TypeParamParamSpec !Identifier                                  -- **P
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Function definitions
data PythonFuncDef = PythonFuncDef
  { pyFuncName       :: !Identifier
  , pyFuncDecorators :: ![Located PythonDecorator]
  , pyFuncTypeParams :: ![TypeParam]      -- Type parameters [T, U] (Python 3.12+)
  , pyFuncParams     :: ![Located PythonParameter]
  , pyFuncReturns    :: !(Maybe (Located PythonTypeExpr))
  , pyFuncBody       :: ![Located PythonStmt]
  , pyFuncDoc        :: !(Maybe Text)
  , pyFuncIsAsync    :: !Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Class definitions
data PythonClassDef = PythonClassDef
  { pyClassName       :: !Identifier
  , pyClassDecorators :: ![Located PythonDecorator]
  , pyClassTypeParams :: ![TypeParam]     -- Type parameters [T, U] (Python 3.12+)
  , pyClassBases      :: ![Located PythonExpr]
  , pyClassKeywords   :: ![(Identifier, Located PythonExpr)]
  , pyClassBody       :: ![Located PythonStmt]
  , pyClassDoc        :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Function parameter
data PythonParameter
  = ParamNormal !Identifier !(Maybe (Located PythonTypeExpr)) !(Maybe (Located PythonExpr))
  | ParamVarArgs !Identifier !(Maybe (Located PythonTypeExpr))    -- *args
  | ParamKwArgs !Identifier !(Maybe (Located PythonTypeExpr))     -- **kwargs
  | ParamKwOnly !Identifier !(Maybe (Located PythonTypeExpr)) !(Maybe (Located PythonExpr))
  | ParamPosOnly !Identifier !(Maybe (Located PythonTypeExpr)) !(Maybe (Located PythonExpr)) -- Before /
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Function call argument
data PythonArgument
  = ArgPositional !(Located PythonExpr)
  | ArgKeyword !Identifier !(Located PythonExpr)
  | ArgStarred !(Located PythonExpr)    -- *expr
  | ArgKwStarred !(Located PythonExpr)  -- **expr
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Decorators
data PythonDecorator = PythonDecorator
  { pyDecoratorName :: !(Located PythonExpr)
  , pyDecoratorArgs :: ![Located PythonArgument]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Comprehension clauses
data PythonComprehension = PythonComprehension
  { pyCompTarget  :: !(Located PythonPattern)
  , pyCompIter    :: !(Located PythonExpr)
  , pyCompFilters :: ![Located PythonExpr]
  , pyCompAsync   :: !Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Import statements
data PythonImport
  = ImportModule !ModuleName !(Maybe Identifier)  -- import module [as alias]
  | ImportFrom !ModuleName ![Identifier] ![Identifier]  -- from module import names [as aliases]
  | ImportFromStar !ModuleName                     -- from module import *
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Pattern matching (Python 3.10+)
data PythonMatch = PythonMatch
  { pyMatchExpr  :: !(Located PythonExpr)
  , pyMatchCases :: ![Located PythonCase]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Match case
data PythonCase = PythonCase
  { pyCasePattern :: !(Located PythonPattern)
  , pyCaseGuard   :: !(Maybe (Located PythonExpr))
  , pyCaseBody    :: ![Located PythonStmt]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Exception handling
data PythonExcept = PythonExcept
  { pyExceptType  :: !(Maybe (Located PythonExpr))
  , pyExceptName  :: !(Maybe Identifier)
  , pyExceptBody  :: ![Located PythonStmt]
  , pyExceptGroup :: !Bool  -- True for except* (Python 3.11+)
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | With statement items
data PythonWithItem = PythonWithItem
  { pyWithContext :: !(Located PythonExpr)
  , pyWithVar     :: !(Maybe (Located PythonPattern))
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Python type expressions (for type annotations)
data PythonTypeExpr
  = TypeName !QualifiedName
  | TypeVar !Text
  | TypeSubscript !(Located PythonTypeExpr) ![Located PythonTypeExpr]
  | TypeTuple ![Located PythonTypeExpr]
  | TypeUnion ![Located PythonTypeExpr]
  | TypeOptional !(Located PythonTypeExpr)
  | TypeCallable ![Located PythonTypeExpr] !(Located PythonTypeExpr)
  | TypeLiteral !(Located PythonExpr)
  | TypeGeneric !Identifier ![Located PythonTypeExpr]  -- For generic type applications
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | Check if a statement is async
isAsyncStmt :: PythonStmt -> Bool
isAsyncStmt (PyFuncDef def) = pyFuncIsAsync def
isAsyncStmt (PyWith {pyWithAsync = async}) = async
isAsyncStmt (PyFor {pyForAsync = async}) = async
isAsyncStmt _ = False

-- | Check if an expression is async
isAsyncExpr :: PythonExpr -> Bool
isAsyncExpr (PyAwait _) = True
isAsyncExpr _ = False