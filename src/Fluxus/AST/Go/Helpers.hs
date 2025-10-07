{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Go-specific helper functions
module Fluxus.AST.Go.Helpers
  ( -- * Visibility helpers
    isGoPointerType
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
import Fluxus.AST.Go.Types
import Fluxus.AST.Go.Common

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