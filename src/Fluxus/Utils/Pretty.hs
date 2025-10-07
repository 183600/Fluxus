{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Pretty printing utilities for AST and other data structures
module Fluxus.Utils.Pretty
  ( -- * Pretty printing class
    Pretty(..)
    -- * Pretty printing combinators
  , Doc
  , renderDoc
  , renderCompact
    -- * Basic combinators - only non-conflicting ones
  , text
  , int
  , double
  , bool
  , empty
    -- * Layout combinators - only non-conflicting ones
  , (</>)
    -- * Enclosing combinators - only export our custom versions to avoid ambiguity
  , quotes
  , doubleQuotes
    -- * Color support - only non-conflicting ones
  , red
  , green
  , blue
  , yellow
  , cyan
  , magenta
  , underline
  ) where

import Data.Text (Text)
import Prettyprinter hiding (Pretty)
import qualified Prettyprinter as PrettyDoc
import Prettyprinter.Render.Text
import Prettyprinter.Render.Terminal

-- | Class for pretty-printable types
class Pretty a where
  pretty :: a -> Doc AnsiStyle

-- | Render document to plain text
renderDoc :: Doc AnsiStyle -> Text
renderDoc = Prettyprinter.Render.Text.renderStrict . layoutPretty defaultLayoutOptions

-- | Render document in compact form (no line breaks unless necessary)
renderCompact :: Doc AnsiStyle -> Text
renderCompact = Prettyprinter.Render.Text.renderStrict . layoutCompact

-- | Basic instances
instance Pretty Text where
  pretty = PrettyDoc.pretty

instance Pretty String where
  pretty = PrettyDoc.pretty

instance Pretty Int where
  pretty = PrettyDoc.pretty

instance Pretty Integer where
  pretty = PrettyDoc.pretty

instance Pretty Double where
  pretty = PrettyDoc.pretty

instance Pretty Bool where
  pretty = PrettyDoc.pretty

instance Pretty a => Pretty [a] where
  pretty = Prettyprinter.list . map Fluxus.Utils.Pretty.pretty

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = text "Nothing"
  pretty (Just x) = text "Just" Prettyprinter.<+> Fluxus.Utils.Pretty.pretty x

-- | Re-export common combinators with simplified types
text :: Text -> Doc AnsiStyle
text = PrettyDoc.pretty

int :: Int -> Doc AnsiStyle
int = Prettyprinter.pretty

double :: Double -> Doc AnsiStyle  
double = Prettyprinter.pretty

bool :: Bool -> Doc AnsiStyle
bool True = text "true"
bool False = text "false"

empty :: Doc AnsiStyle
empty = Prettyprinter.emptyDoc


-- | Vertical composition
(</>) :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
x </> y = x <> Prettyprinter.line <> y

quotes :: Doc AnsiStyle -> Doc AnsiStyle
quotes d = text "'" <> d <> text "'"

doubleQuotes :: Doc AnsiStyle -> Doc AnsiStyle
doubleQuotes d = text "\"" <> d <> text "\""

-- | Color and style combinators
red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Prettyprinter.Render.Terminal.Red)

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Prettyprinter.Render.Terminal.Green)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Prettyprinter.Render.Terminal.Blue)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Prettyprinter.Render.Terminal.Yellow)

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Prettyprinter.Render.Terminal.Cyan)

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Prettyprinter.Render.Terminal.Magenta)

underline :: Doc AnsiStyle -> Doc AnsiStyle
underline = annotate Prettyprinter.Render.Terminal.underlined