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
-- import qualified Data.Text as T  -- unused
import Prettyprinter hiding (Pretty)
import qualified Prettyprinter as PrettyDoc
import Prettyprinter.Render.Text
import Prettyprinter.Render.Terminal
-- import System.Console.ANSI (ColorIntensity(..), ConsoleLayer(..))  -- unused

-- | Class for pretty-printable types
class Pretty a where
  pretty :: a -> Doc AnsiStyle

-- | Render document to plain text
renderDoc :: Doc AnsiStyle -> Text
renderDoc = Data.Text.Prettyprint.Doc.Render.Text.renderStrict . layoutPretty defaultLayoutOptions

-- | Render document in compact form (no line breaks unless necessary)
renderCompact :: Doc AnsiStyle -> Text
renderCompact = Data.Text.Prettyprint.Doc.Render.Text.renderStrict . layoutCompact

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
  pretty = Data.Text.Prettyprint.Doc.list . map Fluxus.Utils.Pretty.pretty

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = text "Nothing"
  pretty (Just x) = text "Just" Data.Text.Prettyprint.Doc.<+> Fluxus.Utils.Pretty.pretty x

-- | Re-export common combinators with simplified types
text :: Text -> Doc AnsiStyle
text = PrettyDoc.pretty

int :: Int -> Doc AnsiStyle
int = Data.Text.Prettyprint.Doc.pretty

double :: Double -> Doc AnsiStyle  
double = Data.Text.Prettyprint.Doc.pretty

bool :: Bool -> Doc AnsiStyle
bool True = text "true"
bool False = text "false"

empty :: Doc AnsiStyle
empty = Data.Text.Prettyprint.Doc.emptyDoc

_space :: Doc AnsiStyle
_space = Prettyprinter.space

_line :: Doc AnsiStyle
_line = Prettyprinter.line

_softline :: Doc AnsiStyle
_softline = Prettyprinter.softline

-- | Horizontal composition with space
(<++>) :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
(<++>) = (Prettyprinter.<+>)

-- | Vertical composition
(</>) :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
x </> y = x <> Data.Text.Prettyprint.Doc.line <> y

-- | Horizontal separation
_hsep :: [Doc AnsiStyle] -> Doc AnsiStyle
_hsep = Prettyprinter.hsep

-- | Vertical separation
_vsep :: [Doc AnsiStyle] -> Doc AnsiStyle
_vsep = Prettyprinter.vsep

-- | Flexible separation (horizontal or vertical)
_sep :: [Doc AnsiStyle] -> Doc AnsiStyle
_sep = Prettyprinter.sep

-- | Concatenation without separation
_cat :: [Doc AnsiStyle] -> Doc AnsiStyle
_cat = Prettyprinter.cat

-- | Horizontal concatenation
_hcat :: [Doc AnsiStyle] -> Doc AnsiStyle
_hcat = Prettyprinter.hcat

-- | Vertical concatenation
_vcat :: [Doc AnsiStyle] -> Doc AnsiStyle
_vcat = Prettyprinter.vcat

-- | Enclosing combinators
_parens :: Doc AnsiStyle -> Doc AnsiStyle
_parens = Prettyprinter.parens

_brackets :: Doc AnsiStyle -> Doc AnsiStyle
_brackets = Prettyprinter.brackets

_braces :: Doc AnsiStyle -> Doc AnsiStyle
_braces = Prettyprinter.braces

_angles :: Doc AnsiStyle -> Doc AnsiStyle
_angles = Prettyprinter.angles

quotes :: Doc AnsiStyle -> Doc AnsiStyle
quotes d = text "'" <> d <> text "'"

doubleQuotes :: Doc AnsiStyle -> Doc AnsiStyle
doubleQuotes d = text "\"" <> d <> text "\""

-- | Indentation
_indent :: Int -> Doc AnsiStyle -> Doc AnsiStyle
_indent = Prettyprinter.indent

_hang :: Int -> Doc AnsiStyle -> Doc AnsiStyle
_hang = Prettyprinter.hang

_align :: Doc AnsiStyle -> Doc AnsiStyle
_align = Prettyprinter.align

-- | Grouping
_group :: Doc AnsiStyle -> Doc AnsiStyle
_group = Prettyprinter.group

_nest :: Int -> Doc AnsiStyle -> Doc AnsiStyle
_nest = Prettyprinter.nest

-- | List formatting
_list :: [Doc AnsiStyle] -> Doc AnsiStyle
_list = Prettyprinter.list

_tupled :: [Doc AnsiStyle] -> Doc AnsiStyle
_tupled = Prettyprinter.tupled

_punctuate :: Doc AnsiStyle -> [Doc AnsiStyle] -> [Doc AnsiStyle]
_punctuate = Prettyprinter.punctuate

-- | Color and style combinators
red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Data.Text.Prettyprint.Doc.Render.Terminal.Red)

green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Data.Text.Prettyprint.Doc.Render.Terminal.Green)

blue :: Doc AnsiStyle -> Doc AnsiStyle
blue = annotate (color Data.Text.Prettyprint.Doc.Render.Terminal.Blue)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Data.Text.Prettyprint.Doc.Render.Terminal.Yellow)

cyan :: Doc AnsiStyle -> Doc AnsiStyle
cyan = annotate (color Data.Text.Prettyprint.Doc.Render.Terminal.Cyan)

magenta :: Doc AnsiStyle -> Doc AnsiStyle
magenta = annotate (color Data.Text.Prettyprint.Doc.Render.Terminal.Magenta)

_bold :: Doc AnsiStyle -> Doc AnsiStyle
_bold = annotate Prettyprinter.Render.Terminal.bold

underline :: Doc AnsiStyle -> Doc AnsiStyle
underline = annotate Data.Text.Prettyprint.Doc.Render.Terminal.underlined