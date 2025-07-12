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
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding (Pretty)
import qualified Data.Text.Prettyprint.Doc as PrettyDoc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Console.ANSI (ColorIntensity(..), ConsoleLayer(..))

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

space :: Doc AnsiStyle
space = Data.Text.Prettyprint.Doc.space

line :: Doc AnsiStyle
line = Data.Text.Prettyprint.Doc.line

softline :: Doc AnsiStyle
softline = Data.Text.Prettyprint.Doc.softline

-- | Horizontal composition with space
(<+>) :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
(<+>) = (Data.Text.Prettyprint.Doc.<+>)

-- | Vertical composition
(</>) :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
x </> y = x <> Data.Text.Prettyprint.Doc.line <> y

-- | Horizontal separation
hsep :: [Doc AnsiStyle] -> Doc AnsiStyle
hsep = Data.Text.Prettyprint.Doc.hsep

-- | Vertical separation
vsep :: [Doc AnsiStyle] -> Doc AnsiStyle
vsep = Data.Text.Prettyprint.Doc.vsep

-- | Flexible separation (horizontal or vertical)
sep :: [Doc AnsiStyle] -> Doc AnsiStyle
sep = Data.Text.Prettyprint.Doc.sep

-- | Concatenation without separation
cat :: [Doc AnsiStyle] -> Doc AnsiStyle
cat = Data.Text.Prettyprint.Doc.cat

-- | Horizontal concatenation
hcat :: [Doc AnsiStyle] -> Doc AnsiStyle
hcat = Data.Text.Prettyprint.Doc.hcat

-- | Vertical concatenation
vcat :: [Doc AnsiStyle] -> Doc AnsiStyle
vcat = Data.Text.Prettyprint.Doc.vcat

-- | Enclosing combinators
parens :: Doc AnsiStyle -> Doc AnsiStyle
parens = Data.Text.Prettyprint.Doc.parens

brackets :: Doc AnsiStyle -> Doc AnsiStyle
brackets = Data.Text.Prettyprint.Doc.brackets

braces :: Doc AnsiStyle -> Doc AnsiStyle
braces = Data.Text.Prettyprint.Doc.braces

angles :: Doc AnsiStyle -> Doc AnsiStyle
angles = Data.Text.Prettyprint.Doc.angles

quotes :: Doc AnsiStyle -> Doc AnsiStyle
quotes d = text "'" <> d <> text "'"

doubleQuotes :: Doc AnsiStyle -> Doc AnsiStyle
doubleQuotes d = text "\"" <> d <> text "\""

-- | Indentation
indent :: Int -> Doc AnsiStyle -> Doc AnsiStyle
indent = Data.Text.Prettyprint.Doc.indent

hang :: Int -> Doc AnsiStyle -> Doc AnsiStyle
hang = Data.Text.Prettyprint.Doc.hang

align :: Doc AnsiStyle -> Doc AnsiStyle
align = Data.Text.Prettyprint.Doc.align

-- | Grouping
group :: Doc AnsiStyle -> Doc AnsiStyle
group = Data.Text.Prettyprint.Doc.group

nest :: Int -> Doc AnsiStyle -> Doc AnsiStyle
nest = Data.Text.Prettyprint.Doc.nest

-- | List formatting
list :: [Doc AnsiStyle] -> Doc AnsiStyle
list = Data.Text.Prettyprint.Doc.list

tupled :: [Doc AnsiStyle] -> Doc AnsiStyle
tupled = Data.Text.Prettyprint.Doc.tupled

punctuate :: Doc AnsiStyle -> [Doc AnsiStyle] -> [Doc AnsiStyle]
punctuate = Data.Text.Prettyprint.Doc.punctuate

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

bold :: Doc AnsiStyle -> Doc AnsiStyle
bold = annotate Data.Text.Prettyprint.Doc.Render.Terminal.bold

underline :: Doc AnsiStyle -> Doc AnsiStyle
underline = annotate Data.Text.Prettyprint.Doc.Render.Terminal.underlined