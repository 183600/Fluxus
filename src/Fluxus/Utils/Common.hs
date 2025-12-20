{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Fluxus.Utils.Common
  ( SourcePos(..)
  , SourceSpan(..)
  , mergeSpans
  , textShow
  , defaultSpan
  , zeroWidthSpan
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Source position in a file
data SourcePos = SourcePos
  { posLine :: Int
  , posColumn :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Hashable SourcePos
instance NFData SourcePos

-- | Source span representing a range in a file
data SourceSpan = SourceSpan
  { spanFilename :: Text
  , spanStart    :: SourcePos
  , spanEnd      :: SourcePos
  } deriving (Eq, Ord, Show, Generic)

instance Hashable SourceSpan
instance NFData SourceSpan

-- Unified mergeSpans implementation
mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans (SourceSpan file start _) (SourceSpan _ _ end) = SourceSpan file start end

-- Unified textShow implementation
textShow :: Show a => a -> Text
textShow = T.pack . show

-- Unified defaultSpan implementation
defaultSpan :: Text -> SourceSpan
defaultSpan file = SourceSpan file (SourcePos 0 0) (SourcePos 0 0)

-- Unified zeroWidthSpan implementation
zeroWidthSpan :: SourceSpan -> SourceSpan
zeroWidthSpan (SourceSpan file start _) = SourceSpan file start start