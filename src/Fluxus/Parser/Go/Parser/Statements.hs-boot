module Fluxus.Parser.Go.Parser.Statements (parseBlockStmt') where

import Control.Monad.Logger (MonadLogger)

import Fluxus.AST.Go (GoStmt)
import Fluxus.Parser.Go.Parser.Common (GoParser)

parseBlockStmt' :: MonadLogger m => GoParser m GoStmt
