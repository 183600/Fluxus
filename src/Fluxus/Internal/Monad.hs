{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Fluxus.Internal.Monad
  ( runCompilerM
  , evalCompilerM
  , execCompilerM
  , throwCompilerError
  , liftIO
  , gets
  , modify
  , asks
  ) where

import Fluxus.Internal.Types
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class

runCompilerM :: CompilerEnv -> CompilerState -> CompilerM a -> Either CompilerError (a, CompilerState)
runCompilerM env st m = runExcept $ runStateT (runReaderT m env) st

evalCompilerM :: CompilerEnv -> CompilerState -> CompilerM a -> Either CompilerError a
evalCompilerM env st m = fst <$> runCompilerM env st m

execCompilerM :: CompilerEnv -> CompilerState -> CompilerM a -> Either CompilerError CompilerState
execCompilerM env st m = snd <$> runCompilerM env st m

throwCompilerError :: CompilerError -> CompilerM a
throwCompilerError = throwError
