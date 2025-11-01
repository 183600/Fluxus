{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Fluxus.CodeGen.CPP.Go
  ( generateCppFromGo
  ) where

import Control.Monad (when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (gets, modify)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Fluxus.AST.Common
import Fluxus.AST.Go
import Fluxus.CodeGen.CPP.Diagnostics (CppCodeGenError(..))
import Fluxus.CodeGen.CPP.Emit
import Fluxus.CodeGen.CPP.Monad
import Fluxus.CodeGen.CPP.Types

notImplemented :: Text -> CppCodeGen a
notImplemented msg = do
  emitError $ "go codegen not implemented: " <> msg
  throwError (CppNotImplemented msg)

unsupported :: Text -> CppCodeGen a
unsupported msg = do
  emitError $ "go codegen unsupported: " <> msg
  throwError (CppUnsupported msg)

-- | Entry point for Go code generation
generateCppFromGo :: GoAST -> CppCodeGen CppUnit
generateCppFromGo (GoAST goPackage) = do
  addInclude "<iostream>"
  addInclude "<string>"
  addInclude "<thread>"
  addInclude "<mutex>"
  addInclude "<condition_variable>"
  addInclude "<queue>"
  addInclude "<vector>"
  addInclude "<functional>"
  addInclude "<atomic>"
  addInclude "<chrono>"

  let packageName = (
        \(Identifier n) -> n
      ) (goPackageName goPackage)

  addComment $ "Generating C++ for Go package: " <> packageName
  generateChannelClass

  let files = goPackageFiles goPackage
  addComment $ "Found " <> T.pack (show (length files)) <> " files in package"
  when (null files) $ addComment "No files found in package"
  mapM_ generateGoFile files

  when (packageName == "main") $ do
    hasMain <- gets (any isMainFunction . cgsDeclarations)
    unless hasMain $ do
      addComment "Generating fallback main function"
      addInclude "<iostream>"
      addDeclaration $ CppFunction "main" CppInt [] [CppReturn (Just (CppLiteral (CppIntLit 0)))]

  includes <- gets cgsIncludes
  namespaces <- gets cgsNamespaces
  decls <- gets cgsDeclarations
  pure $ CppUnit includes namespaces (reverse decls)
  where
    isMainFunction (CppFunction "main" _ _ _) = True
    isMainFunction _ = False

-- | Generate Channel class for Go channel operations
-- This implements a generic channel that can handle any type via templates
generateChannelClass :: CppCodeGen ()
generateChannelClass = do
  let templateParam = CppTemplateType "T" []
      queueType = CppTemplateType "std::queue" [templateParam]
      mutexType = CppClassType "std::mutex" []
      cvType = CppClassType "std::condition_variable" []
      lockType = CppTemplateType "std::unique_lock" [mutexType]

      publicMembers =
        [ CppAccessSpec "public"
        , CppConstructor "Channel" [CppParam "capacity" CppSizeT Nothing]
            [ CppExprStmt $ CppBinary "=" (CppMember CppThis "capacity_") (CppVar "capacity")
            ]
        , CppMethod "send" CppVoid [CppParam "value" templateParam Nothing]
            [ CppDecl $ CppVariable "lock" lockType
                (Just $ CppCall (CppVar "std::unique_lock<std::mutex>") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "wait")
                [ CppVar "lock"
                , CppLambda []
                    [ CppReturn $ Just $ CppBinary "<"
                        (CppCall (CppMember (CppMember CppThis "queue_") "size") [])
                        (CppMember CppThis "capacity_")
                    ]
                ]
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "push") [CppVar "value"]
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "notify_one") []
            ] False
        , CppMethod "receive" templateParam []
            [ CppDecl $ CppVariable "value" templateParam Nothing
            , CppDecl $ CppVariable "lock" lockType
                (Just $ CppCall (CppVar "std::unique_lock<std::mutex>") [CppMember CppThis "mutex_"])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "wait")
                [ CppVar "lock"
                , CppLambda []
                    [ CppReturn $ Just $ CppUnary "!"
                        (CppCall (CppMember (CppMember CppThis "queue_") "empty") [])
                    ]
                ]
            , CppExprStmt $ CppBinary "=" (CppVar "value") (CppCall (CppMember (CppMember CppThis "queue_") "front") [])
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "queue_") "pop") []
            , CppExprStmt $ CppCall (CppMember (CppMember CppThis "cv_") "notify_one") []
            , CppReturn $ Just $ CppVar "value"
            ] False
        ]

      privateMembers =
        [ CppAccessSpec "private"
        , CppVariable "queue_" queueType Nothing
        , CppVariable "mutex_" mutexType Nothing
        , CppVariable "cv_" cvType Nothing
        , CppVariable "capacity_" CppSizeT Nothing
        ]

  addDeclaration $ CppTemplate ["T"] (CppClass "Channel" [] (publicMembers ++ privateMembers))
