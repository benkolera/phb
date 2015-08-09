{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Phb.Api.Internal where

import Control.Applicative       (Applicative)
import Control.Lens              (makeClassy,makeClassyPrisms,(#))
import Control.Monad.Trans       (MonadIO)
import Control.Monad.Error.Hoist ((<?>))
import Control.Monad.Reader      (MonadReader)
import Control.Monad.Except      (MonadError)

import Phb.Db         (DbEnv,HasDbEnv(dbEnv),DbError,AsDbError(_DbError))
import Phb.Types      (RecordType)

data ApiEnv = ApiEnv
  { _apiDbEnv :: DbEnv
  }
makeClassy ''ApiEnv

instance HasDbEnv ApiEnv where
  dbEnv = apiDbEnv . dbEnv

data ApiError
  = NotFound RecordType String
  | ApiDbError DbError
  deriving Show
makeClassyPrisms ''ApiError

instance AsDbError ApiError where
  _DbError = _ApiDbError . _DbError

type CanApiFail e m =
  ( MonadError e m
  , AsApiError e
  , AsDbError  e
  , Functor m
  , Applicative m
  )

type NeedsApiEnv c m =
  ( MonadReader c m
  , HasApiEnv   c
  , HasDbEnv    c
  )

type CanApi c e m =
  ( NeedsApiEnv c m
  , CanApiFail  e m
  , MonadIO       m
  )

require :: CanApiFail e m => RecordType -> String -> Maybe a -> m a
require rt n = (<?> (_NotFound # (rt,n)) )
