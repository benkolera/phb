module Phb.Db.WorkCategory where

import Control.Monad.Trans (MonadIO)
import Data.Time           (UTCTime)
import Database.Esqueleto

import Phb.Db.Internal

loadActiveWorkCategories
  :: MonadIO m
  => UTCTime
  -> SqlPersistT m [Entity WorkCategory]
loadActiveWorkCategories _ = select $ from $ \ wc -> return wc
