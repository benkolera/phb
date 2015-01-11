{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Phb.Db.Success where

import BasePrelude hiding (delete, insert)
import Prelude     ()

import           Control.Lens               hiding (from, (^.))
import qualified Control.Lens               as L
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Reader (runReader)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.Esqueleto

import Phb.Db.Internal

data HeartbeatSuccessInput = HeartbeatSuccessInput
  { _heartbeatSuccessInputWhat         :: Text
  , _heartbeatSuccessInputAchievements :: [Text]
  , _heartbeatSuccessInputPeople       :: [Key Person]
  }
makeLenses ''HeartbeatSuccessInput

upsertHeartbeatSuccesses
  :: (MonadIO m, Applicative m)
  => Key Heartbeat
  -> [HeartbeatSuccessInput]
  -> SqlPersistT m ()
upsertHeartbeatSuccesses hk ss = do
  let sQuery = from $ \ s -> do
        where_ (s ^. SuccessHeartbeat ==. val hk)
        return (s ^. SuccessId)

  delete $ from $ \ sp -> do
    where_ (sp ^. SuccessPersonSuccess `in_` subList_select sQuery)

  delete (void sQuery)

  for_ ss $ \ s -> do
    k <- insert (successInputToDb s)
    insertMany . fmap (SuccessPerson k) $ s L.^.heartbeatSuccessInputPeople

  where
    successInputToDb = runReader $ Success
      <$> pure hk
      <*> view heartbeatSuccessInputWhat
      <*> view (heartbeatSuccessInputAchievements.to T.unlines)
