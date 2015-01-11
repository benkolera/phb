{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
module Phb.Db.TimeLog where

import BasePrelude hiding (on)
import Prelude     ()

import qualified Control.Lens        as L
import           Control.Monad.Trans (MonadIO)
import           Data.Time           (Day)
import           Database.Esqueleto

import Phb.Db.Internal
import Phb.Types.TimeLog

loadTimeLogWhole
  :: (MonadIO m, Applicative m)
  => Entity TimeLog
  -> Db m TimeLogWhole
loadTimeLogWhole twe = do
  let tw = entityVal twe
  p  <- getJust $ tw L.^. timeLogPerson
  pLink <- loadLink tw projectName timeLogProject ProjectLink
  eLink <- loadLink tw eventName timeLogEvent EventLink
  bLink <- loadLink tw backlogName timeLogBacklog BacklogLink
  aLink <- loadLink tw actionName timeLogAction ActionLink
  wcLink <- loadLink tw workCategoryName timeLogCategory WorkCategoryLink
  let lLink = pLink <|> eLink <|> bLink <|> aLink <|> wcLink
  pure (TimeLogWhole twe (Entity (tw L.^.timeLogPerson) p) lLink)
  where
    loadLink tw nl ll lc =
      fmap (\ (k,v) -> TimeLogLink (v L.^. nl) (lc k))
      <$>  traverse (\ k -> (k,) <$> getJust k) (tw L.^. ll)

loadTimeLogsForPeriod
  :: (MonadIO m, Applicative m)
  => Day
  -> Day
  -> Db m [TimeLogWhole]
loadTimeLogsForPeriod s f = logs >>= traverse loadTimeLogWhole
  where
    logs =
      select $ from $ \ (tl) -> do
        where_ (tl ^. TimeLogDay >=. val s &&. tl ^. TimeLogDay <=. val f)
        return tl


mkLinkOptions
  :: Functor f
  => (Key r -> l)
  -> L.Getting n r n
  -> f (Entity r)
  -> f (l, n)
mkLinkOptions lc nl =
  fmap (lc . entityKey &&& L.view nl . entityVal)
