{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Site.Standup where

import BasePrelude
import Prelude     ()

import           Control.Lens
import           Control.Monad.Trans         (lift, liftIO)
import           Data.Text                   (Text)
import           Data.Time                   (Day)
import           Heist
import qualified Heist.Compiled              as C
import           Snap                        (ifTop)
import           Snap.Snaplet.Heist.Compiled

import Phb.Dates
import Phb.Db
import Phb.Types.Standup
import Phb.Types.Task
import Phb.Types.TimeSummary
import Site.Internal
import Site.TimeGraph

standupRoutes :: PhbRoutes
standupRoutes =
  [ ("/standup" , ifTop . userOrIndex $ render "standup" ) ]

yesterdaySplices :: Splices (PhbRuntimeSplice (TaskWhole,TimeSummary,Bool) -> PhbSplice)
yesterdaySplices = mapV (C.pureSplice . C.textSplice) $ do
  "taskName"    ## (^._2._1.taskWholeTask.eVal.taskName)
  "taskHours"   ## (^._2._2.timeSummaryHours.to showText)
  "taskClass"   ## calcClass
  where
    calcClass = bool "" "standup-completed" . (^._3)

todaySplices :: Splices (PhbRuntimeSplice TaskWhole -> PhbSplice)
todaySplices = mapV (C.pureSplice . C.textSplice) $ do
  "taskName" ## (^.taskWholeTask.eVal.taskName)

standupPersonSplices :: Splices (PhbRuntimeSplice StandupPersonSummary -> PhbSplice)
standupPersonSplices = do
  "yesterdayRow" ## rows yesterdaySplices . fmap (^.yesterdayTasks)
  "yesterdayTimeBreakdown" ## timeSummaryDataSplices . fmap (^..yesterdayTasks.traverse._2)
  "todayRow" ## rows todaySplices . fmap (^.todayTasks)
  "name" ## C.pureSplice (C.textSplice (^.person.eVal.personName))
  where
    rows = C.manyWithSplices C.runChildren

standupSplices :: PhbSplice
standupSplices = C.withSplices C.runChildren splices . lift $ do
  pure []
  where
    splices = "standupPerson" ## C.manyWithSplices C.runChildren standupPersonSplices

allStandupSplices :: Splices PhbSplice
allStandupSplices = do
  "standup" ## standupSplices
