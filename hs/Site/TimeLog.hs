{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Site.TimeLog where

import BasePrelude hiding (insert)
import Prelude     ()

import           Control.Lens                  hiding (Action)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (runReaderT)
import           Control.Monad.Trans           (lift)
import qualified Data.List.NonEmpty            as NEL
import           Data.Map.Syntax
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lens                (unpacked)
import           Data.Time                     (Day, UTCTime, addDays,
                                                getCurrentTime)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (getParam, ifTop, redirect, with)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent       (runPersist)
import           Text.Digestive                as DF
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import Phb.Auth          (userDbKey)
import Phb.Db
import Phb.Mail
import Phb.Types.TimeLog
import Phb.Util
import Site.Internal

handlePester :: PhbHandler ()
handlePester = do
  y   <- isJust <$> getParam "yesterday"
  cd  <- (if y then (addDays (-1)) else id) <$> liftIO getCurrentDay
  ps  <- runPersist $ missingTimeLogsFor cd
  view mail >>= liftIO . runReaderT (traverse_ (timelogPesterEmail cd) ps)
  flashSuccess "Pestered!"
  redirect "/time_logs"

timeLogRoutes :: PhbRoutes
timeLogRoutes =
  [ ("/time_logs"               , ifTop . userOrIndex $ render "time_logs/all")
  , ("/time_logs/pester"        , handlePester)
  , ("/time_logs/create"        , ifTop . userOrIndex $ render "time_logs/create" )
  , ("/time_logs/mine"          , ifTop . userOrIndex $ render "time_logs/mine" )
  , ("/time_logs/by_day/:day"   , ifTop . userOrIndex $ render "time_logs/by_day" )
  , ("/time_logs/by_user/:user" , ifTop . userOrIndex $ render "time_logs/by_user" )
  , ("/time_logs/:id/edit"      , ifTop . userOrIndex $ render "time_logs/edit" )
  ]

data TimeLogInput = TimeLogInput
  { _timeLogInputPerson :: (Key Person)
  , _timeLogInputDay    :: Day
  , _timeLogInputDesc   :: T.Text
  , _timeLogInputHours  :: Int
  , _timeLogInputMins   :: Int
  , _timeLogInputLink   :: LinkKey
  } deriving Show
makeLenses ''TimeLogInput


data TimeLogInputRow = TimeLogInputRow
  { _timeLogInputRowDesc  :: T.Text
  , _timeLogInputRowHours :: Int
  , _timeLogInputRowMins  :: Int
  , _timeLogInputRowLink  :: LinkKey
  } deriving Show
makeLenses ''TimeLogInputRow

data TimeLogInputMany = TimeLogInputMany
  { _timeLogInputManyPerson :: (Key Person)
  , _timeLogInputManyDay    :: Day
  , _timeLogInputManyLogs   :: NEL.NonEmpty TimeLogInputRow
  } deriving Show
makeLenses ''TimeLogInputMany


userErrMsg,timeErrMsg :: T.Text
userErrMsg = "Username cannot be empty"
timeErrMsg = "Time spent must be greater than 0"

personChoiceOption :: Entity Person -> (Key Person,T.Text)
personChoiceOption =
  entityKey &&& (^. to entityVal . personName)

groupLinkKeys :: [(LinkKey,Text)] -> [(Text,[(LinkKey,Text)])]
groupLinkKeys=
  -- This could use Data.Map.insertWith except for the fact that
  -- I want to order the output based on the Ord of LinkKey
  -- TODO: Gotta be a better way though.
  fmap (fst . NEL.head &&& fmap snd . toList)
  . NEL.groupBy (on (==) fst)
  . fmap (linkTypeName . fst &&& id)
  . sortBy (on compare fst)
  where
    linkTypeName (ProjectLink _)      = "Project"
    linkTypeName (BacklogLink _)      = "Backlog"
    linkTypeName (ActionLink _)       = "Action"
    linkTypeName (EventLink _)        = "Event"
    linkTypeName (WorkCategoryLink _) = "Work Category"


timeLogForm :: TimeLogWhole -> PhbForm T.Text TimeLogInput
timeLogForm tl =
  check timeErrMsg timeOk $ monadic $ do
  ct <- liftIO getCurrentTime
  cd <- liftIO $ localDayFromUTC ct
  (lk,pp) <- runPersist $ timeLogFormOptions ct
  p  <- (userDbKey =<<) <$> with auth currentUser
  return $ TimeLogInput
    <$> "username" .: choice pp (username <|> p)
    <*> "day"      .: html5DateFormlet (day <|> Just cd)
    <*> "desc"     .: text desc
    <*> "hours"    .: positiveIntForm "Hours" hours
    <*> "minutes"  .: positiveIntForm "Minutes" minutes
    <*> "link"     .: groupedChoice (groupLinkKeys lk) link
  where
    timeOk i = ((i^.timeLogInputHours) + (i^.timeLogInputMins)) > 0
    username = (tl ^? timeLogWholeLog . eVal . timeLogPerson )
    day      = (tl ^? timeLogWholeLog . eVal . timeLogDay )
    desc     = (tl ^? timeLogWholeLog . eVal . timeLogDesc )
    hours    = allMins <&> (`div` 60)
    minutes  = allMins <&> (`mod` 60)
    allMins  = (tl ^? timeLogWholeLog . eVal . timeLogMinutes )
    link     = (tl ^? timeLogWholeLink . _Just . timeLogLinkKey )

timeLogFormMany :: PhbForm T.Text TimeLogInputMany
timeLogFormMany = monadic $ do
  ct <- liftIO getCurrentTime
  cd <- liftIO $ localDayFromUTC ct
  (lk,pp) <- runPersist $ timeLogFormOptions ct
  p  <- (userDbKey =<<) <$> with auth currentUser
  return $ TimeLogInputMany
    <$> "username" .: choice pp p
    <*> "day"      .: html5DateFormlet (Just cd)
    <*> "rows"     .: nelOf rowsErr (timeLogRowForm lk) Nothing
  where
    rowsErr = "Must supply at least one time log"
    timeLogRowForm lk _ = check timeErrMsg timeOk
      $ TimeLogInputRow
      <$> "desc"     .: text Nothing
      <*> "hours"    .: positiveIntForm "Hours" Nothing
      <*> "minutes"  .: positiveIntForm "Minutes" Nothing
      <*> "link"     .: groupedChoice (groupLinkKeys lk) Nothing

    timeOk i = ((i^.timeLogInputRowHours) + (i^.timeLogInputRowMins)) > 0

timeLogFormOptions
  :: (MonadIO m, Applicative m)
  => UTCTime
  -> Db m ([(LinkKey, Text)], [(Key Person, Text)])
timeLogFormOptions ct = (,)
  <$> loadLinkOptions
  <*> (fmap personChoiceOption <$> selectList [PersonLogsTime ==. True] [Asc PersonName])
  where
    loadLinkOptions = do
      fmap fold . sequence $
        [ loadLinkOptions' ProjectLink projectName (loadActiveProjects ct)
        , loadLinkOptions' BacklogLink backlogName (loadActiveBacklog ct)
        , loadLinkOptions' EventLink eventName (loadActiveEvents ct)
        , loadLinkOptions' ActionLink actionName (loadActiveActions ct)
        , loadLinkOptions' WorkCategoryLink workCategoryName (loadActiveWorkCategories ct)
        ]
    loadLinkOptions' lc nl a = mkLinkOptions lc nl <$> a

positiveIntForm
  :: (Show a, Read a, Ord a, Num a, Monad m)
  => Text
  -> Maybe a
  -> Form Text m a
positiveIntForm thing n = validate vf (text (T.pack . show <$> n))
  where
    vf "" = DF.Success 0
    vf x  = maybe err DF.Success . mfilter (>= 0) . readMaybe . T.unpack $ x
    err = DF.Error $ thing <> " must be a positive number"

editTimeLogSplices :: PhbSplice
editTimeLogSplices = do
  promise <- C.newEmptyPromise
  outputChildren <- C.withSplices C.runChildren splice (C.getPromise promise)
  pure . C.yieldRuntime $ do
    tlw <- lift $ do
      requireEntity "time_log" "id" >>= runPersist . loadTimeLogWhole
    (v, result) <- lift $ runForm "timeLog" (timeLogForm tlw)
    case result of
      Just x  -> lift (updateTimeLog x (tlw ^. timeLogWholeLog . eKey))
      Nothing -> C.putPromise promise v >> C.codeGen outputChildren
  where
    splice = do
      "timeLogForm" ## formSplice mempty mempty

    updateTimeLog x k = do
      void . runPersist $ replace k (newTimeLog x)
      flashSuccess $ "Timelog Updated"
      redirect "/time_logs"

    newTimeLog (TimeLogInput p dy d h m l) =
      TimeLog p d (h*60 + m) dy
        (l ^? _ProjectLink)
        (l ^? _EventLink)
        (l ^? _BacklogLink)
        (l ^? _ActionLink)
        (l ^? _WorkCategoryLink)

createTimeLogSplices :: PhbSplice
createTimeLogSplices = do
  promise <- C.newEmptyPromise
  outputChildren <- C.withSplices C.runChildren splice (C.getPromise promise)
  pure . C.yieldRuntime $ do
    (v, result) <- lift $ runForm "timeLogMany" timeLogFormMany
    case result of
      Just x  -> lift (createTimeLog x)
      Nothing -> C.putPromise promise v >> C.codeGen outputChildren
  where
    splice = do
      "timeLogFormMany" ## formSplice mempty mempty

    createTimeLog x = do
      void . runPersist $ insertMany (newTimeLogs x)
      flashSuccess $ "Timelog Created"
      redirect "/time_logs/mine"

    newTimeLogs (TimeLogInputMany p dy rs) = toList
      . fmap (newTimeLog p dy)
      $ rs
    newTimeLog p dy (TimeLogInputRow d h m l) =
      TimeLog p d (h*60 + m) dy
        (l ^? _ProjectLink)
        (l ^? _EventLink)
        (l ^? _BacklogLink)
        (l ^? _ActionLink)
        (l ^? _WorkCategoryLink)

timeLogSplices :: Splices (PhbRuntimeSplice (TimeLogWhole) -> PhbSplice)
timeLogSplices = mapV (C.pureSplice . C.textSplice) $ do
  "personId"    ## (^.timeLogWholeLog.eVal.timeLogPerson.to spliceKey)
  "username"    ## (^.timeLogWholePerson.eVal.personName)
  "day"         ## (^.timeLogWholeLog.eVal.timeLogDay.to spliceDay)
  "minutes"     ## (^.timeLogWholeLog.eVal.timeLogMinutes.to show.from unpacked)
  "timeAgainst" ## (^.timeLogWholeLink._Just.timeLogLinkName )
  "notes"       ## (^.timeLogWholeLog.eVal.timeLogDesc)
  "id"          ## (^.timeLogWholeLog.eKey.to spliceKey )

timeLogsSplices :: PhbRuntimeSplice (Text,[TimeLogWhole]) -> PhbSplice
timeLogsSplices = C.withSplices C.runChildren $ do
  "timeLogTitle" ## (C.pureSplice . C.textSplice) fst
  "timeLogRow"   ## C.manyWithSplices C.runChildren timeLogSplices . fmap snd


byDayTimeLogsSplices :: PhbSplice
byDayTimeLogsSplices = do
  timeLogsSplices . lift $ do
    pgs   <- defPaginationParam
    d     <- requireOr400 "Invalid Day" =<< dateParam "day"
    runPersist $ do
      twes <- selectList [TimeLogDay ==. d] (pgs ++ [Desc TimeLogId])
      twL <- traverse loadTimeLogWhole twes
      pure (T.pack (show d),twL)

byUserTimeLogsSplices :: PhbSplice
byUserTimeLogsSplices = do
  timeLogsSplices . lift $ do
    pgs   <- defPaginationParam
    p     <- requireEntity "UserById" "user"
    runPersist $ do
      twes <- selectList [TimeLogPerson ==. (entityKey p)] (pgs ++ [Desc TimeLogDay])
      twL <- traverse loadTimeLogWhole twes
      pure (p^.eVal.personName,twL)

myTimeLogsSplices :: PhbSplice
myTimeLogsSplices = do
  timeLogsSplices . lift $ do
    pgs    <- defPaginationParam
    Just p <- (userDbKey =<<) <$> with auth currentUser
    runPersist $ do
      twes <- selectList [TimeLogPerson ==. p] (pgs ++ [Desc TimeLogDay])
      twL <- traverse loadTimeLogWhole twes
      pure ("My Time Logs",twL)

allTimeLogsSplices :: PhbSplice
allTimeLogsSplices = do
  timeLogsSplices . lift $ do
    pgs <- defPaginationParam
    runPersist $ do
      twes <- selectList [] (pgs ++ [Desc TimeLogDay])
      twL <- traverse loadTimeLogWhole twes
      pure ("All",twL)

allTimeLogSplices :: Splices PhbSplice
allTimeLogSplices = do
  "allTimeLogs"    ## allTimeLogsSplices
  "myTimeLogs"     ## myTimeLogsSplices
  "byDayTimeLogs"  ## byDayTimeLogsSplices
  "byUserTimeLogs" ## byUserTimeLogsSplices
  "createTimeLog"  ## createTimeLogSplices
  "editTimeLog"    ## editTimeLogSplices
