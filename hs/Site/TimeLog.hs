{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Site.TimeLog where

import           BasePrelude                   hiding (insert)
import           Prelude                       ()

import           Control.Error
import           Control.Lens                  hiding (Action)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (runReaderT)
import           Control.Monad.Trans           (lift)
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as NEL
import           Data.Map.Syntax
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lens                (unpacked)
import           Data.Time                     (Day, addDays)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (getParam, ifTop, redirect)
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent       (runPersist)
import           Text.Digestive                as DF
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import           Phb.Dates
import           Phb.Db
import           Phb.Mail
import           Phb.Types.Task
import           Phb.Types.TimeLog
import           Phb.Util
import           Site.Internal
import           Site.TimeGraph                (timeSummaryDataSplices)

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
  [ ("/time_logs"               , ifTop . userOrIndex $ render "time_logs/list")
  , ("/time_logs/pester"        , handlePester)
  , ("/time_logs/create"        , ifTop . userOrIndex $ render "time_logs/create" )
  , ("/time_logs/:id/edit"      , ifTop . userOrIndex $ render "time_logs/edit" )
  ]

data TimeLogInput = TimeLogInput
  { _timeLogInputPerson :: (Key Person)
  , _timeLogInputDay    :: Day
  , _timeLogInputDesc   :: T.Text
  , _timeLogInputHours  :: Int
  , _timeLogInputMins   :: Int
  , _timeLogInputTask   :: (Key Task)
  } deriving Show
makeLenses ''TimeLogInput


data TimeLogInputRow = TimeLogInputRow
  { _timeLogInputRowDesc  :: T.Text
  , _timeLogInputRowHours :: Int
  , _timeLogInputRowMins  :: Int
  , _timeLogInputRowTask  :: (Key Task)
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

timeLogForm :: TimeLogWhole -> PhbForm T.Text TimeLogInput
timeLogForm tl =
  check timeErrMsg timeOk $ monadic $ do
  (_,p)   <- requireCurrentUser
  cd      <- liftIO $ getCurrentDay
  (ts,pp) <- runPersist $ timeLogFormOptions p cd
  return $ TimeLogInput
    <$> "username" .: choice pp (username <|> Just p)
    <*> "day"      .: html5DateFormlet (day <|> Just cd)
    <*> "desc"     .: text desc
    <*> "hours"    .: positiveIntForm "Hours" hours
    <*> "minutes"  .: positiveIntForm "Minutes" minutes
    <*> "task"     .: choice ts task
  where
    timeOk i = ((i^.timeLogInputHours) + (i^.timeLogInputMins)) > 0
    username = (tl ^? timeLogWholeLog . eVal . timeLogPerson )
    day      = (tl ^? timeLogWholeLog . eVal . timeLogDay )
    desc     = (tl ^? timeLogWholeLog . eVal . timeLogDesc )
    hours    = allMins <&> (`div` 60)
    minutes  = allMins <&> (`mod` 60)
    allMins  = (tl ^? timeLogWholeLog . eVal . timeLogMinutes )
    task     = (tl ^? timeLogWholeTask.taskWholeTask.eKey)

timeLogFormUserDate :: PhbForm T.Text (Key Person,Day)
timeLogFormUserDate = monadic $ do
  (_,cp) <- requireCurrentUser
  cd     <- liftIO getCurrentDay
  pp     <- runPersist $ personOptions
  return $ (,)
    <$> "person" .: choice pp (Just cp)
    <*> "date"    .: html5DateFormlet (Just cd)

timeLogFormRows :: (Key Person,Day) -> PhbForm T.Text TimeLogInputMany
timeLogFormRows (p,cd) = monadic $ do
  ts <- runPersist $ taskOptions p cd
  return $ TimeLogInputMany
    <$> pure p
    <*> pure cd
    <*> "rows"     .: nelOf rowsErr (timeLogRowForm ts) (Just $ initRows ts)
  where
    rowsErr = "Must supply at least one time log"
    timeLogRowForm ts r = check timeErrMsg timeOk
      $ TimeLogInputRow
      <$> "desc"     .: text (r^?_Just.timeLogInputRowDesc)
      <*> "hours"    .: positiveIntForm "Hours" (r^?_Just.timeLogInputRowHours)
      <*> "minutes"  .: positiveIntForm "Minutes" (r^?_Just.timeLogInputRowMins)
      <*> "task"     .: choice ts (r^?_Just.timeLogInputRowTask)

    timeOk i = ((i^.timeLogInputRowHours) + (i^.timeLogInputRowMins)) >= 0
    initRows ts = fmap (TimeLogInputRow "" 0 0 . fst) ts

timeLogFormOptions
  :: (MonadIO m, Applicative m)
  => Key Person
  -> Day
  -> Db m ([(Key Task, Text)], [(Key Person, Text)])
timeLogFormOptions pk cd = (,) <$> taskOptions pk cd <*> personOptions

personOptions
  :: (Applicative m)
  => Db m [(Key Person, Text)]
personOptions = do
  pps <- selectList [PersonLogsTime ==. True] [Asc PersonName]
  pure . fmap personChoiceOption $ pps

taskOptions
  :: (MonadIO m, Applicative m)
  => Key Person
  -> Day
  -> Db m ([(Key Task, Text)])
taskOptions pk cd = do
  tw <- loadTasksForPeopleForDay [pk] cd
  pure . fmap taskOption $ tw
  where
    taskOption =
      (   (^.taskWholeTask.eKey)
      &&& (^.taskWholeLink._Just.to taskLinkText))
    taskLinkText tl =
      (tl^.taskLinkKey.to linkTypeName) <> " - " <> (tl^.taskLinkName)


positiveIntForm
  :: (Show a, Read a, Ord a, Num a, Monad m)
  => Text
  -> Maybe a
  -> Form Text m a
positiveIntForm thing n = validate vf (text (T.pack . show <$> n))
  where
    vf "" = DF.Success 0
    vf x  = maybe e DF.Success . mfilter (>= 0) . readMaybe . T.unpack $ x
    e     = DF.Error $ thing <> " must be a positive number"

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

    newTimeLog (TimeLogInput p dy d h m t) =
      TimeLog p d (h*60 + m) dy t

createTimeLogSplices :: PhbSplice
createTimeLogSplices = do
  promise     <- C.newEmptyPromise
  outChildren <- C.withSplices C.runChildren splices (C.getPromise promise)
  pure . C.yieldRuntime $ do
    changeUD <- lift $ (maybe False (== "Change Person/Date") <$> getParam "action")
    (hv, hRes) <- lift $ runForm "timeLogUserDate" timeLogFormUserDate
    rps <- lift $ if changeUD then rowParamsFromForm hRes else rowParamsFromEnv
    let f = timeLogFormRows rps
    let fname = "timeLogMany"
    if changeUD
    then do
      rv <- lift $ getForm fname f
      C.putPromise promise (hv,rv) >> C.codeGen outChildren
    else do
      (rv, rRes) <- lift $ runForm fname f
      case rRes of
        Just x  -> lift (createTimeLog x)
        Nothing ->
          C.putPromise promise (hv,rv) >> C.codeGen outChildren
  where
    splices = do
      "timeLogFormRows" ## formSplice mempty mempty . fmap snd
      "timeLogUserDate" ## formSplice mempty mempty . fmap fst

    rowParamsFromEnv :: PhbHandler (Key Person,Day)
    rowParamsFromEnv = (,)
      <$> (fmap snd requireCurrentUser)
      <*> (liftIO getCurrentDay)

    rowParamsFromForm :: Maybe (Key Person,Day) -> PhbHandler (Key Person,Day)
    rowParamsFromForm res = maybe rowParamsFromEnv pure res

    createTimeLog x = do
      void . runPersist $ insertMany (newTimeLogs x)
      flashSuccess $ "Timelog Created"
      redirect "/time_logs?user=me"

    newTimeLogs (TimeLogInputMany p dy rs) =
      filter (^.timeLogMinutes.to (> 0))
      . toList
      . fmap (newTimeLog p dy)
      $ rs
    newTimeLog p dy (TimeLogInputRow d h m t) =
      TimeLog p d (h*60 + m) dy t

timeLogSplices
  :: Splices (PhbRuntimeSplice
              ( TimeLogWhole
              )
  -> PhbSplice)
timeLogSplices = mapV (C.pureSplice . C.textSplice) $ do
  "personId"    ## (^.timeLogWholeLog.eVal.timeLogPerson.to spliceKey)
  "username"    ## (^.timeLogWholeTask.taskWholePerson.eVal.personName)
  "day"         ## (^.timeLogWholeLog.eVal.timeLogDay.to spliceDay)
  "minutes"     ## (^.timeLogWholeLog.eVal.timeLogMinutes.to show.from unpacked)
  "timeAgainst" ## (^.timeLogWholeTask.taskWholeLink._Just.taskLinkName )
  "notes"       ## (^.timeLogWholeLog.eVal.timeLogDesc)
  "id"          ## (^.timeLogWholeLog.eKey.to spliceKey )

joinParams :: [(Text,Text)] -> Text
joinParams = (T.intercalate "&" . map (\ (n,v) -> n <> "=" <> v))

timeLogQueryLinkSplices
  :: Splices (PhbRuntimeSplice (Text, [(Text, Text)]) -> PhbSplice)
timeLogQueryLinkSplices = mapV (C.pureSplice . C.textSplice) $ do
  "title" ## fst
  "href"  ## joinParams . snd

possibleOwnerSplices :: Splices (PhbRuntimeSplice (Entity Person) -> PhbSplice)
possibleOwnerSplices = mapV (C.pureSplice . C.textSplice) $ do
  "userId"    ## (^.eKey.to keyToText)
  "userName"  ## (^.eVal.personName)

timeLogsSplices
  :: PhbRuntimeSplice
     ( Maybe Period
     , [Entity Person]
     , Maybe (Int64,Int64)
     , [Entity Person]
     , [TimeLogWhole]
     )
  -> PhbSplice
timeLogsSplices = C.withSplices C.runChildren $ do
  "currentUrl" ## (C.pureSplice . C.textSplice) currentUrl
  "ifQueries"  ## showIfTrue . fmap hasQueries
  "summary"    ## (timeSummaryDataSplices . fmap (^._5.to summariseTimeLogs))
  "timeLogQueryLinks" ##
    C.manyWithSplices C.runChildren timeLogQueryLinkSplices . fmap calculateLinks
  "timeLogRow"        ##
    C.manyWithSplices C.runChildren timeLogSplices . fmap (view _5)
  "possibleOwners" ##
    C.manyWithSplices C.runChildren possibleOwnerSplices . fmap (view _4)

  where
    currentUrl (p,us,_,_,_) = ("/time_logs?" <>) . joinParams . fmap snd $ allQueryParts p us
    hasQueries (p,us,_,_,_) = not . null $ allQueryParts p us

    calculateLinks (p,us,_,_,_)   =
      fmap mkQueryLink
      . multiply [] []
      $ allQueryParts p us

    allQueryParts :: (Maybe Period) -> [Entity Person] -> [(Text,(Text,Text))]
    allQueryParts p us =
      (toList $ fmap periodQuery p) ++ (fmap userQuery us)

    periodQuery x = ("Period: "<> showPeriod x,("period",showPeriod x))
    userQuery x   = ("User: "<> (x^.eVal.personName),("user",x^.eKey.to keyToText))
    showPeriod (ForMonth m) = T.pack . show $ m
    showPeriod (ForWeek  w) = T.pack . show $ w
    showPeriod (ForDay   d) = T.pack . show $ d
    multiply :: [NonEmpty (Text,(Text,Text))] -> [(Text,(Text,Text))] -> [(Text,(Text,Text))] -> [NonEmpty (Text,(Text,Text))]
    multiply out _   []     = out
    multiply out ws (x:xs) = multiply ((x:|(ws++xs)):out) (x:ws) xs
    mkQueryLink qs = (fst . NEL.head $ qs,fmap snd . NEL.tail $ qs)

listTimeLogsSplices :: PhbSplice
listTimeLogsSplices = do
  timeLogsSplices . lift $ do
    pp  <- periodParams "period" <&> lastMay
    ups <- userParams "user"
    pgs <- mfilter (const $ isNothing pp) . Just <$> defPaginationParam
    runPersist $ do
      twL <- queryTimeLogs pp ups pgs
      us  <- traverse getEntity ups <&> catMaybes
      tlp <- timeLoggablePeople
      pure (pp,us,pgs,tlp,twL)

allTimeLogSplices :: Splices PhbSplice
allTimeLogSplices = do
  "listTimeLogs"   ## listTimeLogsSplices
  "createTimeLog"  ## createTimeLogSplices
  "editTimeLog"    ## editTimeLogSplices
