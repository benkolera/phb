{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Site.TimeLog where

import           BasePrelude                   hiding (insert)
import           Prelude                       ()

import           Control.Lens                  hiding (Action)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader          (runReaderT)
import           Control.Monad.Trans           (lift)
import qualified Data.ByteString.Char8         as B
import qualified Data.List.NonEmpty            as NEL
import           Data.Map.Syntax
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lens                (unpacked)
import           Data.Time                     (Day, getCurrentTime)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (getParam, ifTop, redirect)
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent       (runPersist)
import           Text.Digestive
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import           Phb.Db
import           Phb.Mail
import           Phb.Types.TimeLog
import           Phb.Util
import           Site.Internal

timeLogRoutes :: PhbRoutes
timeLogRoutes =
  [ ("/time_logs"         , ifTop $ render "time_logs/all")
  , ("/time_logs/pester"  , handlePester)
  , ("/time_logs/create"  , ifTop $ render "time_logs/create" )
  , ("/time_logs/:id/edit", ifTop $ render "time_logs/edit" )
  ]

data TimeLogInput = TimeLogInput (Key Person) Day T.Text Int LinkKey deriving Show

userErrMsg,minuteErrMsg :: T.Text
userErrMsg = "Username cannot be empty"
minuteErrMsg = "Minutes must be a number"

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


timeLogForm :: Maybe (TimeLogWhole) -> [(LinkKey,Text)] -> [(Key Person,Text)] -> PhbForm T.Text TimeLogInput
timeLogForm tl lk pp = monadic $ do
  cd <- liftIO getCurrentDay
  p  <- getParam "personId" <&> (stringToKey . B.unpack =<<)
  return $ TimeLogInput
    <$> "username" .: choice pp (username <|> p)
    <*> "day"      .: html5DateFormlet (day <|> Just cd)
    <*> "desc"     .: text desc
    <*> "minutes"  .: stringRead minuteErrMsg minutes
    <*> "link"     .: groupedChoice (groupLinkKeys lk) link
  where
    username = (tl ^? _Just . timeLogWholeLog . eVal . timeLogPerson )
    day      = (tl ^? _Just . timeLogWholeLog . eVal . timeLogDay )
    desc     = (tl ^? _Just . timeLogWholeLog . eVal . timeLogDesc )
    minutes  = (tl ^? _Just . timeLogWholeLog . eVal . timeLogMinutes )
    link     = (tl ^? _Just . timeLogWholeLink . _Just . timeLogLinkKey )


timeLogFormSplices :: PhbRuntimeSplice (Maybe TimeLogWhole) -> PhbSplice
timeLogFormSplices rts = do
  promise <- C.newEmptyPromise
  outputChildren <- C.withSplices C.runChildren splice (C.getPromise promise)
  pure . C.yieldRuntime $ do
    tlw <- rts
    let preExisting = isJust tlw
    ct  <- liftIO getCurrentTime
    (links,people) <- lift . runPersist $ (,)
      <$> loadLinkOptions ct
      <*> (fmap personChoiceOption <$> selectList [PersonLogsTime ==. True] [Asc PersonName])
    (v, result) <- lift $ runForm "timeLog" (timeLogForm tlw links people)
    case result of
      Just x  -> lift (createTimeLog x ct (tlw ^? _Just . timeLogWholeLog . eKey))
      Nothing -> C.putPromise promise (v,preExisting) >> C.codeGen outputChildren
  where
    splice = do
      "timeLogForm" ## formSplice mempty mempty . fmap fst
      "action"      ## (C.pureSplice . C.textSplice $ actionText . snd)
      "ifCreate"    ## showIfTrue . fmap (not . snd)

    showIfTrue r = do
      action <- C.runChildren
      return $ C.yieldRuntime $ do
        b <- r
        if b
        then C.codeGen action
        else return mempty

    actionText True  = "Edit"
    actionText False = "Create"

    createTimeLog x ct kMay = do
      -- Should probably change this so that a DB error wouldn't just
      -- Crash us and should put a nice error in the form.
      case kMay of
       Nothing -> do
         a <- getParam "action"
         let continue = maybe False (== "Create & Continue") a
         void . runPersist $ insert (newTimeLog x ct)
         flashSuccess $ "Timelog Created"
         if   continue
         then redirect "/time_logs/create"
         else redirect "/time_logs"
       Just k  -> do
         void . runPersist $ replace k (newTimeLog x ct)
         flashSuccess $ "Timelog Updated"
         redirect "/time_logs"

    loadLinkOptions ct = do
      fmap fold . sequence $
        [ loadLinkOptions' ProjectLink projectName (loadActiveProjects ct)
        , loadLinkOptions' BacklogLink backlogName (loadActiveBacklog ct)
        , loadLinkOptions' EventLink eventName (loadActiveEvents ct)
        , loadLinkOptions' ActionLink actionName (loadActiveActions ct)
        , loadLinkOptions' WorkCategoryLink workCategoryName (loadActiveWorkCategories ct)
        ]
    loadLinkOptions' lc nl a = mkLinkOptions lc nl <$> a
    newTimeLog (TimeLogInput p dy d m l) _ =
      let (pId,eId,bId,aId,wcId) = linkIds l
      in TimeLog p d m dy pId eId bId aId wcId

    -- TODO: Surely this can be solved with prisms
    linkIds (ProjectLink p)      = (Just p,Nothing,Nothing,Nothing,Nothing)
    linkIds (EventLink e)        = (Nothing,Just e,Nothing,Nothing,Nothing)
    linkIds (BacklogLink b)      = (Nothing,Nothing,Just b,Nothing,Nothing)
    linkIds (ActionLink a)       = (Nothing,Nothing,Nothing,Just a,Nothing)
    linkIds (WorkCategoryLink w) = (Nothing,Nothing,Nothing,Nothing,Just w)

timeLogSplices :: Splices (PhbRuntimeSplice (TimeLogWhole) -> PhbSplice)
timeLogSplices = mapV (C.pureSplice . C.textSplice) $ do
  "username"    ## (^. timeLogWholePerson . personName)
  "day"         ## (^. timeLogWholeLog . eVal . timeLogDay . to spliceDay )
  "minutes"     ## (^. timeLogWholeLog . eVal . timeLogMinutes . to show . from unpacked)
  "timeAgainst" ## (^. timeLogWholeLink . _Just . timeLogLinkName )
  "notes"       ## (^. timeLogWholeLog . eVal . timeLogDesc)
  "id"          ## (^. timeLogWholeLog . eKey . to spliceKey )

timeLogsSplices :: PhbRuntimeSplice [TimeLogWhole] -> PhbSplice
timeLogsSplices = C.withSplices C.runChildren $ do
  "timeLogRow" ## C.manyWithSplices C.runChildren timeLogSplices

createTimeLogSplices :: PhbSplice
createTimeLogSplices = timeLogFormSplices (pure Nothing)

editTimeLogSplices :: PhbSplice
editTimeLogSplices = timeLogFormSplices . lift $ do
  tl  <- requireEntity "time_log" "id"
  tlw <- runPersist $ loadTimeLogWhole tl
  pure (Just tlw)

listTimeLogsSplices :: PhbSplice
listTimeLogsSplices = do
  timeLogsSplices . lift $ do
    runPersist $ do
      twes <- selectList [] [Desc TimeLogDay]
      twL <- traverse loadTimeLogWhole twes
      pure twL

allTimeLogSplices :: Splices PhbSplice
allTimeLogSplices = do
  "allTimeLogs"   ## listTimeLogsSplices
  "createTimeLog" ## createTimeLogSplices
  "editTimeLog"   ## editTimeLogSplices

handlePester :: PhbHandler ()
handlePester = do
  cd  <- liftIO getCurrentDay
  ps  <- runPersist $ missingTimeLogsFor cd
  view mail >>= liftIO . runReaderT (traverse_ timelogPesterEmail ps)
  flashSuccess "Pestered!"
  redirect "/time_logs"
