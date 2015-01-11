{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Site.Heartbeat where

import BasePrelude hiding (insert)
import Prelude     ()

import           Blaze.ByteString.Builder.ByteString     (fromByteString)
import           Blaze.ByteString.Builder.Internal.Types (Builder)
import           Control.Error
import           Control.Lens                            hiding (Action)
import qualified Data.ByteString.Char8                   as B
import           Data.Map.Syntax
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Data.Text.Encoding                      (encodeUtf8)
import           Data.Text.Lens                          (unpacked)
import           Data.Time                               (UTCTime, addDays,
                                                          getCurrentTime,
                                                          utctDay)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                          as C
import qualified Heist.Compiled.LowLevel                 as C
import           Snap                                    hiding (get)
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent                 (runPersist)
import           Text.Digestive                          hiding (Success)
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap
import           Text.Printf                             (printf)
import           Text.XmlHtml                            (Node (..))

import           Phb.Db
import qualified Phb.Types     as T
import           Site.Backlog  (backlogRowSplice)
import           Site.Event    (eventRowSplice)
import           Site.Internal
import           Site.Project  (projectRowSplice)

heartbeatRoutes :: PhbRoutes
heartbeatRoutes =
  [ ("heartbeats",ifTop . userOrIndex . render $ "heartbeats/all")
  , ("/heartbeats/create",ifTop . userOrIndex . render $ "heartbeats/create")
  , ("/heartbeats/:id/edit",ifTop . userOrIndex . render $ "heartbeats/edit")
  , ("/heartbeats/:id",ifTop . render $ "heartbeats/view")
  ]

-- TODO: Taking a T.Heartbeat instead of a HeartbeatInput is kinda off
heartbeatForm :: UTCTime -> Maybe (T.Heartbeat) -> PhbForm Text HeartbeatInput
heartbeatForm ct e = monadic $ do
  (as,bs,es,prjs,ss,lhb) <- runPersist $ (,,,,,)
    <$> (choiceOpts actionName  <$> loadActiveActions ct)
    <*> (choiceOpts backlogName <$> loadActiveBacklog ct)
    <*> (choiceOpts eventName   <$> loadActiveEvents ct)
    <*> (choiceOpts projectName <$> loadActiveProjects ct)
    <*> (traverse (traverse successInput) successes)
    <*> lastHeartbeat (utctDay ct)
  return $ HeartbeatInput
    <$> "start"      .: html5DateFormlet (start <|> (lhbStart lhb))
    <*> "finish"     .: html5DateFormlet (finish <|> Just (utctDay ct))
    <*> "upcoming"   .: listOf (neText upcomingErrMsg) upcoming
    <*> "highlights" .: listOf (neText highlightErrMsg) highlights
    <*> "successes"  .: listOf successForm ss
    <*> "actions"    .: defListOf as actions
    <*> "backlog"    .: defListOf bs backlog
    <*> "events"     .: defListOf es events
    <*> "projects"   .: defListOf prjs projects
  where
    defListOf xs curr = listOf (choice xs) (curr <|> Just (fmap fst xs))
    lhbStart   = (^?_Just.eVal.heartbeatFinish.to (addDays 1))
    start      = e^?_Just.T.heartbeatStart
    finish     = e^?_Just.T.heartbeatFinish
    upcoming   = e^?_Just.T.heartbeatUpcoming
    highlights = e^?_Just.T.heartbeatHighlights
    successes  = e^?_Just.T.heartbeatSuccesses
    actions    = e^?_Just.T.heartbeatActions.to (fmap $ view T.actionKey)
    backlog    = e^?_Just.T.heartbeatBacklog.to (fmap $ view T.backlogKey)
    events     = e^?_Just.T.heartbeatEvents.to (fmap $ view T.eventKey)
    projects   = e^?_Just.T.heartbeatProjects.to (fmap $ view T.projectKey)
    upcomingErrMsg = "Upcoming text line cannot be empty"
    highlightErrMsg = "Highlight text line cannot be empty"
    successInput s = do
      ps <- loadRelatedPeople SuccessPersonPerson SuccessPersonSuccess (s^.T.successKey)
      return $ HeartbeatSuccessInput
        (s^.T.successWhat)
        (s^.T.successAchievements)
        (fmap entityKey ps)


successForm :: Maybe HeartbeatSuccessInput -> PhbForm Text HeartbeatSuccessInput
successForm e = monadic $ do
  ps <- runPersist $ choiceOpts personName <$> selectList [] [Asc PersonName]
  return $ HeartbeatSuccessInput
    <$> "what"         .: check whatErrMsg isNotEmpty (text what)
    <*> "achievements" .: listOf achievementForm achievements
    <*> "people"       .: listOf (choice ps) people
  where
    what = e^?_Just.heartbeatSuccessInputWhat
    achievements = e^?_Just.heartbeatSuccessInputAchievements
    people = e^?_Just.heartbeatSuccessInputPeople
    achievementForm a = check achievementsErrMsg isNotEmpty (text a)
    whatErrMsg = "What cannot be blank"
    achievementsErrMsg = "Achievement text cannot be empty"



heartbeatFormSplices :: PhbRuntimeSplice (Maybe T.Heartbeat) -> PhbSplice
heartbeatFormSplices rts = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("heartbeatForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    e  <- rts
    ct <- liftIO getCurrentTime
    (v, result) <- lift $ runForm "heartbeat" (heartbeatForm ct e)

    case result of
      Just x  -> lift $ do
        let kMay = e^?_Just.T.heartbeatKey
        void . runPersist $ upsertHeartbeat x kMay
        case kMay of
          Nothing -> flashSuccess "Timelog Created"
          Just _  -> flashSuccess $ "Timelog Updated"
        redirect "/heartbeats"
      Nothing -> C.putPromise promise v >> C.codeGen out

heartbeatSplices :: Splices (PhbRuntimeSplice (Entity Heartbeat) -> PhbSplice)
heartbeatSplices = ts <> ss
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "id"         ## (^. to (show . fromSqlKey . entityKey) . from unpacked)
      "start"      ## (^. to entityVal . heartbeatStart . to spliceDay)
      "finish"     ## (^. to entityVal . heartbeatFinish . to spliceDay)
    ss = do
      "highlights" ## spliceUl . fmap (^. to entityVal . heartbeatHighlights . to Text.lines)

heartbeatsSplices :: PhbRuntimeSplice [Entity Heartbeat] -> PhbSplice
heartbeatsSplices = C.manyWithSplices C.runChildren heartbeatSplices

listHeartbeatSplices :: PhbSplice
listHeartbeatSplices =
  heartbeatsSplices . lift . runPersist $ (selectList [] [])

getHeartbeat :: PhbHandler T.Heartbeat
getHeartbeat = do
  k <- requireKey "id"
  hbMay <- runPersist . runMaybeT $ do
    hb <- MaybeT $ get k
    lift . loadHeartbeat $ Entity k hb

  requireOr404 "heartbeat" (B.pack . show $ k) hbMay

latestHeartbeat :: PhbHandler (Maybe T.Heartbeat)
latestHeartbeat = runPersist $ do
  hbs <- headMay <$> (selectList [] [Desc HeartbeatFinish])
  traverse loadHeartbeat hbs

actionRowSplice :: PhbRuntimeSplice [T.Action] -> PhbSplice
actionRowSplice = rowSplice (ts <> ss)
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "status" ## (^.T.actionStatusLatest.eVal.actionStatusStatus)
      "due"    ## (^. T.actionDue.to spliceDay)
    ss = do
      "who"       ## spliceLines . fmap (^..T.actionPeople.traverse.eVal.personName)
      "customers" ## spliceLines . fmap (^..T.actionCustomers.traverse.eVal.customerName)
      "notes"     ## spliceLines . fmap (^. T.actionNoteLatest.eVal.actionNoteNote.to Text.lines)

timeLogDataSplice :: PhbRuntimeSplice [T.HeartbeatTimeLog] -> PhbSplice
timeLogDataSplice rts = do
  so <- pure . C.yieldPure $ fromByteString "<script>\nvar timeLogData=["
  js <- return $ C.yieldRuntime $ do
    s <- rts
    return
      . fromByteString
      . B.intercalate ","
      . fmap timeLogWholeJson
      . filter ((>= 0.5) . view T.heartbeatTimeLogHours)$ s
  sc <- pure . C.yieldPure $ fromByteString "];\n</script>"
  return . fold $ [so,js,sc]
  where
    timeLogWholeJson tlw = fold
      [ "{value: "
      , tlw^.T.heartbeatTimeLogHours.to (printf "%.3f").to B.pack
      , ", label: '"
      , tlw^.T.heartbeatTimeLogLabel.to encodeUtf8
      , "'}"
      ]

successRowSplice :: PhbRuntimeSplice [T.Success] -> PhbSplice
successRowSplice = rowSplice (ts <> ss)
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "what" ## view T.successWhat
    ss = do
      "who"         ## spliceLines . fmap (^..T.successWho.traverse.eVal.personName)
      "achievement" ## spliceLines . fmap (^.T.successAchievements)

spliceUl :: PhbRuntimeSplice [Text] -> PhbSplice
spliceUl rt = do
  a <- pure . C.yieldPure $ fromByteString "<ul>"
  b <- pure . C.yieldRuntime $ fmap mkNodes rt
  c <- pure . C.yieldPure $ fromByteString "</ul>"
  return . fold $ [a,b,c]
  where
    mkNodes :: [Text] -> Builder
    mkNodes = foldMap (C.nodeSplice mkUl)
    mkUl = pure . Element "li" [] . pure . TextNode

wholeHeartbeatSplices :: PhbRuntimeSplice T.Heartbeat -> PhbSplice
wholeHeartbeatSplices = C.withSplices C.runChildren (ss <> rs)
  where
    ss,rs :: Splices (RuntimeSplice PhbHandler T.Heartbeat -> C.Splice PhbHandler)
    ss = mapV (C.pureSplice . C.textSplice) $ do
      "start"           ## (^. T.heartbeatStart . to spliceDay)
      "finish"          ## (^. T.heartbeatFinish . to spliceDay)
      "nextHref"        ## (^. T.heartbeatNextKey._Just.to projectLink)
      "nextClass"       ## btnClass . (^. T.heartbeatNextKey)
      "prevHref"        ## (^. T.heartbeatPrevKey._Just.to projectLink)
      "prevClass"       ## btnClass . (^. T.heartbeatPrevKey)

    rs = do
      "highlights"     ## (spliceUl . (fmap (^. T.heartbeatHighlights)))
      "upcomingEvents" ## (spliceUl . (fmap (^. T.heartbeatUpcoming)))
      "actionRow"      ## (actionRowSplice . fmap (^. T.heartbeatActions))
      "successRow"     ## (successRowSplice . fmap (^. T.heartbeatSuccesses))
      "projectRow"     ## (projectRowSplice . fmap (^. T.heartbeatProjects))
      "backlogRow"     ## (backlogRowSplice . fmap (^. T.heartbeatBacklog))
      "eventRow"       ## (eventRowSplice . fmap (^. T.heartbeatEvents))
      "timeLogData"    ## (timeLogDataSplice . fmap (^. T.heartbeatTimeLogs))
    btnClass Nothing = "disabled"
    btnClass _       = ""
    projectLink k    = "/heartbeats/" <> spliceKey k

latestHeartbeatSplice :: PhbSplice
latestHeartbeatSplice = do
  promise <- C.newEmptyPromise
  outputChildren <- C.withSplices C.runChildren ("heartbeat" ## wholeHeartbeatSplices) (C.getPromise promise)
  return $ C.yieldRuntime $ do
    hbMay <- lift $ latestHeartbeat
    let e       = return $ fromByteString "No latest heartbeats! :("
        doHb hb = C.putPromise promise hb >> C.codeGen outputChildren
    maybe e doHb hbMay

createHeartbeatSplices :: PhbSplice
createHeartbeatSplices = heartbeatFormSplices (pure Nothing)

editHeartbeatSplices :: PhbSplice
editHeartbeatSplices = heartbeatFormSplices . lift $ do
  e  <- requireEntity "heartbeat" "id"
  Just <$> runPersist (loadHeartbeat e)

allHeartbeatSplices :: Splices PhbSplice
allHeartbeatSplices = do
  "allHeartbeats"    ## listHeartbeatSplices
  "heartbeat"        ## (wholeHeartbeatSplices $ lift getHeartbeat)
  "heartbeatLatest"  ## latestHeartbeatSplice
  "createHeartbeat"  ## createHeartbeatSplices
  "editHeartbeat"    ## editHeartbeatSplices
