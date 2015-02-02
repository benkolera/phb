{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Site.Task where

import BasePrelude hiding (insert)
import Prelude     ()

import           Control.Lens                  hiding (Action)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans           (lift)
import qualified Data.Map                      as M
import           Data.Map.Syntax
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (Day, UTCTime, getCurrentTime)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (ifTop, redirect, with)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent       (runPersist)
import           Text.Digestive                as DF
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import Phb.Auth       (userDbKey)
import Phb.Dates
import Phb.Db
import Phb.Types.Task
import Phb.Util
import Site.Internal

taskRoutes :: PhbRoutes
taskRoutes =
  [ ("/tasks"               , ifTop . userOrIndex $ render "tasks/all")
  , ("/tasks/create"        , ifTop . userOrIndex $ render "tasks/create" )
  , ("/tasks/:id/edit"      , ifTop . userOrIndex $ render "tasks/edit" )
  ]

data TaskInput = TaskInput
  { _taskInputPerson :: (Key Person)
  , _taskInputName   :: T.Text
  , _taskInputStart  :: Day
  , _taskInputFinish :: Maybe Day
  , _taskInputLink   :: LinkKey
  } deriving Show
makeLenses ''TaskInput

userErrMsg,timeErrMsg :: T.Text
userErrMsg = "Username cannot be empty"
timeErrMsg = "Time spent must be greater than 0"

personChoiceOption :: Entity Person -> (Key Person,T.Text)
personChoiceOption =
  entityKey &&& (^. to entityVal . personName)

groupLinkKeys :: [(LinkKey,Text)] -> [(Text,[(LinkKey,Text)])]
groupLinkKeys =
  fmap (first linkTypeName)
  . sortBy (on compare fst)
  . M.toList
  . foldl step M.empty
  where
    step acc t@(k,_) = M.insertWith (<>) k [t] acc

taskForm :: Maybe TaskWhole -> PhbForm T.Text TaskInput
taskForm tl = monadic $ do
  ct <- liftIO getCurrentTime
  cd <- liftIO $ localDayFromUTC ct
  (lk,pp) <- runPersist $ taskFormOptions ct
  p  <- (userDbKey =<<) <$> with auth currentUser
  return $ TaskInput
    <$> "person"   .: choice pp (username <|> p)
    <*> "name"     .: text name
    <*> "start"    .: html5DateFormlet (start <|> Just cd)
    <*> "finish"   .: html5OptDateFormlet finish
    <*> "link"     .: groupedChoice (groupLinkKeys lk) link
  where
    username = (tl ^?_Just.taskWholeTask.eVal.taskPerson )
    start    = (tl ^?_Just.taskWholeTask.eVal.taskStart )
    finish   = (tl ^?_Just.taskWholeTask.eVal.taskFinish._Just )
    name     = (tl ^?_Just.taskWholeTask.eVal.taskName )
    link     = (tl ^?_Just.taskWholeLink._Just.taskLinkKey )

taskFormOptions
  :: (MonadIO m, Applicative m)
  => UTCTime
  -> Db m ([(LinkKey, Text)], [(Key Person, Text)])
taskFormOptions ct = (,)
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

formSplices :: PhbRuntimeSplice (Maybe TaskWhole) -> PhbSplice
formSplices rts = do
  promise <- C.newEmptyPromise
  outputChildren <- C.withSplices C.runChildren splice (C.getPromise promise)
  pure . C.yieldRuntime $ do
    tlw <- rts
    (v, result) <- lift $ runForm "task" (taskForm tlw)
    case result of
     Just x  -> lift (upsertTask x (tlw ^?_Just.taskWholeTask.eKey))
     Nothing -> C.putPromise promise v >> C.codeGen outputChildren
  where
    splice = do
      "taskForm" ## formSplice mempty mempty

    upsertTask x kMay = do
      case kMay of
        Nothing -> do
          void . runPersist $ insert (newTask x)
          flashSuccess $ "Timelog Created"
        Just k -> do
          void . runPersist $ replace k (newTask x)
          flashSuccess $ "Timelog Updated"
      redirect "/tasks?user=me"

    newTask (TaskInput p n s f l) =
      Task p n s f
        (l ^? _ProjectLink)
        (l ^? _EventLink)
        (l ^? _BacklogLink)
        (l ^? _ActionLink)
        (l ^? _WorkCategoryLink)

editTaskSplices :: PhbSplice
editTaskSplices =
  formSplices . lift $
    requireEntity "task" "id" >>= fmap Just . runPersist . loadTaskWhole

createTaskSplices :: PhbSplice
createTaskSplices = formSplices (pure Nothing)

taskSplices :: Splices (PhbRuntimeSplice TaskWhole -> PhbSplice)
taskSplices = mapV (C.pureSplice . C.textSplice) $ do
  "person"      ## (^.taskWholePerson.eVal.personName)
  "started"     ## (^.taskWholeTask.eVal.taskStart.to spliceDay)
  "finished"    ## (^.taskWholeTask.eVal.taskFinish._Just.to spliceDay)
  "link"        ## (^.taskWholeLink._Just.taskLinkName )
  "name"        ## (^.taskWholeTask.eVal.taskName)
  "id"          ## (^.taskWholeTask.eKey.to spliceKey )

joinParams :: [(Text,Text)] -> Text
joinParams = (T.intercalate "&" . map (\ (n,v) -> n <> "=" <> v))

taskQueryLinkSplices
  :: Splices (PhbRuntimeSplice (Text, [(Text, Text)]) -> PhbSplice)
taskQueryLinkSplices = mapV (C.pureSplice . C.textSplice) $ do
  "title" ## fst
  "href"  ## joinParams . snd

possibleOwnerSplices :: Splices (PhbRuntimeSplice (Entity Person) -> PhbSplice)
possibleOwnerSplices = mapV (C.pureSplice . C.textSplice) $ do
  "userId"    ## (^.eKey.to keyToText)
  "userName"  ## (^.eVal.personName)

tasksSplices
  :: PhbRuntimeSplice [TaskWhole]
  -> PhbSplice
tasksSplices = C.withSplices C.runChildren $ do
  "taskRow"        ## C.manyWithSplices C.runChildren taskSplices

listTasksSplices :: PhbSplice
listTasksSplices = do
  tasksSplices . lift $ do
    cd  <- liftIO getCurrentDay
    pks <- userParams "user"
    runPersist $ loadTasksForPeopleForDay pks cd

allTaskSplices :: Splices PhbSplice
allTaskSplices = do
  "listTasks"   ## listTasksSplices
  "createTask"  ## createTaskSplices
  "editTask"    ## editTaskSplices
