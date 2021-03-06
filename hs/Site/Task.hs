{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Site.Task where

import           BasePrelude                   hiding (insert, (<>))
import           Prelude                       ()

import           Control.Lens                  hiding (Action)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans           (lift)
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as NEL
import qualified Data.Map                      as M
import           Data.Map.Syntax
import           Data.Semigroup                ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (Day, UTCTime, getCurrentTime)
import qualified Database.Esqueleto            as E
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (ifTop, redirect, with)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent       (runPersist)
import           Text.Digestive                hiding (bool)
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import           Phb.Auth                      (userDbKey)
import           Phb.Dates
import           Phb.Db
import           Phb.Types.Task
import           Phb.Util
import           Site.Internal

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
  fmap (second toList)
  . sortBy (on compare (fst . NEL.head . snd))
  . M.toList
  . foldl step M.empty
  where
    step acc t@(k,_) = M.insertWith (<>) (linkTypeName k) (t:|[]) acc

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
    let action = bool "Create" "Update" . isJust $ tlw
    case result of
     Just x  -> lift (upsertTask x (tlw ^?_Just.taskWholeTask.eKey))
     Nothing -> C.putPromise promise (v,action) >> C.codeGen outputChildren
  where
    splice = do
      "taskForm" ## formSplice mempty mempty . fmap fst
      "action"   ## C.pureSplice (C.textSplice snd)

    upsertTask x kMay = do
      case kMay of
        Nothing -> do
          void . runPersist $ insert (newTask x)
          flashSuccess $ "Task Created"
        Just k -> do
          void . runPersist $ replace k (newTask x)
          flashSuccess $ "Task Updated"
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
  "start"       ## (^.taskWholeTask.eVal.taskStart.to spliceDay)
  "finish"      ## (^.taskWholeTask.eVal.taskFinish._Just.to spliceDay)
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

tasksSplices :: PhbRuntimeSplice [TaskWhole] -> PhbSplice
tasksSplices = C.withSplices C.runChildren $ do
  "taskRow"        ## C.manyWithSplices C.runChildren taskSplices

listTasksSplices :: PhbSplice
listTasksSplices = C.withSplices C.runChildren splices $ lift $ do
    cd  <- liftIO getCurrentDay
    pks <- userParams "user"
    let q = if null pks
            then (const $ E.val True)
            else (\t -> t E.^. TaskPerson `E.in_` E.valList pks)
    runPersist $ (,)
      <$> loadActiveTasksForDay q cd
      <*> loadCompletedTasksForDay q cd
  where
    splices :: Splices (PhbRuntimeSplice ([TaskWhole],[TaskWhole]) -> PhbSplice)
    splices = do
      "activeTasks"    ## tasksSplices . fmap fst
      "completedTasks" ## tasksSplices . fmap snd

allTaskSplices :: Splices PhbSplice
allTaskSplices = do
  "allTasks"   ## listTasksSplices
  "createTask"  ## createTaskSplices
  "editTask"    ## editTaskSplices
