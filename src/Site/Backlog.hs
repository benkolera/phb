{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Site.Backlog where

import BasePrelude hiding (insert)
import Prelude     ()

import           Control.Lens
import           Control.Monad.Trans           (lift, liftIO)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (getCurrentTime)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (ifTop, redirect)
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent       (runPersist)
import           Text.Digestive
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import           Phb.Db
import qualified Phb.Types.Backlog as T
import           Site.Internal

backlogRoutes :: PhbRoutes
backlogRoutes =
  [("/backlog",ifTop $ render "backlog/all")
  ,("/backlog/create",ifTop $ render "backlog/create")
  ,("/backlog/:id/edit",ifTop $ render "backlog/edit")
  ]

data BacklogInput = BacklogInput
  { _backlogInputName         :: Text
  , _backlogInputStatus       :: BacklogStatusEnum
  , _backlogInputNote         :: Text
  , _backlogInputCustomers    :: [Key Customer]
  , _backlogInputStakeholders :: [Key Person]
  }
makeLenses ''BacklogInput

-- TODO: We should be able to clean this up by using monadic to load
-- up the select list options.
backlogForm :: Maybe (T.Backlog) -> [(Key Customer,Text)] -> [(Key Person,Text)] -> PhbForm T.Text BacklogInput
backlogForm e cs ps = BacklogInput
  <$> "name"         .: check nameErrMsg isNotEmpty (text name)
  <*> "status"       .: choice statusOpts status
  <*> "notes"        .: text notes
  <*> "customers"    .: listOf (choice cs) customers
  <*> "stakeholders" .: listOf (choice ps) stakeholders
  where
    name    = e ^?_Just.T.backlogName
    status  = e ^?_Just.T.backlogStatusLatest.eVal.backlogStatusStatus
    notes = e ^?_Just.T.backlogNoteLatest.eVal.backlogNoteNote
    customers = e ^?_Just.T.backlogCustomers.to (fmap entityKey)
    stakeholders = e ^?_Just.T.backlogStakeholders.to (fmap entityKey)
    nameErrMsg = "Name must not be empty"
    statusOpts = fmap (id &&& backlogStatusHuman) [minBound..maxBound]

backlogFormSplices :: PhbRuntimeSplice (Maybe T.Backlog) -> PhbSplice
backlogFormSplices rts = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("backlogForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    e  <- rts
    cd <- liftIO getCurrentTime
    (cs,ps) <- lift . runPersist $ do
      cs' <- (fmap customerChoiceOption <$> selectList [] [Asc CustomerName])
      ps' <- (fmap personChoiceOption <$> selectList [] [Asc PersonName])
      pure (cs',ps')
    (v, result) <- lift $ runForm "backlog" (backlogForm e cs ps)

    case result of
      Just x  -> do
        lift (createBacklog x cd (e ^? _Just . T.backlogKey))
      Nothing -> C.putPromise promise v >> C.codeGen out
  where
    createBacklog x cd kMay = do
      -- Should probably change this so that a DB error wouldn't just
      -- Crash us and should put a nice error in the form.
      case kMay of
       Nothing -> do
         void . runPersist $ do
           insert (newBacklog x) >>= updateSatellites x cd
         flashSuccess $ "Backlog Created"
       Just k  -> do
         void . runPersist $ do
           replace k (newBacklog x) >> updateSatellites x cd k
         flashSuccess $ "Backlog Updated"
      redirect "/backlog"
    newBacklog (BacklogInput n _ _ _ _) = Backlog n
    updateSatellites x cd k = do
      updateSatellite k (x ^. backlogInputCustomers)
        BacklogCustomerBacklog
        BacklogCustomerId
        BacklogCustomerCustomer
        backlogCustomerCustomer
        BacklogCustomer
      updateSatellite k (x ^. backlogInputStakeholders)
        BacklogPersonBacklog
        BacklogPersonId
        BacklogPersonPerson
        backlogPersonPerson
        BacklogPerson

      insertTemporal k
        BacklogStatusStart
        BacklogStatusFinish
        BacklogStatusId
        BacklogStatusBacklog
        (view backlogStatusStatus)
        cd
        (BacklogStatus k cd Nothing (x^.backlogInputStatus))

      insertTemporal k
        BacklogNoteStart
        BacklogNoteFinish
        BacklogNoteId
        BacklogNoteBacklog
        (view backlogNoteNote)
        cd
        (BacklogNote k (x^.backlogInputNote) cd Nothing)

    customerChoiceOption (Entity k v) = (k,v ^. customerName)
    personChoiceOption (Entity k v) = (k,v ^. personName)

backlogRowSplice :: PhbRuntimeSplice [T.Backlog] -> PhbSplice
backlogRowSplice = rowSplice (ts <> ss)
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "name"          ## view T.backlogName
      "statusClass"   ## (^.T.backlogStatusLatest.eVal.backlogStatusStatus.to statusClass)
      "id"            ## (^.T.backlogKey.to spliceKey)
      "status"        ## (^.T.backlogStatusLatest.eVal.backlogStatusStatus.to backlogStatusHuman )
    ss = do
      "customers"    ## spliceLines . fmap (^..T.backlogCustomers.traverse.eVal.customerName)
      "stakeholders" ## spliceLines . fmap (^..T.backlogStakeholders.traverse.eVal.personName)
      "notes"        ## spliceLines . fmap (^.T.backlogNoteLatest.eVal.backlogNoteNote.to T.lines)

    statusClass BacklogScoped = "status-green"
    statusClass BacklogNeedsScoping = "status-amber"
    statusClass BacklogInCommercials   = "status-blue"
    statusClass BacklogProjectStarted = "status-gray"
    statusClass BacklogRejected = "status-gray"

listBacklogsSplices :: PhbSplice
listBacklogsSplices =
  C.withSplices
    C.runChildren
    ("backlogRow" ## backlogRowSplice)
    . lift $ do
  ct <- liftIO $ getCurrentTime
  runPersist $ do
    es <- selectList [] [Desc BacklogId]
    traverse (loadBacklog ct) $ traceShowId es

createBacklogSplices :: PhbSplice
createBacklogSplices = backlogFormSplices (pure Nothing)

editBacklogSplices :: PhbSplice
editBacklogSplices = backlogFormSplices . lift $ do
  ct <- liftIO getCurrentTime
  e  <- requireEntity "backlog" "id"
  Just <$> runPersist (loadBacklog ct e)

allBacklogSplices :: Splices PhbSplice
allBacklogSplices = do
  "allBacklog"   ## listBacklogsSplices
  "createBacklog" ## createBacklogSplices
  "editBacklog"   ## editBacklogSplices
