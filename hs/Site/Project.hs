{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Site.Project where

import           BasePrelude                   hiding (bool, index, insert,
                                                phase)
import           Prelude                       ()

import           Control.Lens
import           Control.Monad.Trans           (lift, liftIO)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lens                (unpacked)
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
import           Text.Printf                   (printf)

import           Phb.Db
import qualified Phb.Types.Project             as T
import           Phb.Util
import           Site.Internal

projectRoutes :: PhbRoutes
projectRoutes =
  [("/projects"       ,ifTop . userOrIndex . render $ "projects/all")
  ,("/projects/create",ifTop . userOrIndex . render $ "projects/create")
  ,("/projects/:id/edit",ifTop . userOrIndex . render $ "projects/edit")
  ]



-- TODO: We should be able to clean this up by using monadic to load
-- up the select list options.
projectForm :: Maybe (T.Project) -> [(Key Customer,Text)] -> [(Key Person,Text)] -> PhbForm T.Text ProjectInput
projectForm e cs ps = ProjectInput
  <$> "name"         .: check nameErrMsg isNotEmpty (text name)
  <*> "statusPhase"  .: check phaseErrMsg isNotEmpty (text phase)
  <*> "statusColour" .: choice colourOpts statusC
  <*> "statusDesc"   .: optionalText statusD
  <*> "started"      .: html5DateFormlet started
  <*> "finished"     .: html5OptDateFormlet finished
  <*> "priority"     .: stringRead "Priority must be an int" priority
  <*> "notes"        .: text notes
  <*> "targets"      .: listOf targetFormlet targets
  <*> "customers"    .: listOf (choice cs) customers
  <*> "stakeholders" .: listOf (choice ps) stakeholders
  where
    latestS  = e ^?_Just.T.projectStatusLatest
    latestN  = e ^?_Just.T.projectNoteLatest.eVal
    name     = e ^?_Just.T.projectName
    priority = e ^?_Just.T.projectPriority
    phase    = latestS ^?_Just.T.projectStatusPhase
    statusD  = latestS ^?_Just.T.projectStatusDesc._Just
    statusC  = latestS ^?_Just.T.projectStatusColour
    started  = e ^?_Just.T.projectStarted
    finished = e ^?_Just.T.projectFinished._Just
    notes = latestN ^?_Just.projectNoteNote
    targets = e ^?_Just.T.projectTargetDates
    customers = e ^?_Just.T.projectCustomers.to (fmap entityKey)
    stakeholders = e ^?_Just.T.projectStakeholders.to (fmap entityKey)
    nameErrMsg = "Name must not be empty"
    phaseErrMsg = "Phase must not be empty"
    colourOpts = fmap (id &&& projectStatusColourHuman) [minBound..maxBound]

targetFormlet :: PhbFormlet Text T.TargetDate
targetFormlet td = T.TargetDate
  <$> "date"     .: html5DateFormlet day
  <*> "desc"     .: check descErrMsg isNotEmpty (text desc)
  <*> "handwavy" .: optionalText handwavy
  where
    day = td ^? _Just . T.targetDateDay
    desc = td ^? _Just . T.targetDateDesc
    handwavy = td ^? _Just . T.targetDateHandwavy . _Just
    descErrMsg = "Target Description cannot be empty"

projectFormSplices :: PhbRuntimeSplice (Maybe T.Project) -> PhbSplice
projectFormSplices rts = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("projectForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    e  <- rts
    cd <- liftIO getCurrentTime
    (cs,ps) <- lift . runPersist $ do
      cs' <- (fmap customerChoiceOption <$> selectList [] [Asc CustomerName])
      ps' <- (fmap personChoiceOption <$> selectList [] [Asc PersonName])
      pure (cs',ps')
    (v, result) <- lift $ runForm "project" (projectForm e cs ps)

    case result of
      Just x  -> do
        lift (createProject x cd (e ^? _Just . T.projectKey))
      Nothing -> C.putPromise promise v >> C.codeGen out
  where
    createProject x cd kMay = do
      void . runPersist $ upsertProjectInput x cd kMay
      case kMay of
        Nothing -> flashSuccess "Project Created"
        Just _  -> flashSuccess "Project Updated"
      redirect "/projects"
    customerChoiceOption (Entity k v) = (k,v ^. customerName)
    personChoiceOption (Entity k v) = (k,v ^. personName)

projectRowSplice :: PhbRuntimeSplice [T.Project] -> PhbSplice
projectRowSplice =
  C.withSplices C.runChildren ("projectRow" ## rowSplice (ts <> ss))
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "name"          ## view T.projectName
      "statusClass"   ## (^.T.projectStatusLatest.T.projectStatusColour.to statusClass)
      "duration"      ## (^.T.projectEffortDays.to showText)
      "id"            ## (^.T.projectKey.to spliceKey)
    ss = do
      "status"       ## spliceLines . fmap (^.T.projectStatusLatest.to statusLines)
      "customers"    ## spliceLines . fmap (^..T.projectCustomers.traverse.eVal.customerName)
      "stakeholders" ## spliceLines . fmap (^..T.projectStakeholders.traverse.eVal.personName)
      "notes"        ## spliceLines . fmap (^.T.projectNoteLatest.eVal.projectNoteNote.to T.lines)
      "effort"       ## C.withSplices C.runChildren effortSplices
      "targets"      ## C.withSplices C.runChildren targetsSplice . fmap (^. T.projectTargetDates)
    statusClass StatusGreen = "status-green"
    statusClass StatusAmber = "status-amber"
    statusClass StatusRed   = "status-red"
    statusClass StatusGray  = "status-grey"
    statusLines ps =
      (ps ^. T.projectStatusPhase : ps ^. T.projectStatusDesc . to toList )
    effortSplices = mapV (C.pureSplice . C.textSplice) $ do
      "effortDays"  ## (^. T.projectEffortDays . to (printf "%.3f") . from unpacked)
      "effortStart" ## (^. T.projectStarted . to spliceDay)
    targetsSplice = "target" ## C.manyWithSplices C.runChildren targetSplice
    targetSplice = mapV (C.pureSplice . C.textSplice) $ do
      "desc"          ## (^. T.targetDateDesc)
      "dayOrHandwavy" ## dayOrHandwavy

    dayOrHandwavy td = fromMaybe
      (td ^. T.targetDateDay . to show . from unpacked)
      (td ^. T.targetDateHandwavy)

listProjectsSplices :: PhbSplice
listProjectsSplices = C.withSplices C.runChildren splices rts
  where
    splices = do
      "activeProjects"    ## projectRowSplice . fmap (sortByPriority . fst)
      "completedProjects" ## projectRowSplice . fmap snd
    rts = lift $ do
      ct <- liftIO $ getCurrentTime
      cd <- liftIO $ localDayFromUTC ct
      runPersist $ do
        ps <- selectList [] [Desc ProjectId]
        psW <- traverse (loadProject ct) $ ps
        pure (partition (isActive cd) $ psW)

    sortByPriority = sortBy (compare `on` (view T.projectPriority))
    isActive ct = maybe True (>= ct) . (^.T.projectFinished)

createProjectSplices :: PhbSplice
createProjectSplices = projectFormSplices (pure Nothing)

editProjectSplices :: PhbSplice
editProjectSplices = projectFormSplices . lift $ do
  ct <- liftIO getCurrentTime
  e  <- requireEntity "project" "id"
  Just <$> runPersist (loadProject ct e)

allProjectSplices :: Splices PhbSplice
allProjectSplices = do
  "allProjects"   ## listProjectsSplices
  "createProject" ## createProjectSplices
  "editProject"   ## editProjectSplices
