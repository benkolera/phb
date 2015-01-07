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
import           Data.Time                     (Day, getCurrentTime)
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
import qualified Phb.Types.Project             as T
import           Site.Internal

projectRoutes :: PhbRoutes
projectRoutes =
  [("/projects"       ,ifTop $ render "projects/all")
  ,("/projects/create",ifTop $ render "projects/create")
  ,("/projects/:id/edit",ifTop $ render "projects/edit")
  ]


data ProjectInput = ProjectInput
  { _projectInputName         :: Text
  , _projectInputStatusPhase  :: Text
  , _projectInputStatusColor  :: StatusColourEnum
  , _projectInputStatusDesc   :: Maybe Text
  , _projectInputStarted      :: Day
  , _projectInputFinished     :: Maybe Day
  , _projectInputNote         :: Text
  , _projectTargets           :: [T.TargetDate]
  , _projectInputCustomers    :: [Key Customer]
  , _projectInputStakeholders :: [Key Person]
  }
makeLenses ''ProjectInput

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
  <*> "notes"        .: text notes
  <*> "targets"      .: listOf targetFormlet targets
  <*> "customers"    .: listOf (choice cs) customers
  <*> "stakeholders" .: listOf (choice ps) stakeholders
  where
    latestS = e ^?_Just.T.projectStatusLatest
    latestN = e ^?_Just.T.projectNoteLatest.eVal
    name    = e ^?_Just.T.projectName
    phase   = latestS ^?_Just.T.projectStatusPhase
    statusD = latestS ^?_Just.T.projectStatusDesc._Just
    statusC = latestS ^?_Just.T.projectStatusColour
    started = e ^?_Just.T.projectStarted
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
      -- Should probably change this so that a DB error wouldn't just
      -- Crash us and should put a nice error in the form.
      case kMay of
       Nothing -> do
         void . runPersist $ do
           insert (newProject x) >>= updateSatellites x cd
         flashSuccess $ "Project Created"
       Just k  -> do
         void . runPersist $ do
           replace k (newProject x) >> updateSatellites x cd k
         flashSuccess $ "Project Updated"
      redirect "/projects"
    newProject (ProjectInput n _ _ _ s f _ _ _ _) =
      Project n s f
    updateSatellites x cd k = do
      updateSatellite k (x ^. projectInputCustomers)
        ProjectCustomerProject
        ProjectCustomerId
        ProjectCustomerCustomer
        projectCustomerCustomer
        ProjectCustomer
      updateSatellite k (x ^. projectInputStakeholders)
        ProjectPersonProject
        ProjectPersonId
        ProjectPersonPerson
        projectPersonPerson
        ProjectPerson

      deleteWhere [ProjectTargetDateProject ==. k]
      traverse_ (insert . inputTargetToDb k) (x ^. projectTargets)

      insertTemporal k
        ProjectStatusStart
        ProjectStatusFinish
        ProjectStatusId
        ProjectStatusProject
        fuzzyStatus
        cd $
          ProjectStatus k cd Nothing
            (x ^. projectInputStatusPhase)
            (x ^. projectInputStatusColor)
            (x ^. projectInputStatusDesc)

      insertTemporal k
        ProjectNoteStart
        ProjectNoteFinish
        ProjectNoteId
        ProjectNoteProject
        (view projectNoteNote)
        cd
        (ProjectNote k (x ^. projectInputNote) cd Nothing)

    fuzzyStatus = (,,)
      <$> view projectStatusPhase
      <*> view projectStatusColour
      <*> view projectStatusDesc

    inputTargetToDb k (T.TargetDate dy ds hw) =
      ProjectTargetDate k ds dy hw

    customerChoiceOption (Entity k v) = (k,v ^. customerName)
    personChoiceOption (Entity k v) = (k,v ^. personName)

projectRowSplice :: PhbRuntimeSplice [T.Project] -> PhbSplice
projectRowSplice = rowSplice (ts <> ss)
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
    statusLines ps =
      (ps ^. T.projectStatusPhase : ps ^. T.projectStatusDesc . to toList )
    effortSplices = mapV (C.pureSplice . C.textSplice) $ do
      "effortDays"  ## (^. T.projectEffortDays . to show . from unpacked)
      "effortStart" ## (^. T.projectStarted . to spliceDay)
    targetsSplice = "target" ## C.manyWithSplices C.runChildren targetSplice
    targetSplice = mapV (C.pureSplice . C.textSplice) $ do
      "desc"          ## (^. T.targetDateDesc)
      "dayOrHandwavy" ## dayOrHandwavy

    dayOrHandwavy td = fromMaybe
      (td ^. T.targetDateDay . to show . from unpacked)
      (td ^. T.targetDateHandwavy)

listProjectsSplices :: PhbSplice
listProjectsSplices =
  C.withSplices
    C.runChildren
    ("projectRow" ## projectRowSplice)
    . lift $ do
  ct <- liftIO $ getCurrentTime
  runPersist $ do
    es <- selectList [] [Desc ProjectStarted]
    traverse (loadProject ct) $ es

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
