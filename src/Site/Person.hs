{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Site.Person where


import           BasePrelude                   hiding (bool, insert)
import           Prelude                       ()

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
import           Site.Internal

personRoutes :: PhbRoutes
personRoutes =
  [("/people",ifTop $ render "people/all")
  ,("/people/create",ifTop $ render "people/create")
  ,("/people/:id/edit",ifTop $ render "people/edit")
  ]

data PersonInput = PersonInput
  { _personInputName              :: Text
  , _personInputEmail             :: Text
  , _personInputDepartment        :: Text
  , _personInputReceivesHeartbeat :: Bool
  , _personInputLogsTime          :: Bool
  }
makeLenses ''PersonInput

personForm :: Maybe Person -> PhbForm T.Text PersonInput
personForm e = PersonInput
  <$> "name"              .: check nameErrMsg isNotEmpty (text name)
  <*> "email"             .: check emailErrMsg isNotEmpty (text email)
  <*> "department"        .: check departmentErrMsg isNotEmpty (text department)
  <*> "receivesHeartbeat" .: bool receivesHeartbeat
  <*> "logsTime"          .: bool logsTime
  where
    name              = e ^?_Just.personName
    email             = e ^?_Just.personEmail
    department        = e ^?_Just.personDepartment
    receivesHeartbeat = e ^?_Just.personReceivesHeartbeat
    logsTime          = e ^?_Just.personLogsTime
    nameErrMsg        = "Name must not be empty"
    emailErrMsg       = "Email must not be empty"
    departmentErrMsg  = "Department must not be empty"

personFormSplices :: PhbRuntimeSplice (Maybe (Entity Person)) -> PhbSplice
personFormSplices rts = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("personForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    e  <- rts
    cd <- liftIO getCurrentTime
    (v, result) <- lift $ runForm "person" (personForm . fmap entityVal $ e)

    case result of
      Just x  -> do
        lift (createPerson x cd (e ^? _Just . eKey))
      Nothing -> C.putPromise promise v >> C.codeGen out
  where
    createPerson x _ kMay = do
      -- Should probably change this so that a DB error wouldn't just
      -- Crash us and should put a nice error in the form.
      case kMay of
       Nothing -> do
         void . runPersist $ do
           insert (newPerson x)
         flashSuccess $ "Person Created"
       Just k  -> do
         void . runPersist $ do
           replace k (newPerson x)
         flashSuccess $ "Person Updated"
      redirect "/people"
    newPerson (PersonInput n e d rh lt) = Person n e d rh lt

personRowSplice :: PhbRuntimeSplice [Entity Person] -> PhbSplice
personRowSplice = rowSplice (ts <> ss)
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "name"              ## (^.eVal.personName)
      "email"             ## (^.eVal.personEmail)
      "department"        ## (^.eVal.personDepartment)
      "receivesHeartbeat" ## (^.eVal.personReceivesHeartbeat.to boolYN)
      "logsTime"          ## (^.eVal.personLogsTime.to boolYN)
      "id"                ## (^.eKey.to spliceKey)
    ss = mempty
    boolYN True  = "Yes"
    boolYN False = "No"

listPeopleSplices :: PhbSplice
listPeopleSplices =
  C.withSplices
    C.runChildren
    ("personRow" ## personRowSplice)
    . lift $ do
  runPersist $ do
    selectList [] [Asc PersonName]

createPeopleplices :: PhbSplice
createPeopleplices = personFormSplices (pure Nothing)

editPeopleplices :: PhbSplice
editPeopleplices = personFormSplices . lift $ do
  Just <$> requireEntity "person" "id"

allPersonSplices :: Splices PhbSplice
allPersonSplices = do
  "allPeople"    ## listPeopleSplices
  "createPerson" ## createPeopleplices
  "editPerson"   ## editPeopleplices
