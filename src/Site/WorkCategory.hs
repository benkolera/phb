{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Site.WorkCategory where

import           BasePrelude                   hiding (insert)
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

workCategoryRoutes :: PhbRoutes
workCategoryRoutes =
  [("/work_categories",ifTop . userOrIndex . render $ "work_categories/all")
  ,("/work_categories/create",ifTop . userOrIndex . render $ "work_categories/create")
  ,("/work_categories/:id/edit",ifTop . userOrIndex . render $ "work_categories/edit")
  ]

data WorkCategoryInput = WorkCategoryInput
  { _workCategoryInputName :: Text
  }
makeLenses ''WorkCategoryInput

workCategoryForm :: Maybe WorkCategory -> PhbForm T.Text WorkCategoryInput
workCategoryForm e = WorkCategoryInput
  <$> "name"         .: check nameErrMsg isNotEmpty (text name)
  where
    name    = e ^?_Just.workCategoryName
    nameErrMsg = "Name must not be empty"

workCategoryFormSplices :: PhbRuntimeSplice (Maybe (Entity WorkCategory)) -> PhbSplice
workCategoryFormSplices rts = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("workCategoryForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    e  <- rts
    cd <- liftIO getCurrentTime
    (v, result) <- lift $ runForm "workCategory" (workCategoryForm . fmap entityVal $ e)

    case result of
      Just x  -> do
        lift (createWorkCategory x cd (e ^? _Just . eKey))
      Nothing -> C.putPromise promise v >> C.codeGen out
  where
    createWorkCategory x _ kMay = do
      -- Should probably change this so that a DB error wouldn't just
      -- Crash us and should put a nice error in the form.
      case kMay of
       Nothing -> do
         void . runPersist $ do
           insert (newWorkCategory x)
         flashSuccess $ "WorkCategory Created"
       Just k  -> do
         void . runPersist $ do
           replace k (newWorkCategory x)
         flashSuccess $ "WorkCategory Updated"
      redirect "/work_categories"
    newWorkCategory (WorkCategoryInput n) = WorkCategory n

workCategoryRowSplice :: PhbRuntimeSplice [Entity WorkCategory] -> PhbSplice
workCategoryRowSplice = rowSplice (ts <> ss)
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "name"          ## (^.eVal.workCategoryName)
      "id"            ## (^.eKey.to spliceKey)
    ss = mempty

listWorkCategorysSplices :: PhbSplice
listWorkCategorysSplices =
  C.withSplices
    C.runChildren
    ("workCategoryRow" ## workCategoryRowSplice)
    . lift $ do
  runPersist $ do
    ct <- liftIO getCurrentTime
    loadActiveWorkCategories ct

createWorkCategorySplices :: PhbSplice
createWorkCategorySplices = workCategoryFormSplices (pure Nothing)

editWorkCategorySplices :: PhbSplice
editWorkCategorySplices = workCategoryFormSplices . lift $ do
  Just <$> requireEntity "workCategory" "id"

allWorkCategorySplices :: Splices PhbSplice
allWorkCategorySplices = do
  "allWorkCategories"  ## listWorkCategorysSplices
  "createWorkCategory" ## createWorkCategorySplices
  "editWorkCategory"   ## editWorkCategorySplices
