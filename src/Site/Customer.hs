{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Site.Customer where

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

import Phb.Db
import Site.Internal

customerRoutes :: PhbRoutes
customerRoutes =
  [("/customers",ifTop $ render "customers/all")
  ,("/customers/create",ifTop $ render "customers/create")
  ,("/customers/:id/edit",ifTop $ render "customers/edit")
  ]

data CustomerInput = CustomerInput
  { _customerInputName :: Text
  }
makeLenses ''CustomerInput

customerForm :: Maybe Customer -> PhbForm T.Text CustomerInput
customerForm e = CustomerInput
  <$> "name"         .: check nameErrMsg isNotEmpty (text name)
  where
    name    = e ^?_Just.customerName
    nameErrMsg = "Name must not be empty"

customerFormSplices :: PhbRuntimeSplice (Maybe (Entity Customer)) -> PhbSplice
customerFormSplices rts = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("customerForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    e  <- rts
    cd <- liftIO getCurrentTime
    (v, result) <- lift $ runForm "customer" (customerForm . fmap entityVal $ e)

    case result of
      Just x  -> do
        lift (createCustomer x cd (e ^? _Just . eKey))
      Nothing -> C.putPromise promise v >> C.codeGen out
  where
    createCustomer x _ kMay = do
      -- Should probably change this so that a DB error wouldn't just
      -- Crash us and should put a nice error in the form.
      case kMay of
       Nothing -> do
         void . runPersist $ do
           insert (newCustomer x)
         flashSuccess $ "Customer Created"
       Just k  -> do
         void . runPersist $ do
           replace k (newCustomer x)
         flashSuccess $ "Customer Updated"
      redirect "/customers"
    newCustomer (CustomerInput n) = Customer n

customerRowSplice :: PhbRuntimeSplice [Entity Customer] -> PhbSplice
customerRowSplice = rowSplice (ts <> ss)
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "name"          ## (^.eVal.customerName)
      "id"            ## (^.eKey.to spliceKey)
    ss = mempty

listCustomersSplices :: PhbSplice
listCustomersSplices =
  C.withSplices
    C.runChildren
    ("customerRow" ## customerRowSplice)
    . lift $ do
  runPersist $ do
    loadActiveCustomers

createCustomerSplices :: PhbSplice
createCustomerSplices = customerFormSplices (pure Nothing)

editCustomerSplices :: PhbSplice
editCustomerSplices = customerFormSplices . lift $ do
  Just <$> requireEntity "customer" "id"

allCustomerSplices :: Splices PhbSplice
allCustomerSplices = do
  "allCustomers"   ## listCustomersSplices
  "createCustomer" ## createCustomerSplices
  "editCustomer"   ## editCustomerSplices
