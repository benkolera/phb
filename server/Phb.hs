{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Control.Lens    ((^..),(%~),traversed,from,_Left)
import Control.Monad.Reader (ReaderT,runReaderT)
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.Aeson      (ToJSON)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Network.HTTP.Types
import Network.Wai     
import Network.Wai.Handler.Warp 
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Servant.Server.Internal

import Database.PostgreSQL.Simple (connectPostgreSQL)

import Phb.Db    (DbEnv(DbEnv),DbError,listCustomers)
import Phb.Types (_ApiCustomer,Customer)

instance ToJSON Customer

type PhbApi =
  "login"            :> Raw
  :<|> "logout"      :> Raw
  :<|> "currentUser" :> Raw
  :<|> AuthedApi

type AuthedApi = "customers" :> Get '[JSON] [Customer]

phbAPI :: Proxy PhbApi
phbAPI = Proxy

authedApi :: Proxy AuthedApi
authedApi = Proxy

runPhb :: DbEnv -> (ReaderT DbEnv (ExceptT DbError IO) :~> EitherT ServantErr IO)
runPhb e = Nat $
  EitherT
  . fmap (_Left %~ toServantError)
  . runExceptT
  . flip runReaderT e
  where
    toServantError e = ServantErr
      500
      "Internal Server Error"
      (T.encodeUtf8 . T.pack . show $ e )
      []

serverT :: ServerT AuthedApi (ReaderT DbEnv (ExceptT DbError IO))
serverT = customers
  where
    customers = (^..traversed.from _ApiCustomer) <$> listCustomers

login :: Application
login = undefined

logout :: Application
logout = undefined
        
currentUser :: Application
currentUser = undefined

server :: DbEnv -> Server PhbApi
server e =
  login
  :<|> logout
  :<|> currentUser
  :<|> enter (runPhb e) serverT

app :: DbEnv -> Application
app e = logStdout (serve phbAPI (server e))

main :: IO ()
main = do
  -- Dirty dirty for now
  e <- DbEnv <$> connectPostgreSQL "dbname='phb2'"
  run 8000 (app e)
