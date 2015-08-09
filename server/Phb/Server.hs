{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Phb.Server
  ( PhbEnv(PhbEnv)
  , server
  , phbAPI
  )where

import Control.Applicative (pure)
import Control.Monad.Except (throwError)
import Control.Lens ((^.),(%~),_Left,view)
import Control.Monad.Reader (ReaderT,runReaderT)
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.Aeson      (decode)
import Servant
import Web.ClientSession                    (decrypt)
  
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Phb.Db  (CustomerId)
import Phb.Api (ApiError)
import qualified Phb.Api as A
import Phb.Types 
import Phb.Server.Cookie (Cookie)
import Phb.Server.Auth   (login,logout)
import Phb.Server.Env    (PhbEnv(PhbEnv),AuthedPhbEnv(AuthedPhbEnv),phbSessionKey,authedPhbUser)

instance FromText B.ByteString where
  fromText = pure . T.encodeUtf8

type PhbApi 
  =    "login"  :> Raw
  :<|> "logout" :> Raw
  :<|> Cookie "phbSession" B.ByteString :> AuthedApi

type AuthedApi
  =    "customers"   :> 
    ( Get '[JSON] (A.CustomerList CustomerRead)
    :<|> Capture "customerId" CustomerId :>
      ( Get '[JSON] (A.CustomerRecord CustomerRead)
      :<|> ReqBody '[JSON] (A.CustomerRecord CustomerUpdate) :> Put '[JSON] (A.CustomerRecord CustomerRead)
      )
    )
  :<|> "currentUser" :> Get '[JSON] User

phbAPI :: Proxy PhbApi
phbAPI = Proxy

type AuthedPhb = ReaderT AuthedPhbEnv (ExceptT ApiError IO)

serverT :: ServerT AuthedApi AuthedPhb
serverT = customers :<|> currentUser
  where
    currentUser = view authedPhbUser
    customers   = A.customerList :<|> customer
    customer i  = A.getCustomer i :<|> A.putCustomer i

server :: PhbEnv -> Server PhbApi
server e
  =    login e
  :<|> logout 
  :<|> (\ uMay -> enter (r uMay) serverT)
  where
    r :: Maybe B.ByteString ->  (AuthedPhb :~> EitherT ServantErr IO)
    r uBsMay = Nat $ \ p -> do
      let k    = e ^. phbSessionKey
      let uMay = decode . BL.fromStrict =<< decrypt k =<< uBsMay
      case uMay of
        Nothing -> throwError loginRequired
        Just u  -> EitherT
          . fmap (_Left %~ toServantError)
          . runExceptT
          . flip runReaderT (AuthedPhbEnv u e)
          $ p
    loginRequired = ServantErr 401 "Login Required" "" []
    toServantError err = ServantErr
      500
      "Internal Server Error"
      (TL.encodeUtf8 . TL.pack . show $ err )
      []
