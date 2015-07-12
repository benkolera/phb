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

module Main where

import Blaze.ByteString.Builder (toByteString)
import Control.Monad.Except (throwError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens ((^.),(^..),(%~),traversed,from,_Left,makeClassy,view,makeClassyPrisms)
import Control.Monad ((<=<))
import Control.Monad.Reader (ReaderT,runReaderT,MonadReader)
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.Aeson      (ToJSON,FromJSON,decode,encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp 
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Servant.Server.Internal
import Web.ClientSession (getDefaultKey,Key,decrypt,encrypt,IV,randomIV)
import Web.Cookie (setCookieName,setCookieValue,renderSetCookie,def,setCookieMaxAge)

import Database.PostgreSQL.Simple (connectPostgreSQL)

import Phb.Db (DbEnv(DbEnv),HasDbEnv(dbEnv),DbError,AsDbError(_DbError),listCustomers)
import Phb.Types (_ApiCustomer,Customer,User(User),Login(Login))
import Phb.Servant (Cookie)

instance ToJSON Customer
instance ToJSON User
instance FromJSON User
instance FromJSON Login

instance FromText B.ByteString where
  fromText = pure . T.encodeUtf8

data PhbEnv = PhbEnv
  { _phbSessionKey :: Key
  , _phbDbEnv      :: DbEnv
  }
makeClassy ''PhbEnv

instance HasDbEnv PhbEnv where
  dbEnv = phbDbEnv . dbEnv

data AuthedPhbEnv = AuthedPhbEnv
  { _authedPhbUser   :: User
  , _authedPhbPhbEnv :: PhbEnv
  }
makeClassy ''AuthedPhbEnv

instance HasDbEnv AuthedPhbEnv where
  dbEnv = authedPhbPhbEnv . phbDbEnv . dbEnv

instance HasPhbEnv AuthedPhbEnv where
  phbEnv = authedPhbPhbEnv . phbEnv

type PhbApi 
  =    "login"  :> Raw
  :<|> "logout" :> Raw
  :<|> Cookie "phbSession" B.ByteString :> AuthedApi

type AuthedApi
  =    "customers"   :> Get '[JSON] [Customer]
  :<|> "currentUser" :> Get '[JSON] User

phbAPI :: Proxy PhbApi
phbAPI = Proxy

authedApi :: Proxy AuthedApi
authedApi = Proxy

data LoggedInUser

type AuthedPhb = ReaderT AuthedPhbEnv (ExceptT DbError IO)

serverT :: ServerT AuthedApi AuthedPhb
serverT = customers :<|> currentUser
  where
    customers   = (^..traversed.from _ApiCustomer) <$> listCustomers
    currentUser = view authedPhbUser

login :: PhbEnv -> Application
login e req res = do
  rb <- lazyRequestBody req
  case decode rb of
   Nothing            -> res $ responseLBS status400 [] ""
   Just (Login un pw) -> do
     uMay <- checkLogin un pw
     case uMay of
      Nothing -> res $ responseLBS status403 [] ""
      Just u  -> do
        iv <- randomIV
        let cookieVal = encryptUserCookie (e^.phbSessionKey) iv u
        res $ responseLBS status200 [("Set-Cookie",cookieVal)] ""
  where
    checkLogin u "butts" = pure . Just $ User 1 u
    checkLogin _ _       = pure $ Nothing

decryptUserCookie :: Key -> B.ByteString -> Maybe User
decryptUserCookie k = decode . BL.fromStrict <=< decrypt k

cookieName = "phbSession"

encryptUserCookie :: Key -> IV -> User -> B.ByteString
encryptUserCookie k iv u = toByteString cb
  where
    cb = renderSetCookie $ def
      { setCookieName  = cookieName
      , setCookieValue = encrypt k iv . BL.toStrict . encode $ u
      }

logout :: Application
logout req res = res $ responseLBS status200 [("Set-Cookie",cookieVal)] ""
  where
    cookieVal = toByteString . renderSetCookie $ def
      { setCookieName   = cookieName
      , setCookieMaxAge = Just (-1)
      }

server :: PhbEnv -> Server PhbApi
server e
  =    login e
  :<|> logout 
  :<|> (\ uMay -> enter (run uMay) serverT)
  where
    run :: Maybe B.ByteString ->  (AuthedPhb :~> EitherT ServantErr IO)
    run uBsMay = Nat $ \ p -> do
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
    toServantError e = ServantErr
      500
      "Internal Server Error"
      (TL.encodeUtf8 . TL.pack . show $ e )
      []

app :: PhbEnv -> Application
app e = logStdout (serve phbAPI (server e))

main :: IO ()
main = do
  -- Dirty dirty for now
  e <- PhbEnv
    <$> getDefaultKey
    <*> (DbEnv <$> connectPostgreSQL "dbname='phb2'")
  run 8000 (app e)
