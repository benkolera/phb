{-# LANGUAGE OverloadedStrings #-}
module Phb.Server.Auth where

import Blaze.ByteString.Builder (toByteString)
import Control.Applicative      (pure)
import Control.Lens             ((^.))
import Control.Monad            ((<=<))
import Data.Aeson               (decode,encode)
import Network.HTTP.Types       (status200,status400,status403)
import Network.Wai              (Application,lazyRequestBody,responseLBS)
import Web.ClientSession        (Key,decrypt,encrypt,IV,randomIV)
import Web.Cookie               (setCookieName,setCookieValue,renderSetCookie,def,setCookieMaxAge)

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import Phb.Server.Env  (PhbEnv,phbSessionKey)
import Phb.Types.Login (Login(Login))
import Phb.Types.User  (User(User))

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

cookieName :: B.ByteString
cookieName = "phbSession"

encryptUserCookie :: Key -> IV -> User -> B.ByteString
encryptUserCookie k iv u = toByteString cb
  where
    cb = renderSetCookie $ def
      { setCookieName  = cookieName
      , setCookieValue = encrypt k iv . BL.toStrict . encode $ u
      }

logout :: Application
logout _ res = res $ responseLBS status200 [("Set-Cookie",cookieVal)] ""
  where
    cookieVal = toByteString . renderSetCookie $ def
      { setCookieName   = cookieName
      , setCookieMaxAge = Just (-1)
      }
