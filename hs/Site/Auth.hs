{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Site.Auth where

import           BasePrelude                   hiding (Handler, bool, insert)
import           Prelude                       ()

import           Control.Lens
import           Control.Monad.Trans           (lift)
import           Data.ByteString               (ByteString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Heist                         hiding (Error)
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (getParam, ifTop, redirect,
                                                urlDecode, with)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist.Compiled
import           Text.Digestive
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import           Phb.Auth                      (loginViaLdap)
import           Site.Internal

handleLogout :: PhbHandler ()
handleLogout = do
  with auth logout
  redirect "/"

authRoutes :: PhbRoutes
authRoutes =
  [ ("/login"  , ifTop $ render "login")
  , ("/logout" , ifTop $ handleLogout)
  ]

data LoginInput = LoginInput
  { _loginInputUsername   :: Text
  , _loginInputPassword   :: Text
  , _loginInputAndThen    :: Maybe Text
  , _loginInputRememberMe :: Bool
  }
makeLenses ''LoginInput

loginForm :: PhbForm T.Text (AuthUser,Maybe Text)
loginForm = monadic $ do
  andThen <- fmap decodeUtf8 <$> getParam "andThen"
  return $ validateM login $ LoginInput
    <$> "username"   .: text Nothing
    <*> "password"   .: text Nothing
    <*> "andThen"    .: optionalText andThen
    <*> "rememberMe" .: bool (Just True)
  where
    login i = do
      r <- with auth $ loginViaLdap
        ldap
        (i^.loginInputUsername)
        (i^.loginInputPassword)
        (i^.loginInputRememberMe)
      pure
        . either (const $ Error "Login failed") Success
        . fmap (,i^.loginInputAndThen)
        $ r

loginFormSplices :: PhbSplice
loginFormSplices = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("loginForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    rt <- lift . with auth $ loginByRememberToken
    case rt of
      Right _ -> do
        lift $ andThenParam >>= andThenRedirect
      Left  _ -> do
        (v, result) <- lift $ runForm "login" loginForm
        case result of
          Just (_,andThen) -> lift $ andThenRedirect andThen
          Nothing -> C.putPromise promise v >> C.codeGen out

andThenName :: ByteString
andThenName = "andThen"
andThenParam :: PhbHandler (Maybe Text)
andThenParam = do
  pMay <- getParam andThenName
  pure $ pMay >>= urlDecode <&> decodeUtf8

andThenRedirect :: Maybe Text -> PhbHandler a
andThenRedirect = redirect . maybe "/" encodeUtf8

allAuthSplices :: Splices PhbSplice
allAuthSplices = do
  "login" ## loginFormSplices
