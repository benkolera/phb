{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Phb.Db.PersonLogin where

import           BasePrelude               hiding (insert, on)
import           Prelude                   ()

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Text                 (Text)
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Time                 (getCurrentTime)
import           Database.Esqueleto
import           Snap.Snaplet.Auth         (AuthFailure (..), AuthUser (..),
                                            Password (..), UserId (..))

import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import           Phb.Util

type WholeLogin = (Entity Person,Entity PersonLogin)

textPassword :: Password -> Text
textPassword (Encrypted bs) = decodeUtf8 bs
textPassword (ClearText bs) = decodeUtf8 bs

upsertAuthUser
  :: (MonadIO m,Applicative m)
  => AuthUser
  -> Db m (Either AuthFailure AuthUser)
upsertAuthUser au@AuthUser{..} = do
  ct <- liftIO getCurrentTime
  ($ct) . maybe insertAU updateAU $ userId
  where
    insertAU ct = do
      pk <- insert $ Person userLogin (fromMaybe "" userEmail) "" False False
      void . insert $ PersonLogin
        pk
        userLogin
        (textPassword $ fromMaybe (ClearText "") userPassword)
        userActivatedAt
        userSuspendedAt
        userRememberToken
        userLoginCount
        userFailedLoginCount
        userLockedOutUntil
        userCurrentLoginAt
        userLastLoginAt
        (fmap decodeUtf8 userCurrentLoginIp)
        (fmap decodeUtf8 userLastLoginIp)
        ct
        ct
        Nothing
        Nothing
        ""
        ""

      return $ Right $ au {userUpdatedAt = Just ct}

    updateAU uId ct = fmap (fromMaybe notFound) . runMaybeT $ do
      ((Entity pk _),(Entity plk _)) <- MaybeT $ lookupByUserId uId

      lift $ update $ \ p -> do
        set p [ PersonEmail =. val (fromMaybe "" userEmail) ]
        where_ ( p ^. PersonId ==. val pk)

      lift . update $ \ pl -> do
        set pl . catMaybes $
          [ Just $ PersonLoginLogin =. val userLogin
          , (\ (ClearText p) -> PersonLoginPassword =. val (decodeUtf8 p)) <$> userPassword
          , Just $ PersonLoginActivatedAt =. val userActivatedAt
          , Just $ PersonLoginSuspendedAt =. val userSuspendedAt
          , Just $ PersonLoginRememberToken =. val userRememberToken
          , Just $ PersonLoginLoginCount =. val userLoginCount
          , Just $ PersonLoginFailedLoginCount =. val userFailedLoginCount
          , Just $ PersonLoginLockedOutUntil =. val userLockedOutUntil
          , Just $ PersonLoginCurrentLoginAt =. val userCurrentLoginAt
          , Just $ PersonLoginLastLoginAt =. val userLastLoginAt
          , Just $ PersonLoginCurrentIp =. val (decodeUtf8 <$> userCurrentLoginIp)
          , Just $ PersonLoginLastIp =. val (decodeUtf8 <$> userLastLoginIp)
          , ((PersonLoginCreatedAt =.) . val) <$> userCreatedAt
          , Just $ PersonLoginUpdatedAt =. val ct
          , Just $ PersonLoginResetToken =. val userResetToken
          , Just $ PersonLoginResetRequestedAt =. val userResetRequestedAt
          , Just $ PersonLoginRoles =. val (show userRoles)
          ]
        where_ ( pl ^. PersonLoginId ==. val plk )
      return $ Right $ au {userUpdatedAt = Just ct}

    notFound = Left UserNotFound

lookupByUserId :: (MonadIO m, Applicative m)
  => UserId
  -> Db m (Maybe WholeLogin)
lookupByUserId (UserId t) = runMaybeT $ do
 k <- MaybeT . pure . textToKey $ t
 MaybeT . selectWholeLogin $ \ _ pl ->
   (pl ^. PersonLoginPerson ==. val k)

lookupByLogin :: (MonadIO m, Applicative m)
  => Text
  -> Db m (Maybe WholeLogin)
lookupByLogin login =
  selectWholeLogin $ \ _ pl -> (pl ^. PersonLoginLogin ==. val login)

lookupByRememberToken :: (MonadIO m, Applicative m)
  => Text
  -> Db m (Maybe WholeLogin)
lookupByRememberToken token =
  selectWholeLogin $ \ _ pl ->
   (pl ^. PersonLoginRememberToken ==. val (Just token))

selectWholeLogin
  :: (MonadIO m, Functor m)
  => (SqlExpr (Entity Person) -> SqlExpr (Entity PersonLogin) -> SqlExpr (Value Bool))
  -> Db m (Maybe WholeLogin)
selectWholeLogin w = selectFirstEsq $ from $ \ (p `InnerJoin` pl) -> do
  on (p ^. PersonId ==. pl ^. PersonLoginPerson)
  where_ (w p pl)
  return (p,pl)
