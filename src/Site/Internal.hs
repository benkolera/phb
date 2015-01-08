{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Site.Internal where

import BasePrelude hiding (Handler)
import Prelude     ()

import           Blaze.ByteString.Builder.ByteString     (fromByteString)
import           Blaze.ByteString.Builder.Internal.Types (Builder)
import           Control.Lens
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString.Char8                   as B
import           Data.Map.Syntax                         (( ## ))
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Data.Text.Encoding                      (encodeUtf8)
import           Data.Time                               (Day, LocalTime,
                                                          formatTime)
import           Database.Persist.Sql
import           Heist                                   (RuntimeSplice,
                                                          Splices, getParamNode)
import qualified Heist.Compiled                          as C
import qualified Heist.Compiled.LowLevel                 as C
import           Snap                                    hiding (get)
import           Snap.Snaplet.Auth                       (AuthManager,
                                                          requireUser)
import           Snap.Snaplet.Heist.Compiled             (HasHeist, Heist,
                                                          heistLens)
import           Snap.Snaplet.Persistent                 (HasPersistPool,
                                                          PersistState,
                                                          getPersistPool,
                                                          runPersist)
import           Snap.Snaplet.Session                    (SessionManager,
                                                          commitSession,
                                                          deleteFromSession,
                                                          getFromSession,
                                                          setInSession,
                                                          withSession)
import           System.Locale                           (defaultTimeLocale)
import           Text.Digestive                          (Form, Formlet, check,
                                                          dateFormlet, listOf,
                                                          localTimeFormlet,
                                                          optionalDateFormlet, optionalLocalTimeFormlet,
                                                          text)
import           Text.XmlHtml                            (getAttribute)

import Phb.Db
import Phb.Ldap
import Phb.Mail
import Phb.Util

data Phb = Phb
  { _heist :: Snaplet (Heist Phb)
  , _db    :: Snaplet PersistState
  , _auth  :: Snaplet (AuthManager Phb)
  , _sess  :: Snaplet SessionManager
  , _mail  :: MailConfig
  , _ldap  :: LdapConfig
  }

makeLenses ''Phb

instance HasHeist Phb where
  heistLens = subSnaplet heist

instance HasPersistPool PhbHandler where
  getPersistPool = with db getPersistPool

spliceDay :: Day -> Text
spliceDay = T.pack . formatTime defaultTimeLocale "%F"

spliceKey :: ToBackendKey SqlBackend a => Key a -> Text
spliceKey = T.pack . show . fromSqlKey

showText :: Show s => s -> Text
showText = T.pack . show

spliceLocalTime :: LocalTime -> Text
spliceLocalTime = T.pack . formatTime defaultTimeLocale "%F %H:%M"

requireParam :: B.ByteString -> Handler Phb Phb B.ByteString
requireParam name =
  getParam name >>= maybe (die400 $ "Required param: " <> name) return

requireKey :: PersistEntity r => B.ByteString -> PhbHandler (Key r)
requireKey name = do
  p <- requireParam name
  requireOr400 ("Invalid ID: " <> p) . stringToKey . B.unpack $ p

-- TODO: This sucks because once we've returned the val we have no
-- more transaction.
requireEntity
  :: ToBackendKey SqlBackend a
  => ByteString
  -> ByteString
  -> Handler Phb Phb (Entity a)
requireEntity thing paramName = do
  k <- requireKey paramName
  e <- runPersist $ getEntity k
  requireOr404 thing (B.pack . show . fromSqlKey $ k) e

requireOr400 :: B.ByteString -> Maybe a -> PhbHandler a
requireOr400 = (`maybe` return) . die400

die400 :: B.ByteString -> PhbHandler b
die400 = dieWithStatus 400 "Bad Request"

requireOr404 :: B.ByteString -> B.ByteString -> Maybe a -> PhbHandler a
requireOr404 thing = (`maybe` return) . die404 thing

die404 :: B.ByteString -> B.ByteString -> PhbHandler b
die404 thing thingId = dieWithStatus 404 "Not Found" (thing <> "/" <> thingId)

getOr404 :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => B.ByteString -> Key b -> PhbHandler (Entity b)
getOr404 thing key = do
  e <- runPersist (get key)
  requireOr404 thing (B.pack $ show key) . fmap (Entity key) $ e

dieWithStatus :: Int -> B.ByteString -> B.ByteString -> PhbHandler b
dieWithStatus n m a = do
  modifyResponse $ setResponseStatus n m
  writeBS a
  getResponse >>= finishWith

flashSplice :: PhbSplice
flashSplice = do
  n <- getParamNode
  let typ = maybe "warning" id $ getAttribute "type" n
  let k   = "_" <> typ
  p <- C.newEmptyPromise
  out <- C.withLocalSplices
         (splices typ (C.getPromise p))
         mempty
         (C.callTemplate "_flash")
  return $ C.yieldRuntime $ do
    msg <- lift . withTop sess $ getFromSession k
    case msg of
     Nothing -> return mempty
     Just m -> do
       lift $ withTop sess $ do
         deleteFromSession k
         commitSession
       C.putPromise p m >> C.codeGen out
  where
    splices :: Text -> PhbRuntimeSplice Text -> Splices PhbSplice
    splices t m = do
      "type"    ## pure . C.yieldPureText . translateType $ t
      "message" ## pure $ C.yieldRuntimeText m
    translateType "error" = "danger"
    translateType a = a

flashSplices :: Splices PhbSplice
flashSplices = "flash" ## flashSplice

flash :: Text -> Text -> PhbHandler ()
flash l =
  withSession sess . with sess . setInSession ("_" <> l)

flashSuccess,flashError,flashInfo :: Text -> PhbHandler ()
flashSuccess = flash "success"
flashError   = flash "error"
flashInfo    = flash "info"

rowSplice :: Splices (PhbRuntimeSplice a -> PhbSplice) -> PhbRuntimeSplice [a] -> PhbSplice
rowSplice ss rt = do
  promise <- C.newEmptyPromise
  rows <- C.manyWithSplices C.runChildren ss (C.getPromise promise)
  return $ C.yieldRuntime $ do
    as <- rt
    if (null as)
    then return . fromByteString $ "<tr><td colspan=\"100%\">None</td></tr>"
    else C.putPromise promise as >> C.codeGen rows

spliceLines :: PhbRuntimeSplice [Text] -> PhbSplice
spliceLines rt = do
  a <- pure . C.yieldPure $ fromByteString "<p>"
  b <- pure . C.yieldRuntime $ fmap mkNodes rt
  c <- pure . C.yieldPure $ fromByteString "</p>"
  return . fold $ [a,b,c]
  where
    mkNodes :: [Text] -> Builder
    mkNodes = foldMap ((<> fromByteString "<br />") . (fromByteString . encodeUtf8))

isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null

html5DateFormlet :: Monad m => Formlet Text m Day
html5DateFormlet = dateFormlet "%F"
html5OptDateFormlet :: Monad m => Maybe Day -> Form Text m (Maybe Day)
html5OptDateFormlet = optionalDateFormlet "%F"
html5LocalTimeFormlet :: Monad m => Formlet Text m LocalTime
html5LocalTimeFormlet = localTimeFormlet "%F" "%R"
html5OptLocalTimeFormlet :: Monad m => Maybe LocalTime -> Form Text m (Maybe LocalTime)
html5OptLocalTimeFormlet = optionalLocalTimeFormlet "%F" "%R"

type PhbHandler = Handler Phb Phb
type PhbAuthedHandler = Handler Phb (AuthManager Phb)
type PhbSplice = C.Splice PhbHandler
type PhbRuntimeSplice = RuntimeSplice PhbHandler
type PhbRoutes = [(ByteString, PhbHandler ())]
type PhbForm a b = Form a PhbHandler b
type PhbFormlet a b = Formlet a PhbHandler b

choiceOpt :: Getting c' b c' -> Entity b -> (Key b, c')
choiceOpt fl = entityKey &&& (view fl . entityVal)
choiceOpts :: Getting c' b c' -> [Entity b] -> [(Key b, c')]
choiceOpts = fmap . choiceOpt

nelOf :: Monad m => v -> Formlet v m a -> Maybe [a] -> Form v m [a]
nelOf errMsg opts = check errMsg null . listOf opts

neText :: Monad m => v -> Maybe Text -> Form v m Text
neText errMsg = check errMsg isNotEmpty . text

dateParam :: ByteString -> PhbHandler (Maybe Day)
dateParam l = (parseDay . B.unpack =<<) <$> getParam l

pageParam :: PhbHandler Int
pageParam = do
  pMay <- getParam "page"
  pure
    . fromMaybe 1
    . mfilter (> 1)
    . (readMaybe =<<)
    . fmap B.unpack
    $ pMay

paginationParam  :: Int -> PhbHandler [SelectOpt r]
paginationParam pw = flip paginate pw <$> pageParam

defPaginationParam :: PhbHandler [SelectOpt r]
defPaginationParam = paginationParam 25

userOrIndex :: PhbHandler () -> PhbHandler ()
userOrIndex = requireUser auth notLoggedIn
 where
   notLoggedIn = do
     p <- rqURI <$> getRequest
     flashError "You must be logged in to view this page"
     redirect $ "/login?andThen=" <> urlEncode p
