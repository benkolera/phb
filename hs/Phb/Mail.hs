{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Mail
    ( MailConfig(..)
    , postEmail
    , timelogPesterEmail
    , mkMailConfig
    ) where

import           BasePrelude
import           Prelude                       ()

import           Control.Lens
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Morph
import           Control.Monad.Reader          (Reader, ReaderT (..), asks,
                                                runReader)
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Database.Persist              (Entity (..))
import           Network.Mail.Mime             (Address (..),
                                                renderSendMailCustom,
                                                simpleMail)
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Internal           (textValue)

import           Phb.Db

data MailConfig = MailConfig
  { _mcFromAddr :: Address
  , _mcSmPath   :: String
  , _mcSmOps    :: [String]
  , _mcBaseUrl  :: Text
  } deriving (Show)
makeLenses ''MailConfig

mkMailConfig :: C.Config -> IO MailConfig
mkMailConfig c = MailConfig
  <$> requireFromAddress
  <*> C.lookupDefault "/usr/sbin/sendmail" c "mail.path"
  <*> C.lookupDefault ["-t"]               c "mail.option"
  <*> C.require c "mail.hostname"
  where
    requireFromAddress = Address
      <$> C.lookup  c "mail.fromName"
      <*> C.require c "mail.fromEmail"

postEmail :: Address -> Text -> TL.Text -> Html -> ReaderT MailConfig IO ()
postEmail toAddr subj plain html = do
  f    <- view mcFromAddr
  path <- view mcSmPath
  ops  <- view mcSmOps
  m <- liftIO $ simpleMail toAddr f subj plain (renderHtml html) []
  liftIO $ renderSendMailCustom path ops m

timelogPesterEmail :: Entity Person -> ReaderT MailConfig IO ()
timelogPesterEmail (Entity _ p) = do
  l <- hoist generalize link
  postEmail
   (personToAddress p)
   "PHB - Please log time for today"
   (plain $ TL.fromStrict l)
   (html l)

 where
   intro = "You have not logged time for today."
   plea  = "Please log it here: "
   link  = mkLink $ "/time_logs/create"
   plain l = TL.unlines
     [ intro
     , plea <> l
     ]
   html l = H.html . H.body $ do
     H.p $ H.toHtml intro
     H.p $ do
       H.toHtml plea
       H.a H.! A.href (textValue l) $ H.toHtml l

personToAddress :: Person -> Address
personToAddress = runReader $ Address
  <$> view (personName.to Just)
  <*> view personEmail

mkLink :: Text -> Reader MailConfig Text
mkLink path = asks (\mc -> "http://" <> (mc^.mcBaseUrl) <> rest)
  where
    rest = "/" <> T.dropWhile (== '/') path
