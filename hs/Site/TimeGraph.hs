{-# LANGUAGE OverloadedStrings #-}
module Site.TimeGraph where

import           BasePrelude
import           Prelude                             ()

import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Lens
import           Control.Monad.Trans                 (liftIO)
import qualified Data.ByteString.Char8               as B
import           Data.Map.Syntax
import           Data.Text                           (Text, pack)
import           Data.Text.Encoding                  (encodeUtf8)
import           Data.UUID                           (toString)
import           Data.UUID.V4
import qualified Heist.Compiled                      as C
import qualified Phb.Types.TimeSummary               as T
import           Site.Internal
import           Text.Printf                         (printf)

timeSummaryDataSplices :: PhbRuntimeSplice [T.TimeSummary] -> PhbSplice
timeSummaryDataSplices rts = C.withSplices C.runChildren splices $ do
  ts <- rts
  uuid <- liftIO . fmap (pack . filter (/= '-') . toString) $ nextRandom
  pure (ts,uuid)
  where
    splices = do
      "timeLogData" ## timeSummaryDataSplice
      "uuid"        ## (C.pureSplice (C.textSplice snd))

timeSummaryDataSplice :: PhbRuntimeSplice ([T.TimeSummary],Text) -> PhbSplice
timeSummaryDataSplice rts = do
  so <- pure . C.yieldPure $ fromByteString "<script>\nvar timeLogData"
  js <- return $ C.yieldRuntime $ do
    (s,uuid) <- rts
    return
      . fromByteString
      . ((encodeUtf8 uuid <> "=[]") <>)
      . B.intercalate ","
      . fmap timeLogWholeJson
      . filter ((>= 0.5) . view T.timeSummaryHours) $ s
  sc <- pure . C.yieldPure $ fromByteString "];\n</script>"
  return . fold $ [so,js,sc]
  where
    timeLogWholeJson tlw = fold
      [ "{value: "
      , tlw^.T.timeSummaryHours.to (printf "%.3f").to B.pack
      , ", label: '"
      , tlw^.T.timeSummaryLabel.to encodeUtf8
      , "'}"
      ]
