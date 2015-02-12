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
timeSummaryDataSplices = C.deferMap mkUuid splices
  where
    mkUuid ts = do
      uuid <- liftIO . fmap (pack . filter (/= '-') . toString) $ nextRandom
      pure (ts,uuid)
    splices = C.withSplices C.runChildren $ do
      "timeLogData" ## timeSummaryDataSplice
      "uuid"        ## (C.pureSplice (C.textSplice snd))

-- I hate this, so much
timeSummaryDataSplice :: PhbRuntimeSplice ([T.TimeSummary],Text) -> PhbSplice
timeSummaryDataSplice rts = do
  so  <- pts "<script>\n$( function () {\n var timeLogData = ["
  js <- return $ C.yieldRuntime $ do
    (s,_) <- rts
    return
      . fromByteString
      . B.intercalate ","
      . fmap timeLogWholeJson
      . filter ((>= 0.5) . view T.timeSummaryHours) $ s
  fa   <- pts "];\nvar timeBreakdownChart = PS.Phb.heartbeatTimebreakdown(\""
  uuid <- pure . C.yieldRuntime $ rts >>= pure . fromByteString . encodeUtf8 . snd
  fb   <- pts "\")(timeLogData)(); console.log(timeBreakdownChart); $(\"#timeBreakdownLegend-"
  sc   <- pts "\").append(timeBreakdownChart.generateLegend());}); </script>\n"
  return . fold $ [so,js,fa,uuid,fb,uuid,sc]
  where
    pts = pure . C.yieldPure . fromByteString
    timeLogWholeJson tlw = fold
      [ "{value: "
      , tlw^.T.timeSummaryHours.to (printf "%.3f").to B.pack
      , ", label: '"
      , tlw^.T.timeSummaryLabel.to encodeUtf8
      , "'}"
      ]
