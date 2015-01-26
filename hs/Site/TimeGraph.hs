{-# LANGUAGE OverloadedStrings #-}
module Site.TimeGraph where

import BasePrelude
import Prelude     ()

import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Lens
import qualified Data.ByteString.Char8               as B
import           Data.Map.Syntax
import           Data.Text.Encoding                  (encodeUtf8)
import qualified Heist.Compiled                      as C
import qualified Phb.Types.TimeSummary               as T
import           Site.Internal
import           Text.Printf                         (printf)

timeSummaryDataSplices :: PhbRuntimeSplice [T.TimeSummary] -> PhbSplice
timeSummaryDataSplices = C.withSplices C.runChildren $ do
  "timeLogData" ## timeSummaryDataSplice

timeSummaryDataSplice :: PhbRuntimeSplice [T.TimeSummary] -> PhbSplice
timeSummaryDataSplice rts = do
  so <- pure . C.yieldPure $ fromByteString "<script>\nvar timeLogData=["
  js <- return $ C.yieldRuntime $ do
    s <- rts
    return
      . fromByteString
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
