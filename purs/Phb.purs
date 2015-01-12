module Phb where

import Control.Monad.Eff
import qualified Control.Monad.JQuery as J
import Data.Maybe
import Data.Traversable
import DOM
import Graphics.Canvas
import PleaseJs
import TinyColor
import ChartJs
import Debug.Trace
import Data.Either
import Data.Foreign
import Data.Foreign.Class

heartbeatTimebreakdown
  :: forall eff .
     String
  -> [{value:: Number, label:: String}]
  -> DoughnutChartConfig
  -> Eff ( pleaseJs :: PleaseJs, dom:: DOM,  canvas :: Canvas | eff ) ChartType
heartbeatTimebreakdown canvasId dataz config = do
  Just c <- getCanvasElementById(canvasId)
  ctx    <- getContext2D c
  chart  <- newChart(ctx)
  dataz' <- traverse fillInColor dataz
  doughnutChart chart dataz' config
  where
    fillInColor d = do
      c <- makeColor
      return $
        { label     : d.label
        , value     : d.value
        , color     : c
        , highlight : (tinycolor >>> toHex $ c)
        }

initTimeLogManyForm :: Eff ( dom :: DOM , trace:: Trace ) Unit
initTimeLogManyForm = do
  hs <- J.select ".log-hour"
  ms <- J.select ".log-minute"
  flip (J.on "change") hs $ \_ ob -> do
    Right hour <- read <$> J.getValue ob
    trace $ "Hour changed to " ++ hour
  flip (J.on "change") ms $ \_ ob -> do
    Right minute <- read <$> J.getValue ob
    trace $ "Minute changed to " ++ minute

  return unit
