module Phb where

import Control.Monad.Eff
import Data.Maybe
import Data.Traversable
import DOM
import Graphics.Canvas
import PleaseJs
import ChartJs
import Debug.Trace

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
        , highlight : c
        }
