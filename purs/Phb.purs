module Phb where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import qualified Control.Monad.JQuery as J
import Data.Maybe
import Data.Traversable
import DOM
import Graphics.Canvas
import Math
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
  -> Eff ( pleaseJs :: PleaseJs
         , dom:: DOM
         , canvas :: Canvas
         , ref :: Ref
         | eff ) ChartType
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

initTimeLogManyForm :: Eff ( dom :: DOM , trace:: Trace , ref :: Ref ) Unit
initTimeLogManyForm = do
  hs <- J.select ".log-hour"
  ms <- J.select ".log-minute"
  minRef <- newRef 0
  flip (J.on "change") hs $ \_ ob -> do
    hoursStr <- J.getValue ob
    hours <- readNum <$> J.getValue ob
    addMinutes minRef (hours * 60)
  flip (J.on "change") ms $ \_ ob -> do
    minutes <- readNum <$> J.getValue ob
    addMinutes minRef minutes
  return unit
  where
    readNum f = fromMaybe 0 $ (hush $ readString f) >>= stringToNumber
    hush e = either (const Nothing) Just e

addMinutes
  :: forall eff
   . RefVal Number
  -> Number
  -> Eff ( dom :: DOM , trace :: Trace , ref :: Ref | eff ) Unit
addMinutes ref mins = do
  totalMins <- modifyRef' ref incMins
  let newHours = floor (totalMins / 60)
  let newMins  = totalMins % 60
  he <- J.select "#log-total-hours"
  me <- J.select "#log-total-minutes"
  J.setText (zeroPad newHours) he
  J.setText (zeroPad newMins) me
  return unit
  where
    zeroPad x = (if x < 10 then "0" else "") ++ show x
    incMins x =
      let y = x + mins
      in { retVal: y , newState: y }

stringToNumber :: String -> Maybe Number
stringToNumber = stringToNumberForeign Just Nothing

foreign import stringToNumberForeign
  """
  function stringToNumberForeign (mkJust) {
    return function (nothing) {
      return function (x) {
        var num = parseFloat(x);
        if (!isNaN(num) && isFinite(x)) {
          return mkJust(num);
        } else {
          return nothing;
        }
      }
    }
  }
  """ :: (Number -> Maybe Number) -> Maybe Number -> String -> Maybe Number
