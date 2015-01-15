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
import Data.Either
import Data.Foreign
import Data.Foreign.Class

type TimeInputs = { hours :: J.JQuery , mins :: J.JQuery }

heartbeatTimebreakdown
  :: forall eff .
     String
  -> [{value:: Number, label:: String}]
  -> DoughnutChartConfig
  -> Eff ( pleaseJs :: PleaseJs
         , dom:: DOM
         , canvas :: Canvas
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

initTimeLogManyForm :: forall eff. Eff ( dom :: DOM , ref :: Ref | eff ) Unit
initTimeLogManyForm = do
  t <- allTimeInputs
  initTimeLogRows t
  listTop <- J.select "#timeLogMany\\.rows"
  flip (J.on "addListRow") listTop $ \ _ obj -> do
    newInputs <- timeInputs obj
    initTimeLogRows newInputs
    recalculate

  recalculateOn "deleteListRow" listTop
  return unit

allTimeInputs :: forall eff. Eff ( dom :: DOM , ref :: Ref | eff ) TimeInputs
allTimeInputs = J.body >>= timeInputs

timeInputs :: forall eff. J.JQuery -> Eff ( dom :: DOM , ref :: Ref | eff ) TimeInputs
timeInputs parent = do
  hs <- J.find ".log-hour" parent
  ms <- J.find ".log-minute" parent
  pure {hours: hs,mins: ms}

initTimeLogRows :: forall eff. TimeInputs -> Eff ( dom :: DOM , ref :: Ref | eff ) Unit
initTimeLogRows tis = do
  recalculateOn "change" tis.hours
  recalculateOn "change" tis.mins
  return unit

recalculateOn
  :: forall eff. String -> J.JQuery -> Eff ( dom :: DOM , ref :: Ref | eff ) J.JQuery
recalculateOn event elt = flip (J.on event) elt $ \ _ _ -> recalculate

recalculate
  :: forall eff
   . Eff ( dom :: DOM , ref :: Ref | eff ) Unit
recalculate = do
  tis       <- allTimeInputs
  totalMins <- inputMinutes tis
  let newHours = floor (totalMins / 60)
  let newMins  = totalMins % 60
  he <- J.select "#log-total-hours"
  me <- J.select "#log-total-minutes"
  J.setText (zeroPad newHours) he
  J.setText (zeroPad newMins) me
  return unit
  where
    inputMinutes tis = do
      ref <- newRef 0
      addInputs ref tis.hours ((*) 60)
      addInputs ref tis.mins  id
      readRef ref

    addInputs ref inputs f =
      jqEach inputs $ \ _ ob -> do
        n <- readNum <$> J.getValue ob
        modifyRef ref ((+) (f n))


    readNum f = fromMaybe 0 $ (hush $ readString f) >>= stringToNumber
    hush e = either (const Nothing) Just e
    zeroPad x = (if x < 10 then "0" else "") ++ show x

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

foreign import jqEach
  """
  function jqEach (ob) {
    return function (f) {
      return function () {
        return ob.each(
          function (i) {
            return f(i)($(this))();
          }
        );
      }
    }
  }
  """ :: forall eff
       . J.JQuery
      -> (Number -> J.JQuery -> Eff ( dom :: DOM | eff ) Unit)
      -> Eff ( dom :: DOM | eff ) J.JQuery
