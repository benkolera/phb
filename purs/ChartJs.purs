module ChartJs where

import Control.Monad.Eff
import Data.Foreign
import Data.Maybe
import DOM
import Graphics.Canvas

foreign import data Chart :: *
foreign import data ChartType :: *

type DoughnutChartData =
  { value     :: Number
  , label     :: String
  , color     :: String
  , highlight :: String
  }

data DoughnutChartConfig = DoughnutChartConfig

foreign import newChart
  """
  function newChart (ctx) {
    return function () {
      return new Chart(ctx);
    }
  }
  """ :: forall eff. Context2D -> Eff ( dom :: DOM | eff ) Chart

-- There has to be a better way to unwrap the maybes.
foreign import doughnutChart
  """
  function doughnutChart (chart) {
    return function (data) {
      return function (config) {
        return function () {
          return chart.Doughnut(data,config);
        }
      }
    }
  }
  """ :: forall eff.
         Chart
      -> [DoughnutChartData]
      -> DoughnutChartConfig
      -> Eff (dom :: DOM | eff ) ChartType
