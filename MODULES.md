# Module Documentation

## Module ChartJs

### Types

    data Chart :: *

    data ChartType :: *

    data DoughnutChartConfig where
      DoughnutChartConfig :: DoughnutChartConfig

    type DoughnutChartData = { highlight :: String, color :: String, label :: String, value :: Number }


### Values

    doughnutChart :: forall eff. Chart -> [DoughnutChartData] -> DoughnutChartConfig -> Eff (dom :: DOM | eff) ChartType

    newChart :: forall eff. Context2D -> Eff (dom :: DOM | eff) Chart


## Module Phb

### Types

    type TimeInputs = { mins :: J.JQuery, hours :: J.JQuery }


### Values

    allTimeInputs :: forall eff. Eff (ref :: Ref, dom :: DOM | eff) TimeInputs

    heartbeatTimebreakdown :: forall eff. String -> [{ label :: String, value :: Number }] -> DoughnutChartConfig -> Eff (canvas :: Canvas, dom :: DOM, pleaseJs :: PleaseJs | eff) ChartType

    initTimeLogManyForm :: forall eff. Eff (ref :: Ref, dom :: DOM | eff) Unit

    initTimeLogRows :: forall eff. TimeInputs -> Eff (ref :: Ref, dom :: DOM | eff) Unit

    jqEach :: forall eff. J.JQuery -> (Number -> J.JQuery -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) J.JQuery

    recalculate :: forall eff. Eff (ref :: Ref, dom :: DOM | eff) Unit

    recalculateOn :: forall eff. String -> J.JQuery -> Eff (ref :: Ref, dom :: DOM | eff) J.JQuery

    stringToNumber :: String -> Maybe Number

    stringToNumberForeign :: (Number -> Maybe Number) -> Maybe Number -> String -> Maybe Number

    timeInputs :: forall eff. J.JQuery -> Eff (ref :: Ref, dom :: DOM | eff) TimeInputs


## Module PleaseJs

### Types

    data PleaseJs :: !


### Values

    makeColor :: forall eff. Eff (pleaseJs :: PleaseJs | eff) String


## Module TinyColor

### Types

    data TinyColor :: *


### Values

    lighten :: TinyColor -> TinyColor

    tinycolor :: String -> TinyColor

    toHex :: TinyColor -> String



