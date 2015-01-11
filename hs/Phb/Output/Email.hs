{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Phb.Output.Email where

import BasePrelude
import Prelude     ()

import           Control.Lens                hiding (Action, pre)
import           Data.Map                    (Map, fromList)
import qualified Data.Map                    as M
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Lens
import           Data.Time
import           System.Locale               (defaultTimeLocale)
import           Text.Blaze.Html5            (Html, toHtml, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Phb.Types.Action
import Phb.Types.Backlog
import Phb.Types.Event
import Phb.Types.Heartbeat
import Phb.Types.Project
import Phb.Types.Success

type Style = Map String String

black,white :: String
black     = "#222222"
white     = "#FFFFFF"

borderWidthPx,paddingWidthPx :: Int
borderWidthPx  = 1
paddingWidthPx = 5

darkBlue,lightBlue,red,amber,green,blackBorder,font,header
  ,centered,padding,gray,fixedTable,breakWord :: Style
darkBlue    = fromList [("background-color","#031066"),("color",white)]
lightBlue   = fromList [("background-color","#99CCFF")]
red         = fromList [("background-color","#FD3333")]
amber       = fromList [("background-color","#FFFF99")]
green       = fromList [("background-color","#93F562")]
gray        = fromList [("background-color","#BBBBBB")]
font        = fromList [("font-family","Verdana, sans-serif"),("color",black),("line-height","1.3em"),("font-size","11pt")]
header      = fromList [("font-weight","normal"),("line-height","1.4em"),("font-size","14pt")]
centered    = fromList [("text-align","center")]
padding     = fromList [("padding",show paddingWidthPx <> "px")]
fixedTable  = fromList [("table-layout","fixed")]
breakWord   = fromList [("word-wrap","normal")]
blackBorder = fromList
  [("border-color",black)
  ,("border-style","solid")
  ,("border-width",show borderWidthPx <> "px")
  ,("border-spacing","0")
  ,("border-collapse","collapse")
  ]

styleAttribute :: Style -> H.Attribute
styleAttribute = A.style . H.toValue . intercalate ";" . fmap styleLine . M.toList
  where
    styleLine (n,v) = n <> ": " <> v

pageWidth :: Int
pageWidth = 1600

pageWidthPercent :: Int -> Int
pageWidthPercent p =
  round (fromIntegral (pageWidth * p) / 100 :: Double) - (2 * paddingWidthPx) - borderWidthPx

widthPercent :: Int -> H.Attribute
widthPercent = A.width . H.toValue . pageWidthPercent

table,td :: Style -> Html -> Html
table ss = H.table ! widthPercent 100 ! styleAttribute (fold [blackBorder,ss,fixedTable,breakWord])
td    ss = H.td ! styleAttribute (fold [blackBorder,ss,font,centered,padding,breakWord])

ltd,th :: Int -> Style -> Html -> Html
ltd p ss = H.td ! widthPercent p ! styleAttribute (fold [blackBorder,ss,font,padding])
th w ss  = H.th
  ! widthPercent w
  ! styleAttribute (fold [blackBorder,ss,font,header,padding,centered])

pre :: Html -> Html
pre = H.pre ! styleAttribute font

sectionTitleRow :: Text -> Html
sectionTitleRow t = H.tr
  (th 100 darkBlue $ toHtml t)

sectionHeadersRow :: [(Text,Int)] -> Html
sectionHeadersRow =
  H.tr . mapM_ (\ (t,w) -> th w lightBlue (toHtml t))

sectionTable :: Text -> [(Text,Int)] -> [a] -> (a -> [(Style,Html)]) -> Html
sectionTable title hdrs rows f = do
  let cols = length hdrs
  let wSum = sum . fmap snd $ hdrs
  when (100 /= wSum) $
    error $ fold
      ["Percentages add up to ",show wSum," for ",T.unpack title]
  table mempty $ sectionTitleRow title
  table mempty $ do
    sectionHeadersRow hdrs
    when (null rows) $ do
      H.tr $ td mempty ! A.colspan (H.toValue cols) $ "None"
    mapM_ (H.tr . mapM_ (uncurry td) . f) rows

noStyle :: Html -> (Style,Html)
noStyle = (mempty,)

heartbeatHtml :: Heartbeat -> Html
heartbeatHtml h =
  sequence_ $
    [ hdr
    , h ^. heartbeatSuccesses . to successesHtml
    , h ^. heartbeatProjects . to projectsHtml
    , h ^. heartbeatBacklogs . to backlogsHtml
    , h ^. heartbeatEvents . to eventsHtml
    , h ^. heartbeatActions . to actionsHtml
    ]
  where
    hdr = do
      table mempty $ sectionTitleRow "Development Team Heartbeat"
      table mempty $ do
        hdrRow "Date" (toHtml . formatDay $ h ^. heartbeatDay)
        hdrRow "Upcoming Events"  (h ^. heartbeatUpcoming . to textListHtml)
        hdrRow "Special Mentions" (h ^. heartbeatHighlights . to textListHtml)
    hdrRow l d = H.tr $ (ltd 30 mempty l) >> (ltd 70 mempty d)

successesHtml :: [Success] -> Html
successesHtml ss =
  sectionTable "Successes"
  [ ("Who",10)
  , ("What",25)
  , ("Achievements",65)
  ] ss $ \ s ->
    [ s^.successWho . to textListHtml . to noStyle
    , s^.successWhat . to toHtml . to noStyle
    , s^.successAchievements . to textListHtml . to noStyle
    ]

projectsHtml :: [Project] -> Html
projectsHtml pp = sectionTable "Projects"
  [ ("Project",15)
  , ("Customer",10)
  , ("Status",10)
  , ("Stakeholders",12)
  , ("Target Completion",15)
  , ("Effort / Duration",8)
  , ("Notes / Next Steps",30)
  ] pp $ \ p ->
    [ p^.projectName . to toHtml . to noStyle
    , p^.projectCustomers . to textListHtml . to noStyle
    , p^.projectStatus . to projectStatusHtml
    , p^.projectStakeholders . to textListHtml . to noStyle
    , noStyle $ mapM_ targetDateHtml (p^.projectTargetDates)
    , p^.projectEffortSpent . to effortSpentHtml . to noStyle
    , p^.projectNotes . to textListHtml . to noStyle
    ]
  where
    projectStatusHtml ps =
      (ps ^. projectStatusColour . to projectStatusColourStyle,)
      . H.p $ do
        (ps ^. projectStatusPhase . to toHtml)
        forM_ (ps ^. projectStatusDesc) $ \ d -> do
          H.br
          toHtml ("(" <> d <> ")")
    targetDateHtml t = H.p . toHtml $ do
      (t ^. targetDateDay . to formatDay) <> (t ^. targetDateDesc . unpacked)

    effortSpentHtml es = H.p $ do
      toHtml $ (es ^. effortSpentDays . to show) <> " Person Days"
      H.br
      void "Started: "
      es ^. effortSpentStartDay . to formatDay . to toHtml

    projectStatusColourStyle StatusGreen = green
    projectStatusColourStyle StatusAmber = amber
    projectStatusColourStyle StatusRed   = red

backlogsHtml :: [Backlog] -> Html
backlogsHtml bb = sectionTable "Notable Backlog"
  [ ("Project",10)
  , ("Customer",10)
  , ("Status",10)
  , ("Stakeholders",15)
  , ("Notes",55)
  ] bb $ \ b ->
    [ b^.backlogName . to toHtml . to noStyle
    , b^.backlogCustomers . to textListHtml . to noStyle
    , b^.backlogStatus . to backlogStatusHtml
    , b^.backlogStakeholders . to textListHtml . to noStyle
    , b^.backlogNotes . to textListHtml . to noStyle
    ]
  where
    backlogStatusHtml BacklogScoped = (green,"Scoped")
    backlogStatusHtml BacklogNeedsScoping = (amber,"Needs Scoping")
    backlogStatusHtml BacklogInCommercials = (gray,"In Commercial Discussions")


eventsHtml :: [Event] -> Html
eventsHtml ee = sectionTable "Escalations / Support Events"
  [ ("Escalation or Event",10)
  , ("Customer",10)
  , ("What Happened",35)
  , ("Client Impact",10)
  , ("Duration",10)
  , ("SLA Met",5)
  , ("Notes / Next Steps",20)
  ] ee $ \ e ->
    [ e ^. eventName . to toHtml . to noStyle
    , e ^. eventCustomers . to textListHtml . to noStyle
    , e ^. eventDesc . to textListHtml . to noStyle
    , e ^. eventImpact . to textListHtml . to noStyle
    , e ^. eventDuration . to eventDurationHtml . to noStyle
    , e ^. eventStatus . to eventStatusHtml
    , e ^. eventNotes . to textListHtml . to noStyle
    ]
  where
    eventStatusHtml EventSlaMet     = (green,"Yes")
    eventStatusHtml EventSlaJustMet = (amber,"Barely")
    eventStatusHtml EventSlaNotMet  = (red,"No")
    eventDurationHtml (s,e) = do
      toHtml $ formatLocalTime s
      H.br
      toHtml $ formatLocalTime e

actionsHtml :: [Action] -> Html
actionsHtml aa = sectionTable "Actions"
  [ ("Who",10)
  , ("Customer",10)
  , ("Status",10)
  , ("Due",10)
  , ("Action Detail",60)
  ] aa $ \ a ->
    [ a ^. actionPeople . to textListHtml . to noStyle
    , a ^. actionCustomers . to textListHtml . to noStyle
    , a ^. actionStatus . to toHtml . to noStyle
    , a ^. actionDue . to (toHtml . formatDay) . to noStyle
    , a ^. actionNotes . to textListHtml . to noStyle
    ]

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%F"

formatLocalTime :: LocalTime -> String
formatLocalTime = formatTime defaultTimeLocale "%F %H:%M"

textListHtml :: [Text] -> Html
textListHtml = mapM_ ((>> H.br) . toHtml)
