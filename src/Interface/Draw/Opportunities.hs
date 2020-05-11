module Interface.Draw.Opportunities where

import Brick.Types           (Padding (Max), Widget)
import Brick.Widgets.Border  (borderWithLabel)
import Brick.Widgets.Center  (hCenter)
import Brick.Widgets.Core    (hBox, hLimit, padLeft, padLeftRight, padRight, str, txt, vBox, withAttr)
import Brick.Widgets.List    (list, renderList)
import Core.Types            (opportunityCapitalBTC, opportunityMargin, opportunityProfitable, opportunityRoute,
                              opportunitySizeBTC, opportunitySizeBTC', opportunityTime, routeNumber, routeTracked)
import Data.List             (intersperse)
import Data.Scientific       (FPFormat (Fixed), formatScientific)
import Data.Text             (pack)
import Data.Vector           (fromList)
import Interface.Draw.Common (formatTimeGMT8)
import Interface.State       (State, X (OpportunitiesList), opportunities)
import Interface.Theme       (blueText, cyanHeader, dblueText, magentaText, yellowText)
import Text.Printf           (printf)

drawOpportunities :: State -> Widget X
drawOpportunities s = borderWithLabel (txt "Opportunities") $ hCenter filledWidget
    where   filledWidget = vBox [header, renderList renderSingleOpp False l]
            opps = opportunities s
            l = list OpportunitiesList (fromList opps) 1

            flashYellow o x = if opportunityProfitable o then withAttr yellowText else withAttr x
            flashYellowDefault o = if opportunityProfitable o then withAttr yellowText else id
            fadedBlackDefault o = if not (routeTracked . opportunityRoute $ o) then withAttr dblueText else id

            renderSingleOpp _ o = padLeftRight 1 $ hCenter $ hBox $ intersperse (txt "  ") [
                flashYellowDefault o $ fadedBlackDefault o $ hLimit 2 $ padRight Max $ txt $ pack . show $ routeNumber . opportunityRoute $ o
              , flashYellow o magentaText $ fadedBlackDefault o $ hLimit 1 $ if routeTracked . opportunityRoute $ o then (if opportunityProfitable o then txt "Y" else txt "N") else txt "-"
              , flashYellowDefault o $ fadedBlackDefault o $ hLimit 6 $ padLeft Max $ str $ printf "%.2f%%" $ opportunityMargin o * 100
              , flashYellow o blueText $ fadedBlackDefault o $ hLimit 8 $ hCenter $ str $ formatScientific Fixed (Just 6) $ opportunitySizeBTC o
              , flashYellow o blueText $ fadedBlackDefault o $ hLimit 8 $ hCenter $ str $ formatScientific Fixed (Just 6) $ opportunitySizeBTC' o
              , flashYellow o magentaText $ fadedBlackDefault o $ hLimit 6 $ hCenter $ str $ formatScientific Fixed (Just 4) $ opportunityCapitalBTC o
              , flashYellowDefault o $ fadedBlackDefault o $ hLimit 8 $ hCenter $ str $ formatTimeGMT8 $ opportunityTime o
              ]

            header = withAttr cyanHeader $ padLeftRight 1 $ hCenter $ hBox $ intersperse (txt "  ") [
                hLimit 2 $ padRight Max $ txt "R"
              , hLimit 1 $ txt "$"
              , hLimit 6 $ hCenter $ txt "NetMg"
              , hLimit 8 $ hCenter $ txt "Q/QtCn"
              , hLimit 8 $ hCenter $ txt "Q'/QtCn"
              , hLimit 6 $ hCenter $ txt "C/QtCn"
              , hLimit 8 $ hCenter $ txt "Updated"
              ]
