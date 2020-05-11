module Interface.Draw where

import Brick.Types                  (Padding (Max, Pad), Widget)
import Brick.Widgets.Center         (center)
import Brick.Widgets.Core           (hBox, hLimit, padBottom, padLeft, padLeftRight, padRight, setAvailableSize, str,
                                     txt, vLimit, withAttr, (<+>), (<=>))
import Core.Types                   (Autopilot (APOff, APOn, APSemi), autopilot)
import Data.Text                    (concat)
import Interface.Draw.Balances      (drawBalances)
import Interface.Draw.Common        (formatTimeGMT8)
import Interface.Draw.Exceptions    (drawExceptions)
import Interface.Draw.Markets       (drawMarkets)
import Interface.Draw.Opportunities (drawOpportunities)
import Interface.Draw.Transactions  (drawTransactions)
import Interface.State              (State, Tab (Bals, Excs, Full, Mkts, Trsc), X, display, settings, tab, time)
import Interface.Theme              (blackOnYellow, greenText, redText, yellowText)
import Prelude                      hiding (concat)

drawUI :: State -> [Widget X]
drawUI s = [center $ setSize s $ drawPane s]
    where   setSize s = case tab . display $ s of
                Full -> setAvailableSize (165, 40)
                _    -> setAvailableSize (55, 40)
            drawPane s = case tab . display $ s of
                Full -> drawHeader s <=> drawMainPane s <=> drawFooter s
                _    -> drawHeader s <=> drawMainPane s <=> drawFooter s

drawMainPane :: State -> Widget X
drawMainPane s = drawSelector s
    where   drawSelector s = case tab . display $ s of
                Mkts -> drawMarkets s
                Bals -> drawBalances s <=> drawOpportunities s
                Trsc -> drawTransactions s
                Excs -> drawExceptions s
                Full -> hBox $ map (setAvailableSize (55, 37)) [
                    drawMarkets s
                  , vLimit 12 (drawBalances s) <=> vLimit 11 (drawOpportunities s) <=> drawExceptions s
                  , drawTransactions s
                  ]

drawHeader :: State -> Widget X
drawHeader s = padLeft Max $ padLeftRight 1 $ padBottom (Pad 1) $ hBox [autopilotWidget1, autopilotWidget2, timeWidget]
    where   s' = settings s
            timeWidget = padLeft (Pad 2) $ str $ "Time: " ++ formatTimeGMT8 (time . display $ s)
            autopilotWidget1 = str "Autopilot: "
            autopilotWidget2 = hLimit 4 $ padRight Max $ case autopilot s' of
                APOn   -> withAttr greenText $ txt "ON"
                APOff  -> withAttr redText $ txt "OFF"
                APSemi -> withAttr yellowText $ txt "SEMI"

drawFooter :: State -> Widget X
drawFooter s = padRight Max $ padLeftRight 1 filledWidget
    where   filledWidget = hBox $ map renderIcon [
                ("F/D", "Displ")
              , ("A/S", "AutoP")
              , ("1-4", "Route")
              , ("Tab", "Focus")
              , ("^Q", "Quit")
              ]
            renderIcon (key, legend) =
                withAttr blackOnYellow (txt $ concat [" ", key, " "])
                <+>
                hLimit 7 (padRight Max $ txt $ concat [" ", legend, " "])
