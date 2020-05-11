module Interface.Draw.Balances where

import Brick.Types           (ViewportType (Vertical), Widget)
import Brick.Widgets.Border  (borderWithLabel)
import Brick.Widgets.Center  (hCenter)
import Brick.Widgets.Core    (hBox, hLimit, padLeftRight, str, txt, vBox, vLimit, viewport, withAttr)
import Brick.Widgets.List    (list, renderList)
import Core.Types            (balanceAvailable, balanceCoin, balanceExchange, balanceTotal, balanceUpdated)
import Data.List             (intersperse)
import Data.Map              (elems)
import Data.Scientific       (FPFormat (Fixed), formatScientific)
import Data.Text             (pack)
import Data.Vector           (fromList)
import Interface.Draw.Common (focusHighlight, formatTimeGMT8)
import Interface.State       (State, Tab (Bals), X (BalancesList, BalancesViewport), balances)
import Interface.Theme       (blueText, cyanHeader, greenText, magentaText, redText)

drawBalances :: State -> Widget X
drawBalances s = borderWithLabel (focusHighlight s Bals $ txt "Balances") $ hCenter filledWidget
    where   filledWidget = vBox [header, viewport BalancesViewport Vertical $ vLimit height $ renderList renderSingleBalance False l]
            height =  if null bals then 1 else length bals
            bals = elems $ balances s
            l = list BalancesList (fromList bals) 1

            renderSingleBalance _ b = padLeftRight 1 $ hBox $ intersperse (txt "  ") [
                withAttr magentaText $ hLimit 4 $ hCenter $ txt $ pack . show $ balanceCoin b
              , hLimit 8 $ hCenter $ txt $ pack . show $ balanceExchange b
              , withAttr blueText $ hLimit 10 $ hCenter $ str $ formatScientific Fixed (Just 8) $ balanceAvailable b
              , withAttr blueText $ hLimit 10 $ hCenter $ str $ formatScientific Fixed (Just 8) $ balanceTotal b
              , hLimit 1 $ hCenter $ if balanceAvailable b == balanceTotal b then withAttr greenText $ txt "Y" else withAttr redText $ txt "N"
              , hLimit 8 $ hCenter $ str $ formatTimeGMT8 $ balanceUpdated b
              ]

            header = withAttr cyanHeader $ padLeftRight 1 $ hBox $ intersperse (txt "  ") [
                hLimit 4 $ hCenter $ txt "Coin"
              , hLimit 8 $ hCenter $ txt "Exch"
              , hLimit 10 $ hCenter $ txt "Avail"
              , hLimit 10 $ hCenter $ txt "Total"
              , hLimit 1 $ hCenter $ txt "="
              , hLimit 8 $ hCenter $ txt "Updated"
              ]
