module Interface.Draw.Transactions where

import Brick.Types           (Padding (Max), ViewportType (Both, Horizontal), Widget)
import Brick.Widgets.Border  (borderWithLabel)
import Brick.Widgets.Center  (hCenter)
import Brick.Widgets.Core    (hBox, hLimit, padLeftRight, padRight, setAvailableSize, str, txt, txtWrap, vBox, vLimit,
                              viewport, withAttr)
import Brick.Widgets.List    (list, renderList)
import Core.Types            (Side (Buy, Sell), tradeExchange, tradePrice, tradeProductId, tradeSide, tradeSize,
                              transactionDetails, transactionResponse, transactionTime)
import Data.List             (intersperse)
import Data.Scientific       (FPFormat (Fixed), formatScientific)
import Data.Text             (pack, replace)
import Data.Vector           (fromList)
import Interface.Draw.Common (focusHighlight, formatTimeGMT8)
import Interface.State       (State, Tab (Trsc), X (TransactionsHeaderViewport, TransactionsList, TransactionsViewport),
                              transactions)
import Interface.Theme       (cyanHeader, greenText, magentaText, yellowText)

drawTransactions :: State -> Widget X
drawTransactions s = borderWithLabel (focusHighlight s Trsc $ txt "Transactions") filledWidget
    where   filledWidget = vBox [
                vLimit 1 $ viewport TransactionsHeaderViewport Horizontal $ hLimit 250 header
              , viewport TransactionsViewport Both $ setAvailableSize (250, height) $ renderList renderSingleTransaction False l
              ]
            height =  if null txns then 1 else length txns
            txns = transactions s
            l = list TransactionsList (fromList txns) 1

            renderSingleTransaction _ t = vLimit 1 $ padLeftRight 1 $ hBox $ intersperse (txt "  ") [
                hLimit 8 $ str $ formatTimeGMT8 $ transactionTime t
              , withAttr magentaText $ hLimit 8 $ padRight Max $ txt $ replace "_" "/" . pack . show $ tradeProductId . transactionDetails $ t
              , hLimit 1 $ padRight Max $ txt $ renderOrderSide $ tradeSide . transactionDetails $ t
              , withAttr yellowText $ hLimit 6 $ str $ formatScientific Fixed (Just 4) $ tradeSize . transactionDetails $ t
              , withAttr greenText $ hLimit 10 $ padRight Max $ str $ formatScientific Fixed (Just 8) $ tradePrice . transactionDetails $ t
              , hLimit 8 $ padRight Max $ txt $ pack . show $ tradeExchange . transactionDetails $ t
              , padRight Max $ txtWrap $ transactionResponse t
              ]

            renderOrderSide side = case side of
                Buy  -> "B"
                Sell -> "S"

            header = withAttr cyanHeader $ padLeftRight 1 $ hCenter $ hBox $ intersperse (txt "  ") [
                hLimit 8 $ hCenter $ txt "Time"
              , hLimit 8 $ hCenter $ txt "Pair"
              , hLimit 1 $ hCenter $ txt "Side"
              , hLimit 6 $ hCenter $ txt "Amt"
              , hLimit 10 $ hCenter $ txt "Price"
              , hLimit 8 $ hCenter $ txt "Exch"
              , padRight Max $ txt "Resp"
              ]
