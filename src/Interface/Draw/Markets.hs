module Interface.Draw.Markets where

import Brick.Types           (Padding (Max), ViewportType (Vertical), Widget)
import Brick.Widgets.Border  (borderWithLabel)
import Brick.Widgets.Center  (hCenter)
import Brick.Widgets.Core    (fill, hBox, hLimit, padLeftRight, padRight, str, txt, vBox, vLimit, viewport, withAttr,
                              (<=>))
import Brick.Widgets.List    (list, renderList)
import Core.Coins            (Exchange, ProductId)
import Core.Types            (Market (Market), Markets, Price, Size)
import Data.List             (intersperse)
import Data.Map              (elems, lookupMax, lookupMin)
import Data.Maybe            (mapMaybe)
import Data.Scientific       (FPFormat (Fixed), formatScientific, toRealFloat)
import Data.Text             (append, pack, replace)
import Data.Time             (UTCTime)
import Data.Vector           (fromList)
import Interface.Draw.Common (focusHighlight, formatTimeGMT8)
import Interface.State       (State, Tab (Mkts), X (MarketsList, MarketsViewport), markets)
import Interface.Theme       (blueText, cyanHeader, greenText, magentaText)
import Text.Printf           (printf)

data Snapshot = Snapshot
    { snapshotProductId :: ProductId
    , snapshotExchange  :: Exchange
    , snapshotBidPrice  :: Price
    , snapshotBidSize   :: Size
    , snapshotAskPrice  :: Price
    , snapshotAskSize   :: Size
    , snapshotSpread    :: Double
    , snapshotUpdated   :: UTCTime
    }

getSnapshots_ :: Markets -> [Snapshot]
getSnapshots_ markets = mapMaybe marketToSnapshot_ $ elems markets

marketToSnapshot_ :: Market -> Maybe Snapshot
marketToSnapshot_ (Market pid exc time bids asks) = do
    (askPrice, askSize) <- lookupMin asks
    (bidPrice, bidSize) <- lookupMax bids
    return Snapshot {
        snapshotProductId = pid
      , snapshotExchange = exc
      , snapshotBidPrice = bidPrice
      , snapshotBidSize = bidSize
      , snapshotAskPrice = askPrice
      , snapshotAskSize = askSize
      , snapshotSpread = (toRealFloat askPrice - toRealFloat bidPrice) / toRealFloat bidPrice
      , snapshotUpdated = time
    }

drawMarkets :: State -> Widget X
drawMarkets s = borderWithLabel (focusHighlight s Mkts $ txt "Crypto Markets") $ hCenter filledWidget
    where   filledWidget = vBox [header, viewport MarketsViewport Vertical $ vLimit height $ renderList renderSingleSS False l]
            height =  if null ss then 1 else length ss * 2
            l = list MarketsList (fromList ss) 1
            ss = getSnapshots_ $ markets s

            renderSingleSS _ snapshot = vLimit 2 $ padLeftRight 1 $ hCenter $
                hBox (intersperse (txt "  ") [
                    withAttr magentaText $ hLimit 10 $ padRight Max $ txt $ replace "_" "/" . pack . show $ snapshotProductId snapshot
                  , withAttr greenText $ hLimit 10 $ str $ formatScientific Fixed (Just 8) $ snapshotAskPrice snapshot
                  , withAttr greenText $ hLimit 10 $ hCenter $ str $ formatScientific Fixed (Just 8) $ snapshotBidPrice snapshot
                  , hLimit 5 $ padRight Max $ str $ printf "%.2f%%" $ snapshotSpread snapshot * 100
                  , hLimit 8 $ str $ formatTimeGMT8 $ snapshotUpdated snapshot
                  ])
                <=>
                hBox (intersperse (txt "  ") [
                    hLimit 10 $ padRight Max $ txt $ "  " `append` (pack . show $ snapshotExchange snapshot)
                  , withAttr blueText $ hLimit 10 $ str $ formatScientific Fixed (Just 8) $ snapshotAskSize snapshot
                  , withAttr blueText $ hLimit 10 $ str $ formatScientific Fixed (Just 8) $ snapshotBidSize snapshot
                  , fill ' '
                  ])

            header = withAttr cyanHeader $ padLeftRight 1 $ hCenter $
                hBox $ intersperse (txt "  ") [
                    hLimit 10 $ hCenter $ txt "Pair|Exch"
                  , hLimit 10 $ hCenter $ txt "AskP|AskQ"
                  , hLimit 10 $ hCenter $ txt "BidP|BidQ"
                  , hLimit 6 $ hCenter $ txt "Sprd"
                  , hLimit 8 $ hCenter $ txt "Updated"
                  ]
