module Feed.Poloniex.Types where

import Core.Coins       (Exchange (Poloniex), ProductId)
import Core.Types       (Market (..), Price, Size)
import Data.Aeson       (FromJSON, Value (Object), parseJSON, (.:))
import Data.Aeson.Types (Parser)
import Data.Map         (fromAscList, fromDescList)
import Data.Text        (Text)
import Data.Time        (UTCTime)

-- https://docs.poloniex.com/#returnorderbook
data PoloniexFeedMessage = PoloniexFeedMessage
    { asks     :: [(Price, Size)]
    , bids     :: [(Price, Size)]
    , isFrozen :: Bool
    }

instance FromJSON PoloniexFeedMessage where
    parseJSON (Object o) = do
        isFrozen <- o .: "isFrozen" :: Parser Text

        let toScientific (x, y) = (read x, y)

        if isFrozen == "1"
            then return PoloniexFeedMessage { asks = [], bids = [], isFrozen = True }
            else do
                asks <- o .: "asks"
                bids <- o .: "bids"
                return PoloniexFeedMessage {
                    asks = map toScientific asks
                  , bids = map toScientific bids
                  , isFrozen = False
                }

feedMessageToMarket :: ProductId -> UTCTime -> PoloniexFeedMessage -> Market
feedMessageToMarket pid time (PoloniexFeedMessage asks bids False) = Market
    { marketProductId = pid
    , marketExchange = Poloniex
    , marketUpdated = time
    , marketBids = fromDescList bids
    , marketAsks = fromAscList asks
    }
