module Feed.Binance.Types where

import Core.Coins       (Exchange (Binance), ProductId)
import Core.Types       (Market (..), Price, Size)
import Data.Aeson       (FromJSON, Value (Object), parseJSON, (.:))
import Data.Aeson.Types (Parser)
import Data.Map         (fromAscList, fromDescList)
import Data.Time        (UTCTime)

-- https://binance-docs.github.io/apidocs/testnet/en/#order-book
data BinanceFeedMessage = BinanceFeedMessage
    { asks :: [(Price, Size)]
    , bids :: [(Price, Size)]
    }

instance FromJSON BinanceFeedMessage where
    parseJSON (Object o) = do

        let toScientific (x, y) = (read x, read y)

        bids <- o .: "bids" :: Parser [(String, String)]
        asks <- o .: "asks" :: Parser [(String, String)]
        return BinanceFeedMessage {
                    asks = map toScientific asks
                  , bids = map toScientific bids
                }

feedMessageToMarket :: ProductId -> UTCTime -> BinanceFeedMessage -> Market
feedMessageToMarket pid time (BinanceFeedMessage asks bids) = Market
    { marketProductId = pid
    , marketExchange = Binance
    , marketUpdated = time
    , marketBids = fromDescList bids
    , marketAsks = fromAscList asks
    }
