module Feed.Coinbase.Types where

import Core.Coins       (Exchange (Coinbase), ProductId)
import Core.Types       (Market (..), Price, Size)
import Data.Aeson       (FromJSON, Value (Object), parseJSON, (.:))
import Data.Aeson.Types (Parser)
import Data.Map         (fromAscList, fromDescList)
import Data.Time        (UTCTime)

-- https://docs.pro.coinbase.com/#get-product-order-book
data CoinbaseFeedMessage = CoinbaseFeedMessage
    { asks :: [(Price, Size)]
    , bids :: [(Price, Size)]
    }

instance FromJSON CoinbaseFeedMessage where
    parseJSON (Object o) = do

        let toScientific (x, y, _) = (read x, read y)

        bids <- o .: "bids" :: Parser [(String, String, Int)]
        asks <- o .: "asks" :: Parser [(String, String, Int)]
        return CoinbaseFeedMessage {
                    asks = map toScientific asks
                  , bids = map toScientific bids
                }

feedMessageToMarket :: ProductId -> UTCTime -> CoinbaseFeedMessage -> Market
feedMessageToMarket pid time (CoinbaseFeedMessage asks bids) = Market
    { marketProductId = pid
    , marketExchange = Coinbase
    , marketUpdated = time
    , marketBids = fromDescList bids
    , marketAsks = fromAscList asks
    }
