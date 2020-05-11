module Feed.Bittrex.Types where

import Core.Coins       (Exchange (Bittrex), ProductId)
import Core.Types       (Market (..), Price, Size)
import Data.Aeson       (FromJSON, Value (Object), parseJSON, (.:))
import Data.Aeson.Types (Object, Parser)
import Data.Map         (fromAscList, fromDescList)
import Data.Time        (UTCTime)

-- https://bittrex.github.io/api/v1-1 (GET /public/getorderbook)
data BittrexFeedMessage = BittrexFeedMessage
    { asks    :: [(Price, Size)]
    , bids    :: [(Price, Size)]
    , success :: Bool
    }

instance FromJSON BittrexFeedMessage where
    parseJSON (Object o) = do
        success <- o .: "success" :: Parser Bool

        let convert (BittrexOrderBook p s) = (p, s)

        if success
            then do
                result <- o .: "result" :: Parser Object
                bids   <- result .: "buy" :: Parser [BittrexOrderBook]
                asks   <- result .: "sell" :: Parser [BittrexOrderBook]
                return BittrexFeedMessage {
                    asks = map convert asks
                  , bids = map convert bids
                  , success = True
                }
            else return BittrexFeedMessage { asks = [], bids = [], success = False }

data BittrexOrderBook = BittrexOrderBook Price Size

instance FromJSON BittrexOrderBook where
    parseJSON (Object o) = do
        price   <- o .: "Rate"
        size    <- o .: "Quantity"
        return $ BittrexOrderBook price size

feedMessageToMarket :: ProductId -> UTCTime -> BittrexFeedMessage -> Market
feedMessageToMarket pid time (BittrexFeedMessage asks bids True) = Market
    { marketProductId = pid
    , marketExchange = Bittrex
    , marketUpdated = time
    , marketBids = fromDescList bids
    , marketAsks = fromAscList asks
    }
