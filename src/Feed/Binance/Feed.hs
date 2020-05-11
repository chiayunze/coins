module Feed.Binance.Feed where

import Control.Concurrent      (forkIO, threadDelay)
import Control.Concurrent.STM  (TQueue, atomically, writeTQueue)
import Control.Exception.Safe  (throwString)
import Control.Monad           (forever)
import Core.Coins              (ProductId (..))
import Core.Exceptions         (ExceptionMessage, logException)
import Core.Types              (Market)
import Data.Aeson              (eitherDecode)
import Data.Text               (Text)
import Data.Text.Encoding      (encodeUtf8)
import Data.Time               (getCurrentTime)
import Feed.Binance.Types      (BinanceFeedMessage, feedMessageToMarket)
import Network.HTTP.Client     (httpLbs, newManager, parseRequest, responseBody, setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)

feed :: TQueue Market -> TQueue ExceptionMessage -> ProductId -> IO ()
feed feedQueue exceptQueue pid = do
    manager <- newManager tlsManagerSettings
    request' <- parseRequest "GET https://api.binance.com/api/v3/depth"

    let request = setQueryString [
            ("symbol", Just $ encodeUtf8 . mapPid $ pid)
          , ("limit", Just "5")
          ] request'

        retrieveOnce = do
            response <- httpLbs request manager
            let result = eitherDecode . responseBody $ response :: Either String BinanceFeedMessage
            case result of

                Right feedMsg -> do
                    time <- getCurrentTime
                    let market = feedMessageToMarket pid time feedMsg
                    atomically $ writeTQueue feedQueue market

                Left errorMsg -> throwString errorMsg

    forever $ do
        forkIO $ logException exceptQueue "PoloniexFeed" retrieveOnce
        threadDelay $ 1 * 10^7

mapPid :: ProductId -> Text
mapPid pid = case pid of
    ETH_BTC  -> "ETHBTC"
    BTC_USDT -> "BTCUSDT"
    ETH_USDT -> "ETHUSDT"
    _        -> ""
