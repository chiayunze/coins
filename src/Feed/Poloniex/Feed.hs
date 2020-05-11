module Feed.Poloniex.Feed where

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
import Feed.Poloniex.Types     (PoloniexFeedMessage (PoloniexFeedMessage), feedMessageToMarket)
import Network.HTTP.Client     (httpLbs, newManager, parseRequest, responseBody, setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)

feed :: TQueue Market -> TQueue ExceptionMessage -> ProductId -> IO ()
feed feedQueue exceptQueue pid = do
    manager <- newManager tlsManagerSettings
    request' <- parseRequest "GET https://poloniex.com/public"

    let request = setQueryString [
            ("currencyPair", Just $ encodeUtf8 . mapPid $ pid)
          , ("command", Just "returnOrderBook")
          , ("depth", Just "5")
          ] request'

        retrieveOnce = do
            response <- httpLbs request manager
            let result = eitherDecode . responseBody $ response :: Either String PoloniexFeedMessage
            case result of

                Right feedMsg@(PoloniexFeedMessage _ _ False) -> do
                    time <- getCurrentTime
                    let market = feedMessageToMarket pid time feedMsg
                    atomically $ writeTQueue feedQueue market

                Right (PoloniexFeedMessage _ _ True) -> throwString "market is frozen"

                Left errorMsg -> throwString errorMsg

    forever $ do
        forkIO $ logException exceptQueue "PoloniexFeed" retrieveOnce
        threadDelay $ 1 * 10^7

mapPid :: ProductId -> Text
mapPid pid = case pid of
    ETH_BTC  -> "BTC_ETH"
    BTC_USDT -> "USDT_BTC"
    ETH_USDT -> "USDT_ETH"
    _        -> ""
