module Feed.Bittrex.Feed where

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
import Feed.Bittrex.Types      (BittrexFeedMessage (BittrexFeedMessage), feedMessageToMarket)
import Network.HTTP.Client     (httpLbs, newManager, parseRequest, responseBody, setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)

feed :: TQueue Market -> TQueue ExceptionMessage -> ProductId -> IO ()
feed feedQueue exceptQueue pid = do
    manager <- newManager tlsManagerSettings
    request' <- parseRequest "GET https://bittrex.com/api/v1.1/public/getorderbook"

    let request = setQueryString [
            ("market", Just $ encodeUtf8 . mapPid $ pid)
          , ("type", Just "both")
          ] request'

        retrieveOnce = do
            response <- httpLbs request manager
            let result = eitherDecode . responseBody $ response :: Either String BittrexFeedMessage
            case result of

                Right feedMsg@(BittrexFeedMessage _ _ True) -> do
                    time <- getCurrentTime
                    let market = feedMessageToMarket pid time feedMsg
                    atomically $ writeTQueue feedQueue market

                Right (BittrexFeedMessage _ _ False) -> throwString "api returned error"

                Left errorMsg -> throwString errorMsg

    forever $ do
        forkIO $ logException exceptQueue "BittrexFeed" retrieveOnce
        threadDelay $ 1 * 10^7

mapPid :: ProductId -> Text
mapPid pid = case pid of
    ETH_BTC  -> "BTC-ETH"
    BTC_USD  -> "USD-BTC"
    ETH_USD  -> "USD-ETH"
    BTC_USDT -> "USDT-BTC"
    ETH_USDT -> "USDT-ETH"
    _        -> ""
