module Feed.Coinbase.Feed where

import Control.Concurrent      (forkIO, threadDelay)
import Control.Concurrent.STM  (TQueue, atomically, writeTQueue)
import Control.Exception.Safe  (throwString)
import Control.Monad           (forever)
import Core.Coins              (ProductId (..))
import Core.Exceptions         (ExceptionMessage, logException)
import Core.Types              (Market)
import Data.Aeson              (eitherDecode)
import Data.Time               (getCurrentTime)
import Feed.Coinbase.Types     (CoinbaseFeedMessage, feedMessageToMarket)
import Network.HTTP.Client     (httpLbs, newManager, parseRequest, responseBody, setQueryString)
import Network.HTTP.Client.TLS (tlsManagerSettings)

feed :: TQueue Market -> TQueue ExceptionMessage -> ProductId -> IO ()
feed feedQueue exceptQueue pid = do
    manager <- newManager tlsManagerSettings
    request' <- parseRequest $ "GET https://api.pro.coinbase.com/products/" ++ mapPid pid ++ "/book"

    let request = setQueryString [
            ("level", Just "2")
          ] request'

        retrieveOnce = do
            response <- httpLbs request manager
            let result = eitherDecode . responseBody $ response :: Either String CoinbaseFeedMessage
            case result of

                Right feedMsg -> do
                    time <- getCurrentTime
                    let market = feedMessageToMarket pid time feedMsg
                    atomically $ writeTQueue feedQueue market

                Left errorMsg -> throwString errorMsg

    forever $ do
        forkIO $ logException exceptQueue "CoinbaseFeed" retrieveOnce
        threadDelay $ 1 * 10^7

mapPid :: ProductId -> String
mapPid pid = case pid of
    ETH_BTC -> "ETH-BTC"
    BTC_USD -> "BTC-USD"
    ETH_USD -> "ETH-USD"
    _       -> ""
