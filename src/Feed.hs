module Feed where

import           Control.Concurrent       (forkIO)
import           Control.Concurrent.Async (mapConcurrently_)
import           Control.Concurrent.STM   (TMVar, TQueue, atomically, newTMVar, newTQueue, putTMVar, readTQueue,
                                           takeTMVar)
import           Control.Monad            (forever)
import           Core.Coins               (ProductId (..))
import           Core.Exceptions          (ExceptionMessage)
import           Core.Types               (Market, Markets, marketExchange, marketProductId)
import           Data.Map                 (empty, insert)
import qualified Feed.Binance.Feed        as Binance (feed)
import qualified Feed.Bittrex.Feed        as Bittrex (feed)
import qualified Feed.Coinbase.Feed       as Coinbase (feed)
import qualified Feed.Poloniex.Feed       as Poloniex (feed)

processFeedQueueMessage :: TQueue Market -> TMVar Markets -> IO ()
processFeedQueueMessage feedQueue marketsVar = atomically $ do
    nextMsg <- readTQueue feedQueue
    existing <- takeTMVar marketsVar
    let new = insert (marketProductId nextMsg, marketExchange nextMsg) nextMsg existing
    putTMVar marketsVar new

initFeed :: TQueue ExceptionMessage -> IO (TMVar Markets)
initFeed exceptQueue = do
    (feedQueue, marketsVar) <- atomically $ do
        queue <- newTQueue
        var <- newTMVar empty
        return (queue, var)
    forkIO $ forever $ processFeedQueueMessage feedQueue marketsVar
    forkIO $ mapConcurrently_ (Binance.feed feedQueue exceptQueue) [BTC_USDT, ETH_USDT, ETH_BTC]
    forkIO $ mapConcurrently_ (Bittrex.feed feedQueue exceptQueue) [BTC_USD, BTC_USDT, ETH_USD, ETH_USDT, ETH_BTC]
    forkIO $ mapConcurrently_ (Coinbase.feed feedQueue exceptQueue) [BTC_USD, ETH_USD, ETH_BTC]
    forkIO $ mapConcurrently_ (Poloniex.feed feedQueue exceptQueue) [BTC_USDT, ETH_USDT, ETH_BTC]
    return marketsVar
