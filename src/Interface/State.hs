module Interface.State where

import Control.Concurrent.STM (TMVar, atomically, newTMVar, readTMVar)
import Core.Exceptions        (ExceptionMessage)
import Core.Types             (Balances, Markets, Opportunity, Settings, Transaction)
import Data.Time              (UTCTime, getCurrentTime)

data Display = Display
    { tab   :: Tab
    , focus :: Tab
    , time  :: UTCTime
    }

data Tab = Full | Mkts | Trsc | Bals | Excs deriving (Eq)

initDisplayVar :: IO (TMVar Display)
initDisplayVar = do
    time <- getCurrentTime
    atomically $ newTMVar $ Display Full Mkts time

data State = State
    { markets       :: Markets
    , opportunities :: [Opportunity]
    , transactions  :: [Transaction]
    , balances      :: Balances
    , settings      :: Settings
    , exceptions    :: [ExceptionMessage]
    , display       :: Display
    }

getState :: TMVar Markets -> TMVar Balances -> TMVar Settings -> TMVar [Opportunity] -> TMVar [Transaction] -> TMVar [ExceptionMessage] -> TMVar Display -> IO State
getState marketsVar balancesVar settingsVar opportunitiesVar transactionsVar exceptVar displayVar = do
    (m, b, s, o, t, e, d) <- atomically $ do
        m <- readTMVar marketsVar
        b <- readTMVar balancesVar
        s <- readTMVar settingsVar
        o <- readTMVar opportunitiesVar
        t <- readTMVar transactionsVar
        e <- readTMVar exceptVar
        d <- readTMVar displayVar
        return (m, b, s, o, t, e, d)
    time <- getCurrentTime
    return $ State {
        markets = m
      , opportunities = o
      , transactions = t
      , balances = b
      , settings = s
      , exceptions = e
      , display = d {time = time}
    }

data X = MarketsList
       | MarketsViewport
       | BalancesList
       | BalancesViewport
       | OpportunitiesList
       | TransactionsHeaderViewport
       | TransactionsList
       | TransactionsViewport
       | ExceptionsList
       | ExceptionsViewport
       | ExceptionsHeaderViewport
       | BlankViewport
       deriving (Eq, Ord, Show)
