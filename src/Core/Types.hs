module Core.Types where

import Core.Coins      (Coin, Exchange, ProductId)
import Data.Map        (Map)
import Data.Scientific (Scientific)
import Data.Text       (Text)
import Data.Time       (UTCTime)

type Price = Scientific
type Size = Scientific

type Markets = Map (ProductId, Exchange) Market
data Market = Market
    { marketProductId :: ProductId
    , marketExchange  :: Exchange
    , marketUpdated   :: UTCTime
    , marketBids      :: Map Price Size
    , marketAsks      :: Map Price Size
    }

type Balances = Map (Coin, Exchange) Balance
data Balance = Balance
    { balanceCoin      :: Coin
    , balanceExchange  :: Exchange
    , balanceAvailable :: Size
    , balanceTotal     :: Size
    , balanceUpdated   :: UTCTime
    }

newtype Settings = Settings
    { autopilot    :: Autopilot
    }
data Autopilot = APOn | APOff | APSemi deriving (Eq)

data Opportunity = Opportunity
    { opportunityRoute          :: Route
    , opportunityMargin         :: Double
    , opportunitySizeBTC        :: Size
    , opportunitySizeBTC'       :: Size
    , opportunityCapitalBTC     :: Size
    , opportunityTime           :: UTCTime
    , opportunityProfitable     :: Bool
    , opportunityBalanceLimited :: Bool
    }
data Route = Route
    { routeNumber  :: Int
    , routeTracked :: Bool
    }

data Transaction = Transaction
    { transactionTime     :: UTCTime
    , transactionDetails  :: Trade
    , transactionResponse :: Text
    }
data Trade = Trade
    { tradeProductId :: ProductId
    , tradeExchange  :: Exchange
    , tradeSide      :: Side
    , tradeSize      :: Size
    , tradePrice     :: Price
    }
data Side = Buy | Sell
