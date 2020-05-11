module Core.Coins where

data ProductId =
    ETH_BTC | ETH_USD | ETH_USDT
  | BTC_USD | BTC_USDT
  | OtherPairs
  deriving (Eq, Ord, Show)

data Coin = ETH | BTC | USD | USDT | OtherCoins deriving (Show)

data Exchange = Binance | Bittrex | Poloniex | Coinbase deriving (Eq, Ord, Show)
