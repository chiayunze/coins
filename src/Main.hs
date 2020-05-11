module Main where

import Control.Concurrent.STM (newTMVarIO)
import Core.Exceptions        (initExcept)
import Core.Types             (Autopilot (APOff), Settings (Settings))
import Data.Map               (empty)
import Feed                   (initFeed)
import Interface              (render)
import Interface.State        (initDisplayVar)

main :: IO ()
main = do
   (exceptQueue, exceptVar) <- initExcept
   marketsVar <- initFeed exceptQueue
   balancesVar <- newTMVarIO empty
   settingsVar <- newTMVarIO $ Settings APOff
   opportunitiesVar <- newTMVarIO []
   transactionsVar <- newTMVarIO []
   displayVar <- initDisplayVar
   render marketsVar balancesVar settingsVar opportunitiesVar transactionsVar exceptVar displayVar
