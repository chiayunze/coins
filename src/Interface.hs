module Interface where

import Brick.BChan            (newBChan, writeBChan)
import Brick.Main             (App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent),
                               customMain, neverShowCursor)
import Control.Concurrent     (forkIO, threadDelay)
import Control.Concurrent.STM (TMVar)
import Control.Monad          (forever)
import Core.Exceptions        (ExceptionMessage)
import Core.Types             (Balances, Markets, Opportunity, Settings, Transaction)
import Graphics.Vty           (defaultConfig, mkVty)
import Interface.Draw         (drawUI)
import Interface.Event        (handleEvent)
import Interface.State        (Display, State, X, getState)
import Interface.Theme        (colourMap)

app :: TMVar Markets -> TMVar Balances -> TMVar Settings -> TMVar [Opportunity] -> TMVar [Transaction] -> TMVar [ExceptionMessage] -> TMVar Display -> App State () X
app m b s o t e d = App {
    appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent m b s o t e d
  , appStartEvent = return
  , appAttrMap = const colourMap
  }

render :: TMVar Markets -> TMVar Balances -> TMVar Settings -> TMVar [Opportunity] -> TMVar [Transaction] -> TMVar [ExceptionMessage] -> TMVar Display -> IO ()
render m b s o t e d = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan ()
        threadDelay $ 1 * 10^5
    state <- getState m b s o t e d
    vty <- mkVty defaultConfig
    customMain vty (mkVty defaultConfig) (Just chan) (app m b s o t e d) state
    return ()
