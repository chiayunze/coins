module Interface.Event where

import Brick.Main             (continue, hScrollBy, halt, vScrollBy, viewportScroll)
import Brick.Types            (BrickEvent (AppEvent, VtyEvent), EventM, Next)
import Control.Concurrent.STM (TMVar, atomically, putTMVar, takeTMVar)
import Control.Monad.IO.Class (liftIO)
import Core.Exceptions        (ExceptionMessage)
import Core.Types             (Autopilot (APOff, APOn, APSemi), Balances, Markets, Opportunity, Settings, Transaction,
                               autopilot)
import Graphics.Vty           (Event (EvKey), Key (KChar, KDown, KLeft, KRight, KUp), Modifier (MCtrl))
import Interface.State        (Display (Display), State, Tab (Bals, Excs, Full, Mkts, Trsc), X (BalancesViewport, BlankViewport, ExceptionsHeaderViewport, ExceptionsViewport, MarketsViewport, TransactionsHeaderViewport, TransactionsViewport),
                               display, focus, getState, tab)

handleEvent :: TMVar Markets -> TMVar Balances -> TMVar Settings -> TMVar [Opportunity] -> TMVar [Transaction] -> TMVar [ExceptionMessage] -> TMVar Display -> State -> BrickEvent X () -> EventM X (Next State)
handleEvent m b s o t e d state (AppEvent ())                          = liftIO (getState m b s o t e d) >>= continue
handleEvent m b s o t e d state (VtyEvent (EvKey (KChar 'q') [MCtrl])) = halt state

handleEvent m b s o t e d state (VtyEvent (EvKey (KChar k) [])) = case k of
    'd' -> continue =<< liftIO (do
        toggleDisplayTab d
        getState m b s o t e d)
    'f' -> continue =<< liftIO (do
        setDisplayTabFull d
        getState m b s o t e d)
    's' -> continue =<< liftIO (do
        setAutopilotSemi s
        getState m b s o t e d)
    'a' -> continue =<< liftIO (do
        toggleAutopilot s
        getState m b s o t e d)
    '\t' -> continue =<< liftIO (do
        toggleDisplayFocus d
        getState m b s o t e d)
    _   -> continue state

handleEvent m b s o t e d state (VtyEvent (EvKey k [])) = do
    let selectViewport = case focus . display $ state of
            Trsc -> viewportScroll TransactionsViewport
            Excs -> viewportScroll ExceptionsViewport
            Mkts -> viewportScroll MarketsViewport
            Bals -> viewportScroll BalancesViewport
            _    -> viewportScroll BlankViewport
        selectHeaderViewport = case focus . display $ state of
            Trsc -> viewportScroll TransactionsHeaderViewport
            Excs -> viewportScroll ExceptionsHeaderViewport
            _    -> viewportScroll BlankViewport

    case k of
        KRight -> do
            hScrollBy selectViewport 1
            hScrollBy selectHeaderViewport 1
            continue state
        KLeft  -> do
            hScrollBy selectViewport (-1)
            hScrollBy selectHeaderViewport (-1)
            continue state
        KDown  -> do
            vScrollBy selectViewport 1
            vScrollBy selectHeaderViewport 1
            continue state
        KUp    -> do
            vScrollBy selectViewport (-1)
            vScrollBy selectHeaderViewport (-1)
            continue state
        _        -> continue state

handleEvent m b s o t e d state _ = continue state

setDisplayTabFull :: TMVar Display -> IO ()
setDisplayTabFull displayVar = atomically $ do
    d <- takeTMVar displayVar
    putTMVar displayVar d {tab = Full, focus = Mkts}

toggleDisplayTab :: TMVar Display -> IO ()
toggleDisplayTab displayVar = atomically $ do
    d <- takeTMVar displayVar
    let rotate d = case d of
            Display Full _ time -> Display Mkts Mkts time
            Display Mkts _ time -> Display Bals Bals time
            Display Bals _ time -> Display Trsc Trsc time
            Display Trsc _ time -> Display Excs Excs time
            Display Excs _ time -> Display Mkts Mkts time
    putTMVar displayVar (rotate d)

toggleDisplayFocus :: TMVar Display -> IO ()
toggleDisplayFocus displayVar = atomically $ do
    d <- takeTMVar displayVar
    let rotate f = case f of
            Display Full Mkts time -> Display Full Bals time
            Display Full Bals time -> Display Full Trsc time
            Display Full Trsc time -> Display Full Excs time
            Display Full Excs time -> Display Full Mkts time
            Display tab focus time -> Display tab focus time
    putTMVar displayVar (rotate d)

setAutopilotSemi :: TMVar Settings -> IO ()
setAutopilotSemi settingsVar = atomically $ do
    s <- takeTMVar settingsVar
    putTMVar settingsVar $ s {autopilot = APSemi}

toggleAutopilot :: TMVar Settings -> IO ()
toggleAutopilot settingsVar = atomically $ do
    s <- takeTMVar settingsVar
    let toggled = if autopilot s == APOff then APOn else APOff
    putTMVar settingsVar $ s {autopilot = toggled}
