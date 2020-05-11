module Core.Exceptions where

import Control.Concurrent     (forkIO)
import Control.Concurrent.STM (TMVar, TQueue, atomically, newTMVar, newTQueue, putTMVar, readTQueue, takeTMVar, writeTQueue)
import Control.Exception.Safe (catchAny)
import Control.Monad          (forever)
import Data.Text              (Text, pack)
import Data.Time              (UTCTime, getCurrentTime)

data ExceptionMessage = ExceptionMessage
    { exceptionSource  :: Text
    , exceptionTime    :: UTCTime
    , exceptionMessage :: Text
    } deriving (Show)

logException :: TQueue ExceptionMessage -> Text -> IO () -> IO ()
logException exceptQueue source io = do
    time <- getCurrentTime
    let writeException exception = atomically $ writeTQueue exceptQueue $ ExceptionMessage source time (pack . show $ exception)
    catchAny io writeException

initExcept :: IO (TQueue ExceptionMessage, TMVar [ExceptionMessage])
initExcept = do
    (exceptQueue, exceptVar) <- atomically $ do
        queue <- newTQueue
        var <- newTMVar []
        return (queue, var)
    forkIO $ forever $ processExceptQueueMessage exceptQueue exceptVar
    return (exceptQueue, exceptVar)

processExceptQueueMessage :: TQueue ExceptionMessage -> TMVar [ExceptionMessage] -> IO ()
processExceptQueueMessage exceptQueue exceptVar = atomically $ do
    nextMsg <- readTQueue exceptQueue
    existing <- takeTMVar exceptVar
    let new = existing ++ [nextMsg]
    putTMVar exceptVar new
