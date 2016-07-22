import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

main = do
    master <- newBroadcastTChanIO

    forM_ [1..10] $ \i -> do
        chan <- atomically $ dupTChan master
        forkIO $
            forever $ do
                x <- atomically $ readTChan chan
                putStrLn $ "Thread " ++ show i ++ ": " ++ show x

    atomically $ writeTChan master "h"

    -- Give threads time to complete
    threadDelay 1000000
