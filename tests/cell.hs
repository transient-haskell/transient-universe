{-# LANGUAGE OverloadedStrings #-}
import Transient.Base

import Transient.Move
import Transient.Internals((!>))
import GHCJS.HPlay.Cell
import qualified GHCJS.Perch  as P (input)
import GHCJS.HPlay.View hiding (option, input)
import Control.Monad.IO.Class
import Data.String
import System.Random
import Data.List
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent(threadDelay)




main= keep $ do
   port <- getPort

   initWebApp port  $ onBrowser $ local $ render $ do
       mk cellA  (Just 1)  <|> mk cellB (Just 2)
       calc
       where
       cellA = scell "cella" $ runCloud $ do
                     lliftIO $ print "local"
                     atRemote $ do
                       lliftIO $ print "running cella at server"
                       return 2

       cellB = scell "cellb" $  runCloud $ do
                    lliftIO $ print "local2"
                    atRemote $ do
                       lliftIO $ print "running cellb at server"
                       return 4


main2= keep $ do
   port <- getPort

   initWebApp port  $ onBrowser $  do

       local $ render $ rawHtml $ h1 ("laps" :: String)

       lap <-   atRemote  laps <|> return 0

       lap' <- local $ render $  inputInt (Just lap) `fire` OnKeyUp <|> return lap

       carPositions lap'


   where

   carPositions l = do
        pos <- atRemote $ carPosition l
        local $ render $ rawHtml $ p pos


   carPosition lap= local $ do
       positions <- liftIO $ readIORef rposList  !> ("carpositions", lap)
       if lap >= length positions                !> ("length", length positions)
         then empty
         else return $ positions !!  lap

--   distance=  mkscell "distance" (Just 0) (gcell "lap" * 15) ! size "5"



rposList= unsafePerformIO $ newIORef []  :: IORef [[String]]

laps= local $ do
       r<- parallel $ do
           threadDelay 10000000
           newpos <- carPos
           positions <- readIORef rposList  !> newpos
           writeIORef rposList $ positions ++ [newpos]
           let l= length positions
           return $ if l == totalLaps
                        then SLast $ fromIntegral l
                        else SMore $ fromIntegral l
       case r of
           SLast lap -> empty
           SMore lap -> return lap

    where
    carPos= do
        pos <- randomRIO (0,2)
        let carpos= cars !! pos
        return $ carpos : (cars \\ [carpos])

totalLaps= 42
cars =["car1", "car2", "car3"]

size= atr "size"
fs= fromString


--       rawHtml $ h1 $ ("calculate space, time and speed " :: String)
--
--       wprint ("Can change one of the cell and the other two will be recalculated"::String)
--
--       (pre <<< ("a car runs for" ++> space
--                 **> " Kms during" ++> time **> " Hours;\n"
--                 ++> "His mean speed was" ++> speed <++ "Km/h\n"))
--
--         **> (P.input ! atr "type" "submit" ! atr "value" "calc" `pass` OnClick)
--
--       liftIO $ alert "calc"
--
--       calc
--
--       where
--       space= mkscell "space" (Just 1) (gcell "speed" * gcell "time")   ! size "5"
--       time = mkscell "time"  (Just 1) (gcell "space" / gcell "speed")  ! size "5"
--       speed= mkscell "speed" (Just 1) (gcell "space" / gcell "time")   ! size "5"


getPort :: TransIO Integer
getPort =
      if isBrowserInstance then return 0 else do
          oneThread $ option "start" "re/start" :: TransIO String
          port <- input (const True) "port to listen? "
          liftIO $ putStrLn "node started"
          return port
