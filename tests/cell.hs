{-# LANGUAGE OverloadedStrings #-}
import Transient.Base
import Transient.Move
import Transient.Move.Utils
import GHCJS.HPlay.Cell
import GHCJS.HPlay.View
import Control.Monad.IO.Class
import Control.Monad


-- ++> adds rendering to a widget

main= keep $ initNode  $ inputNodes <|> app

app= onBrowser $ local $ render $ do
       mk space  (Just 1)   ! size "10" <|>  br ++>
        mk time (Just 2)   ! size "10" <|>  br ++>
        mk speed (Just 3)   ! size "10"

       calc
       where
       size= atr "size"

       space = scell "space" $ do -- runCloud $ atRemote $ local $ do
                      liftIO $ print "running cella at server"
                      norender $ gcell "speed" * gcell "time"

       time  = scell "time" $  do -- runCloud $ atRemote $ local $ do
                      liftIO $ print "running cellb at server"
                      norender $ gcell "space"  / gcell "speed"

       speed = scell "speed" $ do -- runCloud $ atRemote $ local $ do
                      liftIO $ print "running cellc at server"
                      norender $ gcell "space" / gcell "time"





