{-# LANGUAGE   CPP #-}


import Transient.Base
import Transient.Move
import Transient.Move.Utils
import Transient.Move.Services
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class


main= do

    initNode $ do
      local $ option "start" "start"
      client ("hello","world")

client params= do
      r <- callService  "" ("service","service") params ""
      lliftIO $ print r




