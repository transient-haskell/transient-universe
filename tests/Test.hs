{-# LANGUAGE CPP, DeriveDataTypeable, NoMonomorphismRestriction #-}
module Main where

import           Control.Applicative
import           Control.Concurrent         (forkIO, threadDelay)



import           Transient.EVars
import   GHCJS.HPlay.View hiding (option)
import   GHCJS.HPlay.Cell
import   Transient.Indeterminism
import           Transient.Internals
import           Transient.Move
import           Transient.Move.Utils
import Control.Applicative
import Control.Monad.IO.Class
import Data.IORef
import Control.Concurrent.MVar
import Data.Typeable
import Data.String
import Prelude hiding (div,id)
import Control.Monad.State
import GHC.Conc


main= keep $ do

   option "go"  "go"
   chs <- liftIO $ newTVarIO []
   modify $ \ s -> s{children= chs}
   killChilds
   x <-   threads 1 $ choose [1..]
   liftIO $ do
     threadDelay 1000000
     liftIO $ print x


{-
goal kill the previous sequence

  chs <- liftIO $ newTVarIO []
   r <-  comp
   modify $ \ s -> s{children= chs}
   killChilds
   return r

es el mismo chs en listen.
como separarlo:
-}
