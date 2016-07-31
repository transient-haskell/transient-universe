{-# LANGUAGE   CPP,NoMonomorphismRestriction  #-}

module Main where

import Prelude hiding (div,id)
import Transient.Base



--import GHCJS.HPlay.Cell
--import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map, input,option)
#else
   hiding (map, option,input)
#endif

import Transient.Base
import Transient.Move
import Transient.Move.Utils
import Transient.EVars
import Transient.Indeterminism

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Map as M
import Transient.MapReduce
import Control.Monad.IO.Class
import Data.String
import qualified Data.Text as T
#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JS hiding (span,empty,strip,words)
#endif

import Control.Concurrent.MVar
import System.IO.Unsafe



main= keep $ initNode $ test


test :: Cloud ()
test= onServer $ do
   local $ option "t" "do test"

   r <- wormhole (Node "localhost" 8080 (unsafePerformIO $ newMVar  []) []) $ do
      teleport
      p <- localIO $ print "ping" >> return "pong"
      teleport
      return p
   localIO $ print r

