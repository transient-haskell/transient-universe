{-# LANGUAGE   CPP,NoMonomorphismRestriction  #-}

module Main where

import Prelude hiding (div,id)
import Transient.Base



import GHCJS.HPlay.Cell
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map, input,option)
#else
   hiding (map, option,input)
#endif


import Transient.Move
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





main= do
     let numNodes = 3
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"
         nodes = map createLocalNode ports
         n2000= Prelude.head nodes
         n2001= nodes !! 1
         n2002= nodes !! 2

     runCloudIO $ do
          runNodes nodes
          local $ option "s" "start"
          runAt n2000 $ localIO $ print "hello"


runNodes nodes= foldl (<|>) empty (map listen nodes) <|> return()
