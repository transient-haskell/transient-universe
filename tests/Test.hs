#!/usr/bin/env ./executor.sh
-- set -e  && docker run -it -v /c/Users/magocoal/OneDrive/Haskell/devel:/devel agocorona/transient:01-27-2017  bash -c "runghc  -j2 -isrc -i/devel/transient/src -i/devel/transient-universe/src /devel/transient-universe/tests/$1 $2 $3 $4"

{-# LANGUAGE ScopedTypeVariables #-}
import Transient.Internals
import Transient.EVars
import Transient.Move
import Transient.Indeterminism
import Transient.Move.Utils
import Control.Applicative
import Control.Exception
import GHC.Conc
import Control.Monad.State
import Data.Maybe
import System.Random
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Monoid


main= keep $ runCloud' $ do
    runTestNodes [2000..2020]
    local $ option  "f" "fire"

   -- nodes <- local getNodes
--    r <- (runAt (nodes !! 0) $ showNode 0 )  <>
--          (runAt (nodes !! 1)  $ showNode 1 )   <>
--          (runAt (nodes !! 2)  $ showNode 2) 
    r <- mclustered $ showNode
    localIO $ print r
    where
 
        
    showNode =  local $ do

      n <-  getMyNode
      -- liftIO $ threadDelay  $ 1000000  * (2-t)
      liftIO $ print n
      return [n]