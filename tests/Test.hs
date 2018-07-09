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
import Control.Concurrent.MVar
import qualified Data.Map as M
main= keep $ runCloud' $ do
    runTestNodes [2000..2002]
    local $ option  "f" "fire"
    nodes <- local getNodes
    r <- mclustered $ showNode
    localIO $ print r
    where
    showNode=  local $ do
      n <-  getMyNode
      -- liftIO $ when (nodePort n ==2000) $ threadDelay 1000000
      liftIO $ print n