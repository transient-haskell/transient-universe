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
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe
import Control.Concurrent.MVar
import qualified Data.Map as M

main= do
     let numNodes = 3
         ports = [2000 .. 2000 + numNodes - 1]
         createLocalNode = createNode "localhost"

     nodes <- mapM createLocalNode ports
     let n2000= head nodes
         n2001= nodes !! 1
         n2002= nodes !! 2


     keep $ runCloud $ do

        runNodes nodes



        nodes <- local getNodes

        let lengthNodes = length nodes

        (node, mpairs) <- local $ choose[
                                [(1,[("2",[1])])],
                                [(0,[("6",[1])])],
                                [(0,[("6",[1])]),(1,[("4",[1])])] ]

        runAt node $ foldAndSend node nodes part


        where




--           foldAndSend :: (Hashable k, Distributable vector a)=> (Int,[(k,vector a)]) -> Cloud ()
        foldAndSend nodei nodes mpairs=  do

             length <- local . return $ M.size mpairs
             port <- local $ getMyNode >>= return . nodePort
             let port2= nodePort nodes ! nodei
             local $ assert (port==port2) $ return ()  !> (port,port2)

             if  length == 0 then sendEnd  port nodes else do

                 nsent <-  onAll $ liftIO $ newMVar 0

                 (i,folded) <- local $ parallelize foldthem (M.assocs  mpairs)

                 n <- localIO  $ modifyMVar nsent $ \r -> return (r+1, r+1)

                 runAt (nodes !! i) $  local $ (putMailbox (Reduce folded)
                                                     !> ("send",n,length,port,i,folded))

                 return () !> (port,n,length)

                 when (n == length) $ sendEnd  port nodes
                 empty




runNodes nodes= foldr (<|>) empty (map listen nodes) <|> return ()
