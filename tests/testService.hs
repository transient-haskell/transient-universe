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

#ifndef Service
client params= do
      r <- callService  "" ("service","service") params ""
      lliftIO $ print r

#else

main= keep $ do

    initNode' [("service","service")] $ do

      makeService service

addService s= do
   nodes <- getNodes
   con@Connection{myNode= mynode} <- getSData <|> error "connection not set. please initialize it"
   mynode <- getMyNode
   mynode'= mynode{services= services mynode++ s}
   setNodes $ mynode': nodes\\[mynode]
   setData con{myNode= mynode'}


makeService :: (a -> b)  -> a  -> TransIO ()
makeService serv params= wormhole notused $ loggedc $ do
      (x,y) <- local $ return params
      serv (x,y)
      teleport
  where
  notused= error "makeService: node should not be used"

service :: (String,String) -> String
service (x,y)= do
      lliftIO $ print x
      return y

service' params= wormhole undefined $ loggedc $ do
      (x,y) <- local $ return params
      lliftIO $ print x
      local $ return y
      teleport
      empty

#endif


