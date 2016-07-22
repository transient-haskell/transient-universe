{-# LANGUAGE   CPP #-}


import Transient.Base
import Transient.Internals((!>))
import Transient.Move
import Transient.Move.Utils
import Transient.Logged
import Transient.Move.Services
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

#ifdef Library
client params= do
      r <- callService  "" ("service","service") params
      lliftIO $ print (r :: String)

#else

main= keep $ runCloud $ do
    runService ("service","service") service
    empty
  <|> do
      runNodes [2001]
--      local $ option "start" "start"
      client ("hello","world")
      empty



addService s= do
   con@Connection{myNode= mynode} <- getSData   <|> error "connection not set. please initialize it"

   let mynode'= mynode{nodeServices= s:nodeServices mynode}
   addNodes [mynode']
   setData con{myNode= mynode'}




service :: (String,String) -> Cloud String
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


