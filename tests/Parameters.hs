{-# LANGUAGE ExistentialQuantification , ScopedTypeVariables, FlexibleInstances #-}
module Transient.Move.Parameters where

import Transient.Internals
import Transient.Move
import Transient.Move.Utils
import Data.Typeable
import Data.Map as M
import System.Random
import System.IO.Unsafe
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad

import Control.Applicative
import Transient.Indeterminism
import Control.Concurrent
import Control.Exception hiding (onException)
import System.CPUTime
-- -- opcion reactiva= parameter= EVar o mailbox

-- data Parameter=  Parameter String Int

parameters= unsafePerformIO $  newIORef  $ M.empty 

-- | Parameters change along the execution and are read bu the application to modify his behaviour. `optimize`change the parameters in order
-- to maximize an expression defined by the programmer. this expression may include latency, troughput, memory usage etc.
--
-- To optimize the function, it uses a monte-carlo method that `optimize` run permanently.
--
-- Parameters can change buffer sizes, number of threads, number of instances. It depends on the programmer.
setParameter ::  String  -> Int -> TransIO ()
setParameter n v= do
     vec <- liftIO $ readIORef parameters
     putMailbox' n v 
     liftIO $ writeIORef parameters $ M.insert  n v vec

-- | The programmer can create a parameter anywhere
addParameter :: MonadIO m => String  -> Int -> m ()
addParameter n v= liftIO $ do
     vec <- readIORef parameters
     writeIORef parameters $ M.insert  n v vec

-- | get the value of a parameter reactively: this means that when `optimize` changes it, the application receives the new value.
getParameter ::  String -> Int -> TransIO Int
getParameter n v= oneThread $ getMailbox' n <|> getParameterNR n v

-- | A non reactive version of `getParameter` 
getParameterNR ::  MonadIO m => String -> Int -> m Int
getParameterNR n v= do 
    map <- liftIO $ readIORef parameters 
    case  M.lookup n map of
      Nothing -> addParameter n v >> return v
      Just v  -> return v

-- | it should be a single `optinize` call. it executes the optimization expression in a loop within a different thread.
-- The next iteration will start as soon as the previous has finished so it is
-- necessary to introduce a delay which may be variable and subject also to optimizations
-- Take into acount that `getParameter` abort any previous subsequent task in order to execute the continuation witht he new parameter.
-- `optimize` will reset the parameter if the perturbed parameter vale gives less performance than the previous 

-- > main= keep $  optimizeProcess <|> process
-- > optimizeProcess= optimize $ do
-- >      liftIO $ threadDelay 1000000
-- >      expression
optimize ::   TransIO Int -> TransIO ()
optimize expr= do
  abduce
  optimize'
  where
  optimize'= do
     v <- expr  !> "OPTIMIZE"
     (randparam,old) <- perturbe 
     v' <- expr 
     when (v > v') $ setParameter randparam old 
     optimize' 
 
  perturbe =  do
    vec <- liftIO $ readIORef parameters !> "PERTURBE"
    i <-  liftIO $ randomRIO (0,M.size vec -1)
    let (name, pvalue) = M.toList vec !! i !> i
    let range= pvalue `div` 10 +1
    sign <- liftIO randomIO
    let pvalue' = max (pvalue + (range * if sign then 1 else -1)) 0
    
    setParameter name  pvalue'
    return ()  !> ("pvalue",pvalue')
    return (name,pvalue)  !> (name,pvalue)



main= keep $ initNode $  local (optimizeProcess <|> process)

process= do 
   ths <- getParameter "number of threads" 20
   liftIO $ print ("new", ths)
   n <- threads ths $ choose  [1..]
   
   liftIO $ do atomicModifyIORef  counter $ \n -> (n+1,())

   
counter= unsafePerformIO $  newIORef (0 :: Int)

optimizeProcess= optimize $ liftIO $ do
    r <- readIORef counter   
    t <- getCPUTime
    threadDelay 1000000
    r' <- readIORef counter
    t' <- getCPUTime
    let ticks= fromIntegral $ (t'-t) `div`  1000000000
    nthreads <- getParameterNR "number of threads" 20
    let rr=  (r' - r)  `div`  ticks `div` (nthreads +1)
    print ("counter",r'-r,ticks,rr, nthreads, rr)
    return $  rr

