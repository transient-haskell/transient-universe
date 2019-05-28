#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
--  runghc     -i../transient/src -i../transient-universe/src -i../axiom/src    $1 $2 $3

import System.IO (hFlush,stdout) 

import System.IO.Unsafe

import Control.Concurrent.MVar

import Control.Applicative



import Control.Concurrent(threadDelay)

import Control.Exception hiding(onException)

import Data.IORef

import Control.Monad(when) 

import Data.Typeable

import System.Random

import Data.Maybe 

import qualified Data.Map as M

import System.Environment

import Data.List(isPrefixOf)

import Unsafe.Coerce

import Data.Monoid 

import Transient.Internals
import Transient.Indeterminism
import Transient.Move
import Transient.Move.Utils
import Control.Monad.State
import Control.Exception hiding(onException)

import System.IO

main= keep $ initNode  $ inputNodes <|>  do

  local $ option "go" "go"
  nodes <- local getNodes
  do
      local $  choose [1 :: Int ..]
      buf <- local $ return "" -- [1..4000 :: Int]
      
      runAt (nodes !!1) $ localIO $ return ()


main1= keep $ initNode  $ inputNodes <|>  do
  local $ option "go" "go"
  do
      local $ setSynchronous True 
      line  <- local $ choose[1..10::Int] 
      localIO $ print ("1",line)
      nodes <- local getNodes
      local $ guard (length nodes ==2)
      runAt (nodes !!1) $ localIO $ print line 
      localIO $ print ("2", line) 