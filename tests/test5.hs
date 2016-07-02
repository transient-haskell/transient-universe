module Main where

import Transient.Move
import Transient.Move.Utils
import           GHCJS.HPlay.View
import Transient.Logged
import Transient.Base
import Transient.Indeterminism
import Transient.EVars
import Control.Applicative

import Control.Monad.IO.Class
import System.Environment
import System.IO.Unsafe
import Data.Monoid
import System.IO
import Control.Monad
import Data.Maybe
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Typeable
import Data.IORef
import Data.List((\\))






-- to be executed with two or more nodes
main = keep $ initNode $  test


test= onBrowser $ do
        local . render $ wlink () $ p "Product Categories"
        r <- local . render $
            (,) <$> inputString Nothing `fire` OnChange
                <*> inputInt Nothing `fire` OnChange

        lliftIO $ print r




--         box <- local newMailBox
--         getMailBox box >>= lliftIO . print <|> putMailBox box "hello"




