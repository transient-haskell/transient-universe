module Main where

import Transient.Base
import Transient.Move
import Transient.Internals
import GHCJS.HPlay.View
import Transient.Move.Utils
import Control.Applicative
import Control.Monad.IO.Class
import Data.String
import Control.Monad.State

-- to be executed with two or more nodes
main = keep $ initNode $  test

alert1 x = liftIO $ do alert $ fromString $ show x ; return x

test= onBrowser $  local $ do

        r <-  render $
            (,)  <$> getString (Just "eee")      `fire` OnChange
                 <*> getString (Just "eee")     `fire` OnChange
                 <** (inputSubmit "click" `fire` OnClick)

        liftIO $ alert $ fromString $ show r

(<*|) a b=  do
      (x,_) <- (,)  <$> a <*> b
      return x

--return1 l= do
--     IDNUM id <- getSData <|> error "error"
--     id1 <- gets mfSequence
--     liftIO $ alert $ fromString $ show (id,id1)
--     return l


