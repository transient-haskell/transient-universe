-----------------------------------------------------------------------------
--
-- Module      :  Transient.Move.Services.MonitorService
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import Transient.Base

import Transient.Move
import Transient.Move.Utils
import Transient.Move.Services
import Control.Applicative
import Control.Monad.IO.Class
import Data.List ((\\))

main = keep . runCloud $ do
    runService monitorService $ \(ident,service) -> do
       mnode <-  (local $ findInNodes service >>= return . Just . head) <|>
                  requestInstall ident service
       return (mnode :: Maybe Node)
    where
    installHere ident service@(package,program)= local $ do
        thisNode <-  getMyNode
        yn<- authorizeService ident service   -- !> "AUTHORIZE"
        if yn
          then do
              node <- liftIO $ do
                    port <- freePort
                    putStr "Monitor: installing " >> putStrLn package
                    install package program  (nodeHost thisNode) port
                    putStrLn "INSTALLED"
                    nodeService thisNode port service
              addNodes [node]
              return  $ Just node
          else return Nothing

    requestInstall :: String -> Service -> Cloud (Maybe Node)
    requestInstall ident service= do
        mnode <- installHere ident service          -- !> "INSTALLHERE"
        case mnode of
          Nothing -> installThere ident service
          justnode -> return justnode

    installThere ident service= do
        nodes  <- onAll $ findInNodes monitorService   -- !> "installThere"
        mynode <- onAll getMyNode                      -- !> nodes
        request $ nodes \\ [mynode]
        where
        request []= empty
        request (n:ns)= do
             mnode <- callService' ident n (ident,service)  -- !> ("calling",n)
             case mnode of
                Nothing  -> request ns
                justnode -> return justnode
