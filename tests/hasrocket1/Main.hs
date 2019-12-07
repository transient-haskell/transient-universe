
{-# LANGUAGE OverloadedStrings #-}

import Transient.Internals
import Transient.Move.Internals
import Transient.EVars
import Transient.Logged
import Transient.Move.Utils
import Data.Aeson as Aeson
import Data.Containers as H
import Control.Applicative

main= keep' $ do
  broad <- newEVar
  initNodeDef "localhost" 3000 $ apisample broad
  return ()

apisample broad= api $
    do   msg <- param
         processMessage broad msg
         
    <|>  watchBroadcast broad



processMessage broad msg= do
    Aeson.Object obj <- emptyIfNothing $ Aeson.decode msg
    Aeson.String typ <- emptyIfNothing $ H.lookup "type" obj
    case typ of
       "echo" -> return msg
       "broadcast" ->  do
                  let res = Aeson.encode  $ insertMap "type" "broadcastResult" obj
                  writeEVar broad msg
                  return res


watchBroadcast broad= threads 0 $ readEVar broad

