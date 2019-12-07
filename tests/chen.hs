#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
--  runghc -DDEBUG    -i../transient/src -i../transient-universe/src -i../axiom/src    tests/chen.hs -p start/localhost/8000
 
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude, DeriveGeneric #-}
module Main
(main
) where

import Protolude hiding (async,local,Symbol,option, onException)
import Transient.Base
import Transient.Move.Internals
import Transient.Move.Services
import Transient.EVars
import Transient.Indeterminism 
import Transient.Internals
import Transient.Move.Utils
import Transient.Parse
import Control.Applicative
import Data.Monoid
import Control.Concurrent

import Data.String
import Control.Monad.State
--import System.IO 
import Control.Exception hiding (onException)
import Data.Char
import Data.Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics

getRESTReq= "GET /station?id=eq.$1 HTTP/1.1\r\n"
         <> "Host: $hostnode\r\n"
       --  <> "Connection: close\r\n"
         <> "\r\n" :: String
         
getRestService = [("type","HTTP")
                 ,("nodehost","47.112.196.170")
                ,("nodeport","9001"),("HTTPstr",getRESTReq)]

postRESTReq=  "POST /station HTTP/1.1\r\n"
           <> "Host: $hostnode\r\n"
          -- <> "Connection: close\r\n"
           <> "Content-Type: application/json\r\n"
           <> "Content-Length: $1\r\n\r\n" 
           <> "$2" :: String

postRestService= [("type","HTTP")
                 ,("nodehost","47.112.196.170")
                 ,("nodeport","9001"),("HTTPstr",postRESTReq)]




type Literal = BS.ByteString  -- appears with " "
type Symbol= String  -- no "  when translated

data Station = Station { name :: Text, remarks :: Maybe Text} deriving (Generic)
instance ToJSON Station

data PostResponse= OK |  ErrorPost Value deriving (Typeable, Read,Show)

instance Loggable1 PostResponse where
   serialize _ = undefined

   deserialize  = (ErrorPost <$> deserialize) <|> return OK
  

main= keep $ initNode $ inputNodes <|> do
      local $ option ("go" :: String) "go"

      let s1 = Station "stat16" (Just "zhongzhou5")
      let jsonmsg= BSL.unpack $ encode s1
      let len= length jsonmsg
      msg <- callService postRestService (len,jsonmsg)  :: Cloud  PostResponse
      local $ do
          headers <- getState <|> return (HTTPHeaders [])
          liftIO $ print headers
          liftIO $ print ("MESSAGE", msg)

{-
      r <- callService getRestService (1 ::Int)
      local $ do
          headers <- getState <|> return (HTTPHeaders [])
          liftIO $ print headers
      localIO $ print  (r :: Value)
-}