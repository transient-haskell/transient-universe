#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
--  runghc -DDEBUG    -i../transient/src -i../transient-universe/src -i../axiom/src    tests/testRestService.hs -p start/localhost/8000


{-# LANGUAGE ScopedTypeVariables, OverloadedStrings   #-}
module Main where

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
import System.IO
import Control.Exception
import Data.Char
import Data.Aeson

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics




getRESTReq= "GET /todos/$1 HTTP/1.1\r\n"
         <> "Host: $hostnode\r\n" 
         <> "\r\n" :: String

         
postRESTReq=  "POST /todos HTTP/1.1\r\n"
           <> "HOST: $hostnode\r\n"
           <> "Content-Type: application/json\r\n\r\n" 
           <>"{\"id\": $1,\"userId\": $2,\"completed\": $3,\"title\":$4}"


postRestService= [("type","HTTP")
                 ,("nodehost","jsonplaceholder.typicode.com")
                 ,("nodeport","80"),("HTTPstr",postRESTReq)]
getRestService = [("type","HTTP")
                 ,("nodehost","jsonplaceholder.typicode.com")
                ,("nodeport","80"),("HTTPstr",getRESTReq)]



type Literal = BS.ByteString  -- appears with " "
type Symbol= String  -- no "  when translated 

main= keep $ initNode $ do
      local $ option ("go" ::String)  "go"

      r <-callService postRestService (10 :: Int,4 :: Int, "true" :: Symbol ,  "title alberto" :: Literal)  :: Cloud Value
      local $ do
          HTTPHeaders _ headers <- getState <|> error "no heraders. That should not happen" 
          liftIO $ print headers 
          liftIO $ print r
 
          
      r <- callService getRestService (10::Int)
      local $ do
          HTTPHeaders _ headers <- getState <|> error "no heraders. That should not happen"
          liftIO $ do
              putStrLn "HEADERS"
              print headers
              putStrLn "RESULT"
              print  (r :: Value)
      
      
