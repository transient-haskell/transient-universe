{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric   #-}
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

main= keep $ initNode $ inputNodes <|> do
      local $ option ("go" ::String)  "go"




      callService "" postRestService (10 :: Int,4 :: Int, "true" :: Symbol ,  "title alberto" :: Literal)  :: Cloud ()
      local $ do
          headers <- getState <|> return (HTTPHeaders []) 
          liftIO $ print headers
          
      r <- callService "" getRestService (10::Int)
      local $ do
          headers <- getState <|> return (HTTPHeaders [])
          liftIO $ print headers
      localIO $ print  (r :: Value)
      
      
data Pepe = Pepe Int deriving (Generic,Read,Show)
  
instance ToJSON Pepe

instance FromJSON Pepe

instance {-# Overlapping #-}  Loggable1 Pepe where
   serialize= encode
   deserialize msg= case decode msg  of
          Just x -> Just (x,mempty)
          _      -> Nothing

main1=keep $ initNode $ inputNodes <|> copyFile  "test5" "sal" 

copyFile :: String -> String  -> Cloud ()
copyFile fnf fnt  = cthreads 0 $ do
  local $ option ("copy" :: String) "copy"
  nodes <- local getNodes
  guard (length nodes> 1)
  let node= nodes !! 1
  defaultChunkSize <- local getBuffSize

  syncStream $ wormhole node $ do
    hout <- fixRemote $ liftIO $ openBinaryFile fnt WriteMode
  

    mline <- local $ do
        hin <- liftIO $ openBinaryFile fnf ReadMode
        parallel $  do
            l <- BS.hGetSome hin defaultChunkSize
            return $ if BS.null l then SDone  else SMore l
                      
    atRemote $  local $ threads 0 $ liftIO $
        case mline of
          SMore line -> BS.hPut hout line
          _ -> do print "CLOSE" ; hClose hout
          
  
cthreads n f= Cloud $ threads n $ runCloud' f



