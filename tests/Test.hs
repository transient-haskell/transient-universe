{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}


module Main where

import           Control.Applicative
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString.Lazy       as DBL hiding (elemIndex, length)
import           Data.Hashable
import           Data.IORef
import           Data.List
import           Data.Map                   as M
import           Data.Maybe
import qualified Data.Text                  as DT
import           Data.Typeable
import           Data.UUID
import           Data.UUID.Aeson
import           Data.UUID.V4
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp   hiding (run)
import           Servant                    hiding (Handler)
import           Servant.API
import           System.IO
import           System.IO.Unsafe
import           Transient.Base
import           Transient.Move
import           Transient.Move.Utils
import           Transient.Internals

newtype VendorId = VendorId UUID
  deriving(Eq, Ord, FromHttpApiData)

newtype ItemId = ItemId UUID
  deriving(Eq, Ord, FromHttpApiData)

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" ItemId :> Capture "vendorId" VendorId :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

instance FromHttpApiData UUID where
  parseUrlPiece t = case fromText t of
    Just u -> Right u
    Nothing -> Left "Invalid UUID"

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  getItems :<|>
  getItemById

type Handler = ExceptT ServantErr IO

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: ItemId -> VendorId -> Handler Item
getItemById i@(ItemId iid) v@(VendorId vid) = do
  let h = hash $ toString iid ++ toString vid
  liftIO $ runCloudIO $ do
    local $ liftIO (readIORef ref) >>= \dat -> modify s{mfData= dat}
    nodes <- onAll getNodes
    let num = h `rem` length nodes
    let node = sort nodes !! num
    m <- hashmap
    quant <- runAt node $ return $ fromJust $ M.lookup (v, i) m
    return $ Item quant "Item 1"




exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId   :: Int,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item


--connectionHandle = connect "localhost" 28015 Nothing
hashmap :: Cloud (Map (VendorId, ItemId) Int)
hashmap = onAll (return $ M.fromList [((VendorId . fromJust $ fromText "bacd5f20-8b46-4790-b93f-73c47b8def72", ItemId . fromJust $ fromText "db6af727-1007-4cae-bd24-f653b1c6e94e"), 10),
                                      ((VendorId . fromJust $ fromText "8f833732-a199-4a74-aa55-a6cd7b19ab66", ItemId . fromJust $ fromText "d6693304-3849-4e69-ae31-1421ea320de4"), 10)])

ref= unsafePerformIO $ newIORef (error "state should have been written here!")
main :: IO ()
main = do
  runCloudIO' $ do
    seed <- lliftIO $ createNode "localhost" 8000
    node <- lliftIO $ createNode "localhost" 8000
    connect node seed
    local $ gets mfData >>= liftIO . writeIORef ref
    nodes <- onAll getNodes
    lliftIO $ print $ length nodes
    m <- hashmap
    -- let num = fromJust $ elemIndex node (sort nodes)
    -- quant <- runAt (nodes !! num) $ return $ M.lookup num m
    -- lliftIO $ print quant
    (do i <- local $ getMailbox "mailbox" ; lliftIO $ print (i::Int))
        <|> (do clustered $ local $ putMailbox "mailbox" (123::Int)  ; Control.Applicative.empty)
        <|> lliftIO run
