{-# LANGUAGE CPP, TypeSynonymInstances,FlexibleInstances #-}
module Transient.Move.PubSub where
import Transient.Base
import Transient.Internals ((!>))
import Transient.Move
import Transient.Move.Utils
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef
import System.IO.Unsafe
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Typeable
#ifndef ghcjs_HOST_OS
import Data.TCache
import Data.TCache.DefaultPersistence
#endif




type Suscribed = M.Map String [Node]

#ifndef ghcjs_HOST_OS

instance Indexable Suscribed where
   key _= "#suscribed"


instance Serializable Suscribed where
   serialize= pack . show
   deserialize= read . unpack
   



suscribed= getDBRef  "#suscribed" :: DBRef Suscribed

atomicModifyDBRef :: DBRef Suscribed -> (Suscribed -> (Suscribed,a)) -> IO a
atomicModifyDBRef ref proc= atomically $ do
    x <- readDBRef  ref `onNothing` return M.empty
    let (r,y) = proc x
    writeDBRef ref r
    return y



#else

suscribed= undefined

atomicModifyDBRef a b= return ()



#endif

suscribe :: (Typeable a,Loggable a) => String -> Cloud a
suscribe key= do
  node <- local getMyNode
  local (getMailbox' key) <|> notifySuscribe key node
  
  
notifySuscribe key node = atServer (do
       localIO $ atomicModifyDBRef suscribed $ \ss -> (insert key [ node] ss,())
       susc node)
  where
  susc node=do
      exploreNet $ localIO $ liftIO $ atomicModifyDBRef suscribed $ \ss -> (insert key  [node] ss,())

            
      empty

  insert h node susc=
       let ns = fromMaybe [] $ M.lookup h susc
       in M.insert h (union node ns) susc
       

    

unsuscribe key withness= do
   node <- local getMyNode
   local $ cleanMailbox' key withness 
   atServer $ exploreNet $ localIO $ atomicModifyDBRef suscribed $ \ss -> (delete key [node] ss,())

     
  where
  delete h nodes susc=
       let ns = fromMaybe [] $ M.lookup h susc
       in M.insert h (ns \\ nodes) susc
          


publish :: (Typeable a, Loggable a) => String -> a -> Cloud ()
publish key dat= do
   n <- local getMyNode
   publishExclude [n] key dat
   where
   -- publishExclude :: Loggable a => [Node] -> String -> a -> Cloud ()
   publishExclude excnodes key dat= foldPublish (<|>) empty excnodes key $ local $ do 
                     putMailbox' key dat  
                     return () !> "PUTMAILBOX"
                     empty
                     return()

-- | executes `proc` in all the nodes suscribed to `key`
foldPublish op init excnodes key proc= atServer $ do
#ifndef ghcjs_HOST_OS
    nodes <- localIO $ atomically  ((readDBRef suscribed) `onNothing` return M.empty) 
                        >>= return . fromMaybe [] . M.lookup key
#else
    nodes <- localIO empty
#endif
    let unodes= union nodes excnodes
    return() !> ("NODES PUB",nodes \\ excnodes)
    foldr op init $ map pub (nodes \\ excnodes)
    empty

    where 

    pub   node= runAt node $ proc 


    
{-
examples
main = keep $ initNode $  inputNodes <|>  (onBrowser $ do

    --addWebNode

    --local $ optionn ("f" :: String) "fire" 
      -- crawl the cloud to list all the nodes connected 
    --r <- exploreNet $ local  $ return <$> getMyNode :: Cloud [Node]
    --localIO $ print r 
    --empty 

    wnode <- local getMyNode
    atRemote $  local $ updateConnectionInfo wnode "" >> return ()


    r <- suscribe "hello" <|> do
              local  $ optionn ("f" :: String) "fire"
              publish ("hello" ::String) ("world" :: String)
              empty

    local $ render $ rawHtml $ p (r :: String) )


-}