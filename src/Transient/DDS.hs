{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable
, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses #-}


module Transient.DDS   (distribute, getText, getUrl, getFile,textUrl, textFile, mapKeyB, mapKeyU, reduce) where
import Transient.Base
import Transient.Move hiding (pack)
import Transient.Logged
import Transient.Indeterminism

import Control.Applicative
import System.Random
import Control.Monad.IO.Class

import System.IO
import Control.Monad
import Data.Monoid
import Data.Maybe

import Data.Typeable
import Data.List hiding (delete, foldl')
import Control.Exception
import Control.Concurrent
--import Data.Time.Clock
import Network.HTTP
import Data.TCache hiding (onNothing)
import Data.TCache.Defs

import Data.ByteString.Lazy.Char8 (pack,unpack)
import Control.Monad.STM
import qualified Data.Map as M
import Control.Arrow (second)
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector as DV
import Data.Hashable
import System.IO.Unsafe

import qualified Data.Foldable as F
import qualified Data.Text as Text

data DDS a= Loggable a => DDS  (Cloud (PartRef a))
data PartRef a= Ref Node Path Save deriving (Typeable, Read, Show)
data Partition a=  Part Node Path Save a deriving (Typeable,Read,Show)
type Save= Bool


instance Indexable (Partition a) where
    key (Part _ string b _)= keyp string b


keyp s True= "PartP@"++s
keyp s False="PartT@"++s

instance Loggable a => IResource (Partition a) where
    keyResource= key
    readResourceByKey k=  r
      where
      typePart :: IO (Maybe a) -> a
      typePart = undefined
      r =  if k !! 4 /= 'P' then return Nothing   else
            defaultReadByKey  (defPath (typePart r) ++ k) >>= return . fmap ( read . unpack)
    writeResource (s@(Part _ _ save _))=
          unless (not save) $ defaultWrite (defPath s ++ key s) (pack $ show s)


eval :: DDS a -> TransIO (PartRef a)
eval (DDS mx) = runCloud mx


type Path=String


instance F.Foldable DVU.Vector where
  {-# INLINE foldr #-}
  foldr = foldr

  {-# INLINE foldl #-}
  foldl = foldl

  {-# INLINE foldr1 #-}
  foldr1 = foldr1

  {-# INLINE foldl1 #-}
  foldl1 = foldl1

--foldlIt' :: V.Unbox a => (b -> a -> b) -> b -> V.Vector a -> b
--foldlIt' f z0 xs= V.foldr f' id xs z0
--      where f' x k z = k $! f z x
--
--foldlIt1 :: V.Unbox a => (a -> a -> a) -> V.Vector a -> a
--foldlIt1 f xs = fromMaybe (error "foldl1: empty structure")
--                    (V.foldl mf Nothing xs)
--      where
--        mf m y = Just (case m of
--                         Nothing -> y
--                         Just x  -> f x y)

class (F.Foldable c, Typeable c, Typeable a, Monoid (c a), Loggable (c a)) => Distributable c a where
   singleton :: a -> c a
   splitAt :: Int -> c a -> (c a, c a)
   fromList :: [a] -> c a

instance (Loggable a) => Distributable DV.Vector a where
   singleton = DV.singleton
   splitAt= DV.splitAt
   fromList = DV.fromList

instance (Loggable a,DVU.Unbox a) => Distributable DVU.Vector a where
   singleton= DVU.singleton
   splitAt= DVU.splitAt
   fromList= DVU.fromList

-- | perform a map and partition the result with different keys using boxed vectors
-- The final result will be used by reduce.
mapKeyB :: (Loggable a, Loggable b,  Loggable k,Ord k)
     => (a -> (k,b))
     -> DDS  (DV.Vector a)
     -> DDS (M.Map k(DV.Vector b))
mapKeyB= mapKey

-- | perform a map and partition the result with different keys using unboxed vectors
-- The final result will be used by reduce.
mapKeyU :: (Loggable a, Loggable b,  Loggable k,Ord k)
     => (a -> (k,b))
     -> DDS  (DVU.Vector a)
     -> DDS (M.Map k(DVU.Vector b))
mapKeyU= mapKey

-- | perform a map and partition the result with different keys.
-- The final result will be used by reduce.
mapKey :: (Distributable vector a,Distributable vector b, Loggable k,Ord k)
     => (a -> (k,b))
     -> DDS  (vector a)
     -> DDS (M.Map k (vector b))
mapKey f (DDS mx)= DDS $  do
        refs <-  mx
        process refs

  where
--  process ::  Partition a -> Cloud [Partition b]
  process  (ref@(Ref node path sav))= runAt node $ local $ do
              xs <- getPartitionData ref   -- !> "CMAP"
              generateRef node $ map1 f xs



--  map1 :: (Ord k, F.Foldable vector) => (a -> (k,b)) -> vector a -> M.Map k(vector b)
  map1 f v=  F.foldl' f1  M.empty v
     where
     f1 map x=
           let (k,r) = f x
           in M.insertWith (<>) k (Transient.DDS.singleton r) map



--instance Show a => Show (MVar a) where
--   show mvx= "MVar " ++ show (unsafePerformIO $ readMVar mvx)
--
--instance Read a => Read (MVar a) where
--   readsPrec n ('M':'V':'a':'r':' ':s)=
--      let [(x,s')]= readsPrec n s
--      in [(unsafePerformIO $ newMVar x,s')]

data ReduceChunk a= EndReduce | Reduce a deriving (Typeable, Read, Show)

reduce ::  (Hashable k,Ord k, Distributable vector a, Loggable k,Loggable a)
             => (a -> a -> a) -> DDS (M.Map k (vector a)) ->Cloud (M.Map k a)
reduce red  (dds@(DDS mx))= do
   box <- local newMailBox
   nodes <- onAll getNodes

   let lengthNodes = length nodes
       shuffler=   do
          ref <- mx
          local $ do
              m <-  getPartitionData ref   -- !> "GETPARTITIONDATA"
              let ass= M.assocs m

              runCloud (parallelize  shuffle ass) <*** runCloud sendEnd

          stop

--       shuffle ::(k,vector a) -> Cloud ()
       shuffle part | null part = empty
                    | otherwise = do

         let (k,vs)= part
             v= foldl1 red vs
             i=  abs $ hash k `rem` length nodes
         runAt  (nodes !! i) $ local $ putMailBox box $ Reduce (k,v)    -- !> (" PUTMAILBOX ",i,v)
         empty :: Cloud ()

       sendEnd =  (clustered $ local $ putMailBox box (EndReduce `asTypeOf` paramOf dds))  -- !> "ENDREDUCE"


       reducer=   mclustered reduce    -- a reduce process in each node

--     reduce :: (Ord k)  => Cloud (M.Map k v)

       reduce =  local $ do
           reduceResults <- liftIO $ newMVar M.empty  -- !>  "CREATE MVAR for results"
           numberSent <- liftIO $ newMVar 0
           minput <- getMailBox box  -- get the chunk once it arrives to the mailbox

           case minput  of -- !> ("received",minput) of
             Reduce (k,inp) -> do
                let input= inp `asTypeOf` atype dds

                liftIO $ modifyMVar_ reduceResults
                       $ \map -> do
                          let maccum =  M.lookup k map
                          return $ M.insert k (case maccum of
                            Just accum ->  red input accum
                            Nothing    ->  input) map
                empty

             EndReduce -> do
                n <- liftIO $ modifyMVar numberSent $ \r -> return (r+1, r+1)
                if n == lengthNodes                         -- !> ( n, lengthNodes)
                 then liftIO $ readMVar reduceResults     -- !> "END reduce"

                 else empty

   reducer <|> shuffler
   where
     atype ::DDS(M.Map k (vector a)) ->  a
     atype = undefined -- type level

     paramOf  :: DDS (M.Map k (vector a)) -> ReduceChunk( k,  a)
     paramOf = undefined -- type level



--parallelize :: Loggable b => (a -> Cloud b) -> [a] -> Cloud b
parallelize f xs =  foldr (<|>) empty $ map f xs






getPartitionData :: Loggable a => PartRef a   -> TransIO  a
getPartitionData (Ref node path save)  = do
    (Part _ _ _ xs) <- (liftIO $ atomically
                                   $ readDBRef
                                   $ getDBRef
                                   $ keyp path save)
                              `onNothing` error ("not found DDS data: "++ keyp path save)
    return xs  -- !> "getPartitionData"

-- en caso de fallo de Node, se lanza un clustered en busca del path
--   si solo uno lo tiene, se copia a otro
--   se pone ese nodo de referencia en Part
runAtP :: Loggable a => Node  -> (Path -> IO a) -> Path -> Cloud a
runAtP node f uuid= do
   r <- streamFrom node $ onAll . liftIO $ (SLast <$> f uuid) `catch` sendAnyError
   case r of
     SLast r -> return r
     SError e -> do
         nodes <-  mclustered $ search uuid
         when(length nodes < 1) $ asyncDuplicate node uuid
         runAtP ( head nodes) f uuid

search uuid= error $ "chunk failover not yet defined. Lookin for: "++ uuid

asyncDuplicate node uuid= do
    forkTo node
    nodes <- onAll getNodes
    let node'= head $ nodes \\ [node]
    content <- onAll . liftIO $ readFile uuid
    runAt node' $ local $ liftIO $ writeFile uuid content

sendAnyError :: SomeException -> IO (StreamData a)
sendAnyError e= return $ SError  e


-- | distribute a vector of values among many nodes.
-- If the vector is static and sharable, better use the get* primitives
-- since each node will load the data independently.
distribute :: (Loggable a, Distributable vector a ) => vector a -> DDS (vector a)
distribute = DDS . distribute'

distribute' xs= loggedc $  do
   nodes <- local getNodes                                        -- !> "DISTRIBUTE"
   let lnodes = length nodes
   let size= case F.length xs `div` (length nodes) of 0 ->1 ; n -> n
       xss= split size lnodes 1 xs                                     -- !> size
   distribute'' xss nodes
   where
   split n s s' xs | s==s' = [xs]
   split n s s' xs=
      let (h,t)= Transient.DDS.splitAt n xs
      in h : split n s (s'+1) t

distribute'' :: (Loggable a, Distributable vector a) => [vector a] -> [Node] -> Cloud (PartRef (vector a))
distribute'' xss nodes =
   parallelize  move $ zip nodes xss   -- !> show xss
   where
   move (node, xs)=  runAt node $ local $ do
                        par <- generateRef node xs
                        return  par

-- | input data from a text that must be static and shared by all the nodes
getText  :: (Loggable a, Distributable vector a) => (String -> [a]) -> String -> DDS (vector a)
getText part str= DDS $ do
   nodes <- onAll getNodes                                        -- !> "DISTRIBUTE"
   let lnodes = length nodes

   parallelize  (process lnodes)  $ zip nodes [0..lnodes]    -- !> show xss
   where
   process lnodes (node,i)= runAt node $ local $   do
                        let xs = part str
                            size= case length xs `div` lnodes of 0 ->1 ; n -> n
                            xss= Transient.DDS.fromList $ take size $ drop  (i *  size) xs
                        par <- generateRef node xss
                        return  par

-- | get the worlds of an URL
textUrl :: String -> DDS (DV.Vector Text.Text)
textUrl= getUrl  (map Text.pack . words)

-- | generate a DDS from the content of a URL
getUrl :: (Loggable a, Distributable vector a) => (String -> [a]) -> String -> DDS (vector a)
getUrl partitioner url= DDS $ do
   nodes <- onAll getNodes                                        -- !> "DISTRIBUTE"
   let lnodes = length nodes

   parallelize  (process lnodes)  $ zip nodes [0..lnodes]    -- !> show xss
   where
   process lnodes (node,i)=  runAt node $ local $ do
                        r <- liftIO . simpleHTTP $ getRequest url
                        body <- liftIO $  getResponseBody r
                        let xs = partitioner body
                            size= case length xs `div` lnodes of 0 ->1 ; n -> n
                            xss= Transient.DDS.fromList $ take size $ drop  (i *  size) xs
                        generateRef node xss


-- | get the words of a file
textFile ::  String -> DDS (DV.Vector Text.Text)
textFile= getFile (map Text.pack . words)

-- | generate a DDS from a file. All the nodes must access the file with the same path
-- the first parameter is the parser that generates elements from the content
getFile :: (Loggable a, Distributable vector a) => (String -> [a]) ->  String -> DDS (vector a)
getFile partitioner file= DDS $ do
   nodes <- local getNodes                                        -- !> "DISTRIBUTE"
   let lnodes = length nodes

   parallelize  (process lnodes) $ zip nodes [0..lnodes]    -- !> show xss
   where
   process lnodes (node, i)=  runAt node $ local $ do
                        content <- liftIO $ readFile file
                        let xs = partitioner content
                            size= case length xs `div` lnodes of 0 ->1 ; n -> n
                            xss=Transient.DDS.fromList $ take size $ drop  (i *  size) xs                                -- !> size
                        generateRef node   xss



generateRef :: Loggable a => Node -> a -> TransIO (PartRef a)
generateRef node x=  liftIO $ do
       temp <- getTempName
       let reg=  Part node temp True  x
       atomically $ newDBRef reg
       return $ getRef reg

getRef (Part n t s x)= Ref n t s

getTempName :: IO String
getTempName=  ("DDS" ++) <$> replicateM  5 (randomRIO ('a','z'))


-------------- Distributed  Datasource Streams ---------
-- | produce a stream of DDS's that can be map-reduced. Similar to spark streams.
-- each interval of time,a new DDS is produced.
streamDDS
  :: (Loggable a, Distributable vector a) =>
     Integer -> IO (StreamData a) -> DDS (vector a)
streamDDS time io= DDS $ do
     xs <- local . groupByTime time $ do
               r <- parallel io
               case r of
                    SDone -> empty
                    SLast x -> return x
                    SMore x -> return x
                    SError e -> error $ show e
     distribute'  $ Transient.DDS.fromList xs




--data CloudArray a= Cloud [a]
--
--instance functor  CloudArray where
--   fmap f mx= do
--        xs <- mx
--        xss <- partition xs
--        rss <- clustered f xss
--        return $ concat rss


