{-# LANGUAGE  ExistentialQuantification, DeriveDataTypeable
, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, CPP #-}


module Transient.MapReduce
(
Distributable(..),distribute, getText,
getUrl, getFile,textUrl, textFile,
mapKeyB, mapKeyU, reduce,eval,
--v* internals
DDS(..),Partition(..),PartRef(..))
 where

#ifdef ghcjs_HOST_OS
import Transient.Base
import Transient.Move hiding (pack)
import Transient.Logged
-- dummy Transient.MapReduce module,
reduce _ _ = local stop :: Loggable a => Cloud a
mapKeyB _ _= undefined
mapKeyU _ _= undefined
distribute _ = undefined
getText _ _ = undefined
textFile _ = undefined
getUrl _ _ = undefined
textUrl _ = undefined
getFile _ _ = undefined
eval _= local stop
data Partition
data DDS= DDS
class Distributable
data PartRef a=PartRef a

#else

import Transient.Internals hiding (Ref)

import Transient.Move.Internals hiding (pack)
import Transient.Indeterminism
import Control.Applicative
import System.Random
import Control.Monad.IO.Class

import Control.Monad
import Data.Monoid

import Data.Typeable
import Data.List hiding (delete, foldl')
import Control.Exception
import Control.Concurrent
--import Data.Time.Clock
import Network.HTTP
import Data.TCache hiding (onNothing)
import Data.TCache.Defs

import Data.ByteString.Lazy.Char8 (pack,unpack)
import qualified Data.Map.Strict as M
import Control.Arrow (second)
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector as DV
import Data.Hashable
import System.IO.Unsafe

import qualified Data.Foldable as F
import qualified Data.Text as Text
import Data.IORef

data DDS a= Loggable a => DDS  (Cloud (PartRef a))
data PartRef a= Ref Node Path Save deriving (Typeable, Read, Show)
data Partition a=  Part Node Path Save a deriving (Typeable,Read,Show)
type Save= Bool


instance Indexable (Partition a) where
    key (Part _ string b _)= keyp string b



keyp s True= "PartP@"++s :: String
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


eval :: DDS a -> Cloud (PartRef a)
eval (DDS mx) =  mx


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
mapKeyU :: (Loggable a, DVU.Unbox a, Loggable b, DVU.Unbox b,  Loggable k,Ord k)
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
mapKey f (DDS mx)= DDS $ loggedc $  do
        refs <-  mx
        process refs                            -- !> ("process",refs)

  where
--  process ::  Partition a -> Cloud [Partition b]
  process  (ref@(Ref node path sav))= runAt node $ local $ do
              xs <- getPartitionData ref         -- !> ("CMAP", ref,node)
              (generateRef  $ map1 f xs)



--  map1 :: (Ord k, F.Foldable vector) => (a -> (k,b)) -> vector a -> M.Map k(vector b)
  map1 f v=  F.foldl' f1  M.empty v
     where
     f1 map x=
           let (k,r) = f x
           in M.insertWith (<>) k (Transient.MapReduce.singleton r) map



data ReduceChunk a= EndReduce | Reduce a deriving (Typeable, Read, Show)

boxids= unsafePerformIO $ newIORef 0


reduce ::  (Hashable k,Ord k, Distributable vector a, Loggable k,Loggable a)
             => (a -> a -> a) -> DDS (M.Map k (vector a)) ->Cloud (M.Map k a)

reduce red  (dds@(DDS mx))= loggedc $ do

   mboxid <- localIO $ atomicModifyIORef boxids $ \n -> let n'= n+1 in (n',n')
   nodes <- local getEqualNodes

   let lengthNodes = length nodes
       shuffler nodes = do
          localIO $ threadDelay 100000
          ref@(Ref node path sav) <- mx     -- return the resulting blocks of the map

          runAt node $ foldAndSend node nodes ref

          stop

--     groupByDestiny :: (Hashable k, Distributable vector a)  => M.Map k (vector a) -> M.Map Int [(k ,vector a)]
       groupByDestiny  map =  M.foldlWithKey' f M.empty  map
              where
--              f ::  M.Map Int [(k ,vector a)] -> k -> vector a -> M.Map Int [(k ,vector a)]
              f map k vs= M.insertWith (<>) (hash1 k) [(k,vs)] map
              hash1 k= abs $ hash k `rem` length nodes


--           foldAndSend :: (Hashable k, Distributable vector a)=> (Int,[(k,vector a)]) -> Cloud ()
       foldAndSend node nodes ref=  do

             pairs <- onAll $ getPartitionData1 ref
                        <|>  return (error $ "DDS computed out of his node:"++ show ref    )
             let mpairs = groupByDestiny pairs

             length <- local . return $ M.size mpairs

             let port2= nodePort node


             if  length == 0 then sendEnd   nodes else do

                 nsent <-  onAll $ liftIO $ newMVar 0

                 (i,folded) <- local $ parallelize foldthem (M.assocs  mpairs)

                 n <- localIO  $ modifyMVar nsent $ \r -> return (r+1, r+1)

                 (runAt (nodes !! i) $  local $ putMailbox' mboxid (Reduce folded))
                                                     !> ("send",n,length,i,folded)

--                 return () !> (port,n,length)

                 when (n == length) $ sendEnd   nodes
                 empty

             where


             foldthem (i,kvs)=  async . return
                                $ (i,map (\(k,vs) -> (k,foldl1 red vs)) kvs)


       sendEnd  nodes   =  onNodes nodes $ local
                            $  putMailbox'  mboxid (EndReduce `asTypeOf` paramOf dds)
--                                                  !> ("send ENDREDUCE ", port))

       onNodes nodes f = foldr (<|>) empty $ map (\n -> runAt n f) nodes

       sumNodes nodes f= foldr (<>) mempty $ map (\n -> runAt n f) nodes

       reducer nodes=   sumNodes nodes reduce1    -- a reduce1 process in each node, get the results and mappend them

--     reduce :: (Ord k)  => Cloud (M.Map k v)

       reduce1 = local $ do
           reduceResults <- liftIO $ newMVar M.empty
           numberSent    <- liftIO $ newMVar 0

           minput <- getMailbox' mboxid  -- get the chunk once it arrives to the mailbox

           case minput  of

             EndReduce -> do

                n <- liftIO $ modifyMVar numberSent $ \r -> let r'= r+1 in return (r', r')


                if n == lengthNodes
--                                              !> ("END REDUCE RECEIVED",n, lengthNodes)
                 then do
                    cleanMailbox' mboxid (EndReduce `asTypeOf` paramOf dds)
                    r <- liftIO $ readMVar reduceResults
                    return r

                 else stop

             Reduce kvs ->  do
                let addIt (k,inp) = do
                        let input= inp `asTypeOf` atype dds
                        liftIO $ modifyMVar_ reduceResults
                               $ \map -> do
                                  let maccum =  M.lookup k map
                                  return $ M.insert k (case maccum of
                                    Just accum ->  red input accum
                                    Nothing    ->  input) map 

                mapM addIt  (kvs `asTypeOf` paramOf' dds)
                                                                     !> ("Received Reduce",kvs)
                stop


   reducer  nodes  <|>  shuffler nodes
   where
     atype ::DDS(M.Map k (vector a)) ->  a
     atype = undefined -- type level

     paramOf  :: DDS (M.Map k (vector a)) -> ReduceChunk [( k,  a)]
     paramOf = undefined -- type level
     paramOf'  :: DDS (M.Map k (vector a)) ->  [( k,  a)]
     paramOf' = undefined -- type level




-- parallelize :: Loggable b => (a -> Cloud b) -> [a] -> Cloud b
parallelize f xs =  foldr (<|>) empty $ map f xs

mparallelize f xs =  loggedc $ foldr (<>) mempty $ map f xs


getPartitionData :: Loggable a => PartRef a   -> TransIO  a
getPartitionData (Ref node path save)  = Transient $ do
    mp <- (liftIO $ atomically
                       $ readDBRef
                       $ getDBRef
                       $ keyp path save)
                  `onNothing` error ("not found DDS data: "++ keyp path save)
    case mp of
       (Part _ _ _ xs) -> return $ Just xs

getPartitionData1 :: Loggable a => PartRef a   -> TransIO  a
getPartitionData1 (Ref node path save)  = Transient $ do
    mp <- liftIO $ atomically
                  $ readDBRef
                  $ getDBRef
                  $ keyp path save

    case mp of
      Just (Part _ _ _ xs) -> return $ Just xs
      Nothing -> return Nothing

getPartitionData2 :: Loggable a => PartRef a   -> IO  a
getPartitionData2 (Ref node path save)  =  do
    mp <- ( atomically
                       $ readDBRef
                       $ getDBRef
                       $ keyp path save)
                  `onNothing` error ("not found DDS data: "++ keyp path save)
    case mp of
       (Part _ _ _ xs) -> return  xs

-- en caso de fallo de Node, se lanza un clustered en busca del path
--   si solo uno lo tiene, se copia a otro
--   se pone ese nodo de referencia en Part
runAtP :: Loggable a => Node  -> (Path -> IO a) -> Path -> Cloud a
runAtP node f uuid= do
   r <- runAt node $ onAll . liftIO $ (SLast <$> f uuid) `catch` sendAnyError
   case r of
     SLast r -> return r
     SError e -> do
         nodes <-  mclustered $ search uuid
         when(length nodes < 1) $ asyncDuplicate node uuid
         runAtP ( head nodes) f uuid

search uuid= error $ "chunk failover not yet defined. Lookin for: "++ uuid

asyncDuplicate node uuid= do
    forkTo node
    nodes <- onAll getEqualNodes
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
   nodes <- local getEqualNodes                                        -- !> "DISTRIBUTE"
   let lnodes = length nodes
   let size= case F.length xs `div` (length nodes) of 0 ->1 ; n -> n
       xss= split size lnodes 1 xs                                     -- !> size
   r <- distribute'' xss nodes
   return r
   where
   split n s s' xs | s==s' = [xs]
   split n s s' xs=
      let (h,t)= Transient.MapReduce.splitAt n xs
      in h : split n s (s'+1) t

distribute'' :: (Loggable a, Distributable vector a)
             => [vector a] -> [Node] -> Cloud (PartRef (vector a))
distribute'' xss nodes =
   parallelize  move $ zip nodes xss   -- !> show xss
   where
   move (node, xs)=  runAt node $ local $ do
                        par <- generateRef  xs
                        return  par
          --   !> ("move", node,xs)

-- | input data from a text that must be static and shared by all the nodes.
-- The function parameter partition the text in words
getText  :: (Loggable a, Distributable vector a) => (String -> [a]) -> String -> DDS (vector a)
getText part str= DDS $ loggedc $ do
   nodes <- local getEqualNodes                                        -- !> "getText"
   let lnodes = length nodes

   parallelize  (process lnodes)  $ zip nodes [0..lnodes-1]
   where

   process lnodes (node,i)= 
      runAt node $ local $ do
            let xs = part str
                size= case length xs `div` lnodes of 0 ->1 ; n -> n
                xss= Transient.MapReduce.fromList $
                       if i== lnodes-1 then drop (i* size) xs else  take size $ drop  (i *  size) xs
            generateRef  xss

-- | get the worlds of an URL
textUrl :: String -> DDS (DV.Vector Text.Text)
textUrl= getUrl  (map Text.pack . words)

-- | generate a DDS from the content of a URL.
-- The first parameter is a function that divide the text in words
getUrl :: (Loggable a, Distributable vector a) => (String -> [a]) -> String -> DDS (vector a)
getUrl partitioner url= DDS $ do
   nodes <- local getEqualNodes                                        -- !> "DISTRIBUTE"
   let lnodes = length nodes

   parallelize  (process lnodes)  $ zip nodes [0..lnodes-1]    -- !> show xss
   where
   process lnodes (node,i)=  runAt node $ local $ do
                        r <- liftIO . simpleHTTP $ getRequest url
                        body <- liftIO $  getResponseBody r
                        let xs = partitioner body
                            size= case length xs `div` lnodes of 0 ->1 ; n -> n
                            xss= Transient.MapReduce.fromList $
                                  if i== lnodes-1 then drop (i* size) xs else  take size $ drop  (i *  size) xs
     
                        generateRef  xss


-- | get the words of a file
textFile ::  String -> DDS (DV.Vector Text.Text)
textFile= getFile (map Text.pack . words)

-- | generate a DDS from a file. All the nodes must access the file with the same path
-- the first parameter is the parser that generates elements from the content
getFile :: (Loggable a, Distributable vector a) => (String -> [a]) ->  String -> DDS (vector a)
getFile partitioner file= DDS $ do
   nodes <- local getEqualNodes                                        -- !> "DISTRIBUTE"
   let lnodes = length nodes

   parallelize  (process lnodes) $ zip nodes [0..lnodes-1]    -- !> show xss
   where
   process lnodes (node, i)=  runAt node $ local $ do
                        content <-  do
                              c <- liftIO $ readFile file
                              length c `seq` return c
                        let xs = partitioner  content
                        
                            size= case length xs `div` lnodes of 0 ->1 ; n -> n
                            xss= Transient.MapReduce.fromList $
                                   if i== lnodes-1 then drop (i* size) xs else  take size $ drop  (i *  size) xs
     
                        generateRef    xss



generateRef :: Loggable a =>  a -> TransIO (PartRef a)
generateRef  x=  do
    node <- getMyNode
    liftIO $ do
       temp <- getTempName
       let reg=  Part node temp False  x
       atomically $ newDBRef reg
--       syncCache
       (return $ getRef reg)     -- !> ("generateRef",reg,node)

getRef (Part n t s x)= Ref n t s

getTempName :: IO String
getTempName=  ("DDS" ++) <$> replicateM  5 (randomRIO ('a','z'))


-------------- Distributed  Datasource Streams ---------
-- | produce a stream of DDS's that can be map-reduced. Similar to spark streams.
-- each interval of time,a new DDS is produced.(to be tested)
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
     distribute'  $ Transient.MapReduce.fromList xs




#endif