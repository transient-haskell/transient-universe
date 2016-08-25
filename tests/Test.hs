import Control.Concurrent.Async
import Control.Concurrent

newtype Stream a = Stream{ runStream :: IO [Async a]}

instance Functor Stream where
  fmap f (Stream mxs) = Stream $ do
     xs <- mxs
     return [fmap f x | x <- xs]

instance Applicative Stream where
  pure x= Stream $ do
            z <- async $ return x
            return [z]

  (Stream mfs) <*> (Stream mas) = Stream $do
      as <- mas
      fs <- mfs
      sequence [
       async $ ( wait f) <*> ( wait a)
          | f <- fs, a <- as]

instance Monad Stream where
  return = pure
  (Stream mxs) >>= f = Stream $ do
    xs <- mxs
    rs <- mapM wait xs
    rr <- sequence [  runStream $ f r | r <- rs]
    return $ concat rr



stream :: [IO a] -> Stream a
stream ioa= Stream  $  mapM async ioa

waitStream :: Stream a -> IO [a]
waitStream  (Stream mxs)= do
   xs <- mxs
   mapM wait xs


main= do
    r <- waitStream $ stream $ map return [1..10]
    print r
    r <- waitStream $ do
          x <-  stream $ map (\x -> do threadDelay 1000000; return x) [1..100]
          return $ 2 * x
    print r

    where
    fact 0 = 1
    fact n= n * fact (n -1)

