{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Main (main) where

import Transient.Base
import Transient.Move
import Transient.Move.Utils

import Data.Monoid

newtype WithIndex v = WithIndex { unWithIndex :: Maybe v } deriving Monoid

class Foldable f => FoldableWithIndex f where
  ifoldMap :: f a -> x
  ifoldMap = undefined


instance Foldable WithIndex where

instance FoldableWithIndex WithIndex where

class A
instance A

class B
instance B

class C
instance C

class HasIndex f where
  keysByIndex :: f a
  findValues :: (A, B, FoldableWithIndex f, C)
             => ()
             -> f b
             -> f b

instance HasIndex WithIndex where
  findValues _ wi = ifoldMap wi


main :: IO ()
main = keep $ initNode $ onBrowser $ lliftIO $ do

  let val = WithIndex mempty :: WithIndex ()
      f val =  (WithIndex $(unWithIndex $ findValues () val)) <>( WithIndex $ (unWithIndex $ findValues () val))
  f val `seq` return ()





