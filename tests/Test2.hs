{-# LANGUAGE DeriveDataTypeable , ExistentialQuantification
    ,ScopedTypeVariables, StandaloneDeriving, RecordWildCards, FlexibleContexts, CPP
    ,GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (div)
import Transient.Base
#ifdef ghcjs_HOST_OS
   hiding ( option,runCloud')
#endif
import GHCJS.HPlay.View
#ifdef ghcjs_HOST_OS
   hiding (map)
#else
   hiding (map, option,runCloud')
#endif

import  Transient.Move  hiding(teleport)
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Monoid
import Data.String

main= simpleWebApp 8081 $ local $  buttons  <|> linksample
    where
    linksample= do
          r <- render $ br ++> wlink "Hi!" (toElem "This link say Hi!")`fire` OnClick
          render $ rawHtml . b  $ " returns "++ r

    buttons= do
           render . rawHtml $ p "Different input elements:"
           radio **> br
               ++> checkButton
               **> br ++> br
               ++> select
               <++ br

    checkButton=do
           rs <- render $  -- getCheckBoxes(
                           ((setCheckBox False "Red"    <++ b "red")   `fire` OnClick)
--                        <> ((setCheckBox False "Green"  <++ b "green") `fire` OnClick)
--                        <> ((setCheckBox False "blue"   <++ b "blue")  `fire` OnClick) --)
           render $ wraw $ fromString " returns: " <> b (show rs)
           empty

    radio= do
           r <- render $ getRadio [fromString v ++> setRadioActive v | v <- ["red","green","blue"]]

           render $ wraw $ fromString " returns: " <> b ( show r )

    select= do
           r <- render $ getSelect (   setOption "red"   (fromString "red")

                          <|> setOption "green" (fromString "green")
                          <|> setOption "blue"  (fromString "blue"))
                  `fire` OnClick

           render $ wraw $ fromString " returns: " <> b ( show r )
