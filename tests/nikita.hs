import Prelude hiding (div, id)
import Transient.Base
import Transient.Move
import GHCJS.HPlay.View
import Control.Applicative
import Data.String
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable

fs= fromString

data AppState = AppState
  { appStateMessage :: Int }
  deriving (Read, Show)

data Action
  = ButtonClicked
  | Stop
  deriving (Read, Show, Eq)

(|>) = flip ($)

initialAppState :: AppState
initialAppState = AppState 0


main= keep $ initNode $ onBrowser $  do
      local . render . rawHtml $ div ! id (fs "appdiv") $ noHtml
      displayState
      app 

app ::  Cloud ()

app  =    do
        action <- displayButton 
        updateState action 
        displayState

        
           
displayButton ::  Cloud Action
displayButton  = local $ render $ wbutton ButtonClicked (fromString "Click me")

displayState=   local $ do
     appState <- getAppState
     render $ at (fs "#appdiv") Insert $ do
            rawHtml (appStateMessage appState |> show |> h1)
 
updateState  ButtonClicked  = local $ do
   AppState v <- getAppState
   setAppState (AppState $ v+1)

getAppState :: TransIO AppState
getAppState= getRData <|>  (setRData initialAppState >> return initialAppState)

setAppState :: AppState -> TransIO ()
setAppState= setRData


---------------------------------------------  State References in the TransIO monad ------------
newtype Ref a = Ref (IORef a)

-- | An state reference that can be updated (similar to STRef in the state monad)
--
-- Initialized the first time it is set.
setRData:: Typeable a => a -> TransIO ()
setRData x= do
     Ref ref <- getSData
     liftIO $ atomicModifyIORef ref $ const (x,())
   <|> do
     ref <- liftIO (newIORef x)
     setData $ Ref ref

getRData :: Typeable a => TransIO a
getRData= do
    Ref ref <- getSData
    liftIO $ readIORef ref
