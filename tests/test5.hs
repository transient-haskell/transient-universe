import    Prelude hiding (div)
import    Transient.Base
import    GHCJS.HPlay.View
import    Transient.Move
import    Data.Dynamic
import    Data.String
import    Control.Monad.IO.Class

fs= fromString

main= keep . initNode $ onBrowser $ do
     loginput <- login
     helloworld
     v<-  atRemote $ validatep loginput         -- validation at the server,  using implicit websockets
     displayResult  v
     mouse

login :: Cloud (String,String)
login= local . render $  (,) <$> inputString Nothing <++ br
                             <*> inputPassword <++  br
                             <** submitButton "login" `fire` OnClick

helloworld :: Cloud ()
helloworld=   local . render $ rawHtml $ h1 "hello world"

validatep :: (String,String) -> Cloud Bool
validatep (u,p)=  if u== "foo" && p == "bar" then return True else return False

displayResult :: Bool -> Cloud()
displayResult v= local . render $ rawHtml $ toElem $ "Login result was: " ++ show v

mouse :: Cloud ()
mouse= local $ do
   render $  wraw (div  ! style (fs "height:100px;background-color:lightgreen;position:relative")
                                   $ h1 "Mouse events here")
                            `fire` OnMouseOut
                            `fire` OnMouseOver
                            `fire` OnMouseDown
                            `fire` OnMouseMove
                            `fire` OnMouseUp
                            `fire` OnClick
                            `fire` OnDblClick

   EventData evname ev <- norender getEventData
   render $ rawHtml $ p (evname, fromDynamic ev :: Maybe EvData)
