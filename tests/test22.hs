import Transient.Base
import Transient.Move
import Transient.Move.Utils

main = keep $ initNode $ 
  localIO $ putStrLn "hello world"
