#!/usr/bin/env ./execthirdline.sh

-- set -e &&  port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v /c/Users/magocoal/OneDrive/Haskell/devel:/devel agocorona/transient:05-02-2017  bash -c "runghc  -j2 -isrc -i/devel/transient/src -i/devel/transient-universe/src -i/devel/ghcjs-hplay/src -i/devel/ghcjs-perch/src /devel/transient-universe/tests/$1 $2 $3 $4"

-- compile it with ghcjs and  execute it with runghc
-- set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work agocorona/transient:05-02-2017  bash -c "runghc /work/${1} ${2} ${3}"

{- execute as ./api.hs  -p start/<docker ip>/<port>

 invoque: curl http://<docker ip>/<port>/api/hello/john
          curl http://<docker ip>/<port>/api/hellos/john
-}

import Transient.Internals
import Transient.Move
import Transient.Move.Utils
import Transient.Indeterminism
import Control.Applicative
import Transient.Logged
import Control.Concurrent(threadDelay)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as BSS

main = keep $  initNode   apisample

apisample= api $ gets <|> posts
    where
    posts= do
       received POST
       postParams <- param
       liftIO $ print (postParams :: PostParams)
       return $ BS.pack "received"

    gets= received GET >>  hello <|> hellostream
    hello= do
        received "hello"
        name <- param
        let msg=  "hello " ++ name ++ "\n"
            len= length msg
        return $ BS.pack $ "HTTP/1.0 200 OK\nContent-Type: text/plain\nContent-Length: "++ show len
                 ++ "\nConnection: close\n\n" ++ msg


    hellostream = do
        received "hellos"
        name <- param
        header <|> stream name
        where
        header=async $ return $ BS.pack $
                       "HTTP/1.0 200 OK\nContent-Type: text/plain\nConnection: close\n\n"++
                       "here follows a stream\n"
        stream name= do
            i <- threads 0 $ choose [1 ..]
            liftIO $ threadDelay 1000000
            return . BS.pack $ " hello " ++ name ++ " "++ show i
