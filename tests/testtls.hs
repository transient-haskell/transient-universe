#!/usr/bin/env ./execthirdline.sh
--  execute it with runghc
-- set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work testtls  bash -c "runghc /work/${1} ${2} ${3}"


-- set -e &&  port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v /c/Users/magocoal/OneDrive/Haskell/devel:/devel testtls  bash -c "mkdir -p static && ghcjs  -j2 -isrc -i/devel/transient/src -i/devel/transient-universe/src -i/devel/ghcjs-hplay/src -i/devel/ghcjs-perch/src /devel/transient-universe/tests/$1 -o static/out && runghc  -j2 -isrc -i/devel/transient/src -i/devel/transient-universe/src -i/devel/ghcjs-hplay/src -i/devel/ghcjs-perch/src /devel/transient-universe/tests/$1 $2 $3 $4"


{-# LANGUAGE   CPP, ScopedTypeVariables, DeriveDataTypeable, BangPatterns, OverloadedStrings #-}

import Transient.Base
import Transient.EVars
import Transient.Move
import Transient.Move.Utils
import Transient.Backtrack
import Transient.Indeterminism
import Transient.Internals
import Transient.Logged
import Data.Typeable
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import System.Directory
import System.Random
import Control.Exception as E

import Network  hiding(accept)
import Network.Socket as NSS
import Network.Socket.ByteString as NS
--import Network.Socket (accept)
import Network.TLS as TLS


import           Network.TLS.Extra                  as TLSExtra
import qualified Crypto.Random.AESCtr               as AESCtr

import qualified Network.Socket.ByteString.Lazy     as SBSL

import qualified Data.ByteString.Lazy               as BL
import qualified Data.ByteString.Lazy.Char8         as BL8
import qualified Data.ByteString.Char8              as B
import qualified Data.ByteString                    as BE
import qualified Data.Certificate.X509              as X
import qualified Data.X509.CertificateStore         as C
import Data.Default


import Debug.Trace

(!>) a b = trace b a

sendTLSData = TLS.sendData


recvTLSData = TLS.recvData


maybeTLSServerHandshake _ _ = return ()

--initNode $ Cloud $ do
--  ParseContext _ input <- getSData <|> error "parse"
--  Connection _(Just (Node2Node _ sock _)) _ _ _ _ _ _  <- getSData <|> error "conn"

-- cabal install tls cprng-aes  certificate

main= keep $  do

  sock <- liftIO . listenOn  $ PortNumber 8080
  (sock,addr) <- waitEvents $ NSS.accept sock                       !>  "wait"
  input <-  liftIO $ SBSL.getContents sock                          !>  "read"

  let fmsg msg= "HTTP/1.0 200 OK\nContent-Type: text/plain\nContent-Length: "
                 ++ show (length msg)
                 ++ "\nConnection: close\n\n" ++ msg

  hs <-maybeTLSHandshake sock input
  case hs of
    Just ctx -> liftIO $ TLS.sendData ctx $ BL8.pack $ fmsg "TLS"
    Nothing -> do

    liftIO $ NS.sendAll sock $ B.pack $ fmsg "NOTLS"
    return ()

maybeTLSServerHandshake sock input=
 if ((not $ BL.null input) && BL.head input  == 0x16)
   then  do
    ctx <- liftIO $ do
          cred <- either error id <$>TLS.credentialLoadX509
                 "/work/certificate.pem"
                 "/work/key.pem"
          let sp = makeServerSettings    cred                                       !> "TLS"
          ctx <- makeServerContext sp sock  input
          TLS.handshake ctx
          return ctx
      -- xxxx save context in connection a¤adir msend
    onFinish $ const $  liftIO $ TLS.contextClose ctx
    return $ Just ctx
   else return Nothing

clientTLSHandshake hostname sock input= do
   ctx <- liftIO $ do
       sp <- makeClientSettings hostname
       ctx <- makeCLientContext sp sock
       TLS.handshake ctx
       -- save..
   onFinish $ const $  liftIO $ TLS.contextClose ctx


makeClientSettings hostname= ClientParams{
         TLS.clientUseMaxFragmentLength= Nothing
      ,  TLS.clientServerIdentification= (hostname,"")
      ,  TLS.clientUseServerNameIndication = False
      ,  TLS.clientWantSessionResume = Nothing
      ,  TLS.clientShared  = def
      ,  TLS.clientHooks = def
      ,  TLS.clientSupported = supported
      }
makeServerSettings credential = def { -- TLS.ServerParams
        TLS.serverWantClientCert = False
      , TLS.serverCACertificates = []
      , TLS.serverDHEParams      = Nothing
      , TLS.serverHooks          = hooks
      , TLS.serverShared         = shared
      , TLS.serverSupported      = supported
      }
    where
    -- Adding alpn to user's tlsServerHooks.
    hooks =  def
--    TLS.ServerHooks {
--        TLS.onALPNClientSuggest = TLS.onALPNClientSuggest tlsServerHooks <|>
--          (if settingsHTTP2Enabled set then Just alpn else Nothing)
--      }

    shared = def {
        TLS.sharedCredentials = TLS.Credentials [credential]
      }
supported = def { -- TLS.Supported
        TLS.supportedVersions       = [TLS.TLS12,TLS.TLS11,TLS.TLS10]
      , TLS.supportedCiphers        = ciphers
      , TLS.supportedCompressions   = [TLS.nullCompression]
      , TLS.supportedHashSignatures = [
          -- Safari 8 and go tls have bugs on SHA 512 and SHA 384.
          -- So, we don't specify them here at this moment.
          (TLS.HashSHA256, TLS.SignatureRSA)
        , (TLS.HashSHA224, TLS.SignatureRSA)
        , (TLS.HashSHA1,   TLS.SignatureRSA)
        , (TLS.HashSHA1,   TLS.SignatureDSS)
        ]
      , TLS.supportedSecureRenegotiation = True
      , TLS.supportedClientInitiatedRenegotiation = False
      , TLS.supportedSession             = True
      , TLS.supportedFallbackScsv        = True
      }
ciphers :: [TLS.Cipher]
ciphers =
    [ TLSExtra.cipher_ECDHE_RSA_AES128GCM_SHA256
    , TLSExtra.cipher_ECDHE_RSA_AES128CBC_SHA256
    , TLSExtra.cipher_ECDHE_RSA_AES128CBC_SHA
    , TLSExtra.cipher_DHE_RSA_AES128GCM_SHA256
    , TLSExtra.cipher_DHE_RSA_AES256_SHA256
    , TLSExtra.cipher_DHE_RSA_AES128_SHA256
    , TLSExtra.cipher_DHE_RSA_AES256_SHA1
    , TLSExtra.cipher_DHE_RSA_AES128_SHA1
    , TLSExtra.cipher_DHE_DSS_AES128_SHA1
    , TLSExtra.cipher_DHE_DSS_AES256_SHA1
    , TLSExtra.cipher_AES128_SHA1
    , TLSExtra.cipher_AES256_SHA1
    ]

makeClientContext params sock = liftIO $ do
     TLS.contextNew backend params

-- | Make a server-side TLS 'Context' for the given settings, on top of the
-- given TCP `Socket` connected to the remote end.
makeServerContext :: MonadIO m => TLS.ServerParams -> Socket -> BL.ByteString  -> m Context
makeServerContext params sock input= liftIO $ do
    inputBuffer <- newIORef input
    TLS.contextNew (backend inputBuffer)  params


    where
    backend inputBuffer= TLS.Backend {
        TLS.backendFlush = return ()
      , TLS.backendClose = NSS.close sock
      , TLS.backendSend  = sendAll' sock
      , TLS.backendRecv  =  \n -> do
          input <- readIORef inputBuffer
          let (res,input')= BL.splitAt (fromIntegral n) input
          writeIORef inputBuffer input'
          return $ toStrict res
      }
    recvAll input= step B.empty
       where step !acc 0 = return acc
             step !acc n = do
                bs <- NS.recv sock n
                step (acc `B.append` bs) (n - B.length bs)

    toStrict s= BE.concat $ BL.toChunks s :: BE.ByteString

    sendAll' sock bs = NS.sendAll sock bs `E.catch` \(SomeException e) ->
        throwIO e
