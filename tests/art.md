**More on parallel parsing and mixing effects: Do NOT separate effects!! **

I had to use the transient parser a lot more with the new serialization mechanism, which uses byteString builders. In the process I realized how convenient and necessary
is to mix parsing with other effects in orde to have composability (i.e. clean code).

A server is a parser of  streams of requests which perform effects and produce responses. To create a server with nice applicative operators obviously follows that you need conbinators that mix parsing with other effects. Thread management, among others is also a very, very convenient in a server.

For that reason every developer and every other library, even functional ones, give up in the effor of achieving composability in presence of effects such is threading, streaming and communications. But Transient does compose these effects.

An example is the parser of requests in node-node communications within Transient-universe. For historical reason `parallelReadHandler` is its name. That method perform a double level parsing. for one side it cuts individual chunks of a given length, each one contains one request. the chunks are extracted from an infinite stream, which is the socket connection.
Once a chunk is separated, a new thread is initiated that parses it in parallel. The parsed data should be given to the request processor within the created thread.

 
 ```haskell
 parallelReadHandler :: Loggable a => TransIO (StreamData a)
 parallelReadHandler= many' extractPacket
     where
     many' p= p <|> many' p
     extractPacket= do   
         len <- integer <|> (do s <- getParseBuffer; error $ show $ ("malformed data received: expected Int, received: ", s))
         chunk <- tTake (fromIntegral len)
         abduce         -- here a new thread is created which take over the rest of the task
         setParseString chunk  
         deserialize
 ```
 
 The use of that parser is:
 
 ```haskell
 do 
  <stream initialization>
  request <- parallelReadHandler
  process request
  return response
 ```
 
 `process` is executed once each request is parsed, in a new thread. 
 
 `abduce`  is where each new thread is created. There, the execution is forked . In the current thread  `extractPacket` execute empty, so a new `extractPacket` is executed, due to `many' p`. in the new thread, the processing continue: It set a new parse context, which is the chunk, which is deserialized using a parse expression (not detailed here).  
 
`integer`  `tTake`  and `deserialize` are parsers and they are mixed seamlessly with all the effect logic.
 
As i say, the mixing of parsing with other effects is beyond fancy. It is absolutely necessary for composition. 
 
Another case it is in parsing REST request. At last, as I said, a server is a parser  from request which perform effects and return responses. if you want to codify a server as such with a combination of applicative terms, you need a parser  which perform effects: read databases, manage threads etc 
 
 Tha [api example](https://github.com/transient-haskell/transient-universe/blob/master/tests/api.hs) is an example.  `received` and `param` are polymorphic parsers which extract a segment of the REST path.

 The same happens for logging; The log of a closure which is sent and deserialized in the destination node needs to be deserialized (parsed) one value at a time, interleaved with other effects while the code is executed:
 
```haskell
do
    local this
    local that
```

`local  this`  either deserialize the return value from the log or execute his parameter if there is no remaining log data. The next `local` do the same, using the rest of the log. It would be impossible to manage it without mixing parsing with other effects. 

The library Attoparsec need his own monad and need an special Partial data to resume parsing. it is though to be the main task that is feed with data by the rest of the effects during its execution. But parsing can be just one detail embedded within a lot of other processing. I neeed the parser to feed itself with the data necessary, by performing other effects. A stream in transient has this definition:

```haskell
data ParseContext  = ParseContext (IO  (StreamData ByteString)) ByteString

parseContext= ParseContext  readMoreData   remainingBufferStillNotParsed
```

Each parse combinator read more data -if necessary- from the stream and leave the leftover in the buffer for the next combinator.



The idea of running one effect at a time seems crazy to me. 
 

