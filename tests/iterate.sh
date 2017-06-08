#!/bin/bash
set -e
ghc   -j2 -isrc -i../transient/src -i../transient-universe/src -i../ghcjs-hplay/src -i../ghcjs-perch/src tests/hasrocket.hs  -O2 -threaded -rtsopts "-with-rtsopts=-N -A64m -n2m"
./tests/hasrocket -p start/localhost/8080 &
sleep 2
../websocket-shootout/bin/websocket-bench broadcast ws://127.0.0.1:8080/ws -c 4 -s 40 --step-size 100
