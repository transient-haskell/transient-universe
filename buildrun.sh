#!/bin/bash



set -e


stack exec --allow-different-user ghcjs --  -j2 -isrc -i../transient/src -i../transient-universe/src -i../ghcjs-hplay/src -i../ghcjs-perch/src $1 -o static/out
stack exec --allow-different-user ghc -- -j2 -isrc -i../transient/src -i../transient-universe/src -i../ghcjs-hplay/src -i../ghcjs-perch/src $1 $2 $3 $4
