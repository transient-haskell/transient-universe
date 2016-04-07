#!/bin/bash

set -e
ghcjs -isrc -i../transient/src -i../ghcjs-hplay/src -i../ghcjs-perch/src $1 -o static/out
runghc -isrc -i../transient/src -i../ghcjs-hplay/src -i../ghcjs-perch/src $1
