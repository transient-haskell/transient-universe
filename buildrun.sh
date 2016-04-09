#!/bin/bash



set -e


stack exec ghcjs -- -isrc -i../transient/src -i../transient-universe/src -i../ghcjs-hplay/src -i../ghcjs-perch/src $1 -o static/out
stack exec runghc -- -isrc -i../transient/src -i../transient-universe/src -i../ghcjs-hplay/src -i../ghcjs-perch/src $1
