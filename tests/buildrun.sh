#!/bin/bash





set -e



ghcjs  -j2 -isrc -i../transient/src -i../transient-universe/src -i../axiom/src -i../ghcjs-perch/src $1 -o static/out


runghc  -i../transient/src -i../transient-universe/src -i../axiom/src -i../ghcjs-perch/src $1 $2 $3 $4
