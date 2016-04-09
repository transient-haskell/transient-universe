ghcjs -isrc -i../transient/src -i../ghcjs-hplay/src -i../ghcjs-perch/src %1 -o static/out
if %errorlevel% neq 0 exit
runghc -isrc -i../transient/src -i../ghcjs-hplay/src -i../ghcjs-perch/src %1

