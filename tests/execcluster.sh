set -e
# compile=`sed -n '3p' ${1} | sed 's/-- //'`
# execute=`sed -n '4p' ${1} | sed 's/-- //'`


# compile with ghcjs and ghc, run a cluster of N nodes: <source.hs> -p start/<host>/<port>  N

compile (){
    docker run  -v $(pwd):/devel agocorona/transient:24-03-2017  bash -c "cd /devel && ghcjs -DGHCJS_BROWSER  $1 -o static/out  && ghc -O -threaded -rtsopts  -j2 $1"
}

compile_no_ghcjs (){
   docker run  -v $(pwd):/devel agocorona/transient:24-03-2017  bash -c "cd /devel && ghc -O -threaded -rtsopts  -j2 $1"
}

execute() {
    docker run -p ${port}:${port} -v $(pwd):/devel agocorona/transient:24-03-2017 bash -c "cd devel && $executable -p start/${host}/$port/add/${host}/$baseport/y +RTS -N"
}

executeone(){
   docker run -p ${port}:${port} -v $(pwd):/devel agocorona/transient:24-03-2017 bash -c "cd devel && $1 $2 $3"
}

# compile with ghcjs and ghc with develop. libraries, run a cluster of N nodes: <source.hs> -p start/<host>/<port>  N

compiled() {
    docker run  -v /c/Users/magocoal/OneDrive/Haskell/devel:/devel agocorona/transient:24-03-2017  bash -c "cd /devel/transient-universe-tls/tests && mkdir -p static && ghcjs -DGHCJS_BROWSER --make  -j2 -isrc -i/devel/transient/src -i/devel/transient-universe/src -i/devel/transient-universe-tls/src -i/devel/axiom/src -i/devel/ghcjs-perch/src $1 -o static/out && ghc -O -threaded -rtsopts --make -j2 -isrc -i/devel/transient/src -i/devel/transient-universe/src -i/devel/transient-universe-tls/src -i/devel/axiom/src -i/devel/ghcjs-perch/src $1"
}


nnodes=$4

re='^[0-9]+$'
if ! [[ $nnodes =~ $re ]] ; then
   nnodes=1
fi

host=`echo ${3} | awk -F/ '{print $(2)}'`
baseport=`echo ${3} | awk -F/ '{print $(3)}'`
finalport=`expr $baseport + $nnodes`
port=$baseport
executable=./$(basename $1 .hs)

echo "compiling"
compile_no_ghcjs $1

echo executing $nnodes nodes
if [ $nnodes -eq 1 ]
  then
    $executeone $executable $2 $3
  else
    while [ "$port" -lt "$finalport" ]
      do
        execute $executable & # >> log${port}.log  &
        sleep 1
        ((port++))
      done
fi
echo "done"


