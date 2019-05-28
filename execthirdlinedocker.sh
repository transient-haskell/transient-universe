#!/bin/bash
command=`sed -n '3p' ${1} | sed 's/-- //'`
echo $command
if [ -f /.dockerenv ]; then
    eval $command $1 $2 $3 $4
else
    set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work agocorona/transient:22-05-2018  bash -c "cd /work  &&  $1 $2 $3 $4"
fi
