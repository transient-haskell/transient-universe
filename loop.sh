#!/bin/bash 
COUNTER=0
while [ true ]; do
             echo $1 $COUNTER
             let COUNTER=COUNTER+1 
             sleep 4
done
