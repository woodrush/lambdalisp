#!/bin/bash
set -e


file=$1
wrapper=$2

if [ -z "$wrapper" ]; then
    wrapper="./wrapper.blc"
fi



code=$(cat $file | tr -d "\n")

code=$(printf $code | sed s/\*/01/g)
code=$(printf $code | sed s/i/00010110000000010111101001110100000110/g)


sed -f - $wrapper << EOF
s/\\[program\\]/$code/g
EOF
