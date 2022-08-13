#!/bin/bash
set -e


file=$1
wrapper=$2
iotasyntax=$3

if [ -z "$wrapper" ]; then
    wrapper="./wrapper.blc"
fi


code=$(cat $file | tr -d "\n")

if [ ! -z "$iotasyntax" ]; then
    code=$(printf $code | sed s/\*/01/g)
    code=$(printf $code | sed s/i/00010110000000010111101001110100000110/g)
else
    code=$(printf $code | sed s/\`/01/g)
    code=$(printf $code | sed s/s/00000001011110100111010/g)
    code=$(printf $code | sed s/k/0000110/g)
    code=$(printf $code | sed s/i/0010/g)
fi

sed -f - $wrapper << EOF
s/\\[program\\]/$code/g
EOF
