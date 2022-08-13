#!/bin/bash
set -e

wrapper=$1
file=$2

code=$(cat $file | tr -d "\n")

code=$(printf $code | sed s/\`/01/g)
code=$(printf $code | sed s/s/00000001011110100111010/g)
code=$(printf $code | sed s/k/0000110/g)
code=$(printf $code | sed s/i/0010/g)

sed -f - $wrapper << EOF
s/\\[program\\]/$code/g
EOF
