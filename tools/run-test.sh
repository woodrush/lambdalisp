#!/bin/bash
set -e

BLC=../uni
ASC2BIN=../asc2bin.com
SBCL=sbcl
LAMBDALISP_BLC=./lambdalisp.blc
EXAMPLES=./examples

function show_error () {
    echo "Outputs differ on $filepath:"
    echo "BLC:"
    echo $1
    echo "SBCL:"
    echo $2
    echo "The test has failed on $filepath."
    echo "If the interpreter uni or Blc exits with a segmentation fault, the interpreter may be compiled with the defualt memory usage configurations."
    echo "Compiling uni/Blc with an extended memory usage setting may avoid the segmentation fault."
    exit 1
}

function compare_blc_sbcl () {
    filepath=$1
    echo "Running BLC..."
    blc_result=$( ( cat $LAMBDALISP_BLC | $ASC2BIN; cat $filepath ) | $BLC | sed -e '1s/> //')
    echo "Running SBCL..."
    sbcl_result=$(sbcl --script $filepath)
    cmp <(echo "$blc_result") <(echo "$sbcl_result") || show_error "$blc_result" "$sbcl_result"
}


for filename in $(ls $EXAMPLES | grep -e ".cl$"); do
    filepath="${EXAMPLES}/$filename"
    echo "Comparing $filepath..."
    compare_blc_sbcl $filepath
    echo "The outputs match."
done
echo "All tests have passed."
