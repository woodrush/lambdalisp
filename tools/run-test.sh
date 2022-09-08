#!/bin/bash
set -e

target=$1
BLC=../uni
ULAMB=../clamb/clamb
LAZYK=../lazyk/lazyk
ASC2BIN=../asc2bin.com
SBCL=sbcl
LAMBDALISP_BLC=./lambdalisp.blc
LAMBDALISP_ULAMB=./lambdalisp.ulamb
LAMBDALISP_LAZYK=./lambdalisp.lazy
EXAMPLES=./examples

function show_error () {
    echo "Outputs differ on $filepath:"
    echo "BLC:"
    echo $1
    echo "SBCL:"
    echo $2
    echo "The test has failed on $filepath."
    if [[ "$3" == "blc" ]]; then
        echo "If the interpreter uni or Blc exits with a segmentation fault, the interpreter may be compiled with the defualt memory usage configurations."
        echo "Compiling uni/Blc with an extended memory usage setting may avoid the segmentation fault."
    fi
    exit 1
}

function compare_blc_sbcl () {
    filepath=$1
    echo "Running BLC..."
    blc_result=$( ( cat $LAMBDALISP_BLC | $ASC2BIN; cat $filepath ) | $BLC | sed -e '1s/> //')
    echo "Running SBCL..."
    sbcl_result=$(sbcl --script $filepath)
    cmp <(echo "$blc_result") <(echo "$sbcl_result") || show_error "$blc_result" "$sbcl_result" blc
}

function compare_ulamb_sbcl () {
    filepath=$1
    echo "Running Universal Lambda..."
    blc_result=$( ( cat $LAMBDALISP_ULAMB | $ASC2BIN; cat $filepath ) | $ULAMB | sed -e '1s/> //')
    echo "Running SBCL..."
    sbcl_result=$(sbcl --script $filepath)
    cmp <(echo "$blc_result") <(echo "$sbcl_result") || show_error "$ulamb_result" "$sbcl_result"
}

function compare_lazyk_sbcl () {
    filepath=$1
    echo "Running Universal Lambda..."
    blc_result=$( cat $filepath | $LAZYK $LAMBDALISP_LAZYK -u | sed -e '1s/> //')
    echo "Running SBCL..."
    sbcl_result=$(sbcl --script $filepath)
    cmp <(echo "$blc_result") <(echo "$sbcl_result") || show_error "$lazyk_result" "$sbcl_result"
}


for filename in $(ls $EXAMPLES | grep -e ".cl$"); do
    filepath="${EXAMPLES}/$filename"
    echo "Comparing $filepath..."
    if [[ "$target" == "ulamb" ]]; then
        compare_ulamb_sbcl $filepath
    elif [[ "$target" == "lazyk" ]]; then
        compare_lazyk_sbcl $filepath
    else
        compare_blc_sbcl $filepath
    fi

    echo "The outputs match."
done
echo "All tests have passed."
