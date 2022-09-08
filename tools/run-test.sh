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

function run_blc () {
    filepath=$1
    ( cat $LAMBDALISP_BLC | $ASC2BIN; cat $filepath ) | $BLC | sed -e '1s/> //'
}

function run_ulamb () {
    filepath=$1
    ( cat $LAMBDALISP_ULAMB | $ASC2BIN; cat $filepath ) | $ULAMB | sed -e '1s/> //'
}

function run_lazyk () {
    filepath=$1
    cat $filepath | $LAZYK $LAMBDALISP_LAZYK -u | sed -e '1s/> //'
}

function run_sbcl () {
    filepath=$1
    sbcl --script $filepath
}

function show_error () {
    target_name=$3
    echo "Outputs differ on $filepath:"
    echo "${target_name}:"
    echo $1
    echo "SBCL:"
    echo $2
    echo "The test has failed on $filepath."
    if [[ "$target_name" == "BLC" ]]; then
        echo "If the interpreter uni or Blc exits with a segmentation fault, the interpreter may be compiled with the defualt memory usage configurations."
        echo "Compiling uni/Blc with an extended memory usage setting may avoid the segmentation fault."
    fi
    exit 1
}

function foreach_example () {
    runner=$1
    target_name=$2
    for filename in $(ls $EXAMPLES | grep -e ".cl$"); do
        filepath="${EXAMPLES}/$filename"
        echo "Comparing $filepath..."

        echo "Running ${target_name}..."
        target_result=$($runner $filepath)
        echo "Running SBCL..."
        sbcl_result=$(run_sbcl $filepath)

        cmp <(echo "$target_result") <(echo "$sbcl_result") || show_error "$target_result" "$sbcl_result" "$target_name"

        echo "The outputs match."
    done
    echo "All tests have passed."

}


if [[ "$target" == "ulamb" ]]; then
    foreach_example run_ulamb "Universal Lambda" 
elif [[ "$target" == "lazyk" ]]; then
    foreach_example run_lazyk "Lazy K"
else
    # Run the BLC test by default
    foreach_example run_blc "BLC"
fi
