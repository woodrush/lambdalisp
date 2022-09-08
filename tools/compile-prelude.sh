#!/bin/bash
set -e

cd ./src;
prelude_lisp=./prelude.lisp

# Count the number of occurrences of each character
if [[ "$1" == "count-chars" ]]; then
    cat $prelude_lisp \
        | sed -e 's/;.*$//g' \
        | tr "\n" " " \
        | sed -e 's/  */ /g' \
        | sed -e 's/) /)/g' \
        | sed -e 's/ (/(/g' \
        | sed -e 's/ *$//g' \
        | rev \
        | sed -e 's/\(.\)/\1\'$'\n/g' \
        | sort | uniq -c | sort -n

# Optimize for SKI combinator calculus
elif [[ "$1" == "compile-lazyk" ]]; then
    printf '(def-lazy **prelude** ((string-concatenator stdin) ';

    ( cat $prelude_lisp \
        | sed -e 's/;.*$//g' \
        | tr "\n" " " \
        | sed -e 's/  */ /g' \
        | sed -e 's/) /)/g' \
        | sed -e 's/ (/(/g' \
        | sed -e 's/ *$//g' \
        | rev \
        | sed -e 's/"/!/g' \
        | sed -e 's/\(.\)/"*\1" /g' \
        | sed -e 's/\\/\\\\/g' \
        | sed -e 's/!/\\"/g' \
        | sed -e 's/\?/\\\\n/g' \
        | sed -e 's/~/tilde/g' \
        | tr -d "\n"; printf "nil"; )

    echo '))';

# Compile to binary lambda calculus notation
else
    printf '(def-lazy **prelude** ((string-concatenator stdin) ';

    ( cat $prelude_lisp \
        | sed -e 's/;.*$//g' \
        | tr "\n" " " \
        | sed -e 's/  */ /g' \
        | sed -e 's/) /)/g' \
        | sed -e 's/ (/(/g' \
        | sed -e 's/ *$//g' \
        | rev \
        | sed -e 's/"/!/g' \
        | sed -e 's/\(.\)/"\1" /g' \
        | sed -e 's/\\/\\\\/g' \
        | sed -e 's/!/\\"/g' \
        | sed -e 's/\?/\\\\n/g' \
        | sed -e 's/~/tilde/g' \
        | tr -d "\n"; printf "nil"; )

    echo '))';
fi
