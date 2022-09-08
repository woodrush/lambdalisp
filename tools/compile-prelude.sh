#!/bin/sh
set -e

prelude_lisp="./src/prelude.lisp"

if [ $# -eq 0 ]; then
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
        | sed -e 's/~/tilde/g'; printf "nil"; )

    echo '))';
else
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
fi
