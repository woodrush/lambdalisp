printf '(def-lazy **prelude** ((string-concatenator stdin) ';
( cat prelude.lisp | sed -e 's/;.*$//g' | tr "\n" " " | sed -e 's/  */ /g' | sed -e 's/) /)/g' | sed -e 's/ *$//g' | rev | sed -e 's/"/!/g' | sed -e 's/\(.\)/"\1" /g' | sed -e 's/!/\\"/g' | sed -e 's/\?/\\\\n/g' | sed -e 's/~/tilde/g'; printf "nil"; )

echo '))';
# cat prelude.lisp | sed -e 's/;.*$//g' | tr "\n" " " | sed -e 's/  */ /g' | sed -e 's/) /)/g' | sed -e 's/\(.\)/\1\'$'\n/g' | sort | uniq -c | sort -n | less
