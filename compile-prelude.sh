( cat lazy-lambda.lisp | sed -e 's/;.*$//g' | tr "\n" " " | sed -e 's/  */ /g' | sed -e 's/) /)/g' | sed -e 's/ *$//g' | rev | sed -e 's/"/!/g' | sed -e 's/\(.\)/"\1" /g' | sed -e 's/!/\\"/g' | sed -e 's/\?/\\\\n/g' | sed -e 's/~/tilde/g'; printf "nil"; )

# cat lazy-lambda.lisp | sed -e 's/;.*$//g' | tr "\n" " " | sed -e 's/  */ /g' | sed -e 's/) /)/g' | sed -e 's/\(.\)/\1\'$'\n/g' | sort | uniq -c | sort -n | less
