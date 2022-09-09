# LambdaLisp

Common Lisp:
- defun, defmacro, lambda (&rest can be used)
- quote, atom, car, cdr, cons, eq
- read, print, format, write-to-string
- let, let*, labels, setq, setf, boundp, defparameter
- progn, loop, block, return, return-from, if, cond, error
- list, append, reverse, length, position, mapcar, reduce
- make-hash-table, gethash (setf can be used)
- equal, and, or, not
- eval, apply, funcall
- intern, stringp, string literals, string concatenation with concatenate
- +, -, *, mod, floor, =, >, <, signed 32-bit integer literals
- set-macro-character, peek-char, read-char, `` ` ``   `,`   `,@`   `'`   `#'`   `#\`

LambdaLisp-exclusive:
- carstr, cdrstr, str, string comparison with =, >, <, string concatenation with +
- defun-local, defglobal, type, macro
- malloc, memread, memwrite
- new, defclass, defmethod, `.`, field rewriting by setf, class inheritance

