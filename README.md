# LambdaLisp


Common Lisp Compatible:
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

## Supported Interpreters

| Language               | Extension | Engine                  | Program Format               |
|------------------------|-----------|-------------------------|------------------------------|
| Binary Lambda Calculus | *.blc     | Untyped Lambda Calculus | Binary (asc2bin can be used) |
| Universal Lambda       | *.ulamb   | Untyped Lambda Calculus | Binary (asc2bin can be used) |
| Lazy K                 | *.lazy    | SKI Combinator Calculus | ASCII                        |

| Interpreter      | Language               | Platforms    | Build Command | Notes                           |
|------------------|------------------------|--------------|---------------|---------------------------------|
| Blc              | Binary Lambda Calculus | x86-64-Linux | `make blc`    | 521-byte interpreter            |
| tromp            | Binary Lambda Calculus | Any          | `make tromp`  | IOCCC 2012 "Most functional"    |
| uni              | Binary Lambda Calculus | Any          | `make uni`    | Unobfuscated version of `tromp` |
| clamb            | Universal Lambda       | Any          | `make clamb`  | Fast UL interpreter             |
| lazyk            | Lazy K                 | Any          | `make lazyk`  | Fast Lazy K interpreter         |

## Usage
To build all interpreters:

```sh
make interpreters
```

Or, to build them individually:
```sh
make blc tromp uni clamb lazyk
```


Running `cat -` connects the standard input after the specified input files,
allowing the user to interact with the interpreter through the terminal after reading a file.
If `cat -` does not work, the following commands can be used instead:
```sh
```


Running LambdaLisp on Binary Lambda Calculus interpreters:
```sh
# Prepare the binary
cat lambdalisp.blc | asc2bin > lambdalisp.blc.bin

cat lambdalisp.blc.bin - | ./bin/Blc                   # Run the LambdaLisp REPL
cat lambdalisp.blc.bin [filepath] | ./bin/Blc          # Run a LambdaLisp script and exit
cat lambdalisp.blc.bin [filepath] - | ./bin/Blc        # Run a LambdaLisp script, then enter the REPL
( cat lambdalisp.blc.bin [filepath]; cat ) | ./bin/Blc # If `cat -` does not work, the following command can be used:
```

Running LambdaLisp on Universal Lambda interpreters:
```sh
# Prepare the binary
cat lambdalisp.ulamb | asc2bin > lambdalisp.ulamb.bin

cat lambdalisp.ulamb.bin - | ./bin/clamb            # Run the LambdaLisp REPL
cat lambdalisp.ulamb.bin [filepath] | ./bin/clamb   # Run a LambdaLisp script and exit
cat lambdalisp.ulamb.bin [filepath] - | ./bin/clamb # Run a LambdaLisp script, then enter the REPL
```

Running LambdaLisp on Lazy K:
```sh
./bin/lazyk lambdalisp.lazy -u                    # Run the LambdaLisp REPL. The -u option is required for interactive programs
cat [filepath] | ./bin/lazyk lambdalisp.lazy -u   # Run a LambdaLisp script and exit
cat [filepath] - | ./bin/lazyk lambdalisp.lazy -u # Run a LambdaLisp script, then enter the REPL
```

