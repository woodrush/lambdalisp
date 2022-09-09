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

### Building the Interpreters
To build all interpreters:

```sh
make interpreters
```

Or, to build them individually:
```sh
make blc tromp uni clamb lazyk
```

- `blc`
- `tromp`
- `uni`
- `clamb`: `git clone https://github.com/irori/clamb`
- `lazyk`: `git clone https://github.com/irori/lazyk`

#### Building Blc
Blc only runs on x86-64-Linux.
For other platforms, tromp or uni can be used.

#### Building tromp on a Mac
Mac has `gcc` installed by default or via Xcode Command Line Tools.
However, `gcc` is actually installed as an alias to `clang`, which is a different compiler.
This is confirmable by running `gcc --version`. On my Mac, running it shows:

```sh
$ gcc --version
Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/include/c++/4.2.1
Apple clang version 12.0.0 (clang-1200.0.32.29)
Target: x86_64-apple-darwin19.6.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
```

One way to circumvent this is to use `uni` instead, which is an unobfuscated version of `tromp` and is compilable by clang.
To build `tromp`, first install gcc via [Homebrew](https://brew.sh/):

```sh
brew install gcc
```

Currently, this should install the command `gcc-11`.
After installing gcc, check the command that it has installed.

Then, edit the `Makefile`'s `CC` configuration:

```diff
- CC=cc
+ CC=gcc-11
```

Then, running
```sh
make tromp
```
will compile `tromp`.


### Running LambdaLisp

Running LambdaLisp on the Binary Lambda Calculus interpreter `Blc` can be done as follows.
The following commands are for running on `Blc`.
To run on `tromp` or `uni`, replace `Blc` with `tromp` or `uni`.
```sh
# Prepare the binary
cat lambdalisp.blc | ./bin/asc2bin > lambdalisp.blc.bin

cat lambdalisp.blc.bin - | ./bin/Blc                   # Run the LambdaLisp REPL
cat lambdalisp.blc.bin [filepath] | ./bin/Blc          # Run a LambdaLisp script and exit
cat lambdalisp.blc.bin [filepath] - | ./bin/Blc        # Run a LambdaLisp script, then enter the REPL
```

Running `cat -` connects the standard input after the specified input files,
allowing the user to interact with the interpreter through the terminal after reading a file.
If `cat -` does not work, the following command can be used instead:

```sh
( cat lambdalisp.blc.bin [filepath]; cat ) | ./bin/Blc
```

Running LambdaLisp on the Universal Lambda interpreter `clamb` can be done as follows.
Note that `lambdalisp.ulamb` and `lambdalisp.blc` are different files although they look similar,
since they are different languages.
This is since the I/O lambda term encoding is different for these languages.
Otherwise, both languages are based entirely on untyped lambda calculus.
```sh
# Prepare the binary
cat lambdalisp.ulamb | ./bin/asc2bin > lambdalisp.ulamb.bin

# The -u option is required for managing I/O properly
cat lambdalisp.ulamb.bin - | ./bin/clamb -u            # Run the LambdaLisp REPL
cat lambdalisp.ulamb.bin [filepath] | ./bin/clamb -u   # Run a LambdaLisp script and exit
cat lambdalisp.ulamb.bin [filepath] - | ./bin/clamb -u # Run a LambdaLisp script, then enter the REPL
```

Running LambdaLisp on the Lazy K interpreter `lazyk`:
```sh
# The -u option is required for managing I/O properly
./bin/lazyk lambdalisp.lazy -u                    # Run the LambdaLisp REPL
cat [filepath] | ./bin/lazyk lambdalisp.lazy -u   # Run a LambdaLisp script and exit
cat [filepath] - | ./bin/lazyk lambdalisp.lazy -u # Run a LambdaLisp script, then enter the REPL
```
