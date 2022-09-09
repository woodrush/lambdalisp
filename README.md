# LambdaLisp


Common Lisp Compatible:
- defun, defmacro, lambda (&rest can be used)
- quote, atom, car, cdr, cons, eq
- read, print, format, write-to-string
- let, let*, labels, setq, boundp, defparameter
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
Since LambdaLisp is written in lambda calculus, it requires a lambda calculus interpreter for it to be run.
When run on a lambda calculus interpreter that runs on a terminal,
LambdaLisp provides a REPL for interactively defining and evaluating lambda terms,
or can be used to load and run an interactive LambdaLisp program from a file.

LambdaLisp currently runs on 3 lambda-calculus-based languages:
Binary Lambda Calculus (BLC), Universal Lambda, and Lazy K.
Lazy K is a language based on the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
which is equivalent to lambda calculus, where lambda terms and SKI terms have a [consistent method](https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis)
of converting to and from each other.

These 3 languages are supported by 5 interpreters:
Blc, tromp, uni, clamb, and lazyk.
Blc, tromp, uni are all interpreters for Binary Lambda Calculus and has the same I/O format.
Here is a summary of the supported languages and interpreters:

| Language               | Extension | Engine                  | Program Format               |
|------------------------|-----------|-------------------------|------------------------------|
| Binary Lambda Calculus | *.blc     | Untyped Lambda Calculus | Binary (asc2bin can be used) |
| Universal Lambda       | *.ulamb   | Untyped Lambda Calculus | Binary (asc2bin can be used) |
| Lazy K                 | *.lazy    | SKI Combinator Calculus | ASCII                        |

| Interpreter      | Language               | Platforms    | Build Command | Author                             | Notes                           |
|------------------|------------------------|--------------|---------------|------------------------------------|---------------------------------|
| Blc              | Binary Lambda Calculus | x86-64-Linux | `make blc`    | [@jart](https://github.com/jart)   | 521-byte interpreter            |
| tromp            | Binary Lambda Calculus | Any          | `make tromp`  | [@tromp](https://github.com/tromp) | IOCCC 2012 "Most functional"    |
| uni              | Binary Lambda Calculus | Any          | `make uni`    | [@tromp](https://github.com/tromp) | Unobfuscated version of `tromp` |
| clamb            | Universal Lambda       | Any          | `make clamb`  | [@irori](https://github.com/irori) | Fast UL interpreter             |
| lazyk            | Lazy K                 | Any          | `make lazyk`  | [@irori](https://github.com/irori) | Fast Lazy K interpreter         |


## Usage
Running LambdaLisp first requires building an untyped lambda calculus interpreter of your choice.

### Building the Interpreters
Among the 3 BLC interpreters, `Blc` can be run on x86-64-Linux systems,
and `tromp` may not compile on a Mac with the defualt gcc (which is actually an alias of clang - details are provided below).
The most reliably compilable BLC interpreter is `uni`, which compiles and runs on both Linux and Mac.
The interpreters for Universal Lambda and Lazy K, `clamb` and `lazyk`, can be run on both of these systems as well.

To build all interpreters:

```sh
make interpreters
```

Or, to build them individually:
```sh
make blc tromp uni clamb lazyk asc2bin
```

Here, asc2bin is a utility required to pack the ASCII 01 bitstream source of BLC and UL to a byte stream,
which is the format accepted by the BLC and UL interpreters.

The interpreters' source codes are obtained from an external source, each published by its authors mentioned in the previous section.
When the make recipe is run, each recipe obtains these external source codes using the following commands:

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
