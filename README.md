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

## How it Works

### Handling I/O in Lambda Calculus
LambdaLisp is written as a function `LambdaLisp = λx. ...`
where it takes one string (representing the Lisp program and the user's standard input) as an input (the `x` is the input string),
and returns one string (representing the standard output) as an output.
A string is represented as a list of bits of its ASCII representation.
In untyped lambda calculus, a method called the [Mogensen-Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding)
can be used to express a list in pure untyped lambda calculus terms, without the help of introducing a non-lambda-type object.
Bits are encoded as `0 = λx.λy.x` and `1 = λx.λy.y`.
Under these rules, the bit sequence `0101` can be expressed as a pure beta-normal lambda term
`λf.(f (λx.λy.x) λg. (g (λx.λy.y λh. (h (λx.λy.x) λi.(i (λx.λy.x) (λx.λy.y))))))`,
where the last `λx.λy.y` is used as a list terminator having the same role as `nil` in Lisp.
By defining the lambda terms `cons = λx.λy.λf.(f x y)`, `t = λx.λy.x`, and `nil = λx.λy.y`, this term can be rewritten as
`(cons t (cons nil (cons t (cons nil nil))))`, which is exactly the same as how lists are constructed in Lisp.
Using this method, both the standard input and output strings can entirely be encoded into pure lambda calculus terms,
allowing for LambdaLisp to operate with beta reduction of lambda terms as its sole rule of computation,
without the requirement of introducing any non-lambda-type object.

The LambdaLisp execution flow is thus as follows: one first encodes the input string (Lisp program and stdin)
as lambda terms, applies it to `LambdaLisp = λx. ...`, beta-reduces it until it is in beta normal form,
and parses the output lambda term as a Mogensen-Scott-encoded list of bits
(inspecting the equivalence of lambda terms is quite simple in this case since it is in beta normal form).
This rather complex flow is supported exactly as is in 3 lambda-calculus-based programming languages,
Binary Lambda Calculus (BLC), Universal Lambda (UL), and Lazy K.

### Lambda-Calculus-Based Programming Languages
Binary Lambda Calculus and Universal Lambda are programming languages with the exact same I/O strategy described above -
a program is expressed as one pure lambda term that takes a Mogensen-Scott-encoded string and returns a Mogensen-Scott-encoded string.
When the interpreters for these languages `Blc` and `clamb` are run on the terminal,
the interpreter automatically encodes the input bytestream to lambda terms, performs beta-reduction,
parses the output lambda term as a list of bits, and prints the output as a string in the terminal.
The differences in BLC and UL are in a slight difference in the method for encoding the I/O.
Otherwise, both of these languages follow the same principle, where lambda terms are the sole object types avalable in the language.

Lazy K is a language with the same I/O strategy except programs are written as
[SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) terms instead of lambda terms.
The SKI combinator calculus is a system equivalent to lambda calculus,
where there are only 3 functions : `S = λx.λy.λz.(x z (y z))`, `K = λx.λy.x`, and `I = λx.x`.
Since every SKI combinator calculus term is written as a combination of these 3 functions,
every SKI term can be easily be converted to an equivalent lambda calculus term by simply rewriting the term with these rules.
Very interestingly, there is a method for converting the other way around -
there is a [consistent method](https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis)
to convert an arbitrary lambda term with an arbitrary number of variables to an equivalent SKI combinator calculus term.
This equivalence relation with lambda calculus proves that SKI combinator calculus is Turing-complete.

Apart from the strong condition that only 3 predefined functions exist,
the beta-reduction rules for the SKI combinator calculus are exactly identical as that of lambda calculus,
so the computation flow and the I/O strategy for Lazy K is the same as BLC and Universal Lambda -
all programs can be written purely in SKI combinator calculus terms without the need of introducing any function other than `S`, `K`, and `I`.
This allows Lazy K's syntax to be astonishingly simple, where only 4 keywords exist - `s`, `k`, `i`, and `` ` `` for function application.
As mentioned in the [original Lazy K design proposal](https://tromp.github.io/cl/lazy-k.html),
if [BF](https://en.wikipedia.org/wiki/Brainfuck) captures the distilled essence of imperative programming,
Lazy K captures the distilled essence of functional programming.
With the simple syntax and rules orchestrating a Turing-complete language,
I find Lazy K to be a very beautiful language being one of my all-time favorites.

LambdaLisp is written in these 3 languages - Binary Lambda Calculus, Universal Lambda, and Lazy K.
In each of these languages, LambdaLisp is expressed as one lambda term or SKI combinator calculus term.
Therefore, to run LambdaLisp, an interpreter for one of these languages is required.
To put in a rather convoluted way, LambdaLisp is a Lisp interpreter that runs on another language's interpreter.

Currenly, LambdaLisp is tested in the following 5 interpreters:
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
