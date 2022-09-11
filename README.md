# LambdaLisp
LambdaLisp is a Lisp interpreter written as a pure untyped lambda calculus term.
The entire lambda calculus expression is viewable as a PDF [here](lambdalisp.pdf).

LambdaLisp is tested by running `examples/*.cl` on both Common Lisp and LambdaLisp and comparing their outputs.
The largest LambdaLisp-Common-Lisp polyglot program that has been tested is [lambdacraft.cl](./examples/lambdacraft.cl),
which runs the lambda calculus compiler [LambdaCraft](https://github.com/woodrush/lambdacraft) I wrote for this project, used to compile LambdaLisp itself.

LambdaLisp is written as a lambda calculus term `LambdaLisp = λx. ...`
which takes a string `x` as an input and returns a string as an output.
The input `x` represents the Lisp program and the user's standard input,
and the output represents the standard output.
Characters are encoded into lambda term representations of natural numbers using the [Church encoding](https://en.wikipedia.org/wiki/Church_encoding),
and strings are encoded to a list of lambda terms using the [Mogensen-Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding),
so the entire computation process solely consists of the beta-reduction of lambda calculus terms,
without the need of introducing any non-lambda-type object.

When run on a lambda calculus interpreter that runs on the terminal,
LambdaLisp presents a REPL where you can interactively define and evaluate Lisp expressions.
These interpreters automatically process the Mogensen-Scott string-to-lambda encoding for handling I/O through the terminal.
Supported interpreters are:

- The [521-byte lambda calculus interpreter](https://justine.lol/lambda/) written by Justine Tunney
- The [IOCCC](https://www.ioccc.org/) 2012 ["Most functional"](https://www.ioccc.org/2012/tromp/hint.html) interpreter written by John Tromp
  (the [source](https://www.ioccc.org/2012/tromp/tromp.c) is in the shape of a λ)
- Universal Lambda interpreter [clamb](https://github.com/irori/clamb) and Lazy K interpreter [lazyk](https://github.com/irori/lazyk) written by Kunihiko Sakamoto

Further details are described in the How it Works section.


## Usage
You can try the LambdaLisp REPL by simply running:

```sh
git clone https://github.com/woodrush/lambdalisp
cd lambdalisp
make run-repl
```
The requirement for running this is `gcc`.
This will build all the required tools and run LambdaLisp on the lambda calculus interpreter `clamb`,
presenting a REPL for interacting with LambdaLisp.
When building `clamb`, Make runs `git clone https://github.com/irori/clamb` to clone `clamb`'s source code.

The source code being run is [lambdalisp.ulamb](lambdalisp.ulamb),
which is the lambda calculus term shown in [lambdalisp.pdf](lambdalisp.pdf) written in [binary lambda calculus](https://tromp.github.io/cl/Binary_lambda_calculus.html) notation.
`clamb` automatically takes care of the [Mogensen-Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding)-based I/O to run LambdaLisp on the terminal.
Interaction is done by writing LambdaLisp in continuation passing style,
allowing a Haskell-style interactive I/O to work on lambda calculus interpreters.
This also allows imperative programming on LambdaLisp with `read` and `print`.

Once `make run-repl` is run, the REPL can also be run with:

```sh
# Pack the 01 bitstream to a bytestream
cat lambdalisp.ulamb | ./bin/asc2bin > lambdalisp.ulamb.bin

# Run the REPL
./bin/clamb lambdalisp.ulamb.bin -u
```


## Example
```lisp
(defun new-counter (init)
  ;; Return a closure.
  ;; Use the let over lambda technique for creating independent and persistent variables
  (let ((i init))
    (lambda () (setq i (+ 1 i)))))

;; Instantiate counters
(setq counter1 (new-counter 0))
(setq counter2 (new-counter 10))

(print (counter1)) ;; => 1
(print (counter1)) ;; => 2
(print (counter2)) ;; => 11
(print (counter1)) ;; => 3
(print (counter2)) ;; => 12
(print (counter1)) ;; => 4
(print (counter1)) ;; => 5
```

can be run as

```sh
( cat lambdalisp.ulamb | ./bin/asc2bin; cat examples/counter.lisp ) | ./bin/clamb -u
( cat lambdalisp.ulamb | ./bin/asc2bin; cat examples/counter.lisp - ) | ./bin/clamb -u  # To enter the REPL after executing the script
```

More examples can be found under [./examples](./examples).
The largest program written for LambdaLisp that has been tested is [lambdacraft.cl](./examples/lambdacraft.cl),
which runs the lambda calculus compiler [LambdaCraft](https://github.com/woodrush/lambdacraft) I wrote for this project, used to compile LambdaLisp itself.


## Features
Key features are:

- Signed 32-bit integer literals
- String literals
- Lexical scopes, closures and persistent bindings with `let`
- Object oriented programming feature with class inheritance (as pre-loaded macros using closures)
- Reader macros with `set-macro-character`
- Access to the interpreter's virtual heap memory with `malloc`, `memread`, and `memwrite`
- Show the call stack trace when an error is invoked
- Garbage collection during macro evaluation

Supported special forms and functions are:

- defun, defmacro, lambda (&rest can be used)
- quote, atom, car, cdr, cons, eq
- +, -, *, /, mod, =, >, <, >=, <=
- read (reads Lisp expressions), print, format, write-to-string, intern, stringp
- let, let*, labels, setq, boundp
- progn, loop, block, return, return-from, if, cond, error
- list, append, reverse, length, position, mapcar
- make-hash-table, gethash (setf can be used)
- equal, and, or, not
- eval, apply
- set-macro-character, peek-char, read-char, `` ` ``   `,`   `,@`   `'`   `#\`
- carstr, cdrstr, str, string comparison with =, >, <, >=, <=, string concatenation with +
- defun-local, defglobal, type, macro
- malloc, memread, memwrite
- new, defclass, defmethod, `.`, field assignment by setf


## Supported Lambda Calculus Interpreters
Below is a summary of the supported languages and interpreters.
These lambda calculus interpreters run on the terminal,
and automatically handles the previously described
[Mogensen-Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding) for the standard input and output.
Each language uses a slightly different lambda term encoding for the I/O,
and a different notation for providing the lambda term as an input to the interpreter.

LambdaLisp is written natively as a lambda term that accepts and produces the I/O encoding of the language [Binary Lambda Calculus](https://tromp.github.io/cl/cl.html).
It is adapted to other languages by wrapping it with an encoder-decoder that absorbs the string-to-lambda encoding differences in each environment.


| Language                                                     | Extension | Engine                  | Program Format               |
|--------------------------------------------------------------|-----------|-------------------------|------------------------------|
| [Binary Lambda Calculus](https://tromp.github.io/cl/cl.html) | *.blc     | Untyped Lambda Calculus | Binary (asc2bin can be used) |
| [Universal Lambda](http://www.golfscript.com/lam/)           | *.ulamb   | Untyped Lambda Calculus | Binary (asc2bin can be used) |
| [Lazy K](https://tromp.github.io/cl/lazy-k.html)             | *.lazy    | SKI Combinator Calculus | ASCII                        |

| Interpreter                                         | Language               | Platforms    | Build Command | Author                             | Notes                                                                                                                                                                                |
|-----------------------------------------------------|------------------------|--------------|---------------|------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [Blc](https://justine.lol/lambda/)                  | Binary Lambda Calculus | x86-64-Linux | `make blc`    | [@jart](https://github.com/jart)   | [521-byte interpreter](https://justine.lol/lambda/)                                                                                                                                  |
| [tromp](https://www.ioccc.org/2012/tromp/hint.html) | Binary Lambda Calculus | Any          | `make tromp`  | [@tromp](https://github.com/tromp) | [IOCCC](https://www.ioccc.org/) 2012 ["Most functional"](https://www.ioccc.org/2012/tromp/hint.html) - the [source](https://www.ioccc.org/2012/tromp/tromp.c) is in the shape of a λ |
| [uni](https://tromp.github.io/cl/cl.html)           | Binary Lambda Calculus | Any          | `make uni`    | [@tromp](https://github.com/tromp) | Unobfuscated version of `tromp`                                                                                                                                                      |
| [clamb](https://github.com/irori/clamb)             | Universal Lambda       | Any          | `make clamb`  | [@irori](https://github.com/irori) | Fast UL interpreter                                                                                                                                                                  |
| [lazyk](https://github.com/irori/lazyk)             | Lazy K                 | Any          | `make lazyk`  | [@irori](https://github.com/irori) | Fast Lazy K interpreter                                                                                                                                                              |

### Building the Lambda Calculus Interpreters
Several notes about the interpreters:

- The BLC intepreter `Blc` only runs on x86-64-Linux systems.
- The BLC interpreter `tromp` may not compile on a Mac with the defualt gcc (which is actually an alias of clang). Details are provided below.
- The most reliably compilable BLC interpreter is `uni`, which compiles and runs on both Linux and Mac.
- The interpreters for Universal Lambda and Lazy K, `clamb` and `lazyk`, can be built and run on both of these systems.

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


### Running LambdaLisp
#### On Binary Lambda Calculus
After building all of the required tools and interpreters, running LambdaLisp on the Binary Lambda Calculus interpreter `Blc` can be done as follows.
To run on `tromp` or `uni`, replace `Blc` with `tromp` or `uni`.
```sh
# Pack the 01 bitstream to a bytestream
cat lambdalisp.blc | ./bin/asc2bin > lambdalisp.blc.bin

cat lambdalisp.blc.bin -            | ./bin/Blc # Run the LambdaLisp REPL
cat lambdalisp.blc.bin [filepath]   | ./bin/Blc # Run a LambdaLisp script and exit
cat lambdalisp.blc.bin [filepath] - | ./bin/Blc # Run a LambdaLisp script, then enter the REPL
```

Running `cat -` with the hyphen connects the standard input after the specified input files,
allowing the user to interact with the interpreter through the terminal after reading a file.
If `cat -` doesn't work, the following command can be used instead:

```sh
( cat lambdalisp.blc.bin [filepath]; cat ) | ./bin/Blc
```

#### On Universal Lambda
Running LambdaLisp on the Universal Lambda interpreter `clamb` can be done as follows.
Note that `lambdalisp.ulamb` and `lambdalisp.blc` are different files although they look similar,
since they are different languages.
This is since the I/O lambda term encoding is different for these languages.
Otherwise, both languages are based entirely on untyped lambda calculus.
```sh
# Pack the 01 bitstream to a bytestream
cat lambdalisp.ulamb | ./bin/asc2bin > lambdalisp.ulamb.bin

# The -u option is required for handling I/O properly
./bin/clamb lambdalisp.ulamb.bin -u                    # Run the LambdaLisp REPL
cat [filepath]   | ./bin/clamb -u lambdalisp.ulamb.bin # Run a LambdaLisp script and exit
cat [filepath] - | ./bin/clamb -u lambdalisp.ulamb.bin # Run a LambdaLisp script, then enter the REPL
```

#### On Lazy K
Running LambdaLisp on the Lazy K interpreter `lazyk` can be done as follows:
```sh
# The -u option is required for handling I/O properly
./bin/lazyk lambdalisp.lazy -u                    # Run the LambdaLisp REPL
cat [filepath]   | ./bin/lazyk lambdalisp.lazy -u # Run a LambdaLisp script and exit
cat [filepath] - | ./bin/lazyk lambdalisp.lazy -u # Run a LambdaLisp script, then enter the REPL
```

### Building 'tromp' on a Mac
Mac has `gcc` installed by default or via Xcode Command Line Tools.
However, `gcc` is actually installed as an alias to `clang`, which is a different compiler that doesn't compile `tromp`.
This is confirmable by running `gcc --version`. On my Mac, running it shows:

```sh
$ gcc --version
Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/include/c++/4.2.1
Apple clang version 12.0.0 (clang-1200.0.32.29)
Target: x86_64-apple-darwin19.6.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
```

A workaround for this is to use `uni` instead, which is an unobfuscated version of `tromp` compilable with clang.
To build `tromp`, first install gcc via [Homebrew](https://brew.sh/):

```sh
brew install gcc
```

Currently, this should install the command `gcc-11`.
After installing gcc, check the command it has installed.

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



## Building from Source
LambdaLisp's source code is written using [LambdaCraft](https://github.com/woodrush/lambdacraft), a DSL written in Common Lisp for compiling
Common Lisp to lambda calculus terms which I wrote for this project.
Building from source requires SBCL (Steel Bank Common Lisp), a Common Lisp interpreter.

First install SBCL with:

```sh
sudo apt install sbcl
```

or on a Mac with:
```sh
brew install sbcl
```

LambdaLisp can then be compiled with:
```sh
make blc     # Builds lambdalisp.blc, for Binary Lambda Calculus
make ulamb   # Builds lambdalisp.ulamb, for Universal Lambda
make lazyk   # Builds lambdalisp.lazy, for Lazy K
```

To compile [lambdalisp.pdf](./lambdalisp.pdf), first install LaTeX, and then run:
```sh
make pdf
```


## Testing
There are 2 types of tests for LambdaLisp.
Each test requires SBCL (Steel Bank Common Lisp), a Common Lisp interpreter.

To run the tests, run:

```sh
make test      # Runs the tests on the BLC interpreter `uni`
make test-all  # Runs the tests on all of the available interpreters
```

`make test-all` uses a maximum of about 5GB of memory (on the largest program [examples/lambdacraft.cl](examples/lambdacraft.cl), which takes several minutes) and takes about 10 minutes on my machine.

### Output Comparison Test
Runs the programs in `./examples/`. Runnable with:

```sh
make test-blc test-blc-uni test-blc-tromp test-ulamb test-lazyk
```

- The files `examples/*.cl` run both on Common Lisp and LambdaLisp producing identical results, except for the initial `> ` printed by the REPL in LambdaLisp. For programs with the extension `*.cl`, the programs are run in Steel Bank Common Lisp (SBCL) and LambdaLisp, and the outputs are compared.
- The files `examples/*.lisp` are LambdaLisp-exclusive programs. The output of these files are compared with `test/*.lisp.out`.
- LambdaLisp runs on three lambda-calculus-based and SKI-combinator-calculus-based languages,
  binary lambda calculus, Universal Lambda, and Lazy K.
  For binary lambda calculus, there are three interpreters, Blc, uni, and tromp.
  Each `make` command shown here runs this test in each of the languages and interpreters.

### LambdaCraft Compiler Hosting Test
- `examples/lambdacraft.cl` runs [LambdaCraft](https://github.com/woodrush/lambdacraft), a Common-Lisp-to-lambda-calculus compiler written in Common Lisp,
  used to compile the lambda calculus source for LambdaLisp.
  It defines a binary lambda calculus (BLC) program that prints the letter `A` and exits, 
  and prints the BLC source code for the defined program.
- The LambdaCraft compiler hosting test first executes `examples/lambdacraft.cl` on LambdaLisp, then runs the output BLC program on a BLC interpreter, and checks if it prints the letter `A` and exits.
- The test is run on binary lambda calculus, with either the interpreter Blc or uni.

Runnable with:
```sh
make test-compiler-hosting-blc test-compiler-hosting-blc-uni
```

### Experimental: Self-Hosting Test
This test is currently theoretical since it requires a lot of time and memory, and is unused in `make test-all`.
This test extends the previous LambdaCraft compiler hosting test and checks if the Common Lisp source code for LambdaLisp runs on LambdaLisp itself. Since the LambdaCraft compiler hosting test runs properly, this test should theoretically run as well, although it requires a tremendous amount of memory and time. One concern is whether the 32-bit heap address space used internally in LambdaLisp is enough to compile this program. This can be circumvented by compiling LambdaLisp with an address space of 64-bit or larger, which can be done simply by replacing the literal `32` (which only appears once in `src/lambdalisp.cl`) with `64`, etc.
The test is run on the binary lambda calculus interpreter Blc.

Runnable with:
```sh
make test-self-host
```


## How it Works
### Handling I/O in Lambda Calculus
LambdaLisp is written as a function `LambdaLisp = λx. ...`
which takes a string as an input and returns a string as an output.
The input represents the Lisp program and the user's standard input (the `x` is the input string),
and the output represents the standard output.
A string is represented as a list of bits of its ASCII representation.
In untyped lambda calculus, a method called the [Mogensen-Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding)
can be used to express a list of lambda terms as a pure untyped lambda calculus term, without the help of introducing a non-lambda-type object.
Bits are encoded as `0 = λx.λy.x` and `1 = λx.λy.y`.
Under these rules, the bit sequence `0101` can be expressed as a pure beta-normal lambda term
`λf.(f (λx.λy.x) λg.(g (λx.λy.y λh.(h (λx.λy.x) λi.(i (λx.λy.y) (λx.λy.y))))))`,
where the last `λx.λy.y` is used as a list terminator having the same role as `nil` in Lisp.
By defining the lambda terms `cons = λx.λy.λf.(f x y)` and `nil = λx.λy.y`, this term can be rewritten as
`(cons 0 (cons 1 (cons 0 (cons 1 nil))))`, which is exactly the same as how lists are constructed in Lisp
(note that `1 == nil`).
Using this method, both the standard input and output strings can entirely be encoded into pure lambda calculus terms,
allowing for LambdaLisp to operate with beta reduction of lambda terms as its sole rule of computation,
without the requirement of introducing any non-lambda-type object.

The LambdaLisp execution flow is thus as follows: you first encode the input string (Lisp program and stdin)
as lambda terms, apply it to `LambdaLisp = λx. ...`, beta-reduce it until it is in beta normal form,
and parse the output lambda term as a Mogensen-Scott-encoded list of bits
(inspecting the equivalence of lambda terms is quite simple in this case since it is in beta normal form).
This rather complex flow is supported exactly as is in 3 lambda-calculus-based programming languages:
Binary Lambda Calculus, Universal Lambda, and Lazy K.

### Lambda-Calculus-Based Programming Languages
Binary Lambda Calculus (BLC) and Universal Lambda (UL) are programming languages with the exact same I/O strategy described above -
a program is expressed as a pure lambda term that takes a Mogensen-Scott-encoded string and returns a Mogensen-Scott-encoded string.
When the interpreters for these languages `Blc` and `clamb` are run on the terminal,
the interpreter automatically encodes the input bytestream to lambda terms, performs beta-reduction,
parses the output lambda term as a list of bits, and prints the output as a string in the terminal.
The differences in BLC and UL are in a slight difference in the method for encoding the I/O.
Otherwise, both of these languages follow the same principle, where lambda terms are the solely avalable object types in the language.
Even Haskell has primitive data types for data such as integers and strings.
In these lambda-calculus-based languages, _everything_ including the data is expressed as lambdas,
making each of them a truly "purely" functional language.

In BLC and UL, lambda terms are written in a notation called [binary lambda calculus](https://tromp.github.io/cl/Binary_lambda_calculus.html).
The BLC notation for a lambda term can be obtained by first rewriting it in [De Bruijn notation](https://en.wikipedia.org/wiki/De_Bruijn_index),
then encoding `λ = 00`, `apply = 01`, and `i = 1^i0`. For example, `λx.λy.λz.λt.y -> λλλλ3 -> 000000001110`, `(λx.x)(λx.x) -> apply λ1 λ1 -> 0100100010`.
The bitstream in [lambdalisp.ulamb](./lambdalisp.ulamb) decodes into the lambda term shown in [lambdalisp.pdf](lambdalisp.pdf) this way.

Lazy K is a language with the same I/O strategy with BLC and UL except programs are written as
[SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) terms instead of lambda terms.
The SKI combinator calculus is a system equivalent to lambda calculus,
where there are only 3 functions : `S = λx.λy.λz.(x z (y z))`, `K = λx.λy.x`, and `I = λx.x`,
and every SKI combinator calculus term is written as a combination of these 3 functions.
Every SKI term can be easily be converted to an equivalent lambda calculus term by simply rewriting the term with these rules.
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
if [BF](https://en.wikipedia.org/wiki/Brainfuck) captures the distilled essence of structured imperative programming,
Lazy K captures the distilled essence of functional programming.
It might as well be the assembly language of lazily evaluated functional programming.
With the simple syntax and rules orchestrating a Turing-complete language,
I find Lazy K to be a very beautiful language being one of my all-time favorites.

LambdaLisp is written in these 3 languages - Binary Lambda Calculus, Universal Lambda, and Lazy K.
In each of these languages, LambdaLisp is expressed as a lambda term or SKI combinator calculus term.
Therefore, to run LambdaLisp, an interpreter for one of these languages is required.
To put in a rather convoluted way, LambdaLisp is a Lisp interpreter that runs on another language's interpreter.

LambdaLisp is written natively as a lambda term that accepts and produces the I/O encoding of the language [Binary Lambda Calculus](https://tromp.github.io/cl/cl.html),
which uses a list of bits to encode each character.
Universal Lambda and Lazy K uses natural numbers in the [Church encoding](https://en.wikipedia.org/wiki/Church_encoding) as the character encoding.
These differences in the string-to-lambda encoding is adapted to other languages by wrapping LambdaLisp's source with an [encoder-decoder wrapper](./src/lazyk-ulamb-blc-wrapper.cl) that absorbs these encoding differences.
