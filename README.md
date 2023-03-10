![LambdaLisp's logo](./bin/lambdalisp_logo.png)
<br>
[![test](https://github.com/woodrush/lambdalisp/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/woodrush/lambdalisp/actions/workflows/test.yml)


LambdaLisp is a Lisp interpreter written as an untyped lambda calculus term.
It basically runs a large subset of Common Lisp as shown in the [Features](#features) section.
The entire lambda calculus expression is viewable as a PDF [here](https://woodrush.github.io/lambdalisp.pdf).


## Overview
LambdaLisp is a Lisp interpreter written as a closed untyped lambda calculus term.
It is written as a lambda calculus term `LambdaLisp = λx. ...`
which takes a string `x` as an input and returns a string as an output.
The input `x` is the Lisp program and the user's standard input,
and the output is the standard output.
Characters are encoded into lambda term representations of natural numbers using the [Church encoding](https://en.wikipedia.org/wiki/Church_encoding),
and strings are encoded as a list of characters with lists expressed as lambdas in the [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding),
so the entire computation process solely consists of the beta-reduction of lambda terms,
without introducing any non-lambda-type object.

Supported features are closures and persistent bindings with `let`, reader macros, 32-bit signed integers, strings, and much more.
LambdaLisp is tested by running programs on both Common Lisp and LambdaLisp and comparing their outputs.
The largest LambdaLisp-Common-Lisp polyglot program that has been tested is [lambdacraft.cl](./examples/lambdacraft.cl),
which runs the Lisp-to-lambda-calculus compiler [LambdaCraft](https://github.com/woodrush/lambdacraft) written for this project, also used to compile LambdaLisp itself.

When run on a lambda calculus interpreter that runs on the terminal,
LambdaLisp presents a REPL where you can interactively define and evaluate Lisp expressions.
These interpreters automatically process the string-to-lambda encoding for handling I/O through the terminal.
Supported interpreters are:

- SectorLambda, the [521-byte lambda calculus interpreter](https://justine.lol/lambda/) written by Justine Tunney
- The [IOCCC](https://www.ioccc.org/) 2012 ["Most functional"](https://www.ioccc.org/2012/tromp/hint.html) interpreter written by John Tromp
  (the [source](https://www.ioccc.org/2012/tromp/tromp.c) is in the shape of a λ)
- Universal Lambda interpreter [clamb](https://github.com/irori/clamb) and Lazy K interpreter [lazyk](https://github.com/irori/lazyk) written by Kunihiko Sakamoto

Lisp has been described by Alan Kay as the [Maxwell's equations of software](https://www.gnu.org/software/mes/manual/html_node/LISP-as-Maxwell_0027s-Equations-of-Software.html).
In the same sense, I believe that lambda calculus is the particle physics of computation.
LambdaLisp may therefore be a gigantic electromagnetic [Lagrangian](https://en.wikipedia.org/wiki/Mathematical_formulation_of_the_Standard_Model) that connects the realm of human-friendly programming to the origins of the notion of computation itself.


## Usage
LambdaLisp runs on x86-64-Linux and other platforms such as Mac.

### Trying the LambdaLisp REPL (on x86-64-Linux)
You can try the LambdaLisp REPL by simply running:

```sh
git clone https://github.com/woodrush/lambdalisp
cd lambdalisp
make run-repl
```
The requirement is `cc` which should be installed by default.
To try it on a Mac, please see the next section.

This will run LambdaLisp on SectorLambda, the [521-byte lambda calculus interpreter](https://justine.lol/lambda/).
The source code being run is [lambdalisp.blc](./bin/lambdalisp.blc),
which is the lambda calculus term shown in [lambdalisp.pdf](lambdalisp.pdf) written in [binary lambda calculus](https://tromp.github.io/cl/Binary_lambda_calculus.html) notation.

SectorLambda automatically takes care of the string-to-lambda I/O encoding to run LambdaLisp on the terminal.
Interaction is done by writing LambdaLisp in continuation-passing style,
allowing a Haskell-style interactive I/O to work on lambda calculus interpreters.

When building SectorLambda, Make runs the following commands to get its source codes:

- `Blc.S`: `wget https://justine.lol/lambda/Blc.S?v=2`
- `flat.lds`: `wget https://justine.lol/lambda/flat.lds`

After running `make run-repl`, the REPL can also be run as:

```sh
( cat ./bin/lambdalisp.blc | ./bin/asc2bin; cat ) | ./bin/Blc
```


### Trying the LambdaLisp REPL (on Other Platforms)
SectorLambda is x86-64-Linux exclusive. On other platforms such as a Mac, the following command can be used:

```sh
git clone https://github.com/woodrush/lambdalisp
cd lambdalisp
make run-repl-ulamb
```
This runs LambdaLisp on the lambda calculus interpreter `clamb`.
The requirement for this is `gcc` or `cc`.

After running `make run-repl-ulamb`, the REPL can also be run as:

```sh
( cat ./bin/lambdalisp.ulamb | ./bin/asc2bin; cat ) | ./bin/clamb -u
```

To run LambdaLisp on other lambda calculus interpreters, please see the Supported Lambda Calculus Interpreters section.



### Playing the Number Guessing Game
Once `make run-repl` is run, you can play the [number guessing game](./examples/number-guessing-game.cl) with:

```sh
( cat ./bin/lambdalisp.blc | ./bin/asc2bin; cat ./examples/number-guessing-game.cl; cat ) | ./bin/Blc
```

If you ran `make run-repl-ulamb`, you can run:

```sh
( cat ./bin/lambdalisp.ulamb | ./bin/asc2bin; cat ./examples/number-guessing-game.cl; cat ) | ./bin/clamb -u
```

You can run the same script on Common Lisp. If you use SBCL, you can run it with:

```sh
sbcl --script ./examples/number-guessing-game.cl
```


## Examples

### Closures
The following LambdaLisp code runs right out of the box:

```lisp
(defun new-counter (init)
  ;; Return a closure.
  ;; Use the let over lambda technique for creating independent and persistent variables.
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

An equivalent JavaScript code is:

```javascript
// Runs on the browser's console
function new_counter (init) {
    let i = init;
    return function () {
        return ++i;
    }
}

var counter1 = new_counter(0);
var counter2 = new_counter(10);

console.log(counter1()); // => 1
console.log(counter1()); // => 2
console.log(counter2()); // => 11
console.log(counter1()); // => 3
console.log(counter2()); // => 12
console.log(counter1()); // => 4
console.log(counter1()); // => 5
```

### Object-Oriented Programming
As described in [Let Over Lambda](https://letoverlambda.com/),
when you have closures, you get object-oriented programming for free.
LambdaLisp has a built-in OOP feature implemented as predefined macros based on closures.
It supports Python-like classes with class inheritance:

```lisp
;; Runs on LambdaLisp
(defclass Counter ()
  (i 0)

  (defmethod inc ()
    (setf (. self i) (+ 1 (. self i))))

  (defmethod dec ()
    (setf (. self i) (- (. self i) 1))))


(defclass Counter-add (Counter)
  (defmethod *init (i)
    (setf (. self i) i))

  (defmethod add (n)
    (setf (. self i) (+ (. self i) n))))


(defparameter counter1 (new Counter))
(defparameter counter2 (new Counter-add 100))

((. counter1 inc))
((. counter2 add) 100)

(setf (. counter1 i) 5)
(setf (. counter2 i) 500)
```

An equivalent Python code is:

```python
class Counter ():
    i = 0

    def inc (self):
        self.i += 1
        return self.i
    
    def dec (self):
        self.i -= 1
        return self.i

class Counter_add (Counter):
    def __init__ (self, i):
        self.i = i
    
    def add (self, n):
        self.i += n
        return self.i

counter1 = Counter()
counter2 = Counter_add(100)

counter1.inc()
counter2.add(100)

counter1.i = 5
counter2.i = 500
```


### More Examples
More examples can be found under [./examples](./examples).
The largest program written for LambdaLisp that has been tested is [lambdacraft.cl](./examples/lambdacraft.cl),
which runs the lambda calculus compiler [LambdaCraft](https://github.io/woodrush/lambdacraft) written for this project, also used to compile LambdaLisp itself.



## Features
Key features are:

- Signed 32-bit integers
- Strings
- Closures, lexical scopes, and persistent bindings with `let`
- Object-oriented programming feature with class inheritance
- Reader macros with `set-macro-character`
- Access to the interpreter's virtual heap memory with `malloc`, `memread`, and `memwrite`
- Show the call stack trace when an error is invoked
- Garbage collection during macro expansion

Supported special forms and functions are:

- defun, defmacro, lambda (&rest can be used)
- quote, atom, car, cdr, cons, eq
- +, -, *, /, mod, =, >, <, >=, <=, integerp
- read (reads Lisp expressions), read-char, peek-char, print, format (supports `~a` and `~%`), write-to-string, intern, stringp
- let, let*, labels, setq, boundp
- progn, loop, block, return, return-from, if, cond, error
- list, append, reverse, length, position, mapcar
- make-hash-table, gethash (setf can be used)
- equal, and, or, not
- eval, apply
- set-macro-character, `` ` ``   `,`   `,@`   `'`   `#\`
- carstr, cdrstr, str, string comparison with =, >, <, >=, <=, string concatenation with +
- defun-local, defglobal, type, macro
- malloc, memread, memwrite
- new, defclass, defmethod, `.`, field assignment by setf


## Supported Lambda Calculus Interpreters
Below is a summary of the supported lambda calculus interpreters.
All interpreters run on the terminal and automatically handles the previously described
string-to-lambda encoding for the standard I/O.
Each interpreter uses a slightly different I/O encoding, classified below as languages.

LambdaLisp is written natively as a lambda term based on the language [Binary Lambda Calculus](https://tromp.github.io/cl/cl.html).
It is adapted to other languages by wrapping it with an [encoder-decoder](./src/lazyk-ulamb-blc-wrapper.cl) that absorbs the language spec differences.


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

- The BLC intepreter `Blc` runs only on x86-64-Linux systems.
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

Here, asc2bin is a utility that packs ASCII 0/1 bitstreams to a byte stream, the format accepted by the BLC and UL interpreters.

The interpreters' source codes are obtained from external locations.
When the make recipe is run, each recipe obtains these external source codes using the following commands:

- `blc`:
  - `Blc.S`: `wget https://justine.lol/lambda/Blc.S?v=2`
  - `flat.lds`: `wget https://justine.lol/lambda/flat.lds`
- `uni`: `wget https://tromp.github.io/cl/uni.c`
- `tromp`: `wget http://www.ioccc.org/2012/tromp/tromp.c`
- `clamb`: `git clone https://github.com/irori/clamb`
- `lazyk`: `git clone https://github.com/irori/lazyk`



### Running LambdaLisp
#### On Binary Lambda Calculus
After building all of the required tools and interpreters, running LambdaLisp on the Binary Lambda Calculus interpreter `Blc` can be done as follows:

```sh
cd ./bin
# Pack the 01 bitstream to a bytestream
cat lambdalisp.blc | ./asc2bin > lambdalisp.blc.bin

cat lambdalisp.blc.bin -            | ./Blc # Run the LambdaLisp REPL
cat lambdalisp.blc.bin [filepath]   | ./Blc # Run a LambdaLisp script and exit
cat lambdalisp.blc.bin [filepath] - | ./Blc # Run a LambdaLisp script, then enter the REPL
```

To run on `tromp` or `uni`, replace `Blc` with `tromp` or `uni`.

Running `cat -` with the hyphen connects the standard input after the specified input files,
allowing the user to interact with the interpreter through the terminal after reading a file.
If `cat -` doesn't work, the following command can be used instead:

```sh
( cat lambdalisp.blc.bin [filepath]; cat ) | ./Blc
```

#### On Universal Lambda
Running LambdaLisp on the Universal Lambda interpreter `clamb` can be done as follows.
Note that `lambdalisp.ulamb` and `lambdalisp.blc` are different files although they look similar,
since they are different languages.
This is since the I/O lambda term encoding is different for these languages.
Otherwise, both languages are based entirely on untyped lambda calculus.
```sh
cd ./bin

# Pack the 01 bitstream to a bytestream
cat lambdalisp.ulamb | ./asc2bin > lambdalisp.ulamb.bin

# The -u option is required for handling I/O properly
./clamb lambdalisp.ulamb.bin -u                    # Run the LambdaLisp REPL
cat [filepath]   | ./clamb -u lambdalisp.ulamb.bin # Run a LambdaLisp script and exit
cat [filepath] - | ./clamb -u lambdalisp.ulamb.bin # Run a LambdaLisp script, then enter the REPL
```

#### On Lazy K
Running LambdaLisp on the Lazy K interpreter `lazyk` can be done as follows:
```sh
cd ./bin
# The -u option is required for handling I/O properly
./lazyk ./lambdalisp.lazy -u                    # Run the LambdaLisp REPL
cat [filepath]   | ./lazyk lambdalisp.lazy -u # Run a LambdaLisp script and exit
cat [filepath] - | ./lazyk lambdalisp.lazy -u # Run a LambdaLisp script, then enter the REPL
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
Common Lisp to lambda calculus terms which written for this project.
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
make blc-src     # Builds lambdalisp.blc, for Binary Lambda Calculus
make ulamb-src   # Builds lambdalisp.ulamb, for Universal Lambda
make lazyk-src   # Builds lambdalisp.lazy, for Lazy K
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

The GitHub Actions CI runs `make test-ulamb`, which does the following:

- Compiles [./bin/lambdalisp.blc](./bin/lambdalisp.blc) from [./src/main.cl](./src/main.cl) using SBCL
- Builds `Blc` (SectorLambda)
- Runs `./examples/src/*.cl` on both LambdaLisp (run with `Blc`) and SBCL and compares the outputs
- Runs `./examples/src/*.lisp` on LambdaLisp and compares the outputs with `./test/*.lisp.out`
- Runs the LambdaCraft Compiler Hosting Test (described below)


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
This test extends the previous LambdaCraft compiler hosting test and checks if the Common Lisp source code for LambdaLisp runs on LambdaLisp itself. Since the LambdaCraft compiler hosting test runs properly, this test should theoretically run as well, although it requires a tremendous amount of memory and time. The test is run on the binary lambda calculus interpreter Blc.

One concern is whether the 32-bit heap address space used internally in LambdaLisp is enough to compile this program. This can be solved by compiling LambdaLisp with an address space of 64-bit or larger, which can be done simply by replacing the literal `32` (which only appears once in `src/lambdalisp.cl`) with `64`, etc.
Another concern is whether if the execution hits Blc's maximum term limit. This can be solved by compiling Blc with a larger memory limit, by editing the rule for `$(BLC)` in the Makefile.


Runnable with:
```sh
make test-self-host
```

## Lambda Calculus Interpreter Written in LambdaLisp
The [examples-advanced](./examples-advanced/) directory features lambda calculus interpreters written in LambdaLisp itself.
This means that we have a lambda calculus interpreter written in Lisp, which is written in lambda calculus, that runs on a lambda calculus interpreter.

Furthermore, the interpreter [lisplambda-bit.lisp](./examples-advanced/lisplambda-bit.lisp)
is capable of running [uni.blc](https://www.ioccc.org/2012/tromp/uni.blc),
the bit-oriented BLC self-interpreter from the IOCCC 2012 "Most functional" entry written by John Tromp.
uni.blc is a bit-oriented BLC interpreter written in bit-oriented BLC itself.
From the standard input, it takes a program and a standard input, and evaluates the result of the program applied with the standard input.

This means that we have a lambda calculus interpreter written in lambda calculus itself, which runs on a lambda calculus interpreter written in Lisp, which is written in lambda calculus, that runs on a lambda calculus interpreter.


## How it Works
Implementation details are introduced in [this blog post](https://woodrush.github.io/blog/lambdalisp.html).
