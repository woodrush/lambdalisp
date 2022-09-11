# Tests
There are 2 types of tests for LambdaLisp.
Each test requires SBCL (Steel Bank Common Lisp), a Common Lisp interpreter.

To run the tests, run:

```sh
make test      # Runs the tests on the BLC interpreter `uni`
make test-all  # Runs the tests on all of the available interpreters
```

`make test-all` uses a maximum of about 5GB of memory (on the largest program [examples/lambdacraft.cl](../examples/lambdacraft.cl), which takes several minutes) and takes about 10 minutes on my machine.

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
