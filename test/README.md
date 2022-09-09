# Tests
There are 3 types of tests for LambdaLisp:
- Output comparison test: `make test-blc test-blc-uni test-blc-tromp test-ulamb test-lazyk`
  - Runs the programs in `./examples/`.
    - The files `examples/*.cl` run both on Common Lisp and LambdaLisp producing identical results, except for the initial `> ` printed by the REPL in LambdaLisp. For programs with the extension `*.cl`, the programs are run in Steel Bank Common Lisp (SBCL) and LambdaLisp, and the outputs are compared.
    - The files `examples/*.lisp` are LambdaLisp-exclusive programs. The output of these files are compared with `test/*.lisp.out`.
    - LambdaLisp runrs on three languages, binary lambda calculus, Universal Lambda, or Lazy K. For binary lambda calculus, there are three interpreters, Blc, uni, and tromp. Each `make` commands shown here runs this test in each of the languages and interpreters.
- LambdaCraft compiler hosting test: `make test-compiler-hosting-blc test-compiler-hosting-blc-uni`
  - `examples/lambdacraft.cl` runs LambdaCraft, a lambda calculus program compiler written in Common Lisp, used to compile the lambda calculus term program for LambdaLisp. `examples/lambdacraft.cl` defines a binary lambda calculus (BLC) program that prints the letter `A` and exits, and prints the BLC source code for the defined program.
  - The LambdaCraft compiler hosting test first executes `examples/lambdacraft.cl` on LambdaLisp, then runs the output BLC program on a BLC interpreter, and checks if it prints the letter `A` and exits.
  - The test is run on binary lambda calculus, with either the interpreter Blc or uni.
- Self-hosting test (currently theoretical, requires a lot of time and memory): `make test-self-host`
  - This extends the previous LambdaCraft compiler hosting test and checks if the Common Lisp source code for LambdaLisp runs on LambdaLisp itself. Since the LambdaCraft compiler hosting test runs properly, this test should theoretically run as well, although it requires a tremendous amount of memory and time. One concern is whether the 32-bit heap address space used internally in LambdaLisp is enough to compile this program. This can be circumvented by compiling LambdaLisp with an address space of 64-bit or larger, which can be done simply by replacing the literal `32` (which only appears once in `src/lambdalisp.cl`) with `64`, etc.
  - The test is run on the binary lambda calculus interpreter Blc.
