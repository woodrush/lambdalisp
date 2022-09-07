target_blc=lambdalisp.blc

lambdalisp_cl=./lambdalisp.cl
lambdacraft_cl=./lambdacraft.cl
def_prelude=./src/def-prelude.cl
prelude_lisp=./src/prelude.lisp
compile_prelude=./tools/compile-prelude.sh
lambdalisp_latex=./lambdalisp.tex

all:
	$(MAKE) $(target_blc)

test:
	$(MAKE) $(target_blc)
	./tools/run-test.sh

.PHONY: latex
latex: $(lambdalisp_latex)
$(lambdalisp_latex): $(lambdalisp_cl) $(def_prelude) $(lambdacraft_cl)
	./tools/make-latex.sh

$(target_blc): $(def_prelude) $(lambdalisp_cl) $(lambdacraft_cl)
	sbcl --script $(lambdalisp_cl) > $(target_blc)

$(def_prelude): $(prelude_lisp) $(compile_prelude)
	./tools/compile-prelude.sh > $(def_prelude)
