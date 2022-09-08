target_blc=lambdalisp.blc

lambdalisp_cl=./src/lambdalisp.cl
lambdacraft_cl=./src/lambdacraft.cl
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
	cd src; sbcl --script lambdalisp.cl > ../$(target_blc)

$(def_prelude): $(prelude_lisp) $(compile_prelude)
	./tools/compile-prelude.sh > $(def_prelude)
