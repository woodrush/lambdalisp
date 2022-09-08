target_blc=lambdalisp.blc
target_lazy=lambdalisp.lazy

lambdalisp_cl=./src/lambdalisp.cl
lambdacraft_cl=./src/lambdacraft.cl
def_prelude=./src/def-prelude.cl
prelude_lisp=./src/prelude.lisp
compile_prelude=./tools/compile-prelude.sh
lambdalisp_latex=./lambdalisp.tex

def_prelude_chars_lazyk=./src/targets/def-prelude-chars-lazyk.cl
def_prelude_lazyk=./src/targets/def-prelude-lazyk.cl
wrapper=./src/targets/blc-lazyk-ulamb-wrapper.cl


all:
	$(MAKE) $(target_blc)

test:
	$(MAKE) $(target_blc)
	./tools/run-test.sh

latex: $(lambdalisp_latex)
$(lambdalisp_latex): $(lambdalisp_cl) $(def_prelude) $(lambdacraft_cl)
	./tools/make-latex.sh

lazyk: $(target_lazy)
$(target_lazy): $(lambdalisp_cl) $(def_prelude_chars_lazyk) $(def_prelude_lazyk) $(lambdacraft_cl)
	cd src; sbcl --script lambdalisp.cl > ../$(target_lazy)

blc: $(target_blc)
$(target_blc): $(def_prelude) $(lambdalisp_cl) $(lambdacraft_cl)
	cd src; sbcl --script lambdalisp.cl > ../$(target_blc)

$(def_prelude): $(prelude_lisp) $(compile_prelude)
	./tools/compile-prelude.sh > $(def_prelude)
