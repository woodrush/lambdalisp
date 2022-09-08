target_blc=lambdalisp.blc
target_lazy=lambdalisp.lazy
target_latex=lambdalisp.tex

def_prelude=./src/def-prelude.cl
def_prelude_lazyk=./src/targets/def-prelude-lazyk.cl


all:
	$(MAKE) $(target_blc)

test:
	$(MAKE) $(target_blc)
	./tools/run-test.sh

latex: $(target_latex)
$(target_latex): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp $(def_prelude) ./src/targets/main-latex.cl
	./tools/make-latex.sh

lazyk: $(target_lazy)
$(target_lazy): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp $(def_prelude_lazyk) ./src/targets/def-prelude-chars-lazyk.cl ./src/targets/main-lazyk.cl ./src/targets/blc-lazyk-ulamb-wrapper.cl
	cd src; sbcl --script ./targets/main-lazyk.cl > ../$(target_lazy)

blc: $(target_blc)
$(target_blc): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp $(def_prelude) ./src/main.cl
	cd src; sbcl --script main.cl > ../$(target_blc)

$(def_prelude): ./src/prelude.lisp ./tools/compile-prelude.sh
	./tools/compile-prelude.sh > $(def_prelude)

$(def_prelude_lazyk): ./src/prelude.lisp ./tools/compile-prelude.sh
	./tools/compile-prelude.sh compile-lazyk > $(def_prelude_lazyk)
