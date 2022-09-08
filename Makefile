target_blc=lambdalisp.blc
target_lazy=lambdalisp.lazy
target_ulamb=lambdalisp.ulamb
target_latex=lambdalisp.tex

def_prelude=./src/def-prelude.cl
def_prelude_lazyk=./src/targets/def-prelude-lazyk.cl


all:
	$(MAKE) $(target_blc)

test:
	$(MAKE) $(target_blc)
	./tools/run-test.sh

blc: $(target_blc)
$(target_blc): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp $(def_prelude) ./src/main.cl
	cd src; sbcl --script main.cl > ../$(target_blc).tmp
	mv $(target_blc).tmp $(target_blc)

$(def_prelude): ./src/prelude.lisp ./tools/compile-prelude.sh
	./tools/compile-prelude.sh > $(def_prelude).tmp
	mv $(def_prelude).tmp $(def_prelude)

lazyk: $(target_lazy)
$(target_lazy): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp $(def_prelude_lazyk) ./src/targets/def-prelude-chars-lazyk.cl ./src/main-lazyk.cl ./src/targets/blc-lazyk-ulamb-wrapper.cl
	cd src; sbcl --script ./main-lazyk.cl > ../$(target_lazy).tmp
	mv $(target_lazy).tmp $(target_lazy)

$(def_prelude_lazyk): ./src/prelude.lisp ./tools/compile-prelude.sh
	./tools/compile-prelude.sh compile-lazyk > $(def_prelude_lazyk).tmp
	mv $(def_prelude_lazyk).tmp $(def_prelude_lazyk)

ulamb: $(target_ulamb)
$(target_ulamb): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp $(def_prelude) ./src/main-ulamb.cl ./src/targets/blc-lazyk-ulamb-wrapper.cl
	cd src; sbcl --script ./main-ulamb.cl > ../$(target_ulamb).tmp
	mv $(target_ulamb).tmp $(target_ulamb)

latex: $(target_latex)
$(target_latex): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp $(def_prelude) ./src/main-latex.cl
	./tools/make-latex.sh

