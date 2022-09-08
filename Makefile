target_blc=lambdalisp.blc
target_lazy=lambdalisp.lazy
target_ulamb=lambdalisp.ulamb
target_latex=lambdalisp.tex

def_prelude=./src/build/def-prelude.cl
def_prelude_lazyk=./src/build/def-prelude-lazyk.cl

BASE_SRC=./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp


all:
	$(MAKE) $(target_blc)

test:
	$(MAKE) $(target_blc)
	./tools/run-test.sh


# Compile the prelude
$(def_prelude): ./src/prelude.lisp ./tools/compile-prelude.sh
	mkdir -p src/build
	./tools/compile-prelude.sh > $(def_prelude).tmp
	mv $(def_prelude).tmp $(def_prelude)

$(def_prelude_lazyk): ./src/prelude.lisp ./tools/compile-prelude.sh
	mkdir -p src/build
	./tools/compile-prelude.sh compile-lazyk > $(def_prelude_lazyk).tmp
	mv $(def_prelude_lazyk).tmp $(def_prelude_lazyk)


# Compile the main code
blc: $(target_blc)
$(target_blc): $(BASE_SRC) $(def_prelude) ./src/main.cl
	cd src; sbcl --script main.cl > ../$(target_blc).tmp
	mv $(target_blc).tmp $(target_blc)

lazyk: $(target_lazy)
$(target_lazy): $(BASE_SRC) $(def_prelude_lazyk) ./src/chars-lazyk.cl ./src/main-lazyk.cl ./src/blc-lazyk-ulamb-wrapper.cl
	cd src; sbcl --script ./main-lazyk.cl > ../$(target_lazy).tmp
	mv $(target_lazy).tmp $(target_lazy)

ulamb: $(target_ulamb)
$(target_ulamb): $(BASE_SRC) $(def_prelude) ./src/main-ulamb.cl ./src/blc-lazyk-ulamb-wrapper.cl
	cd src; sbcl --script ./main-ulamb.cl > ../$(target_ulamb).tmp
	mv $(target_ulamb).tmp $(target_ulamb)

latex: $(target_latex)
$(target_latex): $(BASE_SRC) $(def_prelude) ./src/main-latex.cl
	./tools/make-latex.sh
