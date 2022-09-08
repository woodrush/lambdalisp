target_blc=lambdalisp.blc
target_lazy=lambdalisp.lazy
target_latex=lambdalisp.tex

main_blc=./src/main.cl
main_lazyk=./src/main.cl
main_latex=./src/targets/main_latex.cl

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

latex: $(target_latex)
$(target_latex): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp ./src/def-prelude.cl ./src/targets/main-latex.cl
	./tools/make-latex.sh

lazyk: $(target_lazy)
$(target_lazy): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp ./src/def-prelude-lazyk.cl ./src/def-prelude-chars-lazyk.cl ./src/targets/main-lazyk.cl
	cd src; sbcl --script main-lazy.cl > ../$(target_lazy)

blc: $(target_blc)
$(target_blc): ./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp ./src/def-prelude.cl ./src/main.cl
	cd src; sbcl --script main.cl > ../$(target_blc)

$(def_prelude): ./src/prelude.lisp ./tools/compile-prelude.sh
	./tools/compile-prelude.sh > ./src/def-prelude.cl
