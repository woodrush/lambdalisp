target_blc=lambdalisp.blc
target_lazy=lambdalisp.lazy
target_ulamb=lambdalisp.ulamb
target_latex=lambdalisp.tex

def_prelude=./src/build/def-prelude.cl
def_prelude_lazyk=./src/build/def-prelude-lazyk.cl

BASE_SRCS=./src/lambdalisp.cl ./src/lambdacraft.cl ./src/prelude.lisp


all:
	$(MAKE) $(target_blc)
	$(MAKE) $(target_ulamb)

test: test-blc


# Tests
BLC=../uni
ULAMB=../clamb/clamb
LAZYK=../lazyk/lazyk
SBCL=sbcl
ASC2BIN=../asc2bin.com

examples/%.blc.test : examples/%.cl
	mkdir -p ./test
	( cat ./lambdalisp.blc | $(ASC2BIN); cat $< ) | $(BLC) | sed -e '1s/> //' > ./test/$(notdir $<).blc.out
	( $(SBCL) --script $<; echo ) > ./test/$(notdir $<).sbcl.out
	cmp ./test/$(notdir $<).blc.out ./test/$(notdir $<).sbcl.out || (echo "Test failed at $<" && exit 1)

test-blc : $(addsuffix .blc.test, $(basename $(wildcard examples/*.cl)))
	@echo "All tests have passed for BLC."

examples/%.ulamb.test : examples/%.cl
	mkdir -p ./test
	( cat ./lambdalisp.ulamb | $(ASC2BIN); cat $< ) | $(ULAMB) | sed -e '1s/> //' > ./test/$(notdir $<).ulamb.out
	( $(SBCL) --script $<; echo ) > ./test/$(notdir $<).sbcl.out
	cmp ./test/$(notdir $<).ulamb.out ./test/$(notdir $<).sbcl.out || (echo "Test failed at $<" && exit 1)

test-ulamb : $(addsuffix .ulamb.test, $(basename $(wildcard examples/*.cl)))
	@echo "All tests have passed for Universal Lambda."

examples/%.lazyk.test : examples/%.cl
	mkdir -p ./test
	cat $< | $(LAZYK) ./lambdalisp.lazy -u | sed -e '1s/> //' > ./test/$(notdir $<).lazy.out
	( $(SBCL) --script $<; echo ) > ./test/$(notdir $<).sbcl.out
	cmp ./test/$(notdir $<).lazy.out ./test/$(notdir $<).sbcl.out || (echo "Test failed at $<" && exit 1)

test-lazyk : $(addsuffix .lazyk.test, $(basename $(wildcard examples/*.cl)))
	@echo "All tests have passed for Lazy K."


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
$(target_blc): $(BASE_SRCS) $(def_prelude) ./src/main.cl
	cd src; sbcl --script main.cl > ../$(target_blc).tmp
	mv $(target_blc).tmp $(target_blc)

lazyk: $(target_lazy)
$(target_lazy): $(BASE_SRCS) $(def_prelude_lazyk) ./src/lazyk-chars.cl ./src/main-lazyk.cl ./src/lazyk-ulamb-blc-wrapper.cl
	@echo "Compiling to Lazy K takes a while (several minutes)."
	cd src; sbcl --script ./main-lazyk.cl > ../$(target_lazy).tmp
	mv $(target_lazy).tmp $(target_lazy)

ulamb: $(target_ulamb)
$(target_ulamb): $(BASE_SRCS) $(def_prelude) ./src/main-ulamb.cl ./src/lazyk-ulamb-blc-wrapper.cl
	cd src; sbcl --script ./main-ulamb.cl > ../$(target_ulamb).tmp
	mv $(target_ulamb).tmp $(target_ulamb)


# Additional targets
latex: $(target_latex)
$(target_latex): $(BASE_SRCS) $(def_prelude) ./src/main-latex.cl
	./tools/make-latex.sh
